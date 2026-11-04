import { constants } from "node:fs";
import { access, mkdir } from "node:fs/promises";
import { join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseReconciliationResult } from "midgard-node/commands/reconcile";
import { loadDeploymentRunState } from "midgard-node/e2e/run-state";

import {
  type InitializationObservation,
  initializationRecovery,
  PENDING_REINCLUSION,
} from "./initialization-recovery.js";
import { readJsonIfPresent } from "./journal.js";
import { exportLocalCardanoConfig } from "./native-ledger.js";
import { originStep, restoreL1Origin } from "./origin.js";
import { stackManifestPath, StackProcesses } from "./process.js";
import {
  assertHostDatabaseIsStackDatabase,
  assertPreservedStorage,
} from "./storage.js";
import { deriveStackWallets } from "./wallets.js";
import type { StackStep } from "./workflow.js";

export function stackPaths(processes: Pick<StackProcesses, "config">) {
  return {
    manifest: stackManifestPath(processes.config),
    state: join(processes.config.runDirectory, "deployment-run-state.json"),
  };
}
export async function restoreDeploymentEnvironment(processes: StackProcesses) {
  const paths = stackPaths(processes);
  const state = await loadDeploymentRunState(paths.state);
  const manifest = (await readJsonIfPresent(paths.manifest)) as
    | { hubOracleOneShot?: { txHash: string; outputIndex: number } }
    | undefined;
  const nonce = state?.identity.hubOracleOneShot ?? manifest?.hubOracleOneShot;
  if (nonce) {
    processes.env.HUB_ORACLE_ONE_SHOT_TX_HASH = nonce.txHash;
    processes.env.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX = String(nonce.outputIndex);
    await restoreL1Origin(processes, nonce.txHash);
  }
  // The node refuses a configured manifest that does not exist yet; empty means unset.
  // Before initialization, node commands derive contracts from MIDGARD_RUN_STATE_PATH.
  processes.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH =
    manifest === undefined ? "" : paths.manifest;
}
export async function deploymentStatus(processes: StackProcesses) {
  await restoreDeploymentEnvironment(processes);
  const report = (await processes.node("deployment-status", [
    "deployment-status",
  ])) as InitializationObservation;
  if (
    !report?.protocol ||
    !["empty", "submitted", "complete"].includes(report.protocol.status) ||
    !["live", "spent"].includes(report.protocol.nonce)
  )
    throw new Error("Invalid deployment status response");
  return report;
}
/**
 * The committee's own submitter-wallet floor (`minSpendableUtxoCount` and
 * `DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE` in da-committee-node), which this
 * CLI does not load at run time.
 */
export const DA_SUBMITTER_WALLET_FLOOR = {
  outputs: 2,
  lovelace: 80_000_000n,
} as const;

export async function checkWallets(processes: StackProcesses) {
  const addresses = deriveStackWallets(processes.config, processes.env);
  const paths = stackPaths(processes);
  // Only a deployment that has spent nothing needs its full budget. A resumed one needs working capital.
  // The node writes its run state before it first submits, so no run state means nothing was signed.
  const fresh =
    (await readJsonIfPresent(paths.manifest)) === undefined &&
    (await loadDeploymentRunState(paths.state)) === null;
  for (const [role, wallet] of Object.entries(processes.config.wallets)) {
    const address = addresses[role]!;
    const result = (await processes.walletUtxos(`wallet-${role}`, address)) as {
      totals: { lovelace: string };
      utxos: {
        assets: Record<string, string>;
        inlineDatum: string | null;
        referenceScriptHash: string | null;
        dataHash: string | null;
      }[];
    };
    if (
      BigInt(result.totals.lovelace) <
      (fresh ? BigInt(wallet.minimumLovelace) : 5_000_000n)
    )
      throw new Error(`Wallet ${role} is below its configured funding budget`);
    const plain = result.utxos.filter(
      (utxo) =>
        utxo.inlineDatum === null &&
        utxo.dataHash === null &&
        utxo.referenceScriptHash === null &&
        Object.keys(utxo.assets).length === 1,
    );
    if (!plain.some((utxo) => BigInt(utxo.assets.lovelace ?? 0) >= 5_000_000n))
      throw new Error(
        `Wallet ${role} needs a plain ADA output for collateral/fees`,
      );
    // The committee checks its submitter wallet only once it starts, after
    // deployment has spent; refuse here what it would refuse there.
    if (role === "daSubmitter") {
      const { outputs, lovelace } = DA_SUBMITTER_WALLET_FLOOR;
      const held = plain.reduce(
        (total, utxo) => total + BigInt(utxo.assets.lovelace ?? 0),
        0n,
      );
      if (plain.length < outputs || held < lovelace)
        throw new Error(
          `Wallet daSubmitter needs at least ${outputs} plain ADA outputs holding ${lovelace} lovelace, as its committee checks at start`,
        );
    }
  }
  return addresses;
}

/**
 * The snapshot bootstraps restore the chain and the Kupo index into these
 * directories as an unprivileged user. Docker creates a missing bind-mount
 * directory as root, so the stack creates them first as the host user.
 */
async function prepareCardanoDatabases(processes: StackProcesses) {
  for (const name of ["cardano/db", "cardano/kupo"]) {
    const directory = join(processes.config.nodeRoot, name);
    await mkdir(directory, { recursive: true, mode: 0o700 });
    await access(directory, constants.W_OK).catch(() => {
      throw new Error(
        `${directory} must be writable by the host user, which restores its snapshot`,
      );
    });
  }
}

export function deploymentSteps(processes: StackProcesses): StackStep[] {
  const paths = stackPaths(processes);
  return [
    {
      id: "providers",
      reconcile: async (record) => {
        if (record?.status === "running" && record.data !== null) {
          const report = (await processes.node("provider-confirmation", [
            "l1-provider-preflight",
            "--json",
          ])) as { ok: boolean };
          if (report.ok === true) return { status: "complete", data: report };
        }
        return { status: "retry" };
      },
      execute: async () => {
        await prepareCardanoDatabases(processes);
        await processes.compose("providers-start", [
          "up",
          "-d",
          "--wait",
          "cardano-node-ogmios",
          "kupo",
          "postgres",
        ]);
        await exportLocalCardanoConfig(processes);
        return processes.node("provider-preflight", [
          "l1-provider-preflight",
          "--json",
        ]);
      },
    },
    {
      id: "storage",
      reconcile: async () => {
        await assertPreservedStorage(processes);
        return { status: "complete", data: { preserved: true } };
      },
      execute: async () => null,
    },
    {
      id: "wallets",
      reconcile: async (record) =>
        record?.status === "running" && record.data !== null
          ? { status: "complete", data: record.data }
          : { status: "retry" },
      execute: () => checkWallets(processes),
    },
    {
      id: "nonce",
      reconcile: async () => {
        await restoreDeploymentEnvironment(processes);
        const manifest = await readJsonIfPresent(paths.manifest);
        if (manifest !== undefined) {
          const value = manifest as {
            steps?: { initProtocol?: { status: string } };
          };
          if (value.steps?.initProtocol?.status === "complete") {
            verifyFinalizedDeploymentManifest(manifest);
            const status = await deploymentStatus(processes);
            if (!status.manifest.ok)
              throw new Error(
                "Existing deployment identity/state does not match; preserve its data",
              );
            if (status.protocol.status !== "complete")
              throw new Error(PENDING_REINCLUSION);
            return { status: "complete", data: { attached: true } };
          }
        }
        const state = await loadDeploymentRunState(paths.state);
        // Rerunning the node is safe at every point: it records the signed nonce
        // before submitting it, then only resubmits those bytes, and it refuses a
        // fresh nonce once any run state exists.
        if (state?.steps.hubOracleNonce?.status !== "complete")
          return { status: "retry" };
        // The node reads the nonce by its outref; initialization spends it,
        // so a spent nonce is complete only once initialization landed.
        const status = await deploymentStatus(processes);
        return status.protocol.nonce === "live" ||
          status.protocol.status === "complete"
          ? { status: "complete", data: state.identity.hubOracleOneShot }
          : { status: "pending" };
      },
      execute: async () => {
        await processes.node("nonce-create-or-resume", [
          "--l1",
          "node",
          "prepare-hub-oracle-one-shot-nonce",
          "--run-state",
          paths.state,
          "--json",
        ]);
        await restoreDeploymentEnvironment(processes);
        return (await loadDeploymentRunState(paths.state))?.identity
          .hubOracleOneShot;
      },
    },
    originStep(processes, () => restoreDeploymentEnvironment(processes)),
    {
      id: "references",
      reconcile: async () => {
        await restoreDeploymentEnvironment(processes);
        const result = parseReconciliationResult(
          await processes.node("references-observation", [
            "reconcile",
            "reference-scripts-complete",
            "--scope",
            "node-runtime",
            "--json",
          ]),
        );
        if (result.status === "satisfied")
          return { status: "complete", data: result };
        // Blocked means not yet published, which publishing resolves.
        if (result.status === "ambiguous") return { status: "pending" };
        return { status: "retry" };
      },
      execute: () =>
        processes.node("references-publish", [
          "--l1",
          "node",
          "deploy-reference-script-node-runtime",
          "--run-state",
          paths.state,
          "--contract-deployment-info-output",
          paths.manifest,
        ]),
    },
    {
      id: "initialize",
      reconcile: async () => {
        const status = await deploymentStatus(processes);
        const manifest = (await readJsonIfPresent(paths.manifest)) as
          | { steps?: { initProtocol?: { txHash?: string; status?: string } } }
          | undefined;
        const recovery = initializationRecovery(status, manifest);
        if (recovery.status !== "complete") return recovery;
        if (recovery.reconstruct)
          await processes.node("initialize-reconcile", [
            "reconcile",
            "deployment-manifest",
            "--out",
            paths.manifest,
            "--init-tx-hash",
            recovery.initHash,
            "--json",
          ]);
        verifyFinalizedDeploymentManifest(
          await readJsonIfPresent(paths.manifest),
        );
        return { status: "complete", data: { initHash: recovery.initHash } };
      },
      execute: async () => {
        await assertHostDatabaseIsStackDatabase(processes);
        await processes.node("migrate", ["db:migrate"]);
        return processes.node("initialize-submit", [
          "--l1",
          "node",
          "init",
          "--contract-deployment-info-output",
          paths.manifest,
        ]);
      },
    },
  ];
}

/**
 * Registers the operator. Its commands read the running node's index, so the
 * step runs once the node's services are up.
 */
export const operatorStep = (processes: StackProcesses): StackStep => ({
  id: "operator",
  reconcile: async () => {
    const status = (await processes.node("operator-observation", [
      "operator-status",
    ])) as { state: string };
    if (status.state === "active") return { status: "complete", data: status };
    // register-active-operator resumes from registered, and the ordered set refuses a duplicate key.
    if (["none", "registered"].includes(status.state))
      return { status: "retry" };
    return { status: "pending" };
  },
  execute: () =>
    processes.node("operator-register-or-resume", ["register-active-operator"]),
});
