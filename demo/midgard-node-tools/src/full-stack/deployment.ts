import { join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { parseReconciliationResult } from "midgard-node/commands/reconcile";
import { loadDeploymentRunState } from "midgard-node/e2e/run-state";

import {
  type InitializationObservation,
  initializationRecovery,
  PENDING_REINCLUSION,
} from "./initialization-recovery.js";
import { readJsonIfPresent } from "./journal.js";
import { exportLocalCardanoConfig } from "./native-ledger.js";
import { StackProcesses } from "./process.js";
import {
  assertHostDatabaseIsStackDatabase,
  assertPreservedStorage,
} from "./storage.js";
import { deriveStackWallets } from "./wallets.js";
import type { StackStep } from "./workflow.js";

export function stackPaths(processes: StackProcesses) {
  return {
    manifest: join(
      processes.config.nodeRoot,
      "deploymentInfo/contract-deployment-info.json",
    ),
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
    typeof report.protocol.complete !== "boolean" ||
    typeof report.protocol.empty !== "boolean"
  )
    throw new Error("Invalid deployment status response");
  return report;
}
export async function checkWallets(processes: StackProcesses) {
  const addresses = deriveStackWallets(processes.config, processes.env);
  const paths = stackPaths(processes);
  // Only a deployment that has spent nothing needs its full budget. A resumed one needs working capital.
  const fresh =
    (await readJsonIfPresent(paths.manifest)) === undefined &&
    (await loadDeploymentRunState(paths.state))?.steps.hubOracleNonce ===
      undefined;
  for (const [role, wallet] of Object.entries(processes.config.wallets)) {
    const address = addresses[role]!;
    const result = (await processes.node(`wallet-${role}`, [
      "l1-utxos",
      "--address",
      address,
    ])) as {
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
    if (
      !result.utxos.some(
        (utxo) =>
          utxo.inlineDatum === null &&
          utxo.dataHash === null &&
          utxo.referenceScriptHash === null &&
          Object.keys(utxo.assets).length === 1 &&
          BigInt(utxo.assets.lovelace ?? 0) >= 5_000_000n,
      )
    )
      throw new Error(
        `Wallet ${role} needs a plain ADA output for collateral/fees`,
      );
  }
  return addresses;
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
      reconcile: async (record) => {
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
            if (!status.protocol.complete) throw new Error(PENDING_REINCLUSION);
            return { status: "complete", data: { attached: true } };
          }
        }
        const state = await loadDeploymentRunState(paths.state);
        // The node records the nonce only after submitting it. A started attempt
        // without any record may have submitted one, so never create a second.
        if (
          record?.status === "running" &&
          state?.steps.hubOracleNonce === undefined &&
          manifest === undefined
        )
          return { status: "pending" };
        if (state?.steps.hubOracleNonce?.status !== "complete")
          return { status: "retry" };
        const address = walletFromSeed(
          processes.env[processes.config.wallets.operator!.seedEnv]!,
          { network: "Preprod" },
        ).address;
        const live = (await processes.node("nonce-observation", [
          "l1-utxos",
          "--address",
          address,
        ])) as { utxos: { txHash: string; outputIndex: number }[] };
        if (
          live.utxos.some(
            (utxo) =>
              utxo.txHash === state.identity.hubOracleOneShot?.txHash &&
              utxo.outputIndex === state.identity.hubOracleOneShot.outputIndex,
          )
        )
          return { status: "complete", data: state.identity.hubOracleOneShot };
        // It may have been consumed by initialization before its checkpoint was written.
        const status = await deploymentStatus(processes);
        return status.protocol.complete
          ? { status: "complete", data: state.identity.hubOracleOneShot }
          : { status: "pending" };
      },
      execute: async () => {
        await processes.node("nonce-create-or-resume", [
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
          "init",
          "--contract-deployment-info-output",
          paths.manifest,
        ]);
      },
    },
    {
      id: "operator",
      reconcile: async () => {
        const status = (await processes.node("operator-observation", [
          "operator-status",
        ])) as { state: string };
        if (status.state === "active")
          return { status: "complete", data: status };
        // register-active-operator resumes from registered, and the ordered set refuses a duplicate key.
        if (["none", "registered"].includes(status.state))
          return { status: "retry" };
        return { status: "pending" };
      },
      execute: () =>
        processes.node("operator-register-or-resume", [
          "register-active-operator",
        ]),
    },
  ];
}
