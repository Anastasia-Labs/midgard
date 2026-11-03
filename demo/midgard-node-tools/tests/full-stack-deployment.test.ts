import { access, chmod, constants, mkdir } from "node:fs/promises";
import { join } from "node:path";

import {
  DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
} from "da-committee-node/config";
import {
  createDeploymentRunState,
  type DeploymentRunState,
  loadDeploymentRunState,
  transitionDeploymentStep,
  writeDeploymentRunStateAtomic,
} from "midgard-node/e2e/run-state";
import { afterEach, describe, expect, it } from "vitest";

import {
  checkWallets,
  DA_SUBMITTER_WALLET_FLOOR,
  deploymentSteps,
} from "../src/full-stack/deployment.js";
import { readJsonIfPresent } from "../src/full-stack/journal.js";
import { CommandNotStartedError } from "../src/full-stack/process.js";
import {
  runStackWorkflow,
  type StackStep,
} from "../src/full-stack/workflow.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

afterEach(removeStackFixtures);

const nonce = { txHash: "a".repeat(64), outputIndex: 0 };
const reconciliation = (status: string) => ({
  schemaVersion: "midgard-e2e-reconciliation-v1",
  milestone: "reference-scripts-complete",
  target: {
    scope: "node-runtime",
    address: "addr_test1stack",
    authPolicyId: "b".repeat(56),
  },
  status,
  safeToRetryOriginalStep: false,
  evidence: [{ kind: "reference_scripts", detail: {} }],
  nextAction: status === "satisfied" ? null : "Run with --repair",
  repairActions: [],
});
/** What the node reports before its publication records an auth policy. */
const notStarted = {
  ...reconciliation("blocked"),
  target: {
    scope: "node-runtime",
    address: "addr_test1stack",
    authPolicyId: null,
  },
  evidence: [
    { kind: "reference_script_auth_policy", detail: { recorded: false } },
  ],
  nextAction: "No reference-script auth policy is recorded yet",
};
const running = { status: "running" as const, attempts: 1, data: null };

async function deployment(options: { nonceRecorded: boolean }) {
  const { config } = await stackFixture();
  const processes = new RecordingProcesses(config, stackEnvironment(config));
  const statePath = join(config.runDirectory, "deployment-run-state.json");
  await mkdir(config.runDirectory, { recursive: true });
  if (options.nonceRecorded)
    await writeDeploymentRunStateAtomic(
      statePath,
      transitionDeploymentStep(
        createDeploymentRunState({
          mode: "fresh",
          identity: { network: "Preprod", hubOracleOneShot: nonce },
        }),
        "hubOracleNonce",
        "complete",
      ),
    );
  const steps = Object.fromEntries(
    deploymentSteps(processes).map((step) => [step.id, step]),
  ) as Record<string, StackStep>;
  return { config, processes, statePath, steps };
}

describe("deployment steps before initialization", () => {
  it("publishes and confirms reference scripts while no deployment manifest exists", async () => {
    const { config, processes, statePath, steps } = await deployment({
      nonceRecorded: true,
    });
    let published = false;
    processes.responses["nonce-observation"] = { utxos: [nonce] };
    processes.responses["references-publish"] = () => {
      published = true;
      return { mode: "publish" };
    };
    // As the node reports before its publication records an auth policy.
    processes.responses["references-observation"] = () =>
      published ? reconciliation("satisfied") : notStarted;
    processes.responses["deployment-status"] = {
      manifest: { ok: false },
      protocol: { complete: false, empty: true, hubOracleWitness: null },
    };
    const journal = await runStackWorkflow(
      { directory: config.runDirectory, intentDigest: "c".repeat(64) },
      [steps.nonce!, steps.references!],
    );
    expect(journal.steps.references!.status).toBe("complete");
    expect(
      processes.calls
        .map((call) => call.id)
        .filter((id) => id.startsWith("references-")),
    ).toEqual([
      "references-observation",
      "references-publish",
      "references-observation",
    ]);
    // The operator wallet's nonce is looked up through Kupo, not by an
    // address scan of the local node's ledger.
    expect(
      processes.calls.find((call) => call.id === "nonce-observation")!
        .overrides,
    ).toMatchObject({ L1_ACCESS: "kupmios" });
    expect(await steps.initialize!.reconcile(undefined)).toEqual({
      status: "retry",
    });
    // The node refuses a configured manifest path that does not exist yet.
    for (const call of processes.calls) {
      expect(call.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH).toBe("");
      expect(call.overrides.MIDGARD_RUN_STATE_PATH).toBe(statePath);
    }
    expect(processes.calls.map((call) => call.id)).toContain(
      "deployment-status",
    );
  });
  it("leaves an expired auth policy to the node and stops on its refusal", async () => {
    const { config, processes, statePath, steps } = await deployment({
      nonceRecorded: true,
    });
    // A policy recorded by an earlier attempt, now expired.
    const state = (await loadDeploymentRunState(statePath))!;
    await writeDeploymentRunStateAtomic(statePath, {
      ...state,
      identity: {
        ...state.identity,
        referenceScriptAuthPolicyId: "b".repeat(56),
        referenceScriptAuthPolicy: {
          policyId: "b".repeat(56),
          nativeScript: {
            type: "Native",
            cborHex: "8200581c" + "d".repeat(56),
            expiresAtSlot: 1,
            expiresAtUnixTime: Date.now() - 60 * 60_000,
            timelockDurationMs: 4 * 60 * 60_000,
          },
        },
      },
    });
    const refusal =
      "Reference-script auth policy expired, and 1 reference scripts are already published under it; rerun with --fresh-redeploy --fresh-redeploy-reason <reason>.";
    processes.responses["nonce-observation"] = { utxos: [nonce] };
    processes.responses["references-observation"] = reconciliation("blocked");
    processes.responses["references-publish"] = () => {
      throw new Error(refusal);
    };
    await expect(
      runStackWorkflow(
        { directory: config.runDirectory, intentDigest: "c".repeat(64) },
        [steps.nonce!, steps.references!],
      ),
    ).rejects.toThrow(refusal);
    const publishes = processes.calls.filter(
      (call) => call.id === "references-publish",
    );
    // The node entry point, then the command as the node receives it.
    expect(publishes.map((call) => call.args)).toEqual([
      [
        expect.any(String),
        "deploy-reference-script-node-runtime",
        "--run-state",
        join(config.runDirectory, "deployment-run-state.json"),
        "--contract-deployment-info-output",
        expect.any(String),
      ],
    ]);
  });
  // What the node leaves in its run state at each point of one nonce attempt.
  const stages: Record<
    string,
    (state: DeploymentRunState) => DeploymentRunState
  > = {
    "never started": (state) => state,
    "signed but not submitted": (state) =>
      transitionDeploymentStep(state, "hubOracleNonceSigned", "submitted", {
        txHashes: [nonce.txHash],
      }),
    "submitted, not yet visible": (state) =>
      transitionDeploymentStep(
        stages["signed but not submitted"]!(state),
        "hubOracleNonce",
        "submitted",
        { txHashes: [nonce.txHash] },
      ),
    landed: (state) =>
      transitionDeploymentStep(
        {
          ...stages["submitted, not yet visible"]!(state),
          identity: { network: "Preprod", hubOracleOneShot: nonce },
        },
        "hubOracleNonce",
        "complete",
      ),
  };
  for (const [stage, write] of Object.entries(stages))
    it(`resumes a nonce attempt that stopped ${stage === "landed" ? "after it landed" : `when ${stage}`}`, async () => {
      const { config, processes, statePath, steps } = await deployment({
        nonceRecorded: false,
      });
      const state = createDeploymentRunState({
        mode: "fresh",
        identity: { network: "Preprod" },
      });
      if (stage !== "never started")
        await writeDeploymentRunStateAtomic(statePath, write(state));
      processes.responses["nonce-observation"] = { utxos: [nonce] };
      // The node resumes whatever its run state holds and records the landed nonce.
      processes.responses["nonce-create-or-resume"] = async () =>
        writeDeploymentRunStateAtomic(
          statePath,
          stages.landed!((await loadDeploymentRunState(statePath)) ?? state),
        );
      // A started attempt is never wedged as ambiguous: rerunning the node is safe.
      expect(await steps.nonce!.reconcile(running)).toEqual(
        stage === "landed"
          ? { status: "complete", data: nonce }
          : { status: "retry" },
      );
      const journal = await runStackWorkflow(
        { directory: config.runDirectory, intentDigest: "c".repeat(64) },
        [steps.nonce!],
      );
      expect(journal.steps.nonce).toMatchObject({
        status: "complete",
        data: nonce,
      });
      const runs = processes.calls.filter(
        (call) => call.id === "nonce-create-or-resume",
      );
      expect(runs).toHaveLength(stage === "landed" ? 0 : 1);
      for (const run of runs)
        expect(run.args).not.toContain("--fresh-redeploy");
    });
  it("restores the prior record when the nonce command never started", async () => {
    const { config, processes, steps } = await deployment({
      nonceRecorded: false,
    });
    processes.responses["nonce-create-or-resume"] = () => {
      throw new CommandNotStartedError("nonce-create-or-resume did not start");
    };
    const context = {
      directory: config.runDirectory,
      intentDigest: "c".repeat(64),
    };
    await expect(runStackWorkflow(context, [steps.nonce!])).rejects.toThrow(
      CommandNotStartedError,
    );
    const path = join(config.runDirectory, "stack-journal.json");
    expect(await readJsonIfPresent(path)).toHaveProperty("steps", {});
    processes.responses["nonce-create-or-resume"] = () => {
      throw new Error("nonce-create-or-resume failed");
    };
    await expect(runStackWorkflow(context, [steps.nonce!])).rejects.toThrow(
      "failed",
    );
    expect(await readJsonIfPresent(path)).toMatchObject({
      steps: { nonce: { status: "running", attempts: 1, data: null } },
    });
  });
  it("resumes a started operator registration", async () => {
    const { processes, steps } = await deployment({ nonceRecorded: true });
    for (const [state, expected] of [
      ["none", "retry"],
      ["registered", "retry"],
      ["retired", "pending"],
    ]) {
      processes.responses["operator-observation"] = { state };
      expect(await steps.operator!.reconcile(running)).toEqual({
        status: expected,
      });
    }
  });
});

describe("wallet funding", () => {
  const funded = (lovelace: bigint, outputs = 1) => ({
    totals: { lovelace: String(lovelace) },
    utxos: Array.from({ length: outputs }, () => ({
      assets: { lovelace: String(lovelace / BigInt(outputs)) },
      inlineDatum: null,
      referenceScriptHash: null,
      dataHash: null,
    })),
  });
  // What the committee's own wallet check needs: 80 ADA over two outputs.
  const fundEach = (
    processes: {
      config: { wallets: object };
      responses: Record<string, unknown>;
    },
    lovelace: bigint,
  ) => {
    for (const role of Object.keys(processes.config.wallets))
      processes.responses[`wallet-${role}`] =
        role === "daSubmitter" ? funded(80_000_000n, 2) : funded(lovelace);
  };
  it("holds the DA submitter wallet to the committee's own floor", () => {
    expect(DA_SUBMITTER_WALLET_FLOOR).toEqual({
      outputs: DEFAULT_L1_SUBMITTER_PREFLIGHT.minSpendableUtxoCount,
      lovelace: DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
    });
  });
  it("refuses a DA submitter wallet its committee would refuse after deployment", async () => {
    const { processes } = await deployment({ nonceRecorded: false });
    for (const [role, wallet] of Object.entries(processes.config.wallets))
      processes.responses[`wallet-${role}`] = funded(
        BigInt(wallet.minimumLovelace),
      );
    await expect(checkWallets(processes)).rejects.toThrow(
      "Wallet daSubmitter needs at least 2 plain ADA outputs",
    );
    const resumed = await deployment({ nonceRecorded: true });
    fundEach(resumed.processes, 6_000_000n);
    resumed.processes.responses["wallet-daSubmitter"] = funded(20_000_000n, 2);
    await expect(checkWallets(resumed.processes)).rejects.toThrow(
      "Wallet daSubmitter needs at least 2 plain ADA outputs holding 80000000 lovelace",
    );
  });
  it("requires each configured budget before anything was spent", async () => {
    const { processes } = await deployment({ nonceRecorded: false });
    for (const role of Object.keys(processes.config.wallets))
      processes.responses[`wallet-${role}`] = funded(6_000_000n);
    await expect(checkWallets(processes)).rejects.toThrow(
      "below its configured funding budget",
    );
  });
  it("reads every wallet through the stack's Kupo, never the local node's ledger", async () => {
    const { processes } = await deployment({ nonceRecorded: true });
    fundEach(processes, 6_000_000n);
    await checkWallets(processes);
    const reads = processes.calls.filter((call) =>
      call.id.startsWith("wallet-"),
    );
    expect(reads.map((call) => call.id)).toEqual(
      Object.keys(processes.config.wallets).map((role) => `wallet-${role}`),
    );
    for (const read of reads)
      expect(read.overrides).toMatchObject({
        L1_ACCESS: "kupmios",
        L1_KUPO_URL: "http://127.0.0.1:1442",
        L1_OGMIOS_URL: "http://127.0.0.1:1337",
      });
  });
  it("needs only working capital once the deployment has started spending", async () => {
    const { processes } = await deployment({ nonceRecorded: true });
    fundEach(processes, 6_000_000n);
    await expect(checkWallets(processes)).resolves.toHaveProperty("user");
  });
  it("treats a signed nonce as spending, since it may already have landed", async () => {
    const { processes, statePath } = await deployment({ nonceRecorded: false });
    await writeDeploymentRunStateAtomic(
      statePath,
      transitionDeploymentStep(
        createDeploymentRunState({ mode: "fresh" }),
        "hubOracleNonceSigned",
        "submitted",
      ),
    );
    fundEach(processes, 6_000_000n);
    await expect(checkWallets(processes)).resolves.toHaveProperty("user");
  });
});

describe("host database commands", () => {
  it("migrate only the Postgres the storage checks inspected", async () => {
    const value = await deployment({ nonceRecorded: true });
    value.processes.responses["storage-identity"] = { id: "7" };
    value.processes.responses["host-database-identity"] = "8";
    await expect(value.steps.initialize!.execute(undefined)).rejects.toThrow(
      "does not reach this stack's Postgres",
    );
    expect(value.processes.calls.map((call) => call.id)).toEqual([
      "storage-identity",
      "host-database-identity",
    ]);
    value.processes.responses["host-database-identity"] = "7";
    await value.steps.initialize!.execute(undefined);
    expect(value.processes.calls.map((call) => call.id).slice(2)).toEqual([
      "storage-identity",
      "host-database-identity",
      "migrate",
      "initialize-submit",
    ]);
  });
});

describe("Cardano providers", () => {
  it("create the snapshot directories as the host user before Docker could create them as root", async () => {
    const value = await deployment({ nonceRecorded: false });
    const directories = ["cardano/db", "cardano/kupo"].map((name) =>
      join(value.config.nodeRoot, name),
    );
    let writableAtStart: boolean[] = [];
    value.processes.responses["providers-start"] = async () => {
      writableAtStart = await Promise.all(
        directories.map((directory) =>
          access(directory, constants.W_OK).then(
            () => true,
            () => false,
          ),
        ),
      );
      return null;
    };
    await value.steps.providers!.execute(undefined);
    expect(writableAtStart).toEqual([true, true]);
  });

  it.each(["cardano/db", "cardano/kupo"])(
    "refuse a %s directory the host user cannot write",
    async (name) => {
      const value = await deployment({ nonceRecorded: false });
      const db = join(value.config.nodeRoot, name);
      await mkdir(db, { recursive: true });
      await chmod(db, 0o500);
      try {
        await expect(value.steps.providers!.execute(undefined)).rejects.toThrow(
          "must be writable by the host user",
        );
        expect(value.processes.calls.map((call) => call.id)).not.toContain(
          "providers-start",
        );
      } finally {
        await chmod(db, 0o700);
      }
    },
  );
});
