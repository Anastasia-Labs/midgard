import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import {
  createDeploymentRunState,
  transitionDeploymentStep,
  writeDeploymentRunStateAtomic,
} from "midgard-node/e2e/run-state";
import { afterEach, describe, expect, it } from "vitest";

import { checkWallets, deploymentSteps } from "../src/full-stack/deployment.js";
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
    processes.responses["references-observation"] = () =>
      reconciliation(published ? "satisfied" : "blocked");
    processes.responses["deployment-status"] = {
      manifest: { ok: false },
      protocol: { complete: false, empty: true, hubOracleWitness: null },
    };
    const journal = await runStackWorkflow(
      { directory: config.runDirectory, intentDigest: "c".repeat(64) },
      [steps.nonce!, steps.references!],
    );
    expect(journal.steps.references!.status).toBe("complete");
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
  it("never creates a second nonce after an attempt that left no record", async () => {
    const { steps } = await deployment({ nonceRecorded: false });
    expect(await steps.nonce!.reconcile(undefined)).toEqual({
      status: "retry",
    });
    expect(await steps.nonce!.reconcile(running)).toEqual({
      status: "pending",
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
  const funded = (lovelace: bigint) => ({
    totals: { lovelace: String(lovelace) },
    utxos: [
      {
        assets: { lovelace: String(lovelace) },
        inlineDatum: null,
        referenceScriptHash: null,
        dataHash: null,
      },
    ],
  });
  it("requires each configured budget before anything was spent", async () => {
    const { processes } = await deployment({ nonceRecorded: false });
    for (const role of Object.keys(processes.config.wallets))
      processes.responses[`wallet-${role}`] = funded(6_000_000n);
    await expect(checkWallets(processes)).rejects.toThrow(
      "below its configured funding budget",
    );
  });
  it("needs only working capital once the deployment has started spending", async () => {
    const { processes } = await deployment({ nonceRecorded: true });
    for (const role of Object.keys(processes.config.wallets))
      processes.responses[`wallet-${role}`] = funded(6_000_000n);
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
