import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import {
  createDeploymentRunState,
  writeDeploymentRunStateAtomic,
} from "midgard-node/e2e/run-state";
import { afterEach, describe, expect, it } from "vitest";

import {
  originStep,
  recordOriginStartHint,
  restoreL1Origin,
} from "../src/full-stack/origin.js";
import { L1OriginUndeterminedError } from "../src/l1-origin.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

afterEach(removeStackFixtures);

const NONCE = "ab".repeat(32);
const tip = { slot: 100, blockHash: "11".repeat(32) };
const origin = { slot: 141, blockHash: "cd".repeat(32) };
const derived = {
  origin,
  nonceTxHash: NONCE,
  nonceBlock: { slot: 142, blockHash: "ef".repeat(32) },
};
const ORIGIN_TEXT = `141.${"cd".repeat(32)}`;

async function stack(env: Record<string, string> = {}) {
  const { config } = await stackFixture();
  await mkdir(config.runDirectory, { recursive: true });
  const processes = new RecordingProcesses(config, {
    ...stackEnvironment(config),
    ...env,
  });
  processes.responses["l1-origin-derive"] = derived;
  const step = originStep(processes, () => Promise.resolve());
  const signNonce = () => {
    processes.env.HUB_ORACLE_ONE_SHOT_TX_HASH = NONCE;
    processes.env.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX = "0";
  };
  return { config, processes, step, signNonce };
}
const derivations = (processes: RecordingProcesses) =>
  processes.calls
    .filter((call) => call.id === "l1-origin-derive")
    .map((call) => call.args);

describe("full-stack L1 origin", () => {
  it("derives the origin from the tip read before the nonce was signed", async () => {
    const { config, processes, step, signNonce } = await stack();
    processes.responses["l1-node-tip"] = tip;
    await recordOriginStartHint(processes);
    signNonce();
    expect(await step.reconcile(undefined)).toEqual({ status: "retry" });
    expect(await step.execute(undefined)).toEqual({ l1Origin: ORIGIN_TEXT });
    expect(derivations(processes)).toEqual([[NONCE, `100.${"11".repeat(32)}`]]);
    expect(await step.reconcile(undefined)).toEqual({
      status: "complete",
      data: { l1Origin: ORIGIN_TEXT },
    });
    expect(processes.env.L1_ORIGIN).toBe(ORIGIN_TEXT);
    // A later process restores it with the deployment's nonce.
    const later = new RecordingProcesses(config, stackEnvironment(config));
    await restoreL1Origin(later, NONCE);
    expect(later.env.L1_ORIGIN).toBe(ORIGIN_TEXT);
    await restoreL1Origin(later, "ff".repeat(32));
    expect(later.env.L1_ORIGIN).toBe(ORIGIN_TEXT);
  });

  it("scans from genesis when the node had no blocks before the nonce", async () => {
    const { processes, step, signNonce } = await stack();
    await recordOriginStartHint(processes);
    signNonce();
    await step.execute(undefined);
    expect(derivations(processes)).toEqual([[NONCE, "genesis"]]);
  });

  it("reads no start hint once the node may have signed the nonce", async () => {
    const { config, processes, step, signNonce } = await stack();
    await writeDeploymentRunStateAtomic(
      join(config.runDirectory, "deployment-run-state.json"),
      createDeploymentRunState({
        mode: "fresh",
        identity: { network: "Preprod" },
      }),
    );
    processes.responses["l1-node-tip"] = tip;
    await recordOriginStartHint(processes);
    expect(processes.calls.map((call) => call.id)).not.toContain("l1-node-tip");
    signNonce();
    const refusal = step.execute(undefined);
    await expect(refusal).rejects.toThrow(L1OriginUndeterminedError);
    await expect(refusal).rejects.toThrow(
      `midgard-l1-follower find-origin --tx ${NONCE} --network-magic 1`,
    );
    expect(derivations(processes)).toEqual([]);
    expect(await step.reconcile(undefined)).toEqual({ status: "retry" });
  });

  it("starts the scan at the operator's L1_ORIGIN and accepts only that exact origin", async () => {
    const accepted = await stack({ L1_ORIGIN: ORIGIN_TEXT });
    accepted.signNonce();
    expect(await accepted.step.execute(undefined)).toEqual({
      l1Origin: ORIGIN_TEXT,
    });
    expect(derivations(accepted.processes)).toEqual([[NONCE, ORIGIN_TEXT]]);

    const wrong = `140.${"cd".repeat(32)}`;
    const refused = await stack({ L1_ORIGIN: wrong });
    refused.signNonce();
    await expect(refused.step.execute(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
    expect(await refused.step.reconcile(undefined)).toEqual({
      status: "retry",
    });
    const malformed = await stack({ L1_ORIGIN: "not-a-point" });
    malformed.signNonce();
    await expect(malformed.step.execute(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
    expect(derivations(malformed.processes)).toEqual([]);
  });

  it("refuses a recorded origin that the operator's L1_ORIGIN contradicts", async () => {
    const { config, processes, step, signNonce } = await stack();
    await recordOriginStartHint(processes);
    signNonce();
    await step.execute(undefined);
    const restarted = new RecordingProcesses(config, {
      ...processes.env,
      L1_ORIGIN: `140.${"cd".repeat(32)}`,
    });
    await expect(
      originStep(restarted, () => Promise.resolve()).reconcile(undefined),
    ).rejects.toThrow(L1OriginUndeterminedError);
  });

  it("refuses before the deployment records a nonce", async () => {
    const { step } = await stack({ L1_ORIGIN: ORIGIN_TEXT });
    await expect(step.execute(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
    await expect(step.reconcile(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
  });
});
