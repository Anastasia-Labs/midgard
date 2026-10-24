import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import {
  createDeploymentRunState,
  writeDeploymentRunStateAtomic,
} from "midgard-node/e2e/run-state";
import { afterEach, describe, expect, it } from "vitest";

import {
  originStep,
  restoreL1Origin,
  runL1Origin,
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
/** T: the tip the nonce command recorded in run state before it signed. */
const recorded = { slot: 100, blockHash: "11".repeat(32) };
const RECORDED_TEXT = `100.${"11".repeat(32)}`;
const proven = {
  origin: recorded,
  nonceTxHash: NONCE,
  nonceBlock: { slot: 142, blockHash: "ef".repeat(32) },
};

async function stack(env: Record<string, string> = {}) {
  const { config } = await stackFixture();
  await mkdir(config.runDirectory, { recursive: true });
  const processes = new RecordingProcesses(config, {
    ...stackEnvironment(config),
    ...env,
  });
  processes.responses["l1-origin-derive"] = proven;
  const step = originStep(processes, () => Promise.resolve());
  /** Signs the nonce as the node does: run state records T (null: none). */
  const signNonce = async (l1Origin: typeof recorded | null = recorded) => {
    await writeDeploymentRunStateAtomic(
      join(config.runDirectory, "deployment-run-state.json"),
      createDeploymentRunState({
        mode: "fresh",
        identity: {
          network: "Preprod",
          hubOracleOneShot: { txHash: NONCE, outputIndex: 0 },
          ...(l1Origin === null ? {} : { l1Origin }),
        },
      }),
    );
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
  it("T-a: proves the origin the run state recorded before the nonce was signed, and restores it", async () => {
    const { config, processes, step, signNonce } = await stack();
    await signNonce();
    expect(await step.reconcile(undefined)).toEqual({ status: "retry" });
    expect(await step.execute(undefined)).toEqual({ l1Origin: RECORDED_TEXT });
    expect(derivations(processes)).toEqual([[NONCE, RECORDED_TEXT]]);
    expect(await step.reconcile(undefined)).toEqual({
      status: "complete",
      data: { l1Origin: RECORDED_TEXT },
    });
    expect(processes.env.L1_ORIGIN).toBe(RECORDED_TEXT);
    // A later process restores it with the deployment's nonce.
    const later = new RecordingProcesses(config, stackEnvironment(config));
    await restoreL1Origin(later, NONCE);
    expect(later.env.L1_ORIGIN).toBe(RECORDED_TEXT);
    await restoreL1Origin(later, "ff".repeat(32));
    expect(later.env.L1_ORIGIN).toBe(RECORDED_TEXT);
  });

  it("T-b: refuses, reading no chain, when the run state records no origin", async () => {
    const { processes, step, signNonce } = await stack();
    await signNonce(null);
    const refusal = step.execute(undefined);
    await expect(refusal).rejects.toThrow(L1OriginUndeterminedError);
    await expect(refusal).rejects.toThrow(/records no L1 origin/u);
    expect(derivations(processes)).toEqual([]);
    expect(await step.reconcile(undefined)).toEqual({ status: "retry" });
  });

  it("T-b: refuses, recording nothing, when the chain does not prove the recorded origin", async () => {
    const { processes, step, signNonce } = await stack();
    processes.responses["l1-origin-derive"] = () => {
      throw new L1OriginUndeterminedError("not on the node's chain");
    };
    await signNonce();
    await expect(step.execute(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
    expect(await step.reconcile(undefined)).toEqual({ status: "retry" });
  });

  it("T-b: proves the origin again when the record holds another origin for the nonce", async () => {
    const { processes, step, signNonce } = await stack();
    const derivedBefore = { slot: 141, blockHash: "cd".repeat(32) };
    processes.responses["l1-origin-derive"] = {
      ...proven,
      origin: derivedBefore,
    };
    await signNonce();
    await step.execute(undefined);
    expect(await step.reconcile(undefined)).toEqual({ status: "retry" });
    processes.responses["l1-origin-derive"] = proven;
    expect(await step.execute(undefined)).toEqual({ l1Origin: RECORDED_TEXT });
    expect(await step.reconcile(undefined)).toEqual({
      status: "complete",
      data: { l1Origin: RECORDED_TEXT },
    });
  });

  it("T-b: accepts only an operator L1_ORIGIN that is the recorded origin", async () => {
    const accepted = await stack({ L1_ORIGIN: RECORDED_TEXT });
    await accepted.signNonce();
    expect(await accepted.step.execute(undefined)).toEqual({
      l1Origin: RECORDED_TEXT,
    });
    expect(derivations(accepted.processes)).toEqual([[NONCE, RECORDED_TEXT]]);

    for (const operator of [`141.${"cd".repeat(32)}`, "not-a-point"]) {
      const refused = await stack({ L1_ORIGIN: operator });
      await refused.signNonce();
      await expect(refused.step.execute(undefined)).rejects.toThrow(
        L1OriginUndeterminedError,
      );
      expect(derivations(refused.processes)).toEqual([]);
      expect(await refused.step.reconcile(undefined)).toEqual({
        status: "retry",
      });
    }
  });

  it("refuses a recorded origin that the operator's L1_ORIGIN contradicts", async () => {
    const { config, processes, step, signNonce } = await stack();
    await signNonce();
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
    const { step } = await stack({ L1_ORIGIN: RECORDED_TEXT });
    await expect(step.execute(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
    await expect(step.reconcile(undefined)).rejects.toThrow(
      L1OriginUndeterminedError,
    );
  });

  it("reads the restored origin the generated configurations start from", () => {
    expect(runL1Origin({ L1_ORIGIN: RECORDED_TEXT })).toEqual(recorded);
    expect(runL1Origin({})).toBeUndefined();
    expect(() => runL1Origin({ L1_ORIGIN: "141" })).toThrow(
      L1OriginUndeterminedError,
    );
  });
});
