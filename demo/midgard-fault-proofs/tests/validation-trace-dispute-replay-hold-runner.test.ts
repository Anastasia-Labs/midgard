/** The validation-trace dispute runner over an evaluator that first fails
 * without a verdict, on the emulator: the run is held on that one event and
 * names its readiness reason, nothing is submitted or journaled, the event is
 * not re-executed before its retry time, and once the evaluator recovers the
 * same runner Inits, Opens and VerifySources the claim into a live case on
 * the proven failure. A claim build whose re-execution then fails without a
 * result hands the run back pending under its own readiness state, again
 * submitting and preparing nothing, without an alarm while more than
 * half the case open window is left and as an alarm naming the window's end
 * from then on. Evaluator failures are injected at the validation package,
 * per transaction. */
import { MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  DirectValidationTraceUnavailable,
  RejectCodes,
  ScriptEvaluationIndeterminateError,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_CLAIM_BUILD_HELD } from "../src/validation-trace-dispute/claim-material.js";
import {
  VALIDATION_TRACE_REPLAY_HELD,
  ValidationTraceReplayHeldError,
} from "../src/validation-trace-dispute/replay-hold.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  runnerDeployment,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";

/** Per transaction (canonical CBOR hex): the injected verdict failure, and
 * how often the verdict replay ran; and the claim builds' next re-execution
 * failures. */
const injected = vi.hoisted(() => ({
  faults: new Map<string, Error>(),
  verdictRuns: new Map<string, number>(),
  openFaults: [] as Error[],
}));

vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    replayValidationVerdictEvent: (
      input: Parameters<typeof actual.replayValidationVerdictEvent>[0],
    ) => {
      const key = Buffer.from(input.canonicalTransactionCbor).toString("hex");
      injected.verdictRuns.set(key, (injected.verdictRuns.get(key) ?? 0) + 1);
      const fault = injected.faults.get(key);
      return fault === undefined
        ? actual.replayValidationVerdictEvent(input)
        : Effect.fail(fault);
    },
    openValidationTraceV1: (
      ...input: Parameters<typeof actual.openValidationTraceV1>
    ) => {
      const fault = injected.openFaults.shift();
      return fault === undefined
        ? actual.openValidationTraceV1(...input)
        : Effect.fail(fault);
    },
  };
});

let scene: Scene;

/** The failing block's one transaction, by txId and committed CBOR. */
const failingTransaction = () => {
  const event = scene.failing.evidence.reconstruction.sourceEvents.find(
    (source) => source.phase === "L2Transaction",
  );
  if (event?.phase !== "L2Transaction")
    throw new Error("the failing block commits no transaction");
  return {
    txId: event.entry.txId,
    cbor: Buffer.from(event.entry.fullTransactionCbor).toString("hex"),
  };
};

const rejection = async (run: Promise<FraudProofWorkflowRunResult>) =>
  await run.then(
    (result) => {
      throw new Error(
        `the run completed: ${"reason" in result ? `${result.kind}: ${result.reason}` : result.kind}`,
      );
    },
    (error: unknown) => error,
  );

/** A re-execution failure without a result, for the next claim build. */
const failNextClaimBuild = () => {
  injected.openFaults.push(
    new ScriptEvaluationIndeterminateError(
      Buffer.from(failingTransaction().txId, "hex"),
      "spend: Plutus evaluation stopped without a verdict",
      { cause: new RangeError("Array buffer allocation failed") },
    ),
  );
};

/** The end of the case open window on the failing block, from its
 * state-queue node: `block_time + M/2`. */
const openWindowEnd = async () => {
  const { h } = scene;
  const unit =
    h.contracts.stateQueue.policyId +
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    scene.failing.evidence.headerHash;
  const [node] = await h.proverLucid.utxosAtWithUnit(
    h.contracts.stateQueue.spendingScriptAddress,
    unit,
  );
  const { blockTime } = await Effect.runPromise(
    SDK.validationDisputeChallengedBlock(
      node!,
      h.contracts.stateQueue.policyId,
    ),
  );
  return Number(blockTime) + MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS;
};

/** Half the case open window. */
const HALF_WINDOW = Math.floor(MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS / 2);

describe("validation-trace dispute runner: an evaluator failure holds, then the claim proceeds", () => {
  beforeAll(async () => {
    scene = await setUpScene();
  }, 1_800_000);

  it("holds the failing block while the evaluator has no verdict, then opens the case once it recovers", async () => {
    const { h, chain } = scene;
    const runner = makeRunner(scene);
    const { txId, cbor } = failingTransaction();
    injected.faults.set(
      cbor,
      new ScriptEvaluationIndeterminateError(
        Buffer.from(txId, "hex"),
        "spend: Plutus evaluation stopped without a verdict",
        { cause: new RangeError("Array buffer allocation failed") },
      ),
    );
    const before = injected.verdictRuns.get(cbor) ?? 0;
    const walletBefore = await h.proverLucid.wallet().getUtxos();

    const held = await rejection(runner.runOnce(scene.failing));
    expect(held).toBeInstanceOf(ValidationTraceReplayHeldError);
    expect(held).toMatchObject({
      readiness: VALIDATION_TRACE_REPLAY_HELD,
      category: "validationTraceDispute",
      headerHash: scene.failing.evidence.headerHash,
      holds: [{ txId, attempts: 1 }],
    });
    const { retryAtMs } = held as ValidationTraceReplayHeldError;
    expect(injected.verdictRuns.get(cbor)).toBe(before + 1);

    // Before its retry time the event is not re-executed, even though the
    // evaluator has recovered.
    injected.faults.clear();
    const again = await rejection(runner.runOnce(scene.failing));
    expect(again).toMatchObject({
      retryAtMs,
      holds: [{ txId, attempts: 1 }],
    });
    expect(injected.verdictRuns.get(cbor)).toBe(before + 1);
    // Held runs submit nothing.
    expect(await h.proverLucid.wallet().getUtxos()).toStrictEqual(walletBefore);

    await new Promise((resolve) =>
      setTimeout(resolve, Math.max(0, retryAtMs - Date.now())),
    );
    // The event now decides, but the claim's own re-execution fails without
    // a result: the run is pending under the claim build's state, with
    // nothing prepared or submitted.
    injected.openFaults.push(
      new ScriptEvaluationIndeterminateError(
        Buffer.from(txId, "hex"),
        "spend: Plutus evaluation stopped without a verdict",
        { cause: new RangeError("Array buffer allocation failed") },
      ),
    );
    const pending = await runner.runOnce(scene.failing);
    expect(pending).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: { name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: false },
    });
    expect(injected.openFaults).toStrictEqual([]);
    if (!("entries" in pending)) throw new Error("no workflow journal");
    expect(pending.entries.map(({ event }) => event.kind)).toStrictEqual([
      "started",
    ]);
    expect(await h.proverLucid.wallet().getUtxos()).toStrictEqual(walletBefore);
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const stages = confirmedValidationTraceStages(result.entries);
    expect(stages.map(({ action }) => action.stage)).toStrictEqual([
      "init",
      "open",
      "verify_source",
    ]);
    const [caseClaim] = stages[2]!.outputs;
    const [caseOutput] = await h.proverLucid.utxosByOutRef([
      parseOwnOutRef(caseClaim!.outRef),
    ]);
    expect(caseOutput?.address).toBe(chain.case.spendingScriptAddress);
    const material = await failingMaterial(scene);
    expect(Data.from(caseOutput!.datum!, SDK.Case)).toMatchObject({
      block_header_hash: scene.failing.evidence.headerHash,
      event_key: scene.failing.eventKey,
      challenger_root: material.challengerRoot,
      step_count: material.stepCount,
      bounty_claimant: h.proverSigner.paymentKeyHash,
    });

    // A restarted watcher's resumed run rebuilds the journaled claim to check
    // it; a rebuild that fails without a result holds the run the same way,
    // journaling nothing.
    injected.openFaults.push(
      new ScriptEvaluationIndeterminateError(
        Buffer.from(txId, "hex"),
        "spend: Plutus evaluation stopped without a verdict",
        { cause: new RangeError("Array buffer allocation failed") },
      ),
    );
    const restarted = makeRunner(scene, { journal: runner.journal });
    const resumed = await restarted.runOnce(scene.failing);
    expect(resumed).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: { name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: false },
    });
    expect(injected.openFaults).toStrictEqual([]);
    if (!("entries" in resumed)) throw new Error("no workflow journal");
    expect(resumed.entries).toHaveLength(result.entries.length);

    // The same rebuild once half the case open window or less is left is
    // an alarm.
    const end = await openWindowEnd();
    const late = makeRunner(scene, {
      journal: runner.journal,
      now: () => end - HALF_WINDOW,
    });
    failNextClaimBuild();
    expect(await late.runOnce(scene.failing)).toMatchObject({
      kind: "pending",
      namedState: { name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: true },
    });
    expect(injected.openFaults).toStrictEqual([]);
  });

  it.each([
    { left: HALF_WINDOW + 1, alarm: false },
    { left: HALF_WINDOW, alarm: true },
  ])(
    "holds a claim that does not build with $left ms of the case open window left, alarm $alarm",
    async ({ left, alarm }) => {
      const end = await openWindowEnd();
      const runner = makeRunner(scene, { now: () => end - left });
      failNextClaimBuild();
      const held = await runner.runOnce(scene.failing);
      expect(injected.openFaults).toStrictEqual([]);
      expect(held).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: { name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm },
      });
      if (held.kind !== "pending") throw new Error("the run is not pending");
      expect(held.reason).toMatch(
        alarm
          ? `${VALIDATION_TRACE_CLAIM_BUILD_HELD}: the claim on block ${scene.failing.evidence.headerHash} does not build, and our case must open by ${end.toString()}, the end of the case open window: `
          : new RegExp(
              `^${VALIDATION_TRACE_CLAIM_BUILD_HELD}: (?!the claim)`,
              "u",
            ),
      );
      if (!("entries" in held)) throw new Error("no workflow journal");
      expect(held.entries.map(({ event }) => event.kind)).toStrictEqual([
        "started",
      ]);
    },
  );

  it("decides, never holds, a claim whose replay disagrees once the case open window is at risk", async () => {
    const end = await openWindowEnd();
    const runner = makeRunner(scene, { now: () => end - HALF_WINDOW });
    injected.openFaults.push(
      new DirectValidationTraceUnavailable(RejectCodes.InvalidFieldType),
    );
    const decided = await rejection(runner.runOnce(scene.failing));
    expect(injected.openFaults).toStrictEqual([]);
    expect(decided).toMatchObject({
      message: expect.stringContaining(
        "validation claim: the trace replay disagrees: ",
      ),
    });
  });

  it("alarms on a claim that does not build when the block's open window cannot be read", async () => {
    const deployment = runnerDeployment(scene);
    // The block is looked for where no state-queue node is.
    const runner = makeRunner(scene, {
      deployment: {
        ...deployment,
        stateQueue: {
          ...deployment.stateQueue,
          spendingScriptAddress: scene.chain.case.spendingScriptAddress,
        },
      },
    });
    failNextClaimBuild();
    const held = await runner.runOnce(scene.failing);
    expect(held).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        "does not build, and our case must open by the end of the case open window: ",
      ),
      namedState: { name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: true },
    });
  });
});
