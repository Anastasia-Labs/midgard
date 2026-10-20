/** The validation-trace dispute runner over an evaluator that first fails
 * without a verdict, on the emulator: the run is held on that one event and
 * names its readiness reason, nothing is submitted or journaled, the event is
 * not re-executed before its retry time, and once the evaluator recovers the
 * same runner Inits, Opens and VerifySources the claim into a live case on
 * the proven failure. Evaluator failures are injected at the validation
 * package, per transaction. */
import * as SDK from "@al-ft/midgard-sdk";
import { ScriptEvaluationIndeterminateError } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
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
  runUntil,
} from "./support/validation-trace-dispute-runner.js";

/** Per transaction (canonical CBOR hex): the injected verdict failure, and
 * how often the verdict replay ran. */
const injected = vi.hoisted(() => ({
  faults: new Map<string, Error>(),
  verdictRuns: new Map<string, number>(),
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
  });
});
