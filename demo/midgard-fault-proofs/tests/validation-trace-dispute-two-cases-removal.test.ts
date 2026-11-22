/** A case on our prover key opened by an earlier process, on the emulator.
 * On the chain our key is our identity: a later process of ours opens no
 * second case beside it and plays that case as its own, convicting it and
 * removing the block and its descendants against the record its close
 * mints, restarting after the first descendant is peeled and continuing
 * from the chain in the fresh process. The block's authenticated L1 view
 * reads the case as the block's live thread while it stands. Once the block
 * is gone, the completed-workflow check reads the same terminal from L1. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk/da";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowTerminal } from "../src/workflow/identity.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init/queue-assertions.js";
import { confirmedValidationTraceStages } from "./support/validation-trace-confirmed-stages.js";
import { completedTerminal } from "./support/validation-trace-dispute-completed-terminal.js";
import type { Scene } from "./support/validation-trace-dispute-open-scene.js";
import type { Runner } from "./support/validation-trace-dispute-runner.js";
import {
  openByTheOtherRun,
  type Run,
} from "./support/validation-trace-dispute-two-cases.js";

/** The shared remove stage's transactions `runners` sent, in order. */
const removalSends = (runners: readonly Runner[]) =>
  runners.flatMap((runner) =>
    runner.process
      .sent()
      .filter(({ actionId }) => actionId.startsWith("remove:")),
  );

/** Whether a removal `runner` sent is confirmed: a peel. */
const peeled = async (scene: Scene, runner: Runner) => {
  for (const { txHash } of removalSends([runner]))
    if (
      (await scene.h.emulator.getTransactionStatus(txHash)).status ===
      "confirmed"
    )
      return true;
  return false;
};

const liveAt = async (
  scene: Scene,
  outRef: string,
): Promise<UTxO | undefined> =>
  (await scene.h.proverLucid.utxosByOutRef([parseOutRefLabel(outRef)]))[0];

/** Waits out our case's defense deadline, then runs ours until it
 * completes, restarting once in a fresh process after the first peel;
 * returns the result and every runner that ran. */
const convictAndRemove = async (
  run: Run,
): Promise<{
  readonly result: FraudProofWorkflowRunResult;
  readonly restartedAfterPeel: boolean;
  readonly runners: readonly Runner[];
}> => {
  const { h } = run.scene;
  const lucid = h.proverLucid;
  const caseUtxo = (await liveAt(run.scene, run.ourCase.outputs[0]!.outRef))!;
  const deadline = validationTraceDefenseDeadlineMs(
    Data.from(caseUtxo.datum!, SDK.Case),
  );
  h.emulator.awaitSlot(
    lucid.unixTimeToSlot(deadline) - lucid.currentSlot() + 1,
  );
  // The removal dates its validity window from the application clock,
  // which on the emulator is the emulator's.
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  try {
    const runners = [run.ours];
    let restartedAfterPeel = false;
    const history: string[] = [];
    for (let attempt = 0; attempt < 16; attempt += 1) {
      const outcome = await runners.at(-1)!.runOnce(run.scene.failing);
      history.push(
        "reason" in outcome
          ? `${outcome.kind}: ${outcome.reason}`
          : outcome.kind,
      );
      if (outcome.kind === "terminal_included" || outcome.kind === "completed")
        return { result: outcome, restartedAfterPeel, runners };
      if (!restartedAfterPeel && (await peeled(run.scene, runners.at(-1)!))) {
        runners.push(run.restarted());
        restartedAfterPeel = true;
      }
    }
    throw new Error(`the removal did not complete: ${history.join("\n")}`);
  } finally {
    clock.mockRestore();
  }
};

const facts = (value: FraudProofWorkflowTerminal) => ({
  ...value,
  observedAt: { ...value.observedAt, confirmationDepth: 0 },
});

/** Our record, and the block's removal against it. */
const expectRemovedAgainstOurRecord = async (
  run: Run,
  result: FraudProofWorkflowRunResult,
  runners: readonly Runner[],
) => {
  const { scene, ourCase } = run;
  if (!("terminal" in result)) throw new Error("no terminal");
  const close = (
    await confirmedValidationTraceStages({
      emulator: scene.h.emulator,
      deployment: runners[0]!.deployment,
      sent: runners.flatMap((runner) => runner.process.sent()),
    })
  ).find(({ action }) => action.stage === "close")!;
  expect(close.action.consumes).toBe(ourCase.outputs[0]!.outRef);
  const record = close.outputs[0]!.outRef;
  const [recorded] = await scene.h.proverLucid.utxosByOutRef([
    parseOutRefLabel(record),
  ]);
  expect(
    Data.from(recorded!.datum!, SDKFraudProof.FraudProofTokenDatum)
      .fraud_prover,
  ).toBe(scene.h.proverSigner.paymentKeyHash);
  expect(result.terminal.headerHash).toBe(scene.failing.evidence.headerHash);
  expect(result.terminal.correction.referencedProofTokenOutRef).toBe(record);
  await expectStateQueueHeaderOrder({
    lucid: scene.h.proverLucid,
    contracts: scene.h.contracts,
    expectedHeaderHashes: [scene.failing.evidence.header.prevHeaderHash],
  });
  // Each removal peels one block, the two descendants first, each against
  // our record.
  const sends = removalSends(runners);
  expect(sends).toHaveLength(3);
  for (const { actionId } of sends) expect(actionId.split(":")[2]).toBe(record);
  const completed = await completedTerminal(run);
  expect(completed.kind).toBe("removed");
  expect(
    facts((completed as { terminal: FraudProofWorkflowTerminal }).terminal),
  ).toStrictEqual(facts(result.terminal));
};

describe("validation-trace dispute runner: a case on our key opened by an earlier process, with a restart mid-removal", () => {
  let run: Run;

  beforeAll(async () => {
    run = await openByTheOtherRun();
  }, 1_800_000);

  afterAll(() => run?.recorder.restore());

  it("opens no second case, convicts that case as ours and removes the block against the record its close mints, continuing from the chain after a restart", async () => {
    const { scene, removal, ourCase } = run;
    const { headerHash } = scene.failing.evidence;
    expect(await liveAt(scene, ourCase.outputs[0]!.outRef)).toBeDefined();
    expect(
      (await removal.l1For(headerHash).observe({ headerHash })).stage,
    ).toMatchObject({
      kind: "step",
      step: 3,
      threadOutRef: ourCase.outputs[0]!.outRef,
    });
    const { result, restartedAfterPeel, runners } = await convictAndRemove(run);
    expect(restartedAfterPeel).toBe(true);
    await expectRemovedAgainstOurRecord(run, result, runners);
  }, 900_000);
});
