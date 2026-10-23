/** Two cases opened on the same failing block, on the emulator. The case
 * validators let any number of cases stand on one block, each with the
 * block's thread token and its own case token: here a second runner on the
 * same prover key opens one beside ours, once after ours and once before.
 * Our runner still convicts its own case and removes the block and its
 * descendants against its own record while the other case stands, and
 * completes on the authenticated terminal. In the second order the runner
 * restarts after the first descendant is peeled and resumes from its
 * journal. The block's authenticated L1 view reads the later case as the
 * live thread while both stand, and our record, once it exists, as the
 * block's stage; a prover named by neither case reads the block as not
 * started. Once the block is gone, the completed-workflow check reads the
 * same terminal from L1 while the other case still stands. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  type ConfirmedValidationTraceStage,
  confirmedValidationTraceStages,
} from "../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../src/validation-trace-dispute/adapter.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { CURSOR_FAMILY_ACTION } from "../src/workflow/cursor-family-state.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../src/workflow/family-l1-observation.js";
import {
  admitFraudProofRawL1Snapshot,
  deriveFraudProofRawL1CompletedTerminal,
  fraudProofRawL1HistoryReader,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../src/workflow/index.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  type RecordedRemoval,
  recordedRemoval,
} from "./support/validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runnerDeployment,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";

/** The shared remove stage's journaled intents. */
const removalIntents = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionInput.schemaVersion === CURSOR_FAMILY_ACTION
      ? [event]
      : [],
  );

/** Whether `entries` confirm a removal peel. */
const peeled = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.some(
    ({ event }) =>
      event.kind === "confirmed" && event.actionId.startsWith("remove:"),
  );

const entriesOf = (result: FraudProofWorkflowRunResult) => {
  if (!("entries" in result)) throw new Error("no workflow journal");
  return result.entries;
};

/** The case a runner's journal confirms it opened. */
const caseOf = (result: FraudProofWorkflowRunResult) =>
  confirmedValidationTraceStages(entriesOf(result)).find(
    ({ action }) => action.stage === "verify_source",
  )!;

type Run = Readonly<{
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  scene: Scene;
  removal: RecordedRemoval;
  ours: Runner;
  /** A fresh runner of ours on the same journal: a restart. */
  restarted: () => Runner;
  ourCase: ConfirmedValidationTraceStage;
  otherCase: ConfirmedValidationTraceStage;
}>;

/** Our runner and a second runner on the same key each open a case on the
 * failing block, ours first when `oursFirst`. */
const openTwoCases = async (oursFirst: boolean): Promise<Run> => {
  // Before the scene, so the raw L1 view has the whole history.
  const recorder = recordCrossBlockRawEmulator();
  const scene = await setUpScene();
  const removal = await recordedRemoval(scene, recorder);
  const options = {
    removal: removal.removal,
    terminalVerifier: removal.terminalVerifier(
      scene.failing.evidence.headerHash,
    ),
  };
  const ours = makeRunner(scene, options);
  // Its own journal; it never reaches a conviction.
  const other = makeRunner(scene);
  const open = async (runner: Runner) =>
    caseOf(
      (await runUntil(runner, scene.failing, VALIDATION_TRACE_CASE_OPEN))
        .result,
    );
  const first = await open(oursFirst ? ours : other);
  const second = await open(oursFirst ? other : ours);
  return {
    recorder,
    scene,
    removal,
    ours,
    restarted: () => makeRunner(scene, { ...options, journal: ours.journal }),
    ourCase: oursFirst ? first : second,
    otherCase: oursFirst ? second : first,
  };
};

/** A prover key hash neither case names. */
const ANOTHER_PROVER = "ab".repeat(28);

/** The block's stage on the live L1 view, never held, as `proverCredential`
 * sees it; by default as we do. */
const stageAs = async (run: Run, proverCredential?: string) => {
  const { headerHash } = run.scene.failing.evidence;
  const { releaseFinality, releaseEconomics, definition } = run.removal.raw;
  const base = definition(headerHash);
  return (
    await createFraudProofFamilyRawL1ObservationPort({
      authority: run.recorder.authority,
      releaseFinality,
      releaseEconomics,
      definition: {
        ...base,
        proverCredential: proverCredential ?? base.proverCredential,
        category: "validationTraceDispute",
      },
    }).observe({ headerHash })
  ).stage;
};

const liveAt = async (
  scene: Scene,
  outRef: string,
): Promise<UTxO | undefined> =>
  (await scene.h.proverLucid.utxosByOutRef([parseOwnOutRef(outRef)]))[0];

/** Both cases stand on the block, each with the block's thread token, and
 * the block's authenticated L1 view reads one of them, the later, as the
 * block's live thread. */
const expectTwoLiveCases = async (run: Run, oursFirst: boolean) => {
  const { scene, removal, ourCase, otherCase } = run;
  const { headerHash } = scene.failing.evidence;
  const threadUnit = toUnit(
    scene.h.contracts.computationThread.policyId,
    runnerDeployment(scene).init.category.categoryId + headerHash,
  );
  const cases = [ourCase, otherCase].map(({ outputs }) => outputs[0]!.outRef);
  expect(new Set(cases).size).toBe(2);
  for (const outRef of cases) {
    const live = await liveAt(scene, outRef);
    expect(live?.assets[threadUnit]).toBe(1n);
  }
  const seen = await removal.l1For(headerHash).observe({ headerHash });
  expect(seen.stage).toMatchObject({
    kind: "step",
    step: 3,
    threadOutRef: (oursFirst ? otherCase : ourCase).outputs[0]!.outRef,
  });
  expect(await stageAs(run, ANOTHER_PROVER)).toMatchObject({
    kind: "not_started",
  });
};

/** Waits out our case's defense deadline, then runs ours until it
 * completes, restarting once after the first peel when `restart`. When
 * `holdAtConviction`, the removal's view is held with both cases standing
 * until the runner has convicted, and the live view is read then. */
const convictAndRemove = async (
  run: Run,
  {
    restart,
    holdAtConviction,
  }: { readonly restart: boolean; readonly holdAtConviction: boolean },
): Promise<{
  readonly result: FraudProofWorkflowRunResult;
  readonly restartedAfterPeel: boolean;
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
    if (holdAtConviction) {
      const { headerHash } = run.scene.failing.evidence;
      run.removal.holdView();
      await run.removal.l1For(headerHash).observe({ headerHash });
      const { result } = await runUntil(
        run.ours,
        run.scene.failing,
        VALIDATION_TRACE_CASE_CONVICTED,
      );
      const close = confirmedValidationTraceStages(entriesOf(result)).find(
        ({ action }) => action.stage === "close",
      )!;
      // Our record decides our stage while the other case still stands; it
      // is no record of another prover's.
      expect(
        await liveAt(run.scene, run.otherCase.outputs[0]!.outRef),
      ).toBeDefined();
      expect(await stageAs(run)).toMatchObject({
        kind: "proof_token",
        fraudProofOutRef: close.outputs[0]!.outRef,
      });
      expect(await stageAs(run, ANOTHER_PROVER)).toMatchObject({
        kind: "not_started",
      });
      run.removal.releaseView();
    }
    let runner = run.ours;
    let restartedAfterPeel = false;
    const history: string[] = [];
    for (let attempt = 0; attempt < 16; attempt += 1) {
      const outcome = await runner.runOnce(run.scene.failing);
      history.push(
        "reason" in outcome
          ? `${outcome.kind}: ${outcome.reason}`
          : outcome.kind,
      );
      if (outcome.kind === "terminal_included" || outcome.kind === "completed")
        return { result: outcome, restartedAfterPeel };
      if (restart && !restartedAfterPeel && peeled(entriesOf(outcome))) {
        runner = run.restarted();
        restartedAfterPeel = true;
      }
    }
    throw new Error(`the removal did not complete: ${history.join("\n")}`);
  } finally {
    clock.mockRestore();
  }
};

/** Our record, the block's removal against it, and the other case still
 * standing. */
/** The terminal the completed-workflow check reads from the block's
 * authenticated L1 view. */
const completedTerminal = async ({ scene, removal }: Run) => {
  const { authority, releaseFinality, releaseEconomics } = removal.raw;
  const headerHash = scene.failing.evidence.headerHash;
  const definition = removal.raw.definition(headerHash);
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality,
  });
  return await deriveFraudProofRawL1CompletedTerminal({
    snapshot: admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    }),
    definition,
    releaseEconomics,
    history: fraudProofRawL1HistoryReader({
      authority,
      releaseFinality,
      headerHash,
    }),
  });
};

const facts = (value: FraudProofWorkflowTerminal) => ({
  ...value,
  observedAt: { ...value.observedAt, confirmationDepth: 0 },
});

const expectRemovedAgainstOurRecord = async (
  run: Run,
  result: FraudProofWorkflowRunResult,
) => {
  const { scene, ourCase, otherCase } = run;
  if (!("terminal" in result)) throw new Error("no terminal");
  const close = confirmedValidationTraceStages(entriesOf(result)).find(
    ({ action }) => action.stage === "close",
  )!;
  expect(close.action.consumes).toBe(ourCase.outputs[0]!.outRef);
  const record = close.outputs[0]!.outRef;
  const [recorded] = await scene.h.proverLucid.utxosByOutRef([
    parseOwnOutRef(record),
  ]);
  expect(
    Data.from(recorded!.datum!, SDK.FraudProofTokenDatum).fraud_prover,
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
  const intents = removalIntents(entriesOf(result));
  expect(intents).toHaveLength(3);
  for (const intent of intents)
    expect(intent.actionInput).toMatchObject({
      stage: "remove",
      fraudProofOutRef: record,
    });
  // The other case was never touched, and the completed-workflow check
  // reads our terminal beside it.
  expect(await liveAt(scene, otherCase.outputs[0]!.outRef)).toBeDefined();
  const completed = await completedTerminal(run);
  expect(completed).not.toBeNull();
  expect(facts(completed!)).toStrictEqual(facts(result.terminal));
};

describe("validation-trace dispute runner: a second case on the block, opened after ours", () => {
  let run: Run;

  beforeAll(async () => {
    run = await openTwoCases(true);
  }, 1_800_000);

  afterAll(() => run?.recorder.restore());

  it("convicts our case and removes the block against our record while the other case stands", async () => {
    await expectTwoLiveCases(run, true);
    const { result } = await convictAndRemove(run, {
      restart: false,
      holdAtConviction: true,
    });
    await expectRemovedAgainstOurRecord(run, result);
  }, 900_000);
});

describe("validation-trace dispute runner: a second case on the block, opened before ours, with a restart mid-removal", () => {
  let run: Run;

  beforeAll(async () => {
    run = await openTwoCases(false);
  }, 1_800_000);

  afterAll(() => run?.recorder.restore());

  it("convicts our case and removes the block against our record while the other case stands, resuming from the journal after a restart", async () => {
    await expectTwoLiveCases(run, false);
    const { result, restartedAfterPeel } = await convictAndRemove(run, {
      restart: true,
      holdAtConviction: false,
    });
    expect(restartedAfterPeel).toBe(true);
    await expectRemovedAgainstOurRecord(run, result);
  }, 900_000);
});
