/** Two processes on our prover key over one failing block, for
 * validation-trace-dispute-two-cases-removal.test.ts: an earlier process
 * opens our case, and a later one reads it from the chain as its own. On
 * the chain our key is our identity: every output on it is ours, whichever
 * process made it. */
import { expect } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import type { ConfirmedValidationTraceStage } from "./validation-trace-confirmed-stages.js";
import {
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";
import {
  type RecordedRemoval,
  recordedRemoval,
} from "./validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./validation-trace-dispute-runner.js";

/** The case `runner`'s process opened and the emulator confirmed. */
export const caseOf = async (runner: Runner) =>
  (await runner.confirmed()).find(
    ({ action }) => action.stage === "verify_source",
  )!;

export type Run = Readonly<{
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  scene: Scene;
  removal: RecordedRemoval;
  /** The later process, which reads the case as its own. */
  ours: Runner;
  /** The earlier process on our key, which opened the case. */
  other: Runner;
  /** A fresh process of ours: a restart. */
  restarted: () => Runner;
  ourCase: ConfirmedValidationTraceStage;
}>;

/** An earlier process on our key opens a case on the failing block; then a
 * later process of ours runs until it reads that case open, opening none of
 * its own: the case is ours. */
export const openByTheOtherRun = async (): Promise<Run> => {
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
  const other = makeRunner(scene, options);
  await runUntil(other, scene.failing, VALIDATION_TRACE_CASE_OPEN);
  const opened = await caseOf(other);
  const ours = makeRunner(scene, options);
  await runUntil(ours, scene.failing, VALIDATION_TRACE_CASE_OPEN);
  expect(ours.process.sent()).toStrictEqual([]);
  return {
    recorder,
    scene,
    removal,
    ours,
    other,
    restarted: () => makeRunner(scene, options),
    ourCase: opened,
  };
};
