/**
 * Walk planning over the script-decode step machine port: which frames each
 * Walk reveals, the root below them, and the successor run it writes.
 */
import {
  initialScriptDecodeRun,
  isScriptDecodeTerminal,
  scriptDecodeEnv,
  scriptDecodeRecordOf,
  scriptDecodeScriptHash,
  scriptDecodeStep,
  scriptDecodeStepWithFrames,
} from "./script-decode.step.js";
import { serialiseScriptDecodeData } from "./script-decode.step-data.js";
import { machineData } from "./script-decode.step-encode.js";
import { EMPTY_STACK_ROOT, stackRootOf } from "./script-decode.step-exec.js";
import type {
  Frame,
  Machine,
  ScriptDecodeRecord,
  ScriptDecodeRun,
} from "./script-decode.step-types.js";

type DecodeEnv = ReturnType<typeof scriptDecodeEnv>;

export type ScriptDecodeWalkStep = {
  readonly fuel: number;
  /** The frames the Walk reveals, top first. */
  readonly reveals: readonly Frame[];
  /** The root of the stack below `reveals`. */
  readonly base: Buffer;
  /** The run the Walk spends. */
  readonly before: ScriptDecodeRun;
  /** The successor run the Walk writes. */
  readonly after: ScriptDecodeRun;
};

export type ScriptDecodeWalk = {
  readonly run: ScriptDecodeRun;
  readonly steps: readonly ScriptDecodeWalkStep[];
  readonly record: ScriptDecodeRecord;
};

/** A walk in progress: the current run and its whole stack, top first. */
export type ScriptDecodeWalkCursor = {
  readonly env: DecodeEnv;
  readonly run: ScriptDecodeRun;
  readonly frames: readonly Frame[];
};

const requireFuel = (fuel: number): number => {
  if (!Number.isSafeInteger(fuel) || fuel <= 0) {
    throw new RangeError("script decode: fuel must be a positive integer");
  }
  return fuel;
};

const sameMachine = (left: Machine, right: Machine): boolean =>
  serialiseScriptDecodeData(machineData(left)).equals(
    serialiseScriptDecodeData(machineData(right)),
  );

/** The cursor at the run a Bind opens for `scriptBytes`. */
export const startScriptDecodeWalk = (
  language: number,
  scriptBytes: Uint8Array,
): ScriptDecodeWalkCursor => {
  const run = initialScriptDecodeRun(
    scriptDecodeScriptHash(language, scriptBytes),
    language,
    scriptBytes,
  );
  return { env: scriptDecodeEnv(scriptBytes, run), run, frames: [] };
};

/** The record of a cursor's run, or undefined while it runs. */
export const scriptDecodeWalkRecord = (
  cursor: ScriptDecodeWalkCursor,
): ScriptDecodeRecord | undefined =>
  isScriptDecodeTerminal(cursor.run.machine.control)
    ? scriptDecodeRecordOf(cursor.run)
    : undefined;

/**
 * The next Walk from `cursor`: up to `fuel` ticks, revealing exactly the
 * frames it pops.
 */
export const nextScriptDecodeWalkStep = (
  cursor: ScriptDecodeWalkCursor,
  fuel: number,
): {
  readonly step: ScriptDecodeWalkStep;
  readonly next: ScriptDecodeWalkCursor;
} => {
  requireFuel(fuel);
  const { env, run, frames } = cursor;
  if (isScriptDecodeTerminal(run.machine.control)) {
    throw new Error("script decode: the run is finished");
  }
  const probe = scriptDecodeStepWithFrames(
    env,
    run.machine,
    frames,
    frames.length,
    fuel,
  );
  const visible = probe.consumed;
  const reveals = frames.slice(0, visible);
  const base = stackRootOf(frames.slice(visible), EMPTY_STACK_ROOT);
  const machine = scriptDecodeStep(env, run.machine, reveals, base, fuel);
  if (!machine.stackRoot.equals(probe.machine.stackRoot)) {
    throw new Error("script decode: minimal reveal changed the step");
  }
  if (sameMachine(machine, run.machine)) {
    throw new Error("script decode: a step made no progress");
  }
  const after = { ...run, machine };
  return {
    step: { fuel, reveals, base, before: run, after },
    next: { env, run: after, frames: probe.frames },
  };
};

/**
 * Plans a whole decode walk: each Walk takes `fuel` ticks (fewer on the last)
 * and reveals exactly the frames it pops.
 */
export const planScriptDecodeWalk = (
  language: number,
  scriptBytes: Uint8Array,
  fuel: number,
): ScriptDecodeWalk => {
  requireFuel(fuel);
  const initial = startScriptDecodeWalk(language, scriptBytes);
  const steps: ScriptDecodeWalkStep[] = [];
  let cursor = initial;
  while (!isScriptDecodeTerminal(cursor.run.machine.control)) {
    const { step, next } = nextScriptDecodeWalkStep(cursor, fuel);
    steps.push(step);
    cursor = next;
  }
  const record = scriptDecodeWalkRecord(cursor);
  if (record === undefined) throw new Error("script decode: unfinished run");
  return { run: initial.run, steps, record };
};

/** The record a whole walk of `scriptBytes` ends in. */
export const scriptDecodeRecordOfScript = (
  language: number,
  scriptBytes: Uint8Array,
): ScriptDecodeRecord => {
  const start = startScriptDecodeWalk(language, scriptBytes);
  const done = isScriptDecodeTerminal(start.run.machine.control)
    ? start.run
    : {
        ...start.run,
        machine: scriptDecodeStepWithFrames(
          start.env,
          start.run.machine,
          [],
          0,
          Number.MAX_SAFE_INTEGER,
        ).machine,
      };
  const record = scriptDecodeRecordOf(done);
  if (record === undefined) throw new Error("script decode: unfinished run");
  return record;
};
