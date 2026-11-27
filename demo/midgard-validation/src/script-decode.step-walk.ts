/**
 * Walk planning over the script-decode step machine port: which frames each
 * Walk reveals, the root below them, and the successor run it writes; and
 * how a Walk's ticks split into segments, one per run of a class, each
 * stepped by that class's staker.
 */
import {
  initialScriptDecodeRun,
  isScriptDecodeTerminal,
  scriptDecodeClassStep,
  scriptDecodeControlClass,
  scriptDecodeEnv,
  scriptDecodeRecordOf,
  scriptDecodeScriptHash,
  type ScriptDecodeStakerClass,
  scriptDecodeStep,
  scriptDecodeStepWithFrames,
} from "./script-decode.step.js";
import {
  blake2b256,
  serialiseScriptDecodeData,
} from "./script-decode.step-data.js";
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

/** `machine_hash_v1`: the hash a segment's link passes for a machine. */
export const scriptDecodeMachineHash = (machine: Machine): Buffer =>
  blake2b256(serialiseScriptDecodeData(machineData(machine)));

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
 * The next Walk from `cursor`: up to `fuel` ticks, revealing the fewest top
 * frames that reach the machine a step over the whole stack reaches.
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
  // A tick may open a frame it then leaves on the stack (a CBOR break under a
  // frame it does not close), so the frames the step pops are not always
  // enough: reveal one more until the step reaches the probe's machine.
  const stepWith = (visible: number) => {
    const reveals = frames.slice(0, visible);
    const base = stackRootOf(frames.slice(visible), EMPTY_STACK_ROOT);
    const machine = scriptDecodeStep(env, run.machine, reveals, base, fuel);
    return { reveals, base, machine };
  };
  let visible = probe.consumed;
  let { reveals, base, machine } = stepWith(visible);
  while (!sameMachine(machine, probe.machine) && visible < frames.length) {
    visible += 1;
    ({ reveals, base, machine } = stepWith(visible));
  }
  if (!sameMachine(machine, probe.machine)) {
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
 * and reveals the fewest top frames that reach the same machine.
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

/** One segment of a Walk: `fuel` ticks of one class from `machine`. */
export type ScriptDecodeWalkSegment = {
  /** The class whose staker steps the segment. */
  readonly within: ScriptDecodeStakerClass;
  readonly fuel: number;
  /** The machine the segment starts from, carried in full. */
  readonly machine: Machine;
  /** The frames the segment reveals, top first. */
  readonly reveals: readonly Frame[];
  /** The root of the stack below `reveals`. */
  readonly base: Buffer;
  /** The machine the segment reaches. */
  readonly reached: Machine;
};

/** A Walk as a chain of segments from `before`'s machine to `after`'s. */
export type ScriptDecodeChainedWalk = {
  /** The ticks of all segments. */
  readonly fuel: number;
  /** The segments in chain order; position i is `segments[i]`. */
  readonly segments: readonly ScriptDecodeWalkSegment[];
  readonly before: ScriptDecodeRun;
  readonly after: ScriptDecodeRun;
};

/**
 * The longest segment of the machine's class from `machine` over `frames`,
 * up to `fuel` ticks, revealing the fewest top frames that reach it; or
 * undefined when the machine is terminal or no tick of its class moves it.
 */
const nextSegment = (
  env: DecodeEnv,
  machine: Machine,
  frames: readonly Frame[],
  fuel: number,
):
  | {
      readonly segment: ScriptDecodeWalkSegment;
      readonly frames: readonly Frame[];
    }
  | undefined => {
  const within = scriptDecodeControlClass(machine.control);
  if (within === undefined) return undefined;
  const probe = scriptDecodeStepWithFrames(
    env,
    machine,
    frames,
    frames.length,
    fuel,
    within,
  );
  if (probe.ticks === 0) return undefined;
  const stepWith = (visible: number) => {
    const reveals = frames.slice(0, visible);
    const base = stackRootOf(frames.slice(visible), EMPTY_STACK_ROOT);
    const reached = scriptDecodeClassStep(
      env,
      within,
      machine,
      reveals,
      base,
      probe.ticks,
    );
    return { reveals, base, reached };
  };
  // As in `nextScriptDecodeWalkStep`: the popped frames may not be enough.
  let visible = probe.consumed;
  let { reveals, base, reached } = stepWith(visible);
  while (!sameMachine(reached, probe.machine) && visible < frames.length) {
    visible += 1;
    ({ reveals, base, reached } = stepWith(visible));
  }
  if (!sameMachine(reached, probe.machine)) {
    throw new Error("script decode: minimal reveal changed the segment");
  }
  return {
    segment: { within, fuel: probe.ticks, machine, reveals, base, reached },
    frames: probe.frames,
  };
};

/**
 * The next Walk from `cursor` as a chain of class segments: up to `fuel`
 * ticks in all, a new segment at each change of class, and each segment
 * revealing the fewest top frames that reach the machine a step over the
 * whole stack reaches.
 */
export const nextScriptDecodeChainedWalk = (
  cursor: ScriptDecodeWalkCursor,
  fuel: number,
): {
  readonly walk: ScriptDecodeChainedWalk;
  readonly next: ScriptDecodeWalkCursor;
} => {
  requireFuel(fuel);
  const { env, run } = cursor;
  if (isScriptDecodeTerminal(run.machine.control)) {
    throw new Error("script decode: the run is finished");
  }
  const segments: ScriptDecodeWalkSegment[] = [];
  let machine = run.machine;
  let frames = cursor.frames;
  let left = fuel;
  while (left > 0) {
    const found = nextSegment(env, machine, frames, left);
    if (found === undefined) break;
    segments.push(found.segment);
    left -= found.segment.fuel;
    machine = found.segment.reached;
    frames = found.frames;
  }
  if (segments.length === 0 || sameMachine(machine, run.machine)) {
    throw new Error("script decode: a step made no progress");
  }
  const after = { ...run, machine };
  return {
    walk: { fuel: fuel - left, segments, before: run, after },
    next: { env, run: after, frames },
  };
};
