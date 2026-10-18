/**
 * A TypeScript port of the script-decode step machine
 * (`onchain/aiken/lib/midgard/script-decode-v1.ak`) that builds the witnesses
 * of a decode walk: the run each Bind opens, and for each Walk its fuel, the
 * frames it reveals, the root below them and the successor run.
 *
 * It mirrors `initial_run_v1`, `step_with_frames_v1` and `record_of_v1`
 * exactly, because every Walk must reproduce the on-chain successor datum.
 * Its record is the off-chain verdict on a script: `Decoded` or `Refused`,
 * exactly as Cardano decodes it. The terminal record is also the off-chain
 * admission classifier; graph material is derived separately from accepted
 * stored bytes.
 */
import { nextSetBit } from "./script-decode.step-bits.js";
import { tickBytes, tickFiller } from "./script-decode.step-bytes.js";
import { tickData } from "./script-decode.step-cbor.js";
import { tickValue, tickValueReturn } from "./script-decode.step-const.js";
import { blake2b224 } from "./script-decode.step-data.js";
import {
  at,
  type DecodeEnv,
  EMPTY_STACK_ROOT,
  type Exec,
  pushStackRoot,
  refuse,
  ScriptDecodeClass,
  stackRootOf,
  type Tick,
  tickConsBit,
  tickReturn,
  tickTerm,
  tickTermFold,
} from "./script-decode.step-exec.js";
import {
  tickAssetInnerBit,
  tickAssetOuterBit,
  tickConstFold,
  tickListBit,
} from "./script-decode.step-list.js";
import { tickMagnitude, tickNat } from "./script-decode.step-nat.js";
import { tickTypes } from "./script-decode.step-type-tags.js";
import type {
  Control,
  Frame,
  Machine,
  ScriptDecodeRecord,
  ScriptDecodeRun,
} from "./script-decode.step-types.js";

export {
  type ScriptDecodeData,
  serialiseScriptDecodeData,
} from "./script-decode.step-data.js";
export {
  frameData as scriptDecodeFrameData,
  machineData as scriptDecodeMachineData,
  recordData as scriptDecodeRecordData,
  runData as scriptDecodeRunData,
} from "./script-decode.step-encode.js";
export {
  EMPTY_STACK_ROOT as SCRIPT_DECODE_EMPTY_STACK_ROOT,
  ScriptDecodeClass,
  stackRootOf as scriptDecodeStackRootOf,
} from "./script-decode.step-exec.js";
export type {
  Frame as ScriptDecodeFrame,
  Machine as ScriptDecodeMachine,
  ScriptDecodePhase2Class,
  ScriptDecodeRecord,
  ScriptDecodeRun,
} from "./script-decode.step-types.js";

export const SCRIPT_DECODE_LANGUAGE_PLUTUS_V3 = 3;
export const SCRIPT_DECODE_LANGUAGE_MIDGARD_V1 = 128;
export const SCRIPT_DECODE_PHASE2_NONE = 0;

/** The script hash a run is opened for: `blake2b_224(language ‖ bytes)`. */
export const scriptDecodeScriptHash = (
  language: number,
  scriptBytes: Uint8Array,
): Buffer => blake2b224(Buffer.concat([Buffer.from([language]), scriptBytes]));

type Wrapper =
  | {
      readonly k: "wrapped";
      readonly flatStart: number;
      readonly flatLength: number;
    }
  | { readonly k: "notBytes" };

/**
 * Cardano reads the script as one definite CBOR byte string (a non-minimal
 * length head is accepted).
 */
export const parseScriptDecodeWrapper = (bytes: Uint8Array): Wrapper => {
  const length = bytes.length;
  if (length === 0) return { k: "notBytes" };
  const initial = bytes[0]!;
  const additional = initial % 32;
  if (Math.floor(initial / 32) !== 2 || additional > 27)
    return { k: "notBytes" };
  const width =
    additional < 24
      ? 0
      : additional === 24
        ? 1
        : additional === 25
          ? 2
          : additional === 26
            ? 4
            : 8;
  if (1 + width > length) return { k: "notBytes" };
  let flatLength = BigInt(additional);
  if (width > 0) {
    flatLength = 0n;
    for (let index = 1; index <= width; index += 1) {
      flatLength = flatLength * 256n + BigInt(bytes[index]!);
    }
  }
  if (BigInt(1 + width) + flatLength > BigInt(length)) return { k: "notBytes" };
  return { k: "wrapped", flatStart: 1 + width, flatLength: Number(flatLength) };
};

export const initialScriptDecodeMachine = (): Machine => ({
  cursor: 0,
  version: 0,
  binders: 0,
  constrCaseDepth: 0,
  openTerm: false,
  stackRoot: EMPTY_STACK_ROOT,
  control: {
    k: "nat",
    purpose: { k: "version", index: 0 },
    start: 0,
    groups: 0,
    acc: 0n,
    scale: 1n,
    big: false,
  },
});

/** The run a Bind opens for `scriptBytes`. */
export const initialScriptDecodeRun = (
  scriptHash: Uint8Array,
  language: number,
  scriptBytes: Uint8Array,
): ScriptDecodeRun => {
  const wrapper = parseScriptDecodeWrapper(scriptBytes);
  const base = {
    scriptHash: Buffer.from(scriptHash),
    language,
    byteLength: scriptBytes.length,
  };
  if (wrapper.k === "wrapped") {
    return {
      ...base,
      flatStart: wrapper.flatStart,
      flatLength: wrapper.flatLength,
      machine: initialScriptDecodeMachine(),
    };
  }
  return {
    ...base,
    flatStart: 0,
    flatLength: 0,
    machine: {
      ...initialScriptDecodeMachine(),
      control: {
        k: "refused",
        class: ScriptDecodeClass.cborNotBytes,
        offset: 0,
      },
    },
  };
};

export const scriptDecodeEnv = (
  scriptBytes: Uint8Array,
  run: ScriptDecodeRun,
): DecodeEnv => ({
  window: Buffer.concat([
    Buffer.from(
      scriptBytes.subarray(run.flatStart, run.flatStart + run.flatLength),
    ),
    Buffer.alloc(1),
  ]),
  flatLength: run.flatLength,
  flatBits: 8 * run.flatLength,
  baseBits: 8 * run.flatStart,
  trailing: run.flatStart + run.flatLength < run.byteLength,
});

export const isScriptDecodeTerminal = (control: Control): boolean =>
  control.k === "done" || control.k === "refused";

/** The record of a finished run, or undefined while it runs. */
export const scriptDecodeRecordOf = (
  run: ScriptDecodeRun,
): ScriptDecodeRecord | undefined => {
  const { machine } = run;
  const control = machine.control;
  if (control.k === "done") {
    const major = Math.floor(machine.version / 9);
    const minor = Math.floor(machine.version / 3) % 3;
    const patch = machine.version % 3;
    const available = major === 1 && minor <= 1 && patch === 0;
    return {
      k: "decoded",
      scriptHash: run.scriptHash,
      language: run.language,
      programVersion: available ? minor : -1,
      termRoot: control.termRoot,
      phase2Class: !available
        ? ScriptDecodeClass.versionUnavailable
        : machine.openTerm
          ? ScriptDecodeClass.openTerm
          : SCRIPT_DECODE_PHASE2_NONE,
    };
  }
  if (control.k === "refused") {
    return {
      k: "refused",
      scriptHash: run.scriptHash,
      language: run.language,
      class: control.class,
      bitOffset: control.offset,
    };
  }
  return undefined;
};

// ## Step

const tickFinal = (env: DecodeEnv, ex: Exec, termRoot: Buffer): Tick => {
  const q = nextSetBit(env.window, ex.cursor, env.flatLength);
  if (q === undefined) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, env.flatBits);
  }
  if (q + 1 !== env.flatBits) {
    return refuse(env, ex, ScriptDecodeClass.flatBadPadding, q + 1);
  }
  if (env.trailing) {
    return refuse(env, ex, ScriptDecodeClass.cborTrailingBytes, env.flatBits);
  }
  return at(ex, env.flatBits, { k: "done", termRoot });
};

const tick = (env: DecodeEnv, ex: Exec): Tick => {
  const control = ex.control;
  switch (control.k) {
    case "term":
      return tickTerm(env, ex);
    case "return":
      return tickReturn(ex, control.hash);
    case "consBit":
      return tickConsBit(env, ex);
    case "constrFold":
    case "caseFold":
      return tickTermFold(ex, control);
    case "nat":
      return tickNat(env, ex, control);
    case "magnitude":
      return tickMagnitude(env, ex, control);
    case "types":
      return tickTypes(env, ex, control);
    case "value":
      return tickValue(env, ex, control.ty);
    case "valueReturn":
      return tickValueReturn(env, ex, control.summary, control.memory);
    case "listBit":
      return tickListBit(env, ex, control.element);
    case "assetOuterBit":
      return tickAssetOuterBit(env, ex);
    case "assetInnerBit":
      return tickAssetInnerBit(env, ex);
    case "filler":
      return tickFiller(env, ex, control.purpose);
    case "bytes":
      return tickBytes(env, ex, control.purpose, control.acc);
    case "constFold":
      return tickConstFold(env, ex, control);
    case "data":
      return tickData(env, ex, control.source, control.mode);
    case "final":
      return tickFinal(env, ex, control.termRoot);
    case "done":
    case "refused":
      return undefined;
  }
};

const runTicks = (env: DecodeEnv, start: Exec, fuel: number): Exec => {
  let ex = start;
  for (
    let left = fuel;
    left > 0 && !isScriptDecodeTerminal(ex.control);
    left -= 1
  ) {
    const next = tick(env, ex);
    if (next === undefined) break;
    ex = next;
  }
  return ex;
};

/** The frames `reveals` (top first) opened over `base`, and their root. */
const openReveals = (
  reveals: readonly Frame[],
  base: Uint8Array,
): readonly [(readonly [Frame, Buffer])[], Buffer] => {
  const opened: (readonly [Frame, Buffer])[] = [];
  let root = Buffer.from(base);
  for (let index = reveals.length - 1; index >= 0; index -= 1) {
    const frame = reveals[index]!;
    opened.unshift([frame, root]);
    root = pushStackRoot(frame, root);
  }
  return [opened, root];
};

const startExec = (
  machine: Machine,
  revealed: readonly (readonly [Frame, Buffer])[],
): Exec => ({
  cursor: machine.cursor,
  version: machine.version,
  binders: machine.binders,
  depth: machine.constrCaseDepth,
  open: machine.openTerm,
  mem: null,
  revealed,
  revealedAt: 0,
  hashed: machine.stackRoot,
  control: machine.control,
});

const memFrames = (ex: Exec): Frame[] => {
  const frames: Frame[] = [];
  for (let node = ex.mem; node !== null; node = node.rest)
    frames.push(node.frame);
  return frames;
};

const machineOf = (done: Exec): Machine => ({
  cursor: done.cursor,
  version: done.version,
  binders: done.binders,
  constrCaseDepth: done.depth,
  openTerm: done.open,
  stackRoot: stackRootOf(memFrames(done), done.hashed),
  control: done.control,
});

const runStep = (
  env: DecodeEnv,
  machine: Machine,
  reveals: readonly Frame[],
  base: Uint8Array,
  fuel: number,
): Exec => {
  const [revealed, root] = openReveals(reveals, base);
  if (!root.equals(machine.stackRoot)) {
    throw new Error(
      "script decode: revealed frames do not open the stack root",
    );
  }
  return runTicks(env, startExec(machine, revealed), fuel);
};

/** `step_v1`: the machine a Walk with these witnesses reaches. */
export const scriptDecodeStep = (
  env: DecodeEnv,
  machine: Machine,
  reveals: readonly Frame[],
  base: Uint8Array,
  fuel: number,
): Machine => machineOf(runStep(env, machine, reveals, base, fuel));

export type ScriptDecodeStepResult = {
  readonly machine: Machine;
  /** The whole stack after the step, top first. */
  readonly frames: readonly Frame[];
  /** How many of the revealed frames the step popped. */
  readonly consumed: number;
};

/**
 * `step_with_frames_v1`: the whole stack is `frames` (top first) and the step
 * reveals its top `visible` frames.
 */
export const scriptDecodeStepWithFrames = (
  env: DecodeEnv,
  machine: Machine,
  frames: readonly Frame[],
  visible: number,
  fuel: number,
): ScriptDecodeStepResult => {
  const hidden = frames.slice(visible);
  const done = runStep(
    env,
    machine,
    frames.slice(0, visible),
    stackRootOf(hidden, EMPTY_STACK_ROOT),
    fuel,
  );
  return {
    machine: machineOf(done),
    frames: [
      ...memFrames(done),
      ...done.revealed.slice(done.revealedAt).map(([frame]) => frame),
      ...hidden,
    ],
    consumed: done.revealedAt,
  };
};
