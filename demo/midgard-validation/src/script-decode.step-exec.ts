/**
 * The execution state of one script-decode step and the term ticks, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import {
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
} from "@al-ft/midgard-core";

import { readBits } from "./script-decode.step-bits.js";
import {
  blake2b256,
  serialiseScriptDecodeData,
} from "./script-decode.step-data.js";
import { frameData } from "./script-decode.step-encode.js";
import type { Control, Frame } from "./script-decode.step-types.js";

export const ScriptDecodeClass = Object.freeze({
  cborNotBytes: 1,
  cborTrailingBytes: 2,
  flatTruncated: 3,
  flatBadPadding: 4,
  versionUnavailable: 5,
  unknownTermTag: 6,
  constrCaseBeforeV110: 7,
  unavailableBuiltin: 8,
  constrFieldsOverLimit: 9,
  wordOverflow: 10,
  openTerm: 11,
  unknownConstantType: 12,
  constantTypeTooLarge: 13,
  blsConstant: 14,
  invalidUtf8: 15,
  invalidData: 16,
  nonCanonicalValue: 17,
} as const);

export const MAX_CONSTR_FIELDS = 1024;

// "MidgardScriptDecodeStackV1"
const STACK_DOMAIN = Buffer.from("MidgardScriptDecodeStackV1", "ascii");

export const EMPTY_STACK_ROOT = blake2b256(STACK_DOMAIN);

export const pushStackRoot = (frame: Frame, below: Uint8Array): Buffer =>
  blake2b256(
    Buffer.concat([
      STACK_DOMAIN,
      serialiseScriptDecodeData(frameData(frame)),
      below,
    ]),
  );

/** The root of a stack whose top frames are `frames` (top first) over `base`. */
export const stackRootOf = (
  frames: readonly Frame[],
  base: Uint8Array,
): Buffer => {
  let root = Buffer.from(base);
  for (let index = frames.length - 1; index >= 0; index -= 1) {
    root = pushStackRoot(frames[index]!, root);
  }
  return root;
};

export type DecodeEnv = {
  readonly window: Buffer;
  readonly flatLength: number;
  readonly flatBits: number;
  readonly baseBits: number;
  readonly trailing: boolean;
};

/** Frames pushed during this step, top first. */
export type MemStack = {
  readonly frame: Frame;
  readonly rest: MemStack;
} | null;

export type Exec = {
  readonly cursor: number;
  readonly version: number;
  readonly binders: number;
  readonly depth: number;
  readonly open: boolean;
  readonly mem: MemStack;
  /** Revealed frames, top first, each with the root below it. */
  readonly revealed: readonly (readonly [Frame, Buffer])[];
  readonly revealedAt: number;
  readonly hashed: Buffer;
  readonly control: Control;
};

export type Tick = Exec | undefined;

export const pop = (ex: Exec): readonly [Frame, Exec] | undefined => {
  if (ex.mem !== null) return [ex.mem.frame, { ...ex, mem: ex.mem.rest }];
  const next = ex.revealed[ex.revealedAt];
  if (next === undefined) return undefined;
  const [frame, below] = next;
  return [frame, { ...ex, revealedAt: ex.revealedAt + 1, hashed: below }];
};

export const push = (ex: Exec, frame: Frame): Exec => ({
  ...ex,
  mem: { frame, rest: ex.mem },
});

const stackIsEmpty = (ex: Exec): boolean =>
  ex.mem === null &&
  ex.revealedAt >= ex.revealed.length &&
  ex.hashed.equals(EMPTY_STACK_ROOT);

export const to = (ex: Exec, control: Control): Tick => ({ ...ex, control });

export const at = (ex: Exec, cursor: number, control: Control): Tick => ({
  ...ex,
  cursor,
  control,
});

export const refuse = (
  env: DecodeEnv,
  ex: Exec,
  cls: number,
  position: number,
): Tick => ({
  ...ex,
  control: { k: "refused", class: cls, offset: env.baseBits + position },
});

export const have = (
  env: DecodeEnv,
  position: number,
  width: number,
): boolean => position + width <= env.flatBits;

export const read = (env: DecodeEnv, position: number, width: number): number =>
  readBits(env.window, position, width);

export const byteAt = (env: DecodeEnv, index: number): number =>
  env.window[index]!;

/** An Aiken `expect` on a frame: an honest stack never fails it. */
export const expectFrame = <K extends Frame["k"]>(
  frame: Frame,
  kind: K,
): Extract<Frame, { k: K }> => {
  if (frame.k !== kind) {
    throw new Error(
      `script decode: expected a ${kind} frame, found ${frame.k}`,
    );
  }
  return frame as Extract<Frame, { k: K }>;
};

// ## Terms

const hashTerm = hashMidgardCekTermNode;

type Word64 =
  | { readonly k: "ok"; readonly value: bigint; readonly next: number }
  | { readonly k: "truncated" | "tooLarge"; readonly at: number };

const readWord64 = (env: DecodeEnv, start: number): Word64 => {
  let position = start;
  let acc = 0n;
  let scale = 1n;
  for (let group = 0; ; group += 1) {
    if (!have(env, position, 8)) return { k: "truncated", at: position };
    const v = read(env, position, 8);
    const payload = BigInt(v % 128);
    if (group === 9 && (v >= 128 || payload > 1n)) {
      return { k: "tooLarge", at: position + 8 };
    }
    if (v < 128)
      return { k: "ok", value: acc + payload * scale, next: position + 8 };
    acc += payload * scale;
    scale *= 128n;
    position += 8;
  }
};

const atLeastV110 = (version: number): boolean => {
  const major = Math.floor(version / 9);
  return major === 2 || (major === 1 && Math.floor(version / 3) % 3 >= 1);
};

export const tickTerm = (env: DecodeEnv, ex: Exec): Tick => {
  const cursor = ex.cursor;
  if (!have(env, cursor, 4)) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, cursor);
  }
  const tag = read(env, cursor, 4);
  const c = cursor + 4;
  const word = (then: (value: bigint, next: number) => Tick): Tick => {
    const result = readWord64(env, c);
    if (result.k === "ok") return then(result.value, result.next);
    return refuse(
      env,
      ex,
      result.k === "truncated"
        ? ScriptDecodeClass.flatTruncated
        : ScriptDecodeClass.wordOverflow,
      result.at,
    );
  };
  switch (tag) {
    case 0:
      return word((value, next) => {
        const free = value === 0n || value > BigInt(ex.binders);
        return at({ ...ex, open: ex.open || (ex.depth === 0 && free) }, next, {
          k: "return",
          hash: hashTerm({ kind: "variable", index: value }),
        });
      });
    case 1:
      return at(push(ex, { k: "delay" }), c, { k: "term" });
    case 2:
      return at({ ...push(ex, { k: "lambda" }), binders: ex.binders + 1 }, c, {
        k: "term",
      });
    case 3:
      return at(push(ex, { k: "applyFunction" }), c, { k: "term" });
    case 4:
      return at(ex, c, {
        k: "types",
        count: 0,
        pending: 1,
        top: 0,
        bad: false,
        tags: [],
      });
    case 5:
      return at(push(ex, { k: "force" }), c, { k: "term" });
    case 6:
      return at(ex, c, { k: "return", hash: hashTerm({ kind: "error" }) });
    case 7: {
      if (!have(env, c, 7)) {
        return refuse(env, ex, ScriptDecodeClass.flatTruncated, c);
      }
      const builtinTag = read(env, c, 7);
      if (BigInt(builtinTag) > MIDGARD_CEK_MAX_BUILTIN_TAG) {
        return refuse(env, ex, ScriptDecodeClass.unavailableBuiltin, c + 7);
      }
      return at(ex, c + 7, {
        k: "return",
        hash: hashTerm({ kind: "builtin", tag: BigInt(builtinTag) }),
      });
    }
    case 8:
      if (!atLeastV110(ex.version)) {
        return refuse(env, ex, ScriptDecodeClass.constrCaseBeforeV110, c);
      }
      return word((value, next) =>
        at(
          {
            ...push(ex, { k: "constrFields", tag: value, count: 0 }),
            depth: ex.depth + 1,
          },
          next,
          { k: "consBit" },
        ),
      );
    case 9:
      if (!atLeastV110(ex.version)) {
        return refuse(env, ex, ScriptDecodeClass.constrCaseBeforeV110, c);
      }
      return at(
        { ...push(ex, { k: "caseScrutinee" }), depth: ex.depth + 1 },
        c,
        { k: "term" },
      );
    default:
      return refuse(env, ex, ScriptDecodeClass.unknownTermTag, c);
  }
};

export const tickReturn = (ex: Exec, hash: Buffer): Tick => {
  const popped = pop(ex);
  if (popped === undefined) {
    return stackIsEmpty(ex)
      ? to(ex, { k: "final", termRoot: hash })
      : undefined;
  }
  const [frame, rest] = popped;
  const ret = (next: Exec, termHash: Buffer): Tick =>
    to(next, { k: "return", hash: termHash });
  switch (frame.k) {
    case "delay":
      return ret(rest, hashTerm({ kind: "delay", body: hash }));
    case "lambda":
      return ret(
        { ...rest, binders: rest.binders - 1 },
        hashTerm({ kind: "lambda", body: hash }),
      );
    case "force":
      return ret(rest, hashTerm({ kind: "force", term: hash }));
    case "applyFunction":
      return to(push(rest, { k: "applyArgument", function: hash }), {
        k: "term",
      });
    case "applyArgument":
      return ret(
        rest,
        hashTerm({
          kind: "application",
          function: frame.function,
          argument: hash,
        }),
      );
    case "caseScrutinee":
      return to(push(rest, { k: "caseBranches", scrutinee: hash, count: 0 }), {
        k: "consBit",
      });
    case "constrFields":
      return to(
        push(push(rest, { k: "termChild", hash }), {
          k: "constrFields",
          tag: frame.tag,
          count: frame.count + 1,
        }),
        { k: "consBit" },
      );
    case "caseBranches":
      return to(
        push(push(rest, { k: "termChild", hash }), {
          k: "caseBranches",
          scrutinee: frame.scrutinee,
          count: frame.count + 1,
        }),
        { k: "consBit" },
      );
    case "dataRoot":
    case "termChild":
    case "constant":
    case "valueList":
    case "valueItem":
    case "valuePairFirst":
    case "valuePairSecond":
    case "assetOuter":
    case "assetInner":
    case "pairEntry":
    case "dataList":
    case "dataMap":
    case "valueRun":
      throw new Error("script decode: term return under a non-term frame");
  }
};

export const tickConsBit = (env: DecodeEnv, ex: Exec): Tick => {
  const cursor = ex.cursor;
  if (!have(env, cursor, 1)) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, cursor);
  }
  if (read(env, cursor, 1) === 1) return at(ex, cursor + 1, { k: "term" });
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  const [frame, rest] = popped;
  if (frame.k === "constrFields") {
    if (frame.count > MAX_CONSTR_FIELDS) {
      return refuse(
        env,
        ex,
        ScriptDecodeClass.constrFieldsOverLimit,
        cursor + 1,
      );
    }
    return at(rest, cursor + 1, {
      k: "constrFold",
      tag: frame.tag,
      remaining: frame.count,
      root: Buffer.from(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT),
      length: 0,
    });
  }
  if (frame.k === "caseBranches") {
    return at(rest, cursor + 1, {
      k: "caseFold",
      scrutinee: frame.scrutinee,
      remaining: frame.count,
      root: Buffer.from(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT),
      length: 0,
    });
  }
  throw new Error("script decode: term list bit under a non-list frame");
};

/** One `CConstrFold` or `CCaseFold` tick. */
export const tickTermFold = (
  ex: Exec,
  control: Extract<Control, { k: "constrFold" | "caseFold" }>,
): Tick => {
  if (control.remaining === 0) {
    return to(
      { ...ex, depth: ex.depth - 1 },
      {
        k: "return",
        hash:
          control.k === "constrFold"
            ? hashTerm({
                kind: "constr",
                tag: control.tag,
                termsCount: BigInt(control.length),
                termsRoot: control.root,
              })
            : hashTerm({
                kind: "case",
                scrutinee: control.scrutinee,
                branchesCount: BigInt(control.length),
                branchesRoot: control.root,
              }),
      },
    );
  }
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  const [frame, rest] = popped;
  const child = expectFrame(frame, "termChild");
  return to(rest, {
    ...control,
    remaining: control.remaining - 1,
    root: Buffer.from(
      hashMidgardCekSequenceNode({
        head: child.hash,
        tail: control.root,
        length: BigInt(control.length + 1),
      }),
    ),
    length: control.length + 1,
  });
};
