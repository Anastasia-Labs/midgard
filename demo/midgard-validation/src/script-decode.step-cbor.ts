/**
 * The Data constant ticks of the script-decode step machine: the CBOR of one
 * Plutus Data item read in place from a Flat byte string, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  hashMidgardCekBlobChunk,
  midgardCekIntegerMemoryWords,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

import {
  bytesSummary,
  emptySink,
  integerSummary,
  magFinish,
  magNew,
  magPush,
  sinkLength,
  sinkPush,
  sinkRoot,
} from "./script-decode.step-bits.js";
import {
  CBOR_BREAK,
  dataStop,
  type Head,
  headFrom,
  MAX_DATA_BYTES_CHUNK,
  nextChunk,
  readHead,
  skim,
  stepChunks,
  takeByte,
  takeSlice,
} from "./script-decode.step-cbor-read.js";
import { encodePlutusInteger } from "./script-decode.step-data.js";
import {
  type DecodeEnv,
  type Exec,
  pop,
  push,
  type Tick,
  to,
} from "./script-decode.step-exec.js";
import { foldOne } from "./script-decode.step-list.js";
import type {
  DataMode,
  SequenceSummary,
  Source,
  Summary,
} from "./script-decode.step-types.js";

const constrSummary = (
  constructor: bigint,
  fields: SequenceSummary,
): Summary => {
  if (constructor < 0n) return summarizeMidgardCekListData(fields);
  if (constructor <= 127n) {
    return summarizeMidgardCekSmallConstrData(constructor, fields);
  }
  const encoded = encodePlutusInteger(constructor);
  return summarizeMidgardCekLargeConstrData({
    constructorCborRoot: hashMidgardCekBlobChunk(encoded),
    constructorCborLength: BigInt(encoded.length),
    constructorMemory: 4n + midgardCekIntegerMemoryWords(constructor),
    fields,
  });
};

/** Hands a finished item to the frame below it. */
const deliver = (
  env: DecodeEnv,
  ex: Exec,
  source: Source,
  summary: Summary,
): Tick => {
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  const [frame, rest] = popped;
  const item = (next: Exec): Tick =>
    to(next, { k: "data", source, mode: { k: "item" } });
  switch (frame.k) {
    case "dataRoot":
      return stepChunks(env, rest, source.position + source.left, summary);
    case "dataList": {
      const pushed = push(rest, { k: "valueItem", summary });
      if (frame.remaining === BigInt(frame.count + 1)) {
        return to(pushed, {
          k: "data",
          source,
          mode: {
            k: "fold",
            fold: {
              pairs: false,
              remaining: frame.count + 1,
              sequence: emptyMidgardCekDataListSummary(),
            },
            next: {
              k: "list",
              constr: frame.constr,
              breakAfter: frame.breakAfter,
            },
          },
        });
      }
      return item(push(pushed, { ...frame, count: frame.count + 1 }));
    }
    case "dataMap": {
      if (frame.key === undefined) {
        return item(push(rest, { ...frame, key: summary }));
      }
      const pushed = push(rest, {
        k: "pairEntry",
        key: frame.key,
        value: summary,
      });
      if (frame.remaining === BigInt(frame.count + 1)) {
        return to(pushed, {
          k: "data",
          source,
          mode: {
            k: "fold",
            fold: {
              pairs: true,
              remaining: frame.count + 1,
              sequence: emptyMidgardCekDataPairSummary(),
            },
            next: { k: "map" },
          },
        });
      }
      return item(
        push(pushed, { ...frame, count: frame.count + 1, key: undefined }),
      );
    }
    case "delay":
    case "lambda":
    case "force":
    case "applyFunction":
    case "caseScrutinee":
    case "applyArgument":
    case "constrFields":
    case "caseBranches":
    case "termChild":
    case "constant":
    case "valueList":
    case "valueItem":
    case "valuePairFirst":
    case "valuePairSecond":
    case "assetOuter":
    case "assetInner":
    case "pairEntry":
    case "valueRun":
      throw new Error("script decode: data item under a non-data frame");
  }
};

/** Closes a list or constructor, reading the break of an indefinite tag 102. */
const closeList = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  source: Source,
  summary: Summary,
  breakAfter: boolean,
): Tick => {
  if (!breakAfter) return deliver(env, ex, source, summary);
  const taken = takeByte(env, source);
  if (taken.k !== "got") return dataStop(env, ex, origin, taken);
  return taken.value === CBOR_BREAK
    ? deliver(env, ex, taken.source, summary)
    : skim(ex, origin);
};

const openList = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  source: Source,
  argument: bigint,
  constr: bigint,
  breakAfter: boolean,
): Tick => {
  if (argument === 0n) {
    return closeList(
      env,
      ex,
      origin,
      source,
      constrSummary(constr, emptyMidgardCekDataListSummary()),
      breakAfter,
    );
  }
  return to(
    push(ex, {
      k: "dataList",
      remaining: argument,
      count: 0,
      constr,
      breakAfter,
    }),
    { k: "data", source, mode: { k: "item" } },
  );
};

/** The fields list after a constructor tag. */
const openFields = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  source: Source,
  constr: bigint,
  breakAfter: boolean,
): Tick => {
  const head = readHead(env, source);
  if (head.k !== "got") return dataStop(env, ex, origin, head);
  const [major, , argument] = head.value;
  if (major !== 4) return skim(ex, origin);
  return openList(env, ex, origin, head.source, argument, constr, breakAfter);
};

const dataTagged = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  additional: number,
  tag: bigint,
  source: Source,
): Tick => {
  if (tag < 0n) return skim(ex, origin);
  if (additional === 2 || additional === 3) {
    const head = readHead(env, source);
    if (head.k !== "got") return dataStop(env, ex, origin, head);
    const [major, , length] = head.value;
    if (major !== 2 || length > MAX_DATA_BYTES_CHUNK) return skim(ex, origin);
    return to(ex, {
      k: "data",
      source: head.source,
      mode: {
        k: "bignum",
        magnitude: magNew(additional === 3),
        remaining: length < 0n ? 0 : Number(length),
        indefinite: length < 0n,
      },
    });
  }
  if (tag === 102n) {
    const head = readHead(env, source);
    if (head.k !== "got") return dataStop(env, ex, origin, head);
    const [major, , length] = head.value;
    if (major !== 4 || (length !== 2n && length !== -1n))
      return skim(ex, origin);
    const index = readHead(env, head.source);
    if (index.k !== "got") return dataStop(env, ex, origin, index);
    const [indexMajor, , value] = index.value;
    if (indexMajor !== 0 || value < 0n) return skim(ex, origin);
    return openFields(env, ex, origin, index.source, value, length < 0n);
  }
  if (tag >= 121n && tag <= 127n) {
    return openFields(env, ex, origin, source, tag - 121n, false);
  }
  if (tag >= 1280n && tag <= 1400n) {
    return openFields(env, ex, origin, source, tag - 1273n, false);
  }
  return skim(ex, origin);
};

const dataHead = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  [major, additional, argument]: Head,
  source: Source,
): Tick => {
  if (major === 0 || major === 1) {
    if (argument < 0n) return skim(ex, origin);
    return deliver(
      env,
      ex,
      source,
      integerSummary(major === 0 ? argument : -1n - argument),
    );
  }
  if (major === 2) {
    if (argument > MAX_DATA_BYTES_CHUNK) return skim(ex, origin);
    return to(ex, {
      k: "data",
      source,
      mode: {
        k: "bytes",
        sink: emptySink(),
        remaining: argument < 0n ? 0 : Number(argument),
        indefinite: argument < 0n,
      },
    });
  }
  if (major === 4)
    return openList(env, ex, origin, source, argument, -1n, false);
  if (major === 5) {
    if (argument === 0n) {
      return deliver(
        env,
        ex,
        source,
        summarizeMidgardCekMapData(emptyMidgardCekDataPairSummary()),
      );
    }
    return to(
      push(ex, {
        k: "dataMap",
        remaining: argument,
        count: 0,
        key: undefined,
      }),
      { k: "data", source, mode: { k: "item" } },
    );
  }
  if (major === 6)
    return dataTagged(env, ex, origin, additional, argument, source);
  return skim(ex, origin);
};

const dataBreak = (ex: Exec, origin: Source, source: Source): Tick => {
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  const [frame, rest] = popped;
  if (frame.k === "dataList" && frame.remaining < 0n) {
    return to(rest, {
      k: "data",
      source,
      mode: {
        k: "fold",
        fold: {
          pairs: false,
          remaining: frame.count,
          sequence: emptyMidgardCekDataListSummary(),
        },
        next: { k: "list", constr: frame.constr, breakAfter: frame.breakAfter },
      },
    });
  }
  if (
    frame.k === "dataMap" &&
    frame.remaining < 0n &&
    frame.key === undefined
  ) {
    return to(rest, {
      k: "data",
      source,
      mode: {
        k: "fold",
        fold: {
          pairs: true,
          remaining: frame.count,
          sequence: emptyMidgardCekDataPairSummary(),
        },
        next: { k: "map" },
      },
    });
  }
  return skim(ex, origin);
};

export const tickData = (
  env: DecodeEnv,
  ex: Exec,
  source: Source,
  mode: DataMode,
): Tick => {
  switch (mode.k) {
    case "item": {
      const taken = takeByte(env, source);
      if (taken.k !== "got") return dataStop(env, ex, source, taken);
      if (taken.value === CBOR_BREAK)
        return dataBreak(ex, source, taken.source);
      const head = headFrom(env, taken.value, taken.source);
      if (head.k !== "got") return dataStop(env, ex, source, head);
      return dataHead(env, ex, source, head.value, head.source);
    }
    case "bytes": {
      if (mode.remaining > 0) {
        const piece = takeSlice(env, source, mode.remaining);
        if (piece.k !== "got") return dataStop(env, ex, source, piece);
        return to(ex, {
          k: "data",
          source: piece.source,
          mode: {
            ...mode,
            sink: sinkPush(mode.sink, piece.value),
            remaining: mode.remaining - piece.value.length,
          },
        });
      }
      const finish = (s: Source): Tick =>
        deliver(
          env,
          ex,
          s,
          bytesSummary(sinkLength(mode.sink), sinkRoot(mode.sink)),
        );
      if (!mode.indefinite) return finish(source);
      return nextChunk(env, ex, source, finish, (s, length) =>
        to(ex, {
          k: "data",
          source: s,
          mode: {
            k: "bytes",
            sink: mode.sink,
            remaining: length,
            indefinite: true,
          },
        }),
      );
    }
    case "bignum": {
      if (mode.remaining > 0) {
        const piece = takeSlice(env, source, mode.remaining);
        if (piece.k !== "got") return dataStop(env, ex, source, piece);
        return to(ex, {
          k: "data",
          source: piece.source,
          mode: {
            ...mode,
            magnitude: magPush(mode.magnitude, piece.value),
            remaining: mode.remaining - piece.value.length,
          },
        });
      }
      const finish = (s: Source): Tick =>
        deliver(env, ex, s, magFinish(mode.magnitude)[0]);
      if (!mode.indefinite) return finish(source);
      return nextChunk(env, ex, source, finish, (s, length) =>
        to(ex, {
          k: "data",
          source: s,
          mode: {
            k: "bignum",
            magnitude: mode.magnitude,
            remaining: length,
            indefinite: true,
          },
        }),
      );
    }
    case "fold": {
      const { fold, next } = mode;
      if (fold.remaining > 0) {
        const popped = pop(ex);
        if (popped === undefined) return undefined;
        return to(popped[1], {
          k: "data",
          source,
          mode: { k: "fold", fold: foldOne(fold, popped[0]), next },
        });
      }
      if (next.k === "list") {
        return closeList(
          env,
          ex,
          source,
          source,
          constrSummary(next.constr, fold.sequence),
          next.breakAfter,
        );
      }
      return deliver(
        env,
        ex,
        source,
        summarizeMidgardCekMapData(fold.sequence),
      );
    }
    case "skim":
      return stepChunks(env, ex, source.position + source.left, undefined);
    case "trail":
      return stepChunks(env, ex, source.position, mode.summary);
  }
};
