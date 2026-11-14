/**
 * The Flat byte-string ticks of the script-decode step machine (byte strings,
 * strings, Data fillers and value keys), as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import { midgardCekBytesMemoryWords } from "@al-ft/midgard-core";

import {
  bytesSummary,
  emptySink,
  nextSetBit,
  sinkLength,
  sinkPush,
  sinkRoot,
  utf8Push,
} from "./script-decode.step-bits.js";
import {
  at,
  byteAt,
  type DecodeEnv,
  type Exec,
  expectFrame,
  pop,
  push,
  refuse,
  ScriptDecodeClass,
  type Tick,
} from "./script-decode.step-exec.js";
import type { BytesAcc, BytesPurpose } from "./script-decode.step-types.js";

const MAX_VALUE_KEY_BYTES = 32;

export const tickFiller = (
  env: DecodeEnv,
  ex: Exec,
  purpose: BytesPurpose,
): Tick => {
  const q = nextSetBit(env.window, ex.cursor, env.flatLength);
  if (q === undefined) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, env.flatBits);
  }
  if ((q + 1) % 8 !== 0) {
    return refuse(env, ex, ScriptDecodeClass.flatBadPadding, q + 1);
  }
  if (purpose === "data") {
    return at(push(ex, { k: "dataRoot" }), q + 1, {
      k: "data",
      source: { position: (q + 1) / 8, left: 0 },
      mode: { k: "item" },
    });
  }
  const acc: BytesAcc =
    purpose === "currency" || purpose === "token"
      ? { k: "key", key: Buffer.alloc(0), length: 0 }
      : { k: "blob", sink: emptySink(), utf8: 0 };
  return at(ex, q + 1, { k: "bytes", purpose, acc });
};

const keyFollows = (previous: Buffer | undefined, key: Buffer): boolean =>
  previous === undefined || Buffer.compare(previous, key) < 0;

export const tickBytes = (
  env: DecodeEnv,
  ex: Exec,
  purpose: BytesPurpose,
  acc: BytesAcc,
): Tick => {
  const cursor = ex.cursor;
  const position = Math.floor(cursor / 8);
  if (position >= env.flatLength) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, cursor);
  }
  const n = byteAt(env, position);
  if (n === 0) return bytesDone(env, ex, cursor + 8, purpose, acc);
  if (position + 1 + n > env.flatLength) {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, cursor);
  }
  const chunk = env.window.subarray(position + 1, position + 1 + n);
  const next: BytesAcc =
    acc.k === "blob"
      ? {
          k: "blob",
          sink: sinkPush(acc.sink, chunk),
          utf8:
            purpose === "string" && acc.utf8 >= 0
              ? utf8Push(acc.utf8, chunk)
              : acc.utf8,
        }
      : {
          k: "key",
          key:
            acc.length + n <= MAX_VALUE_KEY_BYTES
              ? Buffer.concat([acc.key, chunk])
              : acc.key,
          length: acc.length + n,
        };
  return at(ex, cursor + 8 * (1 + n), { k: "bytes", purpose, acc: next });
};

const bytesDone = (
  env: DecodeEnv,
  ex: Exec,
  after: number,
  purpose: BytesPurpose,
  acc: BytesAcc,
): Tick => {
  if (acc.k === "blob") {
    const length = sinkLength(acc.sink);
    if (purpose === "string" && acc.utf8 !== 0) {
      return refuse(env, ex, ScriptDecodeClass.invalidUtf8, after);
    }
    return at(ex, after, {
      k: "valueReturn",
      summary: bytesSummary(length, sinkRoot(acc.sink)),
      memory:
        purpose === "string"
          ? BigInt(Math.floor(length / 4))
          : midgardCekBytesMemoryWords(length),
    });
  }
  if (acc.length > MAX_VALUE_KEY_BYTES) {
    return refuse(env, ex, ScriptDecodeClass.nonCanonicalValue, after);
  }
  const popped = pop(ex);
  if (popped === undefined) return undefined;
  const [frame, rest] = popped;
  const key = acc.key;
  if (purpose === "currency") {
    const outer = expectFrame(frame, "assetOuter");
    return at(
      push(
        push(rest, {
          ...outer,
          previous: key,
          invalid: outer.invalid || !keyFollows(outer.previous, key),
        }),
        {
          k: "assetInner",
          currency: key,
          count: 0,
          previous: undefined,
          invalid: false,
        },
      ),
      after,
      { k: "assetInnerBit" },
    );
  }
  const inner = expectFrame(frame, "assetInner");
  return at(
    push(rest, {
      ...inner,
      previous: key,
      invalid: inner.invalid || !keyFollows(inner.previous, key),
    }),
    after,
    {
      k: "nat",
      purpose: { k: "quantity", token: key },
      start: after,
      groups: 0,
      acc: 0n,
      scale: 1n,
      big: false,
    },
  );
};
