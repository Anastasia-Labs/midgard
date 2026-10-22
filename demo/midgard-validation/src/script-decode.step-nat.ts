/**
 * The natural-number ticks of the script-decode step machine (program
 * version, integer constants, value quantities), as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import { midgardCekIntegerMemoryWords } from "@al-ft/midgard-core";

import {
  integerSummary,
  magFinish,
  magNew,
  magPush,
  shortBytesSummary,
} from "./script-decode.step-bits.js";
import {
  type DecodeEnv,
  type Exec,
  expectFrame,
  have,
  pop,
  push,
  read,
  refuse,
  ScriptDecodeClass,
  type Tick,
  to,
} from "./script-decode.step-exec.js";
import type {
  Control,
  MagSink,
  NatPurpose,
} from "./script-decode.step-types.js";

/** Integer varint groups read per tick. */
const NAT_GROUPS_PER_TICK = 10;

/** Magnitude bytes repacked per tick. */
const MAGNITUDE_BYTES_PER_TICK = 32;

const MIN_QUANTITY = -(1n << 127n);
const MAX_QUANTITY = (1n << 127n) - 1n;

const zigzag = (z: bigint): bigint => (z % 2n === 0n ? z / 2n : -(z / 2n) - 1n);

export const tickNat = (
  env: DecodeEnv,
  ex: Exec,
  control: Extract<Control, { k: "nat" }>,
): Tick => {
  const { purpose, start } = control;
  let { groups, acc, scale, big } = control;
  let current = ex;
  for (let budget = NAT_GROUPS_PER_TICK; budget > 0; budget -= 1) {
    const position = current.cursor;
    if (!have(env, position, 8)) {
      return refuse(env, current, ScriptDecodeClass.flatTruncated, position);
    }
    const v = read(env, position, 8);
    const payload = BigInt(v % 128);
    current = { ...current, cursor: position + 8 };
    // Exact below 20 groups (140 bits); a non-zero group past that sets `big`.
    const exact = groups < 20;
    if (exact) {
      acc += payload * scale;
      scale *= 128n;
    }
    big = big || (!exact && payload > 0n);
    groups += 1;
    if (v < 128) return natDone(env, current, purpose, start, groups, acc, big);
  }
  return to(current, { k: "nat", purpose, start, groups, acc, scale, big });
};

const natDone = (
  env: DecodeEnv,
  ex: Exec,
  purpose: NatPurpose,
  start: number,
  groups: number,
  acc: bigint,
  big: boolean,
): Tick => {
  switch (purpose.k) {
    case "version": {
      const component = big || acc > 2n ? 2 : Number(acc);
      const weight = purpose.index === 0 ? 9 : purpose.index === 1 ? 3 : 1;
      const next = { ...ex, version: ex.version + component * weight };
      if (purpose.index === 2) return to(next, { k: "term" });
      return to(next, {
        k: "nat",
        purpose: { k: "version", index: purpose.index + 1 },
        start: ex.cursor,
        groups: 0,
        acc: 0n,
        scale: 1n,
        big: false,
      });
    }
    case "integer": {
      if (big) {
        return to(ex, {
          k: "magnitude",
          start,
          groups,
          emitted: 0,
          magnitude: magNew(read(env, start + 1, 7) % 2 === 1),
        });
      }
      const n = zigzag(acc);
      return to(ex, {
        k: "valueReturn",
        summary: integerSummary(n),
        memory: midgardCekIntegerMemoryWords(n),
      });
    }
    case "quantity": {
      const n = zigzag(acc);
      if (big || n < MIN_QUANTITY || n > MAX_QUANTITY) {
        return refuse(env, ex, ScriptDecodeClass.nonCanonicalValue, ex.cursor);
      }
      const popped = pop(ex);
      if (popped === undefined) return undefined;
      const inner = expectFrame(popped[0], "assetInner");
      return to(
        push(
          push(popped[1], {
            k: "pairEntry",
            key: shortBytesSummary(purpose.token),
            value: integerSummary(n),
          }),
          {
            ...inner,
            count: inner.count + 1,
            invalid: inner.invalid || n === 0n,
          },
        ),
        { k: "assetInnerBit" },
      );
    }
  }
};

/** The 7-bit payload of group `index` of the natural at `start`. */
const groupPayload = (
  env: DecodeEnv,
  start: number,
  groups: number,
  index: number,
): number => (index >= groups ? 0 : read(env, start + 8 * index + 1, 7));

/** Byte `t` (from the least significant) of the magnitude `z >> 1`. */
const magnitudeByte = (
  env: DecodeEnv,
  start: number,
  groups: number,
  t: number,
): number => {
  const k = 8 * t + 1;
  const i0 = Math.floor(k / 7);
  const offset = k - 7 * i0;
  const v =
    groupPayload(env, start, groups, i0) +
    128 * groupPayload(env, start, groups, i0 + 1);
  return Math.floor(v / 2 ** offset) % 256;
};

export const tickMagnitude = (
  env: DecodeEnv,
  ex: Exec,
  control: Extract<Control, { k: "magnitude" }>,
): Tick => {
  const { start, groups, emitted } = control;
  const total = Math.floor((7 * groups - 1 + 7) / 8);
  if (emitted >= total) {
    const [summary, words] = magFinish(control.magnitude);
    return to(ex, { k: "valueReturn", summary, memory: words });
  }
  const upto = Math.min(emitted + MAGNITUDE_BYTES_PER_TICK, total);
  const bytes = Buffer.alloc(upto - emitted);
  for (let b = emitted; b < upto; b += 1) {
    bytes[b - emitted] = magnitudeByte(env, start, groups, total - 1 - b);
  }
  const magnitude: MagSink = magPush(control.magnitude, bytes);
  return to(ex, { ...control, emitted: upto, magnitude });
};
