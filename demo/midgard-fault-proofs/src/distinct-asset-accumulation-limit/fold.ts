/** Byte-level twin of the `distinctAssetAccumulationLimit` fold
 * (`distinct-asset-accumulation-limit/fold.ak`): the output count with its
 * resume points, each mint policy's declared asset count, and the entry
 * units of a bound transaction in the producer's fold order. */
import { decodeMidgardLedgerOutputCommitment } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL as LABEL } from "./contracts.js";

const MAX = SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_MAX_ENTRIES;

/** `OutputResumeV1`: the byte offset of the next policy key or asset name,
 * the policies not yet counted, the assets of the counted policy still to be
 * skipped, and the entries of this output already counted. */
export type OutputResume = Readonly<{
  offset: bigint;
  policies_remaining: bigint;
  assets_remaining: bigint;
  asset_index: bigint;
}>;

const byteAt = (item: Uint8Array, offset: number): number => {
  const value = item[offset];
  if (value === undefined)
    throw new Error(`${LABEL}: item read past its end at ${offset.toString()}`);
  return value;
};

/** Addresses and asset names are at most 255 bytes: only the two short
 * byte-string heads occur. */
const skipShortBytesAt = (item: Uint8Array, offset: number): number => {
  const head = byteAt(item, offset);
  if (head === 0x58) return offset + 2 + byteAt(item, offset + 1);
  if (head < 0x40 || head > 0x57)
    throw new Error(`${LABEL}: no short byte string at ${offset.toString()}`);
  return offset + 1 + head - 0x40;
};

const skipUintAt = (item: Uint8Array, offset: number): number => {
  const head = byteAt(item, offset);
  if (head <= 23) return offset + 1;
  const width = { 24: 1, 25: 2, 26: 4, 27: 8 }[head];
  if (width === undefined)
    throw new Error(`${LABEL}: no unsigned integer at ${offset.toString()}`);
  return offset + 1 + width;
};

/** `codec.decode_definite_map_header_at`: the offset after the header and
 * the declared entry count. */
export const definiteMapHeaderAt = (
  item: Uint8Array,
  offset: number,
): readonly [number, number] => {
  const tag = byteAt(item, offset);
  if (tag >= 0xa0 && tag <= 0xb7) return [offset + 1, tag - 0xa0];
  const width = { 0xb8: 1, 0xb9: 2, 0xba: 4 }[tag];
  if (width === undefined)
    throw new Error(`${LABEL}: no definite map header at ${offset.toString()}`);
  let count = 0;
  for (let at = 1; at <= width; at += 1)
    count = count * 256 + byteAt(item, offset + at);
  return [offset + 1 + width, count];
};

/** The resume point at the first policy of a canonical output's value: map
 * head and key 0 are one byte each; key 1 and the value's array head follow
 * the address; the coin precedes the policy map. */
export const outputValueStart = (item: Uint8Array): OutputResume => {
  const coinOffset = skipShortBytesAt(item, 2) + 2;
  const [offset, policies] = definiteMapHeaderAt(
    item,
    skipUintAt(item, coinOffset),
  );
  return {
    offset: BigInt(offset),
    policies_remaining: BigInt(policies),
    assets_remaining: 0n,
    asset_index: 0n,
  };
};

export type OutputOutcome =
  | Readonly<{ kind: "counted"; entryCount: bigint; budget: bigint }>
  | Readonly<{ kind: "paused"; entryCount: bigint; resume: OutputResume }>
  | Readonly<{ kind: "crosses"; assetIndex: bigint }>;

/** `count_output_v1`: one budget unit per policy and per skipped asset; the
 * last policy's assets are never skipped. */
export const countOutput = (
  item: Uint8Array,
  start: OutputResume,
  startCount: bigint,
  startBudget: bigint,
): OutputOutcome => {
  let resume = start;
  let entryCount = startCount;
  let budget = startBudget;
  for (;;) {
    if (resume.assets_remaining === 0n) {
      if (resume.policies_remaining === 0n)
        return { kind: "counted", entryCount, budget };
      if (budget === 0n) return { kind: "paused", entryCount, resume };
      // The 28-byte policy key is `58 1c` and its bytes.
      const [offset, assets] = definiteMapHeaderAt(
        item,
        Number(resume.offset) + 30,
      );
      const entries = BigInt(assets);
      if (entryCount + entries > MAX)
        return {
          kind: "crosses",
          assetIndex: resume.asset_index + MAX - entryCount,
        };
      entryCount += entries;
      budget -= 1n;
      if (resume.policies_remaining === 1n)
        return { kind: "counted", entryCount, budget };
      resume = {
        offset: BigInt(offset),
        policies_remaining: resume.policies_remaining - 1n,
        assets_remaining: entries,
        asset_index: resume.asset_index + entries,
      };
    } else {
      if (budget === 0n) return { kind: "paused", entryCount, resume };
      resume = {
        ...resume,
        offset: BigInt(
          skipUintAt(item, skipShortBytesAt(item, Number(resume.offset))),
        ),
        assets_remaining: resume.assets_remaining - 1n,
      };
      budget -= 1n;
    }
  }
};

/** Each policy's asset-map size, in the output's value order. */
export const outputPolicySizes = (item: Uint8Array): readonly bigint[] => {
  const sizes: bigint[] = [];
  let resume = outputValueStart(item);
  // One unit at a time: each counted policy adds its size to `asset_index`.
  let counted = 0n;
  while (resume.policies_remaining > 0n) {
    const outcome = countOutput(item, resume, 0n, 1n);
    if (outcome.kind === "crosses")
      throw new Error(`${LABEL}: an output holds more than the bound`);
    if (outcome.kind === "counted") {
      sizes.push(outcome.entryCount);
      return sizes;
    }
    if (outcome.resume.asset_index !== resume.asset_index)
      sizes.push(outcome.resume.asset_index - counted);
    counted = outcome.resume.asset_index;
    resume = outcome.resume;
  }
  return sizes;
};

/** `policy_header_v1(...).declared_count`: a mint item is a pair of a
 * 28-byte policy and its asset map. */
export const mintDeclaredCount = (item: Uint8Array): bigint => {
  if (byteAt(item, 0) !== 0x82 || byteAt(item, 1) !== 0x58)
    throw new Error(`${LABEL}: mint item is not a policy pair`);
  if (byteAt(item, 2) !== 28)
    throw new Error(`${LABEL}: mint policy is not 28 bytes`);
  return BigInt(definiteMapHeaderAt(item, 31)[1]);
};

/** A spent input's ledger descriptor under the event pre-root; `undefined`
 * when the pre-state holds none. */
export const spentDescriptor = (
  material: DirectScriptMaterial,
  item: Uint8Array,
) =>
  material.ledger.root.entries.find((entry) =>
    entry.key.equals(Buffer.from(item)),
  );

/** The spent input's schedule position: its field-0 ordinal plus the
 * reference inputs ordered strictly below it by item bytes. */
export const inputScheduleIndex = (
  material: DirectScriptMaterial,
  ordinal: number,
): bigint => {
  const item = material.fields[0]![ordinal]!;
  return BigInt(
    ordinal +
      material.fields[1]!.filter((reference) => reference.compare(item) < 0)
        .length,
  );
};

/** The entry units of the bound transaction, in the producer's order;
 * `null` when a spent input has no descriptor in the event pre-state, which
 * makes an earlier phase the first fault. */
export const distinctAssetUnits = (
  material: DirectScriptMaterial,
): SDK.DistinctAssetUnits | null => {
  const inputs: { scheduleIndex: bigint; entries: bigint }[] = [];
  for (const [ordinal, item] of material.fields[0]!.entries()) {
    const entry = spentDescriptor(material, item);
    if (entry === undefined) return null;
    inputs.push({
      scheduleIndex: inputScheduleIndex(material, ordinal),
      entries: BigInt(
        decodeMidgardLedgerOutputCommitment(entry.value).assetCount,
      ),
    });
  }
  return {
    inputs,
    outputs: material.fields[2]!.map(outputPolicySizes),
    mint: material.fields[5]!.map(mintDeclaredCount),
  };
};
