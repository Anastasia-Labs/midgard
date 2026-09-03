/**
 * CBOR encoding of validation control data, control lists, frontier peaks, and the script-discovery trace control.
 */

import {
  encodeCbor,
  type MidgardValidationMerkleFrontierV1,
} from "@al-ft/midgard-core";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";

type ValidationControlDataV1 =
  | bigint
  | Buffer
  | readonly ValidationControlDataV1[];

const encodeValidationControlDataV1 = (
  value: ValidationControlDataV1,
): Buffer => {
  if (typeof value === "bigint") {
    return encodeCborInteger(value);
  }
  if (Buffer.isBuffer(value)) {
    if (value.length <= 64) {
      return encodeCborBytes(value);
    }
    const chunks: Buffer[] = [];
    for (let offset = 0; offset < value.length; offset += 64) {
      chunks.push(encodeCborBytes(value.subarray(offset, offset + 64)));
    }
    return Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]);
  }
  return encodeCborArrayRaw(value.map(encodeValidationControlDataV1));
};

export const encodeValidationControlListV1 = (
  values: readonly ValidationControlDataV1[],
): Buffer =>
  Buffer.concat([
    Buffer.from([0x9f]),
    ...values.map(encodeValidationControlDataV1),
    Buffer.from([0xff]),
  ]);

export const encodeValidationFrontierPeaksV1 = (
  frontier: MidgardValidationMerkleFrontierV1,
): readonly (readonly [bigint, Buffer])[] =>
  frontier.peaks.map((peak) => [BigInt(peak.height), peak.hash]);

export type ScriptDiscoveryTraceControlV1 = {
  readonly purposeCursor: number;
  readonly sourceCursor: number;
  readonly redeemerCursor: number;
  readonly currentPurposeKind: -1 | 0 | 1 | 2 | 3;
  readonly currentPurposeIndex: bigint;
  readonly currentScriptHash: Buffer;
  readonly currentSubject: Buffer;
  readonly matchedSourceIndex: number;
  readonly matchedLanguageTag: -1 | 0 | 3 | 128;
  readonly matchedSourceLeaf: Buffer;
  readonly usedInlineBitmap: bigint;
  readonly usedRedeemerBitmap: bigint;
  readonly redeemerItemControlHash: Buffer;
  readonly executionFrontier: MidgardValidationMerkleFrontierV1;
};

export const encodeScriptDiscoveryControlCborV1 = (
  discovery: ScriptDiscoveryTraceControlV1,
): Buffer =>
  encodeCbor([
    BigInt(discovery.purposeCursor),
    BigInt(discovery.sourceCursor),
    BigInt(discovery.redeemerCursor),
    BigInt(discovery.currentPurposeKind),
    discovery.currentPurposeIndex,
    discovery.currentScriptHash,
    discovery.currentSubject,
    BigInt(discovery.matchedSourceIndex),
    BigInt(discovery.matchedLanguageTag),
    discovery.matchedSourceLeaf,
    discovery.usedInlineBitmap,
    discovery.usedRedeemerBitmap,
    discovery.redeemerItemControlHash,
    BigInt(discovery.executionFrontier.count),
    encodeValidationFrontierPeaksV1(discovery.executionFrontier),
  ]);
