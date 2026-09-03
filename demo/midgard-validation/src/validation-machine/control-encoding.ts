/**
 * CBOR encoding of validation control data, control lists, frontier peaks, and the script-discovery trace control.
 */

import {
  encodeCbor,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";

type ValidationControlData = bigint | Buffer | readonly ValidationControlData[];

const encodeValidationControlData = (value: ValidationControlData): Buffer => {
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
  return encodeCborArrayRaw(value.map(encodeValidationControlData));
};

export const encodeValidationControlList = (
  values: readonly ValidationControlData[],
): Buffer =>
  Buffer.concat([
    Buffer.from([0x9f]),
    ...values.map(encodeValidationControlData),
    Buffer.from([0xff]),
  ]);

export const encodeValidationFrontierPeaks = (
  frontier: MidgardValidationMerkleFrontier,
): readonly (readonly [bigint, Buffer])[] =>
  frontier.peaks.map((peak) => [BigInt(peak.height), peak.hash]);

export type ScriptDiscoveryTraceControl = {
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
  readonly executionFrontier: MidgardValidationMerkleFrontier;
};

export const encodeScriptDiscoveryControlCbor = (
  discovery: ScriptDiscoveryTraceControl,
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
    encodeValidationFrontierPeaks(discovery.executionFrontier),
  ]);
