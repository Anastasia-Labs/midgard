/**
 * The step-state encodings outputReferenceScriptDecoding threads carry: the
 * output-scan control's datum form and the off-chain twin of the rule's
 * checkpoint hash.
 */
import { type MidgardLedgerOutputScanControl } from "@al-ft/midgard-core";
import { encodeVerdictSubject, hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import type { OutputReferenceScriptDecodingEvidence } from "./output-reference-script-decoding.js";

export const outputReferenceScriptControlData = (
  control: MidgardLedgerOutputScanControl,
) => ({
  version: BigInt(control.version),
  stage: BigInt(control.stage),
  cursor: BigInt(control.cursor),
  map_entry_count: BigInt(control.mapEntryCount),
  optional_field_count: BigInt(control.optionalFieldCount),
  address: control.address.toString("hex"),
  lovelace: control.lovelace,
  cardano_value_size: BigInt(control.cardanoValueSize),
  policy_remaining: BigInt(control.policyRemaining),
  asset_remaining: BigInt(control.assetRemaining),
  policy_asset_cursor: BigInt(control.policyAssetCursor),
  previous_policy: control.previousPolicy.toString("hex"),
  current_policy: control.currentPolicy.toString("hex"),
  previous_asset_name: control.previousAssetName.toString("hex"),
  asset_count: BigInt(control.assetFrontier.count),
  asset_peaks: control.assetFrontier.peaks.map(({ height, hash }) => ({
    height: BigInt(height),
    hash: hash.toString("hex"),
  })),
  datum_offset: BigInt(control.datumOffset),
  datum_length: BigInt(control.datumLength),
  payload_remaining: BigInt(control.payloadRemaining),
  reference_script_language: BigInt(control.referenceScriptLanguage),
  reference_script_item_offset: BigInt(control.referenceScriptItemOffset),
  reference_script_offset: BigInt(control.referenceScriptOffset),
  reference_script_length: BigInt(control.referenceScriptLength),
});

const cborInteger = (value: number): Buffer => {
  const major = value < 0 ? 1 : 0;
  const normalized = value < 0 ? -1 - value : value;
  if (normalized < 24) return Buffer.from([(major << 5) | normalized]);
  if (normalized < 256) return Buffer.from([(major << 5) | 24, normalized]);
  const result = Buffer.alloc(3);
  result[0] = (major << 5) | 25;
  result.writeUInt16BE(normalized, 1);
  return result;
};
const cborBytes = (value: Buffer): Buffer =>
  value.length < 24
    ? Buffer.concat([Buffer.from([0x40 | value.length]), value])
    : Buffer.concat([Buffer.from([0x58, value.length]), value]);

/** Exact off-chain twin of `rule.checkpoint_v1`. */
export const outputReferenceScriptCheckpoint = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly controlCbor: string;
  readonly nextExpectedScriptHash: string;
}): string =>
  Effect.runSync(
    hashHexWithBlake2b(
      Buffer.concat([
        Buffer.from(
          "midgard/fraud-proofs/output-reference-script-decoding/checkpoint-v1",
          "ascii",
        ),
        encodeVerdictSubject(evidence.subject),
        cborInteger(evidence.outputIndex),
        cborInteger(evidence.accusedClass),
        cborInteger(Buffer.from(evidence.referenceScriptItemHex, "hex").length),
        cborBytes(
          Buffer.from(evidence.referenceScriptItemCommitmentHex, "hex"),
        ),
        cborBytes(Buffer.from(controlCbor, "hex")),
        cborBytes(Buffer.from(nextExpectedScriptHash, "hex")),
      ]).toString("hex"),
      32,
    ),
  );
