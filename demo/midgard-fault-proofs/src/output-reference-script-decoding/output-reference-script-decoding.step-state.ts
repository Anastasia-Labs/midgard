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

/**
 * Twin of `cbor.serialise` on an integer the rejection codec admits, of
 * either sign: a CBOR head of at most 8 argument bytes.
 */
const cborInteger = (input: bigint | number): Buffer => {
  const value = BigInt(input);
  const major = value < 0n ? 1 : 0;
  const normalized = value < 0n ? -1n - value : value;
  if (normalized < 24n) return Buffer.from([(major << 5) | Number(normalized)]);
  if (normalized >= 1n << 64n)
    throw new Error("checkpoint integer is outside the CBOR head range");
  const [width, additional] =
    normalized <= 0xffn
      ? [1, 24]
      : normalized <= 0xffffn
        ? [2, 25]
        : normalized <= 0xffffffffn
          ? [4, 26]
          : [8, 27];
  const argument = Buffer.from(
    normalized.toString(16).padStart(width * 2, "0"),
    "hex",
  );
  return Buffer.concat([Buffer.from([(major << 5) | additional]), argument]);
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
