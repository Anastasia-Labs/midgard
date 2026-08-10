import {
  buildMidgardBoundedItemV1,
  buildMidgardLedgerOutputAssetFrontierV1,
  buildMidgardLedgerOutputMaterialV1,
  ensureHash32,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
  type MidgardLedgerOutputCommitmentV1,
  type MidgardLedgerOutputDataSummaryV1,
  type MidgardLedgerOutputMaterialV1,
  type MidgardLedgerOutputReferenceScriptLanguageV1,
} from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_MAX_OUTPUT_INDEX_V1,
  type MidgardTxOutput,
  midgardValueToCmlValue,
  MidgardVersionedScriptTags,
} from "@al-ft/midgard-core/codec";
import { Constr, Data } from "@lucid-evolution/lucid";

import { summarizeMidgardCekLucidDataV1 } from "./cek-context.js";
import { decodeMidgardOutRefBytes } from "./ledger-tx/codec.js";
import { commitMidgardScriptContextTxOutV1 } from "./script-context-proof.js";

const MAX_LEDGER_OUTPUT_INDEX_V1 = 65_535n;

// §5.3's out-ref item spells the output index as a fixed CBOR uint16, and
// `decodeMidgardOutRefBytes` is the only door onto those bytes, so a decoded
// index is already inside this descriptor's domain — a per-call range check
// would be a gate that cannot fail. What can still go wrong is the two domains
// drifting apart, so that is what is asserted, once, at load time.
if (MAX_LEDGER_OUTPUT_INDEX_V1 !== BigInt(MIDGARD_MAX_OUTPUT_INDEX_V1)) {
  throw new Error(
    "ledger output-index domain has drifted from the §5.3 out-ref index domain",
  );
}

const exactSummary = ({
  root,
  cborLength,
  memory,
}: {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}): MidgardLedgerOutputDataSummaryV1 => ({
  root: ensureHash32(root, "ledger_output_descriptor_v1.semantic_root"),
  cborLength,
  memory,
});

const contextTxOutSummary = (
  output: MidgardTxOutput,
  encoding: "cardano" | "midgard",
): MidgardLedgerOutputDataSummaryV1 =>
  exactSummary(commitMidgardScriptContextTxOutV1(output, encoding));

const cardanoSpendDatumSummary = (
  output: MidgardTxOutput,
): MidgardLedgerOutputDataSummaryV1 => {
  const datum = output.datum;
  return exactSummary(
    summarizeMidgardCekLucidDataV1(
      datum === undefined
        ? new Constr(1, [])
        : new Constr(0, [Data.from(datum.cbor.toString("hex")) as never]),
    ),
  );
};

const flattenedAssets = (output: MidgardTxOutput) =>
  [...output.value.assets.entries()].flatMap(([policyId, assets]) =>
    [...assets.entries()].map(([assetName, quantity]) => ({
      policyId: Buffer.from(policyId, "hex"),
      assetName: Buffer.from(assetName, "hex"),
      quantity,
    })),
  );

const referenceScriptFacts = ({
  outputIndex,
  output,
}: {
  readonly outputIndex: number;
  readonly output: MidgardTxOutput;
}): Pick<
  MidgardLedgerOutputCommitmentV1,
  | "referenceScriptLanguage"
  | "referenceScriptHash"
  | "referenceScriptTotalLength"
  | "referenceScriptItemCommitment"
> => {
  const script = output.script_ref;
  if (script === undefined) {
    return {
      referenceScriptLanguage: -1,
      referenceScriptHash: Buffer.alloc(0),
      referenceScriptTotalLength: 0,
      referenceScriptItemCommitment: Buffer.alloc(0),
    };
  }
  const scriptCbor = encodeMidgardVersionedScript(script);
  const item = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
    itemIndex: outputIndex,
    bytes: scriptCbor,
  });
  return {
    referenceScriptLanguage: Number(
      MidgardVersionedScriptTags[script.language],
    ) as Exclude<MidgardLedgerOutputReferenceScriptLanguageV1, -1>,
    referenceScriptHash: Buffer.from(hashMidgardVersionedScript(script), "hex"),
    referenceScriptTotalLength: item.bytes.length,
    referenceScriptItemCommitment: item.commitment,
  };
};

/**
 * Deterministically constructs the compact ledger descriptor from one exact
 * canonical output. This is the off-chain mirror of the bounded Aiken output
 * scan; admission still fails closed unless that scan authenticates every
 * descriptor fact before the value is inserted into the ledger MPF.
 */
export const buildCanonicalMidgardLedgerOutputMaterialV1 = ({
  outputIndex,
  outputCbor,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputMaterialV1 => {
  const output = decodeMidgardTxOutput(outputCbor);
  const assets = buildMidgardLedgerOutputAssetFrontierV1(
    flattenedAssets(output),
  );
  return buildMidgardLedgerOutputMaterialV1({
    outputIndex,
    outputCbor,
    facts: {
      address: Buffer.from(output.address),
      lovelace: output.value.lovelace,
      assetCount: assets.count,
      assetFrontierCommitment: assets.commitment,
      cardanoValueSize: midgardValueToCmlValue(output.value).to_cbor_bytes()
        .length,
      ...referenceScriptFacts({ outputIndex, output }),
      cardanoTxOut: contextTxOutSummary(output, "cardano"),
      midgardTxOut: contextTxOutSummary(output, "midgard"),
      cardanoSpendDatum: cardanoSpendDatumSummary(output),
    },
  });
};

/**
 * Converts persisted full output material into its compact consensus-ledger
 * value.
 *
 * `decodeMidgardOutRefBytes` is the exact-form gate: it accepts only §5.3's
 * 38-byte fixed-index item and throws on every other shape, which is what these
 * bytes have to be — they are ledger trie keys, the same value on-chain
 * `ledger_outref_key` derives — and the decoded output index is what selects the
 * bounded-item commitment domain below.
 */
export const buildCanonicalMidgardLedgerEntryOutputMaterialV1 = ({
  outRef,
  outputCbor,
}: {
  readonly outRef: Uint8Array;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputMaterialV1 => {
  const decoded = decodeMidgardOutRefBytes(outRef);
  return buildCanonicalMidgardLedgerOutputMaterialV1({
    outputIndex: Number(decoded.index),
    outputCbor,
  });
};
