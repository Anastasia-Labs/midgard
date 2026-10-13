import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardLedgerOutputMaterial,
  buildMidgardLedgerOutputScanTrace,
  ensureHash32,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  type MidgardLedgerOutputDataSummary,
  type MidgardLedgerOutputMaterial,
  type MidgardLedgerOutputReferenceScriptLanguage,
} from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptEnvelope,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_MAX_OUTPUT_INDEX,
  midgardCardanoValueCborLength,
  MidgardScriptHashPrefixes,
  type MidgardTxOutput,
  MidgardVersionedScriptTags,
} from "@al-ft/midgard-core/codec";
import { DataConstr } from "@harmoniclabs/plutus-data";
import { blake2b } from "@noble/hashes/blake2.js";

import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import { decodeMidgardOutRefBytes } from "./ledger-tx/codec.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { commitMidgardScriptContextTxOut } from "./script-context-proof.js";

const MAX_LEDGER_OUTPUT_INDEX = 65_535n;

// §5.3's out-ref item spells the output index as a fixed CBOR uint16, and
// `decodeMidgardOutRefBytes` is the only door onto those bytes, so a decoded
// index is already inside this descriptor's domain — a per-call range check
// would be a gate that cannot fail. What can still go wrong is the two domains
// drifting apart, so that is what is asserted, once, at load time.
if (MAX_LEDGER_OUTPUT_INDEX !== BigInt(MIDGARD_MAX_OUTPUT_INDEX)) {
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
}): MidgardLedgerOutputDataSummary => ({
  root: ensureHash32(root, "ledger_output_descriptor_v1.semantic_root"),
  cborLength,
  memory,
});

const contextTxOutSummary = (
  output: MidgardTxOutput,
  encoding: "cardano" | "midgard",
  referenceScriptHash: string | undefined,
): MidgardLedgerOutputDataSummary =>
  exactSummary(
    commitMidgardScriptContextTxOut(output, encoding, referenceScriptHash),
  );

const cardanoSpendDatumSummary = (
  output: MidgardTxOutput,
): MidgardLedgerOutputDataSummary => {
  const datum = output.datum;
  return exactSummary(
    commitMidgardCekDataTree(
      datum === undefined
        ? new DataConstr(1n, [])
        : new DataConstr(0n, [plutusDataFromCborIterative(datum.cbor)]),
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

/** A reference script as the ledger descriptor sees it: tag, hash and item bytes. */
type ReferenceScriptItem = {
  readonly language: Exclude<MidgardLedgerOutputReferenceScriptLanguage, -1>;
  readonly hash: string;
  readonly cbor: Buffer;
};

const ledgerOutputMaterial = ({
  outputIndex,
  outputCbor,
  output,
  referenceScript,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
  /** Address, value and datum; any `script_ref` is ignored for `referenceScript`. */
  readonly output: MidgardTxOutput;
  readonly referenceScript: ReferenceScriptItem | undefined;
}): MidgardLedgerOutputMaterial => {
  const assets = buildMidgardLedgerOutputAssetFrontier(flattenedAssets(output));
  const scriptItem =
    referenceScript === undefined
      ? undefined
      : buildMidgardBoundedItem({
          fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
          itemIndex: outputIndex,
          bytes: referenceScript.cbor,
        });
  return buildMidgardLedgerOutputMaterial({
    outputIndex,
    outputCbor,
    facts: {
      address: Buffer.from(output.address),
      lovelace: output.value.lovelace,
      assetCount: assets.count,
      assetFrontierCommitment: assets.commitment,
      cardanoValueSize: midgardCardanoValueCborLength(output.value),
      referenceScriptLanguage: referenceScript?.language ?? -1,
      referenceScriptHash: Buffer.from(referenceScript?.hash ?? "", "hex"),
      referenceScriptTotalLength: scriptItem?.bytes.length ?? 0,
      referenceScriptItemCommitment: scriptItem?.commitment ?? Buffer.alloc(0),
      cardanoTxOut: contextTxOutSummary(
        output,
        "cardano",
        referenceScript?.hash,
      ),
      midgardTxOut: contextTxOutSummary(
        output,
        "midgard",
        referenceScript?.hash,
      ),
      cardanoSpendDatum: cardanoSpendDatumSummary(output),
    },
  });
};

/**
 * Deterministically constructs the compact ledger descriptor from one exact
 * canonical output. This is the off-chain mirror of the bounded Aiken output
 * scan; admission still fails closed unless that scan authenticates every
 * descriptor fact before the value is inserted into the ledger MPF.
 */
export const buildCanonicalMidgardLedgerOutputMaterial = ({
  outputIndex,
  outputCbor,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputMaterial => {
  const output = decodeMidgardTxOutput(outputCbor);
  const script = output.script_ref;
  return ledgerOutputMaterial({
    outputIndex,
    outputCbor,
    output,
    referenceScript:
      script === undefined
        ? undefined
        : {
            language: Number(
              MidgardVersionedScriptTags[script.language],
            ) as ReferenceScriptItem["language"],
            hash: hashMidgardVersionedScript(script),
            cbor: encodeMidgardVersionedScript(script),
          },
  });
};

const CBOR_MAP_HEADER = 0xa0;
const OUTPUT_SCRIPT_REF_KEY = 0x03;

/**
 * The ledger descriptor of an output whose reference script is a native
 * script, derived from the output bytes without interpreting the script: its
 * hash, length and item commitment are taken over the script bytes exactly as
 * they stand, as the bounded output scan reads them. The rest of the output
 * must decode canonically. For a decodable native script this equals
 * `buildCanonicalMidgardLedgerOutputMaterial`; it exists for fault evidence
 * about outputs whose native script does not decode, and does not make such
 * an output valid.
 */
export const buildOpaqueNativeReferenceScriptLedgerOutputMaterial = ({
  outputIndex,
  outputCbor,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputMaterial => {
  const bytes = Buffer.from(outputCbor);
  const scan = buildMidgardLedgerOutputScanTrace(bytes).terminal;
  const itemOffset = scan.referenceScriptItemOffset;
  // The reference script is the output map's last key (3): stripping it
  // leaves the canonical encoding of the same output without one.
  if (
    itemOffset < 1 ||
    scan.referenceScriptOffset + scan.referenceScriptLength !== bytes.length ||
    bytes[itemOffset - 1] !== OUTPUT_SCRIPT_REF_KEY ||
    bytes[0]! < CBOR_MAP_HEADER + 3 ||
    bytes[0]! > CBOR_MAP_HEADER + 4
  )
    throw new Error("output does not end in a reference script");
  const cbor = bytes.subarray(itemOffset);
  const envelope = decodeMidgardVersionedScriptEnvelope(cbor);
  if (envelope.language !== "NativeCardano")
    throw new Error("output reference script is not a native script");
  const output = decodeMidgardTxOutput(
    Buffer.concat([
      Buffer.from([bytes[0]! - 1]),
      bytes.subarray(1, itemOffset - 1),
    ]),
  );
  return ledgerOutputMaterial({
    outputIndex,
    outputCbor: bytes,
    output,
    referenceScript: {
      language: Number(
        MidgardVersionedScriptTags.NativeCardano,
      ) as ReferenceScriptItem["language"],
      hash: Buffer.from(
        blake2b(
          Buffer.concat([
            Buffer.from([MidgardScriptHashPrefixes.NativeCardano]),
            envelope.scriptBytes,
          ]),
          { dkLen: 28 },
        ),
      ).toString("hex"),
      cbor: Buffer.from(cbor),
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
export const buildCanonicalMidgardLedgerEntryOutputMaterial = ({
  outRef,
  outputCbor,
}: {
  readonly outRef: Uint8Array;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputMaterial => {
  const decoded = decodeMidgardOutRefBytes(outRef);
  return buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: Number(decoded.index),
    outputCbor,
  });
};

/** Reuse pure descriptor construction only within one retained-ledger reconstruction. */
export const createCanonicalMidgardLedgerDescriptorResolver = () => {
  const descriptors = new Map<string, Buffer>();
  return ({
    outRef,
    outputCbor,
  }: {
    readonly outRef: Uint8Array;
    readonly outputCbor: Uint8Array;
  }): Buffer => {
    const outputIndex = Number(decodeMidgardOutRefBytes(outRef).index);
    const key = `${outputIndex}:${Buffer.from(outputCbor).toString("hex")}`;
    let descriptor = descriptors.get(key);
    if (descriptor === undefined) {
      descriptor = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor,
      }).descriptorCbor;
      descriptors.set(key, descriptor);
    }
    return Buffer.from(descriptor);
  };
};
