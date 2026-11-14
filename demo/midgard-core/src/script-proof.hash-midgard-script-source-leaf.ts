import { buildMidgardBoundedItem } from "./bounded-item.js";
import { midgardBlake2b } from "./codec/blake2b.js";
import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import { decodeMidgardSpendInputItem } from "./codec/native-tx-field-item-decoders.js";
import { encodeMidgardSpendInputItem } from "./codec/native-tx-field-items.js";
import {
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
  MidgardVersionedScriptTags,
} from "./codec/versioned-script.js";

const SOURCE_LEAF_DOMAIN = Buffer.from("MidgardScriptSourceLeafV1", "ascii");

const INLINE_SOURCE_LEAF_DOMAIN = Buffer.from(
  "MidgardInlineScriptSourceLeafV1",
  "ascii",
);

export const REDEEMER_LEAF_DOMAIN = Buffer.from(
  "MidgardRedeemerLeafV1",
  "ascii",
);

export const PURPOSE_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptPurposeLeafV1",
  "ascii",
);

export const SIGNER_LEAF_DOMAIN = Buffer.from("MidgardSignerLeafV1", "ascii");

export const OUTPUT_ITEM_LEAF_DOMAIN = Buffer.from(
  "MidgardOutputItemLeafV1",
  "ascii",
);

export const OUTPUT_DESCRIPTOR_LEAF_DOMAIN = Buffer.from(
  "MidgardOutputDescriptorLeafV1",
  "ascii",
);

export const EXECUTION_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptExecutionLeafV1",
  "ascii",
);

export const MINT_ASSET_LEAF_DOMAIN = Buffer.from(
  "MidgardMintAssetLeafV1",
  "ascii",
);

export const SCRIPT_CONTEXT_ITEM_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptContextItemLeafV1",
  "ascii",
);

export const RESOLVED_CONTEXT_ITEM_LEAF_DOMAIN = Buffer.from(
  "MidgardResolvedContextItemLeafV1",
  "ascii",
);

export const hash32 = (bytes: Uint8Array): Hash32 =>
  ensureHash32(midgardBlake2b(bytes, { dkLen: 32 }), "script_proof_hash");

/**
 * Decodes the only canonical V1 reference-script source key. That key *is* the
 * ledger out-ref, and an out-ref has exactly one byte form in Midgard: §5.3's
 * fixed-index field-0/1 item `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, 38 bytes
 * (`docs/spec/midgard-tx.md` §5.3). So this goes through the §5.3 decoder twin
 * rather than a local CBOR shape check — `19 0000` is deliberately non-minimal
 * and a generic minimal-CBOR reader rejects it.
 *
 * The re-encode is what makes the key canonical rather than merely well-formed:
 * the decoder is injective, so equality against the original bytes admits one
 * spelling per out-ref. Twin of `canonical_reference_source_key`.
 */
const decodeCanonicalReferenceScriptSourceKey = (
  sourceKeyBytes: Uint8Array,
): { readonly outputIndex: bigint } => {
  let decoded;
  try {
    decoded = decodeMidgardSpendInputItem(sourceKeyBytes);
  } catch (cause) {
    throw new Error(
      `reference script source key is not an output reference: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
  if (
    !encodeMidgardSpendInputItem(decoded).equals(Buffer.from(sourceKeyBytes))
  ) {
    throw new Error("reference script source key is not canonical");
  }
  return { outputIndex: BigInt(decoded.outputIndex) };
};

/** Hashes the exact stored Flat script bytes with their language prefix. */
export const hashMidgardV1VersionedScript = (
  script: MidgardVersionedScript,
): string => {
  if (script.language === "NativeCardano") {
    throw new Error("expected a PlutusV3 or MidgardV1 script");
  }
  return hashMidgardVersionedScript(script);
};

export const hashMidgardScriptSourceLeaf = (input: {
  readonly originKind: "inline" | "reference";
  readonly sourceKey: Uint8Array;
  readonly script: MidgardVersionedScript;
}): Hash32 => {
  if (input.originKind === "inline") {
    const decodedSourceIndex = decodeSingleCbor(input.sourceKey);
    const sourceIndex =
      typeof decodedSourceIndex === "number" &&
      Number.isSafeInteger(decodedSourceIndex)
        ? BigInt(decodedSourceIndex)
        : decodedSourceIndex;
    if (
      typeof sourceIndex !== "bigint" ||
      sourceIndex < 0n ||
      sourceIndex > BigInt(Number.MAX_SAFE_INTEGER) ||
      !encodeCbor(sourceIndex).equals(Buffer.from(input.sourceKey))
    ) {
      throw new Error("inline script source key is not a canonical index");
    }
    const item = buildMidgardBoundedItem({
      fieldIndex: 6,
      itemIndex: Number(sourceIndex),
      bytes: encodeMidgardVersionedScript(input.script),
    });
    return hashMidgardInlineScriptSourceLeaf({
      sourceIndex,
      scriptLanguageTag: Number(
        MidgardVersionedScriptTags[input.script.language],
      ) as 0 | 3 | 128,
      scriptHash: Buffer.from(hashMidgardVersionedScript(input.script), "hex"),
      scriptTotalLength: item.bytes.length,
      itemCommitment: item.commitment,
    });
  }
  const scriptHash = Buffer.from(
    hashMidgardVersionedScript(input.script),
    "hex",
  );
  const { outputIndex } = decodeCanonicalReferenceScriptSourceKey(
    input.sourceKey,
  );
  const scriptCbor = encodeMidgardVersionedScript(input.script);
  const item = buildMidgardBoundedItem({
    fieldIndex: 2,
    itemIndex: Number(outputIndex),
    bytes: scriptCbor,
  });
  return hashMidgardReferenceScriptSourceLeaf({
    sourceKey: input.sourceKey,
    scriptLanguageTag: Number(
      MidgardVersionedScriptTags[input.script.language],
    ) as 0 | 3 | 128,
    scriptHash,
    scriptTotalLength: scriptCbor.length,
    itemCommitment: item.commitment,
  });
};

export const hashMidgardReferenceScriptSourceLeaf = (input: {
  readonly sourceKey: Uint8Array;
  readonly scriptLanguageTag: 0 | 3 | 128;
  readonly scriptHash: Uint8Array;
  readonly scriptTotalLength: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  decodeCanonicalReferenceScriptSourceKey(input.sourceKey);
  if (
    input.scriptLanguageTag !== 0 &&
    input.scriptLanguageTag !== 3 &&
    input.scriptLanguageTag !== 128
  ) {
    throw new Error("reference script language tag is not supported");
  }
  const scriptHash = Buffer.from(input.scriptHash);
  if (scriptHash.length !== 28) {
    throw new Error("reference script hash must contain exactly 28 bytes");
  }
  if (
    !Number.isSafeInteger(input.scriptTotalLength) ||
    input.scriptTotalLength <= 0
  ) {
    throw new Error("reference script total length must be positive");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "reference script source item commitment",
  );
  return hash32(
    Buffer.concat([
      SOURCE_LEAF_DOMAIN,
      encodeCbor(1n),
      encodeCbor(Buffer.from(input.sourceKey)),
      encodeCbor(input.scriptLanguageTag),
      encodeCbor(scriptHash),
      encodeCbor(BigInt(input.scriptTotalLength)),
      encodeCbor(itemCommitment),
    ]),
  );
};

export const hashMidgardInlineScriptSourceLeaf = (input: {
  readonly sourceIndex: bigint;
  readonly scriptLanguageTag: 0 | 3 | 128;
  readonly scriptHash: Uint8Array;
  readonly scriptTotalLength: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  if (input.sourceIndex < 0n) {
    throw new Error("inline script source index must be non-negative");
  }
  if (
    input.scriptLanguageTag !== 0 &&
    input.scriptLanguageTag !== 3 &&
    input.scriptLanguageTag !== 128
  ) {
    throw new Error("inline script language tag is not supported");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "inline script source item commitment",
  );
  const scriptHash = Buffer.from(input.scriptHash);
  if (scriptHash.length !== 28) {
    throw new Error("inline script hash must contain exactly 28 bytes");
  }
  if (
    !Number.isSafeInteger(input.scriptTotalLength) ||
    input.scriptTotalLength <= 0
  ) {
    throw new Error("inline script total length must be positive");
  }
  return hash32(
    Buffer.concat([
      INLINE_SOURCE_LEAF_DOMAIN,
      encodeCbor(input.sourceIndex),
      encodeCbor(input.scriptLanguageTag),
      encodeCbor(scriptHash),
      encodeCbor(BigInt(input.scriptTotalLength)),
      encodeCbor(itemCommitment),
    ]),
  );
};
