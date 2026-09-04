import { blake2b } from "@noble/hashes/blake2.js";

import { buildMidgardBoundedItem } from "./bounded-item.js";
import {
  decodeMidgardCekProgramEnvelope,
  type MidgardCekProgramEnvelope,
} from "./cek-proof.js";
import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  decodeMidgardNativeByteListPreimage,
  type MidgardNativeTxCanonical,
} from "./codec/native.js";
import { decodeMidgardSpendInputItem } from "./codec/native-tx-field-item-decoders.js";
import { encodeMidgardSpendInputItem } from "./codec/native-tx-field-items.js";
import { decodeMidgardTxOutput } from "./codec/output.js";
import {
  decodeMidgardVersionedScriptListPreimage,
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
const REDEEMER_LEAF_DOMAIN = Buffer.from("MidgardRedeemerLeafV1", "ascii");
const PURPOSE_LEAF_DOMAIN = Buffer.from("MidgardScriptPurposeLeafV1", "ascii");
const SIGNER_LEAF_DOMAIN = Buffer.from("MidgardSignerLeafV1", "ascii");
const OUTPUT_ITEM_LEAF_DOMAIN = Buffer.from("MidgardOutputItemLeafV1", "ascii");
const OUTPUT_DESCRIPTOR_LEAF_DOMAIN = Buffer.from(
  "MidgardOutputDescriptorLeafV1",
  "ascii",
);
const EXECUTION_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptExecutionLeafV1",
  "ascii",
);
const MINT_ASSET_LEAF_DOMAIN = Buffer.from("MidgardMintAssetLeafV1", "ascii");
const SCRIPT_CONTEXT_ITEM_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptContextItemLeafV1",
  "ascii",
);
const RESOLVED_CONTEXT_ITEM_LEAF_DOMAIN = Buffer.from(
  "MidgardResolvedContextItemLeafV1",
  "ascii",
);

const hash32 = (bytes: Uint8Array): Hash32 =>
  ensureHash32(blake2b(bytes, { dkLen: 32 }), "script_proof_hash");

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

/**
 * Validates V1's meaning of non-native script bytes.
 */
export const decodeMidgardScriptProgramEnvelope = (
  script: MidgardVersionedScript,
): MidgardCekProgramEnvelope | null =>
  script.language === "NativeCardano"
    ? null
    : decodeMidgardCekProgramEnvelope(script.scriptBytes);

/**
 * Computes a V1 script credential only after the executable program
 * envelope is canonical and within the L1/DA proof bounds.
 */
export const hashMidgardV1VersionedScript = (
  script: MidgardVersionedScript,
): string => {
  if (decodeMidgardScriptProgramEnvelope(script) === null) {
    throw new Error(
      "NativeCardano scripts are not canonical Midgard V1 program envelopes",
    );
  }
  return hashMidgardVersionedScript(script);
};

/**
 * Collects every V1 program physically attached to a canonical
 * transaction: inline witness scripts and reference scripts created by its
 * outputs. Programs reached through reference inputs are resolved from ledger
 * state later and therefore are intentionally not inferred here.
 */
export const collectMidgardAttachedProgramEnvelopes = (
  tx: MidgardNativeTxCanonical,
): readonly MidgardCekProgramEnvelope[] => {
  const envelopes: MidgardCekProgramEnvelope[] = [];
  for (const script of decodeMidgardVersionedScriptListPreimage(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  )) {
    const envelope = decodeMidgardScriptProgramEnvelope(script);
    if (envelope !== null) envelopes.push(envelope);
  }
  const outputs = decodeSingleCbor(tx.body.outputsPreimageCbor);
  if (!Array.isArray(outputs)) {
    throw new Error("outputs preimage must be an array");
  }
  for (const [index, outputCbor] of outputs.entries()) {
    if (!(outputCbor instanceof Uint8Array)) {
      throw new Error(`output ${index.toString()} must be a CBOR byte string`);
    }
    const script = decodeMidgardTxOutput(outputCbor).script_ref;
    if (script === undefined) continue;
    const envelope = decodeMidgardScriptProgramEnvelope(script);
    if (envelope !== null) envelopes.push(envelope);
  }
  return Object.freeze(envelopes);
};

/**
 * Resolves V1 programs made available by the transaction's reference
 * inputs. Map keys are the lowercase hex encoding of the exact canonical
 * output-reference CBOR bytes committed by the transaction; values are the
 * corresponding canonical ledger-output bytes.
 */
export const collectMidgardReferencedProgramEnvelopes = (
  tx: MidgardNativeTxCanonical,
  resolvedOutputsByOutRef: ReadonlyMap<string, Uint8Array>,
): readonly MidgardCekProgramEnvelope[] => {
  const envelopes: MidgardCekProgramEnvelope[] = [];
  const referenceInputs = decodeMidgardNativeByteListPreimage(
    tx.body.referenceInputsPreimageCbor,
    "reference_inputs_preimage",
  );
  for (const [index, outRef] of referenceInputs.entries()) {
    const key = Buffer.from(outRef).toString("hex");
    const outputCbor = resolvedOutputsByOutRef.get(key);
    if (outputCbor === undefined) {
      throw new Error(
        `reference input ${index.toString()} (${key}) has no resolved ledger output`,
      );
    }
    const script = decodeMidgardTxOutput(outputCbor).script_ref;
    if (script === undefined) continue;
    const envelope = decodeMidgardScriptProgramEnvelope(script);
    if (envelope !== null) envelopes.push(envelope);
  }
  return Object.freeze(envelopes);
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

export const hashMidgardRedeemerItemLeaf = (input: {
  readonly redeemerIndex: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.redeemerIndex) || input.redeemerIndex < 0) {
    throw new Error("redeemer item index must be a non-negative safe integer");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "redeemer item commitment",
  );
  return hash32(
    Buffer.concat([
      REDEEMER_LEAF_DOMAIN,
      encodeCbor(BigInt(input.redeemerIndex)),
      encodeCbor(itemCommitment),
    ]),
  );
};

export const hashMidgardRedeemerLeaf = (input: {
  readonly redeemerIndex: number;
  readonly canonicalRedeemerWitnessCbor: Uint8Array;
}): Hash32 => {
  const item = buildMidgardBoundedItem({
    fieldIndex: 8,
    itemIndex: input.redeemerIndex,
    bytes: input.canonicalRedeemerWitnessCbor,
  });
  return hashMidgardRedeemerItemLeaf({
    redeemerIndex: input.redeemerIndex,
    itemCommitment: item.commitment,
  });
};

export const hashMidgardScriptPurposeLeaf = (input: {
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Uint8Array;
  readonly subject: Uint8Array;
}): Hash32 => {
  if (input.purposeIndex < 0n) {
    throw new Error("script purpose index must be non-negative");
  }
  const scriptHash = Buffer.from(input.scriptHash);
  if (scriptHash.length !== 28) {
    throw new Error("script purpose hash must contain exactly 28 bytes");
  }
  return hash32(
    Buffer.concat([
      PURPOSE_LEAF_DOMAIN,
      encodeCbor(BigInt(input.purposeKind)),
      encodeCbor(input.purposeIndex),
      encodeCbor(scriptHash),
      encodeCbor(Buffer.from(input.subject)),
    ]),
  );
};

export const hashMidgardSignerLeaf = (signerHash: Uint8Array): Hash32 => {
  const exactSignerHash = Buffer.from(signerHash);
  if (exactSignerHash.length !== 28) {
    throw new Error("signer hash must contain exactly 28 bytes");
  }
  return hash32(
    Buffer.concat([SIGNER_LEAF_DOMAIN, encodeCbor(exactSignerHash)]),
  );
};

export const hashMidgardOutputItemLeaf = (input: {
  readonly outputIndex: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.outputIndex) || input.outputIndex < 0) {
    throw new Error("output index must be a non-negative safe integer");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "output item commitment",
  );
  return hash32(
    Buffer.concat([
      OUTPUT_ITEM_LEAF_DOMAIN,
      encodeCbor(BigInt(input.outputIndex)),
      encodeCbor(itemCommitment),
    ]),
  );
};

export const hashMidgardOutputLeaf = (input: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): Hash32 => {
  const item = buildMidgardBoundedItem({
    fieldIndex: 2,
    itemIndex: input.outputIndex,
    bytes: input.outputCbor,
  });
  return hashMidgardOutputItemLeaf({
    outputIndex: input.outputIndex,
    itemCommitment: item.commitment,
  });
};

/**
 * Commits the exact compact ledger descriptor derived by the bounded output
 * proof. Full output bytes remain available through transaction DA.
 */
export const hashMidgardOutputDescriptorLeaf = (input: {
  readonly outputIndex: number;
  readonly descriptorCbor: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.outputIndex) || input.outputIndex < 0) {
    throw new Error("output index must be a non-negative safe integer");
  }
  return hash32(
    Buffer.concat([
      OUTPUT_DESCRIPTOR_LEAF_DOMAIN,
      encodeCbor(BigInt(input.outputIndex)),
      encodeCbor(Buffer.from(input.descriptorCbor)),
    ]),
  );
};

export const hashMidgardScriptExecutionLeaf = (input: {
  readonly languageTag: 0 | 3 | 128;
  readonly purposeLeaf: Uint8Array;
  readonly sourceLeaf: Uint8Array;
  readonly redeemerLeaf?: Uint8Array;
}): Hash32 => {
  const purposeLeaf = Buffer.from(input.purposeLeaf);
  const sourceLeaf = Buffer.from(input.sourceLeaf);
  const redeemerLeaf = Buffer.from(input.redeemerLeaf ?? []);
  if (purposeLeaf.length !== 32 || sourceLeaf.length !== 32) {
    throw new Error("script execution leaves must contain exactly 32 bytes");
  }
  if (redeemerLeaf.length !== 0 && redeemerLeaf.length !== 32) {
    throw new Error(
      "script execution redeemer leaf must be empty or exactly 32 bytes",
    );
  }
  return hash32(
    Buffer.concat([
      EXECUTION_LEAF_DOMAIN,
      encodeCbor(BigInt(input.languageTag)),
      encodeCbor(purposeLeaf),
      encodeCbor(sourceLeaf),
      encodeCbor(redeemerLeaf),
    ]),
  );
};

export const hashMidgardMintAssetLeaf = (input: {
  readonly policyId: Uint8Array;
  readonly assetName: Uint8Array;
  readonly quantity: bigint;
}): Hash32 => {
  const policyId = Buffer.from(input.policyId);
  const assetName = Buffer.from(input.assetName);
  if (policyId.length !== 28) {
    throw new Error("mint policy id must contain exactly 28 bytes");
  }
  if (assetName.length > 32) {
    throw new Error("mint asset name must contain at most 32 bytes");
  }
  if (input.quantity === 0n) {
    throw new Error("mint quantity must be non-zero");
  }
  return hash32(
    Buffer.concat([
      MINT_ASSET_LEAF_DOMAIN,
      encodeCbor(policyId),
      encodeCbor(assetName),
      encodeCbor(input.quantity),
    ]),
  );
};

export const hashMidgardScriptContextItemLeaf = (input: {
  readonly collectionKind: number;
  readonly itemIndex: number;
  readonly semanticRoot: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}): Hash32 => {
  if (
    !Number.isSafeInteger(input.collectionKind) ||
    input.collectionKind < 0 ||
    input.collectionKind > 7
  ) {
    throw new Error(
      "script-context collection kind must be between zero and seven",
    );
  }
  if (!Number.isSafeInteger(input.itemIndex) || input.itemIndex < 0) {
    throw new Error(
      "script-context item index must be a non-negative safe integer",
    );
  }
  const root = Buffer.from(input.semanticRoot);
  if (root.length !== 32) {
    throw new Error(
      "script-context semantic root must contain exactly 32 bytes",
    );
  }
  if (input.cborLength < 0n || input.memory < 0n) {
    throw new Error("script-context item length and memory must be unsigned");
  }
  return hash32(
    Buffer.concat([
      SCRIPT_CONTEXT_ITEM_LEAF_DOMAIN,
      encodeCbor(BigInt(input.collectionKind)),
      encodeCbor(BigInt(input.itemIndex)),
      encodeCbor(root),
      encodeCbor(input.cborLength),
      encodeCbor(input.memory),
    ]),
  );
};

export const hashMidgardResolvedContextItemLeaf = (input: {
  readonly sourceKind: "spend" | "reference";
  readonly itemIndex: number;
  readonly key: Uint8Array;
  readonly outputCbor: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.itemIndex) || input.itemIndex < 0) {
    throw new Error(
      "resolved context item index must be a non-negative safe integer",
    );
  }
  return hash32(
    Buffer.concat([
      RESOLVED_CONTEXT_ITEM_LEAF_DOMAIN,
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(BigInt(input.itemIndex)),
      encodeCbor(Buffer.from(input.key)),
      encodeCbor(Buffer.from(input.outputCbor)),
    ]),
  );
};
