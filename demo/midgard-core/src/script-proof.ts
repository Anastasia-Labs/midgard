import { blake2b } from "@noble/hashes/blake2.js";

import {
  decodeMidgardCekProgramEnvelopeV1,
  type MidgardCekProgramEnvelopeV1,
} from "./cek-proof.js";
import { buildMidgardBoundedItemV1 } from "./bounded-item-v1.js";
import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  decodeMidgardNativeByteListPreimage,
  type MidgardNativeTxCanonicalV1,
} from "./codec/native.js";
import { decodeMidgardTxOutput } from "./codec/output.js";
import {
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
  MidgardVersionedScriptTags,
} from "./codec/versioned-script.js";

const SOURCE_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptSourceLeafV1",
  "ascii",
);
const INLINE_SOURCE_LEAF_DOMAIN = Buffer.from(
  "MidgardInlineScriptSourceLeafV1",
  "ascii",
);
const REDEEMER_LEAF_DOMAIN = Buffer.from("MidgardRedeemerLeafV1", "ascii");
const PURPOSE_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptPurposeLeafV1",
  "ascii",
);
const SIGNER_LEAF_DOMAIN = Buffer.from("MidgardSignerLeafV1", "ascii");
const OUTPUT_LEAF_DOMAIN = Buffer.from("MidgardOutputLeafV1", "ascii");
const EXECUTION_LEAF_DOMAIN = Buffer.from(
  "MidgardScriptExecutionLeafV1",
  "ascii",
);
const MINT_ASSET_LEAF_DOMAIN = Buffer.from(
  "MidgardMintAssetLeafV1",
  "ascii",
);
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
 * Validates V1's meaning of non-native script bytes.
 */
export const decodeMidgardV1ScriptProgramEnvelope = (
  script: MidgardVersionedScript,
): MidgardCekProgramEnvelopeV1 | null =>
  script.language === "NativeCardano"
    ? null
    : decodeMidgardCekProgramEnvelopeV1(script.scriptBytes);

/**
 * Computes a V1 script credential only after the executable program
 * envelope is canonical and within the L1/DA proof bounds.
 */
export const hashMidgardV1VersionedScript = (
  script: MidgardVersionedScript,
): string => {
  decodeMidgardV1ScriptProgramEnvelope(script);
  return hashMidgardVersionedScript(script);
};

/**
 * Collects every V1 program physically attached to a canonical
 * transaction: inline witness scripts and reference scripts created by its
 * outputs. Programs reached through reference inputs are resolved from ledger
 * state later and therefore are intentionally not inferred here.
 */
export const collectMidgardV1AttachedProgramEnvelopes = (
  tx: MidgardNativeTxCanonicalV1,
): readonly MidgardCekProgramEnvelopeV1[] => {
  const envelopes: MidgardCekProgramEnvelopeV1[] = [];
  for (const script of decodeMidgardVersionedScriptListPreimage(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  )) {
    const envelope = decodeMidgardV1ScriptProgramEnvelope(script);
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
    const envelope = decodeMidgardV1ScriptProgramEnvelope(script);
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
export const collectMidgardV1ReferencedProgramEnvelopes = (
  tx: MidgardNativeTxCanonicalV1,
  resolvedOutputsByOutRef: ReadonlyMap<string, Uint8Array>,
): readonly MidgardCekProgramEnvelopeV1[] => {
  const envelopes: MidgardCekProgramEnvelopeV1[] = [];
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
    const envelope = decodeMidgardV1ScriptProgramEnvelope(script);
    if (envelope !== null) envelopes.push(envelope);
  }
  return Object.freeze(envelopes);
};

export const hashMidgardScriptSourceLeafV1 = (input: {
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
    const item = buildMidgardBoundedItemV1({
      fieldIndex: 7,
      itemIndex: Number(sourceIndex),
      bytes: encodeMidgardVersionedScript(input.script),
    });
    return hashMidgardInlineScriptSourceLeafV1({
      sourceIndex,
      itemCommitment: item.commitment,
    });
  }
  const scriptHash = Buffer.from(
    hashMidgardVersionedScript(input.script),
    "hex",
  );
  return hash32(
    Buffer.concat([
      SOURCE_LEAF_DOMAIN,
      encodeCbor(1n),
      encodeCbor(Buffer.from(input.sourceKey)),
      encodeCbor(MidgardVersionedScriptTags[input.script.language]),
      encodeCbor(scriptHash),
    ]),
  );
};

export const hashMidgardInlineScriptSourceLeafV1 = (input: {
  readonly sourceIndex: bigint;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  if (input.sourceIndex < 0n) {
    throw new Error("inline script source index must be non-negative");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "inline script source item commitment",
  );
  return hash32(
    Buffer.concat([
      INLINE_SOURCE_LEAF_DOMAIN,
      encodeCbor(input.sourceIndex),
      encodeCbor(itemCommitment),
    ]),
  );
};

export const hashMidgardRedeemerItemLeafV1 = (input: {
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

export const hashMidgardRedeemerLeafV1 = (input: {
  readonly redeemerIndex: number;
  readonly canonicalRedeemerWitnessCbor: Uint8Array;
}): Hash32 => {
  const item = buildMidgardBoundedItemV1({
    fieldIndex: 8,
    itemIndex: input.redeemerIndex,
    bytes: input.canonicalRedeemerWitnessCbor,
  });
  return hashMidgardRedeemerItemLeafV1({
    redeemerIndex: input.redeemerIndex,
    itemCommitment: item.commitment,
  });
};

export const hashMidgardScriptPurposeLeafV1 = (input: {
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

export const hashMidgardSignerLeafV1 = (
  signerHash: Uint8Array,
): Hash32 => {
  const exactSignerHash = Buffer.from(signerHash);
  if (exactSignerHash.length !== 28) {
    throw new Error("signer hash must contain exactly 28 bytes");
  }
  return hash32(
    Buffer.concat([SIGNER_LEAF_DOMAIN, encodeCbor(exactSignerHash)]),
  );
};

export const hashMidgardOutputLeafV1 = (input: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.outputIndex) || input.outputIndex < 0) {
    throw new Error("output index must be a non-negative safe integer");
  }
  return hash32(
    Buffer.concat([
      OUTPUT_LEAF_DOMAIN,
      encodeCbor(BigInt(input.outputIndex)),
      encodeCbor(Buffer.from(input.outputCbor)),
    ]),
  );
};

export const hashMidgardScriptExecutionLeafV1 = (input: {
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

export const hashMidgardMintAssetLeafV1 = (input: {
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

export const hashMidgardScriptContextItemLeafV1 = (input: {
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
    throw new Error(
      "script-context item length and memory must be unsigned",
    );
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

export const hashMidgardResolvedContextItemLeafV1 = (input: {
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
