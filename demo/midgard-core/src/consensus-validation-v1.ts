import {
  buildMidgardBoundedCollectionItemProofV1,
  type MidgardBoundedCollectionItemProofV1,
  verifyMidgardBoundedCollectionItemProofV1,
} from "./bounded-collection-v1.js";
import {
  buildMidgardBoundedItemChunkProofV1,
  midgardBoundedItemChunkCountV1,
  type MidgardBoundedItemChunkProofV1,
  verifyMidgardBoundedItemChunkProofV1,
} from "./bounded-item-v1.js";
import { decodeMidgardCekProgramEnvelopeV1 } from "./cek-proof.js";
import { asArray, asBytes, asMap, decodeSingleCbor } from "./codec/cbor.js";
import { computeHash32 } from "./codec/hash.js";
import {
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengthsV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  encodeMidgardNativeTxCanonicalV1,
  type MidgardNativeTxFullV1,
  type MidgardNativeTxProofSourceV1,
  verifyMidgardNativeTxProofSourceV1,
} from "./codec/native.js";
import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_V1_VERSION,
} from "./codec/native-constants.js";
import {
  deriveMidgardNativeFieldCollectionV1,
  reconstructMidgardNativeFieldPreimageV1,
} from "./codec/native-field-items.js";
import type { MidgardNativeScript } from "./codec/native-script.js";
import { decodeMidgardTxOutput } from "./codec/output.js";
import { midgardValueToCmlValue } from "./codec/value.js";
import { decodeMidgardVersionedScriptListPreimage } from "./codec/versioned-script.js";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "./consensus-profile-v1.js";

export const MIDGARD_TX_FIELD_RECEIPT_V1_DOMAIN = Buffer.from(
  "MidgardTxFieldReceiptV1",
  "utf8",
);

const requireExactBytes = (
  value: Uint8Array,
  expectedLength: number,
  fieldName: string,
): Buffer => {
  const bytes = Buffer.from(value);
  if (bytes.length !== expectedLength) {
    throw new Error(
      `${fieldName} must be exactly ${expectedLength.toString()} bytes`,
    );
  }
  return bytes;
};

/**
 * Derives the exact V1 receipt asset name used by the L1 validators.
 *
 * The receipt policy id is deliberately not included in the asset name: the
 * Cardano asset unit already prefixes this 32-byte name with that policy id.
 */
export const deriveMidgardTxFieldReceiptAssetNameV1 = ({
  txOrderPolicyId,
  txOrderTransactionId,
  txOrderOutputIndex,
  transactionCommitment,
  fieldIndex,
  itemIndex,
  chunkIndex,
}: {
  readonly txOrderPolicyId: Uint8Array;
  readonly txOrderTransactionId: Uint8Array;
  readonly txOrderOutputIndex: bigint;
  readonly transactionCommitment: Uint8Array;
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly chunkIndex: number;
}): Buffer => {
  const policyId = requireExactBytes(txOrderPolicyId, 28, "tx-order policy id");
  const orderTransactionId = requireExactBytes(
    txOrderTransactionId,
    32,
    "tx-order transaction id",
  );
  const commitment = requireExactBytes(
    transactionCommitment,
    32,
    "transaction commitment",
  );
  if (txOrderOutputIndex < 0n || txOrderOutputIndex > 0xffff_ffff_ffff_ffffn) {
    throw new Error("tx-order output index must fit uint64");
  }
  if (
    !Number.isSafeInteger(itemIndex) ||
    itemIndex < 0 ||
    itemIndex > Number.MAX_SAFE_INTEGER
  ) {
    throw new Error("V1 transaction field item index must be non-negative");
  }
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_V1_TX_FIELD_NAMES.length
  ) {
    throw new Error(`unknown V1 transaction field index ${fieldIndex}`);
  }
  if (
    !Number.isSafeInteger(chunkIndex) ||
    chunkIndex < 0 ||
    chunkIndex > Number.MAX_SAFE_INTEGER
  ) {
    throw new Error("V1 transaction field chunk index must be non-negative");
  }
  const outputIndex = Buffer.alloc(8);
  outputIndex.writeBigUInt64BE(txOrderOutputIndex);
  const exactChunkIndex = Buffer.alloc(8);
  exactChunkIndex.writeBigUInt64BE(BigInt(chunkIndex));
  const exactItemIndex = Buffer.alloc(8);
  exactItemIndex.writeBigUInt64BE(BigInt(itemIndex));
  return computeHash32(
    Buffer.concat([
      MIDGARD_TX_FIELD_RECEIPT_V1_DOMAIN,
      policyId,
      orderTransactionId,
      outputIndex,
      commitment,
      Buffer.from([fieldIndex]),
      exactItemIndex,
      exactChunkIndex,
    ]),
  );
};

export type MidgardConsensusV1ViolationCode =
  | "E_TX_VERSION"
  | "E_TX_SIZE"
  | "E_IS_VALID_FALSE_FORBIDDEN"
  | "E_AUX_DATA_FORBIDDEN"
  | "E_INPUT_COUNT"
  | "E_REFERENCE_INPUT_COUNT"
  | "E_OUTPUT_COUNT"
  | "E_ADDRESS_WITNESS_COUNT"
  | "E_REQUIRED_SIGNER_COUNT"
  | "E_SCRIPT_EXECUTION_COUNT"
  | "E_OBSERVER_COUNT"
  | "E_FIELD_PREIMAGE_SIZE"
  | "E_LEDGER_OUTPUT_SIZE"
  | "E_VALUE_SIZE"
  | "E_SCRIPT_PROGRAM_SIZE"
  | "E_SCRIPT_PROGRAM_ENCODING"
  | "E_NATIVE_SCRIPT_DEPTH"
  | "E_NATIVE_SCRIPT_NODE_COUNT"
  | "E_ASSET_COUNT";

export type MidgardConsensusV1Violation = {
  readonly code: MidgardConsensusV1ViolationCode;
  readonly featureId: string;
  readonly detail: string;
};

export const MIDGARD_V1_TX_FIELD_NAMES = [
  "spend_inputs",
  "reference_inputs",
  "outputs",
  "required_observers",
  "required_signers",
  "mint",
  "script_witnesses",
  "address_witnesses",
  "redeemers",
] as const;

export type MidgardV1TxFieldName = (typeof MIDGARD_V1_TX_FIELD_NAMES)[number];

export type MidgardV1TxFieldPreimage = {
  readonly fieldIndex: number;
  readonly fieldName: MidgardV1TxFieldName;
  readonly preimageCbor: Buffer;
  readonly expectedHash: Buffer;
};

export type MidgardV1TxFieldChunk = {
  readonly fieldName: MidgardV1TxFieldName;
  readonly collectionProof: MidgardBoundedCollectionItemProofV1;
  readonly proof: MidgardBoundedItemChunkProofV1;
  /** Canonical field bytes completed through this receipt-chain position. */
  readonly fieldEncodedSize: number;
};

const canonicalCborHeaderSizeV1 = (length: number): number => {
  if (!Number.isSafeInteger(length) || length < 0) {
    throw new Error(
      "canonical CBOR length must be a non-negative safe integer",
    );
  }
  if (length < 24) return 1;
  if (length <= 0xff) return 2;
  if (length <= 0xffff) return 3;
  if (length <= 0xffff_ffff) return 5;
  return 9;
};

const nativeFieldItemEncodedSizeV1 = ({
  fieldIndex,
  itemLength,
}: {
  readonly fieldIndex: number;
  readonly itemLength: number;
}): number => {
  if ([0, 1, 2, 3, 4, 7].includes(fieldIndex)) {
    return canonicalCborHeaderSizeV1(itemLength) + itemLength;
  }
  if (fieldIndex === 5) {
    if (itemLength <= 1) {
      throw new Error(
        "native-V1 mint policy item must include its pair header",
      );
    }
    return itemLength - 1;
  }
  if (fieldIndex === 6 || fieldIndex === 8) {
    return itemLength;
  }
  throw new Error(`unknown V1 transaction field index ${fieldIndex}`);
};

export const deriveMidgardV1TxFieldPreimages = (
  canonicalTransactionCbor: Uint8Array,
): readonly MidgardV1TxFieldPreimage[] => {
  const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
    canonicalTransactionCbor,
  );
  const source = deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
    canonicalTransactionCbor,
  );
  const compact = decodeMidgardNativeTxCompactV1(source.compactCbor);
  const witnessSet = decodeMidgardNativeTxWitnessSetCompactV1(
    source.witnessSetCompactCbor,
  );
  const preimages = [
    tx.body.spendInputsPreimageCbor,
    tx.body.referenceInputsPreimageCbor,
    tx.body.outputsPreimageCbor,
    tx.body.requiredObserversPreimageCbor,
    tx.body.requiredSignersPreimageCbor,
    tx.body.mintPreimageCbor,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    tx.witnessSet.addrTxWitsPreimageCbor,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  ] as const;
  const hashes = [
    compact.transactionBody.spendInputsHash,
    compact.transactionBody.referenceInputsHash,
    compact.transactionBody.outputsHash,
    compact.transactionBody.requiredObserversHash,
    compact.transactionBody.requiredSignersHash,
    compact.transactionBody.mintHash,
    witnessSet.scriptTxWitsHash,
    witnessSet.addrTxWitsHash,
    witnessSet.redeemerTxWitsHash,
  ] as const;
  return preimages.map((preimageCbor, fieldIndex) => ({
    fieldIndex,
    fieldName: MIDGARD_V1_TX_FIELD_NAMES[fieldIndex]!,
    preimageCbor: Buffer.from(preimageCbor),
    expectedHash: Buffer.from(hashes[fieldIndex]!),
  }));
};

/** Canonical field/chunk order used by L1 publication and reconstruction. */
export const deriveMidgardV1TxFieldChunks = (
  canonicalTransactionCbor: Uint8Array,
): readonly MidgardV1TxFieldChunk[] => {
  const chunks: MidgardV1TxFieldChunk[] = [];
  for (const field of deriveMidgardV1TxFieldPreimages(
    canonicalTransactionCbor,
  )) {
    const collection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: field.fieldIndex,
      preimageCbor: field.preimageCbor,
    });
    if (!collection.commitment.equals(field.expectedHash)) {
      throw new Error(
        `V1 ${field.fieldName} preimage does not match its compact commitment`,
      );
    }
    let fieldEncodedSize = canonicalCborHeaderSizeV1(collection.items.length);
    for (const [itemIndex, item] of collection.items.entries()) {
      const collectionProof = buildMidgardBoundedCollectionItemProofV1(
        collection,
        itemIndex,
      );
      for (const [chunkIndex] of item.chunkHashes.entries()) {
        if (chunkIndex + 1 === item.chunkHashes.length) {
          fieldEncodedSize += nativeFieldItemEncodedSizeV1({
            fieldIndex: field.fieldIndex,
            itemLength: item.bytes.length,
          });
        }
        chunks.push({
          fieldName: field.fieldName,
          collectionProof,
          proof: buildMidgardBoundedItemChunkProofV1(item, chunkIndex),
          fieldEncodedSize,
        });
      }
    }
    if (fieldEncodedSize !== field.preimageCbor.length) {
      throw new Error(
        `V1 ${field.fieldName} receipt state does not terminate at the committed field length`,
      );
    }
  }
  return chunks;
};

export const verifyMidgardV1TxFieldPreimage = ({
  transactionId,
  transactionCommitment,
  source,
  fieldIndex,
  preimageCbor,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSourceV1;
  readonly fieldIndex: number;
  readonly preimageCbor: Uint8Array;
}): MidgardV1TxFieldPreimage => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_V1_TX_FIELD_NAMES.length
  ) {
    throw new Error(`unknown V1 transaction field index ${fieldIndex}`);
  }
  verifyMidgardNativeTxProofSourceV1({ transactionId, source });
  const computedCommitment = computeMidgardNativeTxProofCommitmentV1(source);
  if (!computedCommitment.equals(Buffer.from(transactionCommitment))) {
    throw new Error(
      "V1 transaction field source does not match transaction commitment",
    );
  }
  const compact = decodeMidgardNativeTxCompactV1(source.compactCbor);
  const witnessSet = decodeMidgardNativeTxWitnessSetCompactV1(
    source.witnessSetCompactCbor,
  );
  const hashes = [
    compact.transactionBody.spendInputsHash,
    compact.transactionBody.referenceInputsHash,
    compact.transactionBody.outputsHash,
    compact.transactionBody.requiredObserversHash,
    compact.transactionBody.requiredSignersHash,
    compact.transactionBody.mintHash,
    witnessSet.scriptTxWitsHash,
    witnessSet.addrTxWitsHash,
    witnessSet.redeemerTxWitsHash,
  ] as const;
  const committedLength = decodeMidgardNativeTxProofFieldLengthsV1(
    source.fieldPreimageLengthsCbor,
  )[fieldIndex]!;
  if (preimageCbor.length !== committedLength) {
    throw new Error(
      `V1 ${MIDGARD_V1_TX_FIELD_NAMES[fieldIndex]} preimage length does not match its compact source: ${preimageCbor.length.toString()} != ${committedLength.toString()}`,
    );
  }
  const expectedHash = Buffer.from(hashes[fieldIndex]!);
  if (
    !deriveMidgardNativeFieldCollectionV1({
      fieldIndex,
      preimageCbor,
    }).commitment.equals(expectedHash)
  ) {
    throw new Error(
      `V1 ${MIDGARD_V1_TX_FIELD_NAMES[fieldIndex]} preimage hash mismatch`,
    );
  }
  return {
    fieldIndex,
    fieldName: MIDGARD_V1_TX_FIELD_NAMES[fieldIndex]!,
    preimageCbor: Buffer.from(preimageCbor),
    expectedHash,
  };
};

export const verifyMidgardV1TxFieldChunk = ({
  transactionId,
  transactionCommitment,
  source,
  collectionProof,
  proof,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSourceV1;
  readonly collectionProof: MidgardBoundedCollectionItemProofV1;
  readonly proof: MidgardBoundedItemChunkProofV1;
}): MidgardBoundedItemChunkProofV1 => {
  if (
    !Number.isSafeInteger(proof.fieldIndex) ||
    proof.fieldIndex < 0 ||
    proof.fieldIndex >= MIDGARD_V1_TX_FIELD_NAMES.length
  ) {
    throw new Error(`unknown V1 transaction field index ${proof.fieldIndex}`);
  }
  const compact = verifyMidgardNativeTxProofSourceV1({
    transactionId,
    source,
  });
  const computedCommitment = computeMidgardNativeTxProofCommitmentV1(source);
  if (!computedCommitment.equals(Buffer.from(transactionCommitment))) {
    throw new Error(
      "V1 transaction field source does not match transaction commitment",
    );
  }
  const witnessSet = decodeMidgardNativeTxWitnessSetCompactV1(
    source.witnessSetCompactCbor,
  );
  const commitments = [
    compact.transactionBody.spendInputsHash,
    compact.transactionBody.referenceInputsHash,
    compact.transactionBody.outputsHash,
    compact.transactionBody.requiredObserversHash,
    compact.transactionBody.requiredSignersHash,
    compact.transactionBody.mintHash,
    witnessSet.scriptTxWitsHash,
    witnessSet.addrTxWitsHash,
    witnessSet.redeemerTxWitsHash,
  ] as const;
  if (
    collectionProof.fieldIndex !== proof.fieldIndex ||
    collectionProof.itemIndex !== proof.itemIndex ||
    collectionProof.itemLength !== proof.totalLength
  ) {
    throw new Error(
      `V1 ${MIDGARD_V1_TX_FIELD_NAMES[proof.fieldIndex]} item descriptor does not match its chunk proof`,
    );
  }
  if (
    !verifyMidgardBoundedCollectionItemProofV1({
      expectedCommitment: commitments[proof.fieldIndex]!,
      proof: collectionProof,
    })
  ) {
    throw new Error(
      `V1 ${MIDGARD_V1_TX_FIELD_NAMES[proof.fieldIndex]} collection proof is invalid`,
    );
  }
  if (
    !verifyMidgardBoundedItemChunkProofV1({
      expectedCommitment: collectionProof.itemCommitment,
      proof,
    })
  ) {
    throw new Error(
      `V1 ${MIDGARD_V1_TX_FIELD_NAMES[proof.fieldIndex]} chunk proof is invalid`,
    );
  }
  return proof;
};

export const reconstructMidgardTransactionV1 = ({
  transactionId,
  transactionCommitment,
  source,
  fieldPreimages,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSourceV1;
  readonly fieldPreimages: readonly Uint8Array[];
}): Buffer => {
  if (fieldPreimages.length !== MIDGARD_V1_TX_FIELD_NAMES.length) {
    throw new Error(
      `V1 transaction reconstruction requires exactly ${MIDGARD_V1_TX_FIELD_NAMES.length.toString()} field preimages`,
    );
  }
  const verified = fieldPreimages.map((preimageCbor, fieldIndex) =>
    verifyMidgardV1TxFieldPreimage({
      transactionId,
      transactionCommitment,
      source,
      fieldIndex,
      preimageCbor,
    }),
  );
  const compact = verifyMidgardNativeTxProofSourceV1({
    transactionId,
    source,
  });
  return encodeMidgardNativeTxCanonicalV1({
    version: compact.version,
    validity: compact.validity,
    body: {
      spendInputsPreimageCbor: verified[0]!.preimageCbor,
      referenceInputsPreimageCbor: verified[1]!.preimageCbor,
      outputsPreimageCbor: verified[2]!.preimageCbor,
      fee: compact.transactionBody.fee,
      validityIntervalStart: compact.transactionBody.validityIntervalStart,
      validityIntervalEnd: compact.transactionBody.validityIntervalEnd,
      requiredObserversPreimageCbor: verified[3]!.preimageCbor,
      requiredSignersPreimageCbor: verified[4]!.preimageCbor,
      mintPreimageCbor: verified[5]!.preimageCbor,
      scriptIntegrityHash: Buffer.from(
        compact.transactionBody.scriptIntegrityHash,
      ),
      auxiliaryDataHash: Buffer.from(compact.transactionBody.auxiliaryDataHash),
      networkId: compact.transactionBody.networkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: verified[7]!.preimageCbor,
      scriptTxWitsPreimageCbor: verified[6]!.preimageCbor,
      redeemerTxWitsPreimageCbor: verified[8]!.preimageCbor,
    },
  });
};

/**
 * Reconstructs an exact canonical transaction from independently publishable
 * L1-sized chunks. The list must contain every committed chunk exactly once
 * in field-major/chunk-major order; missing, duplicate, reordered, or trailing
 * chunks fail closed.
 */
export const reconstructMidgardTransactionV1FromChunks = ({
  transactionId,
  transactionCommitment,
  source,
  chunkProofs,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSourceV1;
  readonly chunkProofs: readonly MidgardV1TxFieldChunk[];
}): Buffer => {
  verifyMidgardNativeTxProofSourceV1({ transactionId, source });
  if (
    !computeMidgardNativeTxProofCommitmentV1(source).equals(
      Buffer.from(transactionCommitment),
    )
  ) {
    throw new Error(
      "V1 transaction chunk source does not match transaction commitment",
    );
  }
  const lengths = decodeMidgardNativeTxProofFieldLengthsV1(
    source.fieldPreimageLengthsCbor,
  );
  let cursor = 0;
  const fieldPreimages = lengths.map((committedLength, fieldIndex) => {
    const items: Buffer[] = [];
    let expectedItemCount: number | null = null;
    let itemIndex = 0;
    while (true) {
      const first = chunkProofs[cursor];
      if (
        first === undefined ||
        first.proof.fieldIndex !== fieldIndex ||
        first.proof.itemIndex !== itemIndex
      ) {
        break;
      }
      expectedItemCount ??= first.collectionProof.itemCount;
      if (first.collectionProof.itemCount !== expectedItemCount) {
        throw new Error("V1 transaction item count changed within a field");
      }
      const chunkCount = midgardBoundedItemChunkCountV1(
        first.proof.totalLength,
      );
      const chunks: Buffer[] = [];
      for (let chunkIndex = 0; chunkIndex < chunkCount; chunkIndex += 1) {
        const entry = chunkProofs[cursor];
        if (
          entry === undefined ||
          entry.proof.fieldIndex !== fieldIndex ||
          entry.proof.itemIndex !== itemIndex ||
          entry.proof.chunkIndex !== chunkIndex ||
          entry.collectionProof.itemIndex !== itemIndex
        ) {
          throw new Error(
            `V1 transaction chunk sequence diverges at field ${fieldIndex}, item ${itemIndex}, chunk ${chunkIndex}`,
          );
        }
        verifyMidgardV1TxFieldChunk({
          transactionId,
          transactionCommitment,
          source,
          collectionProof: entry.collectionProof,
          proof: entry.proof,
        });
        chunks.push(Buffer.from(entry.proof.chunk));
        cursor += 1;
      }
      items.push(Buffer.concat(chunks));
      itemIndex += 1;
      if (itemIndex === expectedItemCount) {
        break;
      }
    }
    const preimage = reconstructMidgardNativeFieldPreimageV1({
      fieldIndex,
      items,
    });
    if (preimage.length !== committedLength) {
      throw new Error(
        `V1 transaction field ${fieldIndex.toString()} reconstructed length mismatch`,
      );
    }
    return preimage;
  });
  if (cursor !== chunkProofs.length) {
    throw new Error("V1 transaction reconstruction has trailing chunks");
  }
  return reconstructMidgardTransactionV1({
    transactionId,
    transactionCommitment,
    source,
    fieldPreimages,
  });
};

const violation = (
  code: MidgardConsensusV1ViolationCode,
  featureId: string,
  detail: string,
): MidgardConsensusV1Violation => ({ code, featureId, detail });

const enforceCount = (
  count: number,
  maximum: number,
  code: MidgardConsensusV1ViolationCode,
  featureId: string,
): MidgardConsensusV1Violation | null =>
  count <= maximum
    ? null
    : violation(code, featureId, `${count.toString()} > ${maximum.toString()}`);

const enforcePreimageSize = (
  bytes: Uint8Array,
  maximum: number,
  featureId: string,
): MidgardConsensusV1Violation | null =>
  bytes.length <= maximum
    ? null
    : violation(
        "E_FIELD_PREIMAGE_SIZE",
        featureId,
        `${bytes.length.toString()} > ${maximum.toString()}`,
      );

type NativeScriptComplexity = {
  readonly depth: number;
  readonly nodeCount: number;
};

const nativeScriptComplexity = (
  script: MidgardNativeScript,
): NativeScriptComplexity => {
  switch (script.type) {
    case "sig":
    case "after":
    case "before":
      return { depth: 1, nodeCount: 1 };
    case "all":
    case "any":
    case "atLeast": {
      let depth = 1;
      let nodeCount = 1;
      for (const child of script.scripts) {
        const childComplexity = nativeScriptComplexity(child);
        depth = Math.max(depth, childComplexity.depth + 1);
        nodeCount += childComplexity.nodeCount;
      }
      return { depth, nodeCount };
    }
  }
};

const nativeScriptBoundViolation = (
  script: MidgardNativeScript,
  featureId: string,
): MidgardConsensusV1Violation | null => {
  const complexity = nativeScriptComplexity(script);
  if (complexity.depth > MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptDepth) {
    return violation(
      "E_NATIVE_SCRIPT_DEPTH",
      featureId,
      `${complexity.depth.toString()} > ${MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptDepth.toString()}`,
    );
  }
  if (
    complexity.nodeCount > MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptNodeCount
  ) {
    return violation(
      "E_NATIVE_SCRIPT_NODE_COUNT",
      featureId,
      `${complexity.nodeCount.toString()} > ${MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptNodeCount.toString()}`,
    );
  }
  return null;
};

/**
 * Enforces the proof-fit bounds that can be checked from canonical V1 bytes.
 * Semantic validity remains the responsibility of ValidationMachineV1.
 */
export const validateMidgardConsensusV1Tx = (
  tx: MidgardNativeTxFullV1,
  canonicalCborByteLength: number,
): MidgardConsensusV1Violation | null => {
  const limits = MIDGARD_CONSENSUS_LIMITS_V1;
  if (tx.version !== MIDGARD_NATIVE_TX_V1_VERSION) {
    return violation(
      "E_TX_VERSION",
      "native_transaction_version",
      `V1 profile requires native transaction version ${MIDGARD_NATIVE_TX_V1_VERSION.toString()}, got ${tx.version.toString()}`,
    );
  }
  if (canonicalCborByteLength > limits.maxTxCanonicalCborBytes) {
    return violation(
      "E_TX_SIZE",
      "transaction_size",
      `${canonicalCborByteLength.toString()} > ${limits.maxTxCanonicalCborBytes.toString()}`,
    );
  }
  if (tx.validity !== "TxIsValid") {
    return violation(
      "E_IS_VALID_FALSE_FORBIDDEN",
      "transaction_validity",
      `user transaction admission requires TxIsValid, got ${tx.validity}`,
    );
  }
  if (!tx.body.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
    return violation(
      "E_AUX_DATA_FORBIDDEN",
      "auxiliary_data",
      "V1 has no authenticated auxiliary-data preimage",
    );
  }

  const boundedPreimages = [
    [
      tx.body.spendInputsPreimageCbor,
      limits.maxSpendInputsPreimageBytes,
      "spend_inputs_preimage",
    ],
    [
      tx.body.referenceInputsPreimageCbor,
      limits.maxReferenceInputsPreimageBytes,
      "reference_inputs_preimage",
    ],
    [
      tx.body.outputsPreimageCbor,
      limits.maxOutputsPreimageBytes,
      "outputs_preimage",
    ],
    [
      tx.body.requiredObserversPreimageCbor,
      limits.maxRequiredObserversPreimageBytes,
      "required_observers_preimage",
    ],
    [
      tx.body.requiredSignersPreimageCbor,
      limits.maxRequiredSignersPreimageBytes,
      "required_signers_preimage",
    ],
    [tx.body.mintPreimageCbor, limits.maxMintPreimageBytes, "mint_preimage"],
    [
      tx.witnessSet.addrTxWitsPreimageCbor,
      limits.maxAddressWitnessesPreimageBytes,
      "address_witnesses_preimage",
    ],
    [
      tx.witnessSet.scriptTxWitsPreimageCbor,
      limits.maxScriptWitnessesPreimageBytes,
      "script_witnesses_preimage",
    ],
    [
      tx.witnessSet.redeemerTxWitsPreimageCbor,
      limits.maxRedeemersPreimageBytes,
      "redeemers_preimage",
    ],
  ] as const;
  for (const [bytes, maximum, featureId] of boundedPreimages) {
    const bounded = enforcePreimageSize(bytes, maximum, featureId);
    if (bounded !== null) return bounded;
  }

  const spendInputs = decodeMidgardNativeByteListPreimage(
    tx.body.spendInputsPreimageCbor,
    "native.inputs",
  );
  let bounded = enforceCount(
    spendInputs.length,
    limits.maxSpendInputCount,
    "E_INPUT_COUNT",
    "spend_inputs",
  );
  if (bounded !== null) return bounded;

  const referenceInputs = decodeMidgardNativeByteListPreimage(
    tx.body.referenceInputsPreimageCbor,
    "native.reference_inputs",
  );
  bounded = enforceCount(
    referenceInputs.length,
    limits.maxReferenceInputCount,
    "E_REFERENCE_INPUT_COUNT",
    "reference_inputs",
  );
  if (bounded !== null) return bounded;

  const outputCbors = decodeMidgardNativeByteListPreimage(
    tx.body.outputsPreimageCbor,
    "native.outputs",
  );
  bounded = enforceCount(
    outputCbors.length,
    limits.maxOutputCount,
    "E_OUTPUT_COUNT",
    "outputs",
  );
  if (bounded !== null) return bounded;

  const addressWitnesses = decodeMidgardNativeByteListPreimage(
    tx.witnessSet.addrTxWitsPreimageCbor,
    "native.address_witnesses",
  );
  bounded = enforceCount(
    addressWitnesses.length,
    limits.maxAddressWitnessCount,
    "E_ADDRESS_WITNESS_COUNT",
    "address_witnesses",
  );
  if (bounded !== null) return bounded;

  const requiredSigners = decodeMidgardNativeByteListPreimage(
    tx.body.requiredSignersPreimageCbor,
    "native.required_signers",
  );
  bounded = enforceCount(
    requiredSigners.length,
    limits.maxRequiredSignerCount,
    "E_REQUIRED_SIGNER_COUNT",
    "required_signers",
  );
  if (bounded !== null) return bounded;

  const observers = decodeMidgardNativeByteListPreimage(
    tx.body.requiredObserversPreimageCbor,
    "native.required_observers",
  );
  bounded = enforceCount(
    observers.length,
    limits.maxRequiredObserverCount,
    "E_OBSERVER_COUNT",
    "required_observers",
  );
  if (bounded !== null) return bounded;

  const redeemerCbors = asArray(
    decodeSingleCbor(tx.witnessSet.redeemerTxWitsPreimageCbor),
    "native.redeemers",
  );
  bounded = enforceCount(
    redeemerCbors.length,
    limits.maxScriptExecutionCount,
    "E_SCRIPT_EXECUTION_COUNT",
    "redeemers",
  );
  if (bounded !== null) return bounded;
  const scripts = decodeMidgardVersionedScriptListPreimage(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  );
  for (let index = 0; index < scripts.length; index += 1) {
    const script = scripts[index]!;
    if (script.language === "NativeCardano") {
      const nativeBound = nativeScriptBoundViolation(
        script.nativeScript,
        `script_witnesses[${index.toString()}]`,
      );
      if (nativeBound !== null) return nativeBound;
    } else {
      try {
        decodeMidgardCekProgramEnvelopeV1(script.scriptBytes);
      } catch (error) {
        return violation(
          "E_SCRIPT_PROGRAM_ENCODING",
          "script_witnesses",
          `script[${index.toString()}] is not a canonical bounded V1 program envelope: ${String(error)}`,
        );
      }
    }
  }

  const distinctAssets = new Set<string>();
  for (let index = 0; index < outputCbors.length; index += 1) {
    if (outputCbors[index]!.length > limits.maxLedgerOutputPreimageBytes) {
      return violation(
        "E_LEDGER_OUTPUT_SIZE",
        "ledger_output_preimage",
        `output[${index.toString()}] ${outputCbors[index]!.length.toString()} > ${limits.maxLedgerOutputPreimageBytes.toString()}`,
      );
    }
    const output = decodeMidgardTxOutput(outputCbors[index]!);
    const cardanoValueBytes = midgardValueToCmlValue(
      output.value,
    ).to_cbor_bytes().length;
    if (cardanoValueBytes > limits.maxOutputValueCborBytes) {
      return violation(
        "E_VALUE_SIZE",
        "output_value",
        `output[${index.toString()}] Cardano Value ${cardanoValueBytes.toString()} > ${limits.maxOutputValueCborBytes.toString()}`,
      );
    }
    for (const [policyId, assets] of output.value.assets) {
      for (const assetName of assets.keys()) {
        distinctAssets.add(`${policyId}.${assetName}`);
      }
    }
    if (output.script_ref?.language === "NativeCardano") {
      const nativeBound = nativeScriptBoundViolation(
        output.script_ref.nativeScript,
        `reference_scripts[${index.toString()}]`,
      );
      if (nativeBound !== null) return nativeBound;
    } else if (output.script_ref !== undefined) {
      try {
        decodeMidgardCekProgramEnvelopeV1(output.script_ref.scriptBytes);
      } catch (error) {
        return violation(
          "E_SCRIPT_PROGRAM_ENCODING",
          "reference_scripts",
          `output[${index.toString()}] reference script is not a canonical bounded V1 program envelope: ${String(error)}`,
        );
      }
    }
  }
  const mintValue = decodeSingleCbor(tx.body.mintPreimageCbor);
  if (!Array.isArray(mintValue)) {
    for (const [policyValue, assetsValue] of asMap(mintValue, "native.mint")) {
      const policyId = asBytes(policyValue, "native.mint.policy").toString(
        "hex",
      );
      for (const assetNameValue of asMap(
        assetsValue,
        "native.mint.assets",
      ).keys()) {
        const assetName = asBytes(
          assetNameValue,
          "native.mint.asset_name",
        ).toString("hex");
        distinctAssets.add(`${policyId}.${assetName}`);
      }
    }
  }
  if (distinctAssets.size > limits.maxDistinctAssetCount) {
    return violation(
      "E_ASSET_COUNT",
      "distinct_assets",
      `${distinctAssets.size.toString()} > ${limits.maxDistinctAssetCount.toString()}`,
    );
  }
  return null;
};

export const validateMidgardConsensusV1TxCbor = (
  txCbor: Uint8Array,
): MidgardConsensusV1Violation | null =>
  validateMidgardConsensusV1Tx(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor),
    txCbor.length,
  );
