import {
  buildMidgardBoundedItem,
  type MidgardBoundedItemChunkProof,
  verifyMidgardBoundedItemChunkProof,
} from "./bounded-item.js";
import { encodeMidgardAddressBytes } from "./codec/address.js";
import { midgardBlake2b } from "./codec/blake2b.js";
import { decodeSingleCbor, encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  decodedBigInt,
  decodedSafeInteger,
  decodeSummary,
  encodeSummary,
  exactAssetCount,
  exactBytes,
  exactCardanoValueSize,
  exactLength,
  exactOutputIndex,
  exactReferenceScript,
  exactUint64,
  LEDGER_OUTPUT_ASSET_LEAF_DOMAIN,
  MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  type MidgardLedgerOutputAsset,
  type MidgardLedgerOutputAssetFrontier,
  type MidgardLedgerOutputCommitment,
  type MidgardLedgerOutputCommitmentFacts,
  type MidgardLedgerOutputMaterial,
} from "./ledger-output-commitment.exact-reference-script.js";
import {
  buildMidgardValidationMerkleFrontier,
  commitMidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

export const encodeMidgardLedgerOutputCommitment = (
  descriptor: MidgardLedgerOutputCommitment,
): Buffer => {
  if (descriptor.version !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION) {
    throw new Error("Invalid V1 ledger output commitment version");
  }
  const address = encodeMidgardAddressBytes(descriptor.address);
  const referenceScript = exactReferenceScript({
    language: descriptor.referenceScriptLanguage,
    hash: descriptor.referenceScriptHash,
    totalLength: descriptor.referenceScriptTotalLength,
    itemCommitment: descriptor.referenceScriptItemCommitment,
  });
  return encodeCbor([
    BigInt(MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION),
    BigInt(exactOutputIndex(descriptor.outputIndex)),
    BigInt(exactLength(descriptor.totalLength)),
    ensureHash32(
      descriptor.itemCommitment,
      "ledger_output_commitment_v1.item_commitment",
    ),
    address,
    exactUint64(descriptor.lovelace, "ledger_output_commitment_v1.lovelace"),
    BigInt(exactAssetCount(descriptor.assetCount)),
    ensureHash32(
      descriptor.assetFrontierCommitment,
      "ledger_output_commitment_v1.asset_frontier_commitment",
    ),
    BigInt(exactCardanoValueSize(descriptor.cardanoValueSize)),
    BigInt(referenceScript.language),
    referenceScript.hash,
    BigInt(referenceScript.totalLength),
    referenceScript.itemCommitment,
    encodeSummary(descriptor.cardanoTxOut, "cardano_tx_out"),
    encodeSummary(descriptor.midgardTxOut, "midgard_tx_out"),
    encodeSummary(descriptor.cardanoSpendDatum, "cardano_spend_datum"),
  ]);
};

export const decodeMidgardLedgerOutputCommitment = (
  bytes: Uint8Array,
): MidgardLedgerOutputCommitment => {
  const value = decodeSingleCbor(bytes);
  if (
    !Array.isArray(value) ||
    value.length !== 16 ||
    !(value[3] instanceof Uint8Array) ||
    !(value[4] instanceof Uint8Array)
  ) {
    throw new Error("Invalid V1 ledger output commitment descriptor");
  }
  const decodedVersion = decodedSafeInteger(value[0]);
  if (decodedVersion !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION) {
    throw new Error("Invalid V1 ledger output commitment version");
  }
  const referenceScript = exactReferenceScript({
    language: decodedSafeInteger(value[9]),
    hash: value[10],
    totalLength: decodedSafeInteger(value[11]),
    itemCommitment: value[12],
  });
  const descriptor: MidgardLedgerOutputCommitment = {
    version: MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
    outputIndex: exactOutputIndex(decodedSafeInteger(value[1])),
    totalLength: exactLength(decodedSafeInteger(value[2])),
    itemCommitment: ensureHash32(
      value[3],
      "ledger_output_commitment_v1.item_commitment",
    ),
    address: encodeMidgardAddressBytes(value[4]),
    lovelace: exactUint64(
      decodedBigInt(value[5], "ledger output lovelace"),
      "ledger_output_commitment_v1.lovelace",
    ),
    assetCount: exactAssetCount(decodedSafeInteger(value[6])),
    assetFrontierCommitment: ensureHash32(
      exactBytes(
        value[7],
        32,
        "ledger_output_commitment_v1.asset_frontier_commitment",
      ),
      "ledger_output_commitment_v1.asset_frontier_commitment",
    ),
    cardanoValueSize: exactCardanoValueSize(decodedSafeInteger(value[8])),
    referenceScriptLanguage: referenceScript.language,
    referenceScriptHash: referenceScript.hash,
    referenceScriptTotalLength: referenceScript.totalLength,
    referenceScriptItemCommitment: referenceScript.itemCommitment,
    cardanoTxOut: decodeSummary(value[13], "cardano_tx_out"),
    midgardTxOut: decodeSummary(value[14], "midgard_tx_out"),
    cardanoSpendDatum: decodeSummary(value[15], "cardano_spend_datum"),
  };
  const canonical = encodeMidgardLedgerOutputCommitment(descriptor);
  if (!canonical.equals(Buffer.from(bytes))) {
    throw new Error(
      "V1 ledger output commitment descriptor is not canonical CBOR",
    );
  }
  return descriptor;
};

export const buildMidgardLedgerOutputMaterial = ({
  outputIndex,
  outputCbor,
  facts,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
  readonly facts: MidgardLedgerOutputCommitmentFacts;
}): MidgardLedgerOutputMaterial => {
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: exactOutputIndex(outputIndex),
    bytes: outputCbor,
  });
  const descriptor: MidgardLedgerOutputCommitment = {
    version: MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
    outputIndex: item.itemIndex,
    totalLength: item.bytes.length,
    itemCommitment: item.commitment,
    ...facts,
  };
  return {
    descriptor,
    descriptorCbor: encodeMidgardLedgerOutputCommitment(descriptor),
    item,
  };
};

export const hashMidgardLedgerOutputAssetLeaf = ({
  policyId,
  assetName,
  quantity,
}: MidgardLedgerOutputAsset): Hash32 => {
  if (policyId.length !== 28) {
    throw new Error("V1 ledger output asset policy id must be 28 bytes");
  }
  if (assetName.length > 32) {
    throw new Error(
      "V1 ledger output asset name must contain at most 32 bytes",
    );
  }
  if (quantity <= 0n) {
    throw new Error("V1 ledger output asset quantity must be positive");
  }
  return ensureHash32(
    midgardBlake2b(
      Buffer.concat([
        LEDGER_OUTPUT_ASSET_LEAF_DOMAIN,
        encodeCbor([policyId, assetName, quantity]),
      ]),
      { dkLen: 32 },
    ),
    "ledger_output_asset_leaf_v1",
  );
};

export const buildMidgardLedgerOutputAssetFrontier = (
  assets: readonly MidgardLedgerOutputAsset[],
): MidgardLedgerOutputAssetFrontier => {
  for (let index = 1; index < assets.length; index += 1) {
    const previous = assets[index - 1]!;
    const current = assets[index]!;
    const policyOrder = Buffer.compare(previous.policyId, current.policyId);
    const assetNameOrder =
      previous.assetName.length - current.assetName.length ||
      Buffer.compare(previous.assetName, current.assetName);
    if (policyOrder > 0 || (policyOrder === 0 && assetNameOrder >= 0)) {
      throw new Error(
        "V1 ledger output assets must be in canonical policy/name order",
      );
    }
  }
  const leaves = assets.map(hashMidgardLedgerOutputAssetLeaf);
  const frontier = buildMidgardValidationMerkleFrontier(leaves);
  return {
    count: leaves.length,
    leaves,
    frontier,
    commitment: commitMidgardValidationMerkleFrontier(frontier),
  };
};

export const verifyMidgardLedgerOutputChunk = ({
  descriptor,
  proof,
}: {
  readonly descriptor: MidgardLedgerOutputCommitment;
  readonly proof: MidgardBoundedItemChunkProof;
}): boolean =>
  descriptor.version === MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION &&
  proof.fieldIndex === MIDGARD_LEDGER_OUTPUT_FIELD_INDEX &&
  proof.itemIndex === descriptor.outputIndex &&
  proof.totalLength === descriptor.totalLength &&
  verifyMidgardBoundedItemChunkProof({
    expectedCommitment: descriptor.itemCommitment,
    proof,
  });

export const verifyMidgardLedgerOutputReferenceScriptChunk = ({
  descriptor,
  proof,
}: {
  readonly descriptor: MidgardLedgerOutputCommitment;
  readonly proof: MidgardBoundedItemChunkProof;
}): boolean =>
  descriptor.version === MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION &&
  descriptor.referenceScriptLanguage !== -1 &&
  descriptor.referenceScriptItemCommitment.length === 32 &&
  proof.fieldIndex === MIDGARD_LEDGER_OUTPUT_FIELD_INDEX &&
  proof.itemIndex === descriptor.outputIndex &&
  proof.totalLength === descriptor.referenceScriptTotalLength &&
  verifyMidgardBoundedItemChunkProof({
    expectedCommitment: descriptor.referenceScriptItemCommitment,
    proof,
  });
