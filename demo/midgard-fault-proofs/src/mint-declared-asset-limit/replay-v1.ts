import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
  midgardFieldCommitmentV1,
  type MidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { daHashPreimageBlockEvidenceFromVerifiedPayloadV1 } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatchV1,
} from "../prepare-double-spend.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1,
  MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1,
  mintDeclaredAssetLimitEvidenceClosesV1,
  type MintDeclaredAssetLimitEvidenceV1,
  prepareMintDeclaredAssetLimitEvidenceV1,
} from "./family-v1.js";
import {
  buildProductionMintDeclaredAssetLimitArtifactV1,
  MintDeclaredAssetLimitForcedSourcePayloadV1Schema,
  type ProductionMintDeclaredAssetLimitArtifactV1,
} from "./production-artifact-v1.js";

export const MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1 =
  "mint-declared-asset-limit" as const;
export const MINT_DECLARED_ASSET_LIMIT_RAW_EVIDENCE_V1 =
  "midgard-mint-declared-asset-limit-raw-evidence-v1" as const;

export type MintDeclaredAssetLimitReplayDetectionV1 = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1;
  position: bigint;
  transactionId: string;
  policyIndex: number;
  source: "accepted" | "forced";
  direction: "wrongfulAcceptance" | "wrongfulRejection";
  forcedIndex?: number;
}>;

export type AuthenticatedMintDeclaredAssetLimitRawTransactionV1 = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterialV1;
}>;

/**
 * L1/root authenticated envelope view intentionally constructed before strict
 * CanonicalBlockEvidence. The accepted machine error is observable from the
 * canonical field-5 prefix and target map header even when its body truncates.
 */
export type MintDeclaredAssetLimitRawBlockEvidenceV1 = Readonly<{
  schemaVersion: typeof MINT_DECLARED_ASSET_LIMIT_RAW_EVIDENCE_V1;
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transactions: readonly AuthenticatedMintDeclaredAssetLimitRawTransactionV1[];
}>;

const decodeSource = (
  cbor: string,
  index: number,
): SDK.L2TransactionSourceV1 => {
  let source: SDK.L2TransactionSourceV1;
  try {
    source = Data.from(cbor, SDK.L2TransactionSourceV1);
  } catch (cause) {
    throw new Error(
      `mintDeclaredAssetLimit transactions[${index.toString()}] source does not decode: ${String(cause)}`,
    );
  }
  if (Data.to(source, SDK.L2TransactionSourceV1) !== cbor) {
    throw new Error(
      `mintDeclaredAssetLimit transactions[${index.toString()}] source is not canonical Data`,
    );
  }
  return source;
};

export const mintDeclaredAssetLimitRawBlockEvidenceFromVerifiedPayloadV1 =
  async ({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    minimumConfirmationDepth,
  }: {
    readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
    readonly payloadEnvelopeCbor: Uint8Array;
    readonly daProvenance: SDK.EvidenceProvenanceV1;
    readonly minimumConfirmationDepth?: number;
  }): Promise<MintDeclaredAssetLimitRawBlockEvidenceV1> => {
    const raw = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
      observation,
      payloadEnvelopeCbor,
      daProvenance,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
    const payloadCbor = Buffer.from(
      (
        await unwrapDaPayloadV1(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        })
      ).innerBytes,
    );
    const payload = SDK.decodeDaPayloadV1(payloadCbor);
    if (!SDK.encodeDaPayloadV1(payload).equals(payloadCbor))
      throw new Error("mintDeclaredAssetLimit DA payload is not canonical");

    const entries = raw.entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }));
    const trie = await buildTrieView(entries);
    await requireTransactionsRootMatchV1({
      sourceRoot: trie.root,
      expectedTransactionsRoot: raw.committedTransactionsRoot,
      count: raw.l2TransactionCount,
    });

    const preimages = new Map(payload.block_body.transaction_preimages);
    if (preimages.size !== payload.block_body.transaction_preimages.length)
      throw new Error(
        "mintDeclaredAssetLimit transaction preimages are duplicated",
      );
    const transactions = raw.entries.map(([key, sourceCbor], index) => {
      const txCbor = preimages.get(key);
      if (txCbor === undefined)
        throw new Error(
          `mintDeclaredAssetLimit transaction preimage omitted ${key}`,
        );
      const source = decodeSource(sourceCbor, index);
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        Buffer.from(txCbor, "hex"),
      );
      if (
        source.tx_id !== key ||
        material.transactionId.toString("hex") !== key ||
        source.source.compact_cbor !==
          material.proofSource.compactCbor.toString("hex") ||
        source.source.witness_set_compact_cbor !==
          material.proofSource.witnessSetCompactCbor.toString("hex") ||
        source.source.field_preimage_lengths_cbor !==
          material.proofSource.fieldPreimageLengthsCbor.toString("hex")
      )
        throw new Error(
          `mintDeclaredAssetLimit transaction ${key} differs from its committed source`,
        );
      return Object.freeze({
        index,
        nodeTxId: key,
        l2TransactionSourceCbor: sourceCbor,
        fullTransactionCbor: txCbor,
        material,
      });
    });
    if (preimages.size !== transactions.length)
      throw new Error(
        "mintDeclaredAssetLimit has uncommitted transaction preimages",
      );
    return Object.freeze({
      schemaVersion: MINT_DECLARED_ASSET_LIMIT_RAW_EVIDENCE_V1,
      headerHash: raw.headerHash,
      committedTransactionsRoot: raw.committedTransactionsRoot,
      l2TransactionCount: raw.l2TransactionCount,
      transactionsPhasRoot: trie.root,
      payloadEnvelopeSha256: raw.payloadEnvelopeSha256,
      payloadSha256: raw.payloadSha256,
      transactions: Object.freeze(transactions),
    });
  };

const acceptedEvidence = (
  transaction: AuthenticatedMintDeclaredAssetLimitRawTransactionV1,
  policyIndex: number,
): MintDeclaredAssetLimitEvidenceV1 | null => {
  if (transaction.material.canonical.validity !== "TxIsValid") return null;
  const field =
    transaction.material.fieldPreimages[
      MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1
    ];
  if (field === undefined) return null;
  try {
    const evidence = prepareMintDeclaredAssetLimitEvidenceV1({
      finding: {
        subject: SDK.acceptedVerdictSubjectV1(transaction.nodeTxId),
        policyIndex,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
    });
    return mintDeclaredAssetLimitEvidenceClosesV1(evidence) ? evidence : null;
  } catch {
    return null;
  }
};

/** Complete accepted scan; every field-5 item is tried as a possible first crossing. */
export const detectMintDeclaredAssetLimitAcceptedRawReplayV1 = (
  block: MintDeclaredAssetLimitRawBlockEvidenceV1,
): readonly MintDeclaredAssetLimitReplayDetectionV1[] => {
  const detections: MintDeclaredAssetLimitReplayDetectionV1[] = [];
  for (const transaction of block.transactions) {
    const field =
      transaction.material.fieldPreimages[
        MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1
      ];
    if (field === undefined) continue;
    let itemCount: number;
    try {
      itemCount = decodeMidgardFieldPreimageV1(field).length;
    } catch {
      // Field-envelope failures belong to the earlier decoding families.
      continue;
    }
    // Trying every authenticated coordinate preserves machine precedence;
    // target item internals remain opaque until after its declared-count test.
    for (let policyIndex = 0; policyIndex < itemCount; policyIndex += 1) {
      const evidence = acceptedEvidence(transaction, policyIndex);
      if (evidence === null) continue;
      detections.push(
        Object.freeze({
          detectionId: `${MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1}:accepted:${transaction.index.toString()}:${transaction.nodeTxId}:${policyIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1,
          position: BigInt(transaction.index),
          transactionId: transaction.nodeTxId,
          policyIndex,
          source: "accepted",
          direction: "wrongfulAcceptance",
        }),
      );
      break;
    }
  }
  return Object.freeze(detections);
};

/** Complete canonical scan of exact wrongful-rejection contradictions. */
export const detectMintDeclaredAssetLimitForcedReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly MintDeclaredAssetLimitReplayDetectionV1[] => {
  const detections: MintDeclaredAssetLimitReplayDetectionV1[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return;
      const reason = verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("MintDeclaredAssetLimit" in reason))
        return;
      const policyIndex = Number(reason.MintDeclaredAssetLimit.policy_index);
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        transaction.fullTransactionCbor,
      );
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          transaction.value.source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          transaction.value.source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          transaction.value.source.field_preimage_lengths_cbor
      )
        throw new Error(
          "mintDeclaredAssetLimit forced transaction differs from its authenticated leaf",
        );
      const field =
        material.fieldPreimages[MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1];
      if (field === undefined) return;
      const evidence = prepareMintDeclaredAssetLimitEvidenceV1({
        finding: {
          subject: SDK.forcedVerdictSubjectV1({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          policyIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
      });
      if (!mintDeclaredAssetLimitEvidenceClosesV1(evidence)) return;
      detections.push(
        Object.freeze({
          detectionId: `${MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${policyIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1,
          position: BigInt(forcedIndex),
          transactionId: transaction.value.tx_id,
          policyIndex,
          source: "forced",
          direction: "wrongfulRejection",
          forcedIndex,
        }),
      );
    },
  );
  return Object.freeze(detections);
};

export const selectCanonicalMintDeclaredAssetLimitDetectionV1 = (
  detections: readonly MintDeclaredAssetLimitReplayDetectionV1[],
): MintDeclaredAssetLimitReplayDetectionV1 => {
  if (detections.length === 0)
    throw new Error(
      `${MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1}: no authenticated detection`,
    );
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};

export const mintDeclaredAssetLimitAcceptedMembershipV1 = async ({
  block,
  transactionId,
}: {
  readonly block: MintDeclaredAssetLimitRawBlockEvidenceV1;
  readonly transactionId: string;
}): Promise<string> => {
  const entries = block.transactions.map((transaction) => ({
    key: Buffer.from(transaction.nodeTxId, "hex"),
    value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
  }));
  return requireProof(
    await buildTrieView(entries),
    Buffer.from(transactionId, "hex"),
    "mintDeclaredAssetLimit accepted transaction",
  );
};

/** Reconstructs the selected accepted artifact without caller-prepared evidence. */
export const prepareProductionMintDeclaredAssetLimitAcceptedArtifactV1 = async (
  block: MintDeclaredAssetLimitRawBlockEvidenceV1,
): Promise<ProductionMintDeclaredAssetLimitArtifactV1> => {
  const detection = selectCanonicalMintDeclaredAssetLimitDetectionV1(
    detectMintDeclaredAssetLimitAcceptedRawReplayV1(block),
  );
  const transaction = block.transactions[Number(detection.position)];
  if (
    transaction === undefined ||
    transaction.nodeTxId !== detection.transactionId
  )
    throw new Error(
      "mintDeclaredAssetLimit selected accepted transaction disappeared",
    );
  const field =
    transaction.material.fieldPreimages[
      MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1
    ];
  if (field === undefined)
    throw new Error("mintDeclaredAssetLimit selected field 5 disappeared");
  const evidence = prepareMintDeclaredAssetLimitEvidenceV1({
    finding: {
      subject: SDK.acceptedVerdictSubjectV1(transaction.nodeTxId),
      policyIndex: detection.policyIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
  return buildProductionMintDeclaredAssetLimitArtifactV1({
    headerHash: block.headerHash,
    detectionId: detection.detectionId,
    position: detection.position,
    evidence,
    nativeTxCompactCbor:
      transaction.material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      transaction.material.proofSource.witnessSetCompactCbor.toString("hex"),
    l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
    transactionsPhasRoot: block.transactionsPhasRoot,
    transactionMembershipCbor: await mintDeclaredAssetLimitAcceptedMembershipV1(
      {
        block,
        transactionId: transaction.nodeTxId,
      },
    ),
  });
};

/** Reconstructs the exact forced wrongful-rejection artifact from canonical replay. */
export const prepareProductionMintDeclaredAssetLimitForcedArtifactV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<ProductionMintDeclaredAssetLimitArtifactV1> => {
  const detection = selectCanonicalMintDeclaredAssetLimitDetectionV1(
    detectMintDeclaredAssetLimitForcedReplayV1(block),
  );
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex!];
  if (transaction === undefined)
    throw new Error(
      "mintDeclaredAssetLimit selected forced transaction disappeared",
    );
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("mintDeclaredAssetLimit forced rejection changed verdict");
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    encodeMidgardNativeTxCanonicalV1(
      adjudicateMidgardNativeTxFullV1Validity(
        decodeMidgardNativeTxFullV1FromCanonicalCbor(
          transaction.fullTransactionCbor,
        ),
        "TxIsInvalid",
      ),
    ),
  );
  const field =
    material.fieldPreimages[MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX_V1];
  if (field === undefined)
    throw new Error("mintDeclaredAssetLimit forced field 5 disappeared");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const evidence = prepareMintDeclaredAssetLimitEvidenceV1({
    finding: {
      subject: SDK.forcedVerdictSubjectV1({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: verdict.ForcedTxInvalid.reason,
      }),
      policyIndex: detection.policyIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
  const forcedSourceCbor = Data.to(
    { header: block.header, membership, direction: 1n } as never,
    MintDeclaredAssetLimitForcedSourcePayloadV1Schema as never,
  );
  return buildProductionMintDeclaredAssetLimitArtifactV1({
    headerHash: block.headerHash,
    detectionId: detection.detectionId,
    position: detection.position,
    evidence,
    sourceKind: "forced",
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    l2TransactionSourceCbor: Data.to(
      {
        tx_id: transaction.value.tx_id,
        source: transaction.value.source,
      } as never,
      SDK.L2TransactionSourceV1 as never,
    ),
    transactionsPhasRoot: "00".repeat(32),
    transactionMembershipCbor: Data.to(
      membership as never,
      SDK.ForcedTransactionSourceMembershipProof as never,
    ),
    forcedSourceCbor,
  });
};

export { buildForcedTransactionLeafMembershipProof };
