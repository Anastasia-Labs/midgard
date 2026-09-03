import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
  type MidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { daHashPreimageBlockEvidenceFromVerifiedPayload } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  MINT_DECLARED_ASSET_LIMIT_CATEGORY,
  MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX,
  type MintDeclaredAssetLimitEvidence,
  mintDeclaredAssetLimitEvidenceCloses,
  prepareMintDeclaredAssetLimitEvidence,
} from "./family-v1.js";
import {
  buildMintDeclaredAssetLimitArtifact,
  type MintDeclaredAssetLimitArtifact,
  MintDeclaredAssetLimitForcedSourcePayloadSchema,
} from "./production-artifact-v1.js";

export const MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID =
  "mint-declared-asset-limit" as const;
export const MINT_DECLARED_ASSET_LIMIT_RAW_EVIDENCE =
  "midgard-mint-declared-asset-limit-raw-evidence-v1" as const;

export type MintDeclaredAssetLimitReplayDetection = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID;
  position: bigint;
  transactionId: string;
  policyIndex: number;
  source: "accepted" | "forced";
  direction: "wrongfulAcceptance" | "wrongfulRejection";
  forcedIndex?: number;
}>;

export type AuthenticatedMintDeclaredAssetLimitRawTransaction = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterial;
}>;

/**
 * L1/root authenticated envelope view intentionally constructed before strict
 * CanonicalBlockEvidence. The accepted machine error is observable from the
 * canonical field-5 prefix and target map header even when its body truncates.
 */
export type MintDeclaredAssetLimitRawBlockEvidence = Readonly<{
  schemaVersion: typeof MINT_DECLARED_ASSET_LIMIT_RAW_EVIDENCE;
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transactions: readonly AuthenticatedMintDeclaredAssetLimitRawTransaction[];
}>;

const decodeSource = (cbor: string, index: number): SDK.L2TransactionSource => {
  let source: SDK.L2TransactionSource;
  try {
    source = Data.from(cbor, SDK.L2TransactionSource);
  } catch (cause) {
    throw new Error(
      `mintDeclaredAssetLimit transactions[${index.toString()}] source does not decode: ${String(cause)}`,
    );
  }
  if (Data.to(source, SDK.L2TransactionSource) !== cbor) {
    throw new Error(
      `mintDeclaredAssetLimit transactions[${index.toString()}] source is not canonical Data`,
    );
  }
  return source;
};

export const mintDeclaredAssetLimitRawBlockEvidenceFromVerifiedPayload =
  async ({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    minimumConfirmationDepth,
  }: {
    readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
    readonly payloadEnvelopeCbor: Uint8Array;
    readonly daProvenance: SDK.EvidenceProvenance;
    readonly minimumConfirmationDepth?: number;
  }): Promise<MintDeclaredAssetLimitRawBlockEvidence> => {
    const raw = await daHashPreimageBlockEvidenceFromVerifiedPayload({
      observation,
      payloadEnvelopeCbor,
      daProvenance,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
    const payloadCbor = Buffer.from(
      (
        await unwrapDaPayload(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        })
      ).innerBytes,
    );
    const payload = SDK.decodeDaPayload(payloadCbor);
    if (!SDK.encodeDaPayload(payload).equals(payloadCbor))
      throw new Error("mintDeclaredAssetLimit DA payload is not canonical");

    const entries = raw.entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }));
    const trie = await buildTrieView(entries);
    await requireTransactionsRootMatch({
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
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
      schemaVersion: MINT_DECLARED_ASSET_LIMIT_RAW_EVIDENCE,
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
  transaction: AuthenticatedMintDeclaredAssetLimitRawTransaction,
  policyIndex: number,
): MintDeclaredAssetLimitEvidence | null => {
  if (transaction.material.canonical.validity !== "TxIsValid") return null;
  const field =
    transaction.material.fieldPreimages[MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX];
  if (field === undefined) return null;
  try {
    const evidence = prepareMintDeclaredAssetLimitEvidence({
      finding: {
        subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
        policyIndex,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    return mintDeclaredAssetLimitEvidenceCloses(evidence) ? evidence : null;
  } catch {
    return null;
  }
};

/** Complete accepted scan; every field-5 item is tried as a possible first crossing. */
export const detectMintDeclaredAssetLimitAcceptedRawReplay = (
  block: MintDeclaredAssetLimitRawBlockEvidence,
): readonly MintDeclaredAssetLimitReplayDetection[] => {
  const detections: MintDeclaredAssetLimitReplayDetection[] = [];
  for (const transaction of block.transactions) {
    const field =
      transaction.material.fieldPreimages[
        MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX
      ];
    if (field === undefined) continue;
    let itemCount: number;
    try {
      itemCount = decodeMidgardFieldPreimage(field).length;
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
          detectionId: `${MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID}:accepted:${transaction.index.toString()}:${transaction.nodeTxId}:${policyIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID,
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
export const detectMintDeclaredAssetLimitForcedReplay = (
  block: CanonicalBlockEvidence,
): readonly MintDeclaredAssetLimitReplayDetection[] => {
  const detections: MintDeclaredAssetLimitReplayDetection[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return;
      const reason = verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("MintDeclaredAssetLimit" in reason))
        return;
      const policyIndex = Number(reason.MintDeclaredAssetLimit.policy_index);
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
        material.fieldPreimages[MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX];
      if (field === undefined) return;
      const evidence = prepareMintDeclaredAssetLimitEvidence({
        finding: {
          subject: SDK.forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          policyIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!mintDeclaredAssetLimitEvidenceCloses(evidence)) return;
      detections.push(
        Object.freeze({
          detectionId: `${MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${policyIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID,
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

export const selectCanonicalMintDeclaredAssetLimitDetection = (
  detections: readonly MintDeclaredAssetLimitReplayDetection[],
): MintDeclaredAssetLimitReplayDetection => {
  if (detections.length === 0)
    throw new Error(
      `${MINT_DECLARED_ASSET_LIMIT_CATEGORY}: no authenticated detection`,
    );
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};

export const mintDeclaredAssetLimitAcceptedMembership = async ({
  block,
  transactionId,
}: {
  readonly block: MintDeclaredAssetLimitRawBlockEvidence;
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
export const prepareMintDeclaredAssetLimitAcceptedArtifact = async (
  block: MintDeclaredAssetLimitRawBlockEvidence,
): Promise<MintDeclaredAssetLimitArtifact> => {
  const detection = selectCanonicalMintDeclaredAssetLimitDetection(
    detectMintDeclaredAssetLimitAcceptedRawReplay(block),
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
    transaction.material.fieldPreimages[MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX];
  if (field === undefined)
    throw new Error("mintDeclaredAssetLimit selected field 5 disappeared");
  const evidence = prepareMintDeclaredAssetLimitEvidence({
    finding: {
      subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
      policyIndex: detection.policyIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
  return buildMintDeclaredAssetLimitArtifact({
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
    transactionMembershipCbor: await mintDeclaredAssetLimitAcceptedMembership({
      block,
      transactionId: transaction.nodeTxId,
    }),
  });
};

/** Reconstructs the exact forced wrongful-rejection artifact from canonical replay. */
export const prepareMintDeclaredAssetLimitForcedArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<MintDeclaredAssetLimitArtifact> => {
  const detection = selectCanonicalMintDeclaredAssetLimitDetection(
    detectMintDeclaredAssetLimitForcedReplay(block),
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
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    encodeMidgardNativeTxCanonical(
      adjudicateMidgardNativeTxFullValidity(
        decodeMidgardNativeTxFullFromCanonicalCbor(
          transaction.fullTransactionCbor,
        ),
        "TxIsInvalid",
      ),
    ),
  );
  const field = material.fieldPreimages[MINT_DECLARED_ASSET_LIMIT_FIELD_INDEX];
  if (field === undefined)
    throw new Error("mintDeclaredAssetLimit forced field 5 disappeared");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const evidence = prepareMintDeclaredAssetLimitEvidence({
    finding: {
      subject: SDK.forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: verdict.ForcedTxInvalid.reason,
      }),
      policyIndex: detection.policyIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
  const forcedSourceCbor = Data.to(
    { header: block.header, membership, direction: 1n } as never,
    MintDeclaredAssetLimitForcedSourcePayloadSchema as never,
  );
  return buildMintDeclaredAssetLimitArtifact({
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
      SDK.L2TransactionSource as never,
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
