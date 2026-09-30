import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  encodeProofThreadForcedSourceKey,
  forcedVerdictSubject,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  RejectionReasonSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { createCanonicalFamilyArtifactPort } from "../workflow/manifest-bound-family-recovery.js";
import {
  type FieldItemWidthEvidence,
  fieldItemWidthEvidenceCloses,
  fieldItemWidthIsIllegal,
  type FieldItemWidthStage,
  prepareFieldItemWidthEvidence,
} from "./field-item-width-illegal.js";
import {
  type FieldItemWidthIllegalAuthenticatedSource,
  type ManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.create-field-item-width-illegal-raw-l1-stage-resolver.js";

/** Derives the only admissible family evidence from L1-bound public retained DA. */
export const deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): FieldItemWidthEvidence => {
  const findings: FieldItemWidthEvidence[] = [];
  const inspect = ({
    canonicalCbor,
    subject,
    forcedCoordinate,
  }: {
    readonly canonicalCbor: Uint8Array;
    readonly subject: ReturnType<typeof acceptedVerdictSubject>;
    readonly forcedCoordinate?: {
      readonly fieldIndex: number;
      readonly itemIndex: number;
    };
  }) => {
    const material = (
      subject.source_kind === 1n
        ? deriveMidgardForcedTxFaultEvidenceMaterial
        : deriveMidgardNativeTxFaultEvidenceMaterial
    )(canonicalCbor);
    if (material.transactionId.toString("hex") !== subject.transaction_id) {
      throw new Error(
        "fieldItemWidthIllegal retained-DA transaction identity changed",
      );
    }
    const coordinates =
      forcedCoordinate === undefined
        ? ([2, 5] as const).flatMap((fieldIndex) =>
            decodeMidgardFieldPreimage(
              material.fieldPreimages[fieldIndex]!,
            ).map((_, itemIndex) => ({ fieldIndex, itemIndex })),
          )
        : [forcedCoordinate];
    for (const coordinate of coordinates) {
      const fieldPreimage = material.fieldPreimages[coordinate.fieldIndex]!;
      const item =
        decodeMidgardFieldPreimage(fieldPreimage)[coordinate.itemIndex];
      if (item === undefined) {
        throw new Error(
          "fieldItemWidthIllegal retained-DA reason coordinate is absent",
        );
      }
      const illegal = fieldItemWidthIsIllegal(
        coordinate.fieldIndex,
        item.length,
      );
      if (forcedCoordinate === undefined && !illegal) continue;
      const prepared = prepareFieldItemWidthEvidence({
        finding: { subject, ...coordinate },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
      });
      if (fieldItemWidthEvidenceCloses(prepared)) findings.push(prepared);
    }
  };
  for (const transaction of block.transactions) {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    inspect({
      canonicalCbor: Buffer.from(transaction.txCbor, "hex"),
      subject: acceptedVerdictSubject(material.transactionId.toString("hex")),
    });
  }
  for (const forced of block.reconstruction.forcedTransactions) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string" || !("FieldItemWidthIllegal" in reason))
      continue;
    const coordinate = reason.FieldItemWidthIllegal;
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    );
    if (material.transactionId.toString("hex") !== forced.value.tx_id) {
      throw new Error(
        "fieldItemWidthIllegal forced retained-DA identity changed",
      );
    }
    inspect({
      canonicalCbor: forced.fullTransactionCbor,
      subject: forcedVerdictSubject({
        transactionId: forced.value.tx_id,
        sourceKey: forced.key,
        rejectionReason: reason,
      }),
      forcedCoordinate: {
        fieldIndex: Number(coordinate.field_index),
        itemIndex: Number(coordinate.item_index),
      },
    });
  }
  if (findings.length !== 1) {
    throw new Error(
      `fieldItemWidthIllegal public retained DA yielded ${findings.length.toString()} exact findings`,
    );
  }
  return findings[0]!;
};

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveFieldItemWidthIllegalAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: FieldItemWidthEvidence;
}): Promise<FieldItemWidthIllegalAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "fieldItemWidthIllegal accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "fieldItemWidthIllegal accepted source trie differs from authenticated reconstruction",
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(selected.txCbor, "hex"),
    );
    return Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: selected.nodeTxId,
        nativeTx: selected.nativeTxCompact,
        nativeTxCompactCbor: selected.nativeCompactCbor,
        l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(selected.nodeTxId, "hex"),
          "fieldItemWidthIllegal accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.subject.transaction_id &&
      encodeProofThreadForcedSourceKey(key).toString("hex") ===
        evidence.subject.source_key,
  );
  if (forced === undefined || forced.value.verdict === "ForcedTxValid") {
    throw new Error(
      "fieldItemWidthIllegal forced subject disappeared from retained DA",
    );
  }
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (
    evidence.subject.rejection_reason === null ||
    Data.to(reason as never, RejectionReasonSchema as never) !==
      Data.to(
        evidence.subject.rejection_reason as never,
        RejectionReasonSchema as never,
      )
  ) {
    throw new Error(
      "fieldItemWidthIllegal forced reason differs from authenticated source",
    );
  }
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.submitted_source.field_preimage_lengths_cbor
  ) {
    throw new Error(
      "fieldItemWidthIllegal forced source material differs from authenticated leaf",
    );
  }
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: forced.key },
  } as const;
  return Object.freeze({
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    forcedHeader: block.header,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
    forcedDirection: 1n,
  });
};

export const fieldItemWidthStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<"fieldItemWidthIllegal">["observe"]
    >
  >["stage"],
): FieldItemWidthStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      throw new Error(
        "fieldItemWidthIllegal L1 stage exceeds three-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/** Material re-derived from admitted canonical evidence before durable encoding. */
export const prepareFieldItemWidthIllegalRecoveryMaterial = async (
  canonical: CanonicalBlockEvidence,
  detectionId: string,
) => {
  const evidence =
    deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(canonical);
  const source = await deriveFieldItemWidthIllegalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  return {
    category: "fieldItemWidthIllegal" as const,
    headerHash: canonical.headerHash,
    detectionId,
    evidence,
    source,
  };
};

export type FieldItemWidthIllegalAssemblyRuntime = Readonly<{
  config: ManifestBoundFieldItemWidthIllegalConfig;
  material: ReturnType<
    typeof createCanonicalFamilyArtifactPort<
      Awaited<ReturnType<typeof prepareFieldItemWidthIllegalRecoveryMaterial>>
    >
  >;
}>;
