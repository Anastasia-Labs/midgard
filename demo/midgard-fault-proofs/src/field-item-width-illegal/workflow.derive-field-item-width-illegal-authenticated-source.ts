import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  encodeProofThreadForcedSourceKey,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  RejectionReasonSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  buildTrieView,
  encodeL2TransactionSourceValue,
  requireProof,
} from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { createCanonicalFamilyArtifactPort } from "../workflow/manifest-bound-family-recovery.js";
import { CanonicalReplayPrerequisiteError } from "../workflow/replay-prerequisite.js";
import {
  fieldItemWidthDetectionId,
  type FieldItemWidthEvidence,
  fieldItemWidthFindingsFromCanonicalBlock,
  type FieldItemWidthStage,
  prepareFieldItemWidthEvidence,
} from "./field-item-width-illegal.js";
import {
  FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
  type FieldItemWidthIllegalAuthenticatedSource,
  type ManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.create-field-item-width-illegal-raw-l1-stage-resolver.js";

/** Derives the only admissible family evidence from L1-bound public retained DA. */
export const deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
  detectionId?: string,
): FieldItemWidthEvidence => {
  const scan = fieldItemWidthFindingsFromCanonicalBlock(
    block,
    detectionId === undefined
      ? undefined
      : {
          detectionId,
          violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
        },
  );
  if (scan.failures.length > 0)
    throw new CanonicalReplayPrerequisiteError(scan.failures);
  const findings = scan.findings.filter(
    (finding) =>
      detectionId === undefined ||
      fieldItemWidthDetectionId(
        finding,
        FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
      ) === detectionId,
  );
  if (findings.length !== 1) {
    throw new Error(
      `fieldItemWidthIllegal public retained DA yielded ${findings.length.toString()} exact findings`,
    );
  }
  const { evidence } = findings[0]!;
  return evidence.kind === "StaticDomain"
    ? evidence
    : prepareFieldItemWidthEvidence({
        finding: evidence,
        fieldPreimage: Buffer.from(evidence.fieldPreimageHex, "hex"),
        committedFieldHashHex: evidence.fieldCommitmentHex,
      });
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
    const decoded = block.transactions.map((transaction) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(transaction.txCbor, "hex"),
      );
      if (material.transactionId.toString("hex") !== transaction.nodeTxId)
        throw new Error(
          "fieldItemWidthIllegal retained-DA transaction identity changed",
        );
      const l2TransactionSourceCbor = encodeL2TransactionSourceValue({
        txId: transaction.nodeTxId,
        proofSource: material.proofSource,
      });
      if (
        transaction.l2TransactionSourceCbor !== undefined &&
        transaction.l2TransactionSourceCbor !== l2TransactionSourceCbor
      )
        throw new Error(
          "fieldItemWidthIllegal retained-DA transaction source changed",
        );
      return { ...transaction, material, l2TransactionSourceCbor };
    });
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "fieldItemWidthIllegal accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(
      decoded.map((transaction) => ({
        key: Buffer.from(transaction.nodeTxId, "hex"),
        value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
      })),
    );
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "fieldItemWidthIllegal accepted source trie differs from authenticated reconstruction",
      );
    }
    const { material } = selected;
    return Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: selected.nodeTxId,
        nativeTx: nativeTxFromCoreCompact(material.compact),
        nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
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
  if (forced === undefined) {
    throw new Error(
      "fieldItemWidthIllegal forced subject disappeared from retained DA",
    );
  }
  const reason =
    forced.value.verdict === "ForcedTxValid"
      ? null
      : forced.value.verdict.ForcedTxInvalid.reason;
  if (
    (reason === null) !== (evidence.subject.rejection_reason === null) ||
    (reason !== null &&
      evidence.subject.rejection_reason !== null &&
      Data.to(reason as never, RejectionReasonSchema as never) !==
        Data.to(
          evidence.subject.rejection_reason as never,
          RejectionReasonSchema as never,
        ))
  ) {
    throw new Error(
      "fieldItemWidthIllegal forced reason differs from authenticated source",
    );
  }
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.transactionId.toString("hex") !== forced.value.tx_id ||
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
    forcedDirection: evidence.subject.direction,
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
  const evidence = deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(
    canonical,
    detectionId,
  );
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
