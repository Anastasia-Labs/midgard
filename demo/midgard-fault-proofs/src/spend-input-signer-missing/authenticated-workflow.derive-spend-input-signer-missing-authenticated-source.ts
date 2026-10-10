import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  encodeProofThreadForcedSourceKey,
  type FraudProofCatalogueCategoryName,
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
import { deriveResolvedOutputPriorLedgerReplay } from "../resolved-output-non-canonical/resolved-output-non-canonical.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { createCanonicalFamilyArtifactPort } from "../workflow/manifest-bound-family-recovery.js";
import {
  type ManifestBoundSpendInputSignerMissingConfig,
  type SpendInputSignerMissingAuthenticatedSource,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";
import {
  detectSpendInputSignerMissingCompleteReplay,
  type SpendInputSignerMissingEvidence,
} from "./spend-input-signer-missing.js";
import { type SpendInputSignerStage } from "./workflow.js";

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveSpendInputSignerMissingAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: SpendInputSignerMissingEvidence;
}): Promise<SpendInputSignerMissingAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "spendInputSignerMissing accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "spendInputSignerMissing accepted source trie differs from authenticated reconstruction",
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
          "spendInputSignerMissing accepted transaction",
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
      "spendInputSignerMissing forced subject disappeared from retained DA",
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
      "spendInputSignerMissing forced reason differs from authenticated source",
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
      "spendInputSignerMissing forced source material differs from authenticated leaf",
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

export const spendInputSignerStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): SpendInputSignerStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "scanning";
      if (stage.step === 5) return "step05";
      throw new Error(
        "spendInputSignerMissing L1 stage exceeds five-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/** Material re-derived from admitted canonical evidence before durable encoding. */
export const prepareSpendInputSignerMissingRecoveryMaterial = async (
  canonical: CanonicalBlockEvidence,
  detectionId: string,
  predecessor: CanonicalBlockEvidence | undefined,
) => {
  const priorLedger = await deriveResolvedOutputPriorLedgerReplay({
    block: canonical,
    predecessor,
  });
  const findings = detectSpendInputSignerMissingCompleteReplay({
    block: canonical,
    priorLedger,
  });
  if (findings.length !== 1)
    throw new Error(
      "spendInputSignerMissing public replay requires one exact finding",
    );
  const evidence = findings[0]!;
  const source = await deriveSpendInputSignerMissingAuthenticatedSource({
    block: canonical,
    evidence,
  });
  return {
    category: "spendInputSignerMissing" as const,
    headerHash: canonical.headerHash,
    detectionId,
    evidence,
    source,
  };
};

export type SpendInputSignerMissingAssemblyRuntime = Readonly<{
  config: ManifestBoundSpendInputSignerMissingConfig;
  material: ReturnType<
    typeof createCanonicalFamilyArtifactPort<
      Awaited<ReturnType<typeof prepareSpendInputSignerMissingRecoveryMaterial>>
    >
  >;
}>;
