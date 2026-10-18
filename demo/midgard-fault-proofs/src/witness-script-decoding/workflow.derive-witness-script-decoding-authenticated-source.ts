import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  encodeProofThreadForcedSourceKey,
  type ForcedInclusionTxV1,
  type Header,
  type OutputReference,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  RejectionReasonSchema,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { firstCanonicalViolationDetection } from "../workflow/classification.js";
import { withoutHeldPlutusScriptItems } from "../workflow/plutus-script-item-hold.js";
import { type WitnessScriptDecodingEvidence } from "./witness-script-decoding.js";
import { witnessScriptDecodingCompleteReplayFindings } from "./workflow.detect-witness-script-decoding-complete-replay.js";

/**
 * Derives the family evidence to prove from L1-bound public retained DA: the
 * first unheld finding in the order `classifyCanonicalBlockViolations`
 * selects, so the workflow proves the detection the classifier chose.
 */
export const deriveWitnessScriptDecodingEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): WitnessScriptDecodingEvidence => {
  const first = firstCanonicalViolationDetection(
    block,
    withoutHeldPlutusScriptItems(
      witnessScriptDecodingCompleteReplayFindings(block),
    ),
  );
  if (first === undefined)
    throw new Error(
      "witnessScriptDecoding public retained DA yielded no exact finding",
    );
  return first.evidence;
};

export type WitnessScriptDecodingAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveWitnessScriptDecodingAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: WitnessScriptDecodingEvidence;
}): Promise<WitnessScriptDecodingAuthenticatedSource> => {
  if (
    evidence.finding.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED
  ) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.finding.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "witnessScriptDecoding accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "witnessScriptDecoding accepted source trie differs from authenticated reconstruction",
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
          "witnessScriptDecoding accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.finding.subject.transaction_id &&
      encodeProofThreadForcedSourceKey(key).toString("hex") ===
        evidence.finding.subject.source_key,
  );
  if (forced === undefined || forced.value.verdict === "ForcedTxValid") {
    throw new Error(
      "witnessScriptDecoding forced subject disappeared from retained DA",
    );
  }
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (
    evidence.finding.subject.rejection_reason === null ||
    Data.to(reason as never, RejectionReasonSchema as never) !==
      Data.to(
        evidence.finding.subject.rejection_reason as never,
        RejectionReasonSchema as never,
      )
  ) {
    throw new Error(
      "witnessScriptDecoding forced reason differs from authenticated source",
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
      "witnessScriptDecoding forced source material differs from authenticated leaf",
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
