import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  acceptedTransactionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  prepareWitnessScriptDecodingEvidence,
  type WitnessScriptDecodingAction,
  type WitnessScriptDecodingEvidence,
  witnessScriptDecodingEvidenceCloses,
  type WitnessScriptDecodingJournalEntry,
} from "./witness-script-decoding.js";
import { type WitnessScriptDecodingAuthenticatedSource } from "./workflow.derive-witness-script-decoding-authenticated-source.js";
import {
  type LoadManifestBoundWitnessScriptDecodingConfig,
  type ManifestBoundWitnessScriptDecodingConfig,
  type WitnessScriptDecodingAuthenticatedStage,
  type WitnessScriptDecodingJournal,
  witnessScriptDecodingViolationId,
} from "./workflow.witness-script-decoding-config-from-binding.js";

/** Complete replay member: scans every accepted field-6 item and exact forced coordinate. */
export const detectWitnessScriptDecodingCompleteReplay = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const detections: CanonicalViolationDetection[] = [];
  for (const [
    transactionIndex,
    transaction,
  ] of evidence.transactions.entries()) {
    const cbor = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(cbor);
    const transactionId = material.transactionId.toString("hex");
    if (transaction.nodeTxId !== transactionId)
      throw new Error(
        "witnessScriptDecoding complete replay transaction identity changed",
      );
    const field = material.fieldPreimages[6]!;
    const witnessSetHash = decodeMidgardNativeTxCompact(
      material.proofSource.compactCbor,
    ).transactionWitnessSetHash.toString("hex");
    for (const [scriptIndex] of decodeMidgardFieldPreimage(field).entries()) {
      const prepared = prepareWitnessScriptDecodingEvidence({
        finding: {
          subject: acceptedVerdictSubject(transactionId),
          witnessSetHash,
          scriptIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!witnessScriptDecodingEvidenceCloses(prepared)) continue;
      const violationId = witnessScriptDecodingViolationId(
        prepared.resultClass,
      );
      detections.push({
        ...acceptedTransactionSubject(transactionId),
        detectionId: `${violationId}:${transactionIndex.toString()}:${transactionId}:${scriptIndex.toString()}:${prepared.resultClass.toString()}`,
        headerHash: evidence.headerHash,
        violationId,
        position: BigInt(transactionIndex),
        diagnostic: `accepted transaction ${transactionId} has undecodable field-6 script ${scriptIndex.toString()}`,
      });
    }
  }
  for (const [
    forcedIndex,
    transaction,
  ] of evidence.reconstruction.forcedTransactions.entries()) {
    if (transaction.value.verdict === "ForcedTxValid") continue;
    const reason = transaction.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string") continue;
    const payload =
      "WitnessScriptHeaderMalformed" in reason
        ? reason.WitnessScriptHeaderMalformed
        : "WitnessNativeScriptMalformed" in reason
          ? reason.WitnessNativeScriptMalformed
          : "WitnessNativeScriptNodeLimit" in reason
            ? reason.WitnessNativeScriptNodeLimit
            : "WitnessNativeScriptDepthLimit" in reason
              ? reason.WitnessNativeScriptDepthLimit
              : undefined;
    if (payload === undefined) continue;
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      transaction.fullTransactionCbor,
    );
    if (
      material.transactionId.toString("hex") !== transaction.value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        transaction.value.submitted_source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        transaction.value.submitted_source.witness_set_compact_cbor ||
      material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
        transaction.value.submitted_source.field_preimage_lengths_cbor
    )
      throw new Error(
        "witnessScriptDecoding forced transaction differs from its authenticated leaf",
      );
    const field = material.fieldPreimages[6]!;
    const scriptIndex = Number(payload.script_index);
    if (decodeMidgardFieldPreimage(field)[scriptIndex] === undefined) continue;
    const prepared = prepareWitnessScriptDecodingEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: transaction.value.tx_id,
          sourceKey: transaction.key,
          rejectionReason: reason,
        }),
        witnessSetHash: decodeMidgardForcedTxCompact(
          material.proofSource.compactCbor,
        ).transactionWitnessSetHash.toString("hex"),
        scriptIndex,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    if (!witnessScriptDecodingEvidenceCloses(prepared)) continue;
    const violationId = witnessScriptDecodingViolationId(
      prepared.finding.accusedClass,
    );
    detections.push({
      ...forcedTransactionSubject(transaction.key),
      detectionId: `${violationId}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${scriptIndex.toString()}:${prepared.resultClass.toString()}`,
      headerHash: evidence.headerHash,
      violationId,
      position: BigInt(forcedIndex),
      diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for a decodable field-6 script ${scriptIndex.toString()}`,
    });
  }
  return detections;
};

export type WitnessScriptDecodingRuntimeLoader = Readonly<{
  config: LoadManifestBoundWitnessScriptDecodingConfig;
  journal: WitnessScriptDecodingJournal;
  observe: (
    identity: string,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  resolveStage: (input: {
    readonly action: Exclude<WitnessScriptDecodingAction, "done"> | "cancel";
    readonly evidence: WitnessScriptDecodingEvidence;
  }) => Promise<WitnessScriptDecodingAuthenticatedStage>;
}>;

export const createWitnessScriptDecodingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundWitnessScriptDecodingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<"witnessScriptDecoding">;
    readonly source: WitnessScriptDecodingAuthenticatedSource;
  }): WitnessScriptDecodingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "witnessScriptDecoding init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "witnessScriptDecoding removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitScanOrResume"
            ? 3
            : 4;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `witnessScriptDecoding ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (
      evidence.finding.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED
    ) {
      return {
        ...common,
        forcedHeader: required(
          source.forcedHeader,
          "authenticated forced header",
        ),
        forcedMembership: required(
          source.forcedMembership,
          "authenticated forced membership",
        ),
        forcedDirection: required(
          source.forcedDirection,
          "authenticated forced direction",
        ),
      };
    }
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "witness-script-decoding",
      stepIndex: 0,
      threadOutRef: stage.threadOutRef,
    });
    return {
      ...common,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      acceptedInclusion: required(
        source.acceptedInclusion,
        "authenticated accepted inclusion",
      ),
    };
  };

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`witnessScriptDecoding missing ${label}`);
  return value;
};
