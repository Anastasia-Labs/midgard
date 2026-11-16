import { createHash } from "node:crypto";

import { MIDGARD_FIELD_COUNT } from "@al-ft/midgard-core";
import { decodeMidgardNativeTxProofFieldLengths } from "@al-ft/midgard-core/codec";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import { claimedIndex } from "../evidence/claimed-coordinate.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { forcedTransactionSubject } from "../workflow/detection-subject.js";
import type { FieldPreimageLengthStage } from "./config.js";
import { exactFieldPreimageLengthRawFinding } from "./evidence-raw.js";
import { fieldPreimageLengthCommittedClaim } from "./prepare-accepted.js";
import {
  fieldPreimageLengthOutOfDomain,
  type PreparedFieldPreimageLengthOutOfDomain,
  type PreparedFieldPreimageLengthWorkflow,
  prepareFieldPreimageLengthOutOfDomainWorkflow,
  prepareFieldPreimageLengthWorkflow,
} from "./workflow.js";

export const FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID =
  "field-preimage-length-mismatch" as const;

const fieldPreimageLengthDetectionId = (
  position: number,
  transactionId: string,
  fieldIndex: bigint,
) =>
  `${FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID}:${position.toString()}:${transactionId}:${fieldIndex.toString()}:wrongfulRejection`;

/** Complete canonical scan of forced wrongful-rejection contradictions. */
export const detectFieldPreimageLengthCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const detections: CanonicalViolationDetection[] = [];
  for (const [
    position,
    forced,
  ] of block.reconstruction.forcedTransactions.entries()) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !("FieldPreimageLengthMismatch" in reason)
    ) {
      continue;
    }
    const claimedField = reason.FieldPreimageLengthMismatch.field_index;
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    );
    if (
      material.transactionId.toString("hex") !== forced.value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        forced.value.submitted_source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        forced.value.submitted_source.witness_set_compact_cbor
    ) {
      throw new Error(
        "fieldPreimageLengthMismatch forced preimage differs from its committed leaf",
      );
    }
    const detection = {
      ...forcedTransactionSubject(forced.key),
      detectionId: fieldPreimageLengthDetectionId(
        position,
        forced.value.tx_id,
        claimedField,
      ),
      headerHash: block.headerHash,
      violationId: FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID,
      position: BigInt(position),
    };
    const fieldIndex = claimedIndex(claimedField, MIDGARD_FIELD_COUNT);
    // A field outside the native layout has no preimage whose length could
    // mismatch, so the rejection is contradicted outright.
    if (fieldIndex === undefined) {
      prepareFieldPreimageLengthOutOfDomainWorkflow({
        headerHash: block.headerHash,
        transactionId: forced.value.tx_id,
        fieldIndex: claimedField,
      });
      detections.push(detection);
      continue;
    }
    const preimage = material.fieldPreimages[fieldIndex]!;
    const declaredLength = decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(
        forced.value.submitted_source.field_preimage_lengths_cbor,
        "hex",
      ),
    )[fieldIndex]!;
    // A truthful forced rejection is healthy for this family. Only equality
    // contradicts the operator's exact mismatch reason.
    if (declaredLength !== preimage.length) continue;
    prepareFieldPreimageLengthWorkflow({
      headerHash: block.headerHash,
      transactionId: forced.value.tx_id,
      direction: "wrongfulRejection",
      sourceKind: "forced",
      fieldIndex,
      fieldPreimageLengthsCbor: material.proofSource.fieldPreimageLengthsCbor,
      fieldPreimage: preimage,
      forcedRejectionReason: reason,
    });
    detections.push(detection);
  }
  return Object.freeze(detections);
};

export type AuthenticatedFieldPreimageLengthEvidence = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflow;
  fieldMaterial: Readonly<{
    nativeTxCompactCbor: string;
    witnessSetCompactCbor: string;
  }>;
  stageEvidence: Omit<
    FieldPreimageLengthStage,
    "fraudulentBlockOutRef" | "threadOutRef" | "cancelStepIndex"
  >;
}>;

/** A forced rejection naming a field outside the native layout. */
export type OutOfDomainFieldPreimageLengthEvidence = Readonly<{
  prepared: PreparedFieldPreimageLengthOutOfDomain;
  fieldMaterial: AuthenticatedFieldPreimageLengthEvidence["fieldMaterial"];
  stageEvidence: Required<
    Pick<FieldPreimageLengthStage, "forcedHeader" | "forcedMembership">
  >;
}>;

export type FieldPreimageLengthWorkflowEvidence =
  | AuthenticatedFieldPreimageLengthEvidence
  | OutOfDomainFieldPreimageLengthEvidence;

export const inDomainFieldPreimageLengthEvidence = (
  evidence: FieldPreimageLengthWorkflowEvidence,
): evidence is AuthenticatedFieldPreimageLengthEvidence =>
  !fieldPreimageLengthOutOfDomain(evidence.prepared);

export type RoutedFieldPreimageLengthEvidence =
  AuthenticatedFieldPreimageLengthEvidence &
    Readonly<{
      position: bigint;
      payloadEnvelopeSha256: string;
      payloadSha256: string;
    }>;

const inlineCarriage = (preimage: Uint8Array): SDK.FieldCarriage => ({
  Inline: { preimage: Buffer.from(preimage).toString("hex") },
});

export const fieldPreimageLengthEvidenceFromCanonicalBlock = async (
  block: Awaited<ReturnType<typeof canonicalBlockEvidenceFromVerifiedPayload>>,
  selected?: CanonicalViolationDetection,
): Promise<FieldPreimageLengthWorkflowEvidence> => {
  const findings: FieldPreimageLengthWorkflowEvidence[] = [];
  for (const [
    position,
    forced,
  ] of block.reconstruction.forcedTransactions.entries()) {
    const subject = forcedTransactionSubject(forced.key);
    if (
      selected !== undefined &&
      (selected.headerHash !== block.headerHash ||
        selected.violationId !== FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID ||
        selected.position !== BigInt(position) ||
        selected.frontier !== subject.frontier ||
        selected.subjectEventKeyCbors.length !== 1 ||
        selected.subjectEventKeyCbors[0] !== subject.subjectEventKeyCbors[0])
    )
      continue;
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !("FieldPreimageLengthMismatch" in reason)
    ) {
      continue;
    }
    const claimedField = reason.FieldPreimageLengthMismatch.field_index;
    if (
      selected !== undefined &&
      selected.detectionId !==
        fieldPreimageLengthDetectionId(
          position,
          forced.value.tx_id,
          claimedField,
        )
    )
      continue;
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    );
    if (
      material.transactionId.toString("hex") !== forced.value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        forced.value.submitted_source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        forced.value.submitted_source.witness_set_compact_cbor
    ) {
      throw new Error(
        "fieldPreimageLengthMismatch forced preimage differs from its committed leaf",
      );
    }
    const fieldMaterial = Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
    });
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: { tx_order_id: forced.key },
    };
    const fieldIndex = claimedIndex(claimedField, MIDGARD_FIELD_COUNT);
    if (fieldIndex === undefined) {
      findings.push(
        Object.freeze({
          prepared: prepareFieldPreimageLengthOutOfDomainWorkflow({
            headerHash: block.headerHash,
            transactionId: forced.value.tx_id,
            fieldIndex: claimedField,
          }),
          fieldMaterial,
          stageEvidence: Object.freeze({
            forcedHeader: block.header,
            forcedMembership: await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey,
            }),
          }),
        }),
      );
      continue;
    }
    const preimage = material.fieldPreimages[fieldIndex]!;
    const basePrepared = prepareFieldPreimageLengthWorkflow({
      headerHash: block.headerHash,
      transactionId: forced.value.tx_id,
      direction: "wrongfulRejection",
      sourceKind: "forced",
      fieldIndex,
      fieldPreimageLengthsCbor: Buffer.from(
        forced.value.submitted_source.field_preimage_lengths_cbor,
        "hex",
      ),
      fieldPreimage: preimage,
      forcedRejectionReason: reason,
    });
    const prepared: PreparedFieldPreimageLengthWorkflow = Object.freeze({
      ...basePrepared,
      evidenceDigest: createHash("sha256")
        .update(basePrepared.evidenceDigest, "hex")
        .update(
          Data.to(forced.key as never, SDK.OutputReferenceSchema as never),
          "hex",
        )
        .update(
          Data.to(reason as never, SDK.RejectionReasonSchema as never),
          "hex",
        )
        .digest("hex"),
    });
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    });
    findings.push(
      Object.freeze({
        prepared,
        fieldMaterial,
        stageEvidence: Object.freeze({
          forcedHeader: block.header,
          forcedMembership: membership,
          ...(prepared.carriage === "Inline"
            ? {
                forcedClaim: fieldPreimageLengthCommittedClaim({
                  fieldIndex,
                  witnessSetCompactCbor:
                    material.proofSource.witnessSetCompactCbor,
                  carriage: inlineCarriage(preimage),
                }),
              }
            : {}),
        }),
      }),
    );
  }
  if (findings.length === 0) {
    throw new Error(
      `fieldPreimageLengthMismatch canonical retained DA yielded ${findings.length.toString()} exact forced findings`,
    );
  }
  return findings[0]!;
};

/**
 * Reopens the exact already-fetched retained-DA envelope after the canonical
 * reconstructor reported the narrowly typed field-length source mismatch.
 * The caller must have authenticated the observation and DA provenance first.
 */
export const fieldPreimageLengthEvidenceFromVerifiedPayload =
  exactFieldPreimageLengthRawFinding;

/**
 * Package-owned production classifier. It first admits the full canonical
 * block. Only the exact source/preimage authentication failure may fall back
 * to the raw transactions-root branch; every other reconstruction failure is
 * preserved as a rejection.
 */
export const detectAuthenticatedFieldPreimageLengthEvidence = async ({
  observation,
  sources,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
}): Promise<FieldPreimageLengthWorkflowEvidence> => {
  const admitted = await SDK.admitAuthenticatedStateQueueHeaderObservation({
    observation,
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: admitted.headerHash,
    sources,
  });
  const provenance = SDK.assertSecurityGradeEvidence(
    SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
  );
  try {
    return await fieldPreimageLengthEvidenceFromCanonicalBlock(
      await canonicalBlockEvidenceFromVerifiedPayload({
        observation: admitted,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
        daProvenance: provenance,
      }),
    );
  } catch (cause) {
    if (
      !(cause instanceof Error) ||
      cause.name !== "TransitionTraceChallengerError" ||
      !(
        cause.message.startsWith("Failed to authenticate transactions[") ||
        cause.message.startsWith("Failed to authenticate forced_transactions[")
      )
    ) {
      throw cause;
    }
    return await exactFieldPreimageLengthRawFinding({
      observation: admitted,
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    });
  }
};
