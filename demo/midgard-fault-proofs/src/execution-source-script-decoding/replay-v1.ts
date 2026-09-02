import {
  decodeMidgardFieldPreimageV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubjectV1 } from "@al-ft/midgard-sdk";

import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  type ExecutionSourceDescriptorV1,
  executionSourceScriptDecodingEvidenceClosesV1,
  type ExecutionSourceScriptDecodingEvidenceV1,
  executionSourceScriptDecodingViolationIdV1,
  prepareExecutionSourceScriptDecodingEvidenceV1,
} from "./family-v1.js";

export type ExecutionSourceAuthenticatedDescriptorV1 = Readonly<{
  executionIndex: number;
  descriptor: ExecutionSourceDescriptorV1;
}>;

export type ExecutionSourceScriptDecodingReplayFindingV1 = Readonly<{
  detection: CanonicalViolationDetectionV1;
  evidence: ExecutionSourceScriptDecodingEvidenceV1;
}>;

/**
 * Accepted-arm replay over the retained raw envelope. Deliberately does not
 * call the full native transaction decoder: malformed native script payloads
 * are the evidence, and must survive until the frozen structural scan.
 *
 * The descriptors are accepted only after their purpose/source/execution
 * memberships verify in `prepareExecutionSourceScriptDecodingEvidenceV1` and
 * the selected field-6 item is byte-identical to the retained envelope.
 */
export const detectExecutionSourceScriptDecodingAcceptedRawReplayV1 = ({
  headerHash,
  position,
  canonicalTransactionCbor,
  authenticatedDescriptors,
}: {
  readonly headerHash: string;
  readonly position: bigint;
  readonly canonicalTransactionCbor: Uint8Array;
  readonly authenticatedDescriptors: readonly ExecutionSourceAuthenticatedDescriptorV1[];
}): readonly ExecutionSourceScriptDecodingReplayFindingV1[] => {
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    canonicalTransactionCbor,
  );
  const transactionId = material.transactionId.toString("hex");
  const items = decodeMidgardFieldPreimageV1(material.fieldPreimages[6]!);
  const findings = authenticatedDescriptors.flatMap(
    ({ executionIndex, descriptor }) => {
      if (descriptor.originKind !== 0) return [];
      const item = items[descriptor.sourceIndex];
      if (
        item === undefined ||
        item.toString("hex") !== descriptor.scriptItemHex
      )
        throw new Error(
          "executionSourceScriptDecoding raw field-6 source item changed",
        );
      const evidence = prepareExecutionSourceScriptDecodingEvidenceV1({
        finding: {
          subject: acceptedVerdictSubjectV1(transactionId),
          executionIndex,
        },
        descriptor,
      });
      if (!executionSourceScriptDecodingEvidenceClosesV1(evidence)) return [];
      const violationId = executionSourceScriptDecodingViolationIdV1(
        evidence.resultClass as 0 | 1 | 2,
      );
      return [
        Object.freeze({
          evidence,
          detection: Object.freeze({
            detectionId: `${violationId}:${position.toString()}:${transactionId}:${executionIndex.toString()}`,
            headerHash,
            violationId,
            position,
            diagnostic: `accepted transaction ${transactionId} has undecodable execution source ${executionIndex.toString()}`,
          }),
        }),
      ];
    },
  );
  return Object.freeze(
    findings.sort(
      (left, right) =>
        left.evidence.finding.executionIndex -
          right.evidence.finding.executionIndex ||
        left.detection.detectionId.localeCompare(right.detection.detectionId),
    ),
  );
};

export const selectExecutionSourceScriptDecodingCanonicalFindingV1 = (
  findings: readonly ExecutionSourceScriptDecodingReplayFindingV1[],
): ExecutionSourceScriptDecodingReplayFindingV1 => {
  const ordered = [...findings].sort((left, right) => {
    const position = Number(left.detection.position - right.detection.position);
    return (
      position ||
      left.detection.detectionId.localeCompare(right.detection.detectionId)
    );
  });
  if (ordered.length === 0)
    throw new Error(
      "executionSourceScriptDecoding retained replay yielded no contradiction",
    );
  return ordered[0]!;
};
