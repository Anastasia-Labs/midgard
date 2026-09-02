import { acceptedVerdictSubjectV1 } from "@al-ft/midgard-sdk";

import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  prepareReceivePurposeLanguageEvidenceV1,
  RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID_V1,
  type ReceivePurposeLanguageDescriptorV1,
  receivePurposeLanguageEvidenceClosesV1,
  type ReceivePurposeLanguageEvidenceV1,
} from "./family-v1.js";

export type ReceivePurposeLanguageAuthenticatedDescriptorV1 = Readonly<{
  transactionId: string;
  position: bigint;
  executionIndex: number;
  descriptor: ReceivePurposeLanguageDescriptorV1;
}>;
export type ReceivePurposeLanguageReplayFindingV1 = Readonly<{
  detection: CanonicalViolationDetectionV1;
  evidence: ReceivePurposeLanguageEvidenceV1;
}>;

/** Detects only from descriptors already reconstructed from authenticated retained DA. */
export const detectReceivePurposeLanguageAcceptedReplayV1 = ({
  headerHash,
  descriptors,
}: {
  readonly headerHash: string;
  readonly descriptors: readonly ReceivePurposeLanguageAuthenticatedDescriptorV1[];
}): readonly ReceivePurposeLanguageReplayFindingV1[] =>
  Object.freeze(
    descriptors
      .flatMap((entry) => {
        const evidence = prepareReceivePurposeLanguageEvidenceV1({
          finding: {
            subject: acceptedVerdictSubjectV1(entry.transactionId),
            executionIndex: entry.executionIndex,
          },
          descriptor: entry.descriptor,
        });
        if (!receivePurposeLanguageEvidenceClosesV1(evidence)) return [];
        return [
          Object.freeze({
            evidence,
            detection: Object.freeze({
              detectionId: `${RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID_V1}:${entry.position.toString()}:${entry.transactionId}:${entry.executionIndex.toString()}`,
              headerHash,
              violationId: RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID_V1,
              position: entry.position,
              diagnostic: `accepted receive execution ${entry.executionIndex.toString()} selected forbidden PlutusV3`,
            }),
          }),
        ];
      })
      .sort((left, right) =>
        left.detection.position === right.detection.position
          ? left.detection.detectionId.localeCompare(
              right.detection.detectionId,
            )
          : left.detection.position < right.detection.position
            ? -1
            : 1,
      ),
  );

export const selectReceivePurposeLanguageCanonicalFindingV1 = (
  findings: readonly ReceivePurposeLanguageReplayFindingV1[],
): ReceivePurposeLanguageReplayFindingV1 => {
  if (findings.length === 0)
    throw new Error(
      "receivePurposeLanguage retained replay yielded no contradiction",
    );
  return [...findings].sort((left, right) =>
    left.detection.position === right.detection.position
      ? left.detection.detectionId.localeCompare(right.detection.detectionId)
      : left.detection.position < right.detection.position
        ? -1
        : 1,
  )[0]!;
};
