import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";

import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  prepareReceivePurposeLanguageEvidence,
  RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID,
  type ReceivePurposeLanguageDescriptor,
  type ReceivePurposeLanguageEvidence,
  receivePurposeLanguageEvidenceCloses,
} from "./family.js";

export type ReceivePurposeLanguageAuthenticatedDescriptor = Readonly<{
  transactionId: string;
  position: bigint;
  executionIndex: number;
  descriptor: ReceivePurposeLanguageDescriptor;
}>;
export type ReceivePurposeLanguageReplayFinding = Readonly<{
  detection: CanonicalViolationDetection;
  evidence: ReceivePurposeLanguageEvidence;
}>;

/** Detects only from descriptors already reconstructed from authenticated retained DA. */
export const detectReceivePurposeLanguageAcceptedReplay = ({
  headerHash,
  descriptors,
}: {
  readonly headerHash: string;
  readonly descriptors: readonly ReceivePurposeLanguageAuthenticatedDescriptor[];
}): readonly ReceivePurposeLanguageReplayFinding[] =>
  Object.freeze(
    descriptors
      .flatMap((entry) => {
        const evidence = prepareReceivePurposeLanguageEvidence({
          finding: {
            subject: acceptedVerdictSubject(entry.transactionId),
            executionIndex: entry.executionIndex,
          },
          descriptor: entry.descriptor,
        });
        if (!receivePurposeLanguageEvidenceCloses(evidence)) return [];
        return [
          Object.freeze({
            evidence,
            detection: Object.freeze({
              detectionId: `${RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID}:${entry.position.toString()}:${entry.transactionId}:${entry.executionIndex.toString()}`,
              headerHash,
              violationId: RECEIVE_PURPOSE_PLUTUS_V3_FORBIDDEN_VIOLATION_ID,
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

export const selectReceivePurposeLanguageCanonicalFinding = (
  findings: readonly ReceivePurposeLanguageReplayFinding[],
): ReceivePurposeLanguageReplayFinding => {
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
