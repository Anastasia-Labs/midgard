import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";

import type { CanonicalViolationDetection } from "../workflow/classification-v1.js";
import {
  type ExecutionSourceDescriptor,
  type MissingScriptSourceEvidence,
  missingScriptSourceEvidenceCloses,
  missingScriptSourceViolationId,
  prepareMissingScriptSourceEvidence,
} from "./family-v1.js";

export type ExecutionSourceAuthenticatedDescriptor = Readonly<{
  executionIndex: number;
  descriptor: ExecutionSourceDescriptor;
}>;

export type MissingScriptSourceReplayFinding = Readonly<{
  detection: CanonicalViolationDetection;
  evidence: MissingScriptSourceEvidence;
}>;

/**
 * Accepted-arm replay over the retained raw envelope. Deliberately does not
 * call the full native transaction decoder: malformed native script payloads
 * are the evidence, and must survive until the frozen structural scan.
 *
 * The descriptors are accepted only after their purpose/source/execution
 * memberships verify in `prepareMissingScriptSourceEvidence` and
 * the selected field-6 item is byte-identical to the retained envelope.
 */
export const detectMissingScriptSourceAcceptedRawReplay = ({
  headerHash,
  position,
  canonicalTransactionCbor,
  authenticatedDescriptors,
}: {
  readonly headerHash: string;
  readonly position: bigint;
  readonly canonicalTransactionCbor: Uint8Array;
  readonly authenticatedDescriptors: readonly ExecutionSourceAuthenticatedDescriptor[];
}): readonly MissingScriptSourceReplayFinding[] => {
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    canonicalTransactionCbor,
  );
  const transactionId = material.transactionId.toString("hex");
  const items = decodeMidgardFieldPreimage(material.fieldPreimages[6]!);
  const findings = authenticatedDescriptors.flatMap(
    ({ executionIndex, descriptor }) => {
      if (descriptor.originKind !== 0) return [];
      const item = items[descriptor.sourceIndex];
      if (
        item === undefined ||
        item.toString("hex") !== descriptor.scriptItemHex
      )
        throw new Error("missingScriptSource raw field-6 source item changed");
      const evidence = prepareMissingScriptSourceEvidence({
        finding: {
          subject: acceptedVerdictSubject(transactionId),
          purposeKind: descriptor.purposeKind,
          purposeIndex: descriptor.purposeIndex,
          executionIndex,
        },
        descriptor,
      });
      if (!missingScriptSourceEvidenceCloses(evidence)) return [];
      const violationId = missingScriptSourceViolationId(
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

export const selectMissingScriptSourceCanonicalFinding = (
  findings: readonly MissingScriptSourceReplayFinding[],
): MissingScriptSourceReplayFinding => {
  const ordered = [...findings].sort((left, right) => {
    const position = Number(left.detection.position - right.detection.position);
    return (
      position ||
      left.detection.detectionId.localeCompare(right.detection.detectionId)
    );
  });
  if (ordered.length === 0)
    throw new Error(
      "missingScriptSource retained replay yielded no contradiction",
    );
  return ordered[0]!;
};
