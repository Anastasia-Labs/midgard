import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { verdictDetectionSubject } from "../workflow/detection-subject.js";
import { completeReplayFindings } from "../workflow/replay-prerequisite.js";
import {
  fieldItemWidthDetectionId,
  fieldItemWidthFindingsFromCanonicalBlock,
} from "./field-item-width-illegal.js";

export const FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID =
  "field-item-width-illegal" as const;

/** Shares exact coordinates and original bytes with public recovery. */
export const detectFieldItemWidthIllegalCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const { findings, failures } =
    fieldItemWidthFindingsFromCanonicalBlock(block);
  return completeReplayFindings(
    findings.map(({ evidence, position }) => ({
      ...verdictDetectionSubject(evidence.subject),
      detectionId: fieldItemWidthDetectionId(
        { evidence, position },
        FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
      ),
      headerHash: block.headerHash,
      violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
      position,
      diagnostic: `transaction ${evidence.subject.transaction_id} contradicts its verdict at field ${evidence.fieldIndex.toString()} item ${evidence.itemIndex.toString()}`,
    })),
    failures,
  );
};
