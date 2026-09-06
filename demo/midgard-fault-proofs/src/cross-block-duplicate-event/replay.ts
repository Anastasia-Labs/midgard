import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { CrossBlockDuplicateEventKindInput } from "./prepare.js";
import {
  type CrossBlockSettlementContext,
  crossBlockSettlementRecords,
} from "./settlement-authority.js";
export type CrossBlockDuplicateCoordinate = Readonly<{
  settledHeaderHash: string;
  kind: CrossBlockDuplicateEventKindInput;
  transactionId: string;
  outputIndex: string;
}>;
export const crossBlockDuplicateDetectionId = (
  c: CrossBlockDuplicateCoordinate,
) =>
  `cross-block-duplicate-event:${c.settledHeaderHash}:${c.kind}:${c.transactionId}#${c.outputIndex}`;
export const detectCrossBlockDuplicateEvents = ({
  evidence,
  context,
}: {
  evidence: CanonicalBlockEvidence;
  context: CrossBlockSettlementContext;
}): readonly CanonicalViolationDetection[] => {
  const detections: CanonicalViolationDetection[] = [];
  for (const settled of crossBlockSettlementRecords(evidence, context)) {
    for (const kind of [
      "deposit",
      "withdrawal",
      "forced-transaction",
    ] as const) {
      const field =
        kind === "deposit"
          ? "deposits"
          : kind === "withdrawal"
            ? "withdrawals"
            : "forcedTransactions";
      const keys = new Set(
        settled.reconstruction[field].map(
          (entry) => `${entry.key.transactionId}#${entry.key.outputIndex}`,
        ),
      );
      for (const entry of evidence.reconstruction[field]) {
        if (!keys.has(`${entry.key.transactionId}#${entry.key.outputIndex}`))
          continue;
        const coordinate: CrossBlockDuplicateCoordinate = {
          settledHeaderHash: settled.headerHash,
          kind,
          transactionId: entry.key.transactionId,
          outputIndex: entry.key.outputIndex.toString(),
        };
        const position = evidence.reconstruction.sourceEvents.findIndex(
          (source) => source.entry === entry,
        );
        if (position < 0)
          throw new Error(
            "duplicate event lacks its canonical source position",
          );
        detections.push({
          headerHash: evidence.headerHash,
          violationId: SDK.CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
          detectionId: crossBlockDuplicateDetectionId(coordinate),
          position: BigInt(position),
          diagnostic: JSON.stringify(coordinate),
        });
      }
    }
  }
  return detections;
};
