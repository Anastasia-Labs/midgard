import type * as Journal from "../database/eventHistoryJournal.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.history-owner-change.js";

export const historyOwnerCoverage = (
  checkpoint: Journal.Checkpoint | null,
  slotToUnixTime: (slot: number) => number,
): HistoryOwnerCoverage & {
  readonly retention: NonNullable<HistoryOwnerCoverage["retention"]>;
} => {
  if (checkpoint === null) throw new Error("History checkpoint is missing");
  const includedThroughMs = slotToUnixTime(checkpoint.head.slot);
  const retainedFromMs = slotToUnixTime(checkpoint.anchor.slot);
  if (
    !Number.isSafeInteger(includedThroughMs) ||
    !Number.isSafeInteger(retainedFromMs) ||
    retainedFromMs > includedThroughMs
  )
    throw new Error("History checkpoint has an invalid time mapping");
  return Object.freeze({
    bindingDigest: checkpoint.bindingDigest,
    checkpointRevision: checkpoint.revision,
    point: Object.freeze({ ...checkpoint.head }),
    snapshotDigest: checkpoint.capture.snapshotDigest,
    includedThroughMs,
    retention: Object.freeze({
      anchor: Object.freeze({ ...checkpoint.anchor }),
      includedThroughMs: retainedFromMs,
    }),
  });
};
