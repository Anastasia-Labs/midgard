import { createHash } from "node:crypto";

import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import { type AcceptedHistoryObservation } from "./history-projection-observations.js";

export type RecordedHistoryBatch = {
  observations: readonly AcceptedHistoryObservation[];
  observedSlot: number;
  observedHeight: number;
  outputs: readonly LedgerSnapshotOutput[];
};

export const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");

export const label = (value: { txHash: string; outputIndex: number }) =>
  `${value.txHash}#${value.outputIndex}`;
