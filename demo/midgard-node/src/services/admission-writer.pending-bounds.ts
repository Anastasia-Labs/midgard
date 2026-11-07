import {
  AdmissionWriterFullError,
  AdmissionWriterStalledError,
} from "./admission-writer.validate-options.js";

/** What the pending set holds when a new tx asks to join it. */
export type PendingSetUse = {
  readonly oldestPendingAgeMs: number;
  readonly stallMs: number;
  readonly rows: number;
  readonly rowCapacity: number;
  readonly bytes: number;
  readonly byteCapacity: number;
  /** The new tx's payload bytes. */
  readonly txBytes: number;
};

/**
 * Why the pending set cannot take one more tx, or `undefined` if it can: its
 * writes are stalled, or its row or byte bound is full.
 */
export const pendingSetRefusal = (
  use: PendingSetUse,
): AdmissionWriterFullError | AdmissionWriterStalledError | undefined => {
  if (use.oldestPendingAgeMs > use.stallMs)
    return new AdmissionWriterStalledError({
      message: "Admission writes are stalled; retry once the node is ready",
      oldestPendingAgeMs: use.oldestPendingAgeMs,
      stallMs: use.stallMs,
    });
  if (use.rows >= use.rowCapacity)
    return new AdmissionWriterFullError({
      message: "Admission writer row bound is full",
      unit: "rows",
      used: use.rows,
      capacity: use.rowCapacity,
    });
  if (use.bytes + use.txBytes > use.byteCapacity)
    return new AdmissionWriterFullError({
      message: "Admission writer byte bound is full",
      unit: "bytes",
      used: use.bytes,
      capacity: use.byteCapacity,
    });
  return undefined;
};
