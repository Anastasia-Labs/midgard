import type { DaPayloadRecord, DaStoredPayloadRecord } from "../domain.js";
import { hasPayloadBytes } from "../store.committee-store.js";
import { withDerivedPayloadFetchStatus } from "../store.parse-decision-outbox-record.js";

const terminalPayloadStatuses = new Set<DaPayloadRecord["validationStatus"]>([
  "verified",
  "malformed_da",
  "root_mismatch",
  "conflicted",
]);

export const resolveDaPayloadSave = (
  existing: DaStoredPayloadRecord | undefined,
  record: DaStoredPayloadRecord,
): DaStoredPayloadRecord => {
  if (existing === undefined) {
    return withDerivedPayloadFetchStatus(record);
  }
  if (hasPayloadBytes(existing) && !hasPayloadBytes(record)) {
    return existing;
  }
  if (
    hasPayloadBytes(existing) &&
    hasPayloadBytes(record) &&
    existing.payloadSha256 !== record.payloadSha256
  ) {
    return {
      ...withDerivedPayloadFetchStatus(record),
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
      validationError: `payload bytes conflict with existing sha256 ${existing.payloadSha256}`,
    };
  }
  if (
    hasPayloadBytes(existing) &&
    hasPayloadBytes(record) &&
    existing.payloadSha256 === record.payloadSha256 &&
    terminalPayloadStatuses.has(existing.validationStatus) &&
    !terminalPayloadStatuses.has(record.validationStatus)
  ) {
    return existing;
  }
  return withDerivedPayloadFetchStatus(record);
};
