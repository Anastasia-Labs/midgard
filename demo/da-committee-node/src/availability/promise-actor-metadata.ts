import type {
  AvailabilityOperationActorSnapshot,
  AvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";

/** Compatibility means a fresh real reconciliation receipt is available;
 * these counters alone never authorize financial clearance or resource release. */
export const promiseAdmissionActorMetadata = (
  input: Readonly<{
    journal: AvailabilityOperationJournal;
    actorId: string;
    deploymentIdentity: string;
    maximumRetainedRecords?: number;
    hasCanonicalCompatibilityAuthority: boolean;
  }>,
) => ({
  busy: (metadata: AvailabilityOperationActorSnapshot): boolean =>
    (metadata.lease !== undefined && metadata.lease.expiresAtMs > 0) ||
    metadata.pendingIntentCount > 0 ||
    metadata.incompatibleResourceCount > 0 ||
    (metadata.reservedResourceCount > 0 &&
      !input.hasCanonicalCompatibilityAuthority) ||
    metadata.protectedForeignWorkflowCount > 0 ||
    metadata.unsettledReleaseCount > 0,
  snapshot: (): AvailabilityOperationActorSnapshot => {
    // Count before constructing the full retained-identity digest.
    const maximum = input.maximumRetainedRecords;
    if (
      maximum !== undefined &&
      (!Number.isSafeInteger(maximum) || maximum < 0)
    )
      throw new Error("Journal retained-row domain is invalid");
    if (maximum !== undefined && input.journal.retainedRecordCount() > maximum)
      throw new Error("Journal exceeds the adopted retained-row domain");
    const metadata = input.journal.actorSnapshot(
      input.actorId,
      input.deploymentIdentity,
    );
    if (maximum !== undefined && metadata.retainedRecordCount > maximum)
      throw new Error("Journal exceeds the adopted retained-row domain");
    return metadata;
  },
});
