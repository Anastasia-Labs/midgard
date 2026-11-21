export type AvailabilityOperationIntent = Readonly<{
  id: string;
  deploymentIdentity: string;
  actor: string;
  headerHash: string;
  action: string;
  signedCbor: string;
  txHash: string;
  spentOutRefs: readonly string[];
  collateralOutRefs: readonly string[];
  expectedOutRefs: readonly string[];
  validUntilSlot: number;
  completesWorkflow: boolean;
}>;

export type AvailabilityOperationRecord = Readonly<{
  intent: AvailabilityOperationIntent;
  state: "pending" | "included" | "confirmed" | "expired" | "conflict";
  inclusionPoint: string | null;
  detail: string | null;
  /** First authenticated post-confirmation/expiry height; a conservative depth bound. */
  retentionBlockNo?: number;
}>;

export type AvailabilityOperationLease = Readonly<{
  scope: string;
  owner: string;
  generation: number;
}>;

/** One live challenge workflow row of an actor (P9), as listed for release. */
export type AvailabilityOperationWorkflow = Readonly<{
  deploymentIdentity: string;
  headerHash: string;
  /** The actor's confirmed Opens for this header, oldest first. */
  confirmedOpens: readonly AvailabilityOperationRecord[];
}>;

/**
 * Why a workflow row ended in a terminal step someone else landed (P20): a
 * finalized, verified transaction burned the header's queue node or closed
 * its challenge. The journal checks only its own facts; the L1 evidence is
 * the caller's to verify.
 */
export type AvailabilityWorkflowReleaseEvidence = Readonly<{
  openIntentId: string;
  reason: "header-node-burned" | "challenge-closed";
  txHash: string;
  spendPoint: string;
  confirmationDepth: number;
  /** Past this depth the foreign terminal is final and no longer re-checked. */
  recoveryDepth: number;
}>;

/**
 * A workflow row a foreign terminal released whose evidence is not yet final:
 * it is re-verified from `open` on every reconciliation and the row revived
 * once that terminal is no longer canonical.
 */
export type AvailabilityOperationUnsettledRelease = Readonly<{
  deploymentIdentity: string;
  headerHash: string;
  open: AvailabilityOperationRecord;
  release: Pick<
    AvailabilityWorkflowReleaseEvidence,
    "reason" | "txHash" | "spendPoint"
  >;
}>;

/**
 * Authenticated evidence that a confirmed intent is still canonical. Its
 * reservations go once `currentSlot` passes an intent's validity, when the
 * signed bytes can never land again; its records go once `confirmationDepth`
 * exceeds `recoveryDepth`, past automatic rollback recovery.
 */
export type AvailabilityOperationRetirement = Readonly<{
  confirmationDepth: number;
  /** Absent when the observation carries no authoritative slot. */
  currentSlot?: number;
  /** Authenticated boundary height; block counts cannot be inferred from slots. */
  currentBlockNo?: number;
  inclusionPoint?: string;
  recoveryDepth: number;
}>;

/** One SQL read binds actor lease identity and all opaque interference counts.
 * Expired-but-unreleased lease rows remain busy until audited ownership release. */
export type AvailabilityOperationActorSnapshot = Readonly<{
  actor: string;
  deploymentIdentity: string;
  /** Exact lease, intent state, resource and workflow identities in this SQL snapshot. */
  stateDigest: string;
  lease?: Readonly<{ owner: string; generation: number; expiresAtMs: number }>;
  retainedRecordCount: number;
  /** Durable exact attempt identities in this actor/deployment, including expiry.
   * Consumers count aggregate failures from states; TTL passage is not a state. */
  retainedAttempts: readonly Readonly<{
    id: string;
    headerHash: string;
    action: string;
    txHash: string;
    state: AvailabilityOperationRecord["state"];
    validUntilSlot: number;
  }>[];
  pendingIntentCount: number;
  reservedResourceCount: number;
  /** Claims outside included/confirmed intents in this exact deployment. */
  incompatibleResourceCount: number;
  foreignWorkflowCount: number;
  /** Foreign capital guards including provisional terminal workflow shadows. */
  protectedForeignWorkflowCount: number;
  unsettledReleaseCount: number;
}>;

export interface AvailabilityOperationJournal {
  actorSnapshot(
    actor: string,
    deploymentIdentity: string,
  ): AvailabilityOperationActorSnapshot;
  /** All retained intent rows across actors, deployments and states; read-only. */
  retainedRecordCount(): number;
  acquire(
    scope: string,
    owner: string,
    nowMs: number,
    durationMs: number,
  ): AvailabilityOperationLease;
  assertLease(lease: AvailabilityOperationLease, nowMs: number): void;
  release(lease: AvailabilityOperationLease): void;
  pending(
    deploymentIdentity: string,
    actor: string,
  ): readonly AvailabilityOperationRecord[];
  unfinalized(
    deploymentIdentity: string,
    actor: string,
  ): readonly AvailabilityOperationRecord[];
  /**
   * The actor's confirmed intents no confirmed intent spends, in every
   * deployment: a redeploy's earlier records still retire and prune.
   */
  finalizedAnchors(actor: string): readonly AvailabilityOperationRecord[];
  /** All unresolved wallet resources, including intents for other deployments. */
  reservedOutRefs(actor: string): readonly string[];
  /**
   * Refuses every step while the actor has a live challenge workflow in a
   * different deployment (including provisional terminal progress), and a 'prepare' for a header whose own Open landed.
   * Any other header's step in the same deployment is admitted.
   */
  assertWorkflow(
    lease: AvailabilityOperationLease,
    deploymentIdentity: string,
    headerHash: string,
    action: string,
    nowMs: number,
  ): void;
  /** The actor's live workflow rows in every deployment. */
  workflows(actor: string): readonly AvailabilityOperationWorkflow[];
  /**
   * Retires the lease actor's workflow row for (deployment, header) once its
   * challenge ended in someone else's terminal step (P20), or refreshes the
   * evidence of a row it retired that is not yet final. The evidence is kept
   * until `confirmationDepth` exceeds `recoveryDepth`. Refuses unless the
   * actor has no pending, included or conflicting intent for that header and
   * `evidence.openIntentId` is its confirmed Open for it. Only the row
   * changes: reservations, intents and leases are untouched.
   */
  releaseWorkflow(
    lease: AvailabilityOperationLease,
    deploymentIdentity: string,
    headerHash: string,
    evidence: AvailabilityWorkflowReleaseEvidence,
    nowMs: number,
  ): void;
  /** Released rows whose foreign terminal is not final, in every deployment. */
  unsettledReleases(
    actor: string,
  ): readonly AvailabilityOperationUnsettledRelease[];
  /**
   * Makes an unsettled released row live again once its foreign terminal is
   * no longer canonical evidence. Refuses any other row.
   */
  reviveWorkflow(
    lease: AvailabilityOperationLease,
    deploymentIdentity: string,
    headerHash: string,
    nowMs: number,
  ): void;
  get(id: string): AvailabilityOperationRecord | null;
  findTransaction(txHash: string): AvailabilityOperationRecord | null;
  persist(
    lease: AvailabilityOperationLease,
    intent: AvailabilityOperationIntent,
    nowMs: number,
  ): void;
  transition(
    lease: AvailabilityOperationLease,
    id: string,
    state: AvailabilityOperationRecord["state"],
    inclusionPoint: string | null,
    detail: string | null,
    nowMs: number,
  ): void;
  /**
   * Returns a confirmed intent to pending once canonical evidence contradicts
   * its inclusion. Its reservations come back and, for an Open or terminal
   * step, its header's workflow is live again, so the same signed bytes can
   * land again; nothing is re-signed. When another intent reserved one of
   * its inputs meanwhile, both claims stay, the intent becomes a conflict,
   * and its recorded detail is returned; otherwise null.
   */
  rewind(
    lease: AvailabilityOperationLease,
    id: string,
    detail: string,
    nowMs: number,
  ): string | null;
  /** Applies {@link AvailabilityOperationRetirement} to `id` and its confirmed ancestors. */
  retire(
    lease: AvailabilityOperationLease,
    id: string,
    evidence: AvailabilityOperationRetirement,
    nowMs: number,
  ): void;
  /** Retains expired progress beyond the recovery horizon, and while a child needs it. */
  pruneExpired(
    lease: AvailabilityOperationLease,
    currentBlockNo: number,
    recoveryDepth: number,
    nowMs: number,
  ): void;
  close(): void;
}
