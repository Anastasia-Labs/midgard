import { mkdir, realpath, stat } from "node:fs/promises";
import { dirname } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
  parseWorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  type WatcherProverFundingReservationTransition,
} from "./prover-funding-reservation.js";
import {
  finalProverFundingCompletion as finalCompletion,
  pendingProverFundingLineage,
  pendingProverFundingLineageParameters,
  proverFundingReobservationInputs,
  retainedProverFundingInputs,
  signedCollateralOutRefs,
} from "./prover-funding-reservation.retained-signed-inputs.js";
import { createProverFundingAbandonmentRecords } from "./sqlite-prover-funding-reservation-store.abandonment-records.js";
import {
  assertPlanMatchesRecord,
  canonicalDatabasePath,
  deriveSignedTransition,
  HEX_32,
  identicalInputs,
  initialRecord,
  makeTransition,
  nextRecord,
  ReservationConflictError,
  WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE,
  type WatcherSqliteProverFundingReservationStoreRuntime,
} from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import { projectRetainedProverFundingLeases } from "./sqlite-prover-funding-reservation-store.lease-claims.js";
import { createProverFundingCancelRows } from "./sqlite-prover-funding-reservation-store.orphan-cancel.js";
import { createProverFundingReclaimOperations } from "./sqlite-prover-funding-reservation-store.orphaned-reservation.js";
import {
  type CompletionHandoff,
  type HandoffRow,
  type LeaseRow,
  type LineageRow,
  parseHandoffRow,
  parseLineageRow,
  parseRecordRow,
  type RecordRow,
  type SubmissionHandoff,
  unanchored,
} from "./sqlite-prover-funding-reservation-store.row-parsers.js";
import {
  excludesEveryUncoveredAttempt,
  landedSupersededAttemptOfAction,
  supersededAttemptFundingOutRefs,
  uncoveredSupersededAttempts,
} from "./sqlite-prover-funding-reservation-store.superseded-exclusion.js";
import {
  createVerifiedRowMemo,
  verifiedRowKey,
} from "./sqlite-prover-funding-reservation-store.verified-row-memo.js";

export const openInternal = async (
  input: Readonly<{ path: string; busyTimeoutMs?: number }>,
  assertPlan: (plan: WatcherProverFundingReservationPlan) => void,
): Promise<WatcherSqliteProverFundingReservationStoreRuntime> => {
  const path = canonicalDatabasePath(input.path);
  const directory = dirname(path);
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if ((await realpath(directory)) !== directory) {
    throw new Error("prover funding reservation directory traverses a symlink");
  }
  try {
    if (
      (await stat(path)).isSymbolicLink() ||
      (await realpath(path)) !== path
    ) {
      throw new Error("prover funding reservation path traverses a symlink");
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const busyTimeoutMs = input.busyTimeoutMs ?? 5_000;
  if (
    !Number.isSafeInteger(busyTimeoutMs) ||
    busyTimeoutMs < 1 ||
    busyTimeoutMs > 120_000
  ) {
    throw new Error("prover funding reservation busy timeout is invalid");
  }

  const database = new DatabaseSync(path, {
    open: true,
    readOnly: false,
    enableForeignKeyConstraints: true,
  });
  database.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA trusted_schema = OFF;
    PRAGMA busy_timeout = ${busyTimeoutMs.toString()};
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_reservation_v1 (
      reservation_id TEXT PRIMARY KEY CHECK (length(reservation_id) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0)
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_lease_v1 (
      out_ref TEXT PRIMARY KEY,
      reservation_id TEXT NOT NULL,
      lease_phase TEXT NOT NULL CHECK (lease_phase IN ('active', 'pending')),
      role TEXT NOT NULL CHECK (role IN ('funding', 'collateral')),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_handoff_v1 (
      reservation_id TEXT NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('submission', 'completion')),
      identity_digest TEXT NOT NULL CHECK (length(identity_digest) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0),
      PRIMARY KEY (reservation_id, kind, identity_digest),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS watcher_prover_funding_completion_handoff_v1
      ON watcher_prover_funding_handoff_v1(reservation_id) WHERE kind = 'completion';
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_abandonment_v1 (
      reservation_id TEXT NOT NULL,
      transition_digest TEXT NOT NULL CHECK(length(transition_digest) = 64),
      record_digest TEXT NOT NULL CHECK(length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK(length(canonical_json) > 0),
      acknowledged_revision TEXT,
      PRIMARY KEY (reservation_id, transition_digest),
      FOREIGN KEY (reservation_id) REFERENCES watcher_prover_funding_reservation_v1(reservation_id) ON DELETE CASCADE
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS watcher_prover_funding_unacknowledged_abandonment_v1
      ON watcher_prover_funding_abandonment_v1(reservation_id) WHERE acknowledged_revision IS NULL;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_lineage_v1 (
      reservation_id TEXT NOT NULL,
      action_kind TEXT NOT NULL,
      output_index INTEGER NOT NULL CHECK (output_index >= 0),
      out_ref TEXT NOT NULL UNIQUE,
      resolved_output_cbor_hex TEXT NOT NULL,
      transition_digest TEXT NOT NULL CHECK (length(transition_digest) = 64),
      phase TEXT NOT NULL CHECK (phase IN ('pending', 'confirmed')),
      lineage_digest TEXT NOT NULL CHECK (length(lineage_digest) = 64),
      PRIMARY KEY (reservation_id, transition_digest, output_index),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
  `);

  const selectAll = database.prepare(`
    SELECT reservation_id, record_digest, canonical_json
    FROM watcher_prover_funding_reservation_v1
    ORDER BY reservation_id ASC
  `);
  const selectOne = database.prepare(`
    SELECT reservation_id, record_digest, canonical_json
    FROM watcher_prover_funding_reservation_v1
    WHERE reservation_id = ?
  `);
  const insertRecord = database.prepare(`
    INSERT INTO watcher_prover_funding_reservation_v1(
      reservation_id, record_digest, canonical_json
    ) VALUES (?, ?, ?)
  `);
  const updateRecord = database.prepare(`
    UPDATE watcher_prover_funding_reservation_v1
    SET record_digest = ?, canonical_json = ?
    WHERE reservation_id = ? AND record_digest = ?
  `);
  const selectAllLeases = database.prepare(`
    SELECT out_ref, reservation_id, lease_phase, role
    FROM watcher_prover_funding_lease_v1
    ORDER BY out_ref ASC
  `);
  const selectLease = database.prepare(`
    SELECT reservation_id
    FROM watcher_prover_funding_lease_v1
    WHERE out_ref = ?
  `);
  const insertLease = database.prepare(`
    INSERT INTO watcher_prover_funding_lease_v1(
      out_ref, reservation_id, lease_phase, role
    ) VALUES (?, ?, ?, ?)
  `);
  const deleteLeases = database.prepare(`
    DELETE FROM watcher_prover_funding_lease_v1
    WHERE reservation_id = ?
  `);
  const insertLineage = database.prepare(`
    INSERT INTO watcher_prover_funding_lineage_v1(
      reservation_id, action_kind, output_index, out_ref,
      resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    ) VALUES (?, ?, ?, ?, ?, ?, 'pending', ?)
  `);
  const confirmLineage = database.prepare(`
    UPDATE watcher_prover_funding_lineage_v1
    SET phase = 'confirmed'
    WHERE reservation_id = ? AND transition_digest = ? AND phase = 'pending'
  `);
  const deletePendingLineage = database.prepare(`
    DELETE FROM watcher_prover_funding_lineage_v1
    WHERE reservation_id = ? AND transition_digest = ? AND phase = 'pending'
  `);
  const selectConfirmedLineage = database.prepare(`
    SELECT reservation_id, action_kind, output_index, out_ref,
           resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    FROM watcher_prover_funding_lineage_v1
    WHERE reservation_id = ? AND out_ref = ?
          AND phase = 'confirmed'
  `);
  // The audit reads every retained row's key and digest only; a row's full
  // content is read when its key is new.
  const selectLineageKeys = database.prepare(`
    SELECT reservation_id, action_kind, output_index, out_ref,
           transition_digest, phase, lineage_digest
    FROM watcher_prover_funding_lineage_v1
    ORDER BY reservation_id ASC, transition_digest ASC, output_index ASC
  `);
  const selectLineage = database.prepare(`
    SELECT reservation_id, action_kind, output_index, out_ref,
           resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    FROM watcher_prover_funding_lineage_v1
    WHERE reservation_id = ? AND transition_digest = ? AND output_index = ?
  `);

  const insertHandoff = database.prepare(`
    INSERT INTO watcher_prover_funding_handoff_v1(
      reservation_id, kind, identity_digest, record_digest, canonical_json
    ) VALUES (?, ?, ?, ?, ?)
  `);
  const selectHandoff = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest, canonical_json
    FROM watcher_prover_funding_handoff_v1
    WHERE reservation_id = ? AND kind = ? AND identity_digest = ?
  `);
  const selectCompletionHandoff = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest, canonical_json
    FROM watcher_prover_funding_handoff_v1
    WHERE reservation_id = ? AND kind = 'completion'
  `);
  const selectHandoffKeys = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest
    FROM watcher_prover_funding_handoff_v1
    ORDER BY reservation_id ASC, kind ASC, identity_digest ASC
  `);
  const selectSubmissionKeys = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest
    FROM watcher_prover_funding_handoff_v1
    WHERE reservation_id = ? AND kind = 'submission'
    ORDER BY identity_digest ASC
  `);
  const selectSignedHistory = database.prepare(`
    SELECT EXISTS (SELECT 1 FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ?1)
        OR EXISTS (SELECT 1 FROM watcher_prover_funding_lineage_v1 WHERE reservation_id = ?1)
        OR EXISTS (SELECT 1 FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ?1)
        AS present
  `);
  const selectDataVersion = database.prepare("PRAGMA data_version");
  const selectTotalChanges = database.prepare(
    "SELECT total_changes() AS changes",
  );
  const {
    retireLegacyAbandonment,
    insertAbandonment,
    abandonmentsOf,
    auditedAbandonments,
    acknowledgeAbandonment,
    unacknowledgedAbandonment,
    acknowledgeOutpacedAbandonments,
    legacyAbandonedTransactions,
  } = createProverFundingAbandonmentRecords(database);
  const handoffMemo = createVerifiedRowMemo(
    (saved: SubmissionHandoff | CompletionHandoff) => ({
      kind: saved.kind,
      identity: saved.handoff.identity,
      final: saved.kind === "completion" && finalCompletion(saved.handoff),
    }),
  );
  const handoffKey = (row: Omit<HandoffRow, "canonical_json">) =>
    verifiedRowKey(
      row.reservation_id,
      row.kind,
      row.identity_digest,
      row.record_digest,
    );
  const readHandoffRow = (row: HandoffRow) =>
    handoffMemo.read(handoffKey(row), () => parseHandoffRow(row));
  const fetchHandoff = (row: Omit<HandoffRow, "canonical_json">) => () => {
    const full = selectHandoff.get(
      row.reservation_id as string,
      row.kind as string,
      row.identity_digest as string,
    ) as HandoffRow | undefined;
    if (full === undefined || full.record_digest !== row.record_digest)
      throw new Error("prover funding handoff row changed during audit");
    return parseHandoffRow(full);
  };
  const assertHandoffReservation = (
    handoff: Readonly<{
      identity: Readonly<{
        deploymentFingerprint: string;
        decisionDigest?: string | undefined;
      }>;
    }>,
    record: WatcherProverFundingReservationRecord,
  ) => {
    if (
      handoff.identity.deploymentFingerprint !== record.deploymentFingerprint ||
      handoff.identity.decisionDigest !== record.decisionDigest
    )
      throw new Error(
        "prover funding handoff differs from reservation identity",
      );
  };
  const persistHandoff = (
    record: WatcherProverFundingReservationRecord,
    kind: "submission" | "completion",
    identityDigest: string,
    value: unknown,
  ) => {
    const canonicalJson = watcherCanonicalJson(value);
    const row = {
      reservation_id: record.reservationId,
      kind,
      identity_digest: identityDigest,
      record_digest: computeDeploymentManifestJsonDigest(value),
      canonical_json: canonicalJson,
    };
    assertHandoffReservation(readHandoffRow(row).handoff, record);
    insertHandoff.run(
      record.reservationId,
      kind,
      identityDigest,
      row.record_digest,
      canonicalJson,
    );
  };

  // The phase lies outside the lineage digest, so the audit reads it from
  // the row; the rest is verified once.
  const lineageMemo = createVerifiedRowMemo(
    (saved: ReturnType<typeof parseLineageRow>) => saved.transitionDigest,
  );

  const recordMemo = createVerifiedRowMemo(
    (record: WatcherProverFundingReservationRecord) => record.state,
  );
  const readRecordRow = (row: RecordRow) =>
    recordMemo.read(verifiedRowKey(row.reservation_id, row.record_digest), () =>
      parseRecordRow(row),
    );
  const readOne = (
    reservationId: string,
  ): WatcherProverFundingReservationRecord | null => {
    const row = selectOne.get(reservationId) as RecordRow | undefined;
    return row === undefined ? null : readRecordRow(row);
  };

  const writeRecord = (
    currentDigest: string | null,
    next: WatcherProverFundingReservationRecord,
  ): void => {
    const canonicalJson = watcherCanonicalJson(next);
    if (currentDigest === null) {
      insertRecord.run(next.reservationId, next.recordDigest, canonicalJson);
      return;
    }
    const result = updateRecord.run(
      next.recordDigest,
      canonicalJson,
      next.reservationId,
      currentDigest,
    );
    if (result.changes !== 1) {
      throw new Error("prover funding reservation compare-and-swap failed");
    }
  };

  const assertLeaseAvailable = (outRef: string, reservationId: string) => {
    const row = selectLease.get(outRef) as
      | Readonly<{ reservation_id: unknown }>
      | undefined;
    if (row === undefined) return;
    if (row.reservation_id !== reservationId) {
      throw new ReservationConflictError(outRef);
    }
  };

  // Preserve a later job's lease after an irreversible release.
  const outputTransferredToAnotherReservation = (
    reservationId: string,
    outRef: string,
  ): boolean => {
    const lease = selectLease.get(outRef) as
      | Readonly<{ reservation_id: string }>
      | undefined;
    if (lease === undefined || lease.reservation_id === reservationId)
      return false;
    const lineage = selectConfirmedLineage.get(reservationId, outRef) as
      | LineageRow
      | undefined;
    return (
      lineage !== undefined &&
      parseLineageRow(lineage).outRef === outRef &&
      readOne(lease.reservation_id)?.activeInputs.some(
        (input) => input.outRef === outRef,
      ) === true
    );
  };

  const leaseInputs = (record: WatcherProverFundingReservationRecord) => {
    const completion = selectCompletionHandoff.get(record.reservationId) as
      | HandoffRow
      | undefined;
    const saved =
      completion === undefined ? undefined : readHandoffRow(completion);
    const completed =
      saved?.kind === "completion" && finalCompletion(saved.handoff);
    // A final completion retains no signed attempt, so none is read.
    const abandoned = completed ? [] : abandonmentsOf(record.reservationId);
    return retainedProverFundingInputs({
      record,
      submissions: completed
        ? []
        : [...recordedSubmissions(record.reservationId), ...abandoned].map(
            ({ transition }) => transition,
          ),
      abandonedTransactionHashes: new Set(
        abandoned
          .filter(
            (saved) => saved.handoff.reconciliation.retirement !== undefined,
          )
          .map((saved) => saved.transition.transactionHash),
      ),
      completed,
    });
  };

  const readAll = () => (selectAll.all() as RecordRow[]).map(readRecordRow);
  const computeLeaseProjection = (all = readAll()) =>
    projectRetainedProverFundingLeases({
      records: all,
      readInputs: leaseInputs,
      readAttempts: (id) =>
        [...recordedSubmissions(id), ...legacyAbandonedTransactions(id)].map(
          ({ transition }) => transition,
        ),
      readUnverifiedHashes: (id) =>
        new Set(
          legacyAbandonedTransactions(id)
            .filter(
              ({ handoff }) => handoff.reconciliation.retirement === undefined,
            )
            .map(({ transition }) => transition.transactionHash),
        ),
      transferred: outputTransferredToAnotherReservation,
      previousOwners: new Map(
        (selectAllLeases.all() as LeaseRow[]).map((row) => [
          String(row.out_ref),
          String(row.reservation_id),
        ]),
      ),
    });

  /** The audited store is unchanged while neither this connection nor any
   * other has written since; its audit and lease projection then stand. */
  const storeVersion = () =>
    `${String((selectDataVersion.get() as { data_version: unknown }).data_version)}:${String((selectTotalChanges.get() as { changes: unknown }).changes)}`;
  let verified: Readonly<{
    version: string;
    records: readonly WatcherProverFundingReservationRecord[];
    projection: ReturnType<typeof computeLeaseProjection>;
  }> | null = null;
  const leaseProjection = () =>
    verified !== null && verified.version === storeVersion()
      ? verified.projection
      : computeLeaseProjection();

  const rebuildLeases = (): void => {
    const projection = leaseProjection();
    for (const record of readAll()) deleteLeases.run(record.reservationId);
    for (const lease of projection.leases)
      insertLease.run(
        lease.outRef,
        lease.reservationId,
        lease.phase,
        lease.role,
      );
  };

  const persistPendingLineage = (input: {
    readonly reservationId: string;
    readonly transition: WatcherProverFundingReservationTransition;
    readonly signedTransactionCborHex: string;
  }): number => {
    const identities = pendingProverFundingLineage(input);
    for (const identity of identities)
      insertLineage.run(...pendingProverFundingLineageParameters(identity));
    return identities.length;
  };

  const recordedSubmissions = (reservationId: string): SubmissionHandoff[] =>
    (
      selectSubmissionKeys.all(reservationId) as Omit<
        HandoffRow,
        "canonical_json"
      >[]
    )
      .map((row) => handoffMemo.read(handoffKey(row), fetchHandoff(row)))
      .filter((row): row is SubmissionHandoff => row.kind === "submission");

  /** A superseded attempt that landed becomes the pending transition again,
   * under the handoff that re-records its intent in the journal. The pending
   * attempt is re-recorded the same way when its workflow journal was lost:
   * the same exact intent, anchored at the recreated journal's tip. */
  const adoptSupersededAttempt = (
    current: WatcherProverFundingReservationRecord,
    transition: ReturnType<typeof makeTransition>,
    adoption: unknown,
  ) => {
    const handoff = parseWorkflowFundingSubmissionHandoff(adoption);
    const superseded = legacyAbandonedTransactions(current.reservationId).find(
      (saved) =>
        saved.transition.transactionHash === transition.transactionHash,
    );
    const row = selectHandoff.get(
      current.reservationId,
      "submission",
      transition.transitionDigest,
    ) as HandoffRow | undefined;
    const saved = row === undefined ? undefined : readHandoffRow(row);
    const attempt = (value: typeof handoff) =>
      unanchored({
        ...value,
        submissionIntent: { ...value.submissionIntent, attempt: null },
      });
    if (
      current.pendingTransition?.transitionDigest ===
      transition.transitionDigest
    ) {
      if (
        saved?.kind !== "submission" ||
        attempt(saved.handoff) !== attempt(handoff)
      )
        throw new Error("prover reservation journal re-anchor mismatch");
    } else if (
      current.pendingTransition !== null ||
      superseded === undefined ||
      superseded.handoff.reconciliation.retirement !== undefined ||
      handoff.submissionIntent.txHash !== transition.transactionHash ||
      handoff.submissionIntent.actionId !==
        superseded.handoff.submissionIntent.actionId
    )
      throw new Error("prover reservation adoption mismatch");
    database
      .prepare(
        "DELETE FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ? AND transition_digest = ?",
      )
      .run(current.reservationId, transition.transitionDigest);
    database
      .prepare(
        "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ? AND kind = 'submission' AND identity_digest = ?",
      )
      .run(current.reservationId, transition.transitionDigest);
    const { transitionDigest: _digest, ...signedTransition } = transition;
    persistHandoff(current, "submission", transition.transitionDigest, {
      transition: signedTransition,
      handoff,
    });
  };

  const uncoveredSuperseded = (reservationId: string) => {
    const abandoned = legacyAbandonedTransactions(reservationId);
    // Only a superseded attempt can be uncovered.
    return abandoned.length === 0
      ? []
      : uncoveredSupersededAttempts({
          abandoned,
          submissions: recordedSubmissions(reservationId).map(
            ({ transition }) => transition,
          ),
        });
  };

  const landedSupersededAttempt = (
    current: WatcherProverFundingReservationRecord,
    actionId: string,
    signedTransactionCborHex: string,
  ) =>
    landedSupersededAttemptOfAction({
      abandoned: legacyAbandonedTransactions(current.reservationId),
      submissions: recordedSubmissions(current.reservationId).map(
        ({ transition }) => transition,
      ),
      actionId,
      signedTransactionCborHex,
      reservedOutRefs: current.activeInputs.map(({ outRef }) => outRef),
    });

  const reobservationInputs = (
    reservationId: string,
    transactionHash: string,
  ) => {
    const record = readOne(reservationId);
    if (record === null) throw new Error("prover reservation is missing");
    return proverFundingReobservationInputs({
      record,
      transactionHash,
      submissions: recordedSubmissions(reservationId).map(
        ({ transition }) => transition,
      ),
    });
  };

  /** Verifies every retained row; the caller adopts the result as the
   * verified store only once its transaction commits. */
  const audit = () => {
    const all = readAll();
    const byReservation = new Map(
      all.map((record) => [record.reservationId, record]),
    );
    const projection = computeLeaseProjection(all);
    const expected = new Map(
      projection.leases.map((lease) => [lease.outRef, lease]),
    );
    const leases = selectAllLeases.all() as LeaseRow[];
    if (leases.length !== expected.size) {
      throw new Error("prover funding reservation lease set mismatch");
    }
    for (const row of leases) {
      if (
        typeof row.out_ref !== "string" ||
        typeof row.reservation_id !== "string" ||
        (row.lease_phase !== "active" && row.lease_phase !== "pending") ||
        (row.role !== "funding" && row.role !== "collateral")
      ) {
        throw new Error("prover funding reservation lease is malformed");
      }
      const value = expected.get(row.out_ref);
      if (
        value === undefined ||
        value.reservationId !== row.reservation_id ||
        value.phase !== row.lease_phase ||
        value.role !== row.role
      ) {
        throw new Error("prover funding reservation lease metadata mismatch");
      }
    }
    const lineageSeen = new Set<string>();
    for (const row of selectLineageKeys.all() as Omit<
      LineageRow,
      "resolved_output_cbor_hex"
    >[]) {
      const key = verifiedRowKey(
        row.reservation_id,
        row.action_kind,
        row.output_index,
        row.out_ref,
        row.transition_digest,
        row.lineage_digest,
      );
      lineageSeen.add(key);
      const transitionDigest = lineageMemo.summary(key, () => {
        const full = selectLineage.get(
          row.reservation_id as string,
          row.transition_digest as string,
          row.output_index as number,
        ) as LineageRow | undefined;
        if (full === undefined || full.lineage_digest !== row.lineage_digest)
          throw new Error("prover funding lineage row changed during audit");
        return parseLineageRow(full);
      });
      if (row.phase !== "pending" && row.phase !== "confirmed")
        throw new Error("prover funding lineage row is malformed");
      const pending = byReservation.get(
        row.reservation_id as string,
      )?.pendingTransition;
      if (
        row.phase === "pending" &&
        (pending == null || pending.transitionDigest !== transitionDigest)
      ) {
        throw new Error(
          "prover funding pending lineage differs from reservation",
        );
      }
    }
    const handoffSeen = new Set<string>();
    for (const row of selectHandoffKeys.all() as Omit<
      HandoffRow,
      "canonical_json"
    >[]) {
      const key = handoffKey(row);
      handoffSeen.add(key);
      const handoff = handoffMemo.summary(key, fetchHandoff(row));
      const record = byReservation.get(row.reservation_id as string);
      if (record === undefined)
        throw new Error("prover funding handoff has no reservation");
      assertHandoffReservation(handoff, record);
      if (handoff.final && record.state !== "released")
        throw new Error(
          "prover funding completion handoff has unreleased inputs",
        );
    }
    const abandonments = auditedAbandonments();
    for (const abandoned of abandonments.rows) {
      const record = byReservation.get(abandoned.reservationId);
      if (record === undefined)
        throw new Error("prover funding abandonment has no reservation");
      assertHandoffReservation(abandoned, record);
      if (
        abandoned.retired &&
        abandoned.acknowledgedRevision === null &&
        (record.pendingTransition !== null || record.state === "released")
      )
        throw new Error(
          "unacknowledged funding abandonment overlaps a new transition or release",
        );
      if (
        abandoned.acknowledgedRevision !== null &&
        BigInt(abandoned.acknowledgedRevision) > BigInt(record.revision)
      )
        throw new Error(
          "funding abandonment acknowledgement is ahead of its reservation",
        );
    }
    const released = new Set(
      all
        .filter(({ state }) => state === "released")
        .map(({ reservationId }) => reservationId),
    );
    recordMemo.retain(
      new Set(
        all.map((record) =>
          verifiedRowKey(record.reservationId, record.recordDigest),
        ),
      ),
      new Set(),
    );
    lineageMemo.retain(lineageSeen, released);
    handoffMemo.retain(handoffSeen, released);
    abandonments.retain(released);
    return Object.freeze({
      version: storeVersion(),
      records: Object.freeze(all),
      projection,
    });
  };

  const assertSubmissionAuthority = (reservationId: string) => {
    if (leaseProjection().heldReservationIds.has(reservationId))
      throw new Error(
        "prover funding reservation overlaps legacy signed attempts; reconciliation only",
      );
  };
  /** Holds only the reservation the operation acts for, before and after it,
   * so a fresh operation can neither use nor create a legacy overlap. */
  const transaction = <T>(
    operation: () => T,
    reservationId: string | null,
  ): T => {
    database.exec("BEGIN IMMEDIATE");
    try {
      acknowledgeOutpacedAbandonments(recordedSubmissions, readOne);
      if (reservationId !== null) assertSubmissionAuthority(reservationId);
      const value = operation();
      if (reservationId !== null) assertSubmissionAuthority(reservationId);
      const audited = audit();
      database.exec("COMMIT");
      verified = audited;
      return value;
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the first storage/authority failure.
      }
      throw error;
    }
  };

  const auditRead = () => {
    database.exec("BEGIN DEFERRED");
    try {
      const audited =
        verified !== null && verified.version === storeVersion()
          ? verified
          : audit();
      database.exec("COMMIT");
      verified = audited;
      return audited.records;
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the first storage/authority failure.
      }
      throw error;
    }
  };

  const cancels = createProverFundingCancelRows(database);
  const hasSignedHistory = (reservationId: string): boolean =>
    (selectSignedHistory.get(reservationId) as { present: unknown }).present ===
      1 || cancels.has(reservationId);

  transaction(rebuildLeases, null);

  const store: WatcherProverFundingReservationStore = Object.freeze({
    readAll: async () => auditRead(),
    isReconciliationOnly: async ({ reservationId }) => {
      auditRead();
      return leaseProjection().heldReservationIds.has(reservationId);
    },
    assertSubmissionAuthority: async ({ reservationId }) => {
      auditRead();
      assertSubmissionAuthority(reservationId);
    },
    readReservedOutRefs: async ({ excludingReservationId }) => {
      auditRead();
      return [
        ...new Set(
          leaseProjection()
            .claims.filter(
              (claim) => claim.reservationId !== excludingReservationId,
            )
            .map((claim) => claim.outRef),
        ),
      ];
    },
    hasSignedHistory: async ({ reservationId }) => {
      if (!auditRead().some((record) => record.reservationId === reservationId))
        throw new Error("prover funding history reservation is missing");
      return hasSignedHistory(reservationId);
    },
    retireLegacyAbandonment: async ({
      plan,
      expectedRevision,
      transactionHash,
      retirement,
    }) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null || current.revision !== expectedRevision)
          throw new Error("Legacy retirement revision changed");
        assertPlanMatchesRecord(plan, current);
        const next = nextRecord({ current });
        if (
          !retireLegacyAbandonment(
            plan.reservationId,
            transactionHash,
            retirement,
            next.revision,
          )
        )
          return current;
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, null);
    },
    readLegacyAbandonedTransactions: async ({ reservationId }) => {
      auditRead();
      return legacyAbandonedTransactions(reservationId);
    },
    readRecordedSubmissions: async ({ reservationId }) => {
      auditRead();
      return recordedSubmissions(reservationId).map(
        ({ transition, handoff }) => ({ transition, handoff }),
      );
    },
    readAbandonmentHandoff: async ({ reservationId }) => {
      const record = auditRead().find(
        (value) => value.reservationId === reservationId,
      );
      const abandoned =
        record === undefined ? null : unacknowledgedAbandonment(record);
      return abandoned === null
        ? null
        : { transition: abandoned.transition, handoff: abandoned.handoff };
    },
    readPendingTransition: async ({ reservationId }) => {
      const current = auditRead().find(
        (record) => record.reservationId === reservationId,
      );
      if (current?.pendingTransition == null) return null;
      const { transitionDigest: _digest, ...transition } =
        current.pendingTransition;
      return transition;
    },
    readPendingHandoff: async ({ reservationId }) => {
      const current = auditRead().find(
        (record) => record.reservationId === reservationId,
      );
      if (current?.pendingTransition == null) return null;
      const row = selectHandoff.get(
        reservationId,
        "submission",
        current.pendingTransition.transitionDigest,
      ) as HandoffRow | undefined;
      if (row === undefined) return null;
      const recovered = readHandoffRow(row);
      if (recovered.kind !== "submission")
        throw new Error("prover funding pending handoff kind mismatch");
      return { transition: recovered.transition, handoff: recovered.handoff };
    },
    readSupersededAttemptFundingOutRefs: async ({ reservationId }) => {
      auditRead();
      return supersededAttemptFundingOutRefs(
        uncoveredSuperseded(reservationId),
      );
    },
    readLandedSupersededAttempt: async ({
      reservationId,
      actionId,
      signedTransactionCborHex,
    }) => {
      auditRead();
      const current = readOne(reservationId);
      return current === null
        ? null
        : landedSupersededAttempt(current, actionId, signedTransactionCborHex);
    },
    readRecordedSignedTransaction: async ({
      reservationId,
      transactionHash,
    }) => {
      auditRead();
      return (
        recordedSubmissions(reservationId).find(
          ({ transition }) => transition.transactionHash === transactionHash,
        )?.transition.signedTransactionCborHex ?? null
      );
    },
    readReobservationInputs: async ({ reservationId, transactionHash }) => {
      auditRead();
      return reobservationInputs(reservationId, transactionHash);
    },
    reobserveTransition: async ({
      plan,
      expectedRevision,
      transactionHash,
      inputs,
      adoption,
    }) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (
          current.revision !== expectedRevision ||
          current.state === "conflict" ||
          unacknowledgedAbandonment(current) !== null
        )
          throw new Error("prover reservation reobservation mismatch");
        const completionRow = selectCompletionHandoff.get(
          current.reservationId,
        ) as HandoffRow | undefined;
        if (completionRow !== undefined) {
          const saved = readHandoffRow(completionRow);
          if (saved.kind !== "completion" || finalCompletion(saved.handoff))
            throw new Error("anchored prover reservation cannot be reopened");
        }
        const recorded = recordedSubmissions(current.reservationId).find(
          ({ transition }) => transition.transactionHash === transactionHash,
        );
        if (recorded === undefined)
          throw new Error(
            "reobserved transaction has no signed funding intent",
          );
        const candidates = new Map(
          reobservationInputs(current.reservationId, transactionHash).map(
            ({ outRef, role }) => [outRef, role],
          ),
        );
        const transition = makeTransition(recorded.transition);
        if (adoption !== undefined)
          adoptSupersededAttempt(current, transition, adoption);
        const produced = new Set(
          transition.producedInputs.map(({ outRef }) => outRef),
        );
        for (const value of inputs) {
          if (candidates.get(value.outRef) !== value.role)
            throw new Error("reobserved funding input has no recorded lineage");
          if (!produced.has(value.outRef))
            assertLeaseAvailable(value.outRef, current.reservationId);
        }
        // Only canonical inputs returned by the authenticated wallet source are
        // selected again. Historical signed attempts stay in the handoff table.
        const next = nextRecord({
          current,
          state: "active",
          activeInputs: inputs.filter(({ outRef }) => !produced.has(outRef)),
          pendingTransition: transition,
          conflictCode: null,
        });
        if (current.pendingTransition !== null)
          deletePendingLineage.run(
            current.reservationId,
            current.pendingTransition.transitionDigest,
          );
        const confirmed = selectConfirmedLineage.get(
          current.reservationId,
          `${transition.transactionHash}#0`,
        );
        if (confirmed === undefined)
          persistPendingLineage({
            reservationId: current.reservationId,
            transition,
            signedTransactionCborHex: transition.signedTransactionCborHex,
          });
        database
          .prepare(
            "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ? AND kind = 'completion'",
          )
          .run(current.reservationId);
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, plan.reservationId);
    },
    readCompletionHandoff: async ({ reservationId }) => {
      auditRead();
      const row = selectCompletionHandoff.get(reservationId) as
        | HandoffRow
        | undefined;
      if (row === undefined) return null;
      const recovered = readHandoffRow(row);
      if (recovered.kind !== "completion")
        throw new Error("prover funding completion handoff kind mismatch");
      return recovered.handoff;
    },
    readConfirmedInput: async ({ reservationId, outRef }) => {
      const row = selectConfirmedLineage.get(reservationId, outRef) as
        | LineageRow
        | undefined;
      if (row === undefined) return null;
      const lineage = parseLineageRow(row);
      return Object.freeze({
        sourceActionKind: lineage.sourceActionKind,
        sourceOutputIndex: lineage.sourceOutputIndex,
        outRef: lineage.outRef,
        resolvedOutputCborHex: lineage.resolvedOutputCborHex,
      });
    },
    ...createProverFundingReclaimOperations({
      database,
      transaction,
      assertPlan,
      readOne,
      writeRecord,
      rebuildLeases,
      recordedSubmissions,
      abandonmentsOf,
      hasSignedHistory,
      cancels,
      persistHandoff,
    }),
    reserve: async (plan, expectedIdleRevision) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current !== null) {
          assertPlanMatchesRecord(plan, current);
          if (
            current.revision === "0" &&
            !identicalInputs(current.activeInputs, plan.inputs)
          ) {
            throw new Error("prover funding reservation plan was substituted");
          }
          if (current.state === "released") {
            throw new Error("prover funding reservation was released");
          }
          if (current.state === "conflict") {
            throw new Error("prover funding reservation is conflicted");
          }
          if (expectedIdleRevision !== undefined) {
            if (
              current.revision !== expectedIdleRevision ||
              current.activeInputs.length !== 0 ||
              current.pendingTransition !== null ||
              unacknowledgedAbandonment(current) !== null
            )
              throw new Error("prover reservation cannot refresh idle inputs");
            for (const value of plan.inputs)
              assertLeaseAvailable(value.outRef, plan.reservationId);
            const next = nextRecord({ current, activeInputs: plan.inputs });
            writeRecord(current.recordDigest, next);
            rebuildLeases();
            return "reserved" as const;
          }
          return "unchanged" as const;
        }
        if (expectedIdleRevision !== undefined)
          throw new Error("idle prover reservation is missing");
        for (const value of plan.inputs) {
          assertLeaseAvailable(value.outRef, plan.reservationId);
        }
        const record = initialRecord(plan);
        writeRecord(null, record);
        rebuildLeases();
        return "reserved" as const;
      }, plan.reservationId);
    },
    prepareTransition: async (transitionInput) => {
      assertPlan(transitionInput.plan);
      return transaction(() => {
        const current = readOne(transitionInput.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(transitionInput.plan, current);
        if (
          current.state !== "active" ||
          current.pendingTransition !== null ||
          unacknowledgedAbandonment(current) !== null ||
          current.revision !== transitionInput.expectedRevision
        ) {
          throw new Error("prover reservation cannot prepare transition");
        }
        const activeOutRefs = new Set(
          current.activeInputs.map(({ outRef }) => outRef),
        );
        if (
          transitionInput.consumedOutRefs.some(
            (outRef) => !activeOutRefs.has(outRef),
          )
        ) {
          throw new Error("prover transition consumes an unreserved input");
        }
        if (
          landedSupersededAttempt(
            current,
            transitionInput.handoff.submissionIntent.actionId,
            transitionInput.signedTransactionCborHex,
          ) !== null
        )
          throw new Error(
            "prover transition repeats an action whose superseded attempt landed",
          );
        if (
          !excludesEveryUncoveredAttempt({
            uncovered: uncoveredSuperseded(current.reservationId),
            signedTransactionCborHex: transitionInput.signedTransactionCborHex,
            reservedFundingOutRefs: current.activeInputs
              .filter(({ role }) => role === "funding")
              .map(({ outRef }) => outRef),
          })
        )
          throw new Error(
            "prover transition must share an input with each superseded attempt",
          );
        const transition = makeTransition(
          deriveSignedTransition({
            plan: transitionInput.plan,
            activeInputs: current.activeInputs,
            input: transitionInput,
          }),
        );
        const { transitionDigest: _digest, ...signedTransition } = transition;
        persistHandoff(current, "submission", transition.transitionDigest, {
          transition: signedTransition,
          handoff: transitionInput.handoff,
        });
        persistPendingLineage({
          reservationId: current.reservationId,
          transition,
          signedTransactionCborHex: transitionInput.signedTransactionCborHex,
        });
        for (const value of transition.producedInputs) {
          assertLeaseAvailable(value.outRef, current.reservationId);
        }
        const next = nextRecord({
          current,
          pendingTransition: transition,
        });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, transitionInput.plan.reservationId);
    },
    confirmTransition: async (confirmation) => {
      assertPlan(confirmation.plan);
      return transaction(() => {
        const current = readOne(confirmation.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(confirmation.plan, current);
        if (
          current.state !== "active" ||
          current.revision !== confirmation.expectedRevision ||
          !HEX_32.test(confirmation.transactionHash)
        ) {
          throw new Error("prover reservation confirmation mismatch");
        }
        if (current.pendingTransition === null) {
          // SQLite confirmation can commit before its acknowledgement reaches
          // the workflow journal. Reconciliation may acknowledge that exact
          // committed transition again, without rotating leases or revision.
          const row = selectConfirmedLineage.get(
            current.reservationId,
            `${confirmation.transactionHash}#0`,
          ) as LineageRow | undefined;
          if (
            current.lastConfirmedTransitionDigest !== null &&
            current.lastConfirmedTransitionDigest ===
              confirmation.transitionDigest &&
            row !== undefined &&
            parseLineageRow(row).transitionDigest ===
              confirmation.transitionDigest
          )
            return current;
          throw new Error("prover reservation confirmation mismatch");
        }
        if (
          current.pendingTransition.transitionDigest !==
            confirmation.transitionDigest ||
          current.pendingTransition.transactionHash !==
            confirmation.transactionHash
        ) {
          throw new Error("prover reservation confirmation mismatch");
        }
        // A confirmed transaction's collateral lease ends here, so it never
        // blocks a later action of any reservation. A rollback that finds it
        // spent re-signs with fresh collateral; this reservation's next action
        // selects collateral again.
        const consumed = new Set([
          ...current.pendingTransition.consumedOutRefs,
          ...signedCollateralOutRefs(
            current.pendingTransition.signedTransactionCborHex,
          ),
        ]);
        const active = [
          ...current.activeInputs.filter(({ outRef }) => !consumed.has(outRef)),
          ...current.pendingTransition.producedInputs.filter(
            ({ outRef }) =>
              !outputTransferredToAnotherReservation(
                current.reservationId,
                outRef,
              ),
          ),
        ];
        const next = nextRecord({
          current,
          activeInputs: active,
          pendingTransition: null,
          lastConfirmedTransitionDigest:
            current.pendingTransition.transitionDigest,
        });
        const confirmedLineage = confirmLineage.run(
          current.reservationId,
          current.pendingTransition.transitionDigest,
        );
        if (confirmedLineage.changes < 1) {
          const prior = selectConfirmedLineage.get(
            current.reservationId,
            `${confirmation.transactionHash}#0`,
          ) as LineageRow | undefined;
          if (
            prior === undefined ||
            parseLineageRow(prior).transitionDigest !==
              confirmation.transitionDigest
          )
            throw new Error("prover reservation confirmation lacks lineage");
        }
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, confirmation.plan.reservationId);
    },
    abandonPendingTransition: async (abandonment) => {
      assertPlan(abandonment.plan);
      return transaction(() => {
        const current = readOne(abandonment.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(abandonment.plan, current);
        const handoff = parseWorkflowFundingAbandonmentHandoff(
          abandonment.handoff,
        );
        assertHandoffReservation(handoff, current);
        // Without a retirement receipt the attempt is superseded, not retired:
        // its funding stays leased and the replacement must spend some of it.
        if (
          current.state !== "active" ||
          current.revision !== abandonment.expectedRevision
        )
          throw new Error("prover reservation abandonment mismatch");
        if (current.pendingTransition === null) {
          const saved = unacknowledgedAbandonment(current);
          if (
            saved === null ||
            saved.transitionDigest !== abandonment.transitionDigest ||
            computeDeploymentManifestJsonDigest(saved.handoff) !==
              computeDeploymentManifestJsonDigest(handoff)
          )
            throw new Error("prover reservation abandonment mismatch");
          return current;
        }
        const { transitionDigest, ...transition } = current.pendingTransition;
        if (
          transitionDigest !== abandonment.transitionDigest ||
          handoff.reconciliation.txHash !== transition.transactionHash
        )
          throw new Error("prover reservation abandonment mismatch");
        const value = { transition, handoff };
        insertAbandonment.run(
          current.reservationId,
          transitionDigest,
          computeDeploymentManifestJsonDigest(value),
          watcherCanonicalJson(value),
        );
        const next = nextRecord({ current, pendingTransition: null });
        deletePendingLineage.run(current.reservationId, transitionDigest);
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, abandonment.plan.reservationId);
    },
    releaseIdle: async ({ plan, expectedRevision }) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (current.revision !== expectedRevision)
          throw new Error("prover reservation idle release revision changed");
        // Releasing frees leases and is not actuation, but a held reservation's
        // leases are part of the legacy overlap, so they stay until it resolves.
        if (
          leaseProjection().heldReservationIds.has(plan.reservationId) ||
          current.state !== "active" ||
          current.activeInputs.length === 0 ||
          current.pendingTransition !== null ||
          unacknowledgedAbandonment(current) !== null
        )
          return current;
        // The workflow authority checks no signed attempt is in flight (confirmed
        // suffices, retirement is not awaited); this admits spent unsigned inputs.
        const next = nextRecord({ current, activeInputs: [] });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, null);
    },
    acknowledgeAbandonment: async (acknowledgement) => {
      assertPlan(acknowledgement.plan);
      return transaction(() => {
        const current = readOne(acknowledgement.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(acknowledgement.plan, current);
        const handoff = parseWorkflowFundingAbandonmentHandoff(
          acknowledgement.handoff,
        );
        assertHandoffReservation(handoff, current);
        const candidates = abandonmentsOf(current.reservationId).filter(
          (saved) =>
            saved.transition.transactionHash === handoff.reconciliation.txHash,
        );
        const saved = candidates[0];
        if (
          candidates.length !== 1 ||
          saved === undefined ||
          current.state !== "active" ||
          current.pendingTransition !== null ||
          current.revision !== acknowledgement.expectedRevision ||
          computeDeploymentManifestJsonDigest(saved.handoff) !==
            computeDeploymentManifestJsonDigest(handoff)
        )
          throw new Error(
            "prover reservation abandonment acknowledgement mismatch",
          );
        if (saved.acknowledgedRevision !== null) {
          if (saved.acknowledgedRevision !== current.revision)
            throw new Error(
              "prover reservation abandonment acknowledgement revision changed",
            );
          return current;
        }
        const next = nextRecord({ current });
        if (
          acknowledgeAbandonment.run(
            next.revision,
            current.reservationId,
            saved.transitionDigest,
          ).changes !== 1
        )
          throw new Error(
            "prover reservation abandonment acknowledgement raced",
          );
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, acknowledgement.plan.reservationId);
    },
    markConflict: async (conflict) => {
      assertPlan(conflict.plan);
      return transaction(() => {
        const current = readOne(conflict.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(conflict.plan, current);
        if (
          current.state !== "active" ||
          current.revision !== conflict.expectedRevision
        ) {
          throw new Error("prover reservation conflict transition mismatch");
        }
        const next = nextRecord({
          current,
          state: "conflict",
          pendingTransition: null,
          conflictCode: conflict.code,
        });
        if (current.pendingTransition !== null) {
          deletePendingLineage.run(
            current.reservationId,
            current.pendingTransition.transitionDigest,
          );
        }
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, conflict.plan.reservationId);
    },
    release: async (release) => {
      assertPlan(release.plan);
      return transaction(() => {
        const current = readOne(release.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(release.plan, current);
        const handoff = parseWorkflowFundingCompletionHandoff(release.handoff);
        assertHandoffReservation(handoff, current);
        if (
          current.revision !== release.expectedRevision ||
          current.pendingTransition !== null ||
          unacknowledgedAbandonment(current) !== null
        )
          throw new Error("prover reservation release mismatch");
        const digest = computeDeploymentManifestJsonDigest(handoff);
        const priorRow = selectCompletionHandoff.get(current.reservationId) as
          | HandoffRow
          | undefined;
        if (priorRow !== undefined) {
          const prior = readHandoffRow(priorRow);
          if (prior.kind !== "completion")
            throw new Error("prover completion handoff is malformed");
          if (prior.identityDigest === digest) return current;
          // A final completion is only re-anchored in a recreated journal.
          if (
            finalCompletion(prior.handoff) &&
            unanchored(prior.handoff) !== unanchored(handoff)
          )
            throw new Error("prover reservation release handoff mismatch");
          database
            .prepare(
              "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ? AND kind = 'completion'",
            )
            .run(current.reservationId);
        }
        if (
          current.state !== "active" &&
          !(current.state === "released" && priorRow !== undefined)
        )
          throw new Error("prover reservation release mismatch");
        persistHandoff(current, "completion", digest, handoff);
        const next = nextRecord({
          current,
          state: finalCompletion(handoff) ? "released" : "active",
          activeInputs: finalCompletion(handoff) ? [] : current.activeInputs,
          pendingTransition: null,
          conflictCode: null,
        });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, release.plan.reservationId);
    },
  });

  return Object.freeze({
    schemaVersion: WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE,
    store,
    close: () => database.close(),
  });
};
