import { mkdir, realpath, stat } from "node:fs/promises";
import { dirname } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofWorkflowJournalEntry,
  parseWorkflowFundingRecordedAttempt,
  type WorkflowFundingRecordedAttempt,
} from "@al-ft/midgard-fault-proofs";

import { createWatcherWorkflowJournal } from "../fault-proofs/watcher-workflow-journal.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  type WatcherProverFundingReservationTransition,
} from "./prover-funding-reservation.js";
import {
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
  identicalInputs,
  initialRecord,
  makeTransition,
  nextRecord,
  ReservationConflictError,
  WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE,
  type WatcherSqliteProverFundingReservationStoreRuntime,
} from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import { projectRetainedProverFundingLeases } from "./sqlite-prover-funding-reservation-store.lease-claims.js";
import { createProverFundingReclaimOperations } from "./sqlite-prover-funding-reservation-store.orphaned-reservation.js";
import {
  type AttemptRow,
  type LeaseRow,
  type LineageRow,
  parseLineageRow,
  parseRecordedAttemptRow,
  parseRecordRow,
  type RecordedAttempt,
  type RecordRow,
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
import { openProverFundingRevision } from "./sqlite-prover-funding-revision.js";

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
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_attempt_v1 (
      reservation_id TEXT NOT NULL,
      transition_digest TEXT NOT NULL CHECK (length(transition_digest) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0),
      PRIMARY KEY (reservation_id, transition_digest),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_superseded_attempt_v1 (
      reservation_id TEXT NOT NULL,
      transition_digest TEXT NOT NULL CHECK (length(transition_digest) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0),
      PRIMARY KEY (reservation_id, transition_digest),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
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
  const journal = createWatcherWorkflowJournal(database);
  const fundingRevision = openProverFundingRevision(database);

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

  const insertAttempt = database.prepare(`
    INSERT INTO watcher_prover_funding_attempt_v1(
      reservation_id, transition_digest, record_digest, canonical_json
    ) VALUES (?, ?, ?, ?)
  `);
  const deleteAttempt = database.prepare(`
    DELETE FROM watcher_prover_funding_attempt_v1
    WHERE reservation_id = ? AND transition_digest = ?
  `);
  const selectAttempt = database.prepare(`
    SELECT reservation_id, transition_digest, record_digest, canonical_json
    FROM watcher_prover_funding_attempt_v1
    WHERE reservation_id = ? AND transition_digest = ?
  `);
  const selectAttemptKeys = database.prepare(`
    SELECT reservation_id, transition_digest, record_digest
    FROM watcher_prover_funding_attempt_v1
    ORDER BY reservation_id ASC, transition_digest ASC
  `);
  const selectAttemptKeysOf = database.prepare(`
    SELECT reservation_id, transition_digest, record_digest
    FROM watcher_prover_funding_attempt_v1
    WHERE reservation_id = ?
    ORDER BY transition_digest ASC
  `);
  const selectSignedHistory = database.prepare(`
    SELECT EXISTS (SELECT 1 FROM watcher_prover_funding_attempt_v1 WHERE reservation_id = ?1)
        OR EXISTS (SELECT 1 FROM watcher_prover_funding_lineage_v1 WHERE reservation_id = ?1)
        OR EXISTS (SELECT 1 FROM watcher_prover_funding_superseded_attempt_v1 WHERE reservation_id = ?1)
        AS present
  `);
  const {
    retireSupersededAttempt,
    insertAbandonment,
    deleteAbandonment,
    abandonmentsOf,
    auditedAbandonments,
    supersededAttempts,
  } = createProverFundingAbandonmentRecords(database);
  const attemptMemo = createVerifiedRowMemo(
    (saved: RecordedAttempt) => saved.reservationId,
  );
  const attemptKey = (row: Omit<AttemptRow, "canonical_json">) =>
    verifiedRowKey(
      row.reservation_id,
      row.transition_digest,
      row.record_digest,
    );
  const fetchAttempt = (row: Omit<AttemptRow, "canonical_json">) => () => {
    const full = selectAttempt.get(
      row.reservation_id as string,
      row.transition_digest as string,
    ) as AttemptRow | undefined;
    if (full === undefined || full.record_digest !== row.record_digest)
      throw new Error("prover funding attempt row changed during audit");
    return parseRecordedAttemptRow(full);
  };
  const persistAttempt = (
    reservationId: string,
    transitionDigest: string,
    attempt: WorkflowFundingRecordedAttempt,
  ) => {
    const value = parseWorkflowFundingRecordedAttempt(attempt);
    insertAttempt.run(
      reservationId,
      transitionDigest,
      computeDeploymentManifestJsonDigest(value),
      watcherCanonicalJson(value),
    );
  };
  /** The workflow journal entries a change commits must be of the
   * reservation's own deployment and decision. */
  const assertJournalIdentity = (
    plan: WatcherProverFundingReservationPlan,
    entries: readonly FraudProofWorkflowJournalEntry[],
  ) => {
    if (
      entries.some(
        ({ identity }) =>
          identity.deploymentFingerprint !== plan.deploymentFingerprint ||
          identity.decisionDigest !== plan.decisionDigest,
      )
    )
      throw new Error(
        "workflow journal entry differs from its funding reservation",
      );
  };
  /** When the change records its intent, the last entry is that intent. */
  const assertIntentEntry = (
    entries: readonly FraudProofWorkflowJournalEntry[],
    attempt: Omit<WorkflowFundingRecordedAttempt, "transition">,
    transactionHash: string,
  ) => {
    const last = entries.at(-1);
    if (
      last !== undefined &&
      (last.event.kind !== "submission_intent" ||
        last.event.txHash !== transactionHash ||
        last.event.actionId !== attempt.actionId ||
        last.workflowId !== attempt.workflowId ||
        last.sequence !== attempt.intentSequence)
    )
      throw new Error("funding attempt differs from its journal intent");
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
    // A released reservation retains no signed attempt, so none is read.
    const completed = record.state === "released";
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
          .filter((saved) => saved.retirement !== undefined)
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
      transferred: outputTransferredToAnotherReservation,
    });

  /** While the funding revision stands, the audit and lease projection
   * stand. */
  const storeVersion = fundingRevision.read;
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

  const recordedSubmissions = (
    reservationId: string,
  ): readonly RecordedAttempt[] =>
    (
      selectAttemptKeysOf.all(reservationId) as Omit<
        AttemptRow,
        "canonical_json"
      >[]
    ).map((row) => attemptMemo.read(attemptKey(row), fetchAttempt(row)));

  /** A superseded attempt that landed becomes the pending transition again,
   * its intent recorded again at `intentSequence` of its workflow journal. */
  const adoptSupersededAttempt = (
    current: WatcherProverFundingReservationRecord,
    transition: ReturnType<typeof makeTransition>,
    intentSequence: number,
    entries: readonly FraudProofWorkflowJournalEntry[],
  ) => {
    const superseded = abandonmentsOf(current.reservationId).find(
      (saved) => saved.transitionDigest === transition.transitionDigest,
    );
    if (
      current.pendingTransition !== null ||
      superseded === undefined ||
      superseded.retirement !== undefined
    )
      throw new Error("prover reservation adoption mismatch");
    const attempt = {
      transition: superseded.transition,
      workflowId: superseded.workflowId,
      intentSequence,
      actionId: superseded.actionId,
    };
    assertIntentEntry(entries, attempt, transition.transactionHash);
    deleteAbandonment(current.reservationId, transition.transitionDigest);
    deleteAttempt.run(current.reservationId, transition.transitionDigest);
    persistAttempt(current.reservationId, transition.transitionDigest, attempt);
  };

  const uncoveredSuperseded = (reservationId: string) => {
    const abandoned = supersededAttempts(reservationId);
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
      abandoned: supersededAttempts(current.reservationId),
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
    const attemptSeen = new Set<string>();
    const abandonments = auditedAbandonments();
    const reservationsOfRows = [
      ...(selectAttemptKeys.all() as Omit<AttemptRow, "canonical_json">[]).map(
        (row) => {
          const key = attemptKey(row);
          attemptSeen.add(key);
          return attemptMemo.summary(key, fetchAttempt(row));
        },
      ),
      ...abandonments.rows,
    ];
    if (reservationsOfRows.some((id) => !byReservation.has(id)))
      throw new Error("prover funding attempt has no reservation");
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
    attemptMemo.retain(attemptSeen, released);
    abandonments.retain(released);
    return Object.freeze({
      version: storeVersion(),
      records: Object.freeze(all),
      projection,
    });
  };

  /** The workflow journal `entries` a change implies commit with it, or
   * neither does. */
  const transaction = <T>(
    operation: () => T,
    entries: readonly FraudProofWorkflowJournalEntry[] = [],
  ): T => {
    database.exec("BEGIN IMMEDIATE");
    try {
      // Before the operation, so no memo of the old revision serves its
      // writes.
      fundingRevision.bump();
      const value = operation();
      const journaled = journal.appendWithin(entries);
      const audited = audit();
      database.exec("COMMIT");
      journaled.commit();
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

  const hasSignedHistory = (reservationId: string): boolean =>
    (selectSignedHistory.get(reservationId) as { present: unknown }).present ===
    1;

  transaction(rebuildLeases);

  const store: WatcherProverFundingReservationStore = Object.freeze({
    readAll: async () => auditRead(),
    commitsJournal: (store) => journal.owns(store),
    readReservedOutRefs: async ({ excludingReservationId }) => {
      auditRead();
      return [
        ...new Set(
          leaseProjection()
            .leases.filter(
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
    retireSupersededAttempt: async ({
      plan,
      expectedRevision,
      transactionHash,
      retirement,
      journal: entries,
    }) => {
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null || current.revision !== expectedRevision)
          throw new Error("Superseded attempt retirement revision changed");
        assertPlanMatchesRecord(plan, current);
        retireSupersededAttempt(
          plan.reservationId,
          transactionHash,
          retirement,
        );
        const next = nextRecord({ current });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, entries);
    },
    readSupersededAttempts: async ({ reservationId }) => {
      auditRead();
      return supersededAttempts(reservationId);
    },
    readRecordedSubmissions: async ({ reservationId }) => {
      auditRead();
      return recordedSubmissions(reservationId).map(
        ({ reservationId: _id, transitionDigest: _digest, ...saved }) => saved,
      );
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
      journal: entries,
    }) => {
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (current.revision !== expectedRevision || current.state !== "active")
          throw new Error("prover reservation reobservation mismatch");
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
          adoptSupersededAttempt(
            current,
            transition,
            adoption.intentSequence,
            entries,
          );
        const produced = new Set(
          transition.producedInputs.map(({ outRef }) => outRef),
        );
        for (const value of inputs) {
          if (candidates.get(value.outRef) !== value.role)
            throw new Error("reobserved funding input has no recorded lineage");
          if (!produced.has(value.outRef))
            assertLeaseAvailable(value.outRef, current.reservationId);
        }
        // Only canonical inputs returned by the authenticated wallet source
        // are selected again. Historical signed attempts stay recorded.
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
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, entries);
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
      readOne,
      writeRecord,
      rebuildLeases,
      recordedSubmissions,
      abandonmentsOf,
      hasSignedHistory,
      forgetWorkflow: journal.forgetWorkflow,
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
              current.pendingTransition !== null
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
      });
    },
    prepareTransition: async (transitionInput) => {
      const { plan, attempt, journal: entries } = transitionInput;
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      assertIntentEntry(entries, attempt, transitionInput.transactionHash);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (
          current.state !== "active" ||
          current.pendingTransition !== null ||
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
            attempt.actionId,
            transitionInput.signedTransactionCborHex,
          ) !== null
        )
          throw new Error(
            "prover transition repeats an action whose superseded attempt landed",
          );
        if (
          !excludesEveryUncoveredAttempt({
            signedTransactionCborHex: transitionInput.signedTransactionCborHex,
            attempt,
            uncovered: uncoveredSuperseded(current.reservationId),
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
            plan,
            activeInputs: current.activeInputs,
            input: transitionInput,
          }),
        );
        const { transitionDigest: _digest, ...signedTransition } = transition;
        persistAttempt(current.reservationId, transition.transitionDigest, {
          transition: signedTransition,
          workflowId: attempt.workflowId,
          intentSequence: attempt.intentSequence,
          actionId: attempt.actionId,
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
      }, entries);
    },
    confirmTransition: async (confirmation) => {
      const { plan, journal: entries } = confirmation;
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (
          current.state !== "active" ||
          current.revision !== confirmation.expectedRevision ||
          current.pendingTransition === null ||
          current.pendingTransition.transitionDigest !==
            confirmation.transitionDigest ||
          current.pendingTransition.transactionHash !==
            confirmation.transactionHash
        ) {
          throw new Error("prover reservation confirmation mismatch");
        }
        // A confirmed transaction's collateral lease ends here, so it never
        // blocks a later action of any reservation. A rollback that finds it
        // spent re-signs with fresh collateral; this reservation's next
        // action selects collateral again.
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
      }, entries);
    },
    abandonPendingTransition: async (abandonment) => {
      const { plan, journal: entries } = abandonment;
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        // Without a retirement the attempt is superseded, not retired: its
        // funding stays leased and the replacement must spend some of it.
        if (
          current.state !== "active" ||
          current.revision !== abandonment.expectedRevision ||
          current.pendingTransition === null ||
          current.pendingTransition.transitionDigest !==
            abandonment.transitionDigest
        )
          throw new Error("prover reservation abandonment mismatch");
        const { transitionDigest } = current.pendingTransition;
        const recorded = recordedSubmissions(current.reservationId).find(
          (saved) => saved.transitionDigest === transitionDigest,
        );
        if (recorded === undefined)
          throw new Error("prover reservation abandonment has no attempt");
        insertAbandonment(
          current.reservationId,
          transitionDigest,
          recorded,
          abandonment.retirement,
        );
        const next = nextRecord({ current, pendingTransition: null });
        deletePendingLineage.run(current.reservationId, transitionDigest);
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, entries);
    },
    releaseIdle: async ({ plan, expectedRevision }) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (current.revision !== expectedRevision)
          throw new Error("prover reservation idle release revision changed");
        // Releasing frees leases and is not actuation.
        if (
          current.state !== "active" ||
          current.activeInputs.length === 0 ||
          current.pendingTransition !== null
        )
          return current;
        // The workflow authority checks no signed attempt is in flight (confirmed
        // suffices, retirement is not awaited); this admits spent unsigned inputs.
        const next = nextRecord({ current, activeInputs: [] });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    markConflict: async (conflict) => {
      const { plan, journal: entries } = conflict;
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
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
      }, entries);
    },
    release: async (release) => {
      const { plan, journal: entries } = release;
      assertPlan(plan);
      assertJournalIdentity(plan, entries);
      const last = entries.at(-1);
      if (last !== undefined && last.event.kind !== "completed")
        throw new Error("prover reservation release lacks its completion");
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (
          current.state !== "active" ||
          current.revision !== release.expectedRevision ||
          current.pendingTransition !== null
        )
          throw new Error("prover reservation release mismatch");
        const next = nextRecord({
          current,
          state: "released",
          activeInputs: [],
          pendingTransition: null,
          conflictCode: null,
        });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      }, entries);
    },
  });

  return Object.freeze({
    schemaVersion: WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE,
    store,
    journal,
    close: () => database.close(),
  });
};
