import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingPreparedTransition,
} from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import { HEX_32 } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import {
  createVerifiedRowMemo,
  verifiedRowKey,
} from "./sqlite-prover-funding-reservation-store.verified-row-memo.js";
export type AbandonmentRow = Readonly<{
  reservation_id: unknown;
  transition_digest: unknown;
  record_digest: unknown;
  canonical_json: unknown;
  acknowledged_revision: unknown;
}>;
export const createProverFundingAbandonmentRecords = (
  database: DatabaseSync,
) => {
  const insertAbandonment = database.prepare(`
    INSERT INTO watcher_prover_funding_abandonment_v1(reservation_id, transition_digest, record_digest, canonical_json)
    VALUES (?, ?, ?, ?)
  `);
  const selectUnacknowledgedAbandonment = database.prepare(`
    SELECT * FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ? AND acknowledged_revision IS NULL
  `);
  const selectAbandonmentsOf = database.prepare(
    `SELECT * FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ? ORDER BY transition_digest`,
  );
  const selectAllUnacknowledged = database.prepare(
    `SELECT * FROM watcher_prover_funding_abandonment_v1 WHERE acknowledged_revision IS NULL ORDER BY reservation_id`,
  );
  const selectAbandonmentKeys = database.prepare(
    `SELECT reservation_id, transition_digest, record_digest, acknowledged_revision FROM watcher_prover_funding_abandonment_v1 ORDER BY reservation_id, transition_digest`,
  );
  const selectAbandonment = database.prepare(
    `SELECT * FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ? AND transition_digest = ?`,
  );
  const acknowledgeAbandonment = database.prepare(`
    UPDATE watcher_prover_funding_abandonment_v1 SET acknowledged_revision = ?
    WHERE reservation_id = ? AND transition_digest = ? AND acknowledged_revision IS NULL
  `);
  const parseAbandonmentRow = (row: AbandonmentRow) => {
    if (
      typeof row.reservation_id !== "string" ||
      !HEX_32.test(row.reservation_id) ||
      typeof row.transition_digest !== "string" ||
      !HEX_32.test(row.transition_digest) ||
      typeof row.record_digest !== "string" ||
      !HEX_32.test(row.record_digest) ||
      typeof row.canonical_json !== "string" ||
      (row.acknowledged_revision !== null &&
        (typeof row.acknowledged_revision !== "string" ||
          !/^(?:0|[1-9][0-9]*)$/u.test(row.acknowledged_revision)))
    )
      throw new Error("prover funding abandonment row is malformed");
    const value: unknown = JSON.parse(row.canonical_json);
    if (
      value === null ||
      typeof value !== "object" ||
      Array.isArray(value) ||
      Object.keys(value).sort().join(",") !== "handoff,transition" ||
      !("handoff" in value) ||
      !("transition" in value) ||
      watcherCanonicalJson(value) !== row.canonical_json ||
      computeDeploymentManifestJsonDigest(value) !== row.record_digest
    )
      throw new Error("prover funding abandonment row digest mismatch");
    const transition = parseWorkflowFundingPreparedTransition(value.transition);
    const handoff = parseWorkflowFundingAbandonmentHandoff(value.handoff);
    if (
      computeDeploymentManifestJsonDigest(transition) !==
        row.transition_digest ||
      handoff.submissionIntent.txHash !== transition.transactionHash ||
      handoff.reconciliation.txHash !== transition.transactionHash
    )
      throw new Error(
        "prover funding abandonment changed signed transaction identity",
      );
    return {
      reservationId: row.reservation_id,
      transitionDigest: row.transition_digest,
      transition,
      handoff,
    };
  };
  const memo = createVerifiedRowMemo(
    (saved: ReturnType<typeof parseAbandonmentRow>) => ({
      identity: saved.handoff.identity,
      retired: saved.handoff.reconciliation.retirement !== undefined,
    }),
  );
  const acknowledgedRevisionOf = (row: AbandonmentRow) => {
    if (
      row.acknowledged_revision !== null &&
      (typeof row.acknowledged_revision !== "string" ||
        !/^(?:0|[1-9][0-9]*)$/u.test(row.acknowledged_revision))
    )
      throw new Error("prover funding abandonment row is malformed");
    return row.acknowledged_revision;
  };
  const abandonmentKey = (row: AbandonmentRow) =>
    verifiedRowKey(
      row.reservation_id,
      row.transition_digest,
      row.record_digest,
    );
  /** The acknowledgement column lies outside the row digest, so it is read
   * from the row each time; the rest is verified once. */
  const readAbandonmentRow = (row: AbandonmentRow) => ({
    ...memo.read(abandonmentKey(row), () => parseAbandonmentRow(row)),
    acknowledgedRevision: acknowledgedRevisionOf(row),
  });
  const abandonmentsOf = (reservationId: string) =>
    (selectAbandonmentsOf.all(reservationId) as AbandonmentRow[]).map(
      readAbandonmentRow,
    );
  /** What the audit checks of every retained abandonment, re-verifying only
   * rows it has not seen. */
  const auditedAbandonments = () => {
    const seen = new Set<string>();
    const rows = (
      selectAbandonmentKeys.all() as Omit<AbandonmentRow, "canonical_json">[]
    ).map((row) => {
      const key = abandonmentKey({ ...row, canonical_json: null });
      seen.add(key);
      const summary = memo.summary(key, () => {
        const full = selectAbandonment.get(
          row.reservation_id as string,
          row.transition_digest as string,
        ) as AbandonmentRow | undefined;
        if (full === undefined || full.record_digest !== row.record_digest)
          throw new Error(
            "prover funding abandonment row changed during audit",
          );
        return parseAbandonmentRow(full);
      });
      return {
        reservationId: row.reservation_id as string,
        acknowledgedRevision: acknowledgedRevisionOf({
          ...row,
          canonical_json: null,
        }),
        ...summary,
      };
    });
    return {
      rows,
      retain: (released: ReadonlySet<string>) => memo.retain(seen, released),
    };
  };
  /** A superseded attempt's row awaits its journal acknowledgement before
   * anything else happens. A row without retirement under a newer pending
   * transition predates that rule and stays hidden; see
   * `acknowledgeOutpacedAbandonments` for when that transition lands. */
  const unacknowledgedAbandonment = (record: {
    readonly reservationId: string;
    readonly pendingTransition: unknown;
  }) => {
    const row = selectUnacknowledgedAbandonment.get(record.reservationId) as
      | AbandonmentRow
      | undefined;
    const saved = row === undefined ? null : readAbandonmentRow(row);
    return saved?.handoff.reconciliation.retirement === undefined &&
      record.pendingTransition !== null
      ? null
      : saved;
  };
  /**
   * A row without retirement that predates the acknowledgement rule can be
   * outpaced: another attempt of its workflow recorded a submission handoff
   * at or after its journal sequence. That journal has moved past it and can
   * never take its acknowledgement, so the row counts as acknowledged at its
   * reservation's current revision. It stays a superseded attempt for
   * exclusion and retirement. The current rules record no submission while a
   * row is unacknowledged, so only such legacy rows match.
   */
  const acknowledgeOutpacedAbandonments = (
    submissions: (reservationId: string) => readonly Readonly<{
      transition: Readonly<{ transactionHash: string }>;
      handoff: Readonly<{
        workflowId: string;
        expectedJournalSequence: number;
      }>;
    }>[],
    readRecord: (reservationId: string) => { readonly revision: string } | null,
  ) => {
    for (const saved of (selectAllUnacknowledged.all() as AbandonmentRow[]).map(
      readAbandonmentRow,
    )) {
      if (
        saved.acknowledgedRevision !== null ||
        saved.handoff.reconciliation.retirement !== undefined ||
        !submissions(saved.reservationId).some(
          ({ transition, handoff }) =>
            transition.transactionHash !== saved.transition.transactionHash &&
            handoff.workflowId === saved.handoff.workflowId &&
            handoff.expectedJournalSequence >=
              saved.handoff.expectedJournalSequence,
        )
      )
        continue;
      const record = readRecord(saved.reservationId);
      if (record === null)
        throw new Error("prover funding abandonment has no reservation");
      acknowledgeAbandonment.run(
        record.revision,
        saved.reservationId,
        saved.transitionDigest,
      );
    }
  };
  const legacyAbandonedTransactions = (reservationId: string) =>
    abandonmentsOf(reservationId).map((saved) => ({
      transition: saved.transition,
      handoff: saved.handoff,
    }));
  const retireLegacyAbandonment = (
    reservationId: string,
    transactionHash: string,
    retirement: import("@al-ft/midgard-fault-proofs").WorkflowFundingAbandonmentHandoff["reconciliation"]["retirement"],
    acknowledgedRevision: string,
  ) => {
    const saved = abandonmentsOf(reservationId).find(
      (row) => row.transition.transactionHash === transactionHash,
    );
    if (saved === undefined)
      throw new Error("Legacy retirement has no exact funding attempt");
    if (saved.handoff.reconciliation.retirement !== undefined) {
      if (
        computeDeploymentManifestJsonDigest(
          saved.handoff.reconciliation.retirement,
        ) !== computeDeploymentManifestJsonDigest(retirement)
      )
        throw new Error("Verified retirement receipt is immutable");
      return false;
    }
    const handoff = parseWorkflowFundingAbandonmentHandoff({
      ...saved.handoff,
      reconciliation: { ...saved.handoff.reconciliation, retirement },
    });
    if (handoff.reconciliation.retirement === undefined)
      throw new Error("Legacy retirement requires a canonical certificate");
    const value = { transition: saved.transition, handoff };
    database
      .prepare(
        "UPDATE watcher_prover_funding_abandonment_v1 SET record_digest = ?, canonical_json = ?, acknowledged_revision = COALESCE(acknowledged_revision, ?) WHERE reservation_id = ? AND transition_digest = ?",
      )
      .run(
        computeDeploymentManifestJsonDigest(value),
        watcherCanonicalJson(value),
        acknowledgedRevision,
        reservationId,
        saved.transitionDigest,
      );
    return true;
  };
  return {
    retireLegacyAbandonment,
    insertAbandonment,
    abandonmentsOf,
    auditedAbandonments,
    acknowledgeAbandonment,
    unacknowledgedAbandonment,
    acknowledgeOutpacedAbandonments,
    legacyAbandonedTransactions,
  };
};
