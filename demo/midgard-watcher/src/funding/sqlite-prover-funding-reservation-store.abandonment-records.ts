import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseWorkflowFundingSupersededAttempt,
  type SignedWorkflowTransactionRetirement,
  type WorkflowFundingRecordedAttempt,
} from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  type AttemptRow,
  parseSupersededAttemptRow,
} from "./sqlite-prover-funding-reservation-store.row-parsers.js";
import {
  createVerifiedRowMemo,
  verifiedRowKey,
} from "./sqlite-prover-funding-reservation-store.verified-row-memo.js";

/** Superseded attempts of the prover funding store: each can still land
 * until it is retired, and a later attempt must exclude it. */
export const createProverFundingAbandonmentRecords = (
  database: DatabaseSync,
) => {
  const insertRow = database.prepare(`
    INSERT INTO watcher_prover_funding_superseded_attempt_v1(
      record_digest, canonical_json, reservation_id, transition_digest
    ) VALUES (?, ?, ?, ?)
  `);
  const updateRow = database.prepare(`
    UPDATE watcher_prover_funding_superseded_attempt_v1
    SET record_digest = ?, canonical_json = ?
    WHERE reservation_id = ? AND transition_digest = ?
  `);
  const deleteRow = database.prepare(`
    DELETE FROM watcher_prover_funding_superseded_attempt_v1
    WHERE reservation_id = ? AND transition_digest = ?
  `);
  const selectOf = database.prepare(
    `SELECT * FROM watcher_prover_funding_superseded_attempt_v1 WHERE reservation_id = ? ORDER BY transition_digest`,
  );
  const selectKeys = database.prepare(
    `SELECT reservation_id, transition_digest, record_digest FROM watcher_prover_funding_superseded_attempt_v1 ORDER BY reservation_id, transition_digest`,
  );
  const selectOne = database.prepare(
    `SELECT * FROM watcher_prover_funding_superseded_attempt_v1 WHERE reservation_id = ? AND transition_digest = ?`,
  );
  const memo = createVerifiedRowMemo(
    (saved: ReturnType<typeof parseSupersededAttemptRow>) =>
      saved.reservationId,
  );
  const key = (row: Omit<AttemptRow, "canonical_json">) =>
    verifiedRowKey(
      row.reservation_id,
      row.transition_digest,
      row.record_digest,
    );
  const abandonmentsOf = (reservationId: string) =>
    (selectOf.all(reservationId) as AttemptRow[]).map((row) =>
      memo.read(key(row), () => parseSupersededAttemptRow(row)),
    );
  /** Verifies every retained row, re-reading only rows it has not seen;
   * each row's reservation id. */
  const auditedAbandonments = () => {
    const seen = new Set<string>();
    const rows = (selectKeys.all() as Omit<AttemptRow, "canonical_json">[]).map(
      (row) => {
        const rowKey = key(row);
        seen.add(rowKey);
        return memo.summary(rowKey, () => {
          const full = selectOne.get(
            row.reservation_id as string,
            row.transition_digest as string,
          ) as AttemptRow | undefined;
          if (full === undefined || full.record_digest !== row.record_digest)
            throw new Error(
              "prover funding superseded attempt changed during audit",
            );
          return parseSupersededAttemptRow(full);
        });
      },
    );
    return {
      rows,
      retain: (released: ReadonlySet<string>) => memo.retain(seen, released),
    };
  };
  const write = (
    statement: typeof insertRow,
    reservationId: string,
    transitionDigest: string,
    value: unknown,
  ) =>
    statement.run(
      computeDeploymentManifestJsonDigest(value),
      watcherCanonicalJson(value),
      reservationId,
      transitionDigest,
    );
  /** Records `attempt` as superseded, retired when `retirement` is given. */
  const insertAbandonment = (
    reservationId: string,
    transitionDigest: string,
    attempt: WorkflowFundingRecordedAttempt,
    retirement: SignedWorkflowTransactionRetirement | undefined,
  ) =>
    write(
      insertRow,
      reservationId,
      transitionDigest,
      parseWorkflowFundingSupersededAttempt({
        transition: attempt.transition,
        workflowId: attempt.workflowId,
        intentSequence: attempt.intentSequence,
        actionId: attempt.actionId,
        ...(retirement === undefined ? {} : { retirement }),
      }),
    );
  const supersededAttempts = (reservationId: string) =>
    abandonmentsOf(reservationId).map(
      ({ reservationId: _id, transitionDigest: _digest, ...saved }) => saved,
    );
  /** Retires a superseded attempt with its exact retirement. */
  const retireSupersededAttempt = (
    reservationId: string,
    transactionHash: string,
    retirement: SignedWorkflowTransactionRetirement,
  ) => {
    const saved = abandonmentsOf(reservationId).find(
      (row) => row.transition.transactionHash === transactionHash,
    );
    if (saved === undefined)
      throw new Error(
        "Superseded attempt retirement has no exact funding attempt",
      );
    if (saved.retirement !== undefined)
      throw new Error("Superseded attempt is already retired");
    const { reservationId: _id, transitionDigest, ...attempt } = saved;
    write(
      updateRow,
      reservationId,
      transitionDigest,
      parseWorkflowFundingSupersededAttempt({ ...attempt, retirement }),
    );
  };
  /** Removes a superseded attempt that landed, so it is pending again. */
  const deleteAbandonment = (reservationId: string, transitionDigest: string) =>
    deleteRow.run(reservationId, transitionDigest);
  return {
    retireSupersededAttempt,
    insertAbandonment,
    deleteAbandonment,
    abandonmentsOf,
    auditedAbandonments,
    supersededAttempts,
  };
};
