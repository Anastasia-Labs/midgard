import type { DatabaseSync } from "node:sqlite";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationRecord,
  AvailabilityOperationRetirement,
  AvailabilityOperationUnsettledRelease,
} from "./availability-operation-journal.types.js";

/** Shared SQL operations; callers hold the journal's actor lease and transaction. */
export const availabilityJournalStorage = (db: DatabaseSync) => {
  const transaction = <T>(run: () => T): T => {
    db.exec("BEGIN IMMEDIATE");
    try {
      const result = run();
      db.exec("COMMIT");
      return result;
    } catch (error) {
      db.exec("ROLLBACK");
      throw error;
    }
  };
  const records = (sql: string, ...args: string[]) =>
    db
      .prepare(sql)
      .all(...args)
      .map(
        (row) => JSON.parse(String(row.record)) as AvailabilityOperationRecord,
      );
  const get = (id: string): AvailabilityOperationRecord | null =>
    records(
      "SELECT record FROM availability_operation_intents WHERE id = ?",
      id,
    )[0] ?? null;
  const write = (record: AvailabilityOperationRecord): void => {
    db.prepare(
      "UPDATE availability_operation_intents SET state = ?, record = ? WHERE id = ?",
    ).run(record.state, JSON.stringify(record), record.intent.id);
  };
  const stamp = (
    record: AvailabilityOperationRecord,
    blockNo: number | undefined,
    inclusionPoint = record.inclusionPoint,
    resetRetention = false,
  ): AvailabilityOperationRecord => {
    const changedPoint = inclusionPoint !== record.inclusionPoint;
    const next = {
      ...record,
      inclusionPoint,
      retentionBlockNo:
        changedPoint || resetRetention
          ? blockNo
          : (record.retentionBlockNo ?? blockNo),
    };
    write(next);
    return next;
  };
  const remove = (id: string): void => {
    for (const sql of [
      "DELETE FROM availability_operation_resources WHERE intent_id = ?",
      "DELETE FROM availability_operation_intents WHERE id = ?",
      "DELETE FROM availability_operation_dependencies WHERE child_id = ?",
      "DELETE FROM availability_operation_workflows WHERE retired_by = ?",
    ])
      db.prepare(sql).run(id);
  };
  const pruneExpired = (
    actor: string,
    blockNo: number,
    recoveryDepth: number,
  ): void => {
    for (const record of records(
      "SELECT record FROM availability_operation_intents WHERE actor = ? AND state = 'expired'",
      actor,
    )) {
      // Starting at the first authenticated post-expiry boundary is conservative
      // and also migrates journals that did not record retention heights.
      const retained = stamp(record, blockNo);
      if (blockNo - retained.retentionBlockNo! <= recoveryDepth) continue;
      const child = db
        .prepare(
          `SELECT 1 FROM availability_operation_dependencies AS d
        JOIN availability_operation_intents AS i ON i.id = d.child_id
        WHERE d.parent_tx_hash = ? AND i.state IN ('pending', 'included', 'conflict')`,
        )
        .get(record.intent.txHash);
      if (!child) remove(record.intent.id);
    }
  };
  const workflows = (actor: string) => {
    const opens = records(
      "SELECT record FROM availability_operation_intents WHERE actor = ? AND state = 'confirmed' ORDER BY rowid",
      actor,
    ).filter((record) => record.intent.action === "open");
    return db
      .prepare(
        "SELECT deployment, header_hash FROM availability_operation_workflows WHERE actor = ? AND retired_by IS NULL ORDER BY deployment, header_hash",
      )
      .all(actor)
      .map((row) => ({
        deploymentIdentity: String(row.deployment),
        headerHash: String(row.header_hash),
        confirmedOpens: opens.filter(
          ({ intent }) =>
            intent.deploymentIdentity === row.deployment &&
            intent.headerHash === row.header_hash,
        ),
      }));
  };
  const unsettledReleases = (actor: string) => {
    return db
      .prepare(
        "SELECT w.deployment, w.header_hash, w.release, i.record FROM availability_operation_workflows AS w JOIN availability_operation_intents AS i ON i.id = w.retired_by WHERE w.actor = ? AND w.release IS NOT NULL ORDER BY w.deployment, w.header_hash",
      )
      .all(actor)
      .map((row) => ({
        deploymentIdentity: String(row.deployment),
        headerHash: String(row.header_hash),
        open: JSON.parse(String(row.record)) as AvailabilityOperationRecord,
        release: JSON.parse(
          String(row.release),
        ) as AvailabilityOperationUnsettledRelease["release"],
      }));
  };
  const restoreWorkflow = (intent: AvailabilityOperationIntent): void => {
    db.prepare(
      `INSERT INTO availability_operation_workflows VALUES (?, ?, ?, NULL, NULL)
      ON CONFLICT(actor, deployment, header_hash) DO UPDATE SET retired_by = NULL, release = NULL`,
    ).run(intent.actor, intent.deploymentIdentity, intent.headerHash);
  };
  const assertRetirementEvidence = (
    evidence: AvailabilityOperationRetirement,
  ): void => {
    const { confirmationDepth, currentSlot, currentBlockNo, recoveryDepth } =
      evidence;
    if (
      !Number.isSafeInteger(confirmationDepth) ||
      !Number.isSafeInteger(recoveryDepth) ||
      recoveryDepth <= 0 ||
      (currentSlot !== undefined && !Number.isSafeInteger(currentSlot)) ||
      (currentBlockNo !== undefined &&
        (!Number.isSafeInteger(currentBlockNo) || currentBlockNo < 0))
    )
      throw new Error("Invalid availability operation retirement evidence");
  };
  return {
    transaction,
    records,
    get,
    write,
    stamp,
    remove,
    pruneExpired,
    workflows,
    unsettledReleases,
    restoreWorkflow,
    assertRetirementEvidence,
  };
};
