import { createHash } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import type {
  AvailabilityOperationActorSnapshot,
  AvailabilityOperationIntent,
  AvailabilityOperationRecord,
  AvailabilityOperationRetirement,
  AvailabilityOperationUnsettledRelease,
} from "./availability-operation-journal.types.js";

/** Runs `run` inside one immediate SQLite transaction on `db`. */
export const journalTransaction = <T>(db: DatabaseSync, run: () => T): T => {
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

/** Shared SQL operations; callers hold the journal's actor lease and transaction. */
export const availabilityJournalStorage = (db: DatabaseSync) => {
  const actorSnapshot = (
    actor: string,
    deploymentIdentity: string,
  ): AvailabilityOperationActorSnapshot => {
    if (!actor || !deploymentIdentity)
      throw new Error("Actor metadata identity is unavailable");
    const row = db
      .prepare(
        `
      SELECT
        (SELECT owner FROM availability_operation_leases WHERE scope = ?) AS owner,
        (SELECT generation FROM availability_operation_leases WHERE scope = ?) AS generation,
        (SELECT expires_at FROM availability_operation_leases WHERE scope = ?) AS expires_at,
        (SELECT COUNT(*) FROM availability_operation_intents) AS retained_count,
        (SELECT COUNT(*) FROM availability_operation_intents WHERE actor = ? AND state IN ('pending','conflict')) AS pending_count,
        (SELECT COUNT(*) FROM availability_operation_resources WHERE actor = ?) AS resource_count,
        (SELECT COUNT(*) FROM availability_operation_resources AS r LEFT JOIN availability_operation_intents AS i ON i.id = r.intent_id WHERE r.actor = ? AND (i.id IS NULL OR i.deployment <> ? OR i.state NOT IN ('included','confirmed'))) AS incompatible_count,
        (SELECT COUNT(*) FROM availability_operation_workflows WHERE actor = ? AND deployment <> ? AND retired_by IS NULL) AS foreign_count,
        (SELECT COUNT(*) FROM availability_operation_workflows AS w LEFT JOIN availability_operation_intents AS i ON i.id = w.retired_by WHERE w.actor = ? AND w.deployment <> ? AND (w.retired_by IS NULL OR w.release IS NOT NULL OR json_extract(i.record, '$.intent.completesWorkflow') = 1)) AS protected_foreign_count,
        (SELECT COUNT(*) FROM availability_operation_workflows AS w JOIN availability_operation_intents AS i ON i.id = w.retired_by WHERE w.actor = ? AND w.release IS NOT NULL) AS release_count,
        (SELECT json_group_array(json_array(id, json_extract(record, '$.intent.headerHash'), json_extract(record, '$.intent.action'), tx_hash, state, json_extract(record, '$.intent.validUntilSlot'))) FROM (SELECT * FROM availability_operation_intents WHERE actor = ? AND deployment = ? ORDER BY id)) AS retained_attempts,
        (SELECT json_group_array(json_array(scope, owner, generation, expires_at)) FROM (SELECT * FROM availability_operation_leases ORDER BY scope)) AS lease_state,
        (SELECT json_group_array(json_array(id, deployment, actor, state, tx_hash, json_extract(record, '$.inclusionPoint'), json_extract(record, '$.retentionBlockNo'), json_extract(record, '$.intent.headerHash'), json_extract(record, '$.intent.action'), json_extract(record, '$.intent.validUntilSlot'))) FROM (SELECT * FROM availability_operation_intents ORDER BY id)) AS intent_state,
        (SELECT json_group_array(json_array(resource, intent_id, kind, actor)) FROM (SELECT * FROM availability_operation_resources ORDER BY resource, intent_id)) AS resource_state,
        (SELECT json_group_array(json_array(actor, deployment, header_hash, retired_by, release)) FROM (SELECT * FROM availability_operation_workflows ORDER BY actor, deployment, header_hash)) AS workflow_state
    `,
      )
      .get(
        actor,
        actor,
        actor,
        actor,
        actor,
        actor,
        deploymentIdentity,
        actor,
        deploymentIdentity,
        actor,
        deploymentIdentity,
        actor,
        actor,
        deploymentIdentity,
      );
    if (!row) throw new Error("Actor metadata snapshot is unavailable");
    const scalar = (value: unknown): number => {
      const count = Number(value);
      if (!Number.isSafeInteger(count) || count < 0)
        throw new Error("Actor metadata count is out of range");
      return count;
    };
    let lease: AvailabilityOperationActorSnapshot["lease"];
    if (row.owner !== null) {
      if (
        typeof row.owner !== "string" ||
        !row.owner ||
        row.generation === null ||
        row.expires_at === null
      )
        throw new Error("Actor lease metadata is malformed");
      const generation = scalar(row.generation);
      if (generation <= 0)
        throw new Error("Actor lease generation is malformed");
      lease = {
        owner: row.owner,
        generation,
        expiresAtMs: scalar(row.expires_at),
      };
    } else if (row.generation !== null || row.expires_at !== null)
      throw new Error("Actor lease metadata is incoherent");
    const stateDigest = createHash("sha256")
      .update(
        JSON.stringify([
          actor,
          deploymentIdentity,
          row.lease_state,
          row.intent_state,
          row.resource_state,
          row.workflow_state,
        ]),
      )
      .digest("hex");
    const attempts: unknown = JSON.parse(String(row.retained_attempts));
    if (!Array.isArray(attempts))
      throw new Error("Retained attempt metadata is malformed");
    const retainedAttempts = attempts.map(
      (
        attempt,
      ): AvailabilityOperationActorSnapshot["retainedAttempts"][number] => {
        if (!Array.isArray(attempt) || attempt.length !== 6)
          throw new Error("Retained attempt metadata is malformed");
        const values: unknown[] = attempt;
        const [id, headerHash, action, txHash, state, validUntilSlot] = values;
        if (
          typeof id !== "string" ||
          !id ||
          typeof headerHash !== "string" ||
          !headerHash ||
          typeof action !== "string" ||
          !action ||
          typeof txHash !== "string" ||
          !txHash ||
          (state !== "pending" &&
            state !== "included" &&
            state !== "confirmed" &&
            state !== "expired" &&
            state !== "conflict") ||
          typeof validUntilSlot !== "number"
        )
          throw new Error("Retained attempt metadata is malformed");
        return {
          id,
          headerHash,
          action,
          txHash,
          state,
          validUntilSlot: scalar(validUntilSlot),
        };
      },
    );
    return {
      actor,
      deploymentIdentity,
      stateDigest,
      ...(lease === undefined ? {} : { lease }),
      retainedRecordCount: scalar(row.retained_count),
      retainedAttempts,
      pendingIntentCount: scalar(row.pending_count),
      reservedResourceCount: scalar(row.resource_count),
      foreignWorkflowCount: scalar(row.foreign_count),
      protectedForeignWorkflowCount: scalar(row.protected_foreign_count),
      incompatibleResourceCount: scalar(row.incompatible_count),
      unsettledReleaseCount: scalar(row.release_count),
    };
  };
  const transaction = <T>(run: () => T): T => journalTransaction(db, run);
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
  const retainedRecordCount = (): number => {
    const count = db
      .prepare("SELECT COUNT(*) AS count FROM availability_operation_intents")
      .get()?.count;
    if (typeof count !== "number" || !Number.isSafeInteger(count) || count < 0)
      throw new Error(
        "Availability journal retained record count is unavailable",
      );
    return count;
  };
  return {
    retainedRecordCount,
    actorSnapshot,
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
