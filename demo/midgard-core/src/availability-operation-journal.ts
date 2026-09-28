import { mkdirSync } from "node:fs";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

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
}>;

export interface AvailabilityOperationJournal {
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
  finalizedAnchors(
    deploymentIdentity: string,
    actor: string,
  ): readonly AvailabilityOperationRecord[];
  /** All unresolved wallet resources, including intents for other deployments. */
  reservedOutRefs(actor: string): readonly string[];
  /**
   * Refuses every step while the actor has a live challenge workflow in a
   * different deployment, and a 'prepare' for a header whose own Open landed.
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
   * Deletes the lease actor's workflow row for (deployment, header) once its
   * challenge ended in someone else's terminal step (P20). Refuses unless the
   * actor has no pending, included or conflicting intent for that header and
   * `evidence.openIntentId` is its confirmed Open for it. Only the row goes:
   * reservations, intents and leases are untouched.
   */
  releaseWorkflow(
    lease: AvailabilityOperationLease,
    deploymentIdentity: string,
    headerHash: string,
    evidence: AvailabilityWorkflowReleaseEvidence,
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
  halt(reason: string): void;
  assertRunning(): void;
  close(): void;
}

/**
 * One live challenge workflow per (actor, deployment, header). Schema 2; a
 * schema-1 journal (one workflow per actor) is migrated in place on open.
 */
const WORKFLOW_COLUMNS = `actor TEXT NOT NULL, deployment TEXT NOT NULL,
  header_hash TEXT NOT NULL, PRIMARY KEY(actor, deployment, header_hash)`;

/** One durable database must be shared by every process using an actor wallet. */
export const openAvailabilityOperationJournal = (
  path: string,
): AvailabilityOperationJournal => {
  if (!isAbsolute(path) || normalize(path) !== path) {
    throw new Error(
      "Availability operation journal requires a canonical absolute path",
    );
  }
  mkdirSync(dirname(path), { recursive: true, mode: 0o700 });
  const db = new DatabaseSync(path);
  db.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA busy_timeout = 5000;
    CREATE TABLE IF NOT EXISTS availability_journal_metadata (
      key TEXT PRIMARY KEY, value TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS availability_operation_leases (
      scope TEXT PRIMARY KEY, owner TEXT NOT NULL, generation INTEGER NOT NULL,
      expires_at INTEGER NOT NULL
    );
    CREATE TABLE IF NOT EXISTS availability_operation_intents (
      id TEXT PRIMARY KEY, deployment TEXT NOT NULL, actor TEXT NOT NULL,
      record TEXT NOT NULL, state TEXT NOT NULL, tx_hash TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS availability_operation_resources (
      resource TEXT NOT NULL, intent_id TEXT NOT NULL, kind TEXT NOT NULL,
      actor TEXT NOT NULL, PRIMARY KEY(resource, intent_id)
    );
    CREATE TABLE IF NOT EXISTS availability_operation_dependencies (
      parent_tx_hash TEXT NOT NULL, child_id TEXT NOT NULL,
      PRIMARY KEY(parent_tx_hash, child_id)
    );
  `);
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
  try {
    transaction(() => {
      const schema = db
        .prepare(
          "SELECT value FROM availability_journal_metadata WHERE key = 'schema'",
        )
        .get()?.value;
      if (schema !== undefined && schema !== "1" && schema !== "2")
        throw new Error("Unsupported availability operation journal schema");
      if (schema === "1") {
        // Schema 1 keyed workflows on the actor alone. Its rows map 1:1 onto
        // the per-header key, so the migration keeps every live workflow.
        db.exec(`
          CREATE TABLE availability_operation_workflows_v2 (${WORKFLOW_COLUMNS});
          INSERT INTO availability_operation_workflows_v2 (actor, deployment, header_hash)
            SELECT actor, deployment, header_hash FROM availability_operation_workflows;
          DROP TABLE availability_operation_workflows;
          ALTER TABLE availability_operation_workflows_v2 RENAME TO availability_operation_workflows;
          UPDATE availability_journal_metadata SET value = '2' WHERE key = 'schema';
        `);
        return;
      }
      db.exec(`
        CREATE TABLE IF NOT EXISTS availability_operation_workflows (${WORKFLOW_COLUMNS});
        INSERT OR IGNORE INTO availability_journal_metadata VALUES ('schema', '2');
      `);
    });
  } catch (error) {
    db.close();
    throw error;
  }
  const assertRunning = (): void => {
    const halt = db
      .prepare(
        "SELECT value FROM availability_journal_metadata WHERE key = 'halt'",
      )
      .get();
    if (halt)
      throw new Error(
        `Availability operation journal halted: ${String(halt.value)}`,
      );
  };
  const assertLease = (
    lease: AvailabilityOperationLease,
    nowMs: number,
  ): void => {
    assertRunning();
    const row = db
      .prepare("SELECT * FROM availability_operation_leases WHERE scope = ?")
      .get(lease.scope);
    if (
      !row ||
      row.owner !== lease.owner ||
      row.generation !== lease.generation ||
      Number(row.expires_at) <= nowMs
    ) {
      throw new Error(
        "Availability operation mutation lease expired or superseded",
      );
    }
  };
  const get = (id: string): AvailabilityOperationRecord | null => {
    const row = db
      .prepare("SELECT record FROM availability_operation_intents WHERE id = ?")
      .get(id);
    return row
      ? (JSON.parse(String(row.record)) as AvailabilityOperationRecord)
      : null;
  };
  const assertWorkflow = (
    lease: AvailabilityOperationLease,
    deployment: string,
    header: string,
    action: string,
    nowMs: number,
  ): void => {
    assertLease(lease, nowMs);
    const live = db
      .prepare(
        "SELECT deployment, header_hash FROM availability_operation_workflows WHERE actor = ?",
      )
      .all(lease.scope);
    // The wallet's removal reserve is computed from one deployment's queue, so
    // a second deployment would count the same balance twice.
    const blocking = live.find(
      (workflow) => workflow.deployment !== deployment,
    );
    if (blocking !== undefined) {
      throw new Error(
        `Availability wallet capital belongs to an unresolved challenge workflow in another deployment (deployment ${String(blocking.deployment)}, header ${String(blocking.header_hash)})`,
      );
    }
    // Within one deployment every header's step is admitted. A header whose
    // Open landed needs no new challenger coin.
    if (
      action === "prepare" &&
      live.some((workflow) => workflow.header_hash === header)
    ) {
      throw new Error(
        "Availability header already has a live challenge workflow",
      );
    }
  };
  const resources = (intent: AvailabilityOperationIntent) => [
    ...intent.spentOutRefs.map((outRef) => ({
      resource: outRef,
      kind: "spend",
    })),
    ...intent.collateralOutRefs.map((outRef) => ({
      resource: outRef,
      kind: "collateral",
    })),
  ];
  return {
    acquire(scope, owner, nowMs, durationMs) {
      if (
        !scope ||
        !owner ||
        !Number.isSafeInteger(nowMs) ||
        !Number.isSafeInteger(durationMs) ||
        durationMs <= 0
      ) {
        throw new Error("Invalid availability operation lease");
      }
      return transaction(() => {
        assertRunning();
        const row = db
          .prepare(
            "SELECT * FROM availability_operation_leases WHERE scope = ?",
          )
          .get(scope);
        if (row && Number(row.expires_at) > nowMs) {
          throw new Error("Availability operation actor is already leased");
        }
        const generation = row ? Number(row.generation) + 1 : 1;
        db.prepare(
          `INSERT INTO availability_operation_leases VALUES (?, ?, ?, ?)
          ON CONFLICT(scope) DO UPDATE SET owner=excluded.owner, generation=excluded.generation, expires_at=excluded.expires_at`,
        ).run(scope, owner, generation, nowMs + durationMs);
        return { scope, owner, generation };
      });
    },
    assertLease,
    reservedOutRefs(actor) {
      assertRunning();
      return db
        .prepare(
          "SELECT DISTINCT resource FROM availability_operation_resources WHERE actor = ? ORDER BY resource",
        )
        .all(actor)
        .map((row) => String(row.resource));
    },
    release(lease) {
      db.prepare(
        "UPDATE availability_operation_leases SET expires_at = 0 WHERE scope = ? AND owner = ? AND generation = ?",
      ).run(lease.scope, lease.owner, lease.generation);
    },
    pending(deployment, actor) {
      assertRunning();
      return db
        .prepare(
          "SELECT record FROM availability_operation_intents WHERE deployment = ? AND actor = ? AND state IN ('pending', 'conflict') ORDER BY id",
        )
        .all(deployment, actor)
        .map(
          (row) =>
            JSON.parse(String(row.record)) as AvailabilityOperationRecord,
        );
    },
    unfinalized(deployment, actor) {
      assertRunning();
      return db
        .prepare(
          "SELECT record FROM availability_operation_intents WHERE deployment = ? AND actor = ? AND state = 'included' ORDER BY id",
        )
        .all(deployment, actor)
        .map(
          (row) =>
            JSON.parse(String(row.record)) as AvailabilityOperationRecord,
        );
    },
    finalizedAnchors(deployment, actor) {
      assertRunning();
      return db
        .prepare(
          `SELECT parent.record FROM availability_operation_intents AS parent
        WHERE parent.deployment = ? AND parent.actor = ? AND parent.state = 'confirmed'
        AND NOT EXISTS (
          SELECT 1 FROM availability_operation_dependencies AS edge
          JOIN availability_operation_intents AS child ON child.id = edge.child_id
          WHERE edge.parent_tx_hash = parent.tx_hash AND child.state = 'confirmed'
          AND child.deployment = parent.deployment AND child.actor = parent.actor
        ) ORDER BY parent.id`,
        )
        .all(deployment, actor)
        .map(
          (row) =>
            JSON.parse(String(row.record)) as AvailabilityOperationRecord,
        );
    },
    assertWorkflow,
    workflows(actor) {
      assertRunning();
      const opens = db
        .prepare(
          "SELECT record FROM availability_operation_intents WHERE actor = ? AND state = 'confirmed' ORDER BY rowid",
        )
        .all(actor)
        .map(
          (row) =>
            JSON.parse(String(row.record)) as AvailabilityOperationRecord,
        )
        .filter((record) => record.intent.action === "open");
      return db
        .prepare(
          "SELECT deployment, header_hash FROM availability_operation_workflows WHERE actor = ? ORDER BY deployment, header_hash",
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
    },
    releaseWorkflow(lease, deployment, header, evidence, nowMs) {
      transaction(() => {
        assertLease(lease, nowMs);
        const actor = lease.scope;
        const unresolved = db
          .prepare(
            "SELECT record FROM availability_operation_intents WHERE deployment = ? AND actor = ? AND state IN ('pending', 'included', 'conflict')",
          )
          .all(deployment, actor)
          .map(
            (row) =>
              JSON.parse(String(row.record)) as AvailabilityOperationRecord,
          )
          .filter(({ intent }) => intent.headerHash === header);
        if (unresolved.length > 0)
          throw new Error(
            "Availability workflow release requires no unresolved intent for the header",
          );
        const open = get(evidence.openIntentId);
        if (
          open === null ||
          open.state !== "confirmed" ||
          open.intent.action !== "open" ||
          open.intent.actor !== actor ||
          open.intent.deploymentIdentity !== deployment ||
          open.intent.headerHash !== header
        )
          throw new Error(
            "Availability workflow release requires the actor's confirmed Open for the header",
          );
        const deleted = db
          .prepare(
            "DELETE FROM availability_operation_workflows WHERE actor = ? AND deployment = ? AND header_hash = ?",
          )
          .run(actor, deployment, header);
        if (Number(deleted.changes) !== 1)
          throw new Error(
            "Availability workflow release names no live workflow",
          );
      });
    },
    get,
    findTransaction(txHash) {
      const row = db
        .prepare(
          "SELECT record FROM availability_operation_intents WHERE tx_hash = ? LIMIT 1",
        )
        .get(txHash);
      return row
        ? (JSON.parse(String(row.record)) as AvailabilityOperationRecord)
        : null;
    },
    persist(lease, intent, nowMs) {
      transaction(() => {
        assertLease(lease, nowMs);
        if (lease.scope !== intent.actor) {
          throw new Error(
            "Availability operation lease belongs to a different actor",
          );
        }
        assertWorkflow(
          lease,
          intent.deploymentIdentity,
          intent.headerHash,
          intent.action,
          nowMs,
        );
        const existing = get(intent.id);
        if (existing) {
          if (JSON.stringify(existing.intent) !== JSON.stringify(intent)) {
            throw new Error(
              "Availability operation identity already binds different signed bytes",
            );
          }
          return;
        }
        if (
          new Set(resources(intent).map(({ resource }) => resource)).size !==
          resources(intent).length
        ) {
          throw new Error(
            "Availability funding, protocol inputs and collateral overlap",
          );
        }
        for (const { resource, kind } of resources(intent)) {
          const reservations = db
            .prepare(
              "SELECT kind, actor FROM availability_operation_resources WHERE resource = ?",
            )
            .all(resource);
          if (
            reservations.some(
              (row) =>
                kind !== "collateral" ||
                row.kind !== "collateral" ||
                row.actor !== intent.actor,
            )
          ) {
            throw new Error(
              "Availability operation resource is already reserved",
            );
          }
          db.prepare(
            "INSERT INTO availability_operation_resources VALUES (?, ?, ?, ?)",
          ).run(resource, intent.id, kind, intent.actor);
        }
        const record: AvailabilityOperationRecord = {
          intent,
          state: "pending",
          inclusionPoint: null,
          detail: null,
        };
        db.prepare(
          "INSERT INTO availability_operation_intents VALUES (?, ?, ?, ?, ?, ?)",
        ).run(
          intent.id,
          intent.deploymentIdentity,
          intent.actor,
          JSON.stringify(record),
          record.state,
          intent.txHash,
        );
        for (const parent of new Set(
          intent.spentOutRefs.map((ref) => ref.split("#")[0]!),
        )) {
          db.prepare(
            "INSERT INTO availability_operation_dependencies VALUES (?, ?)",
          ).run(parent, intent.id);
        }
        // Preparation has no live challenge yet. Opening starts this header's
        // workflow; it ends when the header's terminal step confirms or its
        // Open expires. Other headers in the same deployment keep their own.
        if (intent.action === "open") {
          db.prepare(
            "INSERT OR IGNORE INTO availability_operation_workflows VALUES (?, ?, ?)",
          ).run(intent.actor, intent.deploymentIdentity, intent.headerHash);
        }
      });
    },
    transition(lease, id, state, inclusionPoint, detail, nowMs) {
      transaction(() => {
        assertLease(lease, nowMs);
        const record = get(id);
        if (record && lease.scope !== record.intent.actor) {
          throw new Error(
            "Availability operation transition belongs to a different actor",
          );
        }
        if (
          !record ||
          record.state === "confirmed" ||
          record.state === "expired"
        ) {
          throw new Error(
            "Availability operation transition requires an unresolved intent",
          );
        }
        if ((state === "included" || state === "confirmed") && !inclusionPoint)
          throw new Error("Confirmation requires canonical inclusion");
        const next: AvailabilityOperationRecord = {
          intent: record.intent,
          state,
          inclusionPoint,
          detail,
        };
        db.prepare(
          "UPDATE availability_operation_intents SET state = ?, record = ? WHERE id = ?",
        ).run(state, JSON.stringify(next), id);
        if (state === "confirmed" || state === "expired") {
          db.prepare(
            "DELETE FROM availability_operation_resources WHERE intent_id = ?",
          ).run(id);
        }
        if (
          (state === "confirmed" && record.intent.completesWorkflow) ||
          (state === "expired" && record.intent.action === "open")
        ) {
          db.prepare(
            "DELETE FROM availability_operation_workflows WHERE actor = ? AND deployment = ? AND header_hash = ?",
          ).run(
            record.intent.actor,
            record.intent.deploymentIdentity,
            record.intent.headerHash,
          );
        }
      });
    },
    halt(reason) {
      db.prepare(
        "INSERT INTO availability_journal_metadata VALUES ('halt', ?) ON CONFLICT(key) DO UPDATE SET value=excluded.value",
      ).run(reason);
    },
    assertRunning,
    close: () => db.close(),
  };
};
