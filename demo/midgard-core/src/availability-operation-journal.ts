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
  assertWorkflow(
    lease: AvailabilityOperationLease,
    deploymentIdentity: string,
    headerHash: string,
    action: string,
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
    CREATE TABLE IF NOT EXISTS availability_operation_workflows (
      actor TEXT PRIMARY KEY, deployment TEXT NOT NULL, header_hash TEXT NOT NULL
    );
    INSERT OR IGNORE INTO availability_journal_metadata VALUES ('schema', '1');
  `);
  if (
    db
      .prepare(
        "SELECT value FROM availability_journal_metadata WHERE key = 'schema'",
      )
      .get()?.value !== "1"
  ) {
    db.close();
    throw new Error("Unsupported availability operation journal schema");
  }
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
    const workflow = db
      .prepare("SELECT * FROM availability_operation_workflows WHERE actor = ?")
      .get(lease.scope);
    if (
      workflow &&
      (workflow.deployment !== deployment ||
        workflow.header_hash !== header ||
        action === "prepare")
    ) {
      throw new Error(
        "Availability wallet capital belongs to an unresolved challenge workflow",
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
        // Preparation has no live challenge yet. Opening claims all future
        // capital in this dedicated wallet until its challenge is terminal.
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
