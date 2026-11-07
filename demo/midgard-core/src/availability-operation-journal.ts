import {
  type AvailabilityJournalOpenOptions,
  openAvailabilityJournalDatabase,
} from "#availability-operation-journal-schema";
import { availabilityJournalStorage } from "#availability-operation-journal-storage";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationJournal,
  AvailabilityOperationLease,
  AvailabilityOperationRecord,
} from "./availability-operation-journal.types.js";

// Native strip-types loads the schema and storage sources through the
// package's subpath imports; tsup bundles them for consumers.
export type * from "./availability-operation-journal.types.js";

/** One durable database must be shared by every process using an actor wallet. */
export const openAvailabilityOperationJournal = (
  path: string,
  options: AvailabilityJournalOpenOptions = {},
): AvailabilityOperationJournal => {
  const db = openAvailabilityJournalDatabase(path, options);
  const {
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
  } = availabilityJournalStorage(db);
  const assertLease = (
    lease: AvailabilityOperationLease,
    nowMs: number,
  ): void => {
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
        "SELECT deployment, header_hash, retired_by, release FROM availability_operation_workflows WHERE actor = ?",
      )
      .all(lease.scope);
    // The wallet's removal reserve is computed from one deployment's queue, so
    // a second deployment would count the same balance twice. Provisional
    // terminal progress can roll back, so its capital guard stays until finality.
    const blocking = live.find(
      (workflow) =>
        workflow.deployment !== deployment &&
        (workflow.retired_by === null ||
          workflow.release !== null ||
          get(String(workflow.retired_by))?.intent.completesWorkflow),
    );
    if (blocking !== undefined) {
      throw new Error(
        `Availability wallet capital belongs to an unresolved challenge workflow in another deployment (deployment ${String(blocking.deployment)}, header ${String(blocking.header_hash)})`,
      );
    }
    // One deployment admits every header; a landed Open needs no new challenger coin.
    if (
      action === "prepare" &&
      live.some(
        (workflow) =>
          workflow.header_hash === header && workflow.retired_by === null,
      )
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
  /** Reserves `resource` for `intent`; returns the other intents holding it. */
  const reserve = (
    intent: AvailabilityOperationIntent,
    resource: string,
    kind: string,
  ): string[] => {
    const others = db
      .prepare(
        "SELECT intent_id, kind, actor FROM availability_operation_resources WHERE resource = ? AND intent_id != ?",
      )
      .all(resource, intent.id)
      .filter(
        (row) =>
          kind !== "collateral" ||
          row.kind !== "collateral" ||
          row.actor !== intent.actor,
      )
      .map((row) => String(row.intent_id));
    db.prepare(
      "INSERT OR IGNORE INTO availability_operation_resources VALUES (?, ?, ?, ?)",
    ).run(resource, intent.id, kind, intent.actor);
    return others;
  };
  const confirmedOf = (
    lease: AvailabilityOperationLease,
    id: string,
    nowMs: number,
    step: string,
  ): AvailabilityOperationRecord => {
    assertLease(lease, nowMs);
    const record = get(id);
    if (record && record.intent.actor !== lease.scope)
      throw new Error(
        `Availability operation ${step} belongs to a different actor`,
      );
    if (record?.state !== "confirmed")
      throw new Error(
        `Availability operation ${step} requires a confirmed intent`,
      );
    return record;
  };
  return {
    retainedRecordCount,
    actorSnapshot,
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
      return records(
        "SELECT record FROM availability_operation_intents WHERE deployment = ? AND actor = ? AND state IN ('pending', 'conflict') ORDER BY id",
        deployment,
        actor,
      );
    },
    unfinalized(deployment, actor) {
      return records(
        "SELECT record FROM availability_operation_intents WHERE deployment = ? AND actor = ? AND state = 'included' ORDER BY id",
        deployment,
        actor,
      );
    },
    finalizedAnchors(actor) {
      return records(
        `SELECT parent.record FROM availability_operation_intents AS parent
        WHERE parent.actor = ? AND parent.state = 'confirmed'
        AND NOT EXISTS (
          SELECT 1 FROM availability_operation_dependencies AS edge
          JOIN availability_operation_intents AS child ON child.id = edge.child_id
          WHERE edge.parent_tx_hash = parent.tx_hash AND child.state = 'confirmed'
          AND child.deployment = parent.deployment AND child.actor = parent.actor
        ) ORDER BY parent.id`,
        actor,
      );
    },
    assertWorkflow,
    workflows,
    unsettledReleases,
    releaseWorkflow(lease, deployment, header, evidence, nowMs) {
      const { confirmationDepth, recoveryDepth } = evidence;
      if (
        !Number.isSafeInteger(confirmationDepth) ||
        !Number.isSafeInteger(recoveryDepth) ||
        recoveryDepth <= 0
      )
        throw new Error("Invalid availability workflow release evidence");
      transaction(() => {
        assertLease(lease, nowMs);
        const actor = lease.scope;
        const unresolved = records(
          "SELECT record FROM availability_operation_intents WHERE deployment = ? AND actor = ? AND state IN ('pending', 'included', 'conflict')",
          deployment,
          actor,
        ).filter(({ intent }) => intent.headerHash === header);
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
        // Retired by the Open; retained evidence makes a foreign release reversible.
        const { reason, txHash, spendPoint } = evidence;
        const retired = db
          .prepare(
            "UPDATE availability_operation_workflows SET retired_by = ?, release = ? WHERE actor = ? AND deployment = ? AND header_hash = ? AND (retired_by IS NULL OR (retired_by = ? AND release IS NOT NULL))",
          )
          .run(
            open.intent.id,
            confirmationDepth > recoveryDepth
              ? null
              : JSON.stringify({ reason, txHash, spendPoint }),
            actor,
            deployment,
            header,
            open.intent.id,
          );
        if (Number(retired.changes) !== 1)
          throw new Error(
            "Availability workflow release names no live workflow",
          );
      });
    },
    reviveWorkflow(lease, deployment, header, nowMs) {
      transaction(() => {
        assertLease(lease, nowMs);
        const revived = db
          .prepare(
            "UPDATE availability_operation_workflows SET retired_by = NULL, release = NULL WHERE actor = ? AND deployment = ? AND header_hash = ? AND release IS NOT NULL",
          )
          .run(lease.scope, deployment, header);
        if (Number(revived.changes) !== 1)
          throw new Error(
            "Availability workflow revival names no unsettled release",
          );
      });
    },
    get,
    findTransaction(txHash) {
      return (
        records(
          "SELECT record FROM availability_operation_intents WHERE tx_hash = ? LIMIT 1",
          txHash,
        )[0] ?? null
      );
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
        for (const { resource, kind } of resources(intent))
          if (reserve(intent, resource, kind).length > 0)
            throw new Error(
              "Availability operation resource is already reserved",
            );
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
        // An Open starts this header's workflow; preparation does not.
        if (intent.action === "open") restoreWorkflow(intent);
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
        write(next);
        // Expiry frees inputs whose bytes can never land. A confirmation can
        // still roll back, so it keeps them and only retires its workflow row.
        if (state === "expired")
          db.prepare(
            "DELETE FROM availability_operation_resources WHERE intent_id = ?",
          ).run(id);
        const { actor, deploymentIdentity, headerHash } = record.intent;
        if (state === "confirmed" && record.intent.completesWorkflow)
          db.prepare(
            "UPDATE availability_operation_workflows SET retired_by = ?, release = NULL WHERE actor = ? AND deployment = ? AND header_hash = ? AND (retired_by IS NULL OR release IS NOT NULL)",
          ).run(id, actor, deploymentIdentity, headerHash);
        // Re-opening cleared an earlier confirmed Open's retirement, so its row
        // stays live for the release walk to re-derive, and that Open stays.
        if (state === "expired" && record.intent.action === "open")
          db.prepare(
            `DELETE FROM availability_operation_workflows AS w WHERE actor = ? AND deployment = ? AND header_hash = ? AND NOT EXISTS (SELECT 1 FROM availability_operation_intents AS i WHERE i.actor = w.actor AND i.deployment = w.deployment AND i.state = 'confirmed'
            AND json_extract(i.record, '$.intent.action') = 'open' AND json_extract(i.record, '$.intent.headerHash') = w.header_hash)`,
          ).run(actor, deploymentIdentity, headerHash);
      });
    },
    rewind(lease, id, detail, nowMs) {
      return transaction(() => {
        const record = confirmedOf(lease, id, nowMs, "rewind");
        // Retirement past validity (or, before retention, confirmation) freed
        // these, so another intent may hold one since. Both claims stay and
        // the rewound intent records the double claim as a conflict.
        const claimed = resources(record.intent).flatMap(({ resource, kind }) =>
          reserve(record.intent, resource, kind).map(
            (other) => `${resource} (intent ${other})`,
          ),
        );
        const conflict =
          claimed.length === 0
            ? null
            : `${detail}; its inputs are also reserved by ${claimed.join(", ")}`;
        const next: AvailabilityOperationRecord = {
          intent: record.intent,
          state: conflict === null ? "pending" : "conflict",
          inclusionPoint: null,
          detail: conflict ?? detail,
        };
        write(next);
        // Restore even a workflow row deleted before schema 3.
        if (record.intent.action === "open" || record.intent.completesWorkflow)
          restoreWorkflow(record.intent);
        return conflict;
      });
    },
    retire(lease, id, evidence, nowMs) {
      const { confirmationDepth, currentSlot, currentBlockNo, recoveryDepth } =
        evidence;
      assertRetirementEvidence(evidence);
      transaction(() => {
        // Every confirmed ancestor is at least as deep as its descendant.
        const closure = [confirmedOf(lease, id, nowMs, "retirement")];
        for (let index = 0; index < closure.length; index++)
          for (const ref of closure[index]!.intent.spentOutRefs) {
            const [record] = records(
              "SELECT record FROM availability_operation_intents WHERE tx_hash = ? AND actor = ? AND deployment = ? AND state = 'confirmed'",
              ref.split("#")[0]!,
              lease.scope,
              closure[0]!.intent.deploymentIdentity,
            );
            if (
              record &&
              !closure.some((seen) => seen.intent.id === record.intent.id)
            )
              closure.push(record);
          }
        const reIncluded =
          evidence.inclusionPoint !== undefined &&
          evidence.inclusionPoint !== closure[0]!.inclusionPoint;
        for (const record of closure) {
          const retained = stamp(
            record,
            currentBlockNo,
            record.intent.id === id
              ? (evidence.inclusionPoint ?? record.inclusionPoint)
              : record.inclusionPoint,
            reIncluded,
          );
          const { intent } = retained;
          const depth =
            currentBlockNo === undefined ||
            retained.retentionBlockNo === undefined
              ? confirmationDepth
              : Math.max(
                  confirmationDepth,
                  currentBlockNo - retained.retentionBlockNo,
                );
          const live =
            intent.action === "open" &&
            db
              .prepare(
                "SELECT 1 FROM availability_operation_workflows WHERE actor = ? AND deployment = ? AND header_hash = ? AND (retired_by IS NULL OR release IS NOT NULL OR retired_by != ?)",
              )
              .get(
                intent.actor,
                intent.deploymentIdentity,
                intent.headerHash,
                intent.id,
              );
          const prune = depth > recoveryDepth && !live;
          if (prune || (currentSlot ?? -1) >= intent.validUntilSlot)
            db.prepare(
              "DELETE FROM availability_operation_resources WHERE intent_id = ?",
            ).run(intent.id);
          if (!prune) continue;
          remove(intent.id);
        }
      });
    },
    pruneExpired(lease, currentBlockNo, recoveryDepth, nowMs) {
      if (
        !Number.isSafeInteger(currentBlockNo) ||
        currentBlockNo < 0 ||
        !Number.isSafeInteger(recoveryDepth) ||
        recoveryDepth <= 0
      )
        throw new Error("Invalid availability expired-retention evidence");
      transaction(() => {
        assertLease(lease, nowMs);
        pruneExpired(lease.scope, currentBlockNo, recoveryDepth);
      });
    },
    close: () => db.close(),
  };
};
