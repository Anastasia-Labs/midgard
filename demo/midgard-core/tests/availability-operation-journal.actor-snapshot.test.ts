import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { describe, expect, it } from "vitest";

import { openAvailabilityOperationJournal } from "../src/availability-operation-journal.js";
import { openAvailabilityJournalInventory } from "../src/availability-operation-journal-inventory.js";

const fixture = () => {
  const directory = mkdtempSync(join(tmpdir(), "availability-actor-snapshot-"));
  const path = join(directory, "journal.sqlite");
  const journal = openAvailabilityOperationJournal(path);
  return { directory, path, journal };
};
describe("coherent read-only actor interference metadata", () => {
  it("retains exact aggregate response-attempt states across restart and binds same-count substitutions", () => {
    const f = fixture();
    let journal = f.journal;
    try {
      const lease = journal.acquire("actor", "builder", 0, 100);
      for (const [id, action, deployment, state] of [
        ["failed-1", "publish", "deployment", "expired"],
        ["failed-2", "publish", "deployment", "expired"],
        ["failed-3", "settle", "deployment", "expired"],
        ["failed-4", "close", "deployment", "expired"],
        ["foreign", "publish", "foreign", "expired"],
        ["pending", "publish", "deployment", "pending"],
      ] as const) {
        journal.persist(
          lease,
          {
            id,
            actor: "actor",
            deploymentIdentity: deployment,
            headerHash: "header",
            action,
            signedCbor: `immutable ${id}`,
            txHash: `${id}-transaction`,
            spentOutRefs: [`${id}-input#0`],
            collateralOutRefs: [],
            expectedOutRefs: [`${id}-transaction#0`],
            validUntilSlot: 100,
            completesWorkflow: false,
          },
          1,
        );
        if (state === "expired")
          journal.transition(
            lease,
            id,
            state,
            null,
            "positive expiry evidence",
            2,
          );
      }
      journal.release(lease);
      const before = journal.actorSnapshot("actor", "deployment");
      expect(before.retainedRecordCount).toBe(6);
      expect(before.retainedAttempts.map((attempt) => attempt.id)).toEqual([
        "failed-1",
        "failed-2",
        "failed-3",
        "failed-4",
        "pending",
      ]);
      expect(
        before.retainedAttempts.filter(
          (attempt) => attempt.state === "expired",
        ),
      ).toHaveLength(4);
      expect(
        before.retainedAttempts.find((attempt) => attempt.id === "pending"),
      ).toMatchObject({
        headerHash: "header",
        action: "publish",
        txHash: "pending-transaction",
        state: "pending",
        validUntilSlot: 100,
      });
      const exactBytes = journal.get("failed-1")?.intent;
      journal.close();
      journal = openAvailabilityOperationJournal(f.path);
      expect(
        journal.actorSnapshot("actor", "deployment").retainedAttempts,
      ).toEqual(before.retainedAttempts);
      expect(journal.get("failed-1")?.intent).toEqual(exactBytes);
      const db = new DatabaseSync(f.path);
      try {
        db.prepare(
          "UPDATE availability_operation_intents SET record = json_set(record, '$.intent.action', 'close', '$.intent.validUntilSlot', 101) WHERE id = ?",
        ).run("pending");
        const after = journal.actorSnapshot("actor", "deployment");
        expect(after.retainedRecordCount).toBe(before.retainedRecordCount);
        expect(
          after.retainedAttempts.find((attempt) => attempt.id === "pending"),
        ).toMatchObject({
          action: "close",
          validUntilSlot: 101,
          state: "pending",
        });
        expect(after.stateDigest).not.toBe(before.stateDigest);
      } finally {
        db.close();
      }
    } finally {
      journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it("observes an unsigned lease with no intent and preserves its exact ownership", () => {
    const f = fixture();
    try {
      expect(f.journal.actorSnapshot("actor", "deployment")).toMatchObject({
        retainedRecordCount: 0,
        pendingIntentCount: 0,
        reservedResourceCount: 0,
      });
      const lease = f.journal.acquire("actor", "builder", 100, 20);
      expect(f.journal.actorSnapshot("actor", "deployment")).toMatchObject({
        actor: "actor",
        deploymentIdentity: "deployment",
        lease: { owner: "builder", generation: 1, expiresAtMs: 120 },
        retainedRecordCount: 0,
        pendingIntentCount: 0,
        reservedResourceCount: 0,
        incompatibleResourceCount: 0,
        foreignWorkflowCount: 0,
        unsettledReleaseCount: 0,
      });
      expect(
        f.journal.actorSnapshot("actor", "deployment").stateDigest,
      ).toMatch(/^[0-9a-f]{64}$/u);
      expect(
        f.journal.actorSnapshot("foreign", "deployment").lease,
      ).toBeUndefined();
      f.journal.release(lease);
      expect(
        f.journal.actorSnapshot("actor", "deployment").lease?.expiresAtMs,
      ).toBe(0);
    } finally {
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it("keeps an expired crashed owner unresolved until existing audited takeover/release", () => {
    const f = fixture();
    let journal = f.journal;
    try {
      const old = journal.acquire("actor", "crashed-builder", 0, 10);
      journal.close();
      journal = openAvailabilityOperationJournal(f.path);
      expect(journal.actorSnapshot("actor", "deployment").lease).toEqual({
        owner: "crashed-builder",
        generation: 1,
        expiresAtMs: 10,
      });
      const current = journal.acquire("actor", "reconciler", 100, 10);
      journal.release(old);
      expect(journal.actorSnapshot("actor", "deployment").lease).toEqual({
        owner: "reconciler",
        generation: 2,
        expiresAtMs: 110,
      });
      journal.release(current);
      expect(
        journal.actorSnapshot("actor", "deployment").lease?.expiresAtMs,
      ).toBe(0);
    } finally {
      journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it("counts foreign-deployment pending rows and all retained rows without changing exact bytes", () => {
    const f = fixture();
    try {
      const lease = f.journal.acquire("actor", "builder", 0, 100);
      const intent = {
        id: "foreign",
        actor: "actor",
        deploymentIdentity: "foreign",
        headerHash: "header",
        action: "publish",
        signedCbor: "exact immutable bytes",
        txHash: "transaction",
        spentOutRefs: ["transaction#0"],
        collateralOutRefs: [],
        expectedOutRefs: ["transaction#1"],
        validUntilSlot: 100,
        completesWorkflow: false,
      };
      f.journal.persist(lease, intent, 1);
      f.journal.release(lease);
      expect(f.journal.actorSnapshot("actor", "deployment")).toMatchObject({
        retainedRecordCount: 1,
        pendingIntentCount: 1,
        reservedResourceCount: 1,
      });
      expect(f.journal.actorSnapshot("other", "deployment")).toMatchObject({
        retainedRecordCount: 1,
        pendingIntentCount: 0,
        reservedResourceCount: 0,
      });
      expect(f.journal.get("foreign")?.intent).toEqual(intent);
      f.journal.close();
      expect(() => f.journal.actorSnapshot("actor", "deployment")).toThrow();
    } finally {
      try {
        f.journal.close();
      } catch {
        /* Already closed by the probe. */
      }
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it("binds exact retained claim identities even when their counts remain equal", () => {
    const f = fixture();
    const db = new DatabaseSync(f.path);
    try {
      const lease = f.journal.acquire("actor", "builder", 0, 100);
      f.journal.persist(
        lease,
        {
          id: "prior",
          actor: "actor",
          deploymentIdentity: "deployment",
          headerHash: "header",
          action: "publish",
          signedCbor: "exact bytes",
          txHash: "transaction",
          spentOutRefs: ["normal#0"],
          collateralOutRefs: ["collateral#0"],
          expectedOutRefs: ["transaction#1"],
          validUntilSlot: 100,
          completesWorkflow: false,
        },
        1,
      );
      f.journal.transition(
        lease,
        "prior",
        "confirmed",
        "90:canonical",
        null,
        2,
      );
      f.journal.release(lease);
      const before = f.journal.actorSnapshot("actor", "deployment");
      expect(before).toMatchObject({
        reservedResourceCount: 2,
        incompatibleResourceCount: 0,
      });
      db.prepare(
        "UPDATE availability_operation_resources SET resource = ? WHERE resource = ?",
      ).run("substituted#0", "normal#0");
      const after = f.journal.actorSnapshot("actor", "deployment");
      expect(after.reservedResourceCount).toBe(before.reservedResourceCount);
      expect(after.incompatibleResourceCount).toBe(0);
      expect(after.stateDigest).not.toBe(before.stateDigest);
      // Missing intent ownership never becomes a compatible same-actor claim.
      db.prepare(
        "UPDATE availability_operation_resources SET intent_id = ? WHERE resource = ?",
      ).run("orphan", "substituted#0");
      expect(f.journal.actorSnapshot("actor", "deployment")).toMatchObject({
        reservedResourceCount: 2,
        incompatibleResourceCount: 1,
      });
    } finally {
      db.close();
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
});

describe("pending-only offline journal inventory", () => {
  it("enumerates retained pending work and an unsigned lease even when old unfinalized status is empty", () => {
    const f = fixture();
    try {
      const lease = f.journal.acquire("actor", "builder", 0, 100);
      f.journal.acquire("unsigned", "old-builder", 0, 10);
      f.journal.persist(
        lease,
        {
          id: "pending",
          actor: "actor",
          deploymentIdentity: "deployment",
          headerHash: "header",
          action: "publish",
          signedCbor: "private-bytes",
          txHash: "pending-tx",
          spentOutRefs: ["input#0"],
          collateralOutRefs: [],
          expectedOutRefs: [],
          validUntilSlot: 100,
          completesWorkflow: false,
        },
        1,
      );
      expect(f.journal.unfinalized("deployment", "actor")).toEqual([]);
      expect(f.journal.reservedOutRefs("actor")).toEqual(["input#0"]);
      const reader = openAvailabilityJournalInventory(f.path);
      const found: unknown[] = [];
      try {
        for (;;) {
          const page = reader.nextPage();
          if (page.event === "availability_journal_inventory_complete") {
            expect(page.complete).toBe(true);
            break;
          }
          if (page.family === "intents" || page.family === "leases")
            found.push(...page.rows);
        }
      } finally {
        reader.close();
      }
      expect(found).toContainEqual(
        expect.objectContaining({ id: "pending", state: "pending" }),
      );
      expect(found).toContainEqual(
        expect.objectContaining({ actor: "unsigned", owner: "old-builder" }),
      );
      expect(f.journal.get("pending")?.intent.signedCbor).toBe("private-bytes");
    } finally {
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
});
