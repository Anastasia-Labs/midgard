import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  type AvailabilityOperationIntent,
  openAvailabilityOperationJournal,
} from "../src/availability-operation-journal.js";

const intent = (
  id: string,
  actor: string,
  deploymentIdentity: string,
): AvailabilityOperationIntent => ({
  id,
  actor,
  deploymentIdentity,
  headerHash: id,
  action: "publish",
  signedCbor: `exact signed ${id}`,
  txHash: id,
  spentOutRefs: [`${id}#0`],
  collateralOutRefs: [],
  expectedOutRefs: [`${id}#1`],
  validUntilSlot: 100,
  completesWorkflow: false,
});
describe("availability journal retained input domain", () => {
  it("counts every retained state, actor and deployment without changing intents and reconstructs on restart", () => {
    const directory = mkdtempSync(join(tmpdir(), "availability-count-"));
    const path = join(directory, "journal.sqlite");
    let journal = openAvailabilityOperationJournal(path);
    try {
      expect(journal.retainedRecordCount()).toBe(0);
      const states = [
        "pending",
        "included",
        "confirmed",
        "expired",
        "conflict",
      ] as const;
      for (const [index, state] of states.entries()) {
        const actor = `actor${index}`;
        const record = intent(`id${index}`, actor, `deployment${index}`);
        const lease = journal.acquire(actor, "owner", 0, 10000);
        journal.persist(lease, record, 1);
        if (state !== "pending")
          journal.transition(
            lease,
            record.id,
            state,
            state === "included" || state === "confirmed" ? "100:aa" : null,
            "retained",
            2,
          );
        journal.release(lease);
      }
      const before = states.map((_, index) => journal.get(`id${index}`));
      expect(journal.retainedRecordCount()).toBe(5);
      expect(journal.pending("deployment0", "actor0")).toHaveLength(1);
      expect(journal.unfinalized("deployment1", "actor1")).toHaveLength(1);
      expect(states.map((_, index) => journal.get(`id${index}`))).toEqual(
        before,
      );
      journal.close();
      journal = openAvailabilityOperationJournal(path);
      expect(journal.retainedRecordCount()).toBe(5);
      expect(states.map((_, index) => journal.get(`id${index}`))).toEqual(
        before,
      );
    } finally {
      journal.close();
      rmSync(directory, { recursive: true, force: true });
    }
  });
  it("tracks authoritative pruning while counting expired rows that ordinary active subsets omit", () => {
    const directory = mkdtempSync(join(tmpdir(), "availability-count-prune-"));
    const journal = openAvailabilityOperationJournal(
      join(directory, "journal.sqlite"),
    );
    try {
      const lease = journal.acquire("actor", "owner", 0, 10000);
      const record = intent("expired", "actor", "deployment");
      journal.persist(lease, record, 1);
      journal.transition(lease, record.id, "expired", null, null, 2);
      expect(journal.pending("deployment", "actor")).toHaveLength(0);
      expect(journal.unfinalized("deployment", "actor")).toHaveLength(0);
      expect(journal.finalizedAnchors("actor")).toHaveLength(0);
      expect(journal.retainedRecordCount()).toBe(1);
      journal.pruneExpired(lease, 100, 2160, 3);
      expect(journal.retainedRecordCount()).toBe(1);
      journal.pruneExpired(lease, 2261, 2160, 4);
      expect(journal.retainedRecordCount()).toBe(0);
    } finally {
      journal.close();
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
