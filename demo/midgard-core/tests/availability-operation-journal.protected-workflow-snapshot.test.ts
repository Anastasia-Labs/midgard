import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { openAvailabilityOperationJournal } from "../src/availability-operation-journal.js";

const fixture = () => {
  const dir = mkdtempSync(join(tmpdir(), "protected-workflow-snapshot-"));
  const journal = openAvailabilityOperationJournal(join(dir, "journal.sqlite"));
  const lease = journal.acquire("wallet", "reconciler", 0, 1000);
  const open = {
    id: "open",
    actor: "wallet",
    deploymentIdentity: "old",
    headerHash: "header",
    action: "open",
    signedCbor: "immutable-open",
    txHash: "open-tx",
    spentOutRefs: ["coin#0"],
    collateralOutRefs: [],
    expectedOutRefs: ["open-tx#0"],
    validUntilSlot: 100,
    completesWorkflow: false,
  };
  journal.persist(lease, open, 1);
  return {
    journal,
    lease,
    open,
    close: () => {
      journal.close();
      rmSync(dir, { recursive: true, force: true });
    },
  };
};

describe("protected foreign workflow snapshot capital guards", () => {
  it("preserves provisional own terminal capital after resource and live-workflow counts are zero", () => {
    const f = fixture();
    try {
      f.journal.transition(
        f.lease,
        "open",
        "confirmed",
        "10:canonical",
        null,
        2,
      );
      const close = {
        ...f.open,
        id: "close",
        action: "close",
        signedCbor: "immutable-close",
        txHash: "close-tx",
        spentOutRefs: ["open-tx#0"],
        expectedOutRefs: [],
        completesWorkflow: true,
      };
      f.journal.persist(f.lease, close, 3);
      f.journal.transition(
        f.lease,
        "close",
        "confirmed",
        "15:canonical-close",
        null,
        4,
      );
      f.journal.retire(
        f.lease,
        "close",
        {
          confirmationDepth: 10,
          recoveryDepth: 2160,
          currentSlot: 101,
          currentBlockNo: 20,
        },
        5,
      );
      expect(f.journal.actorSnapshot("wallet", "new")).toMatchObject({
        foreignWorkflowCount: 0,
        reservedResourceCount: 0,
        incompatibleResourceCount: 0,
        unsettledReleaseCount: 0,
        protectedForeignWorkflowCount: 1,
        retainedRecordCount: 2,
      });
      expect(
        f.journal.actorSnapshot("wallet", "old").protectedForeignWorkflowCount,
      ).toBe(0);
      expect(
        f.journal.actorSnapshot("another-wallet", "new")
          .protectedForeignWorkflowCount,
      ).toBe(0);
      expect(() =>
        f.journal.assertWorkflow(f.lease, "new", "candidate", "open", 6),
      ).toThrow("capital belongs");
      f.journal.retire(
        f.lease,
        "close",
        {
          confirmationDepth: 10,
          recoveryDepth: 2160,
          currentSlot: 101,
          currentBlockNo: 2180,
        },
        7,
      );
      expect(
        f.journal.actorSnapshot("wallet", "new").protectedForeignWorkflowCount,
      ).toBe(1);
      f.journal.retire(
        f.lease,
        "close",
        {
          confirmationDepth: 10,
          recoveryDepth: 2160,
          currentSlot: 101,
          currentBlockNo: 2181,
        },
        8,
      );
      expect(
        f.journal.actorSnapshot("wallet", "new").protectedForeignWorkflowCount,
      ).toBe(0);
      expect(f.journal.get("close")).toBeNull();
      expect(() =>
        f.journal.assertWorkflow(f.lease, "new", "candidate", "open", 9),
      ).not.toThrow();
    } finally {
      f.close();
    }
  });

  it("counts provisional foreign terminal releases until the existing protected retirement", () => {
    const f = fixture();
    try {
      expect(
        f.journal.actorSnapshot("wallet", "new").protectedForeignWorkflowCount,
      ).toBe(1);
      f.journal.transition(
        f.lease,
        "open",
        "confirmed",
        "10:canonical",
        null,
        2,
      );
      f.journal.releaseWorkflow(
        f.lease,
        "old",
        "header",
        {
          openIntentId: "open",
          reason: "challenge-closed",
          txHash: "foreign-close",
          spendPoint: "15:close",
          confirmationDepth: 10,
          recoveryDepth: 2160,
        },
        3,
      );
      expect(f.journal.actorSnapshot("wallet", "new")).toMatchObject({
        foreignWorkflowCount: 0,
        protectedForeignWorkflowCount: 1,
        unsettledReleaseCount: 1,
      });
      expect(() =>
        f.journal.assertWorkflow(f.lease, "new", "candidate", "open", 4),
      ).toThrow("capital belongs");
      expect(f.journal.get("open")?.intent).toEqual(f.open);
    } finally {
      f.close();
    }
  });

  it("does not turn an expired never-landed Open into a historical capital lock", () => {
    const f = fixture();
    try {
      f.journal.transition(
        f.lease,
        "open",
        "expired",
        null,
        "past TTL, all inputs unspent",
        2,
      );
      expect(f.journal.actorSnapshot("wallet", "new")).toMatchObject({
        retainedRecordCount: 1,
        foreignWorkflowCount: 0,
        protectedForeignWorkflowCount: 0,
        reservedResourceCount: 0,
      });
      expect(() =>
        f.journal.assertWorkflow(f.lease, "new", "candidate", "open", 3),
      ).not.toThrow();
      expect(f.journal.get("open")?.intent).toEqual(f.open);
    } finally {
      f.close();
    }
  });
});
