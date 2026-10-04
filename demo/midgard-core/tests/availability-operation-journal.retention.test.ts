import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it, vi } from "vitest";

import {
  type AvailabilityOperationIntent,
  openAvailabilityOperationJournal,
} from "../src/availability-operation-journal.js";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "../src/deployment-manifest-identity.js";

const RECOVERY = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
const dirs: string[] = [];
const path = () => {
  const dir = mkdtempSync(join(tmpdir(), "availability-journal-retention-"));
  dirs.push(dir);
  return join(dir, "operations.sqlite");
};
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);
const intent = (
  id: string,
  overrides: Partial<AvailabilityOperationIntent> = {},
): AvailabilityOperationIntent => ({
  id,
  actor: "actor",
  deploymentIdentity: "deployment",
  headerHash: "header",
  action: "publish",
  signedCbor: `signed ${id}`,
  txHash: id,
  spentOutRefs: [`${id}-input#0`],
  collateralOutRefs: ["collateral#0"],
  expectedOutRefs: [`${id}#1`],
  validUntilSlot: 100,
  completesWorkflow: false,
  ...overrides,
});
const rows = (database: string, sql: string) => {
  const db = new DatabaseSync(database);
  try {
    return db
      .prepare(sql)
      .all()
      .map((row) => ({ ...row }));
  } finally {
    db.close();
  }
};
/** A confirmed Open and the confirmed terminal Remove that spends it. */
const finishedChallenge = (database: string) => {
  const journal = openAvailabilityOperationJournal(database);
  const lease = journal.acquire("actor", "owner", 0, 1_000);
  journal.persist(lease, intent("open", { action: "open" }), 1);
  journal.persist(
    lease,
    intent("remove", {
      action: "remove",
      completesWorkflow: true,
      spentOutRefs: ["open#1"],
      validUntilSlot: 200,
    }),
    2,
  );
  journal.transition(lease, "open", "confirmed", "10:aa", null, 3);
  journal.transition(lease, "remove", "confirmed", "20:bb", null, 4);
  return { journal, lease };
};

describe("availability journal retention is bounded by validity and recovery depth, never by confirmation depth", () => {
  it("keeps a confirmed intent's reservations until its validity passes", () => {
    const { journal, lease } = finishedChallenge(path());
    try {
      expect(journal.reservedOutRefs("actor")).toEqual([
        "collateral#0",
        "open#1",
        "open-input#0",
      ]);
      // Deep enough to be confirmed, still able to land again after a rollback.
      journal.retire(
        lease,
        "remove",
        { confirmationDepth: 30, currentSlot: 99, recoveryDepth: RECOVERY },
        5,
      );
      expect(journal.reservedOutRefs("actor")).toEqual([
        "collateral#0",
        "open#1",
        "open-input#0",
      ]);
      // The Open's validity has passed, the Remove's has not.
      journal.retire(
        lease,
        "remove",
        { confirmationDepth: 31, currentSlot: 100, recoveryDepth: RECOVERY },
        6,
      );
      expect(journal.reservedOutRefs("actor")).toEqual([
        "collateral#0",
        "open#1",
      ]);
      // No authoritative slot releases nothing.
      journal.retire(
        lease,
        "remove",
        { confirmationDepth: 32, recoveryDepth: RECOVERY },
        7,
      );
      expect(journal.reservedOutRefs("actor")).toEqual([
        "collateral#0",
        "open#1",
      ]);
      journal.retire(
        lease,
        "remove",
        { confirmationDepth: 33, currentSlot: 200, recoveryDepth: RECOVERY },
        8,
      );
      expect(journal.reservedOutRefs("actor")).toEqual([]);
      expect(journal.get("open")?.state).toBe("confirmed");
      expect(journal.get("remove")?.state).toBe("confirmed");
    } finally {
      journal.close();
    }
  });

  it("retires a terminal workflow row at confirmation and prunes the chain only past recovery depth", () => {
    const database = path();
    const { journal, lease } = finishedChallenge(database);
    try {
      // Provisional progress admits this deployment; the capital obligation survives.
      expect(journal.workflows("actor")).toEqual([]);
      expect(() =>
        journal.assertWorkflow(lease, "other-deployment", "h", "open", 5),
      ).toThrow(/capital belongs/);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "other-header", "open", 5),
      ).not.toThrow();
      expect(
        rows(
          database,
          "SELECT retired_by FROM availability_operation_workflows",
        ),
      ).toEqual([{ retired_by: "remove" }]);
      journal.retire(
        lease,
        "remove",
        {
          confirmationDepth: RECOVERY,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        6,
      );
      expect(journal.get("open")?.state).toBe("confirmed");
      expect(journal.get("remove")?.state).toBe("confirmed");
      expect(
        rows(
          database,
          "SELECT retired_by FROM availability_operation_workflows",
        ),
      ).toHaveLength(1);
      journal.retire(
        lease,
        "remove",
        {
          confirmationDepth: RECOVERY + 1,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        7,
      );
      expect(journal.get("open")).toBeNull();
      expect(journal.get("remove")).toBeNull();
      expect(journal.finalizedAnchors("deployment", "actor")).toEqual([]);
      for (const table of ["workflows", "resources", "dependencies"])
        expect(
          rows(database, `SELECT * FROM availability_operation_${table}`),
        ).toEqual([]);
    } finally {
      journal.close();
    }
  });

  it("never prunes the Open of a live workflow", () => {
    const journal = openAvailabilityOperationJournal(path());
    try {
      const lease = journal.acquire("actor", "owner", 0, 1_000);
      journal.persist(lease, intent("open", { action: "open" }), 1);
      journal.transition(lease, "open", "confirmed", "10:aa", null, 2);
      journal.persist(
        lease,
        intent("publish", { spentOutRefs: ["open#1"] }),
        3,
      );
      journal.transition(lease, "publish", "confirmed", "20:bb", null, 4);
      journal.retire(
        lease,
        "publish",
        {
          confirmationDepth: RECOVERY + 1,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        5,
      );
      expect(journal.get("publish")).toBeNull();
      expect(journal.get("open")?.state).toBe("confirmed");
      expect(journal.workflows("actor")).toMatchObject([
        { headerHash: "header", confirmedOpens: [{ intent: { id: "open" } }] },
      ]);
    } finally {
      journal.close();
    }
  });

  it("retires a workflow a foreign terminal released and prunes it with its Open past recovery depth", () => {
    const database = path();
    const journal = openAvailabilityOperationJournal(database);
    try {
      const lease = journal.acquire("actor", "owner", 0, 1_000);
      journal.persist(lease, intent("open", { action: "open" }), 1);
      journal.transition(lease, "open", "confirmed", "10:aa", null, 2);
      journal.releaseWorkflow(
        lease,
        "deployment",
        "header",
        {
          openIntentId: "open",
          reason: "challenge-closed",
          txHash: "ab".repeat(32),
          spendPoint: "11:cd",
          confirmationDepth: 30,
          recoveryDepth: RECOVERY,
        },
        3,
      );
      expect(journal.workflows("actor")).toEqual([]);
      const workflowRows = () =>
        rows(
          database,
          "SELECT retired_by FROM availability_operation_workflows",
        );
      expect(workflowRows()).toEqual([{ retired_by: "open" }]);
      // Our Open itself rolled back: its workflow is live again.
      journal.rewind(lease, "open", "rolled back", 4);
      expect(journal.workflows("actor")).toMatchObject([
        { headerHash: "header", confirmedOpens: [] },
      ]);
      journal.transition(lease, "open", "confirmed", "12:ee", null, 5);
      journal.releaseWorkflow(
        lease,
        "deployment",
        "header",
        {
          openIntentId: "open",
          reason: "challenge-closed",
          txHash: "ab".repeat(32),
          spendPoint: "13:cd",
          // Final: nothing is left to re-verify, so the Open may go.
          confirmationDepth: RECOVERY + 1,
          recoveryDepth: RECOVERY,
        },
        6,
      );
      journal.retire(
        lease,
        "open",
        {
          confirmationDepth: RECOVERY,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        7,
      );
      expect(workflowRows()).toEqual([{ retired_by: "open" }]);
      journal.retire(
        lease,
        "open",
        {
          confirmationDepth: RECOVERY + 1,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        8,
      );
      expect(journal.get("open")).toBeNull();
      expect(workflowRows()).toEqual([]);
    } finally {
      journal.close();
    }
  });

  it("rewinds a contradicted confirmation, restoring its reservations and its workflow", () => {
    const { journal, lease } = finishedChallenge(path());
    try {
      journal.retire(
        lease,
        "remove",
        { confirmationDepth: 40, currentSlot: 300, recoveryDepth: RECOVERY },
        5,
      );
      expect(journal.reservedOutRefs("actor")).toEqual([]);
      expect(() => journal.rewind(lease, "missing", "rolled back", 6)).toThrow(
        /confirmed intent/,
      );
      const foreign = journal.acquire("other", "owner", 6, 100);
      expect(() => journal.rewind(foreign, "remove", "rolled back", 6)).toThrow(
        /different actor/,
      );
      journal.rewind(lease, "remove", "rolled back", 7);
      expect(journal.get("remove")).toMatchObject({
        state: "pending",
        inclusionPoint: null,
        detail: "rolled back",
      });
      expect(journal.reservedOutRefs("actor")).toEqual([
        "collateral#0",
        "open#1",
      ]);
      expect(journal.pending("deployment", "actor")).toHaveLength(1);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 8),
      ).toThrow(/already has a live challenge workflow/);
      expect(() => journal.rewind(lease, "remove", "again", 9)).toThrow(
        /confirmed intent/,
      );
      // The rewound intent resolves through the ordinary transitions.
      journal.transition(lease, "remove", "included", "21:cc", null, 10);
      journal.transition(lease, "remove", "confirmed", "21:cc", null, 11);
      expect(journal.workflows("actor")).toEqual([]);
    } finally {
      journal.close();
    }
  });

  it("clears a persisted halt row on open with one notice and never refuses work for it", () => {
    const database = path();
    openAvailabilityOperationJournal(database).close();
    const legacy = new DatabaseSync(database);
    legacy.exec(
      "INSERT INTO availability_journal_metadata VALUES ('halt', 'post-finality rollback')",
    );
    legacy.close();
    const notice = vi.fn();
    let journal = openAvailabilityOperationJournal(database, {
      onLegacyHaltCleared: notice,
    });
    try {
      expect(notice.mock.calls).toEqual([["post-finality rollback"]]);
      const lease = journal.acquire("actor", "owner", 0, 100);
      journal.persist(lease, intent("first"), 1);
      expect(journal.pending("deployment", "actor")).toHaveLength(1);
      expect(journal.reservedOutRefs("actor")).toContain("first-input#0");
    } finally {
      journal.close();
    }
    expect(
      rows(
        database,
        "SELECT * FROM availability_journal_metadata WHERE key = 'halt'",
      ),
    ).toEqual([]);
    journal = openAvailabilityOperationJournal(database, {
      onLegacyHaltCleared: notice,
    });
    journal.close();
    expect(notice).toHaveBeenCalledTimes(1);
  });

  it("migrates a schema-2 journal to schema 3 and keeps its live workflow", () => {
    const database = path();
    const legacy = new DatabaseSync(database);
    legacy.exec(`
      CREATE TABLE availability_journal_metadata (
        key TEXT PRIMARY KEY, value TEXT NOT NULL
      );
      CREATE TABLE availability_operation_workflows (actor TEXT NOT NULL,
        deployment TEXT NOT NULL, header_hash TEXT NOT NULL,
        PRIMARY KEY(actor, deployment, header_hash));
      INSERT INTO availability_journal_metadata VALUES ('schema', '2');
      INSERT INTO availability_operation_workflows VALUES ('actor', 'deployment', 'header');
    `);
    legacy.close();
    const journal = openAvailabilityOperationJournal(database);
    try {
      const lease = journal.acquire("actor", "owner", 0, 100);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 1),
      ).toThrow(/already has a live challenge workflow/);
    } finally {
      journal.close();
    }
    expect(
      rows(
        database,
        "SELECT value FROM availability_journal_metadata WHERE key = 'schema'",
      ),
    ).toEqual([{ value: "3" }]);
    expect(
      rows(database, "SELECT * FROM availability_operation_workflows"),
    ).toEqual([
      {
        actor: "actor",
        deployment: "deployment",
        header_hash: "header",
        retired_by: null,
        release: null,
      },
    ]);
  });
});
