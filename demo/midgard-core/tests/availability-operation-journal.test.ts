import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";
import { fileURLToPath } from "node:url";

import { afterEach, describe, expect, it } from "vitest";

import {
  type AvailabilityOperationIntent,
  openAvailabilityOperationJournal,
} from "../src/availability-operation-journal.js";

const dirs: string[] = [];
const path = () => {
  const dir = mkdtempSync(join(tmpdir(), "availability-journal-"));
  dirs.push(dir);
  return join(dir, "operations.sqlite");
};
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);
const intent = (
  id = "first",
  actor = "actor",
): AvailabilityOperationIntent => ({
  id,
  actor,
  deploymentIdentity: "deployment",
  headerHash: "header",
  action: "publish",
  signedCbor: "signed bytes",
  txHash: id,
  spentOutRefs: [`${id}#0`],
  collateralOutRefs: ["collateral#0"],
  expectedOutRefs: [`${id}#1`],
  validUntilSlot: 100,
  completesWorkflow: false,
});

describe("availability operation journal durability and fencing", () => {
  it("survives a killed process after persisting signed intent and its reservation", async () => {
    const database = path();
    const modulePath = fileURLToPath(
      new URL("../src/availability-operation-journal.ts", import.meta.url),
    );
    const child = spawn(
      process.execPath,
      [
        "--experimental-strip-types",
        "--input-type=module",
        "-e",
        `
      import { openAvailabilityOperationJournal } from ${JSON.stringify(modulePath)};
      const journal = openAvailabilityOperationJournal(process.argv[1]);
      const intent = JSON.parse(process.argv[2]);
      const lease = journal.acquire('actor', 'child', 0, 100);
      journal.persist(lease, intent, 1);
      process.stdout.write('persisted');
      setInterval(() => {}, 1000);
    `,
        database,
        JSON.stringify(intent()),
      ],
      { stdio: ["ignore", "pipe", "pipe"] },
    );
    await once(child.stdout!, "data");
    child.kill("SIGKILL");
    await once(child, "exit");
    const journal = openAvailabilityOperationJournal(database);
    try {
      expect(journal.pending("deployment", "actor")[0]?.intent).toEqual(
        intent(),
      );
      const lease = journal.acquire("actor", "restart", 101, 100);
      expect(() =>
        journal.persist(
          lease,
          { ...intent("second"), spentOutRefs: intent().spentOutRefs },
          102,
        ),
      ).toThrow(/reserved/);
    } finally {
      journal.close();
    }
  });

  it("fences superseded builders across connections and preserves same-actor collateral until every child finalizes", () => {
    const database = path();
    const first = openAvailabilityOperationJournal(database);
    const second = openAvailabilityOperationJournal(database);
    try {
      const expired = first.acquire("actor", "old", 0, 100);
      expect(() => second.acquire("actor", "new", 1, 100)).toThrow(/leased/);
      const lease = second.acquire("actor", "new", 101, 100);
      expect(() => first.persist(expired, intent(), 102)).toThrow(/superseded/);
      second.persist(lease, intent(), 102);
      expect(() =>
        first.acquire("actor", "other-deployment", 102, 100),
      ).toThrow(/leased/);
      second.transition(lease, "first", "included", "block1", null, 103);
      second.persist(lease, intent("child"), 104);
      expect(first.reservedOutRefs("actor")).toContain("collateral#0");
      second.transition(lease, "first", "confirmed", "block1", null, 105);
      const other = first.acquire("other", "different-wallet", 106, 100);
      expect(() => first.persist(other, intent("third", "other"), 107)).toThrow(
        /reserved/,
      );
      expect(() =>
        first.persist(
          other,
          {
            ...intent("third", "other"),
            deploymentIdentity: "other-deployment",
          },
          107,
        ),
      ).toThrow(/reserved/);
      expect(() =>
        second.persist(
          lease,
          {
            ...intent("spend-collateral"),
            spentOutRefs: ["collateral#0"],
            collateralOutRefs: ["another#0"],
          },
          108,
        ),
      ).toThrow(/reserved/);
      second.transition(lease, "child", "confirmed", "block2", null, 109);
      expect(first.reservedOutRefs("actor")).toEqual([]);
      first.persist(other, intent("third", "other"), 110);
      expect(first.pending("deployment", "other")).toHaveLength(1);
    } finally {
      first.close();
      second.close();
    }
  });

  it("refuses changed signed bytes and cross-actor transitions, and persists a rollback halt", () => {
    const database = path();
    let journal = openAvailabilityOperationJournal(database);
    const lease = journal.acquire("actor", "owner", 0, 100);
    journal.persist(lease, intent(), 1);
    expect(() =>
      journal.persist(lease, { ...intent(), signedCbor: "replacement" }, 2),
    ).toThrow(/different signed bytes/);
    const foreign = journal.acquire("other", "foreign", 0, 100);
    expect(() =>
      journal.transition(foreign, "first", "expired", null, null, 3),
    ).toThrow(/different actor/);
    journal.halt("post-finality rollback");
    journal.close();
    journal = openAvailabilityOperationJournal(database);
    try {
      expect(() => journal.acquire("actor", "retry", 101, 100)).toThrow(
        /post-finality rollback/,
      );
    } finally {
      journal.close();
    }
  });

  it("admits one challenge workflow per header and keeps each through timeout continuation and finality", () => {
    const journal = openAvailabilityOperationJournal(path());
    try {
      const lease = journal.acquire("actor", "owner", 0, 100);
      journal.persist(lease, { ...intent("open"), action: "open" }, 1);
      journal.transition(lease, "open", "included", "block1", null, 2);
      // Another withheld header of the same deployment is prepared and opened
      // while the first challenge is live.
      expect(() =>
        journal.assertWorkflow(
          lease,
          "deployment",
          "other-header",
          "prepare",
          3,
        ),
      ).not.toThrow();
      journal.persist(
        lease,
        {
          ...intent("open-other"),
          headerHash: "other-header",
          action: "open",
          collateralOutRefs: ["collateral#0"],
        },
        4,
      );
      journal.transition(lease, "open-other", "included", "block2", null, 5);
      // A header whose own Open landed needs no second challenger coin.
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 6),
      ).toThrow(/already has a live challenge workflow/);
      expect(() =>
        journal.assertWorkflow(
          lease,
          "deployment",
          "other-header",
          "prepare",
          6,
        ),
      ).toThrow(/already has a live challenge workflow/);
      journal.persist(
        lease,
        { ...intent("timeout"), action: "timeout", spentOutRefs: ["open#1"] },
        7,
      );
      journal.transition(lease, "timeout", "confirmed", "block3", null, 8);
      journal.persist(
        lease,
        {
          ...intent("remove"),
          action: "remove",
          completesWorkflow: true,
          spentOutRefs: ["timeout#1"],
        },
        9,
      );
      journal.transition(lease, "remove", "included", "block4", null, 10);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 11),
      ).toThrow(/already has a live challenge workflow/);
      journal.transition(lease, "remove", "confirmed", "block4", null, 12);
      // The terminal step released only its own header's workflow.
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 13),
      ).not.toThrow();
      expect(() =>
        journal.assertWorkflow(
          lease,
          "deployment",
          "other-header",
          "prepare",
          13,
        ),
      ).toThrow(/already has a live challenge workflow/);
      expect(
        journal
          .finalizedAnchors("deployment", "actor")
          .map((record) => record.intent.id),
      ).toEqual(["remove"]);
    } finally {
      journal.close();
    }
  });

  it("refuses every step for another deployment while a workflow is live", () => {
    const journal = openAvailabilityOperationJournal(path());
    try {
      const lease = journal.acquire("actor", "owner", 0, 100);
      journal.persist(lease, { ...intent("open"), action: "open" }, 1);
      for (const action of ["prepare", "open", "publish", "timeout"]) {
        expect(() =>
          journal.assertWorkflow(
            lease,
            "other-deployment",
            "header",
            action,
            2,
          ),
        ).toThrow(/capital belongs to an unresolved challenge workflow/);
      }
      expect(() =>
        journal.persist(
          lease,
          {
            ...intent("foreign-open"),
            deploymentIdentity: "other-deployment",
            action: "open",
          },
          3,
        ),
      ).toThrow(/capital belongs/);
      journal.transition(lease, "open", "expired", null, null, 4);
      expect(() =>
        journal.assertWorkflow(
          lease,
          "other-deployment",
          "header",
          "prepare",
          5,
        ),
      ).not.toThrow();
    } finally {
      journal.close();
    }
  });

  describe("releasing a workflow someone else's terminal step ended (P20)", () => {
    const evidence = (openIntentId = "open") => ({
      openIntentId,
      reason: "challenge-closed" as const,
      txHash: "ab".repeat(32),
      spendPoint: "10:cd",
      confirmationDepth: 30,
    });
    // Our Open for (deployment, header) and another for (deployment, other
    // header), both landed; each opens its own workflow row.
    const opened = () => {
      const journal = openAvailabilityOperationJournal(path());
      const lease = journal.acquire("actor", "owner", 0, 1_000);
      journal.persist(lease, { ...intent("open"), action: "open" }, 1);
      journal.persist(
        lease,
        {
          ...intent("other-open"),
          headerHash: "other-header",
          action: "open",
        },
        2,
      );
      journal.transition(lease, "open", "confirmed", "block1", null, 3);
      journal.transition(lease, "other-open", "confirmed", "block1", null, 4);
      return { journal, lease };
    };

    it("lists the actor's rows with their confirmed Opens and deletes only the named row", () => {
      const { journal, lease } = opened();
      try {
        const reserved = journal.reservedOutRefs("actor");
        expect(
          journal
            .workflows("actor")
            .map(({ deploymentIdentity, headerHash, confirmedOpens }) => ({
              deploymentIdentity,
              headerHash,
              opens: confirmedOpens.map(({ intent }) => intent.id),
            })),
        ).toEqual([
          {
            deploymentIdentity: "deployment",
            headerHash: "header",
            opens: ["open"],
          },
          {
            deploymentIdentity: "deployment",
            headerHash: "other-header",
            opens: ["other-open"],
          },
        ]);
        expect(journal.workflows("someone-else")).toEqual([]);
        expect(() =>
          journal.assertWorkflow(lease, "other-deployment", "h", "open", 5),
        ).toThrow(/capital belongs/);
        journal.releaseWorkflow(lease, "deployment", "header", evidence(), 5);
        expect(
          journal.workflows("actor").map(({ headerHash }) => headerHash),
        ).toEqual(["other-header"]);
        journal.releaseWorkflow(
          lease,
          "deployment",
          "other-header",
          evidence("other-open"),
          6,
        );
        expect(journal.workflows("actor")).toEqual([]);
        // Admission in another deployment follows; reservations, intents and
        // the lease are untouched.
        expect(() =>
          journal.assertWorkflow(lease, "other-deployment", "h", "open", 7),
        ).not.toThrow();
        expect(journal.reservedOutRefs("actor")).toEqual(reserved);
        expect(journal.get("open")?.state).toBe("confirmed");
        expect(() => journal.assertLease(lease, 8)).not.toThrow();
        expect(() =>
          journal.releaseWorkflow(lease, "deployment", "header", evidence(), 9),
        ).toThrow("Availability workflow release names no live workflow");
      } finally {
        journal.close();
      }
    });

    it("refuses under a foreign or superseded lease", () => {
      const { journal, lease } = opened();
      try {
        journal.release(lease);
        const foreign = journal.acquire("someone-else", "owner", 10, 100);
        expect(() =>
          journal.releaseWorkflow(
            foreign,
            "deployment",
            "header",
            evidence(),
            11,
          ),
        ).toThrow(/confirmed Open/);
        expect(() =>
          journal.releaseWorkflow(
            lease,
            "deployment",
            "header",
            evidence(),
            11,
          ),
        ).toThrow(/lease expired or superseded/);
        expect(journal.workflows("actor")).toHaveLength(2);
      } finally {
        journal.close();
      }
    });

    it("refuses while the actor has an unresolved intent for the header", () => {
      for (const state of ["pending", "included", "conflict"] as const) {
        const { journal, lease } = opened();
        try {
          journal.persist(lease, { ...intent("close"), action: "close" }, 5);
          if (state !== "pending")
            journal.transition(lease, "close", state, "block2", null, 6);
          expect(() =>
            journal.releaseWorkflow(
              lease,
              "deployment",
              "header",
              evidence(),
              7,
            ),
          ).toThrow(/no unresolved intent/);
          // Another header's unresolved intent does not hold this row.
          journal.releaseWorkflow(
            lease,
            "deployment",
            "other-header",
            evidence("other-open"),
            8,
          );
          expect(
            journal.workflows("actor").map(({ headerHash }) => headerHash),
          ).toEqual(["header"]);
        } finally {
          journal.close();
        }
      }
    });

    it("refuses without the actor's confirmed Open for that header", () => {
      const journal = openAvailabilityOperationJournal(path());
      try {
        const lease = journal.acquire("actor", "owner", 0, 1_000);
        journal.persist(lease, { ...intent("open"), action: "open" }, 1);
        journal.transition(lease, "open", "included", "block1", null, 2);
        // An included Open is unresolved.
        expect(() =>
          journal.releaseWorkflow(lease, "deployment", "header", evidence(), 3),
        ).toThrow(/no unresolved intent/);
        journal.transition(lease, "open", "confirmed", "block1", null, 4);
        journal.persist(lease, intent("publish"), 5);
        journal.transition(lease, "publish", "confirmed", "block2", null, 6);
        for (const id of ["missing", "publish"])
          expect(() =>
            journal.releaseWorkflow(
              lease,
              "deployment",
              "header",
              evidence(id),
              7,
            ),
          ).toThrow(/confirmed Open/);
        expect(() =>
          journal.releaseWorkflow(
            lease,
            "deployment",
            "other-header",
            evidence(),
            7,
          ),
        ).toThrow(/confirmed Open/);
        expect(() =>
          journal.releaseWorkflow(
            lease,
            "other-deployment",
            "header",
            evidence(),
            7,
          ),
        ).toThrow(/confirmed Open/);
        expect(journal.workflows("actor")).toHaveLength(1);
      } finally {
        journal.close();
      }
    });
  });

  it("migrates a schema-1 journal in place and keeps its live workflow", () => {
    const database = path();
    const legacy = new DatabaseSync(database);
    legacy.exec(`
      CREATE TABLE availability_journal_metadata (
        key TEXT PRIMARY KEY, value TEXT NOT NULL
      );
      CREATE TABLE availability_operation_workflows (
        actor TEXT PRIMARY KEY, deployment TEXT NOT NULL, header_hash TEXT NOT NULL
      );
      INSERT INTO availability_journal_metadata VALUES ('schema', '1');
      INSERT INTO availability_operation_workflows VALUES ('actor', 'deployment', 'header');
    `);
    legacy.close();
    let journal = openAvailabilityOperationJournal(database);
    try {
      const lease = journal.acquire("actor", "owner", 0, 100);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 1),
      ).toThrow(/already has a live challenge workflow/);
      expect(() =>
        journal.assertWorkflow(lease, "other-deployment", "header", "open", 1),
      ).toThrow(/capital belongs/);
      journal.persist(
        lease,
        { ...intent("open-other"), headerHash: "other-header", action: "open" },
        2,
      );
      journal.close();
      journal = openAvailabilityOperationJournal(database);
      const reopened = journal.acquire("actor", "restart", 101, 100);
      expect(() =>
        journal.assertWorkflow(
          reopened,
          "deployment",
          "header",
          "prepare",
          102,
        ),
      ).toThrow(/already has a live challenge workflow/);
      expect(() =>
        journal.assertWorkflow(
          reopened,
          "deployment",
          "other-header",
          "prepare",
          102,
        ),
      ).toThrow(/already has a live challenge workflow/);
    } finally {
      journal.close();
    }
    const migrated = new DatabaseSync(database);
    try {
      expect(
        migrated
          .prepare(
            "SELECT value FROM availability_journal_metadata WHERE key = 'schema'",
          )
          .get()?.value,
      ).toBe("2");
      expect(
        migrated
          .prepare(
            "SELECT actor, deployment, header_hash FROM availability_operation_workflows ORDER BY header_hash",
          )
          .all()
          .map((row) => ({ ...row })),
      ).toEqual([
        { actor: "actor", deployment: "deployment", header_hash: "header" },
        {
          actor: "actor",
          deployment: "deployment",
          header_hash: "other-header",
        },
      ]);
    } finally {
      migrated.close();
    }
  });

  it("refuses a journal of an unknown schema", () => {
    const database = path();
    const future = new DatabaseSync(database);
    future.exec(`
      CREATE TABLE availability_journal_metadata (
        key TEXT PRIMARY KEY, value TEXT NOT NULL
      );
      INSERT INTO availability_journal_metadata VALUES ('schema', '3');
    `);
    future.close();
    expect(() => openAvailabilityOperationJournal(database)).toThrow(
      "Unsupported availability operation journal schema",
    );
  });
});
