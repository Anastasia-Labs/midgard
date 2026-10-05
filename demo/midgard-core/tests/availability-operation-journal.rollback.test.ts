import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import {
  type AvailabilityOperationIntent,
  openAvailabilityOperationJournal,
} from "../src/availability-operation-journal.js";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "../src/deployment-manifest-identity.js";

const RECOVERY = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
const dirs: string[] = [];
const path = () => {
  const dir = mkdtempSync(join(tmpdir(), "availability-journal-rollback-"));
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
  action: "open",
  signedCbor: `signed ${id}`,
  txHash: id,
  spentOutRefs: [`${id}-input#0`],
  collateralOutRefs: [],
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
const CLOSE = {
  openIntentId: "open",
  reason: "challenge-closed" as const,
  txHash: "ce".repeat(32),
  spendPoint: "11:cd",
  confirmationDepth: 30,
  recoveryDepth: RECOVERY,
};
/** Our confirmed Open, its workflow released on a foreign Close at depth 30. */
const released = () => {
  const database = path();
  const journal = openAvailabilityOperationJournal(database);
  const lease = journal.acquire("actor", "owner", 0, 1_000);
  journal.persist(lease, intent("open"), 1);
  journal.transition(lease, "open", "confirmed", "10:aa", null, 2);
  journal.releaseWorkflow(lease, "deployment", "header", CLOSE, 3);
  return { database, journal, lease };
};
const live = (
  journal: ReturnType<typeof openAvailabilityOperationJournal>,
  lease: ReturnType<
    ReturnType<typeof openAvailabilityOperationJournal>["acquire"]
  >,
) =>
  expect(() =>
    journal.assertWorkflow(lease, "other-deployment", "h", "open", 9),
  ).toThrow(/capital belongs/);

describe("a foreign terminal's release stays reversible until it is final", () => {
  it("keeps the unsettled evidence and the Open, refreshes it, and revives the row when the terminal rolls back", () => {
    const { journal, lease } = released();
    try {
      expect(journal.workflows("actor")).toEqual([]);
      live(journal, lease);
      expect(journal.unsettledReleases("actor")).toMatchObject([
        {
          deploymentIdentity: "deployment",
          headerHash: "header",
          open: { state: "confirmed", intent: { id: "open" } },
          release: {
            reason: "challenge-closed",
            txHash: CLOSE.txHash,
            spendPoint: CLOSE.spendPoint,
          },
        },
      ]);
      // A final Open is still kept: the release check starts from its bytes.
      journal.retire(
        lease,
        "open",
        {
          confirmationDepth: RECOVERY + 1,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        4,
      );
      expect(journal.get("open")?.state).toBe("confirmed");
      // Re-verified at a new point while unsettled: refreshed, not refused.
      journal.releaseWorkflow(
        lease,
        "deployment",
        "header",
        { ...CLOSE, spendPoint: "12:ef", confirmationDepth: 40 },
        5,
      );
      expect(journal.unsettledReleases("actor")[0]?.release.spendPoint).toBe(
        "12:ef",
      );
      // The Close is no longer canonical: the challenge is live again.
      journal.reviveWorkflow(lease, "deployment", "header", 6);
      expect(journal.unsettledReleases("actor")).toEqual([]);
      expect(journal.workflows("actor")).toMatchObject([
        { headerHash: "header", confirmedOpens: [{ intent: { id: "open" } }] },
      ]);
      live(journal, lease);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 7),
      ).toThrow(/already has a live challenge workflow/);
      expect(() =>
        journal.reviveWorkflow(lease, "deployment", "header", 8),
      ).toThrow(/no unsettled release/);
    } finally {
      journal.close();
    }
  });

  it("settles a release only beyond recovery depth, then refuses to touch it", () => {
    const { journal, lease } = released();
    try {
      journal.releaseWorkflow(
        lease,
        "deployment",
        "header",
        { ...CLOSE, confirmationDepth: RECOVERY + 1 },
        4,
      );
      expect(journal.unsettledReleases("actor")).toEqual([]);
      expect(() =>
        journal.assertWorkflow(lease, "other-deployment", "h", "open", 5),
      ).not.toThrow();
      expect(() =>
        journal.reviveWorkflow(lease, "deployment", "header", 5),
      ).toThrow(/no unsettled release/);
      expect(() =>
        journal.releaseWorkflow(lease, "deployment", "header", CLOSE, 6),
      ).toThrow(/names no live workflow/);
      expect(() =>
        journal.releaseWorkflow(
          lease,
          "deployment",
          "header",
          { ...CLOSE, recoveryDepth: 0 },
          6,
        ),
      ).toThrow(/Invalid availability workflow release evidence/);
      journal.retire(
        lease,
        "open",
        {
          confirmationDepth: RECOVERY + 1,
          currentSlot: 500,
          recoveryDepth: RECOVERY,
        },
        7,
      );
      expect(journal.get("open")).toBeNull();
    } finally {
      journal.close();
    }
  });

  it("lets our own confirmed terminal step settle a release whose foreign evidence is not final", () => {
    const { database, journal, lease } = released();
    try {
      journal.persist(
        lease,
        intent("remove", {
          action: "remove",
          completesWorkflow: true,
          spentOutRefs: ["open#1"],
        }),
        4,
      );
      journal.transition(lease, "remove", "confirmed", "20:bb", null, 5);
      // Our terminal ended the workflow; the foreign one no longer matters.
      expect(journal.unsettledReleases("actor")).toEqual([]);
      expect(
        rows(
          database,
          "SELECT retired_by, release FROM availability_operation_workflows",
        ),
      ).toEqual([{ retired_by: "remove", release: null }]);
      expect(() =>
        journal.reviveWorkflow(lease, "deployment", "header", 6),
      ).toThrow(/no unsettled release/);
    } finally {
      journal.close();
    }
  });

  it("makes a header re-opened after a foreign release live again", () => {
    const { journal, lease } = released();
    try {
      journal.persist(lease, intent("reopen"), 5);
      expect(journal.unsettledReleases("actor")).toEqual([]);
      expect(journal.workflows("actor")).toMatchObject([
        { headerHash: "header" },
      ]);
      live(journal, lease);
    } finally {
      journal.close();
    }
  });

  it("keeps the header's row when that re-open expires, so the earlier Open is neither pruned nor forgotten", () => {
    const { journal, lease } = released();
    try {
      journal.persist(lease, intent("reopen"), 5);
      journal.transition(lease, "reopen", "expired", null, "never landed", 6);
      // The re-open cleared the earlier Open's release evidence, so the row
      // stays live and the release walk re-derives it from that Open.
      expect(journal.workflows("actor")).toMatchObject([
        { headerHash: "header", confirmedOpens: [{ intent: { id: "open" } }] },
      ]);
      live(journal, lease);
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
      expect(journal.get("open")?.state).toBe("confirmed");
      journal.releaseWorkflow(lease, "deployment", "header", CLOSE, 8);
      expect(journal.unsettledReleases("actor")).toMatchObject([
        { open: { intent: { id: "open" } } },
      ]);
    } finally {
      journal.close();
    }
  });

  it("drops the header's row when its only Open expires", () => {
    const journal = openAvailabilityOperationJournal(path());
    try {
      const lease = journal.acquire("actor", "owner", 0, 1_000);
      journal.persist(lease, intent("open"), 1);
      journal.transition(lease, "open", "expired", null, "never landed", 2);
      expect(journal.workflows("actor")).toEqual([]);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 3),
      ).not.toThrow();
    } finally {
      journal.close();
    }
  });
});

describe("a rewound Open or terminal step restores its header's workflow", () => {
  it.each([
    ["terminal Remove", "remove"],
    ["Open", "open"],
  ] as const)(
    "brings the workflow back for a rewound %s whose row an older release deleted",
    (_label, rewound) => {
      const database = path();
      const journal = openAvailabilityOperationJournal(database);
      try {
        const lease = journal.acquire("actor", "owner", 0, 1_000);
        journal.persist(lease, intent("open"), 1);
        journal.persist(
          lease,
          intent("remove", {
            action: "remove",
            completesWorkflow: true,
            spentOutRefs: ["open#1"],
          }),
          2,
        );
        journal.transition(lease, "open", "confirmed", "10:aa", null, 3);
        journal.transition(lease, "remove", "confirmed", "20:bb", null, 4);
        // Schema 2 deleted the row at the terminal's confirmation.
        const legacy = new DatabaseSync(database);
        legacy.exec("DELETE FROM availability_operation_workflows");
        legacy.close();
        expect(() =>
          journal.assertWorkflow(lease, "other-deployment", "h", "prepare", 5),
        ).not.toThrow();
        journal.rewind(lease, rewound, "rolled back", 6);
        expect(journal.workflows("actor")).toMatchObject([
          { headerHash: "header" },
        ]);
        expect(() =>
          journal.assertWorkflow(lease, "other-deployment", "h2", "prepare", 7),
        ).toThrow(/capital belongs/);
      } finally {
        journal.close();
      }
    },
  );
});

it("releases a never-landed Open's capital at expiry while retaining its progress", () => {
  const journal = openAvailabilityOperationJournal(path());
  try {
    const lease = journal.acquire("actor", "owner", 0, 1_000);
    journal.persist(lease, intent("open"), 1);
    journal.transition(lease, "open", "expired", null, "past TTL, unspent", 2);
    expect(journal.get("open")?.state).toBe("expired");
    expect(journal.reservedOutRefs("actor")).toEqual([]);
    expect(journal.workflows("actor")).toEqual([]);
    expect(() =>
      journal.assertWorkflow(lease, "other-deployment", "h", "open", 3),
    ).not.toThrow();
  } finally {
    journal.close();
  }
});

describe("a redeploy", () => {
  it("lists confirmed anchors in every deployment of the actor", () => {
    const journal = openAvailabilityOperationJournal(path());
    try {
      const lease = journal.acquire("actor", "owner", 0, 1_000);
      journal.persist(
        lease,
        intent("old", { action: "publish", deploymentIdentity: "old" }),
        1,
      );
      journal.persist(lease, intent("new", { action: "publish" }), 2);
      journal.transition(lease, "old", "confirmed", "10:aa", null, 3);
      journal.transition(lease, "new", "confirmed", "11:bb", null, 4);
      expect(
        journal.finalizedAnchors("actor").map(({ intent }) => intent.id),
      ).toEqual(["new", "old"]);
      expect(journal.finalizedAnchors("other")).toEqual([]);
    } finally {
      journal.close();
    }
  });
});

describe("lookups by transaction hash", () => {
  it("are served by an index, not a table scan", () => {
    const database = path();
    openAvailabilityOperationJournal(database).close();
    const db = new DatabaseSync(database);
    try {
      const plan = db
        .prepare(
          "EXPLAIN QUERY PLAN SELECT record FROM availability_operation_intents WHERE tx_hash = ? LIMIT 1",
        )
        .all("ab")
        .map((row) => String(row.detail));
      expect(plan.join("\n")).toMatch(/USING INDEX/u);
    } finally {
      db.close();
    }
  });
});
