import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
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

  it("keeps future capital assigned to the opened challenge through timeout continuation and finality", () => {
    const journal = openAvailabilityOperationJournal(path());
    try {
      const lease = journal.acquire("actor", "owner", 0, 100);
      journal.persist(lease, { ...intent("open"), action: "open" }, 1);
      journal.transition(lease, "open", "included", "block1", null, 2);
      expect(() =>
        journal.assertWorkflow(
          lease,
          "deployment",
          "other-header",
          "prepare",
          3,
        ),
      ).toThrow(/capital belongs/);
      expect(() =>
        journal.assertWorkflow(
          lease,
          "other-deployment",
          "header",
          "publish",
          3,
        ),
      ).toThrow(/capital belongs/);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "header", "prepare", 3),
      ).toThrow(/capital belongs/);
      journal.persist(
        lease,
        { ...intent("timeout"), action: "timeout", spentOutRefs: ["open#1"] },
        4,
      );
      journal.transition(lease, "timeout", "confirmed", "block2", null, 5);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "other-header", "open", 6),
      ).toThrow(/capital belongs/);
      journal.persist(
        lease,
        {
          ...intent("remove"),
          action: "remove",
          completesWorkflow: true,
          spentOutRefs: ["timeout#1"],
        },
        7,
      );
      journal.transition(lease, "remove", "included", "block3", null, 8);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "other-header", "open", 9),
      ).toThrow(/capital belongs/);
      journal.transition(lease, "remove", "confirmed", "block3", null, 10);
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "other-header", "open", 11),
      ).not.toThrow();
      expect(
        journal
          .finalizedAnchors("deployment", "actor")
          .map((record) => record.intent.id),
      ).toEqual(["remove"]);
    } finally {
      journal.close();
    }
  });
});
