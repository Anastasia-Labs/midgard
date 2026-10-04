import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  type AvailabilityOperationIntent,
  openAvailabilityOperationJournal,
} from "../src/availability-operation-journal.js";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "../src/deployment-manifest-identity.js";

const RECOVERY = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
let directory: string;
let journal: ReturnType<typeof openAvailabilityOperationJournal>;
let lease: ReturnType<typeof journal.acquire>;
beforeEach(() => {
  directory = mkdtempSync(join(tmpdir(), "availability-journal-pruning-"));
  journal = openAvailabilityOperationJournal(join(directory, "journal.sqlite"));
  lease = journal.acquire("actor", "owner", 0, 10_000);
});
afterEach(() => {
  journal.close();
  rmSync(directory, { recursive: true, force: true });
});
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
  collateralOutRefs: [],
  expectedOutRefs: [`${id}#0`],
  validUntilSlot: 100,
  completesWorkflow: false,
  ...overrides,
});
const confirm = (record: AvailabilityOperationIntent, blockNo: number) => {
  journal.persist(lease, record, 1);
  journal.transition(lease, record.id, "confirmed", `${blockNo}:aa`, null, 2);
  journal.retire(
    lease,
    record.id,
    { confirmationDepth: 10, currentBlockNo: blockNo, recoveryDepth: RECOVERY },
    3,
  );
};
const retire = (id: string, blockNo: number, inclusionPoint?: string) =>
  journal.retire(
    lease,
    id,
    {
      confirmationDepth: 10,
      currentBlockNo: blockNo,
      recoveryDepth: RECOVERY,
      inclusionPoint,
    },
    4,
  );

describe("journal history pruning under continuous activity", () => {
  it("prunes an old ancestor independently while a recent descendant stays confirmed", () => {
    confirm(intent("parent"), 100);
    confirm(intent("child", { spentOutRefs: ["parent#0"] }), 100 + RECOVERY);
    expect(journal.get("parent")?.state).toBe("confirmed");
    retire("child", 101 + RECOVERY);
    expect(journal.get("parent")).toBeNull();
    expect(journal.get("child")?.state).toBe("confirmed");
    expect(
      journal
        .finalizedAnchors("deployment", "actor")
        .map(({ intent }) => intent.id),
    ).toEqual(["child"]);
  });

  it("retains an older Open while its own terminal step can roll back", () => {
    confirm(intent("open", { action: "open" }), 100);
    confirm(
      intent("remove", {
        completesWorkflow: true,
        action: "remove",
        spentOutRefs: ["open#0"],
      }),
      100 + RECOVERY,
    );
    retire("remove", 101 + RECOVERY);
    expect(journal.get("open")?.state).toBe("confirmed");
    journal.rewind(lease, "remove", "rolled back", 5);
    expect(
      journal
        .workflows("actor")[0]
        ?.confirmedOpens.map(({ intent }) => intent.id),
    ).toEqual(["open"]);
    expect(() =>
      journal.assertWorkflow(lease, "other", "h", "open", 6),
    ).toThrow(/capital belongs/);
  });

  it("restarts ancestry retention when a confirmed descendant re-includes at another point", () => {
    confirm(intent("parent"), 100);
    confirm(intent("child", { spentOutRefs: ["parent#0"] }), 110);
    retire("child", 101 + RECOVERY, "new-child-block");
    expect(journal.get("parent")).toMatchObject({
      state: "confirmed",
      retentionBlockNo: 101 + RECOVERY,
    });
    expect(journal.get("child")).toMatchObject({
      state: "confirmed",
      inclusionPoint: "new-child-block",
    });
  });

  it("refreshes re-inclusion evidence and restarts its conservative retention clock", () => {
    confirm(intent("publish"), 100);
    retire("publish", 200, "200:bb");
    expect(journal.get("publish")).toMatchObject({
      inclusionPoint: "200:bb",
      retentionBlockNo: 200,
    });
    retire("publish", 101 + RECOVERY, "200:bb");
    expect(journal.get("publish")?.state).toBe("confirmed");
    retire("publish", 201 + RECOVERY, "200:bb");
    expect(journal.get("publish")).toBeNull();
  });
});

describe("expired progress uses authenticated block depth", () => {
  it("retains progress through the inclusive recovery horizon without holding expired input resources", () => {
    journal.persist(lease, intent("expired", { action: "open" }), 1);
    journal.transition(
      lease,
      "expired",
      "expired",
      null,
      "past TTL, unspent",
      2,
    );
    journal.pruneExpired(lease, 100, RECOVERY, 3);
    expect(journal.reservedOutRefs("actor")).toEqual([]);
    expect(journal.workflows("actor")).toEqual([]);
    expect(() =>
      journal.assertWorkflow(lease, "other", "h", "open", 4),
    ).not.toThrow();
    journal.pruneExpired(lease, 100 + RECOVERY, RECOVERY, 5);
    expect(journal.get("expired")?.state).toBe("expired");
    journal.pruneExpired(lease, 101 + RECOVERY, RECOVERY, 6);
    expect(journal.get("expired")).toBeNull();
  });

  it("keeps expiry evidence needed by an unresolved child, then prunes after that child expires", () => {
    journal.persist(lease, intent("parent"), 1);
    journal.persist(lease, intent("child", { spentOutRefs: ["parent#0"] }), 1);
    journal.transition(
      lease,
      "parent",
      "expired",
      null,
      "past TTL, unspent",
      2,
    );
    journal.pruneExpired(lease, 100, RECOVERY, 3);
    journal.pruneExpired(lease, 101 + RECOVERY, RECOVERY, 4);
    expect(journal.get("parent")?.state).toBe("expired");
    journal.transition(lease, "child", "expired", null, "expired parent", 5);
    journal.pruneExpired(lease, 101 + RECOVERY, RECOVERY, 6);
    expect(journal.get("parent")).toBeNull();
    expect(journal.get("child")?.state).toBe("expired");
  });

  it("fences expiry pruning with the same actor lease as every other journal mutation", () => {
    journal.release(lease);
    expect(() => journal.pruneExpired(lease, 100, RECOVERY, 3)).toThrow(
      /expired or superseded/,
    );
    const foreign = journal.acquire("other", "owner", 3, 100);
    journal.persist(
      (lease = journal.acquire("actor", "new-owner", 3, 100)),
      intent("expired"),
      4,
    );
    journal.transition(lease, "expired", "expired", null, "past TTL", 5);
    journal.pruneExpired(foreign, 100, RECOVERY, 6);
    expect(journal.get("expired")?.retentionBlockNo).toBeUndefined();
  });
});
