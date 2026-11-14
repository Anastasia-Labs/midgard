import { existsSync } from "node:fs";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import { StackQueueTailUnattestedError } from "./stack-journey-da.js";
import { verifiedPredecessorBeforeFamily } from "./stack-verified-predecessor.before-family.js";
import {
  ensureStackVerifiedPredecessor,
  StackVerifiedPredecessorRefusal,
} from "./stack-verified-predecessor.js";
import {
  ABSORB_WITHIN_MS,
  DECISION_WITHIN_MS,
  drillDirectories,
  verifiedPredecessorDrill,
} from "./stack-verified-predecessor.test-support.js";

afterAll(() =>
  Promise.all(
    drillDirectories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);

const refusal = (reason: string, headerHash: string) =>
  expect.objectContaining({
    name: "StackVerifiedPredecessorRefusal",
    reason,
    headerHash,
  });

describe("the stack's verified predecessor before a family", () => {
  it("does nothing when the watcher verified the tail from its payload", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "verified");
    await expect(
      ensureStackVerifiedPredecessor(drill.ports()),
    ).resolves.toEqual({ kind: "verified", block: drill.rootBlock });
    expect(drill.node.submitted).toEqual([]);
    expect(drill.pauses()).toBe(0);
    expect(drill.standings).toEqual([]);
    // One read of the watcher's diagnostics, from its first record.
    expect(drill.diagnosticsPaths).toEqual([
      "/v1/diagnostics?kind=verification&limit=100&cursor=0",
    ]);
  });

  it("reads the tail again after its first read loses its connection, and passes the verified tail", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "verified");
    drill.l1.loseQueueReads(1);
    await expect(
      ensureStackVerifiedPredecessor(drill.ports()),
    ).resolves.toEqual({ kind: "verified", block: drill.rootBlock });
    expect(drill.node.submitted).toEqual([]);
  });

  it("heals a root block the watcher released unverified with one transfer, then waits for its block to be attested and verified", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(drill.node.submitted).toHaveLength(1);
    expect(healed).toEqual({
      kind: "healed",
      from: drill.rootBlock,
      block: committed[0],
      transferTxId: drill.node.submitted[0],
    });
    // Saved under the tail it was built on, before it was sent.
    const saved = JSON.parse(
      await readFile(
        join(
          drill.directory,
          drill.rootBlock.headerHash,
          "successor-transfer.json",
        ),
        "utf8",
      ),
    ) as Record<string, unknown>;
    expect(saved).toMatchObject({
      predecessorHeaderHash: drill.rootBlock.headerHash,
      txId: drill.node.submitted[0],
    });
  });

  it("reads through our node's /tx-status answering 502 for a moment while it commits the transfer", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    drill.node.unavailableStatusReads = 2;
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(healed).toMatchObject({ kind: "healed", block: committed[0] });
    expect(drill.node.unavailableStatusReads).toBe(0);
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("heals a root block a restarted watcher holds no record of", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "verified");
    drill.watcher.restart();
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(healed).toMatchObject({ kind: "healed", block: committed[0] });
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("waits for the watcher to verify a live tail it has pending, and sends nothing", async () => {
    const drill = await verifiedPredecessorDrill();
    const tail = await drill.blockOn(drill.rootBlock.headerHash);
    drill.l1.commit(tail, true);
    drill.watcher.record(tail, "pending_da");
    drill.steps.push(
      () => {},
      () => drill.watcher.record(tail, "verified"),
    );
    await expect(
      ensureStackVerifiedPredecessor(drill.ports()),
    ).resolves.toEqual({ kind: "verified", block: tail });
    expect(drill.node.submitted).toEqual([]);
    expect(drill.pauses()).toBe(2);
  });

  it("heals a live tail that merges unverified while the watcher has it pending", async () => {
    const drill = await verifiedPredecessorDrill();
    const tail = await drill.blockOn(drill.rootBlock.headerHash);
    drill.l1.commit(tail, true);
    // A restarted watcher holds nothing for it, and it merges meanwhile.
    drill.steps.push(() => drill.l1.merge(tail.headerHash));
    const committed = drill.ourNodeCommits(() => tail.headerHash, "verified");
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(healed).toMatchObject({
      kind: "healed",
      from: tail,
      block: committed[0],
    });
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("refuses by name, before any L1 work, a tail the watcher decided is faulty", async () => {
    const drill = await verifiedPredecessorDrill();
    const tail = await drill.blockOn(drill.rootBlock.headerHash);
    drill.l1.commit(tail, true);
    drill.watcher.record(tail, "fault_detected");
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toEqual(
      refusal("tail-not-healthy", tail.headerHash),
    );
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toThrow(
      /fault_detected/u,
    );
    expect(drill.node.submitted).toEqual([]);
    expect(drill.standings).toEqual([]);
    expect(existsSync(join(drill.directory, tail.headerHash))).toBe(false);
  });

  it("refuses by name a healed block that ends released unverified, and sends no second transfer", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "unverified_merged",
    );
    const checking = ensureStackVerifiedPredecessor(drill.ports());
    await expect(checking).rejects.toBeInstanceOf(
      StackVerifiedPredecessorRefusal,
    );
    await expect(checking).rejects.toEqual(
      refusal("healed-block-unverified", committed[0]!.headerHash),
    );
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("refuses by name a live tail the watcher does not decide within its decision allowance", async () => {
    const drill = await verifiedPredecessorDrill();
    const tail = await drill.blockOn(drill.rootBlock.headerHash);
    drill.l1.commit(tail, true);
    drill.watcher.record(tail, "pending_l1");
    const startedAt = drill.now();
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toEqual(
      refusal("watcher-undecided", tail.headerHash),
    );
    expect(drill.now() - startedAt).toBeGreaterThan(DECISION_WITHIN_MS);
    expect(drill.node.submitted).toEqual([]);
  });

  it("refuses by name a transfer our node does not commit within the absorption bound", async () => {
    const drill = await verifiedPredecessorDrill();
    const startedAt = drill.now();
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toEqual(
      refusal("transfer-not-committed", drill.rootBlock.headerHash),
    );
    expect(drill.now() - startedAt).toBeGreaterThan(ABSORB_WITHIN_MS);
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("refuses by name a committed transfer whose block does not reach L1 within the absorption bound", async () => {
    const drill = await verifiedPredecessorDrill();
    const ours = await drill.blockOn(drill.rootBlock.headerHash);
    drill.node.onSubmit = (txId) => {
      drill.node.statuses.set(txId, {
        status: "committed",
        headerHash: ours.headerHash,
      });
    };
    const startedAt = drill.now();
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toEqual(
      refusal("transfer-block-not-on-l1", ours.headerHash),
    );
    expect(drill.now() - startedAt).toBeGreaterThan(ABSORB_WITHIN_MS);
  });

  it("refuses by name, through the attested-tail reader, a healed block never attested", async () => {
    const drill = await verifiedPredecessorDrill();
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      undefined,
      { attests: false },
    );
    const checking = ensureStackVerifiedPredecessor(drill.ports());
    await expect(checking).rejects.toBeInstanceOf(
      StackQueueTailUnattestedError,
    );
    await expect(checking).rejects.toMatchObject({
      headerHash: committed[0]!.headerHash,
    });
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("refuses by name a healed block the watcher does not decide within its decision allowance", async () => {
    const drill = await verifiedPredecessorDrill();
    const committed = drill.ourNodeCommits(() => drill.rootBlock.headerHash);
    const checking = ensureStackVerifiedPredecessor(drill.ports());
    await expect(checking).rejects.toBeInstanceOf(
      StackVerifiedPredecessorRefusal,
    );
    await expect(checking).rejects.toEqual(
      refusal("watcher-undecided", committed[0]!.headerHash),
    );
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("resends the saved transfer after a restart lost its send, and builds no new one", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    drill.node.loseNextResponse = true;
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toThrow(
      /response was lost/u,
    );
    expect(drill.node.submitted).toHaveLength(1);
    // Our node's ledger no longer shows the owner's output: a new build
    // would fail, so the restarted check must send what it saved.
    drill.node.ownerHolds = false;
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(healed).toMatchObject({ kind: "healed", block: committed[0] });
    expect(drill.node.submitted).toHaveLength(2);
    expect(drill.node.submitted[1]).toBe(drill.node.submitted[0]);
  });

  it("resends the same saved transfer when our node, having known it, later answers not_found, and builds no new one", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    const commitsIt = drill.node.onSubmit!;
    drill.node.onSubmit = (txId) => {
      drill.node.onSubmit = commitsIt;
      // Our node knows it now; a pause later it has forgotten it. Its ledger
      // no longer shows the owner's output, so a new build would fail.
      drill.node.ownerHolds = false;
      drill.steps.push(() => {
        drill.node.statuses.delete(txId);
      });
    };
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(healed).toMatchObject({ kind: "healed", block: committed[0] });
    expect(drill.node.submitted).toHaveLength(2);
    expect(drill.node.submitted[1]).toBe(drill.node.submitted[0]);
  });

  it("waits while a lagging queue read still returns the old tail, then takes the transfer's block", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    drill.l1.lag(5_000);
    const healed = await ensureStackVerifiedPredecessor(drill.ports());
    expect(healed).toEqual({
      kind: "healed",
      from: drill.rootBlock,
      block: committed[0],
      transferTxId: drill.node.submitted[0],
    });
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("refuses by name a healed block the queue read does not show within the absorption bound, and sends no second transfer", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "unverified_merged");
    const committed = drill.ourNodeCommits(
      () => drill.rootBlock.headerHash,
      "verified",
    );
    drill.l1.lag(ABSORB_WITHIN_MS * 2);
    const startedAt = drill.now();
    const checking = ensureStackVerifiedPredecessor(drill.ports());
    await expect(checking).rejects.toBeInstanceOf(
      StackVerifiedPredecessorRefusal,
    );
    await expect(checking).rejects.toEqual(
      refusal("healed-block-not-in-queue", committed[0]!.headerHash),
    );
    expect(drill.now() - startedAt).toBeGreaterThan(ABSORB_WITHIN_MS);
    expect(drill.node.submitted).toHaveLength(1);
  });

  it("does not accept a verified record from another payload envelope", async () => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "verified", "ff".repeat(32));
    await expect(ensureStackVerifiedPredecessor(drill.ports())).rejects.toEqual(
      refusal("payload-mismatch", drill.rootBlock.headerHash),
    );
    expect(drill.node.submitted).toEqual([]);
  });
});

describe("the check before each family", () => {
  const family = async (files: Record<string, unknown> = {}) => {
    const directory = await mkdtemp(join(tmpdir(), "verified-predecessor-"));
    drillDirectories.push(directory);
    for (const [name, contents] of Object.entries(files)) {
      await mkdir(dirname(join(directory, name)), { recursive: true });
      await writeFile(join(directory, name), JSON.stringify(contents));
    }
    let closed = false;
    const calls = { checks: 0, closes: 0 };
    const run = (options: { sessionClosed?: boolean; fails?: boolean } = {}) =>
      verifiedPredecessorBeforeFamily({
        familyDirectory: directory,
        isClosed: () => closed || options.sessionClosed === true,
        close: () => {
          calls.closes += 1;
          closed = true;
          return Promise.resolve();
        },
        check: () => {
          calls.checks += 1;
          return options.fails === true
            ? Promise.reject(new Error("refused"))
            : Promise.resolve();
        },
        onStage: () => {},
      });
    return { run, calls };
  };
  const draft = { deploymentFingerprint: "f", predecessor: {}, current: {} };

  it("checks a family that has not started staging", async () => {
    const { run, calls } = await family();
    await expect(run()).resolves.toBe("checked");
    expect(calls.checks).toBe(1);
  });

  it("checks a family whose directory holds only what is written before any fault work", async () => {
    const { run, calls } = await family({
      "workflow-baseline.json": {},
      "timing-plan.json": {},
      "timings.ndjson": {},
      "failure.json": {},
      "chain-reads.ndjson": {},
      "late-shift.json": {},
      "successor-transfer-deferred.json": {},
      "archive/staged-superseded-2026-11-13T00-00-00-000Z.json": {},
    });
    await expect(run()).resolves.toBe("checked");
    expect(calls.checks).toBe(1);
  });

  it("checks a family whose staged fault is an unsigned draft, which staging's own recovery supersedes", async () => {
    const { run, calls } = await family({ "staged.json": draft });
    await expect(run()).resolves.toBe("checked");
    expect(calls.checks).toBe(1);
  });

  it("skips a family resuming with its signed or committed fault, its trace deposit, or its history", async () => {
    for (const files of [
      { "staged.json": { ...draft, signedCommit: { txHash: "aa" } } },
      { "staged.json": { ...draft, commitTxHash: "aa" } },
      { "staged.json": { ...draft, depositEvent: {}, commits: [] } },
      { "history-forced-order.json": {} },
    ]) {
      const { run, calls } = await family(files);
      await expect(run()).resolves.toBe("staging-resumed");
      expect(calls.checks).toBe(0);
    }
  });

  it("skips a family whose preparation saved its L1 deposit, before any staged fault", async () => {
    const { run, calls } = await family({
      "timing-plan.json": {},
      "deposit.json": { outcome: "submitted" },
    });
    await expect(run()).resolves.toBe("staging-resumed");
    expect(calls.checks).toBe(0);
  });

  it("skips a family whose preparation saved its L1 forced order, before any staged fault", async () => {
    const { run, calls } = await family({
      "timing-plan.json": {},
      "forced-order.json": { outcome: "submitted" },
    });
    await expect(run()).resolves.toBe("staging-resumed");
    expect(calls.checks).toBe(0);
  });

  it("does nothing once the shared session has closed", async () => {
    const { run, calls } = await family();
    await expect(run({ sessionClosed: true })).resolves.toBe("session-closed");
    expect(calls.checks).toBe(0);
  });

  it("closes the shared session when the check refuses, so no later family sends anything", async () => {
    const { run, calls } = await family();
    await expect(run({ fails: true })).rejects.toThrow("refused");
    expect(calls.closes).toBe(1);
    await expect(run()).resolves.toBe("session-closed");
    expect(calls.checks).toBe(1);
  });
});
