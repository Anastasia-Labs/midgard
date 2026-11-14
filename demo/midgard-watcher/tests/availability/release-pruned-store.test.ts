import { afterEach, describe, expect, it } from "vitest";

import {
  advance,
  closeJournals,
  openJournal,
  RECOVERY,
  stored,
  tipObservation,
  unpinned,
} from "../support/availability-pruned-journal.js";
import {
  burnNode,
  burnTx,
  continueNode,
  landOpen,
  outputRow,
  queueHeader,
  releasePass,
  workflowRows,
} from "../support/availability-release-pruned.js";
import { harness, K } from "../support/l1-follower-raw-reads-fixture.js";

/**
 * A released challenge workflow settles only once its terminal step is
 * deeper than automatic rollback recovery, far past the follower's retained
 * depth, and every release check walks from the Open's node output. Each
 * case below lets the store prune that walk's first rows while the row is
 * still open, and checks the row still settles, or stays live and reported
 * under a named reason when nothing retained proves its release.
 */

afterEach(closeJournals);

describe("a challenge workflow released against a pruned store", () => {
  it("settles a release found before the store pruned the Open's node output", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    const q0 = await landOpen(fx, journal, header);
    const terminal = await burnNode(fx, header);
    await fx.forward([]);
    expect(await releasePass(fx, journal)).toMatchObject({
      released: [{ headerHash: header, reason: "header-node-burned" }],
      deferred: [],
    });
    expect(workflowRows(journal)).toEqual({
      live: [],
      unsettled: [{ headerHash: header, txHash: terminal }],
    });
    await advance(fx, K + 4);
    expect(await outputRow(fx, q0)).toBe("beyond_retention");
    expect(await releasePass(fx, journal)).toEqual({
      released: [],
      deferred: [],
    });
    expect(workflowRows(journal).unsettled).toHaveLength(1);
    await advance(fx, RECOVERY);
    expect(await releasePass(fx, journal)).toEqual({
      released: [],
      deferred: [],
    });
    expect(workflowRows(journal)).toEqual({ live: [], unsettled: [] });
  });

  it("keeps the terminal it found at the store's edge, though the store would prune it", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    await landOpen(fx, journal, header);
    const terminal = await burnNode(fx, header);
    // Found one block before the burn leaves the retained depth: the next
    // prune step would take the burn and its block with it.
    await advance(fx, K - 1);
    expect(await releasePass(fx, journal)).toMatchObject({
      released: [{ headerHash: header, txHash: terminal }],
    });
    await advance(fx, K + 4);
    expect(await stored(fx, terminal)).toBe("ok");
    await advance(fx, RECOVERY);
    await releasePass(fx, journal);
    expect(workflowRows(journal)).toEqual({ live: [], unsettled: [] });
  });

  it("finds a terminal after the store pruned the Open's node output, from the header's queue history", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    const q0 = await landOpen(fx, journal, header);
    await continueNode(fx, header);
    await advance(fx, K + 4);
    expect(await outputRow(fx, q0)).toBe("beyond_retention");
    expect(await releasePass(fx, journal)).toEqual({
      released: [],
      deferred: [],
    });
    expect(workflowRows(journal).live).toEqual([header]);
    const terminal = await burnNode(fx, header);
    await fx.forward([]);
    expect(await releasePass(fx, journal)).toMatchObject({
      released: [
        {
          headerHash: header,
          reason: "header-node-burned",
          txHash: terminal,
        },
      ],
      deferred: [],
    });
    await advance(fx, RECOVERY + K);
    await releasePass(fx, journal);
    expect(workflowRows(journal)).toEqual({ live: [], unsettled: [] });
  });

  // The row's terminal is read from its recorded block only once the walk
  // from the Open can no longer be read. Here the burn's block was replaced
  // by a rollback, and the same burn landed again in another block at the
  // same slot, before the watcher looked again; by then the store has
  // pruned the walk. The recorded block is no longer canonical, so the
  // release must be undone rather than settled.
  it("revives a release whose recorded block a rollback replaced, once the store pruned the walk", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    const q0 = await landOpen(fx, journal, header);
    const burn = burnTx(fx, header);
    const { hashes, point: recordedBlock } = await fx.forward([burn]);
    const terminal = hashes[0]!;
    expect(await releasePass(fx, journal)).toMatchObject({
      released: [
        {
          headerHash: header,
          txHash: terminal,
          spendPoint: `${recordedBlock.slot}:${recordedBlock.blockHash}`,
        },
      ],
    });
    await fx.backward(2);
    await fx.forward([]);
    const { point: replacement } = await fx.forward([burn]);
    expect(replacement.slot).toBe(recordedBlock.slot);
    expect(replacement.blockHash).not.toBe(recordedBlock.blockHash);
    await advance(fx, K + 4);
    expect(await outputRow(fx, q0)).toBe("beyond_retention");
    expect(await stored(fx, terminal)).toBe("ok");
    expect(await releasePass(fx, journal)).toMatchObject({
      released: [],
      deferred: [
        {
          headerHash: header,
          detail: expect.stringContaining("no longer canonical"),
        },
        // Live again, the row is walked again, and nothing retained proves
        // its release: it stays live.
        { headerHash: header, reason: "release-evidence-beyond-retention" },
      ],
    });
    expect(workflowRows(journal)).toEqual({ live: [header], unsettled: [] });
  });

  // Read from its recorded block, a terminal is as deep as the SDK walk
  // counts it: blocks after its own, to the finalized point plus the
  // observation's finality depth less one. It settles only once that depth
  // passes automatic rollback recovery.
  it("settles a release read from its recorded block exactly past rollback recovery", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    const q0 = await landOpen(fx, journal, header);
    await burnNode(fx, header);
    const burnHeight = Number(fx.tipPoint().blockNo);
    await releasePass(fx, journal);
    await advance(fx, K + 4);
    expect(await outputRow(fx, q0)).toBe("beyond_retention");
    const finality = Number(tipObservation(fx).nativePoint.finalityDepth);
    const depth = () =>
      Number(fx.tipPoint().blockNo) + finality - 1 - burnHeight;
    await advance(fx, RECOVERY - depth());
    expect(depth()).toBe(RECOVERY);
    expect(await releasePass(fx, journal)).toEqual({
      released: [],
      deferred: [],
    });
    expect(workflowRows(journal).unsettled).toHaveLength(1);
    await advance(fx, 1);
    expect(await releasePass(fx, journal)).toEqual({
      released: [],
      deferred: [],
    });
    expect(workflowRows(journal)).toEqual({ live: [], unsettled: [] });
  });

  // A watcher from before the release pins recorded this release, and the
  // store pruned its terminal's block: nothing retained proves it final.
  it("keeps a release unsettled, under a named reason, once the store pruned its terminal's block", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    await landOpen(fx, journal, header);
    const terminal = await burnNode(fx, header);
    await fx.forward([]);
    await releasePass(fx, journal, unpinned);
    await advance(fx, K + 4);
    expect(await stored(fx, terminal)).toBe("beyond_retention");
    const deferral = {
      released: [],
      deferred: [
        { headerHash: header, reason: "release-evidence-beyond-retention" },
      ],
    };
    expect(await releasePass(fx, journal)).toMatchObject(deferral);
    await advance(fx, RECOVERY);
    expect(await releasePass(fx, journal)).toMatchObject(deferral);
    expect(workflowRows(journal)).toEqual({
      live: [],
      unsettled: [{ headerHash: header, txHash: terminal }],
    });
  });

  it("keeps a workflow live, under a named reason, while nothing retained proves its release", async () => {
    const fx = await harness();
    const journal = openJournal();
    const header = await queueHeader(fx);
    const q0 = await landOpen(fx, journal, header);
    const terminal = await burnNode(fx, header);
    await advance(fx, K + 4);
    expect(await outputRow(fx, q0)).toBe("beyond_retention");
    expect(await stored(fx, terminal)).toBe("beyond_retention");
    const deferral = {
      released: [],
      deferred: [
        {
          headerHash: header,
          reason: "release-evidence-beyond-retention",
          detail: expect.any(String),
        },
      ],
    };
    expect(await releasePass(fx, journal)).toMatchObject(deferral);
    await advance(fx, RECOVERY);
    expect(await releasePass(fx, journal)).toMatchObject(deferral);
    expect(workflowRows(journal)).toEqual({ live: [header], unsettled: [] });
  });
});
