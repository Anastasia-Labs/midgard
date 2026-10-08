import type { FraudProofRawL1Snapshot } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  createWatcherFaultProofL1Source,
  WatcherFaultProofL1RefusedError,
} from "../../src/l1-follower/fault-proof-l1-source.js";
import {
  createWatcherProofRetention,
  type WatcherProofRetention,
  type WatcherProofUnitHoldResult,
} from "../../src/l1-follower/proof-retention.js";
import { fixture } from "../support/l1-follower-raw-reads-fixture.js";
import {
  nodeDouble,
  nodeUnit,
  RELEASE,
  requestFor,
  SOURCE_ID,
} from "../support/l1-follower-raw-source-fixture.js";
import { storelessProofRetention } from "../support/proof-retention.js";

/**
 * A capture keeps the result of holding its followed units: a history
 * pruning removed first is refused `beyond_retention`, never read
 * partially; a header no pin holds is a read for no open objective and
 * proceeds.
 */

const captureWith = async (proofRetention: WatcherProofRetention) => {
  const fx = await fixture();
  const request = requestFor(fx.header);
  const source = createWatcherFaultProofL1Source({
    store: fx.store,
    rawReads: fx.reads(),
    node: nodeDouble().node,
    sourceId: SOURCE_ID,
    proofRetention,
  });
  return {
    fx,
    capture: () =>
      source
        .snapshotAuthority({
          releaseFinality: RELEASE,
          observationDepth: "release_finality",
        })
        .capture(request) as Promise<FraudProofRawL1Snapshot>,
  };
};

const answering = (result: WatcherProofUnitHoldResult) => {
  const holds: (readonly [string, readonly string[]])[] = [];
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    holdUnits: async (headerHash, units) => {
      holds.push([headerHash, units]);
      return result;
    },
  };
  return { retention, holds };
};

describe("fault-proof L1 source: unit hold results", () => {
  it("captures once the units are held", async () => {
    const { retention, holds } = answering({ kind: "held" });
    const { fx, capture } = await captureWith(retention);
    expect((await capture()).history).toHaveLength(1);
    expect(holds).toEqual([[fx.header, [nodeUnit(fx.header)]]]);
  });

  it("captures a header no pin holds (a read for no open objective, as classification makes)", async () => {
    const { capture } = await captureWith(
      answering({ kind: "not_pinned" }).retention,
    );
    expect((await capture()).history).toHaveLength(1);
  });

  it("refuses beyond_retention a history pruning removed before the hold", async () => {
    const { capture } = await captureWith(
      answering({ kind: "already_pruned", units: ["ab".repeat(28)] }).retention,
    );
    const refused = capture();
    await expect(refused).rejects.toBeInstanceOf(
      WatcherFaultProofL1RefusedError,
    );
    await expect(refused).rejects.toMatchObject({
      reason: "beyond_retention",
      detail: expect.stringContaining("ab".repeat(28)) as unknown,
    });
  });

  it("over the follower's own retention: captures unpinned, and pinned", async () => {
    const fx = await fixture();
    const retention = createWatcherProofRetention(fx.store);
    const source = createWatcherFaultProofL1Source({
      store: fx.store,
      rawReads: fx.reads(),
      node: nodeDouble().node,
      sourceId: SOURCE_ID,
      proofRetention: retention,
    });
    const capture = () =>
      source
        .snapshotAuthority({
          releaseFinality: RELEASE,
          observationDepth: "release_finality",
        })
        .capture(requestFor(fx.header));
    await expect(capture()).resolves.toBeDefined();
    expect(retention.degradations()).toEqual([]);
    expect(
      await retention.pin({ category: "doubleSpend", headerHash: fx.header }),
    ).toEqual({ kind: "pinned" });
    await expect(capture()).resolves.toBeDefined();
    expect(retention.degradations()).toEqual([]);
  });
});
