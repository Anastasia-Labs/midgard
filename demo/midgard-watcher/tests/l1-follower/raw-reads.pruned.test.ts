import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  bech32,
  byOutRef,
  D,
  expected,
  fixture,
  okValue,
  pruneFixture,
  random,
  reasonOf,
  SEED_LABEL,
  T,
} from "../support/l1-follower-raw-reads-fixture.js";

/**
 * Behaviour of the follower-backed raw reads (ticket W1) once pruned, on a hand-built
 * simulator chain (`l1-follower-raw-reads-fixture.ts`). The oracle is the
 * simulator's own transaction specs: the exact bytes an output must read
 * back as come from the spec's encoding, never from the store.
 */

describe("follower raw reads after pruning: beyond_retention, never missing", () => {
  it("point reads at a pruned block are point_beyond_retention", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const reads = fx.reads();
    // b1's block row is pruned: every point read there is refused.
    const p1 = fx.points.p1;
    expect(reasonOf(await reads.addressUtxosAtPoint(bech32(T), p1))).toBe(
      "point_beyond_retention",
    );
    expect(reasonOf(await reads.utxosByOutRefAtPoint([], p1))).toBe(
      "point_beyond_retention",
    );
    expect(reasonOf(await reads.predecessorPoint(p1))).toBe(
      "point_beyond_retention",
    );
  });

  it("a block kept below the window is canonical; only its live set is refused", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const reads = fx.reads();
    // b3 stays (V's output is live), so the point is canonical, but the
    // live set around it is pruned and cannot be answered whole.
    const p3 = fx.points.p3;
    expect(reasonOf(await reads.addressUtxosAtPoint(bech32(T), p3))).toBe(
      "point_beyond_retention",
    );
    // Each outref is classed on its own row: A#0's spend was pruned.
    const { a } = fx.hashes;
    const read = okValue(await reads.utxosByOutRefAtPoint([`${a}#0`], p3));
    expect(read.beyondRetention).toEqual([`${a}#0`]);
    expect([read.outputs, read.spends, read.unknown]).toEqual([[], [], []]);
    expect(okValue(await reads.predecessorPoint(p3))).toEqual(fx.points.p2);
  });

  it("utxosByOutRefAtPoint tells pruned rows from provably unknown outrefs", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const reads = fx.reads();
    const { B } = fx.specs;
    const { a, b, v } = fx.hashes;
    const read = okValue(
      await reads.utxosByOutRefAtPoint(
        [`${a}#0`, `${a}#1`, `${random(5)}#0`, `${b}#0`, `${v}#7`, `${b}#1`],
        fx.tipPoint(),
      ),
    );
    expect(read.outputs).toEqual([expected(B, b, 0)]);
    // A was pruned: neither its spent tracked output nor its untracked one can be told.
    expect([...read.beyondRetention].sort()).toEqual(
      [`${a}#0`, `${a}#1`, `${random(5)}#0`].sort(),
    );
    // V and B are stored (live outputs): their bodies prove these never existed.
    expect([...read.unknown].sort()).toEqual([`${v}#7`, `${b}#1`].sort());
    expect(read.spends).toEqual([]);
  });

  it("unitHistoryAtPoint keeps a queued header's history; an empty answer is refused", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const reads = fx.reads();
    const unit = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${fx.header}`;
    expect(
      okValue(await reads.unitHistoryAtPoint(unit, fx.tipPoint())).transactions,
    ).toEqual([{ txHash: fx.hashes.commit, inclusionPoint: fx.points.p5 }]);
    const unknown = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"ab".repeat(28)}`;
    expect(
      reasonOf(await reads.unitHistoryAtPoint(unknown, fx.tipPoint())),
    ).toBe("beyond_retention");
  });

  it("transactionInclusion claims null only above the window", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    const reads = fx.reads();
    expect(okValue(await reads.transactionInclusion(fx.hashes.b))).toEqual(
      fx.points.p2,
    );
    expect(okValue(await reads.transactionInclusion(fx.hashes.commit))).toEqual(
      fx.points.p5,
    );
    for (const txHash of [fx.hashes.a, fx.hashes.u, random(7)]) {
      expect(reasonOf(await reads.transactionInclusion(txHash))).toBe(
        "beyond_retention",
      );
      expect(
        reasonOf(await reads.transactionInclusion(txHash, prunedThrough)),
      ).toBe("beyond_retention");
      expect(
        okValue(await reads.transactionInclusion(txHash, prunedThrough + 1)),
      ).toBeNull();
    }
  });

  it("rawTransaction refuses pruned bodies and pruned inputs as beyond_retention", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const { a, b, v } = fx.hashes;
    for (const reads of [fx.reads(), fx.reads(true)]) {
      expect(reasonOf(await reads.rawTransaction(a, fx.points.p1))).toBe(
        "beyond_retention",
      );
      expect(
        reasonOf(await reads.rawTransaction(random(9), fx.points.p1)),
      ).toBe("beyond_retention");
      // B is retained (its output is live); its inputs' bodies and the seed row are gone.
      const raw = okValue(await reads.rawTransaction(b, fx.points.p2));
      expect(raw.transaction.resolvedInputs).toEqual([]);
      expect(byOutRef(raw.unresolvedInputs)).toEqual(
        byOutRef([
          { outRef: `${a}#0`, reason: "beyond_retention" },
          { outRef: `${a}#1`, reason: "beyond_retention" },
          { outRef: SEED_LABEL, reason: "beyond_retention" },
        ]),
      );
      expect(raw.unresolvedReferenceInputs).toEqual([
        { outRef: `${a}#2`, reason: "beyond_retention" },
      ]);
      // b2 is no longer acquirable: U#0 cannot come from the ledger either.
      expect(
        okValue(await reads.rawTransaction(v, fx.points.p3)).unresolvedInputs,
      ).toEqual([{ outRef: `${fx.hashes.u}#0`, reason: "beyond_retention" }]);
    }
  });
});
