import { describe, expect, it } from "vitest";

import {
  fixture,
  harness,
  K,
  okValue,
  pruneFixture,
  random,
  reasonOf,
  SEED_LABEL,
} from "../support/l1-follower-raw-reads-fixture.js";

/**
 * Which outrefs are unspent at a point, without their bytes. A seed row (an
 * output the follower's origin already held) has no stored creating body,
 * so a read of its bytes is refused, but it is unspent from the origin until
 * a spend the store records at or below the point.
 */
describe("follower outref liveness at a point", () => {
  it("reads a seed row as unspent until its recorded spend, while its bytes stay refused", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    const { a, b } = fx.hashes;
    expect(
      okValue(
        await reads.outRefLivenessAtPoint(
          [SEED_LABEL, `${a}#0`, `${a}#1`, `${b}#0`],
          fx.points.p1,
        ),
      ),
    ).toEqual({
      unspent: [SEED_LABEL, `${a}#0`],
      spends: [],
      unknown: [`${a}#1`, `${b}#0`],
      beyondRetention: [],
    });
    expect(
      reasonOf(await reads.utxosByOutRefAtPoint([SEED_LABEL], fx.points.p1)),
    ).toBe("l1_input_before_origin");
    expect(
      okValue(await reads.outRefLivenessAtPoint([SEED_LABEL], fx.points.p2)),
    ).toEqual({
      unspent: [],
      spends: [
        { outRef: SEED_LABEL, spendingTxHash: b, spendPoint: fx.points.p2 },
      ],
      unknown: [],
      beyondRetention: [],
    });
  });

  it("reads an unspent seed row as unspent once the store has pruned", async () => {
    const fx = await harness();
    for (let i = 0; i < K + 4; i += 1) await fx.forward([]);
    await fx.pruneAll();
    expect(reasonOf(await fx.reads().transactionInclusion(random(1)))).toBe(
      "beyond_retention",
    );
    expect(
      okValue(
        await fx.reads().outRefLivenessAtPoint([SEED_LABEL], fx.tipPoint()),
      ),
    ).toEqual({
      unspent: [SEED_LABEL],
      spends: [],
      unknown: [],
      beyondRetention: [],
    });
  });

  it("agrees with the byte read on every outref the byte read answers", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const { a, b, v } = fx.hashes;
    const outRefs = [`${a}#0`, `${random(5)}#0`, `${b}#0`, `${v}#7`];
    const bytes = okValue(
      await fx.reads().utxosByOutRefAtPoint(outRefs, fx.tipPoint()),
    );
    const { unspent, ...rest } = okValue(
      await fx.reads().outRefLivenessAtPoint(outRefs, fx.tipPoint()),
    );
    expect(unspent).toEqual(bytes.outputs.map(({ outRef }) => outRef));
    expect(rest).toEqual({
      spends: bytes.spends,
      unknown: bytes.unknown,
      beyondRetention: bytes.beyondRetention,
    });
  });
});
