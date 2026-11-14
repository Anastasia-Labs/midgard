import { afterEach, describe, expect, it } from "vitest";

import {
  advance,
  type Chain,
  closeJournals,
  fund,
  intake,
  openJournal,
  pass,
  recorded,
  spendElsewhere,
  stored,
  tipObservation,
} from "../support/availability-pruned-journal.js";
import { harness, K, SEED } from "../support/l1-follower-raw-reads-fixture.js";

/**
 * A signed intent that never lands, reconciled against the actor's wallet
 * coins as the follower holds them. The journal, the follower reads and the
 * reconciliation pass are the real ones.
 */

afterEach(closeJournals);

/** Moves the tip `blocks` blocks on without pruning. */
const forward = async (fx: Chain, blocks: number): Promise<void> => {
  for (let i = 0; i < blocks; i += 1) await fx.forward([]);
};

// The wallet is tracked from the follower's origin, so a coin it already
// held then is a seed row: unspent, with no creating transaction stored.
// The collateral is the likeliest such coin: it is reused, and spent only
// when a landing fails.
describe("a signed intent spending a wallet coin from before the follower's origin", () => {
  it.each(["collateral", "normal input"] as const)(
    "reads one whose %s is such a coin as unspent, and expires it past its validity",
    async (place) => {
      const fx = await harness();
      const journal = openJournal();
      const [coin] = await fund(fx, 1);
      const ttl = Number(fx.tipPoint().slot) + 4;
      const { intent } = recorded(fx, journal, "prepare", {
        inputs: [place === "collateral" ? coin! : SEED.outRef],
        collaterals: [place === "collateral" ? SEED.outRef : coin!],
        invalidAfter: ttl,
      });
      await expect(
        intake(fx).operation(tipObservation(fx), intent),
      ).resolves.toEqual({
        status: "unspent",
        currentSlot: Number(fx.tipPoint().slot),
      });
      await forward(fx, 6);
      expect(Number(fx.tipPoint().slot)).toBeGreaterThanOrEqual(ttl);
      expect(await pass(fx, journal)).toMatchObject([
        { status: "expired", txHash: intent.txHash },
      ]);
      expect(journal.get(intent.id)).toMatchObject({
        state: "expired",
        detail: "Expired with every normal input canonically unspent",
      });
    },
  );

  it("expires one whose collateral is such a coin on a store that has pruned past it", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input] = await fund(fx, 1);
    const ttl = Number(fx.tipPoint().slot) + 4;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [SEED.outRef],
      invalidAfter: ttl,
    });
    await advance(fx, K + 4);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail: "Expired with every normal input canonically unspent",
    });
  });
});

// A collateral spent by someone else proves nothing while the intent is
// still valid: the same bytes can land once the actor's collateral is
// settled. Only past its validity does it never land.
describe("a signed intent whose collateral someone else spent", () => {
  it("waits on it while it is valid, and expires it once its validity ends", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input, collateral] = await fund(fx, 2);
    await spendElsewhere(fx, collateral!);
    const ttl = Number(fx.tipPoint().slot) + 6;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [collateral!],
      invalidAfter: ttl,
    });
    expect(await pass(fx, journal)).toMatchObject([
      { status: "waiting", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)?.state).toBe("pending");
    await forward(fx, 6);
    expect(Number(fx.tipPoint().slot)).toBeGreaterThanOrEqual(ttl);
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail: "Expired with every normal input canonically unspent",
    });
  });
});
