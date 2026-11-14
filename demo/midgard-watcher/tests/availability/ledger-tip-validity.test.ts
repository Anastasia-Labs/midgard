import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { fixture, utxo } from "../support/availability-challenge-fixture.js";
import {
  actions,
  ADA,
  io,
  liveChallenge,
  observation,
  OPENING,
  openLanded,
  runtime,
  timedOut,
  TIMEOUT_COLLATERAL,
  withheld,
} from "./concurrent-challenges.fixture.js";

// The fixture's Lucid has one-second slots from the Unix epoch; its ledger tip
// trails the clock by `io.tipLagMs`. The ledger judges a lower bound against
// the slot after its tip, and refuses one past it, so every lower bound must
// sit at least the submit margin (2 slots) behind the observed tip.
const SUBMIT_MARGIN_SLOTS = 2;
const slotOf = (unixTimeMs: bigint) => Number(unixTimeMs / 1000n);
const tipSlotNow = () => Math.floor((Date.now() - io.tipLagMs) / 1000);

/** `snapshot` with its challenge record's response deadline at `deadline`. */
const respondBy = (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  deadline: bigint,
): SDK.DaAvailabilityChallengeSnapshot => ({
  ...snapshot,
  recordDatum: { ...snapshot.recordDatum!, response_deadline: deadline },
});

describe("availability validity bounds from a ledger tip trailing the clock", () => {
  it("takes an Open's lower bound from the ledger tip, not the clock", async () => {
    io.tipLagMs = 40_000;
    const tipSlot = tipSlotNow();
    const header = withheld("47");
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "e1"),
      utxo(1, OPENING, "e2"),
      utxo(2, 100n * ADA, "e2"),
    ];
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(watcher.status()).toMatchObject({ action: "open" });
    } finally {
      await watcher.close();
    }
    expect(io.opens).toHaveLength(1);
    const [{ validFrom, validTo }] = io.opens as [
      { validFrom: bigint; validTo: bigint },
    ];
    expect(slotOf(validFrom) + SUBMIT_MARGIN_SLOTS).toBeLessThanOrEqual(
      tipSlot,
    );
    expect(validTo).toBeGreaterThan(BigInt(Date.now()));
  });

  it("defers a Timeout whose response deadline the ledger tip has not passed, then builds it once the tip catches up", async () => {
    io.tipLagMs = 40_000;
    const deadline = BigInt(Date.now()) - 20_000n;
    const expired = fixture("44", BigInt(Date.now()));
    openLanded(expired.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "c1"), utxo(1, 10n * ADA, "c2")];
    io.tipPool = { ...utxo(9, 100_005n * ADA, "c9"), address: "pool" };
    const state = observation([
      respondBy(timedOut(expired.challenged), deadline),
    ]);
    const watcher = await runtime();
    try {
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({
        stepsDeferred: [
          {
            headerHash: expired.challenged.headerHash,
            action: "timeout",
            reason: "ledger-tip-behind",
            detail: expect.stringContaining("L1 ledger tip"),
          },
        ],
      });
      expect(io.timeouts).toEqual([]);
      expect(actions()).toEqual([]);

      io.tipLagMs = 0;
      const tipSlot = tipSlotNow();
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "timeout" });
      expect(watcher.status().stepsDeferred).toBeUndefined();
      expect(io.timeouts).toHaveLength(1);
      const [{ validFrom }] = io.timeouts as unknown as [{ validFrom: bigint }];
      // At or past the deadline the correction lock's acquire requires, and
      // admissible at the observed tip.
      expect(validFrom).toBeGreaterThanOrEqual(deadline);
      expect(slotOf(validFrom) + SUBMIT_MARGIN_SLOTS).toBeLessThanOrEqual(
        tipSlot,
      );
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[expired.challenged.headerHash, "timeout"]]);
  });

  it("defers settling an expired tranche until the ledger tip passes its response deadline", async () => {
    io.tipLagMs = 40_000;
    const live = liveChallenge("45");
    openLanded(live.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "d1"), utxo(1, 10n * ADA, "d2")];
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([respondBy(live.challenged, BigInt(Date.now()) - 20_000n)]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        stepsDeferred: [
          {
            headerHash: live.challenged.headerHash,
            action: "settle",
            reason: "ledger-tip-behind",
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([]);
  });
});
