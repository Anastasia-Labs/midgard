import { describe, expect, it, vi } from "vitest";

import { fixture, utxo } from "../support/availability-challenge-fixture.js";
import {
  actions,
  ADA,
  io,
  observation,
  OPENING,
  openLanded,
  runtime,
  timedOut,
  TIMEOUT_COLLATERAL,
  withheld,
} from "./concurrent-challenges.fixture.js";

const NO_POOL = {
  onDaBondPool: () => undefined,
  onDaBondPoolReadFailure: () => undefined,
};

describe("an availability key the DA challenger allow-list omits (observe and prepare)", () => {
  it("prepares a due Open instead of building it, still times out a live challenge, and clears it once no longer due", async () => {
    const first = fixture("44", BigInt(Date.now()));
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "c1"),
      utxo(1, OPENING + 100n * ADA, "c2"),
    ];
    io.tipPool = { ...utxo(9, 100_005n * ADA, "c9"), address: "pool" };
    const onOpensPrepared = vi.fn();
    const watcher = await runtime(undefined, { ...NO_POOL, onOpensPrepared });
    try {
      await watcher.reconcile(
        observation([timedOut(first.challenged), second.attested]),
        true,
      );
      const prepared = [
        {
          headerHash: second.attested.headerHash,
          openDeadlineEpochMs: expect.any(Number),
        },
      ];
      expect(onOpensPrepared).toHaveBeenCalledExactlyOnceWith(prepared);
      expect(watcher.status()).toMatchObject({
        action: "timeout",
        openPrepared: prepared,
      });
      expect(watcher.status().openRefused).toBeUndefined();
      // Nothing about the withheld header was built: no prepare, no Open.
      expect(actions()).toEqual([[first.challenged.headerHash, "timeout"]]);

      // A listed challenger opened it: the Open is no longer due.
      onOpensPrepared.mockClear();
      await watcher.reconcile(observation([]), true);
      expect(onOpensPrepared).toHaveBeenCalledExactlyOnceWith([]);
      expect(watcher.status().openPrepared).toBeUndefined();
    } finally {
      await watcher.close();
    }
  });

  it("builds the Open itself when its key is listed", async () => {
    const second = withheld("45");
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "a1"),
      utxo(1, OPENING + 100n * ADA, "a2"),
    ];
    const watcher = await runtime(undefined, NO_POOL);
    try {
      await watcher.reconcile(observation([second.attested]), true);
      expect(watcher.status()).toMatchObject({ action: "prepare" });
      expect(watcher.status().openPrepared).toBeUndefined();
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[second.attested.headerHash, "prepare"]]);
  });
});
