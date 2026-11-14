import {
  buildPastProtectedPredecessor,
  ledgerClock,
} from "midgard-watcher/tests/support/history-predecessor-hold";
import { expect, it } from "vitest";

it("builds an event on the ledger clock only once the tip is fresh enough for its backdated lower bound", async () => {
  // A Preprod tip trails the wall clock by a block gap, which can exceed the
  // builders' sixty-second backdating.
  const wall = 1_000_000;
  let tip = wall - 90_000;
  const order: string[] = [];
  const chain = {
    now: () => wall,
    awaitLedgerTime: async (target: number) => {
      tip = Math.max(tip, target);
      order.push("tip");
    },
  };
  const built = await buildPastProtectedPredecessor(async () => {
    order.push("build");
    // The lower bound the builders take.
    expect(wall - 60_000).toBeLessThanOrEqual(tip);
    return "built";
  }, ledgerClock(chain));
  expect(built).toBe("built");
  expect(order).toEqual(["tip", "build"]);
});
