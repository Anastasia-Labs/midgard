/**
 * The node-ledger tool adapter tells whether a view point it answered is
 * still on the node's chain, from whether the node can acquire it: a point
 * the node refuses as not on its chain is off it, and one too old to acquire
 * is immutable, so on it.
 */
import { TransportRequestError } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import { describe, expect, it } from "vitest";

import { nodeLedgerAccessOver } from "../src/services/l1-node-ledger-access.js";

const ON_CHAIN = { slot: 30, id: "aa".repeat(32) };
const DROPPED = { slot: 40, id: "bb".repeat(32) };
const IMMUTABLE = { slot: 10, id: "cc".repeat(32) };

describe("the node-ledger adapter's ancestry read", () => {
  it("says a point the node refuses as not on its chain is off it, and an acquirable or immutable one on it", async () => {
    const access = nodeLedgerAccessOver(
      fakeL1NodeTransport({
        withLedgerState: async (at, use) => {
          if (at === "tip" || at.kind !== "point")
            throw new Error("only points are acquired here");
          if (at.hash === DROPPED.id)
            throw new TransportRequestError(
              "acquire_point_not_on_chain",
              "a rollback dropped it",
            );
          if (at.hash === IMMUTABLE.id)
            throw new TransportRequestError(
              "acquire_point_too_old",
              "it lies past the volatile window",
            );
          return use({
            query: async () => {
              throw new Error("no query is made");
            },
          });
        },
      }),
      { endpoint: "fake", nodeBehindMaxMs: 60_000 },
    );

    expect(await access.pointOnChain?.(DROPPED)).toBe(false);
    expect(await access.pointOnChain?.(ON_CHAIN)).toBe(true);
    expect(await access.pointOnChain?.(IMMUTABLE)).toBe(true);
  });
});
