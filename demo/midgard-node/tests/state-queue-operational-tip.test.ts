import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { describe, expect, it } from "vitest";

import {
  fetchTip,
  withRequestTimeout,
} from "../src/services/state-queue-correction-observer.decode-kupo-correction-lock-match.js";
import {
  h32,
  tipOnlySource,
} from "./state-queue-correction-observer.authenticated-fraud-transition.js";

// This utility still supplies operational availability transaction reconciliation.
// Canonical transition retirement instead uses the chain-sync ancestry reader.
describe("operational Ogmios tip reader", () => {
  it("binds the Ogmios block height to a tip read on both sides of it", async () => {
    // The first bracket straddles a tip change; the second agrees.
    const tips = [h32("8"), h32("9"), h32("9"), h32("9")];
    let tipReads = 0;
    const { readTip, methods } = tipOnlySource((method) =>
      method === "queryNetwork/tip" ? { id: tips[tipReads++], slot: 130 } : 119,
    );
    await expect(readTip()).resolves.toEqual({
      blockHash: h32("9"),
      slot: 130,
      blockNo: 119,
    });
    expect(methods).toEqual([
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
    ]);
  });

  it("refuses a tip that keeps moving across a bounded number of reads", async () => {
    const { readTip, methods } = tipOnlySource((method, call) =>
      method === "queryNetwork/tip"
        ? { id: h32(call.toString(16).slice(-1)), slot: 100 + call }
        : 119,
    );
    await expect(readTip()).rejects.toThrow(
      "Ogmios tip moved during each of 5 block height reads",
    );
    expect(methods).toHaveLength(15);
  });

  it.each([["origin"], [undefined], [-1], ["119"], [1.5]])(
    "fails closed on an invalid Ogmios block height %j",
    async (height) => {
      const { readTip } = tipOnlySource((method) =>
        method === "queryNetwork/tip" ? { id: h32("9"), slot: 130 } : height,
      );
      await expect(readTip()).rejects.toThrow(
        "Ogmios block height query returned no block height",
      );
    },
  );

  it("fails closed on an origin Ogmios tip", async () => {
    const { readTip } = tipOnlySource(() => "origin");
    await expect(readTip()).rejects.toThrow(
      "Ogmios tip query returned no canonical point",
    );
  });

  it("fails a read from an Ogmios that accepts the request and never answers", async () => {
    const server = createServer(() => {});
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const { port } = server.address() as AddressInfo;
    try {
      const readTip = () =>
        fetchTip(
          `http://127.0.0.1:${port.toString()}`,
          withRequestTimeout(fetch, 200),
        );
      await expect(readTip()).rejects.toMatchObject({ name: "TimeoutError" });
    } finally {
      server.closeAllConnections();
      await new Promise((resolve) => server.close(resolve));
    }
  }, 5_000);
});
