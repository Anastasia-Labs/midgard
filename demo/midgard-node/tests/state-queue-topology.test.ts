import { describe, expect, it } from "vitest";
import type * as SDK from "@al-ft/midgard-sdk";
import { summarizeStateQueueTopology } from "@/services/state-queue-topology.js";

const mkNode = (
  key: SDK.StateQueueDatum["key"],
  next: SDK.StateQueueDatum["next"],
): SDK.StateQueueUTxO =>
  ({
    utxo: {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: "addr_test1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq",
      assets: {},
    },
    datum: {
      key,
      next,
      data: "00",
    } as SDK.StateQueueDatum,
    assetName: "4e6f6465",
  }) as SDK.StateQueueUTxO;

describe("state queue topology", () => {
  it("reports uninitialized topology when no policy UTxOs are present", () => {
    const topology = summarizeStateQueueTopology(0, []);
    expect(topology.initialized).toBe(false);
    expect(topology.healthy).toBe(false);
    expect(topology.reason).toBeUndefined();
  });

  it("accepts a single-node queue as healthy", () => {
    const topology = summarizeStateQueueTopology(
      1,
      [mkNode("Empty", "Empty")],
    );
    expect(topology.initialized).toBe(true);
    expect(topology.healthy).toBe(true);
    expect(topology.rootCount).toBe(1);
    expect(topology.tailCount).toBe(1);
    expect(topology.reason).toBeUndefined();
  });

  it("flags duplicate roots/tails as unhealthy", () => {
    const topology = summarizeStateQueueTopology(
      2,
      [mkNode("Empty", "Empty"), mkNode("Empty", "Empty")],
    );
    expect(topology.initialized).toBe(true);
    expect(topology.healthy).toBe(false);
    expect(topology.reason).toContain("root");
  });

  it("flags non-decodable policy UTxOs", () => {
    const topology = summarizeStateQueueTopology(
      3,
      [mkNode("Empty", "Empty"), mkNode({ Key: { key: "11".repeat(28) } }, "Empty")],
    );
    expect(topology.invalidNodeCount).toBe(1);
    expect(topology.healthy).toBe(false);
    expect(topology.reason).toContain("non-decodable");
  });
});
