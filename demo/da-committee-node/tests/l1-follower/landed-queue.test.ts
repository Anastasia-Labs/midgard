import { describe, expect, it } from "vitest";

import {
  type QueueRow,
  walkLandedQueue,
} from "../../src/l1/follower/landed-queue.js";

const row = (
  outRef: string,
  kind: QueueRow["kind"],
  nodeKey: string | null,
  nextKey: string | null,
): QueueRow => ({
  outRef,
  kind,
  assetName: nodeKey,
  nodeKey,
  nextKey,
  headerHash: kind === "root" ? "00".repeat(28) : nodeKey,
  daStatus: kind === "node" ? "Unattested" : null,
  endTimeMs: kind === "node" ? 1_000n : null,
  problems: [],
  datumHex: null,
  createdSlot: 1,
  createdHeight: 1,
  createdTxIndex: 0,
  spentSlot: null,
});

// The walk's guards against shapes the state-queue validator never lets
// land: defence in depth, so a decoder fault cannot loop the walk or
// attribute one key's node to another.
describe("the landed-queue walk over malformed live rows", () => {
  it("walks one list from the root", () => {
    const queue = walkLandedQueue([
      row("r#0", "root", null, "aa"),
      row("a#0", "node", "aa", "bb"),
      row("b#0", "node", "bb", null),
    ]);
    expect(queue).toMatchObject({ healthy: true, reason: null, strays: [] });
    expect(queue.nodes.map((node) => node.outRef)).toEqual(["a#0", "b#0"]);
  });

  it("is unhealthy with duplicate_key when two live nodes share the key a link names", () => {
    const queue = walkLandedQueue([
      row("r#0", "root", null, "aa"),
      row("a#0", "node", "aa", null),
      row("a#1", "node", "aa", null),
    ]);
    expect(queue).toMatchObject({
      healthy: false,
      reason: "duplicate_key",
      detail: "a#0 a#1",
      nodes: [],
      strays: ["a#0", "a#1"],
    });
  });

  it("is unhealthy with cycle when the links loop back, and stops walking", () => {
    const queue = walkLandedQueue([
      row("r#0", "root", null, "aa"),
      row("a#0", "node", "aa", "bb"),
      row("b#0", "node", "bb", "aa"),
    ]);
    expect(queue).toMatchObject({
      healthy: false,
      reason: "cycle",
      detail: "aa",
      strays: [],
    });
    expect(queue.nodes.map((node) => node.outRef)).toEqual(["a#0", "b#0"]);
  });
});
