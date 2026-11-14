import { describe, expect, it } from "vitest";

import { type LinkedQueueEntry, walkLinkedQueue } from "../src/index.js";

const root = (next: string | null, id = "root"): LinkedQueueEntry => ({
  id,
  kind: "root",
  key: null,
  next,
});

const node = (
  key: string,
  next: string | null,
  id = `n-${key}`,
): LinkedQueueEntry => ({ id, kind: "node", key, next });

const invalid = (id: string): LinkedQueueEntry => ({
  id,
  kind: "invalid",
  key: null,
  next: null,
});

describe("walkLinkedQueue", () => {
  it("walks one list from the root to the tail, whatever the input order", () => {
    const walk = walkLinkedQueue([node("b", null), root("a"), node("a", "b")]);
    expect(walk).toMatchObject({ healthy: true, reason: null, strays: [] });
    expect(walk.root?.id).toBe("root");
    expect(walk.nodes.map((entry) => entry.id)).toEqual(["n-a", "n-b"]);
  });

  it("is healthy for a root with no nodes", () => {
    expect(walkLinkedQueue([root(null)])).toMatchObject({
      healthy: true,
      nodes: [],
    });
  });

  it.each([
    ["no_root", [node("a", null)]],
    ["multiple_roots", [root(null, "r1"), root(null, "r2")]],
    ["broken_link", [root("a"), node("b", null)]],
    ["duplicate_key", [root("a"), node("a", null, "x"), node("a", null, "y")]],
    ["cycle", [root("a"), node("a", "b"), node("b", "a")]],
    ["orphan_node", [root("a"), node("a", null), node("z", null)]],
    ["invalid_output", [root("a"), node("a", null), invalid("bad")]],
  ] as const)("names %s", (reason, entries) => {
    const walk = walkLinkedQueue(entries);
    expect(walk.healthy).toBe(false);
    expect(walk.reason).toBe(reason);
    expect(walk.detail).not.toBeNull();
  });

  it("keeps the walk so far and lists what it did not reach", () => {
    const walk = walkLinkedQueue([
      root("a"),
      node("a", "b"),
      node("b", "c"),
      node("z", null),
    ]);
    expect(walk.reason).toBe("broken_link");
    expect(walk.detail).toBe("c");
    expect(walk.nodes.map((entry) => entry.id)).toEqual(["n-a", "n-b"]);
    expect(walk.strays).toEqual(["n-z"]);
  });

  it("an orphan with a valid shape is unhealthy, not dropped", () => {
    const walk = walkLinkedQueue([root("a"), node("a", null), node("o", null)]);
    expect(walk).toMatchObject({
      healthy: false,
      reason: "orphan_node",
      detail: "n-o",
      strays: ["n-o"],
    });
    expect(walk.nodes.map((entry) => entry.id)).toEqual(["n-a"]);
  });

  it("is unhealthy over the node cap, and healthy at it", () => {
    const entries = [root("a"), node("a", "b"), node("b", null)];
    expect(walkLinkedQueue(entries, { maxNodes: 2 }).healthy).toBe(true);
    expect(walkLinkedQueue(entries, { maxNodes: 1 })).toMatchObject({
      healthy: false,
      reason: "over_cap",
    });
  });
});
