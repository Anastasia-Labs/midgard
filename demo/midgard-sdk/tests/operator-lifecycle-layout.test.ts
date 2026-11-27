import { describe, expect, it } from "vitest";

import { orderedNotMemberWitness } from "../src/operator-lifecycle/layout.js";
import type { LinkedListNodeView } from "../src/operator-lifecycle/primitives.js";

const node = (key: string | null, next: string | null): LinkedListNodeView => ({
  key: key === null ? "Empty" : { Key: { key } },
  next: next === null ? "Empty" : { Key: { key: next } },
  data: 0n,
});

describe("ordered operator insertion witness", () => {
  const first = "40".repeat(28);
  const last = "c0".repeat(28);
  const nodes = [node(null, first), node(first, last), node(last, null)];

  it.each([
    ["before the first operator", "20".repeat(28), 0],
    ["between operators", "80".repeat(28), 1],
    ["after the final operator", "e0".repeat(28), 2],
  ] as const)("selects exactly one anchor %s", (_label, key, index) => {
    expect(
      nodes.filter((entry) => orderedNotMemberWitness(entry, key)),
    ).toEqual([nodes[index]]);
  });

  it.each([first, last])(
    "rejects existing key %s at either interval boundary",
    (key) => {
      expect(nodes.some((entry) => orderedNotMemberWitness(entry, key))).toBe(
        false,
      );
    },
  );

  it("accepts insertion into an empty list", () => {
    expect(orderedNotMemberWitness(node(null, null), first)).toBe(true);
  });
});
