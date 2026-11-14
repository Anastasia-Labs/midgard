import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  buildMidgardValidationTraceTree,
  MidgardValidationTraceFrontier,
  midgardValidationTraceLeafHash,
  MidgardValidationTracePadding,
} from "../src/index.js";

const stateHash = (index: number): Buffer =>
  createHash("sha256").update(`state-${index.toString()}`).digest();

const states = (count: number): Buffer[] =>
  Array.from({ length: count }, (_, index) => stateHash(index));

describe("validation trace frontier", () => {
  it("commits the root the full tree builds, for every state count up to 70", () => {
    for (let count = 1; count <= 70; count += 1) {
      const hashes = states(count);
      const frontier = new MidgardValidationTraceFrontier();
      for (const hash of hashes) frontier.appendState(hash);
      const tree = buildMidgardValidationTraceTree(hashes, "accepted");
      expect(frontier.stateCount).toBe(count);
      expect(frontier.root()).toEqual(tree.descriptor.traceRoot);
      expect(frontier.initialStateHash).toEqual(
        tree.descriptor.initialStateHash,
      );
      expect(frontier.lastStateHash).toEqual(tree.descriptor.terminalStateHash);
    }
  });

  it("reads its root without changing what later appends commit", () => {
    const hashes = states(37);
    const frontier = new MidgardValidationTraceFrontier();
    hashes.forEach((hash, index) => {
      frontier.appendState(hash);
      expect(frontier.root()).toEqual(
        buildMidgardValidationTraceTree(hashes.slice(0, index + 1), "accepted")
          .descriptor.traceRoot,
      );
    });
  });

  it("commits a run of one repeated state as the same states appended singly", () => {
    for (const prefix of [1, 2, 3, 5, 8, 13]) {
      for (const run of [1, 2, 3, 4, 7, 16, 29]) {
        const hashes = [
          ...states(prefix),
          ...Array.from({ length: run }, () => stateHash(999)),
        ];
        const lazy = new MidgardValidationTraceFrontier();
        for (const hash of hashes.slice(0, prefix)) lazy.appendState(hash);
        lazy.appendRun(stateHash(999), run);
        expect(lazy.stateCount).toBe(prefix + run);
        expect(lazy.root()).toEqual(
          buildMidgardValidationTraceTree(hashes, "accepted").descriptor
            .traceRoot,
        );
      }
    }
  });

  it("builds padding roots as the trees of repeated leaves", () => {
    const padding = new MidgardValidationTracePadding(stateHash(7));
    expect(padding.root(0)).toEqual(
      midgardValidationTraceLeafHash(stateHash(7)),
    );
    for (const level of [1, 2, 3, 5]) {
      expect(padding.root(level)).toEqual(
        buildMidgardValidationTraceTree(
          Array.from({ length: 2 ** level }, () => stateHash(7)),
          "accepted",
        ).descriptor.traceRoot,
      );
    }
  });

  it("refuses an empty root and runs past the step-count limit", () => {
    const frontier = new MidgardValidationTraceFrontier();
    expect(() => frontier.root()).toThrow();
    expect(() => frontier.appendRun(stateHash(0), 0)).toThrow();
    expect(() => frontier.appendRun(stateHash(0), 2 ** 32 + 1)).toThrow();
    frontier.appendRun(stateHash(0), 2 ** 32);
    expect(frontier.stateCount).toBe(2 ** 32);
    expect(() => frontier.appendState(stateHash(1))).toThrow();
  });
});
