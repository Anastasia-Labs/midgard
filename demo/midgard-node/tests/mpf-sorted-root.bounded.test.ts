/**
 * The streaming trie builder (`SortedMpfRoot`, `mpf-sorted-root.ts`) takes
 * 1,000,000 random leaves in path order, made one at a time as it asks for
 * them, in memory that does not grow with the leaves it has taken: the
 * peak of the heap in use, with the array buffers held outside it, over
 * the last half of the stream stays within a fixed bound of the peak over
 * its first 100,000 leaves. A builder that kept the leaves or the trie
 * would hold at least their 64 MB of paths and digests by the end. Its
 * roots are the trie's in `confirmed-ledger-roots.stream.test.ts`.
 */
import { randomFillSync } from "node:crypto";

import { describe, expect, it } from "vitest";

import { SortedMpfRoot } from "../src/mpf/mpf-sorted-root.js";

const LEAVES = 1_000_000;
const EARLY = 100_000;
const SAMPLE_EVERY = 10_000;
/** The heap the last half's peak may exceed the early peak by. */
const BOUND_MB = 24;
const MB = 1024 * 1024;

/** The leaf paths' first 6 bytes split into `LEAVES` equal ranges: leaf
 * `index` takes a random path in range `index`, so the paths rise. */
const RANGE = 2 ** 48 / LEAVES;

const gc = (globalThis as { gc?: () => void }).gc;
/** The heap in use, with the array buffers held outside it. */
const heapInUse = () => {
  gc?.();
  const { heapUsed: used, arrayBuffers } = process.memoryUsage();
  return used + arrayBuffers;
};

describe("the streaming trie builder at size", () => {
  it(`takes ${LEAVES.toString()} leaves in path order in a heap that does not grow with them`, () => {
    const path = new Uint8Array(32);
    const valueDigest = new Uint8Array(32);
    const prefix = Buffer.from(path.buffer, path.byteOffset, 6);
    const builder = new SortedMpfRoot();
    const startedAt = Date.now();
    let early = 0;
    let late = 0;
    for (let index = 0; index < LEAVES; index += 1) {
      randomFillSync(path);
      randomFillSync(valueDigest);
      prefix.writeUIntBE(
        Math.floor(index * RANGE + Math.random() * RANGE),
        0,
        6,
      );
      builder.push(path, valueDigest);
      if ((index + 1) % SAMPLE_EVERY !== 0) continue;
      const used = heapInUse();
      if (index < EARLY) early = Math.max(early, used);
      else if (index >= LEAVES / 2) late = Math.max(late, used);
    }
    expect(builder.root()).toMatch(/^[0-9a-f]{64}$/);
    console.log(
      `${LEAVES.toString()} leaves in ${(Date.now() - startedAt).toString()} ms: heap in use peak ${(early / MB).toFixed(1)} MB over the first ${EARLY.toString()}, ${(late / MB).toFixed(1)} MB over the last half${gc === undefined ? "" : " (after gc)"}`,
    );
    expect(late - early).toBeLessThan(BOUND_MB * MB);
  }, 120_000);
});
