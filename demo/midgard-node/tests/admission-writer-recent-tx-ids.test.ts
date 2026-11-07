import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import type { Status } from "../src/database/txAdmissions.js";
import {
  makeRecentTxIds,
  type RecentTxId,
} from "../src/services/admission-writer.js";

const STATUSES: readonly Status[] = [
  "queued",
  "validating",
  "accepted",
  "rejected",
];

const sha = (label: string): Buffer =>
  createHash("sha256").update(label).digest();

/**
 * A tx id whose first four bytes (the index's home hash) come from a small
 * set, so ids pile into the same probe chains and every eviction exercises
 * the backward-shift deletion over long clusters.
 */
const clusteredId = (n: number): Buffer => {
  const id = sha(`clustered:${n.toString()}`);
  id.writeUInt32LE(n % 5, 0);
  return id;
};

const entry = (n: number, version = 0): RecentTxId => ({
  txFullHash: sha(`full:${n.toString()}:${version.toString()}`),
  status: STATUSES[(n + version) % STATUSES.length]!,
  firstSeenAt: new Date(1_700_000_000_000 + n * 1_000 + version),
});

/** A deterministic PRNG, so a failing sequence replays exactly. */
const xorshift = (seed: number) => {
  let state = seed >>> 0 || 1;
  return (bound: number): number => {
    state ^= state << 13;
    state >>>= 0;
    state ^= state >>> 17;
    state ^= state << 5;
    state >>>= 0;
    return state % bound;
  };
};

describe("recent admitted tx ids", () => {
  it("keeps exactly the last N ids, through growth and eviction, against a reference model", () => {
    // 48 starts below the 4,096-slot initial ring; 6,000 grows twice and
    // then evicts.
    for (const maxCapacity of [48, 6_000]) {
      const recent = makeRecentTxIds(maxCapacity);
      const random = xorshift(maxCapacity);
      // Model: insertion order of distinct ids; an update keeps the position.
      const order: number[] = [];
      const latest = new Map<number, RecentTxId>();
      const operations = maxCapacity * 4;
      for (let step = 0; step < operations; step += 1) {
        // Mostly fresh ids, sometimes an update of a remembered one.
        const reuse = order.length > 0 && random(4) === 0;
        const n = reuse ? order[random(order.length)]! : step;
        const value = entry(n, step);
        recent.remember(clusteredId(n), value);
        if (!latest.has(n)) {
          order.push(n);
          if (order.length > maxCapacity) latest.delete(order.shift()!);
        }
        latest.set(n, value);

        if (step % 97 === 0 || step === operations - 1) {
          expect(recent.size()).toBe(order.length);
          for (const kept of order) {
            expect(recent.get(clusteredId(kept))).toEqual(latest.get(kept));
          }
          // A few evicted ids must be gone.
          for (let probe = 0; probe < 20; probe += 1) {
            const candidate = random(step + 1);
            if (!latest.has(candidate)) {
              expect(recent.get(clusteredId(candidate))).toBeUndefined();
            }
          }
        }
      }
    }
  });

  it("ignores ids and hashes that are not 32 bytes", () => {
    const recent = makeRecentTxIds(8);
    recent.remember(Buffer.alloc(31, 1), entry(1));
    recent.remember(sha("short-hash"), {
      ...entry(2),
      txFullHash: Buffer.alloc(31, 2),
    });
    expect(recent.size()).toBe(0);
    expect(recent.get(Buffer.alloc(31, 1))).toBeUndefined();
  });
});
