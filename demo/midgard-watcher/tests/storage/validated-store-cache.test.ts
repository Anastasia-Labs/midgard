import { writeFile } from "node:fs/promises";
import { performance } from "node:perf_hooks";

import { afterEach, expect, it, vi } from "vitest";

import {
  decodeWatcherDurableRecord,
  encodeWatcherDurableRecord,
  type WatcherEncodedRecord,
} from "../../src/storage/durable-record-codec.js";
import * as stores from "../../src/storage/durable-store.js";

const hash = (value: number) => value.toString(16).padStart(64, "0");
const marker = {
  schemaVersion: "midgard-deployment-marker-v1",
  manifestId: hash(1),
} as const;
const fixture = (count = 8, payloadBytes = 64) =>
  stores.makeWatcherDurableStore({
    deploymentMarker: marker,
    revision: "1",
    records: {
      ...stores.makeEmptyWatcherDurableStore(marker),
      chainPoints: Array.from({ length: count }, (_, index) => ({
        chainPointId: hash(index + 1),
        providerId: "local-node",
        blockHash: hash(index + 1),
        blockNo: (index + 1).toString(),
        slot: (index + 1).toString(),
        depth: "30",
      })),
      l1Observations: Array.from({ length: count }, (_, index) => ({
        observationId: hash(index + 1),
        providerId: "local-node",
        chainPointId: hash(index + 1),
        payload: stores.makeWatcherDurablePayload("ab".repeat(payloadBytes)),
      })),
    },
  });
const freeze = (value: unknown): void => {
  if (typeof value !== "object" || value === null) return;
  for (const child of Object.values(value)) freeze(child);
  Object.freeze(value);
};
const bytes = (value: unknown) =>
  Buffer.from(stores.watcherCanonicalJson(value), "utf8");
const encode = (value: unknown, rawBytes: Uint8Array = bytes(value)) => {
  const records = new Map<string, WatcherEncodedRecord>();
  const root = encodeWatcherDurableRecord(
    rawBytes,
    (digest, record) => records.set(digest, record),
    value,
  );
  return { root, records };
};
afterEach(() => vi.restoreAllMocks());

it("reuses only the exact validated immutable store while preserving every encoded record and logical byte", () => {
  const value = fixture();
  freeze(value);
  expect(stores.readValidatedWatcherDurableStoreCaches(value)).toBeUndefined();
  const rawBytes = stores.encodeWatcherDurableStore(value);
  const caches = stores.readValidatedWatcherDurableStoreCaches(value);
  expect(caches).toBe(value.caches);
  expect(Object.isFrozen(caches)).toBe(true);
  expect(Reflect.set(value.caches.entries[0]!, "recordSha256", hash(99))).toBe(
    false,
  );
  const clone = structuredClone(value);
  freeze(clone);
  expect(stores.readValidatedWatcherDurableStoreCaches(clone)).toBeUndefined();
  const rebuild = vi.spyOn(stores, "rebuildWatcherDurableCaches");
  const admitted = encode(value, rawBytes);
  expect(rebuild).not.toHaveBeenCalled();
  const fallback = encode(clone, rawBytes);
  expect(rebuild).toHaveBeenCalledOnce();
  expect(admitted).toEqual(fallback);
  expect(
    decodeWatcherDurableRecord(admitted.root, (digest) => {
      const record = admitted.records.get(digest);
      if (record === undefined) throw new Error("missing encoded record");
      return decodeWatcherDurableRecord(record, () => {
        throw new Error("unexpected nested record");
      });
    }),
  ).toEqual(Buffer.from(rawBytes));
  expect(() => encode(value, Buffer.from("{}"))).toThrow(
    "canonical record hint differs",
  );
});

it("keeps validating frozen containers whose cache or payload children remain mutable", () => {
  const value = fixture();
  Object.freeze(value);
  stores.encodeWatcherDurableStore(value);
  expect(stores.readValidatedWatcherDurableStoreCaches(value)).toBeUndefined();
  Reflect.set(value.caches, "sourceSha256", hash(99));
  expect(() => encode(value)).toThrow("cache source digest mismatch");
  expect(() => stores.encodeWatcherDurableStore(value)).toThrow();
  expect(stores.readValidatedWatcherDurableStoreCaches(value)).toBeUndefined();
});

it.each(["payload", "cache"] as const)(
  "does not issue a receipt after failed immutable %s validation",
  (kind) => {
    const value = fixture();
    if (kind === "payload")
      Reflect.set(value.l1Observations[0]!.payload, "sha256", hash(99));
    else Reflect.set(value.caches, "sourceSha256", hash(99));
    freeze(value);
    expect(() => stores.encodeWatcherDurableStore(value)).toThrow();
    expect(
      stores.readValidatedWatcherDurableStoreCaches(value),
    ).toBeUndefined();
  },
);

it.runIf(process.env.MIDGARD_STORE_CACHE_BENCHMARK === "1")(
  "measures the same codec work with and without the validated immutable receipt",
  async () => {
    const admitted = fixture(512, 8192);
    freeze(admitted);
    const rawBytes = stores.encodeWatcherDurableStore(admitted);
    const fallback = structuredClone(admitted);
    freeze(fallback);
    const encodeKnown = (value: unknown) =>
      encodeWatcherDurableRecord(
        rawBytes,
        () => {
          throw new Error("all retained record hashes are already known");
        },
        value,
        () => true,
      );
    const oldEncoding = encodeKnown(fallback);
    const newEncoding = encodeKnown(admitted);
    expect(newEncoding).toEqual(oldEncoding);
    const sample = (value: unknown) => {
      const start = performance.now();
      encodeKnown(value);
      return performance.now() - start;
    };
    const before: number[] = [];
    const after: number[] = [];
    for (let index = 0; index < 7; index += 1) {
      before.push(sample(fallback));
      after.push(sample(admitted));
    }
    const median = (values: number[]) => [...values].sort((a, b) => a - b)[3]!;
    const report = {
      fixtureRecords: 1024,
      logicalBytes: rawBytes.length,
      samples: 7,
      fallbackMs: before,
      receiptMs: after,
      fallbackMedianMs: median(before),
      receiptMedianMs: median(after),
      reductionPercent: 100 * (1 - median(after) / median(before)),
      exactEncodedBytesEqual: true,
      scope:
        "Isolated record codec, warm retained hashes; not end-to-end persist latency",
    };
    await writeFile(
      "/tmp/midgard-validated-store-cache-benchmark.json",
      `${JSON.stringify(report, null, 2)}\n`,
    );
  },
);
