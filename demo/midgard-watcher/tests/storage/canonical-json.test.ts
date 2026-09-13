import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it, vi } from "vitest";

import {
  encodeWatcherDurableStore,
  makeEmptyWatcherDurableStore,
  watcherCanonicalJson,
  WatcherDurableStoreError,
} from "../../src/storage/durable-store.js";

describe("canonical snapshot encoding", () => {
  it("reuses validated immutable history across successive snapshots", () => {
    const history = Object.freeze(
      Array.from({ length: 256 }, (_, blockNo) =>
        Object.freeze({ slot: blockNo * 20, blockNo }),
      ),
    );
    expect(watcherCanonicalJson({ revision: 1, history })).toBe(
      JSON.stringify({
        history: history.map(({ blockNo, slot }) => ({ blockNo, slot })),
        revision: 1,
      }),
    );
    const descriptors = vi.spyOn(Object, "getOwnPropertyDescriptor");
    try {
      expect(watcherCanonicalJson({ revision: 2, history })).toBe(
        JSON.stringify({
          history: history.map(({ blockNo, slot }) => ({ blockNo, slot })),
          revision: 2,
        }),
      );
      // Advancing the revision must not revisit every historical record.
      expect(descriptors.mock.calls.length).toBeLessThan(10);
    } finally {
      descriptors.mockRestore();
    }
  });

  it("rechecks mutable descendants of frozen objects and arrays", () => {
    const point = { slot: 20 };
    const snapshot = Object.freeze({ history: Object.freeze([point]) });
    expect(watcherCanonicalJson(snapshot)).toBe('{"history":[{"slot":20}]}');
    point.slot = 40;
    expect(watcherCanonicalJson(snapshot)).toBe('{"history":[{"slot":40}]}');
    Reflect.set(point, "unsupported", undefined);
    expect(() => watcherCanonicalJson(snapshot)).toThrow(
      WatcherDurableStoreError,
    );
  });

  it("does not cache mutable containers that share immutable children", () => {
    const point = Object.freeze({ slot: 20 });
    const history: unknown[] = [point, point];
    expect(watcherCanonicalJson(history)).toBe('[{"slot":20},{"slot":20}]');
    history.push(history);
    expect(() => watcherCanonicalJson(history)).toThrow(
      WatcherDurableStoreError,
    );
    history.pop();
    history.push(Object.freeze({ slot: 40 }));
    expect(watcherCanonicalJson(history)).toBe(
      '[{"slot":20},{"slot":20},{"slot":40}]',
    );
  });

  it("validates frozen values before admitting their encodings", () => {
    const getter = vi.fn(() => 20);
    const accessor = Object.freeze(
      Object.defineProperty({}, "slot", { get: getter, enumerable: true }),
    );
    const point = Object.freeze({ slot: 20 });
    watcherCanonicalJson(point);
    const invalid = [
      accessor,
      new Proxy(point, {}),
      Object.freeze(new Array(2)),
      Object.freeze({ [Symbol("slot")]: 20 }),
      Object.freeze(Object.assign([20], { extra: true })),
    ];
    for (const value of invalid) {
      expect(() => watcherCanonicalJson(value)).toThrow(
        WatcherDurableStoreError,
      );
      expect(() => watcherCanonicalJson(value)).toThrow(
        WatcherDurableStoreError,
      );
    }
    expect(getter).not.toHaveBeenCalled();
  });

  it("reuses immutable store validation while returning detached bytes", () => {
    const store = makeEmptyWatcherDurableStore(
      makeDeploymentMarker("ab".repeat(32)),
    );
    const freeze = (value: unknown): void => {
      if (value === null || typeof value !== "object") return;
      for (const child of Object.values(value)) freeze(child);
      Object.freeze(value);
    };
    freeze(store);
    const expected = encodeWatcherDurableStore(store);
    const first = encodeWatcherDurableStore(store);
    first.fill(0);
    expect(encodeWatcherDurableStore(store)).toEqual(expected);
    const descriptors = vi.spyOn(Object, "getOwnPropertyDescriptor");
    try {
      expect(encodeWatcherDurableStore(store)).toEqual(expected);
      expect(descriptors).not.toHaveBeenCalled();
    } finally {
      descriptors.mockRestore();
    }
  });

  it("revalidates store integrity when only the top-level object is frozen", () => {
    const store = makeEmptyWatcherDurableStore(
      makeDeploymentMarker("ab".repeat(32)),
    );
    Object.freeze(store);
    encodeWatcherDurableStore(store);
    Reflect.set(store.caches, "sourceSha256", "00".repeat(32));
    expect(() => encodeWatcherDurableStore(store)).toThrow(
      WatcherDurableStoreError,
    );
  });
});
