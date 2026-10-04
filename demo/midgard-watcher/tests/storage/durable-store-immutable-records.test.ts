import { describe, expect, it } from "vitest";

import {
  encodeWatcherDurableStore,
  makeEmptyWatcherDurableStore,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  type WatcherDurableRecords,
} from "../../src/storage/durable-store.js";
import {
  parseChainPoint,
  parseL1Observation,
} from "../../src/storage/durable-store.parse-l1-observation.js";
import { expectStoreError, marker } from "./durable-store.records-fixture.js";

const pair = (ordinal: number) => {
  const chainPointId = ordinal.toString(16).padStart(64, "0");
  return {
    point: {
      chainPointId,
      providerId: "local-node",
      blockHash: (ordinal + 100).toString(16).padStart(64, "0"),
      slot: (ordinal * 20).toString(),
      blockNo: ordinal.toString(),
      depth: "10",
    },
    observation: {
      observationId: (ordinal + 200).toString(16).padStart(64, "0"),
      providerId: "local-node",
      chainPointId,
      payload: { ...makeWatcherDurablePayload("8180") },
    },
  };
};
const build = (records: WatcherDurableRecords, revision: string) =>
  makeWatcherDurableStore({ deploymentMarker: marker, records, revision });
const fresh = (records: WatcherDurableRecords): WatcherDurableRecords =>
  structuredClone(records);

describe("owned immutable L1 durable records", () => {
  it("retains validated identities across append/prune with unchanged persisted bytes", () => {
    const pairs = Array.from({ length: 16 }, (_, i) => pair(i + 1));
    const first = build(
      {
        ...makeEmptyWatcherDurableStore(marker),
        chainPoints: pairs.map(({ point }) => point),
        l1Observations: pairs.map(({ observation }) => observation),
      },
      "1",
    );
    const added = pair(17);
    const appended = build(
      {
        ...first,
        chainPoints: [...first.chainPoints, added.point],
        l1Observations: [...first.l1Observations, added.observation],
      },
      "2",
    );
    const pruned = build(
      {
        ...appended,
        chainPoints: appended.chainPoints.slice(8),
        l1Observations: appended.l1Observations.slice(8),
      },
      "3",
    );
    for (let index = 0; index < first.chainPoints.length; index++) {
      expect(appended.chainPoints[index]).toBe(first.chainPoints[index]);
      expect(appended.l1Observations[index]).toBe(first.l1Observations[index]);
      expect(Object.isFrozen(first.chainPoints[index])).toBe(true);
      expect(Object.isFrozen(first.l1Observations[index])).toBe(true);
      expect(Object.isFrozen(first.l1Observations[index]!.payload)).toBe(true);
    }
    expect(pruned.chainPoints[0]).toBe(first.chainPoints[8]);
    expect(pruned.l1Observations[0]).toBe(first.l1Observations[8]);
    for (const store of [first, appended, pruned]) {
      expect(encodeWatcherDurableStore(store)).toEqual(
        encodeWatcherDurableStore(build(fresh(store), store.revision)),
      );
      expect(parseWatcherDurableStore(store).l1Observations[0]).toBe(
        store.l1Observations[0],
      );
    }
  });

  it("detaches caller records and does not memoize a frozen container's mutable payload", () => {
    const caller = pair(1);
    const first = parseChainPoint(caller.point, "$point");
    expect(first).not.toBe(caller.point);
    expect(Object.isFrozen(caller.point)).toBe(false);
    caller.point.depth = "11";
    expect(first.depth).toBe("10");
    expect(parseChainPoint(caller.point, "$point").depth).toBe("11");
    const frozenContainer = Object.freeze(caller.observation);
    const observed = parseL1Observation(frozenContainer, "$observation");
    expect(observed).not.toBe(frozenContainer);
    expect(observed.payload).not.toBe(frozenContainer.payload);
    expect(Object.isFrozen(frozenContainer.payload)).toBe(false);
    frozenContainer.payload.cborHex = "81";
    expect(observed.payload.cborHex).toBe("8180");
    expectStoreError(
      () => parseL1Observation(frozenContainer, "$observation"),
      "integrity_mismatch",
    );
  });

  it("keeps parser domains distinct and reparses wrappers and accessors", () => {
    const caller = pair(1);
    const point = parseChainPoint(caller.point, "$point");
    const observation = parseL1Observation(caller.observation, "$observation");
    expectStoreError(
      () => parseL1Observation(point, "$observation"),
      "unknown_field",
    );
    expectStoreError(
      () => parseChainPoint(observation, "$point"),
      "unknown_field",
    );
    const wrapper = new Proxy(point, {});
    expect(parseChainPoint(wrapper, "$point")).not.toBe(point);
    const changedWrapper = new Proxy(
      { ...point },
      {
        get: (target, key) =>
          key === "depth" ? "invalid" : Reflect.get(target, key),
      },
    );
    expectStoreError(
      () => parseChainPoint(changedWrapper, "$point"),
      "invalid_field",
    );
    let depth = "12";
    const accessor = Object.freeze({
      ...caller.point,
      get depth() {
        return depth;
      },
    });
    const parsed = parseChainPoint(accessor, "$point");
    expect(parsed.depth).toBe("12");
    depth = "invalid";
    expectStoreError(
      () => parseChainPoint(accessor, "$point"),
      "invalid_field",
    );
    expect(parsed.depth).toBe("12");
  });

  it("checks duplicate, ordering, reference and cache guards despite owned records", () => {
    const a = pair(1);
    const b = pair(2);
    const store = build(
      {
        ...makeEmptyWatcherDurableStore(marker),
        chainPoints: [a.point, b.point],
        l1Observations: [a.observation, b.observation],
      },
      "1",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore({
          ...store,
          chainPoints: [store.chainPoints[0], store.chainPoints[0]],
        }),
      "duplicate_key",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore({
          ...store,
          chainPoints: [...store.chainPoints].reverse(),
        }),
      "unsorted_records",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore({
          ...store,
          chainPoints: store.chainPoints.slice(1),
        }),
      "broken_reference",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore({
          ...store,
          caches: { ...store.caches, sourceSha256: "f".repeat(64) },
        }),
      "cache_mismatch",
    );
    expectStoreError(
      () =>
        parseL1Observation(
          {
            ...store.l1Observations[0],
            payload: { ...store.l1Observations[0]!.payload, cborHex: "81" },
          },
          "$substituted",
        ),
      "integrity_mismatch",
    );
    const substituted = parseChainPoint(
      {
        ...store.chainPoints[0],
        providerId: "other-node",
      },
      "$substituted",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore({
          ...store,
          chainPoints: [substituted, store.chainPoints[1]],
        }),
      "broken_reference",
    );
  });
});
