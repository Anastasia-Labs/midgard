import { describe, expect, it } from "vitest";

import { type L1SourceState } from "../src/store.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

const sourceState: L1SourceState = {
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "93".repeat(32),
  status: "healthy",
  observations: [],
  observedAt: "2026-10-07T00:00:00.000Z",
};

describe("retained Postgres store input domain", () => {
  it("counts every durable row and its serialized bytes without modifying records", async () => {
    const store = await openTestCommitteeStore();
    const empty = await store.promiseStoreResourceUsage();
    expect(empty).toEqual({ storeRecords: 0, storeEncodedBytes: 0 });

    await store.saveL1SourceState(sourceState);
    const one = await store.promiseStoreResourceUsage();
    expect(one.storeRecords).toBe(1);
    expect(one.storeEncodedBytes).toBeGreaterThan(0);
    // Reading the usage changes nothing it counts.
    expect(await store.promiseStoreResourceUsage()).toEqual(one);
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      authoritySha256: sourceState.authoritySha256,
    });
    await expect(
      store.promiseStoreResourceUsage({
        storeRecords: 1,
        storeEncodedBytes: one.storeEncodedBytes - 1,
      }),
    ).rejects.toThrow("resource domain");
  });
});
