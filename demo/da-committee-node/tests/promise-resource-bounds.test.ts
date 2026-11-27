import { describe, expect, it } from "vitest";

import { postgresPromiseResources } from "../src/store/promise-resource-usage.js";

describe("retained store resource bound", () => {
  it("forwards byte and row limits through the installed store delegate", async () => {
    const pool = {
      query: async () => ({ rows: [{ records: "4", bytes: "32" }] }),
    };
    // This is the SQL aggregate response only, not an independent database snapshot.
    const delegated = postgresPromiseResources(
      () => pool as unknown as import("pg").Pool,
    );
    await expect(
      delegated({ storeRecords: 3, storeEncodedBytes: 1024 }),
    ).rejects.toThrow("resource domain");
    await expect(
      delegated({ storeRecords: 4, storeEncodedBytes: 31 }),
    ).rejects.toThrow("resource domain");
    expect(await delegated({ storeRecords: 4, storeEncodedBytes: 32 })).toEqual(
      { storeRecords: 4, storeEncodedBytes: 32 },
    );
  });
});
