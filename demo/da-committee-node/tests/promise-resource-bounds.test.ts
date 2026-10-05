import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  jsonPromiseResources,
  jsonPromiseStoreResourceUsage,
  postgresPromiseResources,
} from "../src/store/promise-resource-usage.js";
import { tempDir } from "./helpers.js";

describe("pre-parse retained JSON bound", () => {
  it("rejects an over-limit malformed file before JSON parsing", async () => {
    const path = join(await tempDir(), "bounded.json");
    await writeFile(path, "x".repeat(1025));
    await expect(
      jsonPromiseStoreResourceUsage(path, {
        storeRecords: 3,
        storeEncodedBytes: 1024,
      }),
    ).rejects.toThrow("byte domain");
  });
  it("retains exact bytes at the inclusive limit and refuses excess total rows", async () => {
    const path = join(await tempDir(), "bounded.json");
    const bytes = JSON.stringify({ headers: { one: {}, two: {} } });
    await writeFile(path, bytes);
    expect(
      await jsonPromiseStoreResourceUsage(path, {
        storeRecords: 2,
        storeEncodedBytes: Buffer.byteLength(bytes),
      }),
    ).toEqual({ storeRecords: 2, storeEncodedBytes: Buffer.byteLength(bytes) });
    await expect(
      jsonPromiseStoreResourceUsage(path, {
        storeRecords: 1,
        storeEncodedBytes: Buffer.byteLength(bytes),
      }),
    ).rejects.toThrow("resource domain");
  });
  it("forwards byte and row limits through both installed thin store delegates", async () => {
    const path = join(await tempDir(), "delegated.json");
    await writeFile(path, "x".repeat(1025));
    await expect(
      jsonPromiseResources(() => path)({
        storeRecords: 3,
        storeEncodedBytes: 1024,
      }),
    ).rejects.toThrow("byte domain");
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
    expect(await delegated({ storeRecords: 4, storeEncodedBytes: 32 })).toEqual(
      { storeRecords: 4, storeEncodedBytes: 32 },
    );
  });
});
