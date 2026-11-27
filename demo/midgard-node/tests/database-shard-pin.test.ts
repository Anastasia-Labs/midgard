// Deliberately imports neither `tests/utils.ts` nor any database helper: the
// shard is the worker's whatever the file imports.
import { expect, it } from "vitest";

import { testDatabaseNameForShard } from "./test-env.js";

it("runs a file that imports no database helper on its worker's own shard", () => {
  expect(process.env.POSTGRES_DB).toBe(
    testDatabaseNameForShard(process.env.VITEST_POOL_ID ?? ""),
  );
});
