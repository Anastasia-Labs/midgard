// Deliberately imports neither `midgard-node/tests/utils` nor any database
// helper: the shard is the worker's whatever the file imports.
import { testDatabaseNameForShard } from "midgard-node/tests/test-env";
import { expect, it } from "vitest";

it("runs a file that imports no database helper on its worker's own shard", () => {
  expect(process.env.POSTGRES_DB).toBe(
    testDatabaseNameForShard(process.env.VITEST_POOL_ID ?? ""),
  );
});
