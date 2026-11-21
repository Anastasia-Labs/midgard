/**
 * The ledger store's exit measurements on the LevelDB adapter, one phase per
 * process (see the fault-proofs `ledger-store-exit-bench` test support for the
 * phases and the environment that sizes them):
 *
 *   LEDGER_STORE_EXIT_DIR=/some/dir LEDGER_STORE_EXIT_PHASE=build \
 *     pnpm --dir demo --filter midgard-node exec vitest run \
 *     --config vitest.bench.config.ts tests/ledger-store-level-exit.bench.ts
 *
 * then the same with `sweep`, then `steady`. Outputs come from the test
 * reader, as the adapter keeps none. The size on disk is the sum of the
 * database directory's files, read after the batch that wrote them resolved.
 */
import { readdir, stat } from "node:fs/promises";
import { join } from "node:path";

import { testOutputOf } from "@al-ft/midgard-fault-proofs/test-support/ledger-store-conformance";
import {
  exitBenchInputFromEnv,
  runLedgerStoreExitBench,
} from "@al-ft/midgard-fault-proofs/test-support/ledger-store-exit-bench";
import { Level } from "level";
import { expect, it } from "vitest";

import { createLevelLedgerStorage } from "../src/mpf/ledger-store-level.js";
import { JSON_LEVEL_ENCODING_OPTS } from "../src/mpf/store-primitives.js";
import type { MpfStoredValue } from "../src/mpf/types.js";

const bytesOnDisk = async (path: string) => {
  let total = 0;
  for (const name of await readdir(path))
    total += (await stat(join(path, name))).size;
  return total;
};

it("measures the LevelDB ledger store", { timeout: 7_200_000 }, async () => {
  const input = exitBenchInputFromEnv();
  const path = join(input.directory, "mpf");
  const report = await runLedgerStoreExitBench({
    ...input,
    adapter: "leveldb",
    open: async () => {
      const level = new Level<string, MpfStoredValue>(
        path,
        JSON_LEVEL_ENCODING_OPTS,
      );
      await level.open();
      return {
        storage: createLevelLedgerStorage({
          level,
          readOutputs: async (outRefs) => outRefs.map(testOutputOf),
        }),
        bytesOnDisk: async () => await bytesOnDisk(path),
        close: async () => await level.close(),
      };
    },
  });
  expect(report.phase).toBe(input.phase);
});
