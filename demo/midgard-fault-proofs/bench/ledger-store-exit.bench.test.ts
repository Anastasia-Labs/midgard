/**
 * The ledger store's exit measurements on the SQLite adapter, one phase per
 * process (see `tests/support/ledger-store-exit-bench.ts` for the phases and
 * the environment that sizes them):
 *
 *   LEDGER_STORE_EXIT_DIR=/some/dir LEDGER_STORE_EXIT_PHASE=build \
 *     pnpm --dir demo --filter @al-ft/midgard-fault-proofs run bench \
 *     bench/ledger-store-exit.bench.test.ts
 *
 * then the same with `window`, then `drop`. The database's size on disk is
 * read after a WAL checkpoint, so it counts what reached the database file.
 */
import { stat } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { expect, it } from "vitest";

import { openSqliteLedgerStorage } from "../src/ledger-store/sqlite.js";
import {
  exitBenchInputFromEnv,
  runLedgerStoreExitBench,
} from "../tests/support/ledger-store-exit-bench.js";

const bytesOnDisk = async (path: string) => {
  const checkpoint = new DatabaseSync(path);
  try {
    checkpoint.exec("PRAGMA busy_timeout = 5000");
    checkpoint.exec("PRAGMA wal_checkpoint(TRUNCATE)");
  } finally {
    checkpoint.close();
  }
  return (await stat(path)).size;
};

it("measures the SQLite ledger store", async () => {
  const input = exitBenchInputFromEnv();
  const path = join(input.directory, "ledger.sqlite");
  const report = await runLedgerStoreExitBench({
    ...input,
    adapter: "sqlite",
    open: async () => {
      const storage = openSqliteLedgerStorage(path);
      return {
        storage,
        bytesOnDisk: async () => await bytesOnDisk(path),
        close: async () => {
          storage.close();
        },
      };
    },
  });
  expect(report.phase).toBe(input.phase);
});
