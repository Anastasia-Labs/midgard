/**
 * What moving one ledger snapshot costs, with no network between the two
 * stores. Run by `pnpm bench`, not by `test`:
 *
 *   LEDGER_SNAPSHOT_BENCH_DIR=/some/empty/dir \
 *     pnpm --dir demo --filter @al-ft/midgard-fault-proofs run bench \
 *     bench/ledger-snapshot-transfer.bench.test.ts
 *
 * A SQLite store that holds `LEDGER_SNAPSHOT_BENCH_ENTRIES` entries (default
 * 200000, outputs of mixed size) serves them the way a node does: each page
 * opens the ledger at its root, descends to the out-ref the page before it
 * ended on, fills one `ledger-snapshot` response within the 1 MiB inline
 * bound and encodes it. A second, empty SQLite store decodes each response
 * and admits the stream against the root.
 *
 * One JSON line reports the pages and bytes sent, the time spent serving
 * (opening, descending, reading and encoding), the time of the whole
 * transfer (serving, decoding and `admit`), one unpaged read of every entry
 * for comparison, and the process's peak memory during the transfer. It is
 * also written to `<directory>/transfer.json`.
 *
 * Not in it: the libp2p stream, and a node's own output reads, which come
 * from Postgres rather than from the store's file. Times depend on the
 * machine and its load and are not asserted.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  daLedgerSnapshotPage,
  decodeDaLedgerSnapshotResponseCbor,
  encodeDaLedgerSnapshotResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { expect, it } from "vitest";

import type { LedgerEntry } from "../src/ledger-at.js";
import { createLedgerStore } from "../src/ledger-store/index.js";
import { openSqliteLedgerStorage } from "../src/ledger-store/sqlite.js";
import { oracleLedgerRoot } from "../tests/support/ledger-root-oracle.js";
import {
  SIZED_ENTRIES_FROM,
  testLedgerEntries,
} from "../tests/support/ledger-store-conformance.js";
import { sampled } from "./sampled.js";

/** The node listener's inline response bound. */
const PAGE_BYTES = 2 ** 20;

const entriesFromEnv = (): number => {
  const raw = process.env.LEDGER_SNAPSHOT_BENCH_ENTRIES;
  if (raw === undefined) return 200_000;
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 1)
    throw new Error(
      `LEDGER_SNAPSHOT_BENCH_ENTRIES must be a positive integer, got ${raw}`,
    );
  return parsed;
};

it("measures one ledger snapshot's transfer between two SQLite stores", async () => {
  const directory = process.env.LEDGER_SNAPSHOT_BENCH_DIR;
  if (directory === undefined || directory === "")
    throw new Error("LEDGER_SNAPSHOT_BENCH_DIR must name an empty directory");
  await mkdir(directory, { recursive: true });
  const count = entriesFromEnv();
  const base: LedgerEntry[] = testLedgerEntries(SIZED_ENTRIES_FROM, count);
  const root = await oracleLedgerRoot(base);

  const serverStorage = openSqliteLedgerStorage(
    join(directory, "server.sqlite"),
  );
  const clientStorage = openSqliteLedgerStorage(
    join(directory, "client.sqlite"),
  );
  try {
    const server = createLedgerStore(serverStorage);
    const client = createLedgerStore(clientStorage);
    await server.admit(base, root);

    const unpagedStarted = performance.now();
    let unpaged = 0;
    const whole = (await server.at(root)).entries()[Symbol.asyncIterator]();
    while (!(await whole.next()).done) unpaged += 1;
    const unpagedMs = performance.now() - unpagedStarted;

    let pages = 0;
    let bytes = 0;
    let largestPageBytes = 0;
    let serveMs = 0;
    const served = async function* (): AsyncIterable<LedgerEntry> {
      let after: Buffer | undefined;
      for (;;) {
        const started = performance.now();
        const ledger = await server.at(root);
        const page = await daLedgerSnapshotPage(
          ledger.entries(after === undefined ? {} : { after }),
          PAGE_BYTES,
        );
        const response = encodeDaLedgerSnapshotResponseCbor({
          status: "ok",
          ...page,
        });
        serveMs += performance.now() - started;
        pages += 1;
        bytes += response.length;
        largestPageBytes = Math.max(largestPageBytes, response.length);
        const decoded = decodeDaLedgerSnapshotResponseCbor(response);
        if (decoded.status !== "ok") throw new Error("the page is not ok");
        for (const entry of decoded.entries) {
          after = Buffer.from(entry.outRef);
          yield { outRef: after, output: Buffer.from(entry.output) };
        }
        if (decoded.done) return;
      }
    };

    const transfer = await sampled(async () => {
      const ledger = await client.admit(served(), root);
      return ledger.root;
    });

    const report = {
      entries: count,
      pages,
      bytes,
      largestPageBytes,
      unpagedReadMs: Math.round(unpagedMs),
      serveMs: Math.round(serveMs),
      transferMs: Math.round(transfer.milliseconds),
      admitAndDecodeMs: Math.round(transfer.milliseconds - serveMs),
      baselineRssMiB: transfer.baselineRssMiB,
      peakRssMiB: transfer.peakRssMiB,
      baselineHeapMiB: transfer.baselineHeapMiB,
      peakHeapMiB: transfer.peakHeapMiB,
    };
    console.log(JSON.stringify(report));
    await writeFile(join(directory, "transfer.json"), JSON.stringify(report));

    expect(transfer.result).toBe(root);
    expect(unpaged).toBe(count);
    expect(largestPageBytes).toBeLessThanOrEqual(PAGE_BYTES);
  } finally {
    serverStorage.close();
    clientStorage.close();
  }
});
