import { Level } from "level";
import { describe, expect, it, vi } from "vitest";

import {
  type ReconciliationReport,
  STATE_RECONCILIATION_CHECK_IDS,
  stateReconciliationProgram,
} from "../src/commands/state-reconciliation.js";
import { ROOT_KEY } from "../src/mpf/store-primitives.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  attestQueuedStateQueueHeader,
  configureEmulatorDaRuntimeManifest,
  Effect,
  fetchLatestCommittedBlock,
  initializeNodeRuntime,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  mergeMaturityWindow,
  resetActiveRuntimePaths,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runMergeUntilMerged,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  SDK,
  SqlClient,
  stateQueueFetchConfig,
  submitDepositWithDiagnostics,
} from "./deposit-flow-emulator-shared.js";

/**
 * The reconciliation command against a real node pipeline: Lucid emulator L1,
 * the worker's Postgres shard, and the legacy MPF LevelDB. A committed,
 * locally finalized deposit block must reconcile clean; one introduced SQL or
 * native inconsistency at a time must fail the check that compares it.
 */

type Harness = Parameters<typeof runNodeCommandProgram>[1];

const reconcile = (harness: Harness, allowInFlight = false) =>
  runNodeCommandProgram(
    stateReconciliationProgram({ allowInFlight, maxAttempts: 1 }),
    harness,
  );

const describeReport = (report: ReconciliationReport) =>
  report.checks
    .map(
      (c) =>
        `${c.id}=${c.status} (${c.reason})${c.failures.map((f) => `\n  FAIL ${f}`).join("")}`,
    )
    .join("\n");

const expectStatuses = (
  report: ReconciliationReport,
  expected: Partial<
    Record<(typeof STATE_RECONCILIATION_CHECK_IDS)[number], string>
  >,
  fallback: "PASS" | "PASS_OR_SKIPPED",
) => {
  for (const check of report.checks) {
    const want = expected[check.id];
    if (want !== undefined) {
      expect(check.status, describeReport(report)).toBe(want);
    } else if (fallback === "PASS") {
      expect(check.status, describeReport(report)).toBe("PASS");
    } else {
      expect(["PASS", "SKIPPED"], describeReport(report)).toContain(
        check.status,
      );
    }
    expect(check.reason.length).toBeGreaterThan(0);
  }
};

const sqlMutation = <A>(
  run: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown>,
) =>
  runNodeDatabaseEffect(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* run(sql);
    }),
  );

/** Applies one inconsistency, reconciles, and always restores it. */
const withMutation = async (
  apply: () => Promise<unknown>,
  restore: () => Promise<unknown>,
  harness: Harness,
): Promise<ReconciliationReport> => {
  await apply();
  try {
    return await reconcile(harness);
  } finally {
    await restore();
  }
};

const withNativeRootMarker = async <A>(
  root: string,
  run: () => Promise<A>,
): Promise<A> => {
  const path = process.env.LEDGER_MPF_DB_PATH!;
  const open = async () => {
    const db = new Level<string, unknown>(path, {
      valueEncoding: "json",
      createIfMissing: false,
    });
    await db.open();
    return db;
  };
  const db = await open();
  const original = await db.get(ROOT_KEY);
  await db.put(ROOT_KEY, root);
  await db.close();
  try {
    return await run();
  } finally {
    const restoreDb = await open();
    if (original === undefined) await restoreDb.del(ROOT_KEY);
    else await restoreDb.put(ROOT_KEY, original);
    await restoreDb.close();
  }
};

describe.sequential(
  "state reconciliation against the emulator pipeline",
  () => {
    it("reconciles a committed deposit block, fails the targeted check per injected inconsistency, and reconciles again after merge", async () => {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();
      await configureEmulatorDaRuntimeManifest();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      const l2Address = await fixture.depositorLucid.wallet().address();
      await submitDepositWithDiagnostics(fixture, {
        l2Address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
      });
      const depositUtxos = await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
          ...SDK.eventHistoryDeploymentFromContracts(
            SDK.requireEventHistoryContracts(fixture.contracts).deposit,
          ),
        }),
      );
      expect(depositUtxos).toHaveLength(1);
      fixture.emulator.awaitSlot(
        fixture.operatorLucid.unixTimeToSlot(
          Number(depositUtxos[0]!.facts.inclusion_time),
        ) + 1,
      );
      vi.setSystemTime(new Date(fixture.emulator.now()));

      const commitOutput = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        ),
      });
      await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
      const globals = await makeGlobalsService();
      await runBlockConfirmation(globals, fixture.contracts, lucidService);
      await runLocalFinalizationRecoveryWorker(
        globals,
        fixture.contracts,
        lucidService,
      );
      const harness: Harness = { fixture, lucidService, globals };

      const queue = await Effect.runPromise(
        SDK.fetchSortedStateQueueUTxOsProgram(
          fixture.operatorLucid,
          stateQueueFetchConfig(fixture.contracts),
        ),
      );
      expect(queue).toHaveLength(2);
      const queuedHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(queue[1]!.datum),
      );
      const headerHash = await Effect.runPromise(
        SDK.hashBlockHeader(queuedHeader),
      );
      const headerHashBuffer = Buffer.from(headerHash, "hex");

      // Positive polarity: a live, consistent pipeline.
      const clean = await reconcile(harness);
      expect(clean.ok, describeReport(clean)).toBe(true);
      expect(clean.exitCode).toBe(0);
      expectStatuses(clean, {}, "PASS");
      expect(clean.snapshot.nativeRoot).toContain(queuedHeader.utxosRoot);
      expect(clean.snapshot.finalizedTip).toContain(headerHash);

      // confirmed-root: a well-formed but foreign confirmed_ledger entry (a
      // copy of a real L2 output under a different outref). The journal
      // chain's base no longer matches, so the ledger points become
      // unrecomputable (SKIPPED, pointing at confirmed-root) rather than being
      // reported a second time.
      const template = await sqlMutation(
        (sql) =>
          sql<{
            readonly tx_id: Buffer;
            readonly outref: Buffer;
            readonly output: Buffer;
            readonly address: string;
          }>`SELECT tx_id, outref, output, address FROM mempool_ledger LIMIT 1`,
      );
      expect(template).toHaveLength(1);
      const foreignOutref = Buffer.from(template[0]!.outref);
      foreignOutref[10] = foreignOutref[10]! ^ 0xff;
      const confirmedRootReport = await withMutation(
        () =>
          sqlMutation(
            (
              sql,
            ) => sql`INSERT INTO confirmed_ledger (tx_id, outref, output, address)
              VALUES (${template[0]!.tx_id}, ${foreignOutref}, ${template[0]!.output}, ${template[0]!.address})`,
          ),
        () =>
          sqlMutation(
            (sql) =>
              sql`DELETE FROM confirmed_ledger WHERE outref = ${foreignOutref}`,
          ),
        harness,
      );
      expectStatuses(
        confirmedRootReport,
        {
          "confirmed-root": "FAIL",
          "native-root": "SKIPPED",
          "ledger-cache": "SKIPPED",
        },
        "PASS",
      );
      expect(
        confirmedRootReport.checks.find((c) => c.id === "confirmed-root")
          ?.reason,
      ).toContain("SQL confirmed_ledger root");
      expect(
        confirmedRootReport.checks.find((c) => c.id === "native-root")?.reason,
      ).toContain("reported by confirmed-root");

      // confirmed-root: an entry that cannot be encoded at all is reported,
      // not a crash of the whole command.
      const bogusOutref = Buffer.from("ee".repeat(34), "hex");
      const unencodableReport = await withMutation(
        () =>
          sqlMutation(
            (
              sql,
            ) => sql`INSERT INTO confirmed_ledger (tx_id, outref, output, address)
              VALUES (${Buffer.alloc(32, 0xee)}, ${bogusOutref}, ${Buffer.from("a0", "hex")}, 'addr_test_bogus')`,
          ),
        () =>
          sqlMutation(
            (sql) =>
              sql`DELETE FROM confirmed_ledger WHERE outref = ${bogusOutref}`,
          ),
        harness,
      );
      expectStatuses(
        unencodableReport,
        {
          "confirmed-root": "FAIL",
          "native-root": "SKIPPED",
          "ledger-cache": "SKIPPED",
        },
        "PASS",
      );
      expect(
        unencodableReport.checks.find((c) => c.id === "confirmed-root")?.reason,
      ).toContain("cannot be encoded");

      // state-queue-journal: the journal's expected deposits root diverges from
      // the on-chain header's.
      const journalReport = await withMutation(
        () =>
          sqlMutation(
            (sql) => sql`UPDATE pending_block_finalizations
            SET expected_deposits_root = ${"ab".repeat(32)}
            WHERE header_hash = ${headerHashBuffer}`,
          ),
        () =>
          sqlMutation(
            (sql) => sql`UPDATE pending_block_finalizations
            SET expected_deposits_root = ${queuedHeader.depositsRoot}
            WHERE header_hash = ${headerHashBuffer}`,
          ),
        harness,
      );
      expectStatuses(journalReport, { "state-queue-journal": "FAIL" }, "PASS");
      expect(
        journalReport.checks
          .find((c) => c.id === "state-queue-journal")
          ?.failures.join("\n"),
      ).toContain("deposits root differs");

      // state-queue-journal: blocks still references a header nobody has.
      const phantomHeader = Buffer.alloc(28, 0x5a);
      const blocksReport = await withMutation(
        () =>
          sqlMutation(
            (sql) => sql`INSERT INTO blocks (header_hash, tx_id)
            VALUES (${phantomHeader}, ${Buffer.alloc(32, 0x5a)})`,
          ),
        () =>
          sqlMutation(
            (sql) =>
              sql`DELETE FROM blocks WHERE header_hash = ${phantomHeader}`,
          ),
        harness,
      );
      expectStatuses(blocksReport, { "state-queue-journal": "FAIL" }, "PASS");

      // deposits: the included deposit is assigned to a header nobody has.
      const depositReport = await withMutation(
        () =>
          sqlMutation(
            (
              sql,
            ) => sql`UPDATE deposits_utxos SET projected_header_hash = ${Buffer.alloc(28, 0x6b)}
            WHERE projected_header_hash = ${headerHashBuffer}`,
          ),
        () =>
          sqlMutation(
            (
              sql,
            ) => sql`UPDATE deposits_utxos SET projected_header_hash = ${headerHashBuffer}
            WHERE projected_header_hash = ${Buffer.alloc(28, 0x6b)}`,
          ),
        harness,
      );
      expectStatuses(depositReport, { deposits: "FAIL" }, "PASS");
      expect(
        depositReport.checks
          .find((c) => c.id === "deposits")
          ?.failures.join("\n"),
      ).toContain("not on the L1 queue, not merged");

      // ledger-cache: a row the ledger does not contain.
      const cacheOutref = Buffer.from("cd".repeat(34), "hex");
      const cacheReport = await withMutation(
        () =>
          sqlMutation(
            (
              sql,
            ) => sql`INSERT INTO mempool_ledger (tx_id, outref, output, address)
            VALUES (${Buffer.alloc(32, 0xcd)}, ${cacheOutref}, ${Buffer.from("a0", "hex")}, 'addr_test_bogus')`,
          ),
        () =>
          sqlMutation(
            (sql) =>
              sql`DELETE FROM mempool_ledger WHERE outref = ${cacheOutref}`,
          ),
        harness,
      );
      expectStatuses(cacheReport, { "ledger-cache": "FAIL" }, "PASS");
      expect(
        cacheReport.checks.find((c) => c.id === "ledger-cache")?.reason,
      ).toContain("unexpected outref");

      // Native root: a divergent persisted marker. The native root is the one
      // value both native-root (against SQL) and state-queue-tail-root (against
      // L1) compare, so exactly those two fail.
      const nativeReport = await withNativeRootMarker("ab".repeat(32), () =>
        reconcile(harness),
      );
      expectStatuses(
        nativeReport,
        { "native-root": "FAIL", "state-queue-tail-root": "FAIL" },
        "PASS",
      );

      // Every mutation was restored.
      const restored = await reconcile(harness);
      expect(restored.ok, describeReport(restored)).toBe(true);

      // Merge the block: confirmed state, settlement and SQL all advance.
      await attestQueuedStateQueueHeader({
        fixture,
        lucidService,
        globals,
        headerHash,
      });
      await advanceEmulatorPastUnixTime(
        fixture,
        mergeMaturityWindow(fixture.operatorLucid, Number(queuedHeader.endTime))
          .readyAfterUnixTime,
      );
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await runMergeUntilMerged({ fixture, lucidService, globals });

      const merged = await reconcile(harness);
      expect(merged.ok, describeReport(merged)).toBe(true);
      expectStatuses(merged, {}, "PASS");
      expect(
        merged.checks.find((c) => c.id === "settlements")?.reason,
      ).toContain("1 settlement datum equal");

      // settlements: after the merge the journal's expected withdrawals root is
      // what the settlement is compared against.
      const settlementReport = await withMutation(
        () =>
          sqlMutation(
            (sql) => sql`UPDATE pending_block_finalizations
            SET expected_withdrawals_root = ${"ab".repeat(32)}
            WHERE header_hash = ${headerHashBuffer}`,
          ),
        () =>
          sqlMutation(
            (sql) => sql`UPDATE pending_block_finalizations
            SET expected_withdrawals_root = ${queuedHeader.withdrawalsRoot}
            WHERE header_hash = ${headerHashBuffer}`,
          ),
        harness,
      );
      expectStatuses(settlementReport, { settlements: "FAIL" }, "PASS");

      // deposits after merge: a merged header's deposit must be consumed.
      const mergedDepositReport = await withMutation(
        () =>
          sqlMutation(
            (sql) => sql`UPDATE deposits_utxos SET status = 'projected'
            WHERE projected_header_hash = ${headerHashBuffer}`,
          ),
        () =>
          sqlMutation(
            (sql) => sql`UPDATE deposits_utxos SET status = 'consumed'
            WHERE projected_header_hash = ${headerHashBuffer}`,
          ),
        harness,
      );
      expectStatuses(mergedDepositReport, { deposits: "FAIL" }, "PASS");
      expect(
        mergedDepositReport.checks.find((c) => c.id === "deposits")?.reason,
      ).toContain("expected consumed");

      const final = await reconcile(harness);
      expect(final.ok, describeReport(final)).toBe(true);
      vi.useRealTimers();
    }, 900_000);
  },
);
