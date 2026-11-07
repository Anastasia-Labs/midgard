import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { expect } from "vitest";

import {
  type ReconciliationReport,
  STATE_RECONCILIATION_CHECK_IDS,
  stateReconciliationProgram,
} from "../src/commands/state-reconciliation.js";
import { decideAdmissionBatch } from "../src/fibers/tx-queue-processor.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import {
  Database,
  Effect,
  makeFixture,
  MempoolDB,
  MempoolLedgerDB,
  NodeConfig,
  processedTxFromValidatedTx,
  type QueuedTx,
  randomUUID,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  runPhaseAValidation,
  SqlClient,
  TxAdmissionsDB,
  WithdrawalsDB,
  WriteBehindLive,
} from "./deposit-flow-emulator-shared.js";

const describeReconciliation = (report: ReconciliationReport): string =>
  report.checks
    .map(
      (check) =>
        `${check.id}=${check.status} (${check.reason})${check.failures
          .map((failure) => `\n  FAIL ${failure}`)
          .join("")}${check.notes.map((note) => `\n  NOTE ${note}`).join("")}`,
    )
    .join("\n");

/**
 * `reconcile-state` exactly as the CLI runs it (no in-flight allowance), at a
 * quiescent point of the journey: every check must PASS and none may be
 * skipped.
 */
export const expectReconciled = async (
  stage: string,
  harness: Parameters<typeof runNodeCommandProgram>[1],
): Promise<ReconciliationReport> => {
  const report = await runNodeCommandProgram(
    stateReconciliationProgram({ maxAttempts: 1 }),
    harness,
  );
  const described = `${stage}\n${describeReconciliation(report)}`;
  expect(report.checks.map((check) => check.id)).toEqual([
    ...STATE_RECONCILIATION_CHECK_IDS,
  ]);
  for (const check of report.checks) {
    expect(check.status, described).toBe("PASS");
  }
  expect(report.ok, described).toBe(true);
  return report;
};

/**
 * Runs one L2 transaction through the queue processor's admission path
 * (durable admission, lease claim, Phase A, the batch decision against the
 * pending withdrawals and mempool_ledger, durable accept or reject) and
 * returns its durable admission row.
 */
export const admitL2Tx = async (
  fixture: Awaited<ReturnType<typeof makeFixture>>,
  built: { readonly txId: Buffer; readonly txCbor: Buffer },
) => {
  const admittedL2Transfer = await runNodeDatabaseEffect(
    TxAdmissionsDB.admit({
      txId: built.txId,
      txCanonicalCbor: built.txCbor,
      submitSource: "native",
    }),
  );
  expect(admittedL2Transfer.kind).toBe("new");
  expect(admittedL2Transfer.entry[TxAdmissionsDB.Columns.STATUS]).toBe(
    TxAdmissionsDB.Status.Queued,
  );

  const l2TransferLeaseOwner = `deposit-flow:${randomUUID()}`;
  const claimL2TransferOnce = () =>
    runNodeDatabaseEffect(
      TxAdmissionsDB.claimBatchLease({
        limit: 1,
        leaseOwner: l2TransferLeaseOwner,
        leaseDurationMs: 30_000,
      }),
    );
  // The node's own claim loop treats an empty claim as an ordinary tick
  // outcome and re-claims on its next tick (see the `claimedLeases.length
  // === 0` branch in src/fibers/tx-queue-processor.ts), so requiring the
  // very first attempt to succeed asserts more than the production contract
  // guarantees and made this journey intermittently red under load. Poll
  // under the same lease owner for a bounded window instead. An admission
  // that never becomes claimable is still a hard failure, and it reports the
  // durable row state so a genuine liveness defect cannot hide here.
  // performance.now() is deliberate: Date is faked for this test.
  const claimDeadlineMs = 30_000;
  const claimStartedAt = performance.now();
  let claimedL2Transfers = await claimL2TransferOnce();
  let claimAttempts = 1;
  while (
    claimedL2Transfers.length === 0 &&
    performance.now() - claimStartedAt < claimDeadlineMs
  ) {
    await new Promise((resolve) => setTimeout(resolve, 100));
    claimedL2Transfers = await claimL2TransferOnce();
    claimAttempts += 1;
  }
  if (claimedL2Transfers.length === 0) {
    const admissionRows = await runNodeDatabaseEffect(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql`
          SELECT
            encode(tx_id, 'hex') AS tx_id,
            status::text AS status,
            arrival_seq::text AS arrival_seq,
            lease_owner,
            next_attempt_at,
            NOW() AS db_now,
            (next_attempt_at <= NOW()) AS claimable
          FROM ${sql(TxAdmissionsDB.tableName)}
          ORDER BY arrival_seq
        `;
      }),
    );
    throw new Error(
      `Durable admission never became claimable after ${claimAttempts.toString()} attempts across ${claimDeadlineMs.toString()}ms: expectedTxId=${built.txId.toString("hex")} rows=${JSON.stringify(admissionRows)}`,
    );
  }
  expect(claimedL2Transfers).toHaveLength(1);
  const loadedL2Transfers = await runNodeDatabaseEffect(
    TxAdmissionsDB.loadClaimedPayloads({
      claimed: claimedL2Transfers,
      leaseOwner: l2TransferLeaseOwner,
    }),
  );
  expect(loadedL2Transfers).toHaveLength(1);
  const queuedL2Transfer: QueuedTx = {
    txId: loadedL2Transfers[0]![TxAdmissionsDB.Columns.TX_ID],
    txCbor: loadedL2Transfers[0]![TxAdmissionsDB.Columns.TX_CANONICAL_CBOR],
    arrivalSeq: loadedL2Transfers[0]![TxAdmissionsDB.Columns.ARRIVAL_SEQ],
    createdAt: loadedL2Transfers[0]![TxAdmissionsDB.Columns.FIRST_SEEN_AT],
  };
  const phaseA = await Effect.runPromise(
    runPhaseAValidation([queuedL2Transfer], {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    }),
  );
  const pendingWithdrawalOutRefHexes = await runNodeDatabaseEffect(
    WithdrawalsDB.retrievePendingLedgerOutRefHexes,
  );
  const ledgerEntries = await runNodeDatabaseEffect(
    MempoolLedgerDB.retrieveSpendable,
  );
  const ledgerState = new Map(
    ledgerEntries.map((entry) => [
      entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
      entry[MempoolLedgerDB.Columns.OUTPUT],
    ]),
  );
  const { phaseB, allRejected } = await Effect.runPromise(
    decideAdmissionBatch({
      phaseA,
      pendingWithdrawalOutRefHexes,
      ledgerState,
      phaseBConfig: {
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        nowCardanoSlotNo: BigInt(fixture.operatorLucid.currentSlot()),
        bucketConcurrency: 1,
        enforceScriptBudget: true,
      },
    }),
  );
  // This harness uses explicit unowned model writes throughout; the verdict
  // needs the same fixture gate as admission and leasing above.
  await Effect.runPromise(
    Effect.scoped(
      Effect.gen(function* () {
        yield* TxAdmissionsDB.markRejected({
          rows: claimedL2Transfers,
          leaseOwner: l2TransferLeaseOwner,
          rejectedTxs: allRejected,
        });
        if (phaseB.accepted.length > 0) {
          yield* TxAdmissionsDB.markAccepted({
            rows: claimedL2Transfers,
            leaseOwner: l2TransferLeaseOwner,
            processedTxs: phaseB.accepted.map(processedTxFromValidatedTx),
          });
        }
      }).pipe(
        Effect.provideService(FollowerWriteFixture, true),
        Effect.provide(WriteBehindLive),
        Effect.provide(Database.layer),
        Effect.provide(NodeConfig.layer),
      ),
    ),
  );
  return await runNodeDatabaseEffect(TxAdmissionsDB.getByTxId(built.txId));
};

/**
 * Admits one L2 transaction through {@link admitL2Tx}, requires it accepted,
 * then dates its mempool row inside the next block window.
 */
export const admitAndAcceptL2Tx = async (
  fixture: Awaited<ReturnType<typeof makeFixture>>,
  built: { readonly txId: Buffer; readonly txCbor: Buffer },
  alignedMempoolTimestamp: Date,
): Promise<void> => {
  const admission = await admitL2Tx(fixture, built);
  expect(admission?.[TxAdmissionsDB.Columns.STATUS]).toBe(
    TxAdmissionsDB.Status.Accepted,
  );
  expect(alignedMempoolTimestamp.getTime()).toBeLessThanOrEqual(Date.now());
  const alignedMempoolRows = await runNodeDatabaseEffect(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql`
        UPDATE ${sql(MempoolDB.tableName)}
        SET time_stamp_tz = ${alignedMempoolTimestamp}
        WHERE tx_id = ${built.txId}
        RETURNING tx_id
      `;
    }),
  );
  expect(alignedMempoolRows).toHaveLength(1);
};
