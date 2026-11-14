import { expect } from "vitest";

import {
  type ReconciliationReport,
  STATE_RECONCILIATION_CHECK_IDS,
  stateReconciliationProgram,
} from "../src/commands/state-reconciliation.js";
import {
  Effect,
  MempoolDB,
  runNodeDatabaseEffect,
  SqlClient,
  TxAdmissionsDB,
} from "./deposit-flow-emulator-shared.js";
import { settleAdmissions } from "./helpers/emulator-l2-transfer.js";
import type { ProductionLifecycle } from "./helpers/production-lifecycle.js";

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
 * `reconcile-state` exactly as the CLI runs it (no in-flight allowance), on
 * the running node at a quiescent point of the journey: every check must
 * PASS and none may be skipped.
 */
export const expectReconciled = async (
  stage: string,
  h: Pick<ProductionLifecycle, "command">,
): Promise<ReconciliationReport> => {
  const report = await h.command(
    stateReconciliationProgram({ maxAttempts: 1 }),
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
 * Queues one L2 transaction in the running node's durable admission queue,
 * as `/submit` does, and drains the queue with the node's processor until
 * the row is terminal; returns its durable admission row. The node does not
 * follow the chain meanwhile: the admission is judged at the view it last
 * applied.
 */
export const admitL2Tx = async (
  h: Pick<ProductionLifecycle, "fixture" | "runWithoutSynchronizing">,
  built: { readonly txId: Buffer; readonly txCbor: Buffer },
) => {
  const admitted = await h.runWithoutSynchronizing(
    TxAdmissionsDB.admit({
      txId: built.txId,
      txCanonicalCbor: built.txCbor,
      submitSource: "native",
    }),
  );
  expect(admitted.kind).toBe("new");
  await settleAdmissions(
    { fixture: h.fixture, command: h.runWithoutSynchronizing },
    [built.txId],
  );
  return await runNodeDatabaseEffect(TxAdmissionsDB.getByTxId(built.txId));
};

/**
 * Admits one L2 transaction through {@link admitL2Tx}, requires it accepted,
 * then dates its mempool row inside the next block window.
 */
export const admitAndAcceptL2Tx = async (
  h: Pick<ProductionLifecycle, "fixture" | "runWithoutSynchronizing">,
  built: { readonly txId: Buffer; readonly txCbor: Buffer },
  alignedMempoolTimestamp: Date,
): Promise<void> => {
  const admission = await admitL2Tx(h, built);
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
