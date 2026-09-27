import { Effect } from "effect";

import { MempoolLedgerDB } from "../../src/database/index.js";
import type { DatabaseError } from "../../src/database/utils/common.js";
import { reconcileDepositProjection } from "../../src/fibers/project-deposits-to-mempool-ledger.js";
import {
  Database,
  Globals,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "../../src/services/index.js";

/**
 * Test harness: projects every due deposit into `mempool_ledger` and publishes
 * the cache delta, standing in for the history owner, which runs
 * `reconcileDepositProjection` inside its recovery transaction. Needs a
 * history-ingestion permit or the unowned-history fixture, like production.
 */
export const projectDepositsToMempoolLedger: Effect.Effect<
  void,
  DatabaseError,
  Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const config = yield* NodeConfig;
  const { reconciled, projectedCount } = yield* reconcileDepositProjection(
    new Date(),
  );
  const reconciledCount = reconciled.mutationCount;
  const totalMutations = reconciledCount + projectedCount;
  if (totalMutations <= 0) {
    return;
  }
  yield* publishMempoolLedgerDelta(
    globals,
    {
      full: false,
      // Newly projected deposits remain intentionally hidden from
      // retrieveSpendable until a confirmed header is assigned. An empty
      // incremental delta advances the cache journal without a full reload;
      // recovery rows already assigned to a header are immediately spendable.
      upserts: reconciled.spendableUpserts.map((entry) => [
        entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
        entry[MempoolLedgerDB.Columns.OUTPUT],
      ]),
      deletes: [],
    },
    config.VALIDATION_LEDGER_DELTA_LOG_MAX,
  );
  yield* Effect.logInfo(
    `🏦 Reconciled ${reconciledCount} projected deposit UTxO(s) and projected ${projectedCount} awaiting deposit UTxO(s) into mempool_ledger.`,
  );
});
