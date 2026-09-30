import { Data, Effect, pipe } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  ConfigError,
  ContractDeploymentIdentity,
  Database,
  DatabaseInitializationError,
  Lucid,
  MidgardContracts,
  MidgardContractServices,
  NodeConfig,
} from "../services/index.js";

export const EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS = 120_000;

export const EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS = 5_000;

export const EXPLICIT_COMMIT_BLOCK_VISIBILITY_DELAY = "5 seconds";

export const EXPLICIT_COMMIT_BLOCK_VISIBILITY_RETRIES = 18;

export class CommitWorkerInvariantError extends Data.TaggedError(
  "CommitWorkerInvariantError",
)<{
  readonly message: string;
}> {}

export const provideCommitBlockWorkerServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
  >,
): Effect.Effect<A, E | ConfigError | DatabaseInitializationError, never> =>
  pipe(
    effect,
    Effect.provide(MidgardContractServices),
    Effect.provide(Database.workerLayer),
    Effect.provide(NodeConfig.layer),
  );

export type CommitLucidFactory = () => Effect.Effect<Lucid, ConfigError>;

export const defaultCommitLucidFactory: CommitLucidFactory = () =>
  Effect.provide(Lucid, Lucid.Default);

type PendingUserEventCounts = {
  readonly deposits: number;
  readonly forcedTransactions: number;
  readonly withdrawals: number;
};

export const pendingUserEventCountsUpTo = (
  effectiveEndTime: Date,
  excluded?: {
    readonly depositEventIds: ReadonlySet<string>;
    readonly forcedTransactionEventIds: ReadonlySet<string>;
    readonly withdrawalEventIds: ReadonlySet<string>;
  },
): Effect.Effect<PendingUserEventCounts, DatabaseError, Database> =>
  Effect.gen(function* () {
    const [depositEntries, forcedTransactionEntries, withdrawalEntries] =
      yield* Effect.all(
        [
          DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
          ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          ),
          WithdrawalsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
        ],
        { concurrency: "unbounded" },
      );
    return {
      deposits: depositEntries.filter(
        (entry) =>
          !excluded?.depositEventIds.has(
            entry[DepositsDB.Columns.ID].toString("hex"),
          ),
      ).length,
      forcedTransactions: forcedTransactionEntries.filter(
        (entry) =>
          !excluded?.forcedTransactionEventIds.has(
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          ),
      ).length,
      withdrawals: withdrawalEntries.filter(
        (entry) =>
          !excluded?.withdrawalEventIds.has(
            entry[WithdrawalsDB.Columns.ID].toString("hex"),
          ),
      ).length,
    };
  });

export const pendingUserEventCountUpTo = (
  effectiveEndTime: Date,
  excluded?: {
    readonly depositEventIds: ReadonlySet<string>;
    readonly forcedTransactionEventIds: ReadonlySet<string>;
    readonly withdrawalEventIds: ReadonlySet<string>;
  },
): Effect.Effect<number, DatabaseError, Database> =>
  pendingUserEventCountsUpTo(effectiveEndTime, excluded).pipe(
    Effect.map(
      ({ deposits, forcedTransactions, withdrawals }) =>
        deposits + forcedTransactions + withdrawals,
    ),
  );

export const shouldHydrateCommitBaseEntries = ({
  payloadRootCheck,
  recordCorpus,
  candidateTxCount,
  pendingForcedTransactionCount,
  pendingWithdrawalCount,
}: {
  readonly payloadRootCheck: string;
  readonly recordCorpus: string;
  readonly candidateTxCount: number;
  readonly pendingForcedTransactionCount: number;
  readonly pendingWithdrawalCount: number;
}): boolean =>
  payloadRootCheck === "every_block" ||
  recordCorpus.trim().length > 0 ||
  candidateTxCount > 0 ||
  pendingForcedTransactionCount > 0 ||
  pendingWithdrawalCount > 0;
