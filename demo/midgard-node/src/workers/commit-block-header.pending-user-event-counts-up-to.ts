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
  FollowerLucidLive,
  Lucid,
  MidgardContracts,
  MidgardContractServices,
  NodeConfig,
} from "../services/index.js";
import {
  type IntentJournal,
  IntentJournalLive,
} from "../services/intent-journal.js";

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
    | MidgardContracts
    | ContractDeploymentIdentity
    | Database
    | NodeConfig
    | IntentJournal
  >,
): Effect.Effect<A, E | ConfigError | DatabaseInitializationError, never> =>
  pipe(
    effect,
    // The worker journals in the node database; the node's S6 reconciles.
    Effect.provide(IntentJournalLive),
    Effect.provide(MidgardContractServices),
    Effect.provide(Database.workerLayer),
    Effect.provide(NodeConfig.layer),
  );

/**
 * The Lucid a commit program builds and submits with. There is no default:
 * every caller chooses, so a role's follower Lucid is never built in a
 * command by omission (option E). The commit worker thread `listen` starts
 * passes `followerCommitLucidFactory`; a command passes its own tool Lucid.
 */
export type CommitLucidFactory = () => Effect.Effect<Lucid, ConfigError>;

/** The role factory: the commit worker thread's follower Lucid. */
export const followerCommitLucidFactory: CommitLucidFactory = () =>
  Effect.provide(Lucid, FollowerLucidLive);

/** A command's factory: the Lucid service its layers already provide (the
 * tool access `--l1` selects, from `cli-runtime`). */
export const environmentCommitLucidFactory: Effect.Effect<
  CommitLucidFactory,
  never,
  Lucid
> = Effect.map(
  Lucid,
  (lucid): CommitLucidFactory =>
    () =>
      Effect.succeed(lucid),
);

type PendingUserEventCounts = {
  readonly deposits: number;
  readonly forcedTransactions: number;
  readonly withdrawals: number;
};

export const pendingUserEventCountsUpTo = (
  effectiveEndTime: Date,
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
      deposits: depositEntries.length,
      forcedTransactions: forcedTransactionEntries.length,
      withdrawals: withdrawalEntries.length,
    };
  });

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
