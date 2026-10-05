import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect, Exit, Option } from "effect";
import { describe, expect } from "vitest";

import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import type { DatabaseError } from "../src/database/utils/common.js";
import { type Database, Lucid } from "../src/services/index.js";
import { TxSubmitError } from "../src/transactions/utils.js";
import { COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import {
  runWithStaleOperatorWalletRetry,
  signalStaleOperatorWalletRetry,
} from "../src/workers/commit-block-header/submission.run-with-stale-operator-wallet-retry.js";
import { signedCommit } from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  header,
  isolatedDb,
  journalFixture,
  Status,
} from "./local-mutation-job-abandonment.journal-fixture.js";

/**
 * Every attempt of a commit whose operator-wallet view stays stale prepares
 * its journal and then signals a retry; the signal discards the journal while
 * it holds no signed intent. Once the retries run out, the submit error that
 * caused them is what the commit reports.
 */

const fakeLucid = {
  switchToOperatorsMainWallet: Effect.void,
  api: {
    wallet: () => ({
      address: () => Promise.resolve("addr_test_operator"),
      getUtxos: () => Promise.resolve([]),
    }),
  },
} as unknown as Lucid;

const staleSubmit = (headerHash: Buffer) =>
  new TxSubmitError({
    message: "Stale operator-wallet input",
    cause: "BadInputsUTxO",
    txHash: headerHash.toString("hex").padEnd(64, "0"),
  });

const retryUntilExhausted = (
  attempt: (index: number) => Effect.Effect<Buffer, DatabaseError, Database>,
) => {
  let attempts = 0;
  return runWithStaleOperatorWalletRetry({
    label: "Commit",
    attempt: () =>
      Effect.gen(function* () {
        const pendingHeaderHash = yield* attempt(attempts);
        attempts += 1;
        return yield* signalStaleOperatorWalletRetry({
          pendingHeaderHash,
          error: staleSubmit(pendingHeaderHash),
          label: "Commit",
        });
      }),
  }).pipe(
    Effect.provideService(Lucid, fakeLucid),
    Effect.exit,
    Effect.map((exit) => ({ exit, attempts })),
  );
};

const failureOf = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit)
    ? Option.getOrUndefined(
        Exit.causeOption(exit).pipe(
          Option.flatMap((cause) =>
            Option.fromNullable(
              (cause as { _tag: string; error?: unknown }).error,
            ),
          ),
        ),
      )
    : undefined;

/** The commit's failure is the submit error, not a bookkeeping refusal that
 * replaced it. */
const expectSubmitError = (exit: Exit.Exit<unknown, unknown>) => {
  const failure = failureOf(exit);
  expect(
    failure,
    formatUnknownError(failure, { includeCause: true }),
  ).toBeInstanceOf(TxSubmitError);
  return failure as TxSubmitError;
};

const journalStatus = (headerHash: Buffer) =>
  Effect.map(
    PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
    (journal) =>
      Option.getOrUndefined(journal)?.[
        PendingBlockFinalizationsDB.Columns.STATUS
      ],
  );

const prepare = (label: string) =>
  Effect.gen(function* () {
    const headerHash = header(label);
    yield* PendingBlockFinalizationsDB.preparePendingSubmission(
      journalFixture(headerHash),
    );
    return headerHash;
  });

/** A journal whose signed commit is recorded: it may already be on L1. */
const prepareWithIntent = (label: string) =>
  Effect.gen(function* () {
    const headerHash = yield* prepare(label);
    const commit = signedCommit(`${"ab".repeat(32)}#0`, 1_000);
    const txHash = Buffer.from(commit.hash, "hex");
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE pending_block_finalizations
      SET prepared_tx_hash = ${txHash}, intended_tx_hash = ${txHash},
        signed_tx_cbor = ${commit.cbor}
      WHERE header_hash = ${headerHash}`;
    return headerHash;
  });

describe("a commit whose stale operator-wallet retries run out", () => {
  it.effect(
    "reports the submit error, with every intent-free journal discarded and no job left behind",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const prepared: Buffer[] = [];
          const { exit, attempts } = yield* retryUntilExhausted((index) =>
            prepare(`stale-attempt-${index.toString()}`).pipe(
              Effect.tap((headerHash) => prepared.push(headerHash)),
            ),
          );
          expect(attempts).toBe(COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES + 1);
          expect(expectSubmitError(exit).txHash).toBe(
            staleSubmit(prepared.at(-1)!).txHash,
          );
          for (const headerHash of prepared)
            expect(yield* journalStatus(headerHash)).toBeUndefined();
          expect(yield* MutationJobsDB.countUnfinished).toBe(0n);
        }),
      ),
  );

  it.effect(
    "never abandons a journal whose signed commit is recorded, and still reports the submit error",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const signed = yield* prepareWithIntent("stale-with-intent");
          const { exit } = yield* retryUntilExhausted(() =>
            Effect.succeed(signed),
          );
          expectSubmitError(exit);
          expect(yield* journalStatus(signed)).toBe(Status.PendingSubmission);
        }),
      ),
  );

  it.effect(
    "abandons an intent-free journal the discard missed, and still reports the submit error",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          // A transient refuses the discard; the journal survives it.
          yield* sql.unsafe(`CREATE OR REPLACE FUNCTION lf_job_refuse_delete()
            RETURNS trigger LANGUAGE plpgsql AS $$
            BEGIN
              RAISE EXCEPTION 'injected transient: discard refused';
            END $$`);
          yield* sql.unsafe(`CREATE TRIGGER lf_job_refuse_delete
            BEFORE DELETE ON pending_block_finalizations
            FOR EACH ROW EXECUTE FUNCTION lf_job_refuse_delete()`);
          const missed = yield* prepare("stale-discard-missed");
          const { exit } = yield* retryUntilExhausted(() =>
            Effect.succeed(missed),
          ).pipe(
            Effect.ensuring(
              sql
                .unsafe(
                  `DROP TRIGGER IF EXISTS lf_job_refuse_delete ON pending_block_finalizations`,
                )
                .pipe(Effect.orDie),
            ),
          );
          expectSubmitError(exit);
          expect(yield* journalStatus(missed)).toBe(Status.Abandoned);
        }),
      ),
  );
});
