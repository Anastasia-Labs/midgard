/**
 * How both commit paths (`submitDepositOnlyCommit`, `submitTxBackedCommit`)
 * answer a failed submit attempt once its journal is prepared. The commit's
 * pre-broadcast gate journals the signed intent before any provider sees its
 * bytes (`submitWithDurableIntent`), so the journal alone tells whether the
 * commit can be on L1:
 *
 * - `retained`: the signed intent is journaled, so the commit may have been
 *   broadcast. The journal is kept for canonical reconciliation.
 * - `stale_tail`: nothing was sent, and the live tail moved under the
 *   attempt. It is abandoned, and the next tick rebuilds on the live tail.
 * - `pre_submit_deferred`: nothing was sent, because the commit is not yet
 *   valid. It is abandoned, and the next tick rebuilds it.
 * - `pre_broadcast_refused`: nothing was sent. The attempt is abandoned and
 *   retried with its payload kept, under the refusal's bounded code.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../../database/index.js";
import { COMMIT_ANCHOR_NOT_CANONICAL_MESSAGE } from "../../database/pendingBlockFinalizations.assert-canonical-event-members.js";
import { DatabaseError } from "../../database/utils/common.js";
import type { CommitSubmitRefusalCode } from "../../fibers/block-commitment.worker-readiness.js";
import type { Database } from "../../services/index.js";
import { IntentJournalRefused } from "../../services/intent-journal.refusals.js";
import {
  isNoInlineSubmitDefer,
  TxSignError,
  TxSubmitError,
} from "../../transactions/utils.js";
import type {
  CommitSubmitRefusal,
  WorkerOutput,
} from "../utils/commit-block-header.js";
import { isStaleCommitBaseError } from "./submission.commit-event-sources.js";

type CommitSubmitFailure =
  | { readonly kind: "retained" }
  | { readonly kind: "stale_tail" }
  | { readonly kind: "pre_submit_deferred" }
  | {
      readonly kind: "pre_broadcast_refused";
      readonly code: CommitSubmitRefusalCode;
    };

/** `error` and the causes under it. */
const causeChain = (error: unknown): readonly unknown[] => {
  const chain: unknown[] = [];
  for (
    let at = error;
    at !== undefined && at !== null && chain.length < 8;
    at = typeof at === "object" ? (at as { cause?: unknown }).cause : undefined
  )
    chain.push(at);
  return chain;
};

const refusalCode = (error: unknown): CommitSubmitRefusalCode => {
  const chain = causeChain(error);
  if (
    chain.some(
      (cause) =>
        cause instanceof DatabaseError &&
        cause.message === COMMIT_ANCHOR_NOT_CANONICAL_MESSAGE,
    )
  )
    return "COMMIT_ANCHOR_NOT_CANONICAL";
  if (chain.some((cause) => cause instanceof IntentJournalRefused))
    return "INTENT_JOURNAL_REFUSED";
  if (chain.some((cause) => cause instanceof TxSignError))
    return "SIGNING_FAILED";
  return "UNCLASSIFIED";
};

/** Classifies a commit's failed submit attempt by whether its signed intent
 * was journaled (`intentJournaled`), and otherwise by the failure. */
const classifyCommitSubmitFailure = (
  error: unknown,
  intentJournaled: boolean,
): CommitSubmitFailure => {
  if (intentJournaled) return { kind: "retained" };
  if (isStaleCommitBaseError(error)) return { kind: "stale_tail" };
  if (
    error instanceof TxSubmitError &&
    error.dueSlot !== undefined &&
    isNoInlineSubmitDefer(error.cause) &&
    error.cause.kind === "pre_submit_validity"
  )
    return { kind: "pre_submit_deferred" };
  return { kind: "pre_broadcast_refused", code: refusalCode(error) };
};

/**
 * The output of a commit attempt whose submit failed after its journal was
 * prepared under `headerHash`. `preservePayload` keeps the attempt's payload
 * for the retry of a refused submission and gives the output that says so.
 */
export const commitSubmitFailureOutput = ({
  error,
  headerHash,
  preservePayload,
}: {
  readonly error: unknown;
  readonly headerHash: Buffer;
  readonly preservePayload: (
    refusal: CommitSubmitRefusal,
  ) => Effect.Effect<WorkerOutput, never, Database>;
}): Effect.Effect<WorkerOutput, DatabaseError, Database> =>
  Effect.gen(function* () {
    const record =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    const failure = classifyCommitSubmitFailure(
      error,
      Option.isSome(record) &&
        record.value[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] !=
          null,
    );
    const detail = formatUnknownError(error);
    if (failure.kind === "retained")
      return {
        type: "FailureOutput",
        error: `Signed commit intent retained for canonical reconciliation: ${detail}`,
      } satisfies WorkerOutput;
    yield* PendingBlockFinalizationsDB.discardUnsignedJournal(headerHash).pipe(
      Effect.catchAll(() => Effect.void),
    );
    if (failure.kind === "stale_tail") {
      yield* Effect.logWarning(
        `🔹 Commit base became stale before submission; the next worker tick rebuilds on the live tail: ${detail}`,
      );
      return { type: "NothingToCommitOutput" } satisfies WorkerOutput;
    }
    if (failure.kind === "pre_submit_deferred") {
      yield* Effect.logInfo(`🔹 ${detail}; the next worker tick retries it.`);
      return { type: "NothingToCommitOutput" } satisfies WorkerOutput;
    }
    yield* Effect.logWarning(
      `🔹 Commit submission refused before broadcast (${failure.code}): ${detail}`,
    );
    return yield* preservePayload({ code: failure.code, detail });
  });
