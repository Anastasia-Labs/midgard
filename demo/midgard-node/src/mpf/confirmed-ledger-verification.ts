/**
 * The full verification of the landed ledger, off the commit path: in the
 * main thread, its roots derived in a worker thread of its own
 * (`confirmed-ledger-root-worker.ts`). It reads the frontier, the landed
 * rows and `confirmed_ledger` on one snapshot, `confirmed_ledger` a page at
 * a time in trie order (`landed-blocks/ledger-leaves.ts`), and derives two
 * roots in one pass (`confirmed-ledger-roots.ts`): `confirmed_ledger`'s,
 * checked against the frontier's root, and, with the processed deltas from
 * the frontier to the queue tail applied in order, each first checked as
 * the commit checks it (`walkLandedLedger`), the tail's, checked against
 * the tail row's root. Every `confirmed_ledger` entry and every stored
 * delta byte up to the tail is under a root it is checked against, and the
 * run's memory does not grow with `confirmed_ledger`. A pass records the
 * frontier in `VERIFIED_CONFIRMED_LEDGER`, with the frontier generation
 * read on the same snapshot (migration 0038) and the digest of each delta
 * it checked, and every commit worker receives the frontier and generation
 * (`WorkerInput.verifiedFrontier`): a commit on a landed tail builds from
 * that frontier only, at that generation, applying the landed deltas after
 * it (`verifiedLandedLedgerAt`), and waits as
 * `commit_base_ledger_unverified` while the frontier or its generation is
 * another. A clean fold advances the record to the folded block, and a
 * pass is replayed over the folds logged since its run read
 * (`confirmed-ledger-advance.ts`); a pass that stays below the recorded
 * generation is ignored. A divergence clears the record and is counted
 * under its check, `confirmed_ledger` at the frontier or
 * `confirmed_ledger_tail` at the tail, in
 * `midgard_local_ledger_divergence_total`; the run raises
 * `commit_base_ledger_divergence:<check>` under
 * `confirmed_ledger_verification` for the first (the frontier's), with a
 * detail line for each. A run that fails before either raises
 * `confirmed_ledger_verification_failed` there instead, leaving the record
 * as it was. The next run that completes clears either.
 *
 * It runs at startup and once the frontier or its generation moves, each
 * fold commit and each write to `confirmed_ledger` (the frontier is watched
 * every `watchIntervalMs`), and again every
 * `CONFIRMED_LEDGER_RECHECK_INTERVAL_MS`, which catches a landed row changed
 * outside processing, and once the fold log overflows. One run at a time:
 * one due while another runs is skipped, not queued.
 */
import { Cause, Duration, Effect, MutableRef, Ref, Schedule } from "effect";

import {
  COMMIT_BASE_LEDGER_DIVERGENCE,
  CONFIRMED_LEDGER_VERIFICATION_FAILED,
} from "../fibers/block-commitment.worker-readiness.js";
import { Frontier, type GeneratedFrontier } from "../landed-blocks/store.js";
import type { Database } from "../services/database.js";
import { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import {
  confirmedLedgerRunDue,
  confirmedLedgerRunStarted,
  generationBelow,
  replayFolds,
  type VerifiedConfirmedLedger,
} from "./confirmed-ledger-advance.js";
import type { DeriveConfirmedLedgerRoots } from "./confirmed-ledger-roots.js";
import {
  type ConfirmedLedgerVerification,
  verifyConfirmedLedger,
} from "./confirmed-ledger-run.js";
import {
  countLedgerDivergence,
  ledgerDivergenceDetail,
} from "./local-ledger-divergence.js";

/** The liveness source the verification's divergence or failure is raised
 * under. */
export const CONFIRMED_LEDGER_VERIFICATION_SOURCE =
  "confirmed_ledger_verification";

/**
 * How long after a run the next runs regardless of the frontier: ten
 * minutes. A run's roots in the built worker measured about 295 s at
 * 1,000,000 entries, about 30 s per 100,000, with the whole process under
 * 740 MB resident (`tests/confirmed-ledger-root.bounded.compiled.test.ts`).
 * The interval bounds only an idle node's re-check: a frontier move starts
 * the next run as soon as one ends, so a node folding steadily keeps one
 * worker busy for about 30 s per 100,000 entries in each run. A run longer
 * than the interval only delays the next, which starts its delay once it
 * ends.
 */
export const CONFIRMED_LEDGER_RECHECK_INTERVAL_MS = 10 * 60_000;

/**
 * Records `outcome`: the verified frontier at its generation, or each
 * divergence's count and the first's reason, with one detail line per
 * divergence; either clears a failure an earlier run raised. A pass is
 * first replayed over the folds logged since (`replayFolds`), so a fold
 * committed while the run read moves it on, and is recorded when it reaches
 * the recorded generation or beyond: it replaces the record, with the
 * digests its run checked. One that stays below it is ignored: the record
 * names a later ledger, which a later run or a clean fold verified. A
 * divergence below the recorded generation is never replayed: it leaves
 * the record and readiness as they are, but is counted and logged, since
 * some writer produced a ledger that does not derive its root.
 */
export const recordConfirmedLedgerVerification = (
  globals: Pick<
    Globals,
    | "LIVENESS_REASONS"
    | "VERIFIED_CONFIRMED_LEDGER"
    | "CONFIRMED_LEDGER_FOLD_LOG"
  >,
  outcome: ConfirmedLedgerVerification,
): Effect.Effect<void> => {
  if (outcome.kind === "no_frontier")
    return Effect.zipRight(
      Ref.set(globals.VERIFIED_CONFIRMED_LEDGER, undefined),
      clearLivenessIncident(globals, CONFIRMED_LEDGER_VERIFICATION_SOURCE),
    );
  return Ref.modify(
    globals.VERIFIED_CONFIRMED_LEDGER,
    (recorded): [RecordedPass, VerifiedConfirmedLedger | undefined] => {
      if (outcome.kind === "diverged")
        return recorded !== undefined &&
          generationBelow(outcome.generation, recorded.generation)
          ? [{ recorded: false, at: recorded }, recorded]
          : [{ recorded: true }, undefined];
      const replay = replayFolds(
        {
          headerHash: outcome.frontier.headerHash,
          utxosRoot: outcome.frontier.utxosRoot,
          generation: outcome.generation,
          tailDigests: outcome.tailDigests,
        },
        MutableRef.get(globals.CONFIRMED_LEDGER_FOLD_LOG).folds,
      );
      return recorded === undefined ||
        !generationBelow(replay.record.generation, recorded.generation)
        ? [{ recorded: true, replay }, replay.record]
        : [{ recorded: false, at: recorded, replay }, recorded];
    },
  ).pipe(
    Effect.flatMap((pass) =>
      Effect.zipRight(
        pass.replay === undefined
          ? Effect.void
          : replayedLog(outcome.generation, pass.replay),
        pass.recorded
          ? recordedOutcome(globals, outcome)
          : outcome.kind === "verified"
            ? Effect.logInfo(
                `confirmed_ledger verification at generation ${outcome.generation} ignored: replayed to generation ${pass.replay?.record.generation ?? outcome.generation}, the record is at generation ${pass.at.generation}`,
              )
            : Effect.zipRight(
                Effect.forEach(outcome.divergences, countLedgerDivergence, {
                  discard: true,
                }),
                Effect.logWarning(
                  `confirmed_ledger verification at generation ${outcome.generation}, below the recorded generation ${pass.at.generation}, diverged; counted, the record and readiness left as they are:\n${outcome.divergences.map(ledgerDivergenceDetail).join("\n")}`,
                ),
              ),
      ),
    ),
  );
};

/** What the recorder did with an outcome: recorded it, or left the record
 * `at` a later generation; and a pass's replay over the fold log. */
type RecordedPass = Readonly<{ replay?: ReturnType<typeof replayFolds> }> &
  (
    | Readonly<{ recorded: true }>
    | Readonly<{ recorded: false; at: VerifiedConfirmedLedger }>
  );

/** The log line of a pass's replay over the folds made since its run read,
 * if it replayed any or stopped at one. */
const replayedLog = (
  generation: string,
  { record, replayed, stop }: ReturnType<typeof replayFolds>,
) =>
  replayed === 0 && stop === undefined
    ? Effect.void
    : (stop === undefined || stop.routine ? Effect.logInfo : Effect.logWarning)(
        `confirmed_ledger verification at generation ${generation} replayed ${replayed.toString()} folds made during the run, to ${record.headerHash} (generation ${record.generation})${stop === undefined ? "" : `; stopped at ${stop.fold.headerHash}: ${stop.fault}`}`,
      );

/** The incident an outcome recorded at or above the recorded generation
 * raises or clears, and the divergences it counts. */
const recordedOutcome = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  outcome: Exclude<ConfirmedLedgerVerification, { kind: "no_frontier" }>,
): Effect.Effect<void> => {
  switch (outcome.kind) {
    case "verified":
      return clearLivenessIncident(
        globals,
        CONFIRMED_LEDGER_VERIFICATION_SOURCE,
      );
    case "diverged":
      return Effect.all(
        [
          Effect.forEach(outcome.divergences, countLedgerDivergence, {
            discard: true,
          }),
          raiseLivenessIncident(
            globals,
            CONFIRMED_LEDGER_VERIFICATION_SOURCE,
            `${COMMIT_BASE_LEDGER_DIVERGENCE}:${outcome.divergences[0].check}`,
            outcome.divergences.map(ledgerDivergenceDetail).join("\n"),
          ),
        ],
        { discard: true },
      );
  }
};

/**
 * Records a run that failed before it passed or found a divergence:
 * raises `confirmed_ledger_verification_failed` with `cause`, leaving the
 * verified frontier as it was.
 */
export const recordConfirmedLedgerVerificationFailure = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  cause: Cause.Cause<unknown>,
): Effect.Effect<void> =>
  Effect.zipRight(
    Effect.logError("confirmed_ledger verification failed", cause),
    raiseLivenessIncident(
      globals,
      CONFIRMED_LEDGER_VERIFICATION_SOURCE,
      CONFIRMED_LEDGER_VERIFICATION_FAILED,
      `confirmed_ledger verification failed: ${Cause.pretty(cause)}`,
    ),
  );

const sameFrontier = (
  left: GeneratedFrontier | undefined,
  right: GeneratedFrontier,
) =>
  left?.headerHash === right.headerHash &&
  left.utxosRoot === right.utxosRoot &&
  left.generation === right.generation;

/**
 * The verification's fiber: a run at startup and whenever the frontier or
 * its generation is not the one the last completed run read, or the fold
 * log overflowed since the last run started (`confirmedLedgerRunDue`),
 * polled every `watchIntervalMs`, and a run every `recheckIntervalMs` after
 * the last. A run due while another runs is skipped, and the next poll
 * after it ends takes it; a failed run raises
 * `confirmed_ledger_verification_failed` and the frontier it would have read
 * is retried at the next poll.
 */
export const confirmedLedgerVerificationFiber = ({
  watchIntervalMs,
  recheckIntervalMs,
  deriveRoots,
}: {
  readonly watchIntervalMs: number;
  readonly recheckIntervalMs?: number;
  readonly deriveRoots?: DeriveConfirmedLedgerRoots;
}): Effect.Effect<void, never, Database | Globals> =>
  confirmedLedgerVerificationLoop({
    verify: verifyConfirmedLedger(deriveRoots),
    frontier: Frontier.retrieveWithGeneration,
    watchIntervalMs,
    ...(recheckIntervalMs === undefined ? {} : { recheckIntervalMs }),
  });

/** The verification's fiber (`confirmedLedgerVerificationFiber`) over
 * `verify`, a full run, and `frontier`, the frontier's read. */
export const confirmedLedgerVerificationLoop = <R>({
  verify,
  frontier: readFrontier,
  watchIntervalMs,
  recheckIntervalMs = CONFIRMED_LEDGER_RECHECK_INTERVAL_MS,
}: {
  readonly verify: Effect.Effect<ConfirmedLedgerVerification, unknown, R>;
  readonly frontier: Effect.Effect<GeneratedFrontier | undefined, unknown, R>;
  readonly watchIntervalMs: number;
  readonly recheckIntervalMs?: number;
}): Effect.Effect<void, never, R | Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const permit = yield* Effect.makeSemaphore(1);
    const lastRead = yield* Ref.make<GeneratedFrontier | undefined>(undefined);
    const run = permit.withPermitsIfAvailable(1)(
      Effect.zipRight(confirmedLedgerRunStarted(globals), verify).pipe(
        Effect.tap((outcome) =>
          Effect.all(
            [
              recordConfirmedLedgerVerification(globals, outcome),
              Ref.set(
                lastRead,
                outcome.kind === "no_frontier"
                  ? undefined
                  : { ...outcome.frontier, generation: outcome.generation },
              ),
              outcome.kind === "no_frontier"
                ? Effect.void
                : Effect.logInfo(
                    `confirmed_ledger verification at frontier ${outcome.frontier.headerHash} (generation ${outcome.generation}), tail ${outcome.tail.headerHash}: ${outcome.kind === "diverged" ? `diverged (${outcome.divergences.map(({ check }) => check).join(", ")})` : outcome.kind} (entries=${outcome.entries.toString()}, ${outcome.durationMs.toString()} ms)`,
                  ),
            ],
            { discard: true },
          ),
        ),
        Effect.catchAllCause((cause) =>
          recordConfirmedLedgerVerificationFailure(globals, cause),
        ),
      ),
    );
    const watch = Effect.gen(function* () {
      const frontier = yield* readFrontier;
      if (
        frontier !== undefined &&
        ((yield* confirmedLedgerRunDue(globals)) ||
          !sameFrontier(yield* lastRead, frontier))
      )
        yield* run;
    }).pipe(
      Effect.catchAllCause((cause) =>
        Effect.logError("confirmed_ledger frontier read failed", cause),
      ),
      Effect.repeat(Schedule.spaced(Duration.millis(watchIntervalMs))),
    );
    const recheck = run.pipe(
      Effect.delay(Duration.millis(recheckIntervalMs)),
      Effect.forever,
    );
    yield* Effect.all([watch, recheck], {
      concurrency: "unbounded",
      discard: true,
    });
  });
