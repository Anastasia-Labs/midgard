/**
 * The full verification of the landed ledger, off the commit path: in the
 * main thread, its roots derived in a worker thread of its own
 * (`workers/confirmed-ledger-root.ts`). It reads the frontier,
 * `confirmed_ledger` and the landed rows on one snapshot and builds one
 * trie: `confirmed_ledger` checked against the frontier's root, then the
 * processed deltas from the frontier to the queue tail applied in order,
 * each first checked as the commit checks it (`walkLandedLedger`), and
 * checked against the tail row's root. Every `confirmed_ledger` entry and
 * every stored delta byte up to the tail is under a root it is checked
 * against. A pass records the frontier in `VERIFIED_CONFIRMED_LEDGER`,
 * which every commit worker receives (`WorkerInput.verifiedFrontier`): a
 * commit on a landed tail builds from that frontier only, applying the
 * landed deltas after it (`verifiedLandedLedgerAt`), and waits as
 * `commit_base_ledger_unverified` while the frontier is another. A
 * divergence clears the record and is counted under its check,
 * `confirmed_ledger` at the frontier or `confirmed_ledger_tail` at the
 * tail, in `midgard_local_ledger_divergence_total`; the run raises
 * `commit_base_ledger_divergence:<check>` under
 * `confirmed_ledger_verification` for the first (the frontier's), with a
 * detail line for each. A run that fails before either raises
 * `confirmed_ledger_verification_failed` there instead, leaving the record
 * as it was. The next run that completes clears either.
 *
 * It runs at startup and once the frontier moves, each fold commit (the
 * frontier is watched every `watchIntervalMs`), and again every
 * `CONFIRMED_LEDGER_RECHECK_INTERVAL_MS`, which catches a row changed
 * outside a fold. One run at a time: one due while another runs is skipped,
 * not queued.
 */
import { Worker } from "node:worker_threads";

import { Cause, Duration, Effect, Either, Ref, Schedule } from "effect";

import {
  COMMIT_BASE_LEDGER_DIVERGENCE,
  CONFIRMED_LEDGER_VERIFICATION_FAILED,
} from "../fibers/block-commitment.worker-readiness.js";
import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import {
  type LandedLedger,
  landedLedger,
  onOneSnapshot,
} from "../landed-blocks/ledger.js";
import {
  Frontier,
  type HeaderRoot,
  retrieveRows,
} from "../landed-blocks/store.js";
import type { Database } from "../services/database.js";
import { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "../services/validation-pool.bundled-worker-exec-argv.js";
import { walkLandedLedger } from "../workers/commit-block-header.landed-commit-base.js";
import {
  type ConfirmedLedgerDeltaOp,
  type ConfirmedLedgerRoots,
  type ConfirmedLedgerRootsInput,
  packConfirmedLedgerRootsRequest,
  parseConfirmedLedgerRootsResponse,
} from "./confirmed-ledger-root-message.js";
import {
  countLedgerDivergence,
  type LedgerDivergence,
  ledgerDivergenceDetail,
} from "./local-ledger-divergence.js";

/** The liveness source the verification's divergence or failure is raised
 * under. */
export const CONFIRMED_LEDGER_VERIFICATION_SOURCE =
  "confirmed_ledger_verification";

/**
 * How long after a run the next runs regardless of the frontier: ten
 * minutes. A run's roots measured about 52 s at 100,000 entries (its
 * process near 2.1 GB resident), so at 100,000 entries the re-check holds
 * one core under 10% of the time; a run longer than the interval only
 * delays the next, which starts its delay once it ends.
 */
export const CONFIRMED_LEDGER_RECHECK_INTERVAL_MS = 10 * 60_000;

/**
 * The old-generation heap of the roots' worker thread, in MB: 2048. The
 * trie it builds, with 1,000 outrefs spent and 1,000 produced after the
 * frontier, took about 907 MB at 100,000 entries and 2,031 MB at 240,000
 * (135 s, the thread's process near 4.2 GB resident); 250,000 ran out of
 * this heap after 137 s, so a ledger past about 240,000 entries fails each
 * run as `confirmed_ledger_verification_failed`. At the limit the process
 * measured near 4.3 GB resident, inside the node's 8 GB container limit
 * with the native MPF store's 2 GiB. Node ignores the limit when the
 * process itself runs with `--max-old-space-size`; the thread then shares
 * the process's heap bound.
 */
export const CONFIRMED_LEDGER_ROOT_WORKER_HEAP_MB = 2048;

/** The roots of `input`, derived in a worker thread started for it. */
export const confirmedLedgerRootsInWorkerThread = (
  input: ConfirmedLedgerRootsInput,
): Effect.Effect<ConfirmedLedgerRoots, Error> =>
  Effect.async<ConfirmedLedgerRoots, Error>((resume) => {
    const { request, transfer } = packConfirmedLedgerRootsRequest(input);
    const worker = new Worker(
      resolveWorkerEntry(import.meta.url, "confirmed-ledger-root.js"),
      {
        workerData: request,
        transferList: transfer,
        execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
        resourceLimits: {
          maxOldGenerationSizeMb: CONFIRMED_LEDGER_ROOT_WORKER_HEAP_MB,
        },
      },
    );
    let settled = false;
    const settle = (outcome: Effect.Effect<ConfirmedLedgerRoots, Error>) => {
      if (settled) return;
      settled = true;
      void worker.terminate();
      resume(outcome);
    };
    worker.once("message", (message: unknown) =>
      settle(
        Effect.try({
          try: () => parseConfirmedLedgerRootsResponse(message),
          catch: (cause) =>
            cause instanceof Error ? cause : new Error(String(cause)),
        }),
      ),
    );
    worker.once("error", (error) => settle(Effect.fail(error)));
    worker.once("exit", (code) =>
      settle(
        Effect.fail(
          new Error(
            `confirmed_ledger root worker exited with code ${code.toString()} before posting its roots`,
          ),
        ),
      ),
    );
    return Effect.sync(() => {
      settled = true;
      void worker.terminate();
    });
  });

/** Derives the roots of an input; the worker thread's by default. */
export type DeriveConfirmedLedgerRoots = (
  input: ConfirmedLedgerRootsInput,
) => Effect.Effect<ConfirmedLedgerRoots, unknown>;

/** What a verification run found: at the frontier and the queue tail it
 * read (the frontier itself, with no processed row after it). A divergence
 * run names each check that failed, the frontier's first. */
export type ConfirmedLedgerVerification =
  | Readonly<{ kind: "no_frontier" }>
  | Readonly<{
      kind: "verified";
      frontier: HeaderRoot;
      tail: HeaderRoot;
      entries: number;
      durationMs: number;
    }>
  | Readonly<{
      kind: "diverged";
      frontier: HeaderRoot;
      tail: HeaderRoot;
      divergences: readonly [LedgerDivergence, ...LedgerDivergence[]];
      entries: number;
      durationMs: number;
    }>;

/** The changes `chain`'s rows make, in order: each row's spent outrefs,
 * then its produced entries. */
const landedDeltaOps = (
  chain: LandedLedger["chain"],
): ConfirmedLedgerDeltaOp[] =>
  chain.flatMap((row) => [
    ...row.spent.map((outref) => ({ kind: "spent" as const, outref })),
    ...row.produced.map((entry) => ({ kind: "produced" as const, ...entry })),
  ]);

/**
 * Reads the frontier, `confirmed_ledger` and the landed rows on one
 * snapshot and verifies them (`verifyLandedLedger`), with `deriveRoots`.
 */
export const verifyConfirmedLedger = (
  deriveRoots: DeriveConfirmedLedgerRoots = confirmedLedgerRootsInWorkerThread,
): Effect.Effect<ConfirmedLedgerVerification, unknown, Database> =>
  Effect.gen(function* () {
    const landed = yield* onOneSnapshot(
      Effect.flatMap(retrieveRows, landedLedger),
    );
    if (landed === undefined) return { kind: "no_frontier" } as const;
    return yield* verifyLandedLedger(landed, deriveRoots);
  });

/**
 * Verifies `landed` from one trie `deriveRoots` builds: `confirmed_ledger`
 * against the frontier's root (`confirmed_ledger`), then, with the
 * processed deltas to the queue tail applied in order, against the tail
 * row's root (`confirmed_ledger_tail`). Each delta is first checked to
 * apply as the commit checks it (`walkLandedLedger`); one that does not
 * is the tail's divergence, and the frontier is still checked. A
 * `confirmed_ledger` entry a delta spends is gone from the tail's ledger,
 * so only the frontier's check covers it; a delta byte only the tail's.
 */
export const verifyLandedLedger = (
  landed: LandedLedger,
  deriveRoots: DeriveConfirmedLedgerRoots = confirmedLedgerRootsInWorkerThread,
): Effect.Effect<ConfirmedLedgerVerification, unknown> =>
  Effect.gen(function* () {
    const startedAt = Date.now();
    const { frontier, chain } = landed;
    const last = chain.at(-1);
    const tail: HeaderRoot =
      last === undefined
        ? frontier
        : { headerHash: last.headerHash, utxosRoot: last.utxosRoot };
    const walked = yield* Effect.either(
      walkLandedLedger(landed, tail.headerHash, "confirmed_ledger_tail"),
    );
    if (Either.isRight(walked) && walked.right === undefined)
      return yield* Effect.dieMessage(
        `the queue tail ${tail.headerHash} is not on the processed chain it ends`,
      );
    const roots = yield* deriveRoots({
      confirmed: landed.confirmed,
      delta: Either.isLeft(walked) ? [] : landedDeltaOps(chain),
    });
    const divergences: LedgerDivergence[] = [];
    if (roots.frontierRoot !== frontier.utxosRoot)
      divergences.push({
        check: "confirmed_ledger",
        headerHash: frontier.headerHash,
        root: frontier.utxosRoot,
        derivedRoot: roots.frontierRoot,
      });
    if (Either.isLeft(walked)) divergences.push(walked.left);
    else if (last !== undefined && roots.tailRoot !== tail.utxosRoot)
      divergences.push({
        check: "confirmed_ledger_tail",
        headerHash: tail.headerHash,
        root: tail.utxosRoot,
        derivedRoot: roots.tailRoot,
      });
    const result = {
      frontier,
      tail,
      entries: landed.confirmed.length,
      durationMs: Date.now() - startedAt,
    };
    const [first, ...rest] = divergences;
    return first === undefined
      ? ({ kind: "verified", ...result } as const)
      : ({
          kind: "diverged",
          ...result,
          divergences: [first, ...rest],
        } as const);
  });

/** Records `outcome`: the verified frontier, or each divergence's count
 * and the first's reason, with one detail line per divergence; either
 * clears a failure an earlier run raised. */
export const recordConfirmedLedgerVerification = (
  globals: Pick<Globals, "LIVENESS_REASONS" | "VERIFIED_CONFIRMED_LEDGER">,
  outcome: ConfirmedLedgerVerification,
): Effect.Effect<void> => {
  switch (outcome.kind) {
    case "no_frontier":
      return Effect.zipRight(
        Ref.set(globals.VERIFIED_CONFIRMED_LEDGER, undefined),
        clearLivenessIncident(globals, CONFIRMED_LEDGER_VERIFICATION_SOURCE),
      );
    case "verified":
      return Effect.zipRight(
        Ref.set(globals.VERIFIED_CONFIRMED_LEDGER, {
          headerHash: outcome.frontier.headerHash,
          utxosRoot: outcome.frontier.utxosRoot,
        }),
        clearLivenessIncident(globals, CONFIRMED_LEDGER_VERIFICATION_SOURCE),
      );
    case "diverged":
      return Effect.all(
        [
          Ref.set(globals.VERIFIED_CONFIRMED_LEDGER, undefined),
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

const sameFrontier = (left: HeaderRoot | undefined, right: HeaderRoot) =>
  left?.headerHash === right.headerHash && left.utxosRoot === right.utxosRoot;

/**
 * The verification's fiber: a run at startup and whenever the frontier is
 * not the one the last completed run read, polled every `watchIntervalMs`,
 * and a run every `recheckIntervalMs` after the last. A run due while
 * another runs is skipped; a failed run raises
 * `confirmed_ledger_verification_failed` and the frontier it would have read
 * is retried at the next poll.
 */
export const confirmedLedgerVerificationFiber = ({
  watchIntervalMs,
  recheckIntervalMs = CONFIRMED_LEDGER_RECHECK_INTERVAL_MS,
  deriveRoots,
}: {
  readonly watchIntervalMs: number;
  readonly recheckIntervalMs?: number;
  readonly deriveRoots?: DeriveConfirmedLedgerRoots;
}): Effect.Effect<void, never, Database | Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const permit = yield* Effect.makeSemaphore(1);
    const lastRead = yield* Ref.make<HeaderRoot | undefined>(undefined);
    const run = permit.withPermitsIfAvailable(1)(
      verifyConfirmedLedger(deriveRoots).pipe(
        Effect.tap((outcome) =>
          Effect.all(
            [
              recordConfirmedLedgerVerification(globals, outcome),
              Ref.set(
                lastRead,
                outcome.kind === "no_frontier" ? undefined : outcome.frontier,
              ),
              outcome.kind === "no_frontier"
                ? Effect.void
                : Effect.logInfo(
                    `confirmed_ledger verification at frontier ${outcome.frontier.headerHash}, tail ${outcome.tail.headerHash}: ${outcome.kind === "diverged" ? `diverged (${outcome.divergences.map(({ check }) => check).join(", ")})` : outcome.kind} (entries=${outcome.entries.toString()}, ${outcome.durationMs.toString()} ms)`,
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
      const frontier = yield* Frontier.retrieve;
      if (frontier !== undefined && !sameFrontier(yield* lastRead, frontier))
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
