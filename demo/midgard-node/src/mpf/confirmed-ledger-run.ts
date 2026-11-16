/**
 * One full verification run of the landed ledger
 * (`confirmed-ledger-verification.ts`): on one snapshot, the frontier at
 * its generation, the processed chain after it and `confirmed_ledger`,
 * read a page at a time in trie order (`landed-blocks/ledger-leaves.ts`),
 * and the two roots derived in one pass (`confirmed-ledger-roots.ts`), in
 * the worker thread by default (`confirmed-ledger-root-worker.ts`).
 */
import { Effect, Either } from "effect";

import { landedDeltaDigest } from "../landed-blocks/delta-digest.js";
import { type LandedLedger, processedChain } from "../landed-blocks/ledger.js";
import {
  confirmedLedgerEntriesNamed,
  feedConfirmedLedgerLeaves,
  onOneWritableSnapshot,
} from "../landed-blocks/ledger-leaves.js";
import {
  Frontier,
  type HeaderRoot,
  retrieveRows,
} from "../landed-blocks/store.js";
import type { Database } from "../services/database.js";
import { walkLandedLedger } from "../workers/commit-block-header.landed-commit-base.js";
import type { ConfirmedLedgerDeltaOp } from "./confirmed-ledger-root-message.js";
import { confirmedLedgerRootsInWorkerThread } from "./confirmed-ledger-root-worker.js";
import {
  type ConfirmedLedgerRootsSession,
  type DeriveConfirmedLedgerRoots,
  sortedLeaves,
} from "./confirmed-ledger-roots.js";
import type { LedgerDivergence } from "./local-ledger-divergence.js";

/** What a verification run found: at the frontier, the frontier
 * generation and the queue tail it read (the frontier itself, with no
 * processed row after it). A pass carries the digest of each delta it
 * checked, by header; a divergence names each check that failed, the
 * frontier's first. */
export type ConfirmedLedgerVerification =
  | Readonly<{ kind: "no_frontier" }>
  | Readonly<{
      kind: "verified";
      frontier: HeaderRoot;
      generation: string;
      tail: HeaderRoot;
      tailDigests: ReadonlyMap<string, string>;
      entries: number;
      durationMs: number;
    }>
  | Readonly<{
      kind: "diverged";
      frontier: HeaderRoot;
      generation: string;
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

/** A run's reads on its snapshot besides `confirmed_ledger` itself: the
 * frontier at its generation, the processed chain after it, and the
 * `confirmed_ledger` entries the chain's deltas name (`named`), all a
 * delta's check reads (`walkLandedLedger`). `feed` feeds every
 * `confirmed_ledger` leaf to a session in path order and returns how many
 * there were. */
type LandedLedgerRun<R> = Omit<LandedLedger, "confirmed"> &
  Readonly<{
    named: LandedLedger["confirmed"];
    feed: (
      session: ConfirmedLedgerRootsSession,
    ) => Effect.Effect<number, unknown, R>;
  }>;

/** Every outref `chain`'s deltas name, once. */
const namedOutRefs = (chain: LandedLedger["chain"]): Buffer[] => [
  ...new Map(
    chain
      .flatMap((row) => [
        ...row.spent,
        ...row.produced.map((entry) => entry.outref),
      ])
      .map((outRef) => [outRef.toString("hex"), outRef] as const),
  ).values(),
];

/**
 * Reads the frontier, the landed rows and `confirmed_ledger` on one
 * snapshot and verifies them (`verifyLandedRun`), with `deriveRoots`:
 * `confirmed_ledger` is fed a page at a time (`feedConfirmedLedgerLeaves`),
 * so the run's memory does not grow with it.
 */
export const verifyConfirmedLedger = (
  deriveRoots: DeriveConfirmedLedgerRoots = confirmedLedgerRootsInWorkerThread,
): Effect.Effect<ConfirmedLedgerVerification, unknown, Database> =>
  onOneWritableSnapshot(
    Effect.gen(function* () {
      const startedAt = Date.now();
      const rows = yield* retrieveRows;
      const read = yield* Frontier.retrieveWithGeneration;
      if (read === undefined) return { kind: "no_frontier" } as const;
      const { generation, ...frontier } = read;
      const chain = processedChain(rows, frontier.headerHash);
      return yield* verifyLandedRun(
        {
          frontier,
          generation,
          chain,
          named: yield* confirmedLedgerEntriesNamed(namedOutRefs(chain)),
          feed: feedConfirmedLedgerLeaves,
        },
        deriveRoots,
        startedAt,
      );
    }),
  );

/** `verifyLandedRun` over `landed`, held whole. */
export const verifyLandedLedger = (
  landed: LandedLedger,
  deriveRoots: DeriveConfirmedLedgerRoots = confirmedLedgerRootsInWorkerThread,
): Effect.Effect<ConfirmedLedgerVerification, unknown> =>
  verifyLandedRun(
    {
      ...landed,
      named: landed.confirmed,
      feed: (session) =>
        session.leaves(landed.confirmed).pipe(
          Effect.flatMap((leaves) => session.feed(sortedLeaves(leaves))),
          Effect.as(landed.confirmed.length),
        ),
    },
    deriveRoots,
  );

/**
 * Verifies `run` from one pass of a session `deriveRoots` starts:
 * `confirmed_ledger` against the frontier's root (`confirmed_ledger`),
 * then, with the processed deltas to the queue tail applied in order,
 * against the tail row's root (`confirmed_ledger_tail`). Each delta is
 * first checked to apply as the commit checks it (`walkLandedLedger`); one
 * that does not is the tail's divergence, and the frontier is still
 * checked. A `confirmed_ledger` entry a delta spends is gone from the
 * tail's ledger, so only the frontier's check covers it; a delta byte only
 * the tail's.
 */
const verifyLandedRun = <R>(
  { feed, named, ...landed }: LandedLedgerRun<R>,
  deriveRoots: DeriveConfirmedLedgerRoots,
  startedAt = Date.now(),
): Effect.Effect<ConfirmedLedgerVerification, unknown, R> =>
  Effect.gen(function* () {
    const { frontier, chain } = landed;
    const last = chain.at(-1);
    const tail: HeaderRoot =
      last === undefined
        ? frontier
        : { headerHash: last.headerHash, utxosRoot: last.utxosRoot };
    const walked = yield* Effect.either(
      walkLandedLedger(
        { ...landed, confirmed: named },
        tail.headerHash,
        "confirmed_ledger_tail",
      ),
    );
    if (Either.isRight(walked) && walked.right === undefined)
      return yield* Effect.dieMessage(
        `the queue tail ${tail.headerHash} is not on the processed chain it ends`,
      );
    const { roots, entries } = yield* Effect.scoped(
      Effect.gen(function* () {
        const session = yield* deriveRoots(
          Either.isLeft(walked) ? [] : landedDeltaOps(chain),
        );
        const entries = yield* feed(session);
        return { roots: yield* session.roots, entries };
      }),
    );
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
      generation: landed.generation,
      tail,
      entries,
      durationMs: Date.now() - startedAt,
    };
    const [first, ...rest] = divergences;
    return first === undefined
      ? ({
          kind: "verified",
          ...result,
          tailDigests: new Map(
            chain.map((row) => [row.headerHash, landedDeltaDigest(row)]),
          ),
        } as const)
      : ({
          kind: "diverged",
          ...result,
          divergences: [first, ...rest],
        } as const);
  });
