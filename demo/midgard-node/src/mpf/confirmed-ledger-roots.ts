/**
 * The landed ledger's two roots from one pass over `confirmed_ledger`'s
 * leaves in path order: `confirmed_ledger`'s at the frontier, and the queue
 * tail's, with the processed deltas after the frontier applied in order.
 * Each root is built by a `SortedMpfRoot`, so a pass holds the deltas, the
 * leaves it is given at a time and the open branches of the two tries,
 * however many entries `confirmed_ledger` has. The tail's trie takes each
 * leaf no delta names, merged in path order with the leaf each named
 * outref is left with after its last change, if it was last produced; each
 * named outref's first change must apply to `confirmed_ledger` (a spent
 * one held, a produced one not), and each later one to the change before
 * it. Run in the verification's worker thread
 * (`workers/confirmed-ledger-root.ts`), or in the calling thread
 * (`deriveConfirmedLedgerRoots`).
 */
import { Effect, type Scope } from "effect";

import type * as Ledger from "../database/utils/ledger.js";
import type {
  ConfirmedLedgerDeltaOp,
  ConfirmedLedgerRoots,
  ConfirmedLedgerRootsInput,
} from "./confirmed-ledger-root-message.js";
import { ledgerEntryToInsertBatchOp } from "./ledger-delta.js";
import { mpfPath, mpfValueDigest, SortedMpfRoot } from "./mpf-sorted-root.js";

/** A packed leaf's bytes: its 32-byte path, then its value's digest. */
export const LEAF_BYTES = 64;
const PATH_BYTES = 32;

/** `entries`' trie leaves, packed in their order. */
export const ledgerLeaves = (
  entries: readonly Ledger.MinimalEntry[],
): Uint8Array => {
  const leaves = new Uint8Array(entries.length * LEAF_BYTES);
  entries.forEach((entry, index) => {
    const { key, value } = ledgerEntryToInsertBatchOp(entry);
    leaves.set(mpfPath(key), index * LEAF_BYTES);
    leaves.set(mpfValueDigest(value), index * LEAF_BYTES + PATH_BYTES);
  });
  return leaves;
};

const pathAt = (leaves: Uint8Array, index: number) =>
  leaves.subarray(index * LEAF_BYTES, index * LEAF_BYTES + PATH_BYTES);

const comparePaths = (left: Uint8Array, right: Uint8Array) =>
  Buffer.compare(left, right);

/** Packed `leaves` in path order. */
export const sortedLeaves = (leaves: Uint8Array): Uint8Array => {
  const count = leaves.length / LEAF_BYTES;
  const order = Array.from({ length: count }, (_, index) => index).sort(
    (left, right) => comparePaths(pathAt(leaves, left), pathAt(leaves, right)),
  );
  const sorted = new Uint8Array(leaves.length);
  order.forEach((from, to) =>
    sorted.set(
      leaves.subarray(from * LEAF_BYTES, (from + 1) * LEAF_BYTES),
      to * LEAF_BYTES,
    ),
  );
  return sorted;
};

/** An outref the deltas name: its path, its first change, the leaf its
 * last change leaves (none if it was spent), and whether
 * `confirmed_ledger` holds it. */
type Named = {
  readonly outref: string;
  readonly path: Uint8Array;
  readonly firstSpent: boolean;
  last: Uint8Array | undefined;
  held: boolean;
};

/** The outrefs `delta` names, in path order; throws on a change that does
 * not apply to the change before it. */
const namedOutRefs = (delta: readonly ConfirmedLedgerDeltaOp[]): Named[] => {
  const byOutRef = new Map<string, Named>();
  for (const op of delta) {
    const outref = op.outref.toString("hex");
    const last =
      op.kind === "produced"
        ? mpfValueDigest(ledgerEntryToInsertBatchOp(op).value)
        : undefined;
    const named = byOutRef.get(outref);
    if (named === undefined) {
      byOutRef.set(outref, {
        outref,
        path: mpfPath(op.outref),
        firstSpent: op.kind === "spent",
        last,
        held: false,
      });
      continue;
    }
    if ((named.last !== undefined) === (op.kind === "produced"))
      throw new Error(
        op.kind === "produced"
          ? `the delta produces ${outref} again before spending it`
          : `the delta spends ${outref} again before producing it`,
      );
    named.last = last;
  }
  return [...byOutRef.values()].sort((left, right) =>
    comparePaths(left.path, right.path),
  );
};

/**
 * Takes `confirmed_ledger`'s packed leaves in strictly increasing path
 * order, over any number of `feed`s, and gives both roots once.
 */
export class ConfirmedLedgerRootsBuilder {
  readonly #frontier = new SortedMpfRoot();
  readonly #tail = new SortedMpfRoot();
  readonly #named: readonly Named[];
  #next = 0;

  constructor(delta: readonly ConfirmedLedgerDeltaOp[]) {
    this.#named = namedOutRefs(delta);
  }

  feed(leaves: Uint8Array): void {
    if (leaves.length % LEAF_BYTES !== 0)
      throw new Error("packed leaves are not whole 64-byte leaves");
    for (let at = 0; at < leaves.length; at += LEAF_BYTES) {
      const path = leaves.subarray(at, at + PATH_BYTES);
      const digest = leaves.subarray(at + PATH_BYTES, at + LEAF_BYTES);
      this.#frontier.push(path, digest);
      this.#mergeNamedBefore(path);
      const named = this.#named[this.#next];
      if (named !== undefined && comparePaths(named.path, path) === 0) {
        named.held = true;
        this.#mergeNamed(named);
      } else this.#tail.push(path, digest);
    }
  }

  roots(): ConfirmedLedgerRoots {
    this.#mergeNamedBefore(undefined);
    const misapplied = this.#named.find(
      (named) => named.firstSpent !== named.held,
    );
    if (misapplied !== undefined)
      throw new Error(
        misapplied.firstSpent
          ? `the delta spends ${misapplied.outref}, which confirmed_ledger does not hold`
          : `the delta produces ${misapplied.outref}, which confirmed_ledger already holds`,
      );
    return { frontierRoot: this.#frontier.root(), tailRoot: this.#tail.root() };
  }

  /** Merges into the tail's trie each named outref before `path` (all
   * that are left, without one). */
  #mergeNamedBefore(path: Uint8Array | undefined): void {
    for (
      let named = this.#named[this.#next];
      named !== undefined &&
      (path === undefined || comparePaths(named.path, path) < 0);
      named = this.#named[this.#next]
    )
      this.#mergeNamed(named);
  }

  #mergeNamed(named: Named): void {
    if (named.last !== undefined) this.#tail.push(named.path, named.last);
    this.#next += 1;
  }
}

/** A pass's session: the packed leaves of a page of `confirmed_ledger`
 * entries, a feed of leaves in path order, and the two roots once fed. */
export type ConfirmedLedgerRootsSession = Readonly<{
  leaves: (
    entries: readonly Ledger.MinimalEntry[],
  ) => Effect.Effect<Uint8Array, unknown>;
  feed: (leaves: Uint8Array) => Effect.Effect<void, unknown>;
  roots: Effect.Effect<ConfirmedLedgerRoots, unknown>;
}>;

/** Starts a session over the processed deltas `delta`, for the scope; the
 * worker thread's by default (`confirmed-ledger-root-worker.ts`). */
export type DeriveConfirmedLedgerRoots = (
  delta: readonly ConfirmedLedgerDeltaOp[],
) => Effect.Effect<ConfirmedLedgerRootsSession, unknown, Scope.Scope>;

const attempt = <A>(run: () => A) =>
  Effect.try({
    try: run,
    catch: (cause) =>
      cause instanceof Error ? cause : new Error(String(cause)),
  });

/** A session in the calling thread. */
export const deriveConfirmedLedgerRoots: DeriveConfirmedLedgerRoots = (delta) =>
  attempt(() => new ConfirmedLedgerRootsBuilder(delta)).pipe(
    Effect.map((builder) => ({
      leaves: (entries) => attempt(() => ledgerLeaves(entries)),
      feed: (leaves) => attempt(() => builder.feed(leaves)),
      roots: attempt(() => builder.roots()),
    })),
  );

/** The roots of `input`, held whole, from one session `deriveRoots`
 * starts: its entries' leaves sorted, then fed at once. */
export const confirmedLedgerRootsOf = (
  input: ConfirmedLedgerRootsInput,
  deriveRoots: DeriveConfirmedLedgerRoots = deriveConfirmedLedgerRoots,
): Effect.Effect<ConfirmedLedgerRoots, unknown> =>
  Effect.scoped(
    Effect.gen(function* () {
      const session = yield* deriveRoots(input.delta);
      const leaves = yield* session.leaves(input.confirmed);
      yield* session.feed(sortedLeaves(leaves));
      return yield* session.roots;
    }),
  );
