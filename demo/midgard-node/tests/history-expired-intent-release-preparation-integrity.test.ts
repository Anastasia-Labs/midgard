import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  journal,
  S_COMMIT,
  S_HEADER,
  seedDisplaced,
  SOURCE,
  W_COMMIT,
  W_HEADER,
  W_NODE_OUT,
  W_NODE_TX,
  wHoldsTheSlot,
  X_HEADER,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  ledgerRoot,
  observerSees,
  onNode,
  ownerModel,
  plans,
  release,
  revival,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

/**
 * A `SignedIntentReplacementIntegrityError` met by either history recovery
 * preparation is held, never a failure of the history owner (which its
 * supervisor would restart into the same evidence): the preparation
 * completes, `signed_intent_replacement_integrity` is raised once under its
 * source (readiness fails with every raised reason, see
 * readiness-liveness-reasons-route.test.ts), nothing is written, and the
 * reason clears once the evidence no longer shows two landed siblings. And
 * the replaced-block revival with no journal active decides a locally
 * finalized sibling an L1 rollback displaced exactly as the release does.
 */

const fixture = vi.hoisted(
  (): Fixture => ({ queue: undefined, coverage: "unavailable" }),
);

vi.mock("../src/l1-event-history-source.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.ledgerSnapshot(original),
  ),
);
vi.mock(
  "../src/services/history-expired-intent-release.signed-commit-node.js",
  (original) =>
    import(
      "./helpers/history-expired-intent-release-preparation.mocks.js"
    ).then((mocks) => mocks.queueAuthentication(original, fixture)),
);
vi.mock("../src/database/eventHistoryCanonicalCoverage.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.canonicalCoverage(original, fixture),
  ),
);
vi.mock("../src/workers/utils/commit-block-header.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.nodeSerialization(original),
  ),
);

const INTEGRITY = "signed_intent_replacement_integrity";
const UNDECIDED = "signed_intent_undecided";
const REVIVAL_SOURCE = "history_replaced_block_revival";

/** W's node output 6 blocks deep at head height 10 (past the depth of 3),
 * and, when `siblingLanded`, S's own signed commit in the canonical history
 * too: two landed blocks of one base. */
const coverage = (siblingLanded: boolean, depth = 6) => ({
  head: 10,
  start: 1,
  txs: {
    [W_NODE_TX]: 10 - depth + 1,
    ...(siblingLanded && { [S_COMMIT.hash]: 7 }),
  },
});

const outcome = (headers: readonly Buffer[]) =>
  Effect.gen(function* () {
    const statuses: (Pending.Status | undefined)[] = [];
    for (const header of headers)
      statuses.push((yield* statusOf(header))?.status);
    return {
      statuses,
      plans: (yield* plans).map(({ state }) => state),
      ledger: yield* ledgerRoot,
    };
  });

describe("an integrity failure met by the signed-intent release", () => {
  it("is held and raised under the release source, writes nothing, and clears once the evidence changes", async () => {
    fixture.queue = wHoldsTheSlot;
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(owner, (node) =>
      Effect.gen(function* () {
        yield* seedDisplaced(Pending.Status.Finalized);
        yield* withNativeReplay(X_HEADER);
        fixture.coverage = coverage(true);
        const first = yield* release(node);
        const second = yield* release(node);
        const held = {
          ...(yield* outcome([W_HEADER, S_HEADER, X_HEADER])),
          restores: owner.restores,
        };
        fixture.coverage = coverage(false);
        const settled = yield* release(node);
        return { first, second, held, settled };
      }),
    );
    for (const attempt of [result.first, result.second]) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(SOURCE)).toBe(INTEGRITY);
      expect(attempt.reasons).toContain(INTEGRITY);
    }
    // Raised once: one reason under the source, not one per evaluation.
    expect(result.second.reasons.filter((r) => r === INTEGRITY)).toHaveLength(
      1,
    );
    expect(result.held).toEqual({
      statuses: [
        Pending.Status.Abandoned,
        Pending.Status.Finalized,
        Pending.Status.PendingSubmission,
      ],
      plans: [],
      ledger: ZERO_ROOT,
      restores: 0,
    });
    // S's commit is gone from the canonical history: W alone landed, the
    // release re-lands it over the displaced S, and the hold clears.
    expect(result.settled.failure).toBeUndefined();
    expect(result.settled.raised.get(SOURCE)).toBeUndefined();
    expect(result.settled.reasons).not.toContain(INTEGRITY);
    expect(owner.restores).toBe(1);
  });
});

/** W replaced and abandoned, S landed beside it and locally finalized, no
 * journal active; the correction observer's cursor queue shows W. */
const revivalState = Effect.gen(function* () {
  yield* journal(W_HEADER, Pending.Status.Abandoned, W_COMMIT, 2_000_000, {
    abandonment: "replacement",
  });
  yield* journal(S_HEADER, Pending.Status.Finalized, S_COMMIT, 3_000_000, {
    empty: true,
  });
  yield* observerSees([
    { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
  ]);
});

describe("the replaced-block revival with no journal active", () => {
  it("holds both landed replacement candidates rather than choosing one", async () => {
    fixture.queue = wHoldsTheSlot;
    fixture.coverage = coverage(true);
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* journal(
            W_HEADER,
            Pending.Status.Abandoned,
            W_COMMIT,
            2_000_000,
            { abandonment: "replacement" },
          );
          yield* journal(
            S_HEADER,
            Pending.Status.Abandoned,
            S_COMMIT,
            3_000_000,
            { abandonment: "replacement" },
          );
          yield* observerSees([
            { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
            {
              headerHash: S_HEADER.toString("hex"),
              outRef: `${S_COMMIT.hash}#0`,
            },
          ]);
          const held = yield* revival(node);
          return { held, after: yield* outcome([W_HEADER, S_HEADER]) };
        }),
      UTXOS_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(REVIVAL_SOURCE)).toBe(INTEGRITY);
    expect(result.after).toEqual({
      statuses: [Pending.Status.Abandoned, Pending.Status.Abandoned],
      plans: [],
      ledger: UTXOS_ROOT,
    });
  });
  it("holds undecided short of the confirmation depth, then revives the winner over the displaced sibling", async () => {
    fixture.queue = wHoldsTheSlot;
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* revivalState;
          fixture.coverage = coverage(false, 1);
          const short = yield* revival(node);
          const before = yield* outcome([W_HEADER, S_HEADER]);
          fixture.coverage = coverage(false);
          const deep = yield* revival(node);
          return {
            short,
            before,
            deep,
            after: yield* outcome([W_HEADER, S_HEADER]),
            sibling: yield* statusOf(S_HEADER),
          };
        }),
      UTXOS_ROOT,
    );
    expect(result.short.failure).toBeUndefined();
    expect(result.short.raised.get(REVIVAL_SOURCE)).toBe(UNDECIDED);
    expect(result.short.reasons).toContain(UNDECIDED);
    expect(result.before).toEqual({
      statuses: [Pending.Status.Abandoned, Pending.Status.Finalized],
      plans: [],
      ledger: UTXOS_ROOT,
    });
    expect(result.deep.failure).toBeUndefined();
    expect(result.deep.raised.get(REVIVAL_SOURCE)).toBeUndefined();
    expect(result.deep.reasons).not.toContain(UNDECIDED);
    // S is abandoned (revivable, under its own replacement digest) in the
    // revival's transaction; W is revived and its marker follows it.
    expect(result.after).toEqual({
      statuses: [
        Pending.Status.ObservedWaitingStability,
        Pending.Status.Abandoned,
      ],
      plans: [],
      ledger: ZERO_ROOT,
    });
    expect(result.sibling?.digest).toBeDefined();
  });

  it("holds an integrity failure under the revival source and writes nothing", async () => {
    fixture.queue = wHoldsTheSlot;
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* revivalState;
          fixture.coverage = coverage(true);
          const held = yield* revival(node);
          const after = yield* outcome([W_HEADER, S_HEADER]);
          fixture.coverage = coverage(false);
          const settled = yield* revival(node);
          return { held, after, settled };
        }),
      UTXOS_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(REVIVAL_SOURCE)).toBe(INTEGRITY);
    expect(result.held.reasons).toContain(INTEGRITY);
    expect(result.after).toEqual({
      statuses: [Pending.Status.Abandoned, Pending.Status.Finalized],
      plans: [],
      ledger: UTXOS_ROOT,
    });
    expect(result.settled.failure).toBeUndefined();
    expect(result.settled.raised.get(REVIVAL_SOURCE)).toBeUndefined();
  });
});

/** S's journal moving the ledger root: it carried the replaced block's
 * reopened members. */
const sMovedTheRoot = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql`UPDATE pending_block_finalizations
    SET expected_utxos_root = ${"11".repeat(32)}
    WHERE header_hash = ${S_HEADER}`,
);

describe("a displaced sibling that moved the ledger root", () => {
  it("is held as the integrity failure by the release, which writes nothing", async () => {
    fixture.queue = wHoldsTheSlot;
    const owner = ownerModel(ZERO_ROOT);
    const result = await onNode(owner, (node) =>
      Effect.gen(function* () {
        yield* seedDisplaced(Pending.Status.Finalized);
        yield* sMovedTheRoot;
        yield* withNativeReplay(X_HEADER);
        fixture.coverage = coverage(false);
        const held = yield* release(node);
        return {
          held,
          after: {
            ...(yield* outcome([W_HEADER, S_HEADER, X_HEADER])),
            restores: owner.restores,
          },
        };
      }),
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(SOURCE)).toBe(INTEGRITY);
    expect(result.held.reasons).not.toContain(UNDECIDED);
    expect(result.after).toEqual({
      statuses: [
        Pending.Status.Abandoned,
        Pending.Status.Finalized,
        Pending.Status.PendingSubmission,
      ],
      plans: [],
      ledger: ZERO_ROOT,
      restores: 0,
    });
  });

  it("is held as the integrity failure by the revival with no journal active, which writes nothing", async () => {
    fixture.queue = wHoldsTheSlot;
    const result = await onNode(
      undefined,
      (node) =>
        Effect.gen(function* () {
          yield* revivalState;
          yield* sMovedTheRoot;
          fixture.coverage = coverage(false);
          const held = yield* revival(node);
          return { held, after: yield* outcome([W_HEADER, S_HEADER]) };
        }),
      UTXOS_ROOT,
    );
    expect(result.held.failure).toBeUndefined();
    expect(result.held.raised.get(REVIVAL_SOURCE)).toBe(INTEGRITY);
    expect(result.held.reasons).not.toContain(UNDECIDED);
    expect(result.after).toEqual({
      statuses: [Pending.Status.Abandoned, Pending.Status.Finalized],
      plans: [],
      ledger: UTXOS_ROOT,
    });
  });
});
