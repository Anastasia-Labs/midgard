import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  BASE_HEADER,
  BASE_OUT,
  bytes,
  hex,
  insertJournal,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  journal,
  queueNode,
  root,
  S_COMMIT,
  S_HEADER,
  W_COMMIT,
  W_HEADER,
  W_NODE_OUT,
  W_NODE_TX,
  wHoldsTheSlot,
} from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  type Fixture,
  ledgerRoot,
  observerSees,
  onNode,
  ownerModel,
  plans,
  revival,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
} from "./helpers/history-expired-intent-release-preparation.js";

/** Rollback past confirmation depth can switch a base slot's winner. Root-moving
 * displacements must reconcile retained native/SQL obligations across each stop. */

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
const REVIVAL_SOURCE = "history_replaced_block_revival";
const S_NODE_TX = hex("reversal:s-node-tx");
const S_NODE_OUT = `${S_NODE_TX}#0`;

/** The queue after the deep rollback: D links to S, whose node is on it. */
const sHoldsTheSlot = {
  root,
  nodes: [
    root,
    queueNode(
      BASE_HEADER.toString("hex"),
      root.headerHash,
      S_HEADER.toString("hex"),
      BASE_OUT,
    ),
    queueNode(
      S_HEADER.toString("hex"),
      BASE_HEADER.toString("hex"),
      undefined,
      S_NODE_OUT,
    ),
  ],
} as never;

/** `tx`'s node output 6 blocks deep at head height 10, past the depth 3. */
const deep = (tx: string) => ({ head: 10, start: 1, txs: { [tx]: 5 } });

const sql = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => Effect.flatMap(SqlClient.SqlClient, statement);

/** The correction observer's cursor queue: the root, then `node`. */
const observerNow = (node: { headerHash: string; outRef: string }) =>
  sql((sql) => sql`DELETE FROM state_queue_terminal_observer_states`).pipe(
    Effect.zipRight(observerSees([node])),
  );

const outcome = Effect.gen(function* () {
  return {
    w: (yield* statusOf(W_HEADER))?.status,
    s: (yield* statusOf(S_HEADER))?.status,
    plans: (yield* plans).map(({ state }) => state),
    ledger: yield* ledgerRoot,
  };
});

/** W revived over S, W locally finalized, then the rollback deeper than the
 * confirmation depth lands S again, and the revival runs twice. */
const reversal = (
  wChangedTheLedger: boolean,
  stopAfterCas = false,
  rollbackAfterCas = false,
  stopInverse = false,
  repeatCycle = false,
) => {
  const owner = ownerModel(UTXOS_ROOT);
  return onNode(
    owner,
    (node) =>
      Effect.gen(function* () {
        yield* journal(
          W_HEADER,
          Pending.Status.Abandoned,
          W_COMMIT,
          2_000_000,
          {
            abandonment: "replacement",
            empty: !wChangedTheLedger,
          },
        );
        yield* withNativeReplay(W_HEADER);
        yield* journal(
          S_HEADER,
          Pending.Status.LocallyApplied,
          S_COMMIT,
          3_000_000,
          {
            empty: true,
          },
        );
        yield* observerNow({
          headerHash: W_HEADER.toString("hex"),
          outRef: W_NODE_OUT,
        });
        fixture.queue = wHoldsTheSlot;
        fixture.coverage = deep(W_NODE_TX);
        const forward = yield* revival(node);
        const revived = yield* outcome;
        // W's local finalization completes.
        yield* sql(
          (sql) => sql`UPDATE pending_block_finalizations
            SET status = ${Pending.Status.LocallyApplied}
            WHERE header_hash = ${W_HEADER}`,
        );
        owner.durableRoot = wChangedTheLedger ? "00".repeat(32) : UTXOS_ROOT;
        yield* sql(
          (sql) =>
            sql`UPDATE mpf_engine_state SET root_hex = ${owner.durableRoot} WHERE store_name = 'ledger'`,
        );
        // The rollback deeper than the confirmation depth: S holds D's slot
        // again, deep, and W's commit is gone from the canonical history.
        yield* observerNow({
          headerHash: S_HEADER.toString("hex"),
          outRef: S_NODE_OUT,
        });
        fixture.queue = sHoldsTheSlot;
        fixture.coverage = deep(S_NODE_TX);
        if (stopAfterCas)
          owner.afterRestore = async () => {
            throw new Error("stop after displacement CAS");
          };
        const interrupted = stopAfterCas ? yield* revival(node) : undefined;
        const retained = yield* plans;
        owner.afterRestore = undefined;
        if (rollbackAfterCas) {
          yield* observerNow({
            headerHash: W_HEADER.toString("hex"),
            outRef: W_NODE_OUT,
          });
          fixture.queue = wHoldsTheSlot;
          fixture.coverage = deep(W_NODE_TX);
        }
        if (stopInverse)
          owner.afterRestore = async () => {
            throw new Error("stop after inverse CAS");
          };
        const inverseInterrupted = stopInverse
          ? yield* revival(node)
          : undefined;
        const inverseRetained = yield* plans;
        owner.afterRestore = undefined;
        const reversed = [yield* revival(node), yield* revival(node)];
        const cycleAttempts = [];
        if (repeatCycle) {
          yield* sql(
            (sql) =>
              sql`UPDATE pending_block_finalizations SET status = ${Pending.Status.LocallyApplied} WHERE header_hash = ${S_HEADER}`,
          );
          yield* observerNow({
            headerHash: W_HEADER.toString("hex"),
            outRef: W_NODE_OUT,
          });
          fixture.queue = wHoldsTheSlot;
          fixture.coverage = deep(W_NODE_TX);
          cycleAttempts.push(yield* revival(node));
          yield* sql(
            (sql) =>
              sql`UPDATE pending_block_finalizations SET status = ${Pending.Status.LocallyApplied} WHERE header_hash = ${W_HEADER}`,
          );
          owner.durableRoot = "00".repeat(32);
          yield* sql(
            (sql) =>
              sql`UPDATE mpf_engine_state SET root_hex = ${owner.durableRoot} WHERE store_name = 'ledger'`,
          );
          yield* observerNow({
            headerHash: S_HEADER.toString("hex"),
            outRef: S_NODE_OUT,
          });
          fixture.queue = sHoldsTheSlot;
          fixture.coverage = deep(S_NODE_TX);
          cycleAttempts.push(yield* revival(node));
        }

        return {
          cycleAttempts,
          cyclePlans: yield* plans,
          inverseInterrupted,
          inverseRetained,
          operations: owner.operations,
          forward,
          revived,
          reversed,
          after: yield* outcome,
          durableRoot: owner.durableRoot,
          restores: owner.restores,
          interrupted,
          retained: retained.map(({ state }) => state),
        };
      }),
    UTXOS_ROOT,
  );
};

describe("a displacement undone by a rollback deeper than the confirmation depth", () => {
  it("revives the sibling again over the displaced winner when the winner changed no ledger state", async () => {
    const result = await reversal(false);
    expect(result.forward.failure).toBeUndefined();
    expect(result.revived).toEqual({
      w: Pending.Status.ObservedWaitingStability,
      s: Pending.Status.Abandoned,
      plans: [],
      ledger: UTXOS_ROOT,
    });
    for (const attempt of result.reversed) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(REVIVAL_SOURCE)).toBeUndefined();
    }
    // S is revived; W is abandoned as displaced, revivable again in turn.
    expect(result.after).toEqual({
      w: Pending.Status.Abandoned,
      s: Pending.Status.ObservedWaitingStability,
      plans: [],
      ledger: UTXOS_ROOT,
    });
  });

  it("rewinds the displaced winner's native root and revives the sibling after a rollback", async () => {
    const result = await reversal(true);
    expect(result.forward.failure).toBeUndefined();
    expect(result.revived.w).toBe(Pending.Status.ObservedWaitingStability);
    expect(result.revived.s).toBe(Pending.Status.Abandoned);
    for (const attempt of result.reversed) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(REVIVAL_SOURCE)).toBeUndefined();
      expect(attempt.reasons).not.toContain(INTEGRITY);
    }
    expect(result.after).toEqual({
      w: Pending.Status.Abandoned,
      s: Pending.Status.ObservedWaitingStability,
      plans: ["applied"],
      ledger: UTXOS_ROOT,
    });
    expect(result.durableRoot).toBe(UTXOS_ROOT);
    expect(result.restores).toBe(1);
  });

  it("resumes the same displaced-chain plan after its native CAS and before SQL repair", async () => {
    const result = await reversal(true, true);
    expect(result.interrupted?.failure).toContain(
      "stop after displacement CAS",
    );
    expect(result.retained).toEqual(["prepared"]);
    expect(result.after).toEqual({
      w: Pending.Status.Abandoned,
      s: Pending.Status.ObservedWaitingStability,
      plans: ["applied"],
      ledger: UTXOS_ROOT,
    });
    expect(result.durableRoot).toBe(UTXOS_ROOT);
    expect(result.restores).toBe(2);
    for (const attempt of result.reversed) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.reasons).not.toContain(INTEGRITY);
    }
  });

  it.each([false, true])(
    "reconciles a root-moving sibling and descendant (only prefix returns: %s)",
    async (prefixReturns) => {
      const child = bytes("reversal:child", 28);
      const siblingRoot = "11".repeat(32);
      const childRoot = "22".repeat(32);
      const owner = ownerModel(childRoot);
      const result = await onNode(
        owner,
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
              Pending.Status.LocallyApplied,
              S_COMMIT,
              3_000_000,
            );
            yield* sql(
              (sql) =>
                sql`UPDATE pending_block_finalizations SET expected_utxos_root = ${siblingRoot} WHERE header_hash = ${S_HEADER}`,
            );
            yield* withNativeReplay(S_HEADER);
            yield* insertJournal({
              header: child,
              status: Pending.Status.LocallyApplied,
              commit: signedCommit(S_NODE_OUT, TTL + 5),
              baseOut: S_NODE_OUT,
              baseHeader: S_HEADER,
              createdAt: new Date(3_500_000),
            });
            yield* sql(
              (sql) => sql`UPDATE pending_block_finalizations SET
        base_utxos_root = ${siblingRoot}, expected_utxos_root = ${childRoot},
        block_end_time = block_start_time + INTERVAL '1 second' WHERE header_hash = ${child}`,
            );
            yield* withNativeReplay(child);
            yield* observerSees([
              { headerHash: W_HEADER.toString("hex"), outRef: W_NODE_OUT },
            ]);
            fixture.queue = wHoldsTheSlot;
            fixture.coverage = deep(W_NODE_TX);
            if (prefixReturns)
              owner.afterRestore = async () => {
                throw new Error("stop after displacement CAS");
              };
            const recovered = yield* revival(node);
            owner.afterRestore = undefined;
            if (prefixReturns) {
              yield* observerNow({
                headerHash: S_HEADER.toString("hex"),
                outRef: S_NODE_OUT,
              });
              fixture.queue = sHoldsTheSlot;
              fixture.coverage = deep(S_NODE_TX);
            }
            const again = yield* revival(node);
            return {
              recovered,
              again,
              after: yield* outcome,
              child: (yield* statusOf(child))?.status,
            };
          }),
        childRoot,
      );
      if (prefixReturns) {
        expect(result.recovered.failure).toContain(
          "stop after displacement CAS",
        );
        expect(result.again.failure).toBeUndefined();
        expect(result.after.plans).toEqual(["applied"]);
        expect(result.child).toBe(Pending.Status.Abandoned);
        expect(result.after.s).toBe(Pending.Status.LocallyApplied);
        expect(owner.durableRoot).toBe(siblingRoot);
        expect(result.after.ledger).toBe(siblingRoot);
        expect(owner.restores).toBe(2);
        return;
      }
      expect(result.recovered.failure).toBeUndefined();
      expect(result.recovered.reasons).not.toContain(INTEGRITY);
      expect(result.again.failure).toBeUndefined();
      expect(result.after).toEqual({
        w: Pending.Status.ObservedWaitingStability,
        s: Pending.Status.Abandoned,
        plans: ["applied"],
        ledger: "00".repeat(32),
      });
      expect(result.child).toBe(Pending.Status.Abandoned);
      expect(owner.durableRoot).toBe(UTXOS_ROOT);
      expect(owner.restores).toBe(1);
    },
  );
});

describe("retained displacement after a branch return", () => {
  it("resumes a lost inverse acknowledgement with the original closure still retained", async () => {
    const result = await reversal(true, true, true, true);
    expect(result.inverseInterrupted?.failure).toContain(
      "stop after inverse CAS",
    );
    expect(result.inverseRetained.map(({ state }) => state)).toEqual([
      "prepared",
    ]);
    expect(result.operations).toHaveLength(2);
    expect(result.operations[1]).not.toBe(
      result.inverseRetained[0]?.recovery_id,
    );
    expect(result.after.plans).toEqual([]);
    expect(result.durableRoot).toBe(result.after.ledger);
    expect(result.after.w).toBe(Pending.Status.LocallyApplied);
    expect(result.after.s).toBe(Pending.Status.Abandoned);
    for (const attempt of result.reversed)
      expect(attempt.failure).toBeUndefined();
  });
  it("restores consistent native and SQL roots before retiring the moot displacement plan", async () => {
    const result = await reversal(true, true, true);
    expect(result.interrupted?.failure).toContain(
      "stop after displacement CAS",
    );
    expect(result.retained).toEqual(["prepared"]);
    expect(result.after.plans).not.toContain("prepared");
    expect(result.after).toMatchObject({
      w: Pending.Status.LocallyApplied,
      s: Pending.Status.Abandoned,
      ledger: "00".repeat(32),
    });
    expect(result.durableRoot).toBe(result.after.ledger);
    for (const attempt of result.reversed)
      expect(attempt.failure).toBeUndefined();
  });
});

describe("a later displacement with identical signed journals", () => {
  it("issues a fresh durable attempt identity and repairs SQL instead of reusing an old applied receipt", async () => {
    const result = await reversal(true, false, false, false, true);
    expect(result.durableRoot).toBe(result.after.ledger);
    expect(result.after.w).toBe(Pending.Status.Abandoned);
    expect(result.after.s).toBe(Pending.Status.ObservedWaitingStability);
    expect(result.cyclePlans).toHaveLength(2);
    expect(
      new Set(result.cyclePlans.map(({ recovery_id }) => recovery_id)).size,
    ).toBe(2);
    expect(result.cyclePlans.every(({ state }) => state === "applied")).toBe(
      true,
    );
    for (const attempt of result.cycleAttempts)
      expect(attempt.failure).toBeUndefined();
  });
});
