import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED } from "../src/services/liveness-halt.js";
import {
  bytes,
  insertJournal,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  journal,
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
  observerSees,
  onNode,
  ownerModel,
  revival,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
} from "./helpers/history-expired-intent-release-preparation.js";
import {
  deep,
  observerNow,
  outcome,
  reversalOn,
  S_NODE_OUT,
  S_NODE_TX,
  sHoldsTheSlot,
  sql,
} from "./helpers/history-expired-intent-release-reversal-scenario.js";

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
const reversal = reversalOn(fixture);

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

describe("a retained displacement whose inverse restore is refused as not retained", () => {
  it("holds under the revival source with its plan prepared, then completes once the root is retained", async () => {
    const result = await reversal(true, true, true, false, false, true);
    const held = result.inverseHeld!;
    for (const attempt of held.attempts) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(REVIVAL_SOURCE)).toBe(
        SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
      );
    }
    // Held: the displacement plan prepared, native MPF at its CAS target,
    // the SQL marker where W's finalization left it, no further restore.
    expect(held.plans).toEqual(result.inverseRetained);
    expect(held.plans.map(({ state }) => state)).toEqual(["prepared"]);
    expect(held.ledger).toBe("00".repeat(32));
    expect(held.native).not.toBe(held.ledger);
    // Only the displacement's own CAS ran; the refused inverse did not.
    expect(held.operations).toHaveLength(1);
    // Retained again: the inverse completes and the reason clears.
    for (const attempt of result.reversed) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(REVIVAL_SOURCE)).toBeUndefined();
    }
    expect(result.operations).toHaveLength(2);
    expect(result.after.plans).not.toContain("prepared");
    expect(result.after).toMatchObject({
      w: Pending.Status.LocallyApplied,
      s: Pending.Status.Abandoned,
      ledger: "00".repeat(32),
    });
    expect(result.durableRoot).toBe(result.after.ledger);
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
