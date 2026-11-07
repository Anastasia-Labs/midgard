import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Effect, Exit, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  COMMIT_HOLD_BASE_MS,
  COMMIT_HOLD_MAX_COUNTED_USER_EVENTS,
  COMMIT_HOLD_PER_TX_MS,
  COMMIT_HOLD_PER_USER_EVENT_MS,
  COMMIT_HOLD_STEP_DOWN_TX_WORK_FACTOR,
  commitHoldBudgetMs,
  extendCommitmentHoldForBacklog,
} from "../src/fibers/block-commitment.commit-hold-budget.js";
import { Globals, withL1ControlPlane } from "../src/services/globals.js";
import {
  extendL1ControlPlaneHold,
  L1_CONTROL_PLANE_HOLD_CEILING_MS,
} from "../src/services/globals.l1-control-plane.js";
import { commitDaFrameStepDownPassBound } from "../src/workers/utils/commit-block-planner.js";

/** The consensus cap on one block's L2 transactions (config refuses more). */
const CONSENSUS_MAX_L2_TX_COUNT =
  MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount;

const runWithGlobals = <A, E>(effect: Effect.Effect<A, E, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

describe("commitment hold budget", () => {
  it("keeps the old 180 s budget for an empty or small batch", () => {
    expect(
      commitHoldBudgetMs({
        mempoolTxCount: 0,
        maxL2TxCount: CONSENSUS_MAX_L2_TX_COUNT,
        pendingUserEventCount: 0,
        consecutiveHoldTimeouts: 0,
      }),
    ).toBe(COMMIT_HOLD_BASE_MS);
  });

  it("grows with a large batch past the fixed 180 s", () => {
    const budget = commitHoldBudgetMs({
      mempoolTxCount: 8_000,
      maxL2TxCount: CONSENSUS_MAX_L2_TX_COUNT,
      pendingUserEventCount: 500,
      consecutiveHoldTimeouts: 0,
    });
    expect(budget).toBeGreaterThan(180_000);
    // Every step-down pass rebuilds the events (15 passes at most for 8,000
    // transactions), and all passes build under 3 * 8,000 transactions.
    expect(commitDaFrameStepDownPassBound(8_000)).toBe(15);
    expect(budget).toBe(
      COMMIT_HOLD_BASE_MS +
        COMMIT_HOLD_PER_TX_MS * COMMIT_HOLD_STEP_DOWN_TX_WORK_FACTOR * 8_000 +
        COMMIT_HOLD_PER_USER_EVENT_MS * 500 * 15,
    );
    expect(budget).toBe(1_035_000);
    // The control-plane ceiling clips what this asks for: with 500 events the
    // budget passes 900 s above 5,750 transactions, so the step-down worst
    // case at the largest backlogs gets 900 s, not its full budget.
    expect(budget).toBeGreaterThan(L1_CONTROL_PLANE_HOLD_CEILING_MS);
  });

  it("counts a backlog only up to what one block can carry", () => {
    const atCap = commitHoldBudgetMs({
      mempoolTxCount: CONSENSUS_MAX_L2_TX_COUNT,
      maxL2TxCount: CONSENSUS_MAX_L2_TX_COUNT,
      pendingUserEventCount: COMMIT_HOLD_MAX_COUNTED_USER_EVENTS,
      consecutiveHoldTimeouts: 0,
    });
    const farAboveCap = commitHoldBudgetMs({
      mempoolTxCount: 5_000_000,
      maxL2TxCount: CONSENSUS_MAX_L2_TX_COUNT,
      pendingUserEventCount: 5_000_000,
      consecutiveHoldTimeouts: 0,
    });
    expect(farAboveCap).toBe(atCap);
    // One block carries up to the consensus caps of each user-event kind.
    expect(COMMIT_HOLD_MAX_COUNTED_USER_EVENTS).toBe(
      MIDGARD_CONSENSUS_LIMITS.maxDepositCount +
        MIDGARD_CONSENSUS_LIMITS.maxForcedTransactionCount +
        MIDGARD_CONSENSUS_LIMITS.maxWithdrawalCount,
    );
  });

  it("doubles after hold timeouts, at most twice", () => {
    const base = {
      mempoolTxCount: 100,
      maxL2TxCount: CONSENSUS_MAX_L2_TX_COUNT,
      pendingUserEventCount: 0,
    };
    const zero = commitHoldBudgetMs({ ...base, consecutiveHoldTimeouts: 0 });
    expect(commitHoldBudgetMs({ ...base, consecutiveHoldTimeouts: 1 })).toBe(
      2 * zero,
    );
    expect(commitHoldBudgetMs({ ...base, consecutiveHoldTimeouts: 9 })).toBe(
      4 * zero,
    );
  });
});

describe("extending an L1 control-plane hold", () => {
  it("lets work that sized itself finish past the initial cap, and the cap still applies without it", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const extended = yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs: 100 },
          Effect.gen(function* () {
            const granted = yield* extendL1ControlPlaneHold(600);
            yield* Effect.sleep(250);
            return granted;
          }),
        ).pipe(Effect.exit);
        const unextended = yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs: 100 },
          Effect.sleep(250),
        ).pipe(Effect.exit);
        return { extended, unextended };
      }),
    );
    expect(Exit.isSuccess(outcome.extended)).toBe(true);
    if (Exit.isSuccess(outcome.extended)) {
      expect(outcome.extended.value).toBe(600);
    }
    expect(Exit.isFailure(outcome.unextended)).toBe(true);
  });

  it("never exceeds the ceiling, never shortens a hold, and is inert outside one", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const capped = yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs: 100 },
          extendL1ControlPlaneHold(Number.MAX_SAFE_INTEGER),
        );
        const notShortened = yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs: 5_000 },
          extendL1ControlPlaneHold(10),
        );
        const outside = yield* extendL1ControlPlaneHold(1_000);
        return { capped, notShortened, outside };
      }),
    );
    expect(outcome.capped).toBe(L1_CONTROL_PLANE_HOLD_CEILING_MS);
    expect(outcome.notShortened).toBe(5_000);
    expect(outcome.outside).toBeUndefined();
  });
});

describe("sizing the commitment hold from the counted backlog", () => {
  const holdForBacklog = (
    backlog: {
      readonly mempoolTxCount: number;
      readonly pendingUserEventCount: number;
    },
    maxHoldMs: number,
  ) =>
    runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.COMMIT_PIPELINE_BACKLOG, backlog);
        return yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs },
          Effect.gen(function* () {
            const sized = yield* extendCommitmentHoldForBacklog(
              globals,
              CONSENSUS_MAX_L2_TX_COUNT,
            );
            const holder = (yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY))
              .holder;
            return {
              ...sized,
              heldForMs:
                holder === null
                  ? undefined
                  : holder.deadlineMs - holder.sinceMs,
            };
          }),
        );
      }),
    );

  it("extends the hold's deadline to the budget of a batch larger than the base", async () => {
    const backlog = { mempoolTxCount: 5_000, pendingUserEventCount: 200 };
    const expected =
      COMMIT_HOLD_BASE_MS +
      COMMIT_HOLD_PER_TX_MS * COMMIT_HOLD_STEP_DOWN_TX_WORK_FACTOR * 5_000 +
      COMMIT_HOLD_PER_USER_EVENT_MS *
        200 *
        commitDaFrameStepDownPassBound(5_000);
    expect(expected).toBe(630_000);
    const outcome = await holdForBacklog(backlog, COMMIT_HOLD_BASE_MS);
    expect(outcome.holdBudgetMs).toBe(expected);
    expect(outcome.grantedHoldMs).toBe(expected);
    expect(outcome.heldForMs).toBe(expected);
  });

  it("leaves an empty backlog's hold at its base and grants no more than the ceiling", async () => {
    const empty = await holdForBacklog(
      { mempoolTxCount: 0, pendingUserEventCount: 0 },
      COMMIT_HOLD_BASE_MS,
    );
    expect(empty.heldForMs).toBe(COMMIT_HOLD_BASE_MS);
    const full = await holdForBacklog(
      {
        mempoolTxCount: CONSENSUS_MAX_L2_TX_COUNT,
        pendingUserEventCount: COMMIT_HOLD_MAX_COUNTED_USER_EVENTS,
      },
      COMMIT_HOLD_BASE_MS,
    );
    expect(full.holdBudgetMs).toBeGreaterThan(L1_CONTROL_PLANE_HOLD_CEILING_MS);
    expect(full.grantedHoldMs).toBe(L1_CONTROL_PLANE_HOLD_CEILING_MS);
    expect(full.heldForMs).toBe(L1_CONTROL_PLANE_HOLD_CEILING_MS);
  });
});
