/**
 * The landed-block rebase's native restore (N4, plan §1.4 "MPF interim"):
 * a durable root off the target chain is restored with the native owner's
 * `restoreCanonicalRoot`, once per rewind, and every refusal is a named
 * hold with the process up (`restore-holds.ts`), never a halt.
 */
import { Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import {
  moveNativeRoot,
  rebaseRecoveryId,
} from "../src/landed-blocks/rebase.js";
import type { RebaseTarget } from "../src/landed-blocks/rebase-target.js";
import {
  LandedChainRootNotRetained,
  restoreRefusalHold,
} from "../src/landed-blocks/restore-holds.js";
import {
  MPF_CLOSURE_MISSING,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
} from "../src/services/liveness-halt.js";
import {
  type NativeMpfOwnerService,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/index.js";

const DURABLE = "d1".repeat(32);
const FRONTIER = "f0".repeat(32);

/** A target with no steps past the frontier: the rewind is one restore. */
const target = {
  landed: {
    frontier: { headerHash: "aa".repeat(28), utxosRoot: FRONTIER },
    confirmed: [],
    chain: [],
  },
  steps: [],
} as unknown as RebaseTarget;

const ownerRefusing = (refusal: (targetRoot: string) => unknown) => {
  const restores: unknown[] = [];
  const owner = {
    diagnostics: () => Promise.resolve({ durableRoot: DURABLE }),
    restoreCanonicalRoot: (request: { targetRoot: string }) => {
      restores.push(request);
      const error = refusal(request.targetRoot);
      return error === undefined ? Promise.resolve() : Promise.reject(error);
    },
  } as unknown as NativeMpfOwnerService;
  return { owner, restores };
};

const move = (owner: NativeMpfOwnerService) =>
  Effect.runPromiseExit(
    moveNativeRoot(owner, target, { assertCurrent: Effect.void }),
  );

const failureOf = (exit: Exit.Exit<unknown, unknown>) => {
  if (!Exit.isFailure(exit) || exit.cause._tag !== "Fail")
    throw new Error(`expected a failure, got ${JSON.stringify(exit)}`);
  return exit.cause.error;
};

describe("the landed-block rebase's native restore", () => {
  it("restores the durable root onto the target with one restoreCanonicalRoot", async () => {
    const { owner, restores } = ownerRefusing(() => undefined);
    expect(Exit.isSuccess(await move(owner))).toBe(true);
    expect(restores).toEqual([
      {
        recoveryId: rebaseRecoveryId(DURABLE, FRONTIER),
        expectedRoot: DURABLE,
        targetRoot: FRONTIER,
      },
    ]);
  });

  it("holds by name, at once, when the store retains no root of the landed chain", async () => {
    const { owner, restores } = ownerRefusing(
      (root) => new NativeMpfRootNotRetained(root),
    );
    const failure = failureOf(await move(owner));
    expect(failure).toBeInstanceOf(LandedChainRootNotRetained);
    expect(restores).toHaveLength(1);
    expect(restoreRefusalHold([new Error("rebase"), failure])).toEqual({
      reason: MPF_CLOSURE_MISSING,
      escalateAfterMs: 0,
    });
    // The detail names both roots and the operator's clearing action.
    expect((failure as Error).message).toContain(DURABLE);
    expect((failure as Error).message).toContain(
      `retains root ${FRONTIER} in full`,
    );
  });

  it("names a refusal that crossed the native child's boundary by its tag, and nothing else", async () => {
    const capped = { _tag: "NativeMpfFullIndexCapExceeded" };
    const { owner, restores } = ownerRefusing(() => capped);
    // A refusal other than a missing root is not retried at an older root.
    expect(failureOf(await move(owner))).toBe(capped);
    expect(restores).toHaveLength(1);
    expect(restoreRefusalHold([capped])).toEqual({
      reason: NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
      escalateAfterMs: 0,
    });
    expect(
      restoreRefusalHold([
        new Error("x"),
        { _tag: "NativeMpfRestoreReadFailed" },
      ]),
    ).toEqual({
      reason: NATIVE_MPF_RESTORE_READ_TRANSIENT,
      escalateAfterMs: NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
    });
    // Any other failure is the rebase's own `landed_block_rebase_failed`.
    expect(restoreRefusalHold([new Error("x"), { _tag: "Other" }, 7])).toBe(
      undefined,
    );
  });
});
