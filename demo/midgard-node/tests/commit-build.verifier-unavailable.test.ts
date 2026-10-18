/**
 * A block build in a process whose Ed25519 verifier cannot answer: libsodium
 * fails to initialise. Each stage that waits for the verifier fails with its
 * typed local fault, never a defect and never a transaction's verdict: the
 * withdrawal classification fails, and Phase A's failure on a normal
 * transaction holds the build instead of rejecting the transaction. The
 * worker reports either as the verifier's local fault, which the parent
 * bounds (`commit-worker.local-fault-bound.test.ts`).
 */
import {
  findLedgerSignatureVerifierUnavailable,
  LedgerSignatureVerifierUnavailableError,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { runPhaseAValidation } from "@al-ft/midgard-validation";
import { Cause, Effect, Either } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  attempt,
  confirmTransactionFailure,
  heldBuildStage,
} from "../src/mpf/commit-rejection.evict-uncommittable-normal-txs.js";
import { captureCommitWorkerFailure } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import { classifyWithdrawalsInOrder } from "../src/workers/utils/mpf/withdrawal-classification.js";

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    const failed = Promise.reject(new Error("injected initialisation failure"));
    failed.catch(() => undefined);
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? failed
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const STAGE = "Phase A";

const phaseA = runPhaseAValidation([], {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
});

const expectVerifierFault = (error: unknown) =>
  expect(findLedgerSignatureVerifierUnavailable(error)?.message).toContain(
    "injected initialisation failure",
  );

/** The worker's output for a run that ended on `error`. */
const workerOutput = (error: unknown) =>
  Effect.runPromise(captureCommitWorkerFailure(Effect.fail(error)));

const VERIFIER_FAULT = {
  type: "FailureOutput",
  localFault: "verifier_unavailable",
} as const;

describe("a block build whose Ed25519 verifier cannot answer", () => {
  it("fails the withdrawal classification with the verifier's fault", async () => {
    const result = await Effect.runPromise(
      Effect.either(
        classifyWithdrawalsInOrder({ entries: [], baseOutputs: new Map() }),
      ),
    );
    expect(Either.isLeft(result)).toBe(true);
    const error = Either.isLeft(result) ? result.left : undefined;
    expect(error).toMatchObject({ _tag: "DatabaseError" });
    expectVerifierFault(error);
    expect(await workerOutput(error)).toMatchObject(VERIFIER_FAULT);
  });

  it("holds the build on Phase A's failure, rejecting no transaction", async () => {
    const first = await Effect.runPromise(attempt(phaseA));
    expect(first.ok).toBe(false);
    if (first.ok) return;
    expectVerifierFault(Cause.squash(first.cause));
    const decided = await Effect.runPromise(
      Effect.either(confirmTransactionFailure(STAGE, first.cause, phaseA)),
    );
    // A Right would be the reproduced cause the transaction is rejected on.
    expect(Either.isLeft(decided)).toBe(true);
    const held = Either.isLeft(decided) ? decided.left : undefined;
    expect(heldBuildStage(held)).toBe(STAGE);
    expectVerifierFault(held);
    expect(await workerOutput(held)).toMatchObject(VERIFIER_FAULT);
  });

  it("holds the build when the verifier's fault is raised as a defect", async () => {
    const fault = new LedgerSignatureVerifierUnavailableError(
      "Ed25519 verification is unavailable: injected initialisation failure",
    );
    const decided = await Effect.runPromise(
      Effect.either(
        confirmTransactionFailure(STAGE, Cause.die(fault), Effect.die(fault)),
      ),
    );
    expect(Either.isLeft(decided)).toBe(true);
    const held = Either.isLeft(decided) ? decided.left : undefined;
    expect(heldBuildStage(held)).toBe(STAGE);
    expect(await workerOutput(held)).toMatchObject(VERIFIER_FAULT);
  });
});
