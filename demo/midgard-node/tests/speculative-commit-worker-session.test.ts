import { Effect, Fiber } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  hasActiveSpeculativeCommitSession,
  invalidateSpeculativeSessionForTest,
  shutdownSpeculativeCommitSession,
  spawnSpeculativeSessionForTest,
  type SpeculativeCommitWorkerPort,
} from "@/fibers/speculative-commit-builder.js";
import type { SpeculativeCandidateSummary } from "@/fibers/speculative-commit-state.js";
import type {
  SpeculativeCommitWorkerInstruction,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { WorkerError } from "@/workers/utils/common.js";

const candidate: SpeculativeCandidateSummary = {
  candidateId: "late-candidate",
  baseHeaderHash: "aa".repeat(28),
  endTimeMs: 2_000,
  builtAtMs: 1_500,
  buildDurationMs: 500,
  invalidationKey: `${"aa".repeat(28)}:2000:1000`,
  watermarks: {
    depositMs: 1_000,
    withdrawalMs: 1_000,
    txOrderMs: 1_000,
    refreshedAtMs: 1_100,
  },
  expectedUserEventCounts: {
    deposits: 0,
    forcedTransactions: 0,
    withdrawals: 0,
  },
  expectedL2TransactionCount: 1,
  roots: {
    utxos: "01".repeat(32),
    rawTransactions: "08".repeat(32),
    transactions: "02".repeat(32),
    deposits: "03".repeat(32),
    forcedTransactions: "04".repeat(32),
    withdrawals: "05".repeat(32),
    transitionTrace: "06".repeat(32),
    eventToStep: "07".repeat(32),
  },
};

class ControllableWorker implements SpeculativeCommitWorkerPort {
  private messageListener?: (output: WorkerOutput) => void;
  private errorListener?: (error: Error) => void;
  private exitListener?: (code: number) => void;
  private resolveTermination!: (code: number) => void;
  private readonly termination = new Promise<number>((resolve) => {
    this.resolveTermination = resolve;
  });
  terminateCalls = 0;

  on(event: "message", listener: (output: WorkerOutput) => void): void;
  on(event: "error", listener: (error: Error) => void): void;
  on(event: "exit", listener: (code: number) => void): void;
  on(
    event: "message" | "error" | "exit",
    listener:
      | ((output: WorkerOutput) => void)
      | ((error: Error) => void)
      | ((code: number) => void),
  ): void {
    if (event === "message") {
      this.messageListener = listener as (output: WorkerOutput) => void;
    } else if (event === "error") {
      this.errorListener = listener as (error: Error) => void;
    } else {
      this.exitListener = listener as (code: number) => void;
    }
  }

  postMessage(_instruction: SpeculativeCommitWorkerInstruction): void {}

  terminate(): Promise<number> {
    this.terminateCalls += 1;
    return this.termination;
  }

  emitMessage(output: WorkerOutput): void {
    this.messageListener?.(output);
  }

  emitError(error: Error): void {
    this.errorListener?.(error);
  }

  finishTermination(code = 1): void {
    this.resolveTermination(code);
    this.exitListener?.(code);
  }
}

describe("speculative worker session lifecycle", () => {
  it("rejects a candidate-ready message that races after build invalidation", async () => {
    const worker = new ControllableWorker();
    const buildResult = Effect.runPromise(
      Effect.either(spawnSpeculativeSessionForTest(worker)),
    );

    await Promise.resolve();
    expect(hasActiveSpeculativeCommitSession()).toBe(true);

    const invalidation = Effect.runPromise(
      invalidateSpeculativeSessionForTest("T1"),
    );
    await Promise.resolve();
    expect(worker.terminateCalls).toBe(1);

    worker.emitMessage({
      type: "SpeculativeCandidateReadyOutput",
      candidate,
    });

    let buildSettled = false;
    void buildResult.then(() => {
      buildSettled = true;
    });
    await Promise.resolve();
    expect(buildSettled).toBe(false);
    expect(hasActiveSpeculativeCommitSession()).toBe(true);
    worker.finishTermination();
    const result = await buildResult;
    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") throw new Error("expected worker failure");
    expect(result.left).toBeInstanceOf(WorkerError);
    expect(result.left.message).toContain(
      "Speculative commit worker session failed",
    );
    expect(hasActiveSpeculativeCommitSession()).toBe(false);
    expect(worker.terminateCalls).toBe(1);

    await invalidation;
    expect(hasActiveSpeculativeCommitSession()).toBe(false);
  });

  it("terminates and awaits an active worker when final-output waiting is interrupted", async () => {
    const worker = new ControllableWorker();
    const buildResult = Effect.runPromise(
      spawnSpeculativeSessionForTest(worker),
    );

    await Promise.resolve();
    worker.emitMessage({
      type: "SpeculativeCandidateReadyOutput",
      candidate,
    });
    await buildResult;
    expect(hasActiveSpeculativeCommitSession()).toBe(true);

    const invalidationFiber = Effect.runFork(
      invalidateSpeculativeSessionForTest("T1"),
    );
    await Promise.resolve();
    const interrupted = Effect.runPromise(Fiber.interrupt(invalidationFiber));

    await vi.waitFor(() => expect(worker.terminateCalls).toBe(1));
    expect(hasActiveSpeculativeCommitSession()).toBe(true);

    let interruptionSettled = false;
    void interrupted.then(() => {
      interruptionSettled = true;
    });
    await Promise.resolve();
    expect(interruptionSettled).toBe(false);

    worker.finishTermination();
    await interrupted;
    expect(interruptionSettled).toBe(true);
    expect(worker.terminateCalls).toBe(1);
    expect(hasActiveSpeculativeCommitSession()).toBe(false);
  });

  it("runs lease cleanup only after an invalidated worker has terminated", async () => {
    const worker = new ControllableWorker();
    const cleanup = vi.fn(async () => undefined);
    const buildResult = Effect.runPromise(
      spawnSpeculativeSessionForTest(worker, cleanup),
    );

    await Promise.resolve();
    worker.emitMessage({
      type: "SpeculativeCandidateReadyOutput",
      candidate,
    });
    await buildResult;

    const invalidation = Effect.runPromise(
      invalidateSpeculativeSessionForTest("T1"),
    );
    await Promise.resolve();
    worker.emitMessage({
      type: "SpeculativeCandidateInvalidatedOutput",
      candidateId: candidate.candidateId,
      reason: "T1",
    });
    await vi.waitFor(() => expect(worker.terminateCalls).toBe(1));
    expect(cleanup).not.toHaveBeenCalled();
    expect(hasActiveSpeculativeCommitSession()).toBe(true);

    const replacement = await Effect.runPromise(
      Effect.either(spawnSpeculativeSessionForTest(new ControllableWorker())),
    );
    expect(replacement._tag).toBe("Left");

    worker.finishTermination();
    await invalidation;
    expect(cleanup).toHaveBeenCalledTimes(1);
    expect(hasActiveSpeculativeCommitSession()).toBe(false);
  });

  it("shutdown awaits and clears a detached ready worker", async () => {
    const worker = new ControllableWorker();
    const cleanup = vi.fn(async () => undefined);
    const buildResult = Effect.runPromise(
      spawnSpeculativeSessionForTest(worker, cleanup),
    );

    await Promise.resolve();
    worker.emitMessage({
      type: "SpeculativeCandidateReadyOutput",
      candidate,
    });
    await buildResult;

    const shutdown = Effect.runPromise(shutdownSpeculativeCommitSession());
    await vi.waitFor(() => expect(worker.terminateCalls).toBe(1));
    expect(hasActiveSpeculativeCommitSession()).toBe(true);
    expect(cleanup).not.toHaveBeenCalled();

    worker.finishTermination();
    await shutdown;
    expect(cleanup).toHaveBeenCalledTimes(1);
    expect(hasActiveSpeculativeCommitSession()).toBe(false);
  });

  it("keeps a session blocked and its cleanup untouched when termination is unconfirmed", async () => {
    const worker = new (class extends ControllableWorker {
      override terminate(): Promise<number> {
        this.terminateCalls += 1;
        return Promise.reject(new Error("termination not confirmed"));
      }
    })();
    const cleanup = vi.fn(async () => undefined);
    const buildResult = Effect.runPromise(
      spawnSpeculativeSessionForTest(worker, cleanup),
    );

    await Promise.resolve();
    worker.emitMessage({
      type: "SpeculativeCandidateReadyOutput",
      candidate,
    });
    await buildResult;
    await Effect.runPromise(shutdownSpeculativeCommitSession());

    expect(worker.terminateCalls).toBe(1);
    expect(cleanup).not.toHaveBeenCalled();
    expect(hasActiveSpeculativeCommitSession()).toBe(true);
    const replacement = await Effect.runPromise(
      Effect.either(spawnSpeculativeSessionForTest(new ControllableWorker())),
    );
    expect(replacement._tag).toBe("Left");
  });
});
