import { Effect, Ref } from "effect";
import { expect } from "vitest";

import type { CommitWorkerMessage } from "../../src/fibers/block-commitment.js";
import {
  hasActiveSpeculativeCommitSession,
  spawnSpeculativeSessionForTest,
  type SpeculativeCommitWorkerPort,
} from "../../src/fibers/speculative-commit-builder.js";
import {
  reduceSpeculativeCommitState,
  type SpeculativeCandidateSummary,
} from "../../src/fibers/speculative-commit-state.js";
import type { Globals } from "../../src/services/globals.js";
import type { NativeMpfOwnerService } from "../../src/services/mpf-native-owner/protocol.js";
import type { SpeculativeCommitWorkerInstruction } from "../../src/workers/utils/commit-block-header.js";

/** A speculative worker whose Ready candidate holds a real native owner
 * fork, as the production worker's candidate does. It answers an
 * invalidation the way the worker does and records what it was told. */
export class ReadyCandidateWorker implements SpeculativeCommitWorkerPort {
  private messageListener?: (message: CommitWorkerMessage) => void;
  private exitListener?: (code: number) => void;
  readonly instructions: SpeculativeCommitWorkerInstruction[] = [];

  constructor(private readonly candidate: SpeculativeCandidateSummary) {}

  on(event: "message", listener: (message: CommitWorkerMessage) => void): void;
  on(event: "error", listener: (error: Error) => void): void;
  on(event: "exit", listener: (code: number) => void): void;
  on(
    event: "message" | "error" | "exit",
    listener:
      | ((message: CommitWorkerMessage) => void)
      | ((error: Error) => void)
      | ((code: number) => void),
  ): void {
    if (event === "message")
      this.messageListener = listener as (message: CommitWorkerMessage) => void;
    else if (event === "exit")
      this.exitListener = listener as (code: number) => void;
  }

  ready(): void {
    this.messageListener?.({
      type: "SpeculativeCandidateReadyOutput",
      candidate: this.candidate,
    });
  }

  postMessage(instruction: SpeculativeCommitWorkerInstruction): void {
    this.instructions.push(instruction);
    if (instruction.type === "InvalidateSpeculativeCandidate")
      this.messageListener?.({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: this.candidate.candidateId,
        reason: instruction.reason,
      });
  }

  terminate(): Promise<number> {
    this.exitListener?.(1);
    return Promise.resolve(1);
  }
}

/** What the candidate's release saw: `observed` is the caller's snapshot
 * taken at the moment the terminated worker's fork is released, before the
 * fork is discarded. */
export type CandidateRelease<T> = {
  observed?: T;
  discarded?: boolean;
  discardError?: unknown;
};

/**
 * Installs a speculative session whose Ready candidate, built on
 * `baseHeaderHash`, holds a real fork of the native owner's durable root, and
 * moves the pipeline to `ReadyToSubmit` as the production worker's Ready
 * output does. Production releases the terminated worker's ledger lease when
 * the session ends; here the release first runs `observe`, then discards the
 * fork (a refused discard is recorded, not thrown, so a failing test does not
 * leave the session blocked).
 */
export const installReadyCandidate = async <T>(input: {
  readonly globals: Globals;
  readonly owner: NativeMpfOwnerService;
  readonly baseHeaderHash: string;
  readonly nowMs: number;
  readonly maxAttempts: number;
  readonly observe: () => Promise<T>;
}) => {
  const root = (await input.owner.diagnostics()).durableRoot;
  const fork = await input.owner.fork(root);
  const released: CandidateRelease<T> = {};
  const candidate: SpeculativeCandidateSummary = {
    candidateId: `ready-candidate-${input.baseHeaderHash.slice(0, 16)}`,
    baseHeaderHash: input.baseHeaderHash,
    endTimeMs: input.nowMs + 60_000,
    builtAtMs: input.nowMs,
    buildDurationMs: 1,
    invalidationKey: `${input.baseHeaderHash}:candidate`,
    watermarks: {
      depositMs: 0,
      withdrawalMs: 0,
      txOrderMs: 0,
      refreshedAtMs: 0,
    },
    expectedUserEventCounts: {
      deposits: 0,
      forcedTransactions: 0,
      withdrawals: 0,
    },
    expectedL2TransactionCount: 0,
    roots: {
      utxos: root,
      rawTransactions: "08".repeat(32),
      transactions: "02".repeat(32),
      deposits: "03".repeat(32),
      forcedTransactions: "04".repeat(32),
      withdrawals: "05".repeat(32),
      transitionTrace: "06".repeat(32),
      eventToStep: "07".repeat(32),
    },
  };
  const worker = new ReadyCandidateWorker(candidate);
  const session = Effect.runPromise(
    spawnSpeculativeSessionForTest(worker, async () => {
      released.observed = await input.observe();
      await input.owner.discard(fork).then(
        () => {
          released.discarded = true;
        },
        (cause: unknown) => {
          released.discardError = cause;
        },
      );
    }),
  );
  worker.ready();
  expect(await session).toEqual(candidate);
  await Effect.runPromise(
    Ref.update(input.globals.SPECULATIVE_COMMIT_STATE, (state) =>
      [
        {
          _tag: "SubmittedBase",
          baseHeaderHash: candidate.baseHeaderHash,
          atMs: candidate.builtAtMs,
        } as const,
        { _tag: "CandidateReady", candidate } as const,
      ].reduce(
        (next, event) =>
          reduceSpeculativeCommitState(next, event, input.maxAttempts),
        state,
      ),
    ),
  );
  expect(speculativeState(input.globals)).toBe("ReadyToSubmit");
  expect(hasActiveSpeculativeCommitSession()).toBe(true);
  return { candidate, root, worker, released };
};

export const speculativeState = (globals: Globals) =>
  Effect.runSync(Ref.get(globals.SPECULATIVE_COMMIT_STATE))._tag;

/** Exactly one T1 invalidation reached the worker, its session is gone, the
 * candidate's fork was released and discarded, and the pipeline left
 * `ReadyToSubmit`. */
export const expectInvalidatedByT1 = <T>(installed: {
  readonly worker: ReadyCandidateWorker;
  readonly released: CandidateRelease<T>;
  readonly globals: Globals;
}) => {
  expect(installed.worker.instructions).toEqual([
    { type: "InvalidateSpeculativeCandidate", reason: "T1" },
  ]);
  expect(hasActiveSpeculativeCommitSession()).toBe(false);
  expect(installed.released.discardError).toBeUndefined();
  expect(installed.released.discarded).toBe(true);
  expect(speculativeState(installed.globals)).not.toBe("ReadyToSubmit");
};
