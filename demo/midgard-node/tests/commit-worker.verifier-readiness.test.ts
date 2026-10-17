/**
 * A commit worker run waits for its Ed25519 verifier before any work, for at
 * most one startup attempt of the ledger signature verifier (10 s). An
 * initialisation that fails, or does not settle within that, ends the run at
 * the worker's failure boundary as the verifier's local fault, which the
 * parent bounds: runs that end on it `LOCAL_FAULT_RUN_LIMIT` times without a
 * successful commit exit the node non-zero.
 */
import { LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import { HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import {
  Deferred,
  Effect,
  Exit,
  Fiber,
  Logger,
  TestClock,
  TestContext,
} from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { COMMIT_WORKER_SOURCE } from "../src/fibers/block-commitment.worker-readiness.js";
import { Globals } from "../src/services/globals.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { IntentJournal } from "../src/services/intent-journal.js";
import { LOCAL_FAULT_RUN_LIMIT } from "../src/services/l1-follower.coalesced-runner.js";
import { findTransientBudgetExhausted } from "../src/services/transient-exhaustion.js";
import type { CommitLucidFactory } from "../src/workers/commit-block-header.pending-user-event-counts-up-to.js";
import {
  captureCommitWorkerFailure,
  runCommitBlockHeaderWorkerProgram,
} from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import { takeCommitWorkerOutput } from "../src/workers/commit-worker-message.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "../src/workers/utils/commit-block-header.js";

/** The verifier initialisation each run sees, when the test sets one. */
const injected = vi.hoisted(() => ({
  ready: undefined as Promise<void> | undefined,
}));

vi.mock("@al-ft/midgard-core", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-core")>();
  return Object.defineProperty({ ...actual }, "ledgerEd25519SignatureReady", {
    enumerable: true,
    get: () => injected.ready ?? actual.ledgerEd25519SignatureReady,
  });
});

afterEach(() => {
  injected.ready = undefined;
});

/** The bound on the wait: one startup attempt of the verifier. */
const BOUND_MS = 10_000;
/** Each row's own limit, so a wait that never ends fails the row. */
const ROW_TIMEOUT_MS = 20_000;
/** The reason the node exits under, from source `commit_worker`. */
const EXIT_REASON = "commit_worker_verifier_unavailable";

/** An initialisation that never settles. */
const hang = () => new Promise<void>(() => undefined);

/** One commit worker run as the worker thread runs it, to its output. The
 * run ends before it reads its input or any service. */
const run = captureCommitWorkerFailure(
  runCommitBlockHeaderWorkerProgram(
    { data: {} } as unknown as WorkerInput,
    undefined,
    undefined as unknown as CommitLucidFactory,
  ).pipe(
    // A failed run hands the journal's unwritten holds off.
    Effect.provideService(IntentJournal, { handOff: () => [] } as never),
    Effect.provideService(MidgardContracts, {} as never),
    Effect.provideService(ContractDeploymentIdentity, {} as never),
    Effect.provideService(SqlClient.SqlClient, {} as never),
    Effect.provideService(NodeConfig, {} as never),
  ),
);

/**
 * Runs one commit worker run under the test clock: its output, or none, once
 * `beforeMs` has passed, and its output once the bound has.
 */
const runOnTestClock = (beforeMs: number) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const fiber = yield* Effect.fork(run);
      yield* TestClock.adjust(beforeMs);
      const early = fiber.unsafePoll();
      yield* TestClock.adjust(BOUND_MS - beforeMs);
      return { early, output: yield* Fiber.join(fiber) };
    }).pipe(Effect.provide(TestContext.TestContext)),
  );

/** The serving node's exit once `globals` signalled exhaustion, as
 * `runListen` races it, and the lines it logged. */
const servingExit = async (globals: Globals) => {
  const messages: unknown[] = [];
  const exit = await Effect.runPromiseExit(
    withStartupHttpServer(0, (startup) =>
      Effect.gen(function* () {
        yield* startup.publish(
          Effect.succeed(HttpServerResponse.unsafeJson({ ready: true })),
        );
        return yield* Effect.raceFirst(
          Effect.never,
          Deferred.await(globals.TRANSIENT_EXHAUSTION),
        );
      }),
    ).pipe(
      Effect.provide(
        Logger.replace(
          Logger.defaultLogger,
          Logger.make(({ message }) => {
            messages.push(message);
          }),
        ),
      ),
    ),
  );
  return { exit, messages: messages.flat() };
};

const FAULT = {
  type: "FailureOutput",
  localFault: "verifier_unavailable",
} as const;

describe("a commit worker run's wait for its Ed25519 verifier", () => {
  it(
    "ends the run as the verifier's local fault at the bound, and not before, when the initialisation does not settle",
    async () => {
      injected.ready = hang();
      const { early, output } = await runOnTestClock(BOUND_MS - 1);
      expect(early).toBeNull();
      expect(output).toMatchObject(FAULT);
      expect((output as { readonly error: string }).error).toContain(
        "did not initialise within 10s",
      );
    },
    ROW_TIMEOUT_MS,
  );

  it.each([
    [
      "the verifier's own fault",
      () =>
        new LedgerSignatureVerifierUnavailableError(
          "injected verifier initialisation failure",
        ),
      "injected verifier initialisation failure",
    ],
    [
      "another error",
      () => new Error("injected initialisation failure"),
      "The commit worker's Ed25519 verifier failed to initialise",
    ],
  ])(
    "ends the run as the verifier's local fault when the initialisation fails with %s",
    async (_label, failure, reported) => {
      const rejected = Promise.reject(failure());
      rejected.catch(() => undefined);
      injected.ready = rejected;
      const output: WorkerOutput = await Effect.runPromise(run);
      expect(output).toMatchObject(FAULT);
      expect((output as { readonly error: string }).error).toContain(reported);
    },
    ROW_TIMEOUT_MS,
  );

  it(
    "exits the node non-zero once runs end on a hung initialisation for the parent's bound",
    async () => {
      injected.ready = hang();
      const globals = await Effect.runPromise(
        Effect.provide(Globals, Globals.Default),
      );
      const exhausted = () =>
        Effect.runPromise(Deferred.isDone(globals.TRANSIENT_EXHAUSTION));
      for (let index = 1; index <= LOCAL_FAULT_RUN_LIMIT; index += 1) {
        const { output } = await runOnTestClock(0);
        expect(output).toMatchObject(FAULT);
        expect(await exhausted()).toBe(false);
        takeCommitWorkerOutput(globals, output, 16);
        expect(await exhausted()).toBe(index === LOCAL_FAULT_RUN_LIMIT);
      }
      const { exit, messages } = await servingExit(globals);
      expect(
        Exit.isFailure(exit) && findTransientBudgetExhausted(exit.cause),
      ).toMatchObject({ source: COMMIT_WORKER_SOURCE, reason: EXIT_REASON });
      expect(messages).toEqual([
        `node_transient_budget_exhausted stage=serving source=${COMMIT_WORKER_SOURCE} reason=${EXIT_REASON}; a transient failure outlived its bound; the node exits non-zero`,
      ]);
    },
    ROW_TIMEOUT_MS,
  );
});
