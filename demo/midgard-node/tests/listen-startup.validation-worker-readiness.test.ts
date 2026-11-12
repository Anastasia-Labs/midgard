/**
 * The node's startup acquires its validation workers (`validationPoolLayer`)
 * before the first step of `runNode`, and each worker initialises its own
 * Ed25519 verifier. Here the node's real startup (`runNode` under its startup
 * HTTP listener, over the shared test Postgres) runs the built worker with
 * that initialisation set per case:
 *
 * - it fails, or never settles: the start is retried on a fresh worker a
 *   bounded number of times, then startup fails exhausted by step and reason
 *   in its first stage and the process exits non-zero;
 * - it fails once: the retry gets startup past the validation workers;
 * - it succeeds: startup gets past the validation workers on one worker.
 *
 * Or a worker stops while it answers its readiness job, on an error it does
 * not handle or at its heap limit: that start is retried on a fresh worker
 * within the same bound, then startup fails exhausted and the process exits
 * non-zero; one that stops once is retried and startup gets past the
 * validation workers. A worker whose module does not load fails startup at
 * once, and the process exits non-zero. No failure leaves the node held up
 * unready.
 *
 * The node is configured without a settlement wallet, so the first step after
 * the validation workers (`local_preflight`) holds it: `/readyz` then names
 * `local_preflight` as the failed stage, which only a startup that got past
 * its validation workers can reach. A startup that fails in its validation
 * workers fails in the stage before (`runtime_services`).
 *
 * The worker thread is the one seam: `node:worker_threads` starts the test's
 * worker module, which sets libsodium's initialisation and then loads the
 * built `dist/validation.js`, in place of that module, and gives the
 * `out-of-memory` worker a small heap.
 */
import { existsSync, mkdtempSync, readdirSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  findLedgerSignatureVerifierUnavailable,
  LEDGER_ED25519_READY_TIMEOUT_MS,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServer } from "@effect/platform";
import { PgClient } from "@effect/sql-pg";
import { Deferred, Effect, Exit, Fiber, Logger, Redacted } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { DEFAULT_IMPORT_CHECK_CONFIG } from "../src/services/config.import-check.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { findStartupStepFailure } from "../src/services/startup-waiting.js";
import { ValidationWorkerError } from "../src/services/validation-pool.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

/** Every validation worker the node started, and whether it has exited. */
const spawned = vi.hoisted(() => ({
  workers: [] as { exited: boolean }[],
}));

vi.mock("node:worker_threads", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:worker_threads")>();
  const testWorker = new URL(
    "./listen-startup.validation-worker-readiness.worker.mjs",
    import.meta.url,
  );
  class Worker extends actual.Worker {
    constructor(
      entry: string | URL,
      options?: import("node:worker_threads").WorkerOptions,
    ) {
      const mode = process.env.MIDGARD_TEST_WORKER_READINESS_MODE;
      const validation =
        mode !== undefined && String(entry).endsWith("/dist/validation.js");
      super(
        validation ? testWorker : entry,
        validation && mode === "out-of-memory"
          ? { ...options, resourceLimits: { maxOldGenerationSizeMb: 32 } }
          : options,
      );
      if (validation) {
        const worker = { exited: false };
        spawned.workers.push(worker);
        this.once("exit", () => {
          worker.exited = true;
        });
      }
    }
  }
  return { ...actual, default: { ...actual, Worker }, Worker };
});

// A worker answers its readiness job once it has loaded its module and its
// verifier is ready: up to 8.3 s on a loaded two-core host. The job waits as
// long as the verifier's own bound, which starts only once the worker has
// loaded, so the job times out first when an initialisation never settles.
const JOB_TIMEOUT_MS = LEDGER_ED25519_READY_TIMEOUT_MS;
/** Worker starts in all: the ledger signature verifier's startup bound. */
const ATTEMPTS = 3;
const INJECTED = "injected worker libsodium initialisation failure";

// The configuration a node is started with: one validation worker, and no
// settlement wallet, so `local_preflight` holds the node.
const startupConfig = makeTestNodeConfig({
  PORT: 0,
  VALIDATION_WORKER_POOL_SIZE: 1,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: JOB_TIMEOUT_MS,
  NETWORK: "Preprod",
  MIN_FEE_A: 0n,
  MIN_FEE_B: 0n,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
  IMPORT_CHECK: DEFAULT_IMPORT_CHECK_CONFIG,
  L1_SETTLEMENT_SEED_PHRASE: "",
});

/** The shared test Postgres the node's runtime services connect to. */
const testPostgres = PgClient.layer({
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  username: process.env.POSTGRES_USER ?? "postgres",
  password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
  database: "postgres",
  maxConnections: 1,
});

type Mode =
  | "fault"
  | "never"
  | "fault-once"
  | "ready"
  | "crash"
  | "crash-once"
  | "out-of-memory"
  | "load";

/** `cause` and the causes it carries, outermost first. */
const causeChain = (cause: unknown): unknown[] => {
  const chain: unknown[] = [];
  for (let next = cause; next !== undefined && chain.length < 8; ) {
    chain.push(next);
    next =
      typeof next === "object" && next !== null && "cause" in next
        ? (next as { readonly cause?: unknown }).cause
        : undefined;
  }
  return chain;
};

/** The Node error code on `cause` or a cause it carries. */
const causeCodes = (cause: unknown): unknown[] =>
  causeChain(cause).map((link) =>
    typeof link === "object" && link !== null && "code" in link
      ? (link as { readonly code?: unknown }).code
      : undefined,
  );

/** The startup listener's address, from inside its server's scope. */
const listenerUrl = HttpServer.addressWith((address) =>
  address._tag === "TcpAddress"
    ? Effect.succeed(`http://127.0.0.1:${address.port}`)
    : Effect.die("Expected a TCP listener"),
);

/** Starts the node with the workers' initialisation set to `mode`, runs
 * `observe` on it, then stops it. */
const startNode = async (
  mode: Mode,
  observe: (node: {
    readonly fiber: Fiber.RuntimeFiber<void, unknown>;
    /** The startup listener's `/readyz` answer. */
    readonly readyz: () => Promise<unknown>;
    readonly messages: readonly unknown[];
    /** How many validation workers the node started. */
    readonly workersStarted: () => number;
    /** How many workers read their libsodium initialisation. */
    readonly workersRead: () => number;
  }) => Promise<void>,
  jobTimeoutMs = JOB_TIMEOUT_MS,
) => {
  await ledgerEd25519SignatureReady();
  expect(existsSync(new URL("../dist/validation.js", import.meta.url))).toBe(
    true,
  );
  const directory = mkdtempSync(join(tmpdir(), "midgard-worker-readiness-"));
  const marker = join(directory, "read");
  vi.stubEnv("MIDGARD_TEST_WORKER_READINESS_MODE", mode);
  vi.stubEnv("MIDGARD_TEST_WORKER_READINESS_MARKER", marker);
  spawned.workers.splice(0);
  const messages: unknown[] = [];
  const listening = Effect.runSync(Deferred.make<string>());
  const fiber = Effect.runFork(
    (
      withStartupHttpServer(0, (startup) =>
        listenerUrl.pipe(
          Effect.flatMap((url) => Deferred.succeed(listening, url)),
          Effect.zipRight(runNode(startup)),
          Effect.provideService(NodeConfig, {
            ...startupConfig,
            VALIDATION_WORKER_JOB_TIMEOUT_MS: jobTimeoutMs,
          }),
          Effect.provideService(
            ContractDeploymentIdentity,
            ContractDeploymentIdentity.make({
              kind: "derived",
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            }),
          ),
          Effect.provide(Globals.Default),
          Effect.provide(testPostgres),
        ),
      ) as Effect.Effect<void, unknown>
    ).pipe(
      Effect.provide(
        Logger.replace(
          Logger.defaultLogger,
          Logger.make(({ message }) => {
            messages.push(...[message].flat());
          }),
        ),
      ),
    ),
  );
  try {
    const url = await Effect.runPromise(
      Deferred.await(listening).pipe(Effect.timeout("10 seconds")),
    );
    await observe({
      fiber,
      readyz: async () => (await fetch(`${url}/readyz`)).json(),
      messages,
      workersStarted: () => spawned.workers.length,
      workersRead: () =>
        readdirSync(directory).filter((name) => /^read\.\d+$/u.test(name))
          .length,
    });
  } finally {
    await Effect.runPromise(Fiber.interrupt(fiber));
    // Every worker the node started has stopped.
    await vi.waitFor(() =>
      expect(spawned.workers.every((worker) => worker.exited)).toBe(true),
    );
    vi.unstubAllEnvs();
    rmSync(directory, { recursive: true, force: true });
  }
};

/** `/readyz` once startup got past its validation workers: the next step,
 * `local_preflight`, holds the node (it has no settlement wallet). */
const PAST_THE_VALIDATION_WORKERS = {
  ready: false,
  reasons: ["startup_failed"],
  stage: "fatal",
  failedStage: "local_preflight",
};

/** The outcome a held startup logs: the node stays up, unready. */
const HELD = expect.stringMatching(/the node stays up, unready/u);

/** Waits until startup got past its validation workers, and is held there,
 * still running; or fails on the exit of a startup that ended first. */
const pastTheValidationWorkers = async ({
  fiber,
  readyz,
  messages,
}: {
  readonly fiber: Fiber.RuntimeFiber<void, unknown>;
  readonly readyz: () => Promise<unknown>;
  readonly messages: readonly unknown[];
}) => {
  await vi.waitFor(
    async () => {
      if (fiber.unsafePoll() !== null) return;
      expect(await readyz()).toEqual(PAST_THE_VALIDATION_WORKERS);
    },
    { timeout: 3 * JOB_TIMEOUT_MS, interval: 100 },
  );
  expect(fiber.unsafePoll()).toBeNull();
  expect(await readyz()).toEqual(PAST_THE_VALIDATION_WORKERS);
  expect(messages).toContainEqual(
    "node_startup_failed stage=local_preflight; the node stays up, unready, until it is restarted",
  );
};

/** The node's exit, once startup ended it; fails if it is still running. */
const nodeExit = (
  fiber: Fiber.RuntimeFiber<void, unknown>,
  withinMs = 5 * JOB_TIMEOUT_MS,
) =>
  Effect.runPromise(
    Fiber.await(fiber).pipe(Effect.timeout(`${withinMs} millis`)),
  );

describe("node startup while its validation workers initialise", () => {
  it.each([
    [
      "fails",
      "fault",
      (cause: unknown) => {
        // The worker's typed verifier fault, its original text kept.
        const fault = findLedgerSignatureVerifierUnavailable(cause);
        expect(fault).toBeDefined();
        expect(String((fault?.cause as Error | undefined)?.message)).toContain(
          INJECTED,
        );
      },
    ],
    [
      "never settles",
      "never",
      (cause: unknown) => {
        expect(cause).toBeInstanceOf(ValidationWorkerError);
        expect(String((cause as ValidationWorkerError).cause)).toContain(
          `timed out after ${JOB_TIMEOUT_MS}ms`,
        );
      },
    ],
  ] as const)(
    "a worker initialisation that %s is retried within the bound, then exits non-zero in the first startup stage",
    async (_label, mode, lastCause) => {
      await startNode(
        mode,
        async ({ fiber, messages, workersStarted, workersRead }) => {
          const exit = await nodeExit(fiber);
          expect(Exit.isFailure(exit)).toBe(true);
          const step = Exit.isFailure(exit)
            ? findStartupStepFailure(exit.cause)
            : undefined;
          expect(step).toMatchObject({
            step: "validation_workers",
            reason: "ledger_signature_verifier_unavailable",
            exhausted: true,
            attempts: ATTEMPTS,
          });
          lastCause(step?.cause);
          expect(workersStarted()).toBe(ATTEMPTS);
          expect(workersRead()).toBe(ATTEMPTS);
          expect(messages).toContainEqual(
            "node_startup_failed stage=runtime_services step=validation_workers reason=ledger_signature_verifier_unavailable; a transient failure outlived its bound; the node exits non-zero",
          );
          expect(messages).not.toContainEqual(HELD);
        },
      );
    },
    120_000,
  );

  it.each([
    [
      "throws an error it does not handle",
      "crash",
      (cause: unknown) =>
        expect(
          causeChain(cause).map((link) => (link as Error).message),
        ).toContainEqual("injected worker crash"),
      JOB_TIMEOUT_MS,
      5 * JOB_TIMEOUT_MS,
    ],
    [
      // A job timeout well past the time a worker takes to load and reach
      // its heap limit, so the worker stops before its job times out; a
      // worker near its heap limit collects garbage often, so each attempt
      // takes seconds.
      "reaches its heap limit",
      "out-of-memory",
      (cause: unknown) =>
        expect(causeCodes(cause)).toContain("ERR_WORKER_OUT_OF_MEMORY"),
      30_000,
      90_000,
    ],
  ] as const)(
    "a worker that %s while it answers its readiness job is retried within the bound, then exits non-zero in the first startup stage",
    async (_label, mode, lastCause, jobTimeoutMs, exitWithinMs) => {
      await startNode(
        mode,
        async ({ fiber, messages, workersStarted }) => {
          const exit = await nodeExit(fiber, exitWithinMs);
          expect(Exit.isFailure(exit)).toBe(true);
          const step = Exit.isFailure(exit)
            ? findStartupStepFailure(exit.cause)
            : undefined;
          expect(step).toMatchObject({
            step: "validation_workers",
            reason: "validation_worker_crashed",
            exhausted: true,
            attempts: ATTEMPTS,
          });
          lastCause(step?.cause);
          expect(workersStarted()).toBe(ATTEMPTS);
          expect(messages).toContainEqual(
            "node_startup_failed stage=runtime_services step=validation_workers reason=validation_worker_crashed; a transient failure outlived its bound; the node exits non-zero",
          );
          expect(messages).not.toContainEqual(HELD);
        },
        jobTimeoutMs,
      );
    },
    120_000,
  );

  it("a worker whose module does not load fails startup at once and exits non-zero in the first startup stage", async () => {
    await startNode("load", async ({ fiber, messages, workersStarted }) => {
      const exit = await nodeExit(fiber);
      expect(Exit.isFailure(exit)).toBe(true);
      const step = Exit.isFailure(exit)
        ? findStartupStepFailure(exit.cause)
        : undefined;
      expect(step).toMatchObject({
        step: "validation_workers",
        reason: "validation_worker_load_failed",
        exhausted: false,
        terminal: true,
        attempts: 1,
      });
      expect(causeCodes(step?.cause)).toContain("ERR_MODULE_NOT_FOUND");
      expect(workersStarted()).toBe(1);
      expect(messages).toContainEqual(
        "node_startup_failed stage=runtime_services step=validation_workers reason=validation_worker_load_failed; a failure waiting does not clear; the node exits non-zero",
      );
      expect(messages).not.toContainEqual(HELD);
    });
  }, 60_000);

  it("a worker that stops once while it answers its readiness job is retried and startup gets past the validation workers", async () => {
    await startNode("crash-once", async ({ workersStarted, ...node }) => {
      await pastTheValidationWorkers(node);
      expect(workersStarted()).toBe(2);
    });
  }, 120_000);

  it("a worker initialisation that fails once is retried and startup gets past the validation workers", async () => {
    await startNode(
      "fault-once",
      async ({ workersStarted, workersRead, ...node }) => {
        await pastTheValidationWorkers(node);
        expect(workersStarted()).toBe(2);
        expect(workersRead()).toBe(2);
      },
    );
  }, 120_000);

  it("a worker initialisation that succeeds gets startup past the validation workers on one worker", async () => {
    await startNode(
      "ready",
      async ({ workersStarted, workersRead, ...node }) => {
        await pastTheValidationWorkers(node);
        expect(workersStarted()).toBe(1);
        expect(workersRead()).toBe(1);
      },
    );
  }, 120_000);
});
