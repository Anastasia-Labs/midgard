/**
 * The node's startup acquires its validation workers (`validationPoolLayer`,
 * started by `FixedValidationWorkerPool.start`) before `runNode` runs, and
 * each worker initialises its own Ed25519 verifier. Here the real startup
 * graph runs the built worker with that initialisation set per case:
 *
 * - it fails, or never settles: the start is retried on a fresh pool a
 *   bounded number of times, then startup fails exhausted by step and reason
 *   and the process exits non-zero; no instance lock, DA sequence or
 *   follower is reached;
 * - it fails once: the retry starts the node;
 * - it succeeds: the node starts as before, on one pool.
 */
import { existsSync, mkdtempSync, readdirSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  findLedgerSignatureVerifierUnavailable,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect, Exit, Fiber, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { findStartupStepFailure } from "../src/services/startup-waiting.js";
import {
  FixedValidationWorkerPool,
  ValidationWorkerError,
} from "../src/services/validation-pool.js";

const reached = vi.hoisted(() => ({ instanceLock: 0, da: 0, follower: 0 }));

vi.mock("../src/fibers/resolve-worker-entry.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/fibers/resolve-worker-entry.js")
    >();
  return {
    ...actual,
    resolveWorkerEntry: (moduleUrl: string, fileName: string) =>
      fileName === "validation.js"
        ? new URL(
            "./listen-startup.validation-worker-readiness.worker.mjs",
            import.meta.url,
          )
        : actual.resolveWorkerEntry(moduleUrl, fileName),
  };
});
vi.mock("../src/services/index.js", async (importOriginal) => {
  const { Layer } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/services/index.js")>()),
    // validationPoolLayer stays the real one.
    mempoolLedgerCacheLayer: Layer.empty,
  };
});
vi.mock("../src/services/settlement.js", () => ({
  settlementWalletAddress: () => "unused-in-local-preflight",
}));
vi.mock("../src/e2e/phase1-accept-crash-checkpoint.js", async () => {
  const { Effect: E } = await import("effect");
  return { assertPhase1AcceptCrashCheckpointConfiguration: E.void };
});
vi.mock("../src/services/native-mpf-startup.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/native-mpf-startup.js")
    >()),
    requirePinnedNativeOwnerBinary: () => E.void,
  };
});
vi.mock("../src/services/node-instance-lock.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/node-instance-lock.js")
    >()),
    acquireNodeInstanceLock: () =>
      E.sync(() => {
        reached.instanceLock += 1;
        return { followerWriterLease: () => Promise.resolve(null) };
      }),
  };
});
vi.mock("../src/da/startup.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/da/startup.js")>()),
    runDaIdentityGatedStartupSequence: () =>
      E.sync(() => {
        reached.da += 1;
      }),
  };
});
vi.mock("../src/services/intent-journal.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/intent-journal.js")
    >()),
    makeIntentJournal: E.succeed({}),
  };
});
vi.mock("../src/services/l1-follower.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/l1-follower.js")
    >()),
    startL1Follower: () =>
      E.sync(() => {
        reached.follower += 1;
      }).pipe(E.zipRight(E.never)),
  };
});

const JOB_TIMEOUT_MS = 2_000;
/** Pool starts in all: the ledger signature verifier's startup bound. */
const ATTEMPTS = 3;
const INJECTED = "injected worker libsodium initialisation failure";

// The configuration a node is started with; the steps it reaches read only
// these fields.
const startupConfig = {
  PORT: 0,
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: 5432,
  POSTGRES_USER: "midgard",
  POSTGRES_PASSWORD: "midgard",
  POSTGRES_DB: "midgard",
  VALIDATION_WORKER_POOL_SIZE: 1,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: JOB_TIMEOUT_MS,
  NETWORK: "Preprod",
  MIN_FEE_A: 0n,
  MIN_FEE_B: 0n,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
} as unknown as NodeConfig["Type"];

type Mode = "fault" | "never" | "fault-once" | "ready";

/** Starts the node with the workers' initialisation set to `mode`, runs
 * `observe` on it, then stops it. */
const startNode = async (
  mode: Mode,
  observe: (node: {
    readonly fiber: Fiber.RuntimeFiber<void, unknown>;
    readonly pools: readonly FixedValidationWorkerPool[];
    readonly failures: readonly unknown[];
    readonly messages: readonly unknown[];
    /** How many workers read their libsodium initialisation. */
    readonly workersRead: () => number;
  }) => Promise<void>,
) => {
  await ledgerEd25519SignatureReady;
  expect(existsSync(new URL("../dist/validation.js", import.meta.url))).toBe(
    true,
  );
  const directory = mkdtempSync(join(tmpdir(), "midgard-worker-readiness-"));
  const marker = join(directory, "read");
  vi.stubEnv("MIDGARD_TEST_WORKER_READINESS_MODE", mode);
  vi.stubEnv("MIDGARD_TEST_WORKER_READINESS_MARKER", marker);
  reached.instanceLock = 0;
  reached.da = 0;
  reached.follower = 0;
  const pools: FixedValidationWorkerPool[] = [];
  const failures: unknown[] = [];
  const messages: unknown[] = [];
  const originalStart = FixedValidationWorkerPool.prototype.start;
  const startObserver = vi
    .spyOn(FixedValidationWorkerPool.prototype, "start")
    .mockImplementation(function (this: FixedValidationWorkerPool) {
      if (!pools.includes(this)) pools.push(this);
      return originalStart.call(this);
    });
  const fiber = Effect.runFork(
    (
      withStartupHttpServer(0, (startup) =>
        runNode(startup).pipe(
          Effect.tapError((error) => Effect.sync(() => failures.push(error))),
          Effect.provideService(NodeConfig, startupConfig),
          Effect.provideService(
            ContractDeploymentIdentity,
            ContractDeploymentIdentity.make({
              kind: "derived",
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            }),
          ),
          Effect.provide(Globals.Default),
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
    await observe({
      fiber,
      pools,
      failures,
      messages,
      workersRead: () =>
        readdirSync(directory).filter((name) => /^read\.\d+$/u.test(name))
          .length,
    });
  } finally {
    await Effect.runPromise(Fiber.interrupt(fiber));
    expect(pools.every((pool) => pool.isClosed())).toBe(true);
    startObserver.mockRestore();
    vi.unstubAllEnvs();
    rmSync(directory, { recursive: true, force: true });
  }
};

/** The node's exit, once startup ended it; fails if it is still running. */
const nodeExit = (fiber: Fiber.RuntimeFiber<void, unknown>) =>
  Effect.runPromise(
    Fiber.await(fiber).pipe(Effect.timeout(`${8 * JOB_TIMEOUT_MS} millis`)),
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
    "a worker initialisation that %s is retried within the bound, then exits non-zero before any durable work",
    async (_label, mode, lastCause) => {
      await startNode(mode, async ({ fiber, pools, messages, workersRead }) => {
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
        expect(reached).toEqual({ instanceLock: 0, da: 0, follower: 0 });
        expect(pools).toHaveLength(ATTEMPTS);
        expect(pools.every((pool) => pool.isClosed())).toBe(true);
        expect(workersRead()).toBe(ATTEMPTS);
        expect(messages).toContainEqual(
          expect.stringMatching(
            /^node_startup_failed stage=\S+ step=validation_workers reason=ledger_signature_verifier_unavailable; a transient failure outlived its bound; the node exits non-zero$/u,
          ),
        );
      });
    },
    60_000,
  );

  it("a worker initialisation that fails once is retried and the node starts", async () => {
    await startNode("fault-once", async ({ pools, failures, workersRead }) => {
      await vi.waitFor(() => expect(reached.follower).toBe(1), {
        timeout: 20_000,
      });
      expect(reached).toEqual({ instanceLock: 1, da: 1, follower: 1 });
      expect(failures).toEqual([]);
      expect(pools).toHaveLength(2);
      expect(pools[0]!.isClosed()).toBe(true);
      expect(pools[1]!.isClosed()).toBe(false);
      expect(workersRead()).toBe(2);
    });
  }, 60_000);

  it("a worker initialisation that succeeds starts the node as before, on one pool", async () => {
    await startNode(
      "ready",
      async ({ fiber, pools, failures, workersRead }) => {
        await vi.waitFor(() => expect(reached.follower).toBe(1), {
          timeout: 20_000,
        });
        expect(reached).toEqual({ instanceLock: 1, da: 1, follower: 1 });
        expect(failures).toEqual([]);
        expect(pools).toHaveLength(1);
        expect(pools[0]!.isClosed()).toBe(false);
        expect(workersRead()).toBe(1);
        expect(fiber.unsafePoll()).toBeNull();
      },
    );
  }, 60_000);
});
