/**
 * The node's startup waits for the ledger signature verifier before any
 * durable work or the L1 follower
 * (`listen-startup.await-ledger-signature-verifier.ts`). Here libsodium's
 * initialisation fails: startup fails by step and reason and the process
 * exits non-zero for its supervisor to restart, instead of running and
 * holding. The instance lock is never taken and the follower never starts,
 * so no block is followed, processed or held.
 */
import { Effect, Exit } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import {
  LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS,
  LEDGER_SIGNATURE_VERIFIER_STEP,
  LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
} from "../src/commands/listen-startup.await-ledger-signature-verifier.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { findStartupStepFailure } from "../src/services/startup-waiting.js";

const reached = vi.hoisted(() => ({ instanceLock: 0, follower: 0 }));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    const failed = Promise.reject(new Error("WebAssembly is unavailable"));
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
vi.mock("../src/services/index.js", async (importOriginal) => {
  const { Layer } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/services/index.js")>()),
    validationPoolLayer: Layer.empty,
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
    runDaIdentityGatedStartupSequence: () => E.void,
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

describe("node startup when the ledger signature verifier fails to initialise", () => {
  it("fails the startup by step and reason, exiting, and never starts the follower", async () => {
    // The configuration a node is started with; the steps it reaches read
    // only these fields.
    const exit = await Effect.runPromiseExit(
      withStartupHttpServer(0, (startup) =>
        runNode(startup).pipe(
          Effect.provideService(NodeConfig, {
            PORT: 0,
            POSTGRES_HOST: "127.0.0.1",
            POSTGRES_PORT: 5432,
            POSTGRES_USER: "midgard",
            POSTGRES_PASSWORD: "midgard",
            POSTGRES_DB: "midgard",
          } as unknown as NodeConfig["Type"]),
          Effect.provide(Globals.Default),
        ),
      ) as Effect.Effect<void, unknown>,
    );

    expect(Exit.isFailure(exit)).toBe(true);
    const step = Exit.isFailure(exit)
      ? findStartupStepFailure(exit.cause)
      : undefined;
    expect(step).toMatchObject({
      step: LEDGER_SIGNATURE_VERIFIER_STEP,
      reason: LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
      exhausted: true,
      attempts: LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS.maxAttempts,
    });
    expect(reached).toEqual({ instanceLock: 0, follower: 0 });
  }, 30_000);
});
