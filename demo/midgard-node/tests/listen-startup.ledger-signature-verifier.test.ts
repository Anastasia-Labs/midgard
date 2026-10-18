/**
 * The node's startup waits for the ledger signature verifier before any
 * durable work or the L1 follower
 * (`listen-startup.await-ledger-signature-verifier.ts`). Here libsodium
 * initialises: startup takes the instance lock and starts the L1 follower.
 * Its failure: `listen-startup.ledger-signature-verifier.fault.test.ts`.
 */
import { Effect, Fiber } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";

const reached = vi.hoisted(() => ({ instanceLock: 0, follower: 0 }));

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

describe("node startup once the ledger signature verifier initialises", () => {
  it("takes the instance lock and starts the follower", async () => {
    // The configuration a node is started with; the steps it reaches read
    // only these fields. The follower, once started, holds startup there.
    const fiber = Effect.runFork(
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
    try {
      await vi.waitFor(() => expect(reached.follower).toBe(1), {
        timeout: 20_000,
        interval: 50,
      });
      expect(reached.instanceLock).toBe(1);
      expect(fiber.unsafePoll()).toBeNull();
    } finally {
      await Effect.runPromise(Fiber.interrupt(fiber));
    }
  }, 30_000);
});
