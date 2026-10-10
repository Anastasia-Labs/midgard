import type { Server } from "node:http";

import { Cause, Effect, Exit, Fiber } from "effect";
import { describe, expect, it, vi } from "vitest";

import type { NodeConfig } from "../src/services/config.js";

const seen = vi.hoisted(() => ({
  servers: [] as Server[],
  resumeProvider: undefined as
    | ((effect: Effect.Effect<never, Error>) => void)
    | undefined,
  running: undefined as Fiber.RuntimeFiber<unknown, unknown> | undefined,
  databaseAcquisitions: 0,
}));

vi.mock("node:http", async (importOriginal) => {
  const original = await importOriginal<typeof import("node:http")>();
  return {
    ...original,
    createServer: (...args: Parameters<typeof original.createServer>) => {
      const server = original.createServer(...args);
      seen.servers.push(server);
      return server;
    },
  };
});
vi.mock("../src/runtime-env.js", () => ({ loadRuntimeDotenv: () => {} }));
vi.mock("../src/services/config.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/config.js")>();
  const { Layer } = await import("effect");
  // Only locally decoded fields read before the real provider resolver. No
  // wallets, artifacts or live configuration are loaded in this wiring probe.
  Object.defineProperty(original.NodeConfig, "layer", {
    value: Layer.succeed(original.NodeConfig, {
      PORT: 0,
      NETWORK: "Custom",
      L1_NATIVE_LEDGER: {
        socketPath: "/run/cardano/node.socket",
        nodeConfigPath: "/etc/cardano/config.json",
        binaryPath: "/opt/midgard/bin/midgard-l1-node-transport",
      },
      L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 100,
    } as NodeConfig["Type"]),
  });
  return original;
});
// The node's L1 access opens without a node; the slot-mapping read below
// is the boundary the startup waits on.
vi.mock("../src/services/l1-provider.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/l1-provider.js")>();
  return {
    ...original,
    openNodeL1AccessFromConfig: async () => ({
      endpoint: "/run/cardano/node.socket",
      close: async () => {},
    }),
  };
});
vi.mock("../src/custom-slot-mapping.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/custom-slot-mapping.js")>();
  const { Effect } = await import("effect");
  return {
    ...original,
    resolveLucidSlotMapping: () =>
      Effect.async<never, Error>((resume) => {
        seen.resumeProvider = resume;
      }),
  };
});
vi.mock("../src/services/index.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/index.js")>();
  const { Effect, Layer } = await import("effect");
  // The actual provider composition and Lucid.Default remain; later services
  // must stay unreachable while Lucid waits on the ledger's slot mapping.
  Object.defineProperty(original.Globals, "Default", { value: Layer.empty });
  return {
    ...original,
    Database: {
      ...original.Database,
      layer: Layer.effectDiscard(
        Effect.sync(() => {
          seen.databaseAcquisitions++;
        }),
      ),
    },
    MidgardContractServices: Layer.empty,
    AdmissionWriterLive: Layer.empty,
    WriteBehindLive: Layer.empty,
  };
});
vi.mock("../src/commands/cli-runtime.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/commands/cli-runtime.js")>();
  const { Effect } = await import("effect");
  // Replace process teardown only; execute the actual registered command.
  return {
    ...original,
    runCliEffect: (effect: Effect.Effect<unknown, unknown>) => {
      seen.running = Effect.runFork(effect);
    },
  };
});

import "../src/index.registration-2.js";

import { program } from "../src/index.registration.js";
import { startupStepFailed } from "../src/services/startup-waiting.js";

/**
 * Starts the registered `listen` command up to the provider boundary and
 * checks what the listener serves there: live probes, no admission or init,
 * no database. Returns the listener's base URL.
 */
const startToProviderBoundary = async (): Promise<string> => {
  seen.servers = [];
  seen.resumeProvider = undefined;
  seen.running = undefined;
  await program.parseAsync(["node", "midgard-node", "listen"]);
  await vi.waitFor(() => expect(seen.resumeProvider).toBeTypeOf("function"));
  expect(seen.servers).toHaveLength(1);
  const address = seen.servers[0]!.address();
  expect(address).toBeTypeOf("object");
  if (address === null || typeof address === "string")
    throw new Error("Expected bound TCP listener");
  const url = `http://127.0.0.1:${address.port}`;
  const health = await fetch(`${url}/healthz`);
  expect(health.status).toBe(200);
  expect(await health.json()).toEqual({
    status: "ok",
    stage: "runtime_services",
  });
  const ready = await fetch(`${url}/readyz`);
  expect(ready.status).toBe(503);
  expect(await ready.json()).toEqual({
    ready: false,
    reasons: ["startup_incomplete"],
    stage: "runtime_services",
  });
  expect(
    (await fetch(`${url}/submit`, { method: "POST", body: "unparsed" })).status,
  ).toBe(503);
  expect((await fetch(`${url}/init`)).status).toBe(503);
  expect(seen.databaseAcquisitions).toBe(0);
  return url;
};

/** Interrupts a startup still running, so a failed case cannot leak it. */
const stopStartup = async () => {
  if (seen.running !== undefined && seen.running.unsafePoll() === null)
    await Effect.runPromise(Fiber.interrupt(seen.running));
};

// A failure no restart repairs holds the process up and unready, and a
// transient one that outlived its bound exits (plan §7.5, owner ruling
// 2026-10-09; `withStartupHttpServer`).
describe("registered listen command startup/provider order", () => {
  it("binds probes before the real Lucid provider boundary, rejects admission/init, then holds a deterministic provider failure up and unready", async () => {
    const url = await startToProviderBoundary();
    try {
      seen.resumeProvider!(Effect.fail(new Error("provider boundary failed")));
      await expect
        .poll(async () => {
          const ready = await fetch(`${url}/readyz`);
          return { status: ready.status, body: await ready.json() };
        })
        .toEqual({
          status: 503,
          body: {
            ready: false,
            reasons: ["startup_failed"],
            stage: "fatal",
            failedStage: "runtime_services",
          },
        });
      const health = await fetch(`${url}/healthz`);
      expect(health.status).toBe(200);
      expect(await health.json()).toEqual({
        status: "held",
        stage: "fatal",
        failedStage: "runtime_services",
      });
      // Held, not exited: the command has not completed.
      expect(seen.running!.unsafePoll()).toBeNull();
      expect(seen.databaseAcquisitions).toBe(0);
    } finally {
      await stopStartup();
    }
    expect(seen.servers.every((server) => !server.listening)).toBe(true);
  });

  it("propagates a provider read whose transient budget ran out, and closes", async () => {
    await startToProviderBoundary();
    const exhausted = startupStepFailed({
      step: "l1_slot_mapping",
      reason: "l1_slot_mapping_pending",
      cause: new Error("provider boundary unreachable"),
      exhausted: true,
      attempts: 3,
    });
    try {
      seen.resumeProvider!(Effect.fail(exhausted));
      const exit = await Effect.runPromise(Fiber.await(seen.running!));
      expect(Exit.isFailure(exit)).toBe(true);
      if (Exit.isFailure(exit)) {
        expect(Cause.pretty(exit.cause)).toContain(
          "Failed to initialize the Lucid slot mapping",
        );
        expect(Cause.failureOption(exit.cause)).toMatchObject({
          _tag: "Some",
          value: { cause: exhausted },
        });
      }
    } finally {
      await stopStartup();
    }
    expect(seen.servers.every((server) => !server.listening)).toBe(true);
    expect(seen.databaseAcquisitions).toBe(0);
  });
});
