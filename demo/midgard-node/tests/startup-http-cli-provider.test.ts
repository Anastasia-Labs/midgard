import type { Server } from "node:http";

import { Cause, Effect, Exit } from "effect";
import { describe, expect, it, vi } from "vitest";

import type { NodeConfig } from "../src/services/config.js";

const seen = vi.hoisted(() => ({
  servers: [] as Server[],
  resumeProvider: undefined as
    | ((effect: Effect.Effect<never, Error>) => void)
    | undefined,
  completion: undefined as Promise<Exit.Exit<unknown, unknown>> | undefined,
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
      L1_OGMIOS_KEY: "http://provider.invalid",
      L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 100,
    } as NodeConfig["Type"]),
  });
  return original;
});
vi.mock("../src/custom-slot-mapping.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/custom-slot-mapping.js")>();
  const { Effect } = await import("effect");
  return {
    ...original,
    resolveCustomSlotMapping: () =>
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
  // must stay unreachable while Lucid waits for its Custom network observation.
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
      seen.completion = Effect.runPromiseExit(effect);
    },
  };
});

import "../src/index.registration-2.js";

import { program } from "../src/index.registration.js";

describe("registered listen command startup/provider order", () => {
  it("binds probes before the real Lucid provider boundary, rejects admission/init, then propagates failure and closes", async () => {
    await program.parseAsync(["node", "midgard-node", "listen"]);
    await vi.waitFor(() => expect(seen.resumeProvider).toBeTypeOf("function"));
    const providerFailure = new Error("provider boundary failed");
    try {
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
        (await fetch(`${url}/submit`, { method: "POST", body: "unparsed" }))
          .status,
      ).toBe(503);
      expect((await fetch(`${url}/init`)).status).toBe(503);
      expect(seen.databaseAcquisitions).toBe(0);
    } finally {
      seen.resumeProvider!(Effect.fail(providerFailure));
      const exit = await seen.completion!;
      expect(Exit.isFailure(exit)).toBe(true);
      if (Exit.isFailure(exit)) {
        expect(Cause.pretty(exit.cause)).toContain(
          "Failed to initialize the Lucid slot mapping",
        );
        expect(Cause.failureOption(exit.cause)).toMatchObject({
          _tag: "Some",
          value: { cause: providerFailure },
        });
      }
      expect(seen.servers.every((server) => !server.listening)).toBe(true);
    }
  });
});
