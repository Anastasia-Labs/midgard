import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { Context, Effect, Fiber, Layer } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";

const nativeStartup = vi.hoisted(() => ({
  entered: undefined as (() => void) | undefined,
}));

vi.mock("../src/services/index.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/services/index.js")>()),
  // No worker or ledger-cache startup is needed to exercise the boundary
  // before the native binary preflight. All later domain effects remain real.
  validationPoolLayer: Layer.empty,
  mempoolLedgerCacheLayer: Layer.empty,
}));
vi.mock("../src/services/settlement.js", () => ({
  settlementWalletAddress: () => "unused-before-native-preflight",
}));
vi.mock("../src/e2e/phase1-accept-crash-checkpoint.js", () => ({
  assertPhase1AcceptCrashCheckpointConfiguration: Effect.void,
}));
vi.mock("../src/services/native-mpf-startup.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/services/native-mpf-startup.js")
  >()),
  requirePinnedNativeOwnerBinary: () =>
    Effect.sync(() => nativeStartup.entered?.()).pipe(
      Effect.zipRight(Effect.never),
    ),
}));

const unusedPort = async (): Promise<number> => {
  const server = createServer();
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  await new Promise<void>((resolve, reject) =>
    server.close((error) => (error ? reject(error) : resolve())),
  );
  return port;
};

const assertPortReleased = async (port: number): Promise<void> => {
  const server = createServer();
  try {
    server.listen(port, "127.0.0.1");
    await once(server, "listening");
  } finally {
    if (server.listening) {
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    }
  }
};

class ApplicationLabel extends Context.Tag("StartupHttpApplicationLabel")<
  ApplicationLabel,
  string
>() {}

describe("node HTTP startup boundary", () => {
  it("binds before caller service bootstrap and keeps serving probes while runNode starts", async () => {
    const port = await unusedPort();
    let releaseBootstrap!: () => void;
    let signalBootstrap!: () => void;
    const bootstrapEntered = new Promise<void>((resolve) => {
      signalBootstrap = resolve;
    });
    const bootstrapRelease = new Promise<void>((resolve) => {
      releaseBootstrap = resolve;
    });
    const nativeEntered = new Promise<void>((resolve) => {
      nativeStartup.entered = resolve;
    });
    const provider = Layer.effect(
      ApplicationLabel,
      Effect.promise(async () => {
        signalBootstrap();
        await bootstrapRelease;
        return "provider-ready";
      }),
    );
    // Same ordering as the operator listen action (`runListen`): the early
    // listener wraps service bootstrap, and runNode publishes into it.
    const fiber = Effect.runFork(
      withStartupHttpServer(port, (startup) =>
        runNode(startup).pipe(
          Effect.provide(provider),
          Effect.provideService(NodeConfig, {
            PORT: port,
          } as NodeConfig["Type"]),
          Effect.provide(Globals.Default),
        ),
      ) as Effect.Effect<void, unknown>,
    );
    const stopped = Effect.runPromise(Fiber.await(fiber)).then(() => {
      throw new Error("Node stopped before reaching the held startup stage");
    });
    try {
      await Promise.race([bootstrapEntered, stopped]);
      const base = `http://127.0.0.1:${port.toString()}`;
      const health = await fetch(`${base}/healthz`);
      expect(health.status).toBe(200);
      await health.arrayBuffer();
      const ready = await fetch(`${base}/readyz`);
      expect(ready.status).toBe(503);
      expect(await ready.json()).toMatchObject({
        reasons: ["startup_incomplete"],
        stage: "runtime_services",
      });
      releaseBootstrap();
      await Promise.race([nativeEntered, stopped]);
      const stillStarting = await fetch(`${base}/readyz`);
      expect(stillStarting.status).toBe(503);
      expect(await stillStarting.json()).toMatchObject({
        reasons: ["startup_incomplete"],
        stage: "local_preflight",
      });
    } finally {
      releaseBootstrap();
      nativeStartup.entered = undefined;
      await Effect.runPromise(Fiber.interrupt(fiber));
    }
    await assertPortReleased(port);
  }, 15_000);

  it("serves probes and refuses work while the real startup sequence waits on native preflight", async () => {
    const port = await unusedPort();
    const entered = new Promise<void>((resolve) => {
      nativeStartup.entered = resolve;
    });
    // Startup is deliberately held before it dereferences the remaining
    // settings or services. The cast describes that partial test boundary.
    const fiber = Effect.runFork(
      withStartupHttpServer(port, (startup) =>
        runNode(startup).pipe(
          Effect.provideService(NodeConfig, {
            PORT: port,
          } as NodeConfig["Type"]),
          Effect.provide(Globals.Default),
        ),
      ) as Effect.Effect<void, unknown>,
    );
    try {
      await Promise.race([
        entered,
        Effect.runPromise(Fiber.await(fiber)).then(() => {
          throw new Error("Node stopped before reaching native preflight");
        }),
      ]);
      const base = `http://127.0.0.1:${port.toString()}`;
      const health = await fetch(`${base}/healthz`);
      expect(health.status).toBe(200);
      expect(await health.json()).toMatchObject({ status: "ok" });
      const ready = await fetch(`${base}/readyz`);
      expect(ready.status).toBe(503);
      expect(await ready.json()).toMatchObject({
        ready: false,
        reasons: ["startup_incomplete"],
      });
      for (const route of ["/submit", "/deposit/build", "/commit", "/init"]) {
        const response = await fetch(`${base}${route}`, { method: "POST" });
        expect(response.status, route).toBe(503);
        await response.arrayBuffer();
      }
    } finally {
      nativeStartup.entered = undefined;
      await Effect.runPromise(Fiber.interrupt(fiber));
    }
  }, 15_000);
});
