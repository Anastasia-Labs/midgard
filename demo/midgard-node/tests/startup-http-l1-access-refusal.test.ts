/**
 * `listen` started with a non-follower L1 setting, or with no local node,
 * is a deterministic startup failure no restart repairs (plan §7.5): the
 * registered command holds up and unready, and both `/readyz` and the log
 * name the `l1_access` step and its reason.
 */
import type { Server } from "node:http";

import { Effect, Fiber } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { NodeConfig } from "../src/services/config.js";

const seen = vi.hoisted(() => ({
  servers: [] as Server[],
  running: undefined as Fiber.RuntimeFiber<unknown, unknown> | undefined,
  followerOpens: 0,
  nativeLedger: true,
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
  const { Effect, Layer } = await import("effect");
  Object.defineProperty(original.NodeConfig, "layer", {
    value: Layer.effect(
      original.NodeConfig,
      Effect.sync(
        () =>
          ({
            PORT: 0,
            NETWORK: "Custom",
            L1_NATIVE_LEDGER: seen.nativeLedger
              ? {
                  socketPath: "/run/cardano/node.socket",
                  nodeConfigPath: "/etc/cardano/config.json",
                  binaryPath: "/opt/midgard/bin/midgard-l1-node-transport",
                }
              : undefined,
            L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 100,
          }) as NodeConfig["Type"],
      ),
    ),
  });
  return original;
});
vi.mock("../src/services/l1-provider.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/l1-provider.js")>();
  return {
    ...original,
    openNodeL1AccessFromConfig: async () => {
      seen.followerOpens += 1;
      throw new Error("the follower store must not be opened");
    },
  };
});
vi.mock("../src/services/index.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/index.js")>();
  const { Layer } = await import("effect");
  Object.defineProperty(original.Globals, "Default", { value: Layer.empty });
  return {
    ...original,
    Database: { ...original.Database, layer: Layer.empty },
    MidgardContractServices: Layer.empty,
    AdmissionWriterLive: Layer.empty,
    WriteBehindLive: Layer.empty,
  };
});
vi.mock("../src/commands/cli-runtime.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/commands/cli-runtime.js")>();
  const { Effect } = await import("effect");
  return {
    ...original,
    runCliEffect: (effect: Effect.Effect<unknown, unknown>) => {
      seen.running = Effect.runFork(effect);
    },
  };
});

import "../src/index.registration-2.js";

import { program } from "../src/index.registration.js";

const CLEARED = [
  "L1_ACCESS",
  "L1_PROVIDER",
  "L1_KUPO_URL",
  "L1_OGMIOS_URL",
  "L1_BLOCKFROST_URL",
  "L1_BLOCKFROST_PROJECT_ID",
];

/** Everything the process wrote to its console, for the log assertion. */
const consoleText = (spies: ReturnType<typeof vi.spyOn>[]) =>
  spies
    .flatMap((spy) => spy.mock.calls)
    .map((call) => call.map(String).join(" "))
    .join("\n");

/** Starts `listen` and returns its readiness once startup has failed. */
const heldReadiness = async () => {
  seen.servers = [];
  seen.running = undefined;
  seen.followerOpens = 0;
  await program.parseAsync(["node", "midgard-node", "listen"]);
  await vi.waitFor(() => expect(seen.servers).toHaveLength(1));
  const address = seen.servers[0]!.address();
  if (address === null || typeof address === "string")
    throw new Error("Expected bound TCP listener");
  const url = `http://127.0.0.1:${address.port}`;
  let body: Record<string, unknown> = {};
  await expect
    .poll(async () => {
      const ready = await fetch(`${url}/readyz`);
      body = (await ready.json()) as Record<string, unknown>;
      return { status: ready.status, stage: body.stage };
    })
    .toEqual({ status: 503, stage: "fatal" });
  // Held, not exited: no restart loop over a failure a restart cannot fix.
  expect(seen.running!.unsafePoll()).toBeNull();
  return body;
};

afterEach(async () => {
  if (seen.running !== undefined && seen.running.unsafePoll() === null)
    await Effect.runPromise(Fiber.interrupt(seen.running));
  expect(seen.servers.every((server) => !server.listening)).toBe(true);
  vi.unstubAllEnvs();
  vi.restoreAllMocks();
  seen.nativeLedger = true;
});

describe("listen's L1-access refusals reach readiness", () => {
  it("names role_non_follower_l1_config for a Kupmios setting, and opens no store", async () => {
    for (const key of CLEARED) vi.stubEnv(key, "");
    vi.stubEnv("L1_KUPO_URL", "http://kupo:1442");
    const spies = (["log", "info", "error", "warn"] as const).map((method) =>
      vi.spyOn(console, method),
    );
    const body = await heldReadiness();
    expect(body).toMatchObject({
      ready: false,
      reasons: ["startup_failed", "role_non_follower_l1_config"],
      failedStep: "l1_access",
      failedReason: "role_non_follower_l1_config",
    });
    expect(seen.followerOpens).toBe(0);
    expect(consoleText(spies)).toContain(
      "step=l1_access reason=role_non_follower_l1_config",
    );
  });

  it("names l1_node_unconfigured for a follower with no local node", async () => {
    for (const key of CLEARED) vi.stubEnv(key, "");
    seen.nativeLedger = false;
    const spies = (["log", "info", "error", "warn"] as const).map((method) =>
      vi.spyOn(console, method),
    );
    const body = await heldReadiness();
    expect(body).toMatchObject({
      reasons: ["startup_failed", "l1_node_unconfigured"],
      failedStep: "l1_access",
      failedReason: "l1_node_unconfigured",
    });
    expect(seen.followerOpens).toBe(0);
    expect(consoleText(spies)).toContain(
      "step=l1_access reason=l1_node_unconfigured",
    );
  });
});
