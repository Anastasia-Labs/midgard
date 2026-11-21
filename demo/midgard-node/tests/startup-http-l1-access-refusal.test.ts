/**
 * `listen` started with a non-follower L1 setting, or with no local node,
 * is a deterministic startup failure no restart repairs (plan §7.5): the
 * registered command holds up and unready, and both `/readyz` and the log
 * name the `l1_access` step and its reason. The refusal comes before any
 * L1 open: the local node configured never sees a handshake. The process
 * starts from a derived deployment (`helpers/derived-deployment.ts`), whose
 * contracts make the
 * first L1 read.
 */
import type { Server } from "node:http";

import { Effect, Fiber } from "effect";
import {
  afterAll,
  afterEach,
  beforeAll,
  describe,
  expect,
  it,
  vi,
} from "vitest";

import type { NativeLedgerSettings } from "../src/services/native-ledger.js";
import {
  type DerivedDeployment,
  derivedDeployment,
} from "./helpers/derived-deployment.js";
import {
  type FakeLocalNode,
  fakeLocalNode,
} from "./helpers/fake-local-node.js";

const seen = vi.hoisted(() => ({
  servers: [] as Server[],
  running: undefined as Fiber.RuntimeFiber<unknown, unknown> | undefined,
  nativeLedger: undefined as NativeLedgerSettings | undefined,
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
      Effect.sync(() =>
        makeTestNodeConfig({
          PORT: 0,
          NETWORK: "Custom",
          L1_NATIVE_LEDGER: seen.nativeLedger,
          L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 100,
        }),
      ),
    ),
  });
  return original;
});
vi.mock("../src/services/index.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/index.js")>();
  const { Layer } = await import("effect");
  Object.defineProperty(original.Globals, "Default", { value: Layer.empty });
  return {
    ...original,
    Database: { ...original.Database, layer: Layer.empty },
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
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

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

let node: FakeLocalNode;
let deployment: DerivedDeployment;

beforeAll(async () => {
  node = await fakeLocalNode();
  await node.answer("answer");
  deployment = await derivedDeployment();
});

afterAll(async () => {
  await node.remove();
  await deployment.remove();
});

/** The process environment with every L1 setting cleared. */
const clearedEnvironment = () => {
  for (const key of CLEARED) vi.stubEnv(key, "");
  deployment.stubEnvironment();
};

/** Starts `listen` and returns its readiness once startup has failed. */
const heldReadiness = async () => {
  seen.servers = [];
  seen.running = undefined;
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
});

describe("listen's L1-access refusals reach readiness", () => {
  it("names role_non_follower_l1_config for a Kupmios setting, and opens nothing", async () => {
    clearedEnvironment();
    vi.stubEnv("L1_KUPO_URL", "http://kupo:1442");
    seen.nativeLedger = node.nativeLedger;
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
    expect(await node.journal()).toEqual([]);
    expect(consoleText(spies)).toContain(
      "step=l1_access reason=role_non_follower_l1_config",
    );
  });

  it("names l1_node_unconfigured for a follower with no local node", async () => {
    clearedEnvironment();
    seen.nativeLedger = undefined;
    const spies = (["log", "info", "error", "warn"] as const).map((method) =>
      vi.spyOn(console, method),
    );
    const body = await heldReadiness();
    expect(body).toMatchObject({
      reasons: ["startup_failed", "l1_node_unconfigured"],
      failedStep: "l1_access",
      failedReason: "l1_node_unconfigured",
    });
    expect(consoleText(spies)).toContain(
      "step=l1_access reason=l1_node_unconfigured",
    );
  });
});
