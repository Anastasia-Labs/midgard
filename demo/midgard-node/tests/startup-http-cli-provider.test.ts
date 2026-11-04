/**
 * The registered `listen` command over a local node (a fake transport
 * sidecar, `helpers/fake-local-node.ts`) from a derived deployment: its
 * contracts and Lucid read the chain parameters from the node before any
 * later service. While the node has not answered, the probes are bound,
 * admission and init refuse, and no database is acquired. A node not up yet
 * is waited through under its named reason, and startup passes the L1
 * services once the node answers; an answer no restart repairs holds the
 * process up and unready (plan §7.5, owner ruling 2026-10-09;
 * `withStartupHttpServer`).
 */
import type { Server } from "node:http";

import * as LE from "@lucid-evolution/lucid";
import { Effect, Fiber } from "effect";
import {
  afterAll,
  afterEach,
  beforeAll,
  beforeEach,
  describe,
  expect,
  it,
  vi,
} from "vitest";

import type { NodeConfig } from "../src/services/config.js";
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
  databaseAcquisitions: 0,
  nativeLedger: undefined as NativeLedgerSettings | undefined,
  seeds: [] as string[],
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
  const LE = await import("@lucid-evolution/lucid");
  const { Effect, Layer } = await import("effect");
  const address = (seed: string) =>
    LE.walletFromSeed(seed, { network: "Custom" }).address;
  Object.defineProperty(original.NodeConfig, "layer", {
    value: Layer.effect(
      original.NodeConfig,
      Effect.sync(
        () =>
          ({
            PORT: 0,
            NETWORK: "Custom",
            L1_NATIVE_LEDGER: seen.nativeLedger,
            L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 100,
            L1_NODE_BEHIND_MAX_MS: 60_000,
            HUB_ORACLE_ONE_SHOT_TX_HASH: "11".repeat(32),
            HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: 0,
            L1_OPERATOR_SEED_PHRASE: seen.seeds[0],
            L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: seen.seeds[1],
            L1_REFERENCE_SCRIPT_SEED_PHRASE: seen.seeds[2],
            L1_REFERENCE_SCRIPT_ADDRESS: address(seen.seeds[2]!),
            L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: address(seen.seeds[2]!),
            // The follower store is opened lazily; nothing here reads it.
            POSTGRES_HOST: "127.0.0.1",
            POSTGRES_PORT: 1,
            POSTGRES_USER: "unused",
            POSTGRES_PASSWORD: "unused",
            POSTGRES_DB: "unused",
          }) as unknown as NodeConfig["Type"],
      ),
    ),
  });
  return original;
});
vi.mock("../src/services/index.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/index.js")>();
  const { Effect, Layer } = await import("effect");
  // The contracts, the follower adapter and Lucid.Default are the real
  // ones; the database acquisition marks startup past the L1 services.
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

const L1_SETTINGS = [
  "L1_ACCESS",
  "L1_PROVIDER",
  "L1_KUPO_URL",
  "L1_OGMIOS_URL",
  "L1_BLOCKFROST_URL",
  "L1_BLOCKFROST_PROJECT_ID",
];

let deployment: DerivedDeployment;
let node: FakeLocalNode;

beforeAll(async () => {
  seen.seeds = [0, 1, 2].map(() => LE.generateSeedPhrase());
  deployment = await derivedDeployment();
});

afterAll(async () => {
  await deployment.remove();
});

beforeEach(async () => {
  for (const key of L1_SETTINGS) vi.stubEnv(key, "");
  deployment.stubEnvironment();
  node = await fakeLocalNode();
  seen.nativeLedger = node.nativeLedger;
  seen.servers = [];
  seen.running = undefined;
  seen.databaseAcquisitions = 0;
});

afterEach(async () => {
  if (seen.running !== undefined && seen.running.unsafePoll() === null)
    await Effect.runPromise(Fiber.interrupt(seen.running));
  expect(seen.servers.every((server) => !server.listening)).toBe(true);
  await node.remove();
  vi.unstubAllEnvs();
});

const readiness = async (url: string) => {
  const ready = await fetch(`${url}/readyz`);
  return {
    status: ready.status,
    body: (await ready.json()) as Record<string, unknown>,
  };
};

/**
 * Starts the registered `listen` command up to its first chain-parameter
 * query, which the node holds unanswered, and checks what the listener
 * serves there: live probes, no admission or init, no database. Returns the
 * listener's base URL.
 */
const startToL1Boundary = async (): Promise<string> => {
  await program.parseAsync(["node", "midgard-node", "listen"]);
  await vi.waitFor(
    async () =>
      expect((await node.journal()).some((entry) => entry !== "hello")).toBe(
        true,
      ),
    { timeout: 60_000 },
  );
  expect(seen.servers).toHaveLength(1);
  const address = seen.servers[0]!.address();
  if (address === null || typeof address === "string")
    throw new Error("Expected bound TCP listener");
  const url = `http://127.0.0.1:${address.port}`;
  const health = await fetch(`${url}/healthz`);
  expect(health.status).toBe(200);
  expect(await health.json()).toEqual({
    status: "ok",
    stage: "runtime_services",
  });
  expect(await readiness(url)).toEqual({
    status: 503,
    body: {
      ready: false,
      reasons: ["startup_incomplete"],
      stage: "runtime_services",
    },
  });
  expect(
    (await fetch(`${url}/submit`, { method: "POST", body: "unparsed" })).status,
  ).toBe(503);
  expect((await fetch(`${url}/init`)).status).toBe(503);
  expect(seen.databaseAcquisitions).toBe(0);
  return url;
};

describe("registered listen command startup over its local node", () => {
  it("binds probes before the node answers, rejects admission/init, then holds a slot mapping no restart repairs up and unready", async () => {
    const url = await startToL1Boundary();
    // The same start passes the L1 services over an answering node (below):
    // only the node's slot length fails it here.
    await node.answer("slot-length-2000");
    await expect
      .poll(() => readiness(url), { timeout: 30_000 })
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
  });

  it("waits through a node that is not up yet under its named reason, then passes the L1 services once the node answers", async () => {
    const url = await startToL1Boundary();
    await node.answer("unavailable");
    await expect
      .poll(async () => (await readiness(url)).body.reasons, {
        timeout: 30_000,
      })
      .toContain("l1_slot_mapping_pending");
    expect(seen.databaseAcquisitions).toBe(0);
    await node.answer("answer");
    await expect
      .poll(() => seen.databaseAcquisitions, { timeout: 60_000 })
      .toBe(1);
  });
});
