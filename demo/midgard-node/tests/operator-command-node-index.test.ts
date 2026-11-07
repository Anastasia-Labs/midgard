/**
 * An operator command under `--l1 node` reads protocol state from its
 * running node's index (owner ruling D1): the services an operator command
 * runs under (`provideOperatorTxServices`) answer a protocol address from
 * the node's follower store, read-only, while the local node (a fake
 * transport sidecar, `helpers/fake-local-node.ts`) refuses every address
 * scan. A store the node has not initialized, or a database with no store,
 * fails the read with no address scan sent.
 */
import "./utils.js";

import {
  applyChainSyncEvent,
  type FactStore,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
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

import { provideOperatorTxServices } from "../src/commands/cli-runtime.js";
import { ToolMidgardContractServices } from "../src/commands/l1-tool-adapter.js";
import { NodeConfig } from "../src/services/config.js";
import {
  nodeFollowerStoreOptions,
  nodeIndexFromDeployment,
} from "../src/services/l1-follower.index-spec.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import {
  type DerivedDeployment,
  derivedDeployment,
} from "./helpers/derived-deployment.js";
import {
  type FakeLocalNode,
  fakeLocalNode,
} from "./helpers/fake-local-node.js";
import { testDatabases } from "./helpers/l1-events-store.js";

/** The suite's compiled profile is a Preprod one. */
const PREPROD_MAGIC = 1;
const databases = testDatabases();
let deployment: DerivedDeployment;
let node: FakeLocalNode;
const stores: FactStore[] = [];
/** The journal entries earlier tests left on the shared node. */
let journalled = 0;

beforeAll(async () => {
  deployment = await derivedDeployment();
  node = await fakeLocalNode(PREPROD_MAGIC);
  await node.answer("preprod");
});

afterAll(async () => {
  await databases.dropAll();
  await deployment.remove();
  await node.remove();
});

/** A database of its own, and the operator command environment over it. */
beforeEach(async () => {
  const database = new URL(await databases.create()).pathname.slice(1);
  for (const key of [
    "L1_PROVIDER",
    "L1_KUPO_URL",
    "L1_OGMIOS_URL",
    "L1_BLOCKFROST_URL",
    "L1_BLOCKFROST_PROJECT_ID",
  ])
    vi.stubEnv(key, "");
  deployment.stubEnvironment();
  vi.stubEnv("HUB_ORACLE_ONE_SHOT_TX_HASH", "11".repeat(32));
  vi.stubEnv("HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX", "0");
  vi.stubEnv("L1_ACCESS", "node");
  vi.stubEnv("L1_NODE_SOCKET_PATH", node.nativeLedger.socketPath);
  vi.stubEnv("L1_NODE_CONFIG_PATH", node.nativeLedger.nodeConfigPath);
  vi.stubEnv("L1_NODE_TRANSPORT_BINARY_PATH", node.nativeLedger.binaryPath);
  vi.stubEnv("POSTGRES_DB", database);
  journalled = (await node.journal()).length;
});

afterEach(async () => {
  await Promise.all(stores.splice(0).map((store) => store.close()));
  vi.unstubAllEnvs();
});

/** The node's follower store (the writer `listen` runs), started. */
const startNodeStore = async () => {
  const { config, contracts, identity } = await Effect.runPromise(
    Effect.all({
      config: NodeConfig,
      contracts: MidgardContracts,
      identity: ContractDeploymentIdentity,
    }).pipe(
      Effect.provide(ToolMidgardContractServices),
      Effect.provide(NodeConfig.layer),
    ),
  );
  const built = nodeIndexFromDeployment(config, contracts, identity);
  if (built.kind === "unconfigured") throw new Error(built.detail);
  const store = openPostgresFactStore({
    ...nodeFollowerStoreOptions(built.index, 0),
    connection: {
      connectionString: `postgresql://${config.POSTGRES_USER}:${config.POSTGRES_PASSWORD}@${config.POSTGRES_HOST}:${String(config.POSTGRES_PORT)}/${config.POSTGRES_DB}`,
    },
  });
  stores.push(store);
  expect((await store.start()).kind).toBe("ready");
  return { store, reserve: contracts.reserve.spendingScriptAddress };
};

/** One block paying 3 ADA to `address`, and its transaction. */
const payTo = (address: string) => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const { event, encoded } = chain.forward([
    {
      inputs: [chain.outsideInput()],
      outputs: [
        {
          address: Buffer.from(getAddressDetails(address).address.hex, "hex"),
          lovelace: 3_000_000n,
        },
      ],
      nonce: chain.nonce(),
    },
  ]);
  return { event, txHash: encoded.txHashes[0]!.toString("hex") };
};

/** An operator command reading `address` through its Lucid. */
const operatorRead = (address: string) =>
  Effect.runPromiseExit(
    provideOperatorTxServices(
      Effect.flatMap(Lucid, (lucid) =>
        Effect.tryPromise({
          try: () => lucid.api.utxosAt(address),
          catch: (cause) => cause,
        }),
      ),
    ),
  );

const failureOf = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit) ? String(Cause.squash(exit.cause)) : "succeeded";

const addressScans = async () =>
  (await node.journal())
    .slice(journalled)
    .filter((entry) => entry === "utxo_by_address");

describe("an operator command under --l1 node", () => {
  it("answers a protocol address from the node's store, never scanning the ledger", async () => {
    const { store, reserve } = await startNodeStore();
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const { event, txHash } = payTo(reserve);
    expect((await applyChainSyncEvent(store, event)).result.kind).toBe(
      "applied",
    );

    const read = await operatorRead(reserve);
    if (Exit.isFailure(read)) throw new Error(Cause.pretty(read.cause));
    expect(
      read.value.map((utxo) => [utxo.txHash, utxo.assets.lovelace]),
    ).toEqual([[txHash, 3_000_000n]]);
    expect(await addressScans()).toEqual([]);
  });

  it("fails on a store the node has not initialized, with no address scan", async () => {
    const { reserve } = await startNodeStore();
    expect(failureOf(await operatorRead(reserve))).toMatch(/not_initialized/);
    expect(await addressScans()).toEqual([]);
  });

  it("fails on a database with no store, with no address scan", async () => {
    const { contracts } = await Effect.runPromise(
      Effect.all({ contracts: MidgardContracts }).pipe(
        Effect.provide(ToolMidgardContractServices),
        Effect.provide(NodeConfig.layer),
      ),
    );
    const read = await operatorRead(contracts.reserve.spendingScriptAddress);
    expect(failureOf(read)).toMatch(/store unavailable/);
    expect(await addressScans()).toEqual([]);
  });
});
