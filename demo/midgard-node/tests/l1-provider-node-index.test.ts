/**
 * The node's L1 reader is configured with the node's index from its open
 * (`nodeIndexSpec`): a protocol address is answered from the follower's
 * facts, never by a ledger address scan, even before the follower has
 * recorded its tracked set; and the reader and `listen`'s follower configure
 * the same set, so reopening the store through either never resets it.
 */
import type { LedgerQuery } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import {
  applyChainSyncEvent,
  type FactStore,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import type * as SDK from "@al-ft/midgard-sdk";
import { generateSeedPhrase, getAddressDetails } from "@lucid-evolution/lucid";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  nodeFollowerStoreOptions,
  type NodeIndexSpec,
  nodeIndexSpec,
} from "../src/services/l1-follower.index-spec.js";
import {
  type NodeL1Access,
  nodeL1AccessOver,
} from "../src/services/l1-provider.js";
import { emulatorFollowerPlan } from "./helpers/follower-emulator.host.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const databases = testDatabases();
const stores: FactStore[] = [];
const accesses: NodeL1Access[] = [];
afterEach(async () => {
  await Promise.all(accesses.splice(0).map((access) => access.close()));
  await Promise.all(stores.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

let contracts: SDK.MidgardValidators;
let index: NodeIndexSpec;
beforeAll(async () => {
  contracts = await loadRealMidgardContractsForTest({
    txHash: "11".repeat(32),
    outputIndex: 0,
  });
  index = nodeIndexSpec(
    {
      NETWORK: "Preprod",
      L1_OPERATOR_SEED_PHRASE: generateSeedPhrase(),
      L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "",
      L1_SETTLEMENT_SEED_PHRASE: "",
      L1_REFERENCE_SCRIPT_SEED_PHRASE: "",
      L1_REFERENCE_SCRIPT_ADDRESS: "",
      L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: "",
    },
    contracts,
    emulatorFollowerPlan({
      contracts,
      network: "Preprod",
      origin: {
        slot: SIM_ORIGIN.point.slot,
        hash: SIM_ORIGIN.point.hash.toString("hex"),
      },
      securityParameter: 6,
    }),
  );
});

/** `listen`'s follower store: the writer. */
const openListenStore = (connectionString: string): FactStore => {
  const store = openPostgresFactStore({
    ...nodeFollowerStoreOptions(index, 0),
    connection: { connectionString },
  });
  stores.push(store);
  return store;
};

/** A ledger that refuses every address scan, and the scans it was sent. */
const scanRefusingTransport = () => {
  const scans: LedgerQuery[] = [];
  const query = async (query: LedgerQuery): Promise<Uint8Array> => {
    if (query.query === "utxo_by_address") scans.push(query);
    throw new Error(`the ledger refuses ${query.query}`);
  };
  const transport = fakeL1NodeTransport({
    query,
    withLedgerState: async (_at, use) => use({ query }),
  });
  return { scans, transport };
};

const openReader = (
  connectionString: string,
  transport: ReturnType<typeof scanRefusingTransport>["transport"],
): NodeL1Access => {
  const access = nodeL1AccessOver(transport, {
    index,
    connectionString,
    endpoint: "/dev/null",
    nodeBehindMaxMs: 60_000,
  });
  accesses.push(access);
  return access;
};

/** One block paying 3 ADA to the reserve validator's address. */
const payReserve = () => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const address = contracts.reserve.spendingScriptAddress;
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
  return { address, event, txHash: encoded.txHashes[0]!.toString("hex") };
};

const applied = async (
  store: FactStore,
  event: ReturnType<typeof payReserve>["event"],
) => {
  const { result } = await applyChainSyncEvent(store, event);
  expect(result.kind).toBe("applied");
};

describe("the node's L1 reader and the node's index", () => {
  it("reads a protocol address from the follower, never by a ledger scan, from before the follower has recorded its set", async () => {
    const connectionString = await databases.create();
    const { scans, transport } = scanRefusingTransport();
    const reader = openReader(connectionString, transport);
    const listen = openListenStore(connectionString);
    expect((await listen.start()).kind).toBe("ready");
    const { address, event, txHash } = payReserve();

    const early = await reader.provider.getUtxos(address).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(early).toBeInstanceOf(L1ProviderTransientError);
    expect(early).toMatchObject({
      source: "follower",
      reason: "not_initialized",
    });
    expect(scans).toEqual([]);

    expect((await listen.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applied(listen, event);
    const utxos = await reader.provider.getUtxos(address);
    expect(utxos.map((utxo) => [utxo.txHash, utxo.assets.lovelace])).toEqual([
      [txHash, 3_000_000n],
    ]);
    expect(scans).toEqual([]);
  });

  it("configures the set listen's follower records: reopening the store through either never resets it", async () => {
    const connectionString = await databases.create();
    const listen = openListenStore(connectionString);
    expect((await listen.start()).kind).toBe("ready");
    expect((await listen.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const { address, event, txHash } = payReserve();
    await applied(listen, event);
    stores.splice(stores.indexOf(listen), 1);
    await listen.close();

    const { scans, transport } = scanRefusingTransport();
    const reader = openReader(connectionString, transport);
    const readerStart = await reader.store.start();
    if (readerStart.kind !== "ready")
      throw new Error(`reader start: ${readerStart.kind}`);
    expect(readerStart.trackedSet.kind).toBe("equal");
    await reader.store.close();

    const reopened = openListenStore(connectionString);
    const listenStart = await reopened.start();
    if (listenStart.kind !== "ready")
      throw new Error(`listen start: ${listenStart.kind}`);
    expect(listenStart.trackedSet.kind).toBe("equal");

    const after = openReader(
      connectionString,
      scanRefusingTransport().transport,
    );
    const utxos = await after.provider.getUtxos(address);
    expect(utxos.map((utxo) => utxo.txHash)).toEqual([txHash]);
    expect(scans).toEqual([]);
  });
});
