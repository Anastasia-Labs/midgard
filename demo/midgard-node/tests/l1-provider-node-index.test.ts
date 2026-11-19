/**
 * The node's L1 reader is configured with the node's index from its open
 * (`nodeIndexSpec`): a protocol address is answered from the follower's
 * facts, never by a ledger address scan, even before the follower has
 * recorded its tracked set; and the reader and `listen`'s follower configure
 * the same set, so reopening the store through either never resets it.
 */
import { encodeCbor, type LedgerQuery } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import {
  applyChainSyncEvent,
  type FactStore,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  type L1FollowerProvider,
  L1ProviderTransientError,
} from "@al-ft/midgard-l1-follower/provider";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateSeedPhrase,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
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
import { awaitSubmittedTransactionConfirmation } from "../src/transactions/utils.await-submitted-transaction-confirmation.js";
import { emulatorFollowerPlan } from "./helpers/follower-emulator.host.js";
import { freezablePostgresProxy } from "./helpers/freezable-postgres-proxy.js";
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

describe("the node's L1 reader and a rollback", () => {
  it("says a tip it answered is off its chain once a rollback onto a fork at a higher slot dropped it, and its ancestors still on it", async () => {
    const connectionString = await databases.create();
    const reader = openReader(
      connectionString,
      scanRefusingTransport().transport,
    );
    const listen = openListenStore(connectionString);
    expect((await listen.start()).kind).toBe("ready");
    expect((await listen.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    await applied(listen, chain.forward([]).event);
    const ancestor = await reader.viewPoint();
    await applied(listen, chain.forward([]).event);
    const dropped = await reader.viewPoint();
    const { result } = await applyChainSyncEvent(listen, chain.backward(1));
    expect(result.kind).toBe("rewound");
    await applied(listen, chain.forward([]).event);
    await applied(listen, chain.forward([]).event);
    const forkTip = await reader.viewPoint();
    expect(forkTip.slot).toBeGreaterThan(dropped.slot);

    expect(await reader.pointOnChain?.(dropped)).toBe(false);
    expect(await reader.pointOnChain?.(ancestor)).toBe(true);
    expect(await reader.pointOnChain?.(forkTip)).toBe(true);
  });
});

describe("the node's L1 reader and an abort", () => {
  it("hands a read's abort signal to the follower store, so an aborted read ends with its reason", async () => {
    const connectionString = await databases.create();
    const reader = openReader(
      connectionString,
      scanRefusingTransport().transport,
    );
    const listen = openListenStore(connectionString);
    expect((await listen.start()).kind).toBe("ready");
    expect((await listen.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const { event, txHash } = payReserve();
    await applied(listen, event);
    const tip = await reader.viewPoint();
    const outRef = { txHash, outputIndex: 0 };
    expect(await reader.pointOnChain?.(tip)).toBe(true);
    expect(await reader.utxosByOutRef?.([outRef])).toHaveLength(1);

    const signal = AbortSignal.abort(new Error("the read's deadline passed"));
    await expect(reader.pointOnChain?.(tip, { signal })).rejects.toThrow(
      "the read's deadline passed",
    );
    await expect(reader.utxosByOutRef?.([outRef], { signal })).rejects.toThrow(
      "the read's deadline passed",
    );
    await expect(
      (reader.provider as L1FollowerProvider).getTransactionStatus(txHash, {
        signal,
      }),
    ).rejects.toThrow("the read's deadline passed");
  });

  // The view read's first read is the node's tip, which the transport
  // bounds; its store read is the one the signal ends.
  it("ends a synchronized view read at its abort though the store never answers it", async () => {
    const connectionString = await databases.create();
    const listen = openListenStore(connectionString);
    expect((await listen.start()).kind).toBe("ready");
    expect((await listen.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applied(listen, payReserve().event);
    const proxy = await freezablePostgresProxy(connectionString);
    const reader = nodeL1AccessOver(
      fakeL1NodeTransport({
        query: async (query) => {
          if (query.query !== "chain_point")
            throw new Error(`the ledger refuses ${query.query}`);
          return encodeCbor([0n, Buffer.alloc(32)]);
        },
      }),
      {
        index,
        connectionString: proxy.url,
        endpoint: "/dev/null",
        nodeBehindMaxMs: 60_000,
      },
    );
    try {
      await reader.viewPoint();
      // The reader's one open connection goes silent.
      proxy.freeze();
      const controller = new AbortController();
      const read = reader.synchronizedViewPoint({ signal: controller.signal });
      read.catch(() => undefined);
      await new Promise((resolve) => setTimeout(resolve, 200));
      const reason = new Error("the view read's deadline passed");
      controller.abort(reason);
      await expect(within("the view read", 5_000, read)).rejects.toMatchObject({
        cause: reason,
      });
    } finally {
      await within("closing the reader", 5_000, reader.close()).catch(
        () => undefined,
      );
      await proxy.close();
    }
  });
});

/** `promise`'s outcome, or a failure naming `what` after `ms`. */
const within = <T>(what: string, ms: number, promise: Promise<T>) =>
  new Promise<T>((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error(`${what} did not settle within ${String(ms)} ms`)),
      ms,
    );
    promise.then(
      (value) => {
        clearTimeout(timer);
        resolve(value);
      },
      (error: unknown) => {
        clearTimeout(timer);
        reject(error);
      },
    );
  });

/** A signed transaction with no validity bounds: its wait is unbounded. */
const unboundedTxCbor = (): string => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  return CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  ).to_cbor_hex();
};

describe("the node's L1 reader and a store connection that goes silent", () => {
  // The clock's tip read passes no deadline. Two of them hold both of the
  // reader's connections while the server never answers; they end at the
  // reader's request bound with their connections, so the confirmation
  // wait's status read opens a fresh one and decides.
  it("decides a confirmation wait once the reads stuck without a deadline reach the reader's request bound", async () => {
    const connectionString = await databases.create();
    const listen = openListenStore(connectionString);
    expect((await listen.start()).kind).toBe("ready");
    expect((await listen.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const { event, txHash } = payReserve();
    await applied(listen, event);
    const proxy = await freezablePostgresProxy(connectionString);
    const reader = nodeL1AccessOver(scanRefusingTransport().transport, {
      index,
      connectionString: proxy.url,
      endpoint: "/dev/null",
      nodeBehindMaxMs: 60_000,
      requestTimeoutMs: 1_000,
    });
    try {
      // Both of the reader's connections are open, then go silent.
      await Promise.all([reader.tipSlot(), reader.tipSlot()]);
      expect(proxy.accepted()).toBe(2);
      proxy.freeze();
      const stuck = [reader.tipSlot(), reader.tipSlot()];
      for (const read of stuck) read.catch(() => undefined);

      const wait = awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider: reader.provider }),
          transactionStatus: (
            hash: string,
            options?: { signal?: AbortSignal },
          ) =>
            (reader.provider as L1FollowerProvider).getTransactionStatus(
              hash,
              options,
            ),
          wallet: () => ({}),
        } as never,
        { txHash, signedTxCbor: unboundedTxCbor() },
        {
          confirmationTimeoutMs: 300,
          confirmationRetries: 19,
          confirmationPollIntervalMs: 50,
        },
      );
      expect(
        await within("the confirmation wait", 15_000, Effect.runPromise(wait)),
      ).toBe(txHash);
      for (const read of stuck)
        await expect(
          within("a stuck read", 5_000, read),
        ).rejects.toBeInstanceOf(L1ProviderTransientError);
    } finally {
      await within("closing the reader", 5_000, reader.close()).catch(
        () => undefined,
      );
      await proxy.close();
    }
  });
});
