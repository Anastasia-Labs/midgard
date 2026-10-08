/**
 * The node's L1 provider on the lucid-evolution emulator: every provider call
 * Lucid makes on an `Emulator` goes through a `NodeL1Provider` (the node's
 * `L1FollowerProvider`) over an in-memory follower store that follows the
 * emulator's chain, and a node transport double over the emulator's ledger.
 *
 * - The emulator stays the ledger and the clock: Lucid still sees an
 *   emulator (its slot and time), and the emulator still validates every
 *   submitted transaction.
 * - Each emulator block that confirms submitted transactions is applied to
 *   the store as a block of those transactions' exact bytes, so reads, datum
 *   lookups and transaction status come from the follower's facts.
 * - Origin: the follower starts at the emulator's ledger at its first
 *   provider call (genesis, or a restored snapshot). Its key-credential
 *   addresses are the own wallets; they and the script credentials and
 *   policies that ledger holds are tracked and seeded at the origin. Every
 *   script payment credential an output pays to and every policy a
 *   transaction mints under is tracked from the block that first shows it.
 *   Any other address is read from the ledger over the transport, as the
 *   node reads an address outside its tracked set.
 * - Rollback: when an input a stored block consumed is unspent on the
 *   emulator's ledger again (a test restored an earlier ledger), the store
 *   rewinds to that block's parent. A transaction the emulator dropped from
 *   its mempool is no longer confirmed by a later block.
 * - Mempool: like the ledger, the follower offers an output a pending
 *   transaction spends until the spend confirms; the emulator's own reads
 *   hide it. A node build funds from its wallet view, which holds the
 *   inputs of the node's live intents (`intent-journal.wallet-view.ts`).
 * - The transport double answers `protocol_params`, the UTxO queries and the
 *   reward-account queries from the emulator, submits to the emulator, and
 *   answers mempool presence from its pending transactions.
 *
 * `installFollowerEmulator` routes every emulator's provider calls through
 * its own follower until `restore`. No emulator internals other than the
 * ledger, the transaction history and the reward-account chain are read.
 */
import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  decodeBlock,
  decodeLedgerUtxos,
  type FactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import { Emulator, getAddressDetails } from "@lucid-evolution/lucid";

import { NodeL1Provider } from "../../src/services/l1-provider.js";
import {
  emulatorNodeTransport,
  encodeFollowerBlock,
  stateOf,
  trackedItemsOf,
  transactionHash,
  utxoAnswer,
} from "./follower-emulator.ledger.js";

const ORIGIN_HASH = Buffer.alloc(32, 0x4f);
/** Bound on the follower provider's `awaitTx` for a tx the emulator never saw. */
const AWAIT_TX_TIMEOUT_MS = 2_000;
const AWAIT_TX_CHECK_MS = 25;

/** What one emulator's follower served: the red-check evidence. */
export type FollowerEmulatorUse = Readonly<{
  providerCalls: number;
  submitted: readonly string[];
  /** Submitted transactions the emulator confirmed. */
  confirmed: readonly string[];
  /** Confirmed transactions the follower store holds. */
  stored: readonly string[];
}>;

export type FollowerEmulatorHost = {
  readonly provider: NodeL1Provider;
  readonly store: FactStore;
  /** Counts one provider call Lucid made on the emulator. */
  called(): void;
  /** Applies the emulator's newly confirmed submissions as one block. */
  sync(): Promise<void>;
  use(): Promise<FollowerEmulatorUse>;
  close(): Promise<void>;
};

const openHost = async (
  emulator: Emulator,
  original: OriginalMethods,
): Promise<FollowerEmulatorHost> => {
  const state = stateOf(emulator);
  // The ledger at the follower's origin: genesis accounts, or a restored
  // deployment snapshot.
  const genesis = Object.values(state.ledger).flatMap(({ utxo, spent }) =>
    spent ? [] : [utxo],
  );
  const wallets = [
    ...new Map(
      genesis
        .filter(
          ({ address }) =>
            getAddressDetails(address).paymentCredential?.type === "Key",
        )
        .map(({ address }) => {
          const hex = getAddressDetails(address).address.hex;
          return [hex, Buffer.from(hex, "hex")] as const;
        }),
    ).values(),
  ];
  const genesisScripts = genesis.flatMap(({ address }) => {
    const credential = getAddressDetails(address).paymentCredential;
    return credential?.type === "Script" ? [credential.hash] : [];
  });
  const store = openSqliteFactStore({
    securityParameter: 2_160,
    trackedSet: {
      addresses: new Set(),
      paymentCredentials: new Set(genesisScripts),
      policies: new Set(
        genesis.flatMap(({ assets }) =>
          Object.keys(assets).flatMap((unit) =>
            unit === "lovelace" ? [] : [unit.slice(0, 56)],
          ),
        ),
      ),
    },
    wallets,
    path: ":memory:",
  });
  const started = await store.start();
  if (started.kind !== "ready")
    throw new Error(`the follower store did not start: ${started.kind}`);
  const origin = { slot: state.slot, hash: ORIGIN_HASH };
  const initialized = await store.initialize({ point: origin, height: 0 });
  if (!("cursor" in initialized) || initialized.kind !== "initialized")
    throw new Error(
      `the follower store did not initialize: ${initialized.kind}`,
    );

  const pending: string[] = [];
  const submitted: string[] = [];
  const transport = emulatorNodeTransport({
    emulator,
    store,
    origin,
    submitTx: original.submitTx,
    accepted: (cbor) => {
      const hash = transactionHash(cbor);
      if (!pending.includes(cbor)) pending.push(cbor);
      if (!submitted.includes(hash)) submitted.push(hash);
    },
  });
  const provider = new NodeL1Provider({
    store,
    transport: transport as unknown as L1NodeTransport,
    wallets,
    awaitTxTimeoutMs: AWAIT_TX_TIMEOUT_MS,
  });
  await provider.refreshTrackedSet();

  // The genesis outputs the follower tracks predate its origin: seed them.
  const seeds = decodeLedgerUtxos(utxoAnswer(genesis)).filter(
    ({ output }) =>
      store.trackedSet().addresses.has(output.address.toString("hex")) ||
      (output.paymentCredential !== null &&
        store
          .trackedSet()
          .paymentCredentials.has(
            output.paymentCredential.hash.toString("hex"),
          )),
  );
  if (seeds.length > 0) {
    const seeded = await store.insertSeedOutputs(origin, seeds);
    if (seeded === null || seeded.kind !== "seeded")
      throw new Error(
        `the follower store did not seed: ${seeded?.kind ?? "uninitialized"}`,
      );
  }

  /** The blocks applied since the origin: parents and the outrefs they consumed. */
  const chain: {
    parent: Readonly<{ slot: number; hash: Buffer }>;
    consumed: readonly string[];
  }[] = [];
  /**
   * A rollback on the emulator (its ledger again holds an output a block the
   * store applied consumed) rewinds the store to the block before it.
   */
  const followRollback = async (): Promise<void> => {
    const rolledBack = chain.findIndex(({ consumed }) =>
      consumed.some((key) => state.ledger[key] !== undefined),
    );
    if (rolledBack < 0) return;
    const rewound = await store.rewind(chain[rolledBack]!.parent);
    if (rewound.kind !== "rewound")
      throw new Error(`the follower store did not rewind: ${rewound.kind}`);
    chain.splice(rolledBack);
  };
  let applying = Promise.resolve();
  const applyConfirmed = async (): Promise<void> => {
    await followRollback();
    // A transaction the emulator dropped from its mempool is gone.
    pending.splice(
      0,
      pending.length,
      ...pending.filter(
        (cbor) => state.transactionHistory[transactionHash(cbor)] !== undefined,
      ),
    );
    const confirmed = pending.filter(
      (cbor) =>
        state.transactionHistory[transactionHash(cbor)]?.status === "confirmed",
    );
    if (confirmed.length === 0) return;
    const cursor = await store.cursor();
    if (cursor === null) throw new Error("the follower store has no cursor");
    const raw = encodeFollowerBlock(
      confirmed,
      { hash: cursor.point.hash, height: cursor.height },
      Math.max(state.slot, cursor.point.slot + 1),
    );
    const block = decodeBlock(raw);
    const expected = confirmed.map(transactionHash);
    if (
      block.txs.map((tx) => tx.hash.toString("hex")).join() !== expected.join()
    )
      throw new Error("the follower block changed a transaction identity");
    const items = trackedItemsOf(block);
    const tracked = store.trackedSet();
    store.setTrackedSet({
      addresses: tracked.addresses,
      paymentCredentials: new Set([
        ...tracked.paymentCredentials,
        ...items.credentials,
      ]),
      policies: new Set([...tracked.policies, ...items.policies]),
    });
    const applied = await store.applyBlock(block);
    if (applied.kind !== "applied")
      throw new Error(
        `the follower store did not apply a block: ${applied.kind}${"detail" in applied ? `: ${String(applied.detail)}` : ""}`,
      );
    chain.push({
      parent: cursor.point,
      consumed: block.txs.flatMap((tx) =>
        (tx.isValid ? tx.inputs : tx.collaterals).map(
          ({ txHash, index }) => `${txHash.toString("hex")}${index.toString()}`,
        ),
      ),
    });
    pending.splice(
      0,
      pending.length,
      ...pending.filter((cbor) => !confirmed.includes(cbor)),
    );
  };
  let providerCalls = 0;
  return {
    provider,
    store,
    called: () => {
      providerCalls += 1;
    },
    sync: () => {
      applying = applying.then(applyConfirmed);
      return applying;
    },
    use: async () => {
      applying = applying.then(applyConfirmed);
      await applying;
      const confirmed = submitted.filter(
        (hash) => state.transactionHistory[hash]?.status === "confirmed",
      );
      const stored: string[] = [];
      for (const hash of confirmed)
        if ((await store.txByHash(Buffer.from(hash, "hex"))) !== null)
          stored.push(hash);
      return {
        providerCalls,
        submitted: [...submitted],
        confirmed,
        stored,
      };
    },
    close: async () => {
      await applying.catch(() => undefined);
      await store.close();
    },
  };
};

type OriginalMethods = {
  submitTx: (this: Emulator, tx: string) => Promise<string>;
  awaitBlock: (this: Emulator, height?: number) => void;
  awaitSlot: (this: Emulator, length?: number) => void;
};

/** The emulator's provider methods the follower takes over. */
const PROVIDER_METHODS = [
  "getProtocolParameters",
  "getUtxos",
  "getUtxosWithUnit",
  "getUtxoByUnit",
  "getUtxosByOutRef",
  "getDatum",
  "getRewardAccount",
  "getDelegation",
  "getTransactionStatus",
  "submitTx",
  "evaluateTx",
] as const;

export type FollowerEmulatorInstallation = Readonly<{
  /** Every emulator the follower hosted, with what it served. */
  hosts: () => readonly FollowerEmulatorHost[];
  restore: () => Promise<void>;
}>;

/**
 * Routes every `Emulator`'s provider calls through its own follower
 * provider (created on the emulator's first provider call) until `restore`.
 */
export const installFollowerEmulator = (): FollowerEmulatorInstallation => {
  const prototype = Emulator.prototype as unknown as Record<string, unknown>;
  const saved = new Map<string, unknown>(
    [...PROVIDER_METHODS, "awaitTx", "awaitBlock", "awaitSlot"].map(
      (name) => [name, prototype[name]] as const,
    ),
  );
  const original: OriginalMethods = {
    submitTx: saved.get("submitTx") as OriginalMethods["submitTx"],
    awaitBlock: saved.get("awaitBlock") as OriginalMethods["awaitBlock"],
    awaitSlot: saved.get("awaitSlot") as OriginalMethods["awaitSlot"],
  };
  const hosts = new Map<Emulator, Promise<FollowerEmulatorHost>>();
  const hostOf = (emulator: Emulator): Promise<FollowerEmulatorHost> => {
    let host = hosts.get(emulator);
    if (host === undefined) {
      host = openHost(emulator, original);
      hosts.set(emulator, host);
    }
    return host;
  };
  const opened: FollowerEmulatorHost[] = [];
  for (const name of PROVIDER_METHODS)
    prototype[name] = async function (this: Emulator, ...args: unknown[]) {
      const host = await hostOf(this);
      if (!opened.includes(host)) opened.push(host);
      host.called();
      await host.sync();
      const provider = host.provider as unknown as Record<
        string,
        (...input: unknown[]) => Promise<unknown>
      >;
      return await provider[name]!.call(provider, ...args);
    };
  prototype.awaitTx = async function (this: Emulator, txHash: string) {
    const host = await hostOf(this);
    if (!opened.includes(host)) opened.push(host);
    host.called();
    if (stateOf(this).transactionHistory[txHash]?.status === "pending")
      this.awaitBlock();
    await host.sync();
    return await host.provider.awaitTx(txHash, AWAIT_TX_CHECK_MS);
  };
  prototype.awaitBlock = function (this: Emulator, height?: number) {
    original.awaitBlock.call(this, height);
    const host = hosts.get(this);
    if (host !== undefined) void host.then((open) => open.sync());
  };
  prototype.awaitSlot = function (this: Emulator, length?: number) {
    original.awaitSlot.call(this, length);
    const host = hosts.get(this);
    if (host !== undefined) void host.then((open) => open.sync());
  };
  return {
    hosts: () => [...opened],
    restore: async () => {
      for (const [name, method] of saved) prototype[name] = method;
      for (const host of hosts.values())
        await host.then(
          (open) => open.close(),
          () => undefined,
        );
    },
  };
};
