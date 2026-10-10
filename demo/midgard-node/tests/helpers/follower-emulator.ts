/**
 * The node's L1 provider on the lucid-evolution emulator: every provider call
 * Lucid makes on an `Emulator` goes through a `NodeL1Provider` (the node's
 * `L1FollowerProvider`) over the process's one follower host
 * (`follower-emulator.host.ts`: the node's Postgres follower store following
 * the emulator's chain), and a node transport double over the emulator's
 * ledger.
 *
 * - The emulator stays the ledger and the clock: Lucid still sees an
 *   emulator (its slot and time), and the emulator still validates every
 *   submitted transaction.
 * - Before each provider call the host brings the store to the calling
 *   emulator's chain (`follower-emulator.chain.ts`: one block per change of
 *   its block height, of the exact bytes of the transactions it confirmed),
 *   so reads, datum lookups and transaction status come from the follower's
 *   facts. An address outside the tracked set is read from the ledger over
 *   the transport, as the node reads one.
 * - Mempool: like the ledger, the follower offers an output a pending
 *   transaction spends until the spend confirms; the emulator's own reads
 *   hide it. A node build funds from its wallet view, which holds the
 *   inputs of the node's live intents (`intent-journal.wallet-view.ts`).
 * - The transport double answers `protocol_params`, the UTxO queries and the
 *   reward-account queries from the emulator, submits to the emulator, and
 *   answers mempool presence from its pending transactions.
 * - The red-check evidence is gathered as the suite runs: after each sync,
 *   and each time the host moves the store off an emulator's chain (caught
 *   up to that chain first), the emulator's confirmed transactions not yet
 *   found are looked up in the store. `use` syncs an emulator again only
 *   when one of its confirmed transactions was never found that way.
 *
 * `installFollowerEmulator` routes every emulator's provider calls through
 * the host until `restore`. No emulator internals other than the ledger, the
 * transaction history and the reward-account chain are read.
 */
import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import { type FactStore, mergeTrackedSets } from "@al-ft/midgard-l1-follower";
import { Emulator } from "@lucid-evolution/lucid";

import { NodeL1Provider } from "../../src/services/l1-provider.js";
import { followedChainOf, followedPoint } from "./follower-emulator.chain.js";
import {
  followerHostWallets,
  onFollowerHostCheck,
  releaseFollowerHost,
  syncFollowerHost,
  withFollowerHost,
} from "./follower-emulator.host.js";
import {
  emulatorNodeTransport,
  stateOf,
  transactionHash,
} from "./follower-emulator.ledger.js";

/** Bound on the follower provider's `awaitTx` for a tx the emulator never saw. */
const AWAIT_TX_TIMEOUT_MS = 2_000;
const AWAIT_TX_CHECK_MS = 25;

/** What the follower served one emulator: the red-check evidence. */
export type FollowerEmulatorUse = Readonly<{
  providerCalls: number;
  submitted: readonly string[];
  /** Submitted transactions the emulator confirmed. */
  confirmed: readonly string[];
  /** Confirmed transactions the follower store holds. */
  stored: readonly string[];
  /** Whether `use` synced the store to the emulator's chain to check. */
  synced: boolean;
}>;

export type FollowerEmulatorHost = Readonly<{
  emulator: Emulator;
  /** Brings the store to the emulator's chain and reports what it served. */
  use: () => Promise<FollowerEmulatorUse>;
}>;

type OriginalMethods = {
  submitTx: (this: Emulator, tx: string) => Promise<string>;
};

type Served = {
  providerCalls: number;
  readonly submitted: string[];
  /** Confirmed transactions found in the store at this emulator's chain. */
  readonly stored: Set<string>;
  provider?: Readonly<{ store: FactStore; provider: NodeL1Provider }>;
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
  /** Every emulator the follower served. */
  hosts: () => readonly FollowerEmulatorHost[];
  restore: () => Promise<void>;
}>;

/**
 * Routes every `Emulator`'s provider calls through a follower provider over
 * the host's store, synchronized to that emulator's chain, until `restore`.
 */
export const installFollowerEmulator = (): FollowerEmulatorInstallation => {
  const prototype = Emulator.prototype as unknown as Record<string, unknown>;
  const saved = new Map<string, unknown>(
    [...PROVIDER_METHODS, "awaitTx"].map(
      (name) => [name, prototype[name]] as const,
    ),
  );
  const original: OriginalMethods = {
    submitTx: saved.get("submitTx") as OriginalMethods["submitTx"],
  };
  const served = new Map<Emulator, Served>();
  const servedOf = (emulator: Emulator): Served => {
    let entry = served.get(emulator);
    if (entry === undefined) {
      entry = { providerCalls: 0, submitted: [], stored: new Set() };
      served.set(emulator, entry);
    }
    return entry;
  };
  /** The provider over the store at `emulator`'s chain. */
  const providerOf = async (emulator: Emulator): Promise<NodeL1Provider> => {
    const entry = servedOf(emulator);
    entry.providerCalls += 1;
    const store = await syncFollowerHost(emulator);
    if (entry.provider?.store === store) return entry.provider.provider;
    const transport = emulatorNodeTransport({
      emulator,
      store,
      origin: followedPoint(followedChainOf(emulator), -1),
      submitTx: original.submitTx,
      accepted: (cbor) => {
        const hash = transactionHash(cbor);
        if (!entry.submitted.includes(hash)) entry.submitted.push(hash);
      },
    });
    const provider = new NodeL1Provider({
      store,
      transport: transport as unknown as L1NodeTransport,
      wallets: followerHostWallets(),
      awaitTxTimeoutMs: AWAIT_TX_TIMEOUT_MS,
    });
    // The provider adopts the recorded set and the wallets; the items the
    // host tracks from the blocks that first showed them stay tracked.
    const widened = store.trackedSet();
    await provider.refreshTrackedSet();
    store.setTrackedSet(mergeTrackedSets(store.trackedSet(), widened));
    entry.provider = { store, provider };
    return provider;
  };
  /** `emulator`'s confirmed transactions not yet found in the store. */
  const unfound = (emulator: Emulator): readonly string[] => {
    const entry = served.get(emulator);
    if (entry === undefined) return [];
    const history = stateOf(emulator).transactionHistory;
    return entry.submitted.filter(
      (hash) =>
        !entry.stored.has(hash) && history[hash]?.status === "confirmed",
    );
  };
  /** Adds the unfound transactions `store` holds to `emulator`'s found. */
  const findStored = async (emulator: Emulator, store: FactStore) => {
    const entry = servedOf(emulator);
    for (const hash of unfound(emulator))
      if ((await store.txByHash(Buffer.from(hash, "hex"))) !== null)
        entry.stored.add(hash);
  };
  const unregisterCheck = onFollowerHostCheck(async (emulator, chain) => {
    if (unfound(emulator).length > 0) await findStored(emulator, await chain());
  });
  for (const name of PROVIDER_METHODS)
    prototype[name] = async function (this: Emulator, ...args: unknown[]) {
      const provider = (await providerOf(this)) as unknown as Record<
        string,
        (...input: unknown[]) => Promise<unknown>
      >;
      return await provider[name]!.call(provider, ...args);
    };
  prototype.awaitTx = async function (this: Emulator, txHash: string) {
    if (stateOf(this).transactionHistory[txHash]?.status === "pending")
      this.awaitBlock();
    const provider = await providerOf(this);
    return await provider.awaitTx(txHash, AWAIT_TX_CHECK_MS);
  };
  const use =
    (emulator: Emulator) => async (): Promise<FollowerEmulatorUse> => {
      const entry = servedOf(emulator);
      const history = stateOf(emulator).transactionHistory;
      const confirmed = entry.submitted.filter(
        (hash) => history[hash]?.status === "confirmed",
      );
      const synced = unfound(emulator).length > 0;
      if (synced)
        await withFollowerHost(emulator, (store) =>
          findStored(emulator, store),
        );
      return {
        providerCalls: entry.providerCalls,
        submitted: [...entry.submitted],
        confirmed,
        stored: confirmed.filter((hash) => entry.stored.has(hash)),
        synced,
      };
    };
  return {
    hosts: () =>
      [...served.keys()].map((emulator) => ({ emulator, use: use(emulator) })),
    restore: async () => {
      unregisterCheck();
      for (const [name, method] of saved) prototype[name] = method;
      await releaseFollowerHost();
    },
  };
};
