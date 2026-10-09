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
      entry = { providerCalls: 0, submitted: [] };
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
      const { providerCalls, submitted } = servedOf(emulator);
      const history = stateOf(emulator).transactionHistory;
      const confirmed = submitted.filter(
        (hash) => history[hash]?.status === "confirmed",
      );
      const stored = await withFollowerHost(emulator, async (store) => {
        const held: string[] = [];
        for (const hash of confirmed)
          if ((await store.txByHash(Buffer.from(hash, "hex"))) !== null)
            held.push(hash);
        return held;
      });
      return {
        providerCalls,
        submitted: [...submitted],
        confirmed,
        stored,
      };
    };
  return {
    hosts: () =>
      [...served.keys()].map((emulator) => ({ emulator, use: use(emulator) })),
    restore: async () => {
      for (const [name, method] of saved) prototype[name] = method;
      await releaseFollowerHost();
    },
  };
};
