import {
  type L1NodeTransport,
  TransportRequestError,
} from "@al-ft/l1-node-transport";

import { decodeLedgerUtxos, type LedgerUtxo } from "../decode/utxo.js";
import type { FactStore } from "../store/fact-store.js";
import { withTrackedAddresses } from "../store/tracked-set-record.js";
import type { OutRef, Point } from "../types.js";
import { transportPoint } from "./chain-sync.js";

/** The ledger-state reads the seed needs (an `L1NodeTransport`). */
export type WalletLedger = Pick<L1NodeTransport, "withLedgerState">;

/** The readiness reason while a wallet's seed is owed (plan §5.3 step 4). */
export const WALLET_SEED_PENDING = "wallet_seed_pending";

/** Why a seed did not happen yet. Every reason is transient: retry later. */
export type WalletSeedPendingReason =
  /** The store has no cursor yet. */
  | "not_initialized"
  /**
   * The node cannot acquire the cursor point: more than k blocks below its
   * tip (the follower is still replaying), or not on its chain (a fork the
   * follower has not rolled back yet).
   */
  | "cursor_not_acquirable"
  /** Blocks or rewinds kept landing between the read and the write. */
  | "cursor_moved"
  /** The transport or the node did not answer. */
  | "ledger_unavailable"
  /** The answer did not decode, or held an output at another address. */
  | "ledger_answer_invalid"
  /** The store refused or failed the write. */
  | "store_error"
  /**
   * Another process holds the writer lease, or this one lost it. Transient:
   * the role starts its store again, as for any `store_locked`.
   */
  | "store_locked";

export type WalletSeeded = Readonly<{
  kind: "seeded";
  /** The cursor the UTxOs were read and written at (the rows' `seed_slot`). */
  at: Point;
  generation: number;
  inserted: readonly OutRef[];
  /** Already stored as a row (created while tracked, or seeded before). */
  skipped: readonly OutRef[];
}>;

export type WalletSeedPending = Readonly<{
  kind: "pending";
  reason: WalletSeedPendingReason;
  detail: string;
}>;

export type WalletSeedResult = WalletSeeded | WalletSeedPending;

/** The sidecar takes at most 4,096 addresses per query. */
const ADDRESSES_PER_QUERY = 4_096;
const DEFAULT_ATTEMPTS = 3;

const pending = (
  reason: WalletSeedPendingReason,
  detail: string,
): WalletSeedPending => ({ kind: "pending", reason, detail });

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const NOT_ACQUIRABLE = new Set([
  "acquire_point_too_old",
  "acquire_point_not_on_chain",
]);

class AnswerInvalid extends Error {}

const readWalletUtxos = async (
  ledger: WalletLedger,
  at: Point,
  addresses: readonly Buffer[],
): Promise<LedgerUtxo[]> =>
  await ledger.withLedgerState(transportPoint(at), async (state) => {
    const wanted = new Set(addresses.map((a) => a.toString("hex")));
    const utxos: LedgerUtxo[] = [];
    for (let i = 0; i < addresses.length; i += ADDRESSES_PER_QUERY) {
      const answer = await state.query({
        query: "utxo_by_address",
        addresses: addresses.slice(i, i + ADDRESSES_PER_QUERY),
      });
      let decoded: LedgerUtxo[];
      try {
        decoded = decodeLedgerUtxos(answer);
      } catch (error) {
        throw new AnswerInvalid(`undecodable UTxO answer: ${message(error)}`);
      }
      for (const utxo of decoded) {
        if (!wanted.has(utxo.output.address.toString("hex")))
          throw new AnswerInvalid(
            `the answer holds ${utxo.outRef.txHash.toString("hex")}#${utxo.outRef.index} at an address that was not asked for`,
          );
        utxos.push(utxo);
      }
    }
    return utxos;
  });

/**
 * The LSQ wallet seed (plan §5.3 step 4). Reads `GetUTxOByAddress` for the
 * given wallet addresses from the ledger state at the store's cursor P and
 * inserts the outrefs the store holds no row for as seed rows
 * (`created_slot NULL`, `seed_slot = P`): outputs created before the origin,
 * and outputs created after it while the wallet was not tracked. The write is refused unless the
 * cursor is still P, so no spend after P can be missed; the read is then
 * retried at the new cursor, up to `attempts` times.
 *
 * LSQ acquires only volatile points, so this succeeds once the cursor is
 * within k blocks of the node's tip; before that it is `pending` with
 * `cursor_not_acquirable`. Seeding is idempotent: a second seed of the same
 * wallet inserts only outrefs that are live at the new cursor and still
 * unknown to the store. It never throws.
 */
export const seedWallets = async (
  store: FactStore,
  ledger: WalletLedger,
  addresses: readonly Buffer[],
  attempts = DEFAULT_ATTEMPTS,
): Promise<WalletSeedResult> => {
  let last: WalletSeedPending = pending("cursor_moved", "no attempt made");
  for (let attempt = 0; attempt < attempts; attempt += 1) {
    let at: Point;
    try {
      const cursor = await store.cursor();
      if (cursor === null)
        return pending("not_initialized", "the store has no cursor");
      at = cursor.point;
    } catch (error) {
      return pending("store_error", `cursor read: ${message(error)}`);
    }
    let utxos: LedgerUtxo[];
    try {
      utxos = await readWalletUtxos(ledger, at, addresses);
    } catch (error) {
      if (error instanceof AnswerInvalid)
        return pending("ledger_answer_invalid", error.message);
      if (
        error instanceof TransportRequestError &&
        NOT_ACQUIRABLE.has(error.code)
      )
        return pending(
          "cursor_not_acquirable",
          `cursor slot ${at.slot}: ${error.message}`,
        );
      return pending("ledger_unavailable", message(error));
    }
    const written = await store.insertSeedOutputs(at, utxos);
    if (written === null)
      return pending("not_initialized", "the store has no cursor");
    if (written.kind === "error")
      return pending("store_error", written.error.message);
    if (written.kind === "store_locked")
      return pending("store_locked", written.detail);
    if (written.kind === "cursor_moved") {
      last = pending(
        "cursor_moved",
        `read at slot ${at.slot}, cursor now at slot ${written.cursor.point.slot}`,
      );
      continue;
    }
    return {
      kind: "seeded",
      at,
      generation: written.cursor.generation,
      inserted: written.inserted,
      skipped: written.skipped,
    };
  }
  return last;
};

export { withTrackedAddresses };

export type WalletSeedStatus =
  | Readonly<{ kind: "ready" }>
  | Readonly<{
      kind: "pending";
      reason: WalletSeedPendingReason;
      detail: string;
      wallets: readonly Buffer[];
    }>;

/**
 * Owes and settles the seed of each own wallet for one role process. Every
 * wallet starts owed (a restart re-seeds, which inserts nothing it already
 * holds). A seed is a fact observed at its seed point: a committed rewind
 * below that point deletes the wallet's seed rows from it, and the wallet is
 * owed again until it is read at a later cursor. Bootstrap and added wallets
 * take the same path.
 */
export type WalletSeeder = Readonly<{
  /** Wallets whose seed is owed; the role reports `wallet_seed_pending` (transient) while any is. */
  owed(): readonly Buffer[];
  ready(): boolean;
  /**
   * Adds wallets to the store's tracked set, then owes their seed (and only
   * theirs). Tracking comes first, so outputs in blocks after the seed point
   * are written by the follower and the seed covers everything up to it,
   * including outputs of stored txs that got no row while untracked.
   */
  addWallets(addresses: readonly Buffer[]): void;
  /** Seeds every owed wallet at the store's cursor. Call after follower steps until ready. */
  step(): Promise<WalletSeedStatus>;
  /** Stops listening for rewinds. */
  close(): void;
}>;

export const createWalletSeeder = (
  input: Readonly<{
    store: FactStore;
    ledger: WalletLedger;
    /** The role's own wallets; they must already be in the store's tracked set (`FactStoreOptions.wallets`). */
    wallets: readonly Buffer[];
    attempts?: number;
  }>,
): WalletSeeder => {
  const { store, ledger } = input;
  /**
   * Hex address -> the slot it was last seeded at (null while owed) and a
   * version that each `addWallets` bumps, so a seed in flight when the wallet
   * was added again does not settle the newer debt.
   */
  const wallets = new Map<string, { slot: number | null; version: number }>();
  const owe = (hex: string): void => {
    const known = wallets.get(hex);
    wallets.set(hex, { slot: null, version: (known?.version ?? 0) + 1 });
  };
  for (const address of input.wallets) owe(address.toString("hex"));
  const owedHex = (): string[] =>
    [...wallets].flatMap(([hex, state]) => (state.slot === null ? [hex] : []));
  // The rewind deleted the seed rows above its target (§7.1): owe again
  // every wallet whose last seed point lies above it.
  const unsubscribe = store.onGeneration(({ rewound }) => {
    for (const state of wallets.values())
      if (state.slot !== null && rewound.to.slot < state.slot)
        state.slot = null;
  });
  return {
    owed: () => owedHex().map((hex) => Buffer.from(hex, "hex")),
    ready: () => owedHex().length === 0,
    addWallets: (addresses) => {
      store.setTrackedSet(withTrackedAddresses(store.trackedSet(), addresses));
      for (const address of addresses) owe(address.toString("hex"));
    },
    step: async () => {
      const owed = owedHex().map((hex) => ({
        hex,
        version: wallets.get(hex)?.version ?? 0,
      }));
      if (owed.length === 0) return { kind: "ready" };
      const addresses = owed.map(({ hex }) => Buffer.from(hex, "hex"));
      const result = await seedWallets(
        store,
        ledger,
        addresses,
        input.attempts,
      );
      if (result.kind === "pending") return { ...result, wallets: addresses };
      // A rewind committed after the write may have gone below `at`, and its
      // listener ran while these wallets were still owed: settle nothing.
      const generation = (await store.cursor())?.generation;
      if (generation === result.generation)
        for (const { hex, version } of owed) {
          const state = wallets.get(hex);
          if (state !== undefined && state.version === version)
            state.slot = result.at.slot;
        }
      const left = owedHex();
      return left.length === 0
        ? { kind: "ready" }
        : {
            kind: "pending",
            reason: "cursor_moved",
            detail:
              "a rewind or a wallet addition landed during the seed; the seed is owed again",
            wallets: left.map((hex) => Buffer.from(hex, "hex")),
          };
    },
    close: unsubscribe,
  };
};
