import {
  type L1NodeTransport,
  TransportRequestError,
} from "@al-ft/l1-node-transport";

import { decodeLedgerUtxos, type LedgerUtxo } from "../decode/utxo.js";
import type { FactStore } from "../store/fact-store.js";
import type { Rewound } from "../store/rewind.js";
import { withTrackedAddresses } from "../store/tracked-set-record.js";
import type { OutRef, Point } from "../types.js";
import { transportPoint } from "./chain-sync.js";

/** The ledger-state reads the seed needs (an `L1NodeTransport`). */
export type WalletLedger = Pick<L1NodeTransport, "withLedgerState">;

/** The readiness reason while a wallet's seed is owed (plan §5.3 step 4). */
export const WALLET_SEED_PENDING = "wallet_seed_pending";

/** Why a seed did not happen yet. Every reason is transient: retry later. */
export type WalletSeedPendingReason =
  /** The store has no cursor yet, or no seed was tried yet. */
  | "not_initialized"
  /**
   * The node cannot acquire the cursor point: more than k blocks below its
   * tip (the follower is still replaying), or not on its chain (a fork the
   * follower has not rolled back yet).
   */
  | "cursor_not_acquirable"
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
  /** The cursor the owed wallets were read and written at (the rows' `seed_slot`). */
  at: Point;
  generation: number;
  /** Every requested wallet's stored seed point, by lowercase hex address. */
  seeds: ReadonlyMap<string, Point>;
  /** The wallets read from the ledger now; empty when every one was seeded before. */
  read: readonly Buffer[];
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

/**
 * `GetUTxOByAddress` at `at`. The node answers it with a scan of its whole
 * UTxO set (tens of seconds on a public network), so it is sent with no
 * request deadline: it ends with the node's answer or the node connection.
 */
const readWalletUtxos = async (
  ledger: WalletLedger,
  at: Point,
  addresses: readonly Buffer[],
): Promise<LedgerUtxo[]> =>
  await ledger.withLedgerState(transportPoint(at), async (state) => {
    const wanted = new Set(addresses.map((a) => a.toString("hex")));
    const utxos: LedgerUtxo[] = [];
    for (let i = 0; i < addresses.length; i += ADDRESSES_PER_QUERY) {
      const answer = await state.query(
        {
          query: "utxo_by_address",
          addresses: addresses.slice(i, i + ADDRESSES_PER_QUERY),
        },
        { deadline: "none" },
      );
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
 * The LSQ wallet seed (plan §5.3 step 4), once per wallet. A wallet whose
 * seed point the store holds (`l1_wallet_seeds`) is not read again. The
 * others are read with `GetUTxOByAddress` from the ledger state at the
 * store's cursor P inside the store's writer lane (`FactStore.seedWallets`),
 * so no block, rewind or prune lands between the read and the write. The
 * outrefs the store holds no row for become seed rows (`created_slot NULL`,
 * `seed_slot = P`): outputs created before the origin, and outputs created
 * after it while the wallet was not tracked. The wallets are recorded as
 * seeded at P, those with no outputs included; a committed rewind below P
 * deletes both, and the next seed reads the wallet again.
 *
 * LSQ acquires only volatile points, so a read succeeds once the cursor is
 * within k blocks of the node's tip; before that it is `pending` with
 * `cursor_not_acquirable`. It never throws.
 */
export const seedWallets = async (
  store: FactStore,
  ledger: WalletLedger,
  addresses: readonly Buffer[],
): Promise<WalletSeedResult> => {
  const read: { at?: Point } = {};
  let written: Awaited<ReturnType<FactStore["seedWallets"]>>;
  try {
    written = await store.seedWallets(addresses, (at, wallets) => {
      read.at = at;
      return readWalletUtxos(ledger, at, wallets);
    });
  } catch (error) {
    return pending("store_error", message(error));
  }
  if (written === null)
    return pending("not_initialized", "the store has no cursor");
  switch (written.kind) {
    case "error":
      return pending("store_error", written.error.message);
    case "store_locked":
      return pending("store_locked", written.detail);
    case "read_failed": {
      const { error } = written;
      if (error instanceof AnswerInvalid)
        return pending("ledger_answer_invalid", error.message);
      if (
        error instanceof TransportRequestError &&
        NOT_ACQUIRABLE.has(error.code)
      )
        return pending(
          "cursor_not_acquirable",
          `cursor slot ${read.at?.slot}: ${error.message}`,
        );
      return pending("ledger_unavailable", message(error));
    }
    case "seeded":
      return {
        kind: "seeded",
        at: written.cursor.point,
        generation: written.cursor.generation,
        seeds: written.seeds,
        read: written.read,
        inserted: written.inserted,
        skipped: written.skipped,
      };
  }
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
 * wallet starts owed until the first `step` reads the store's seed points:
 * a wallet the store holds one for settles with no ledger read, so a restart
 * reads nothing again. A seed is a fact observed at its seed point: a
 * committed rewind below that point (or a store reset) deletes the wallet's
 * seed rows and seed point, and the wallet is owed again until it is read at
 * a later cursor. Bootstrap and added wallets take the same path.
 */
export type WalletSeeder = Readonly<{
  /** Wallets whose seed is owed; the role reports `wallet_seed_pending` (transient) while any is. */
  owed(): readonly Buffer[];
  ready(): boolean;
  /** `ready`, or why a seed is owed (the last step's pending reason). */
  status(): WalletSeedStatus;
  /**
   * Adds wallets to the store's tracked set, then owes their seed (and only
   * theirs). Tracking comes first, so outputs in blocks after the seed point
   * are written by the follower and the seed covers everything up to it,
   * including outputs of stored txs that got no row while untracked. A
   * wallet already seeded and tracked is not read again.
   */
  addWallets(addresses: readonly Buffer[]): void;
  /**
   * Seeds every owed wallet at the store's cursor, and again while a rewind
   * or an `addWallets` during the seed owes one, until none is owed or a
   * seed is pending. A call while a step runs joins it. It never throws.
   * Call after follower steps until ready.
   */
  step(): Promise<WalletSeedStatus>;
  /** Stops listening for rewinds; a running step ends after its seed. */
  close(): void;
}>;

const NOT_SEEDED_YET = "own wallets not seeded yet";

export const createWalletSeeder = (
  input: Readonly<{
    store: FactStore;
    ledger: WalletLedger;
    /** The role's own wallets; they must already be in the store's tracked set (`FactStoreOptions.wallets`). */
    wallets: readonly Buffer[];
  }>,
): WalletSeeder => {
  const { store, ledger } = input;
  /** Hex address -> the slot it is seeded at, or null while owed. */
  const wallets = new Map<string, number | null>();
  for (const address of input.wallets)
    wallets.set(address.toString("hex"), null);
  const owedHex = (): string[] =>
    [...wallets].flatMap(([hex, slot]) => (slot === null ? [hex] : []));
  const owed = (): Buffer[] => owedHex().map((hex) => Buffer.from(hex, "hex"));
  /** Whether the rewind deleted a seed taken at `slot` (§7.1). */
  const deletes = (rewound: Rewound, slot: number): boolean =>
    rewound.reset === true || rewound.to.slot < slot;
  /** The rewinds heard while a seed runs. */
  let heard: Rewound[] | null = null;
  const unsubscribe = store.onGeneration(({ rewound }) => {
    heard?.push(rewound);
    for (const [hex, slot] of wallets)
      if (slot !== null && deletes(rewound, slot)) wallets.set(hex, null);
  });
  let lastPending: WalletSeedPending = pending(
    "not_initialized",
    NOT_SEEDED_YET,
  );
  let closed = false;
  let running: Promise<WalletSeedStatus> | null = null;
  const status = (): WalletSeedStatus =>
    owedHex().length === 0
      ? { kind: "ready" }
      : { ...lastPending, wallets: owed() };
  /** Settles the requested wallets still owed at their seed points. */
  const settle = (result: WalletSeeded, rewinds: readonly Rewound[]): void => {
    for (const [hex, point] of result.seeds) {
      if (wallets.get(hex) !== null) continue;
      // A rewind committed after the seed (a later generation) may have
      // gone below its point while the wallet was still owed here.
      if (
        rewinds.some(
          (rewound) =>
            rewound.generation > result.generation &&
            deletes(rewound, point.slot),
        )
      )
        continue;
      wallets.set(hex, point.slot);
    }
  };
  const run = async (): Promise<WalletSeedStatus> => {
    while (!closed && owedHex().length > 0) {
      const rewinds: Rewound[] = [];
      heard = rewinds;
      let result: WalletSeedResult;
      try {
        result = await seedWallets(store, ledger, owed());
      } finally {
        heard = null;
      }
      if (result.kind === "pending") {
        lastPending = result;
        break;
      }
      settle(result, rewinds);
      lastPending = pending("not_initialized", NOT_SEEDED_YET);
    }
    return status();
  };
  return {
    owed,
    ready: () => owedHex().length === 0,
    status,
    addWallets: (addresses) => {
      store.setTrackedSet(withTrackedAddresses(store.trackedSet(), addresses));
      for (const address of addresses)
        wallets.set(address.toString("hex"), null);
    },
    step: () =>
      (running ??= run().finally(() => {
        running = null;
      })),
    close: () => {
      closed = true;
      unsubscribe();
    },
  };
};
