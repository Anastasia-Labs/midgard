import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import type {
  Address,
  Credential,
  Datum,
  DatumHash,
  OutRef as LucidOutRef,
  TransactionStatus,
  TxHash,
  Unit,
  UTxO,
} from "@lucid-evolution/lucid";

import { addressCredentials } from "../decode/output.js";
import type { SqlTx } from "../sql/backend.js";
import { datumByHashIn } from "../store/datum.js";
import type { FactStore } from "../store/fact-store.js";
import { isTrackedOutput } from "../store/qualify.js";
import * as reads from "../store/reads.js";
import type { OutputSummary, OutRef, StoredOutput } from "../types.js";
import {
  L1AwaitTxTimeoutError,
  L1CarriagePendingError,
  L1ProviderError,
  L1ProviderScopeError,
  L1ProviderTransientError,
  L1UnitLookupError,
} from "./errors.js";
import {
  DEFAULT_CHECK_INTERVAL_MS,
  LedgerProvider,
  lucidUtxo,
  outRefKey,
  toOutRef,
} from "./ledger-provider.js";
import { holdsUnit, splitUnit, utxoSubject } from "./utxo.js";

export type L1FollowerProviderOptions = Readonly<{
  /** The role's fact store: tracked outputs, qualifying txs, blocks. */
  store: FactStore;
  /** The role's node transport (LSQ, LocalTxSubmission, LocalTxMonitor). */
  transport: L1NodeTransport;
  /** How long `awaitTx` waits for the tx to reach the facts (default 160 s). */
  awaitTxTimeoutMs?: number;
}>;

/**
 * A Lucid provider over the L1 follower (plan §4.4): the node-ledger
 * provider ({@link LedgerProvider}) with the role's fact store over it.
 * UTxO, datum and transaction reads in the tracked scope come from the
 * store; protocol parameters, slot configuration, untracked UTxOs and reward
 * accounts from local state query at the node's tip; submission through
 * LocalTxSubmission and mempool presence through LocalTxMonitor. It never
 * reads Kupo or Ogmios and never evaluates scripts remotely. Every failure
 * is a typed error; none ends the process.
 *
 * Scope: an address the role tracks (by address or payment credential) is
 * answered from the facts alone, at the follower's tip, seed rows included.
 * An untracked address is answered from the ledger at the node's tip. A
 * payment credential the role does not track is refused: the ledger query
 * cannot enumerate by credential.
 */
export class L1FollowerProvider extends LedgerProvider {
  readonly #store: FactStore;

  constructor(options: L1FollowerProviderOptions) {
    super({
      transport: options.transport,
      ...(options.awaitTxTimeoutMs === undefined
        ? {}
        : { awaitTxTimeoutMs: options.awaitTxTimeoutMs }),
      // Untracked reads keep the node's tip: the store is the role's view.
      pinPointMs: 0,
    });
    this.#store = options.store;
  }

  /** The one live tracked output holding `unit`; only tracked outputs are searched. */
  override async getUtxoByUnit(unit: Unit): Promise<UTxO> {
    const { policyId, assetName } = splitUnit(unit);
    const rows = (
      await this.#factsUtxos({ by: "unit", policyId, assetName })
    ).filter((row) => holdsUnit(row.output, unit));
    if (rows.length !== 1) throw new L1UnitLookupError(unit, rows.length);
    return lucidUtxo(rows[0]!);
  }

  /**
   * The live outputs among `outRefs`, in request order. An outref the facts
   * hold is answered from the facts (a spent row is omitted); any other one
   * from the ledger at the node's tip, unless the ledger's output is in the
   * tracked scope, whose state is the follower's.
   */
  override async getUtxosByOutRef(
    outRefs: Array<LucidOutRef>,
  ): Promise<UTxO[]> {
    const wanted = [
      ...new Map(
        outRefs.map((outRef) => {
          const parsed = toOutRef(outRef);
          return [outRefKey(parsed), parsed] as const;
        }),
      ).values(),
    ];
    const stored = await this.#read(async (tx) => {
      const rows: (StoredOutput | null)[] = [];
      // One connection per transaction: the reads run one after another.
      for (const outRef of wanted)
        rows.push(await reads.outputIn(tx, this.#store.dialect, outRef));
      return rows;
    });
    const found = new Map<string, UTxO>();
    const missing: OutRef[] = [];
    stored.forEach((row, index) => {
      if (row === null) missing.push(wanted[index]!);
      else if (row.spent === null)
        found.set(outRefKey(row.outRef), lucidUtxo(row));
    });
    const tracked = this.#store.trackedSet();
    for (const entry of await this.ledgerUtxosByTxIn(missing, "tip"))
      if (!isTrackedOutput(entry.output, tracked))
        found.set(outRefKey(entry.outRef), lucidUtxo(entry));
    return wanted.flatMap((outRef) => found.get(outRefKey(outRef)) ?? []);
  }

  /** A datum preimage from the stored witness sets; never an empty answer. */
  override async getDatum(datumHash: DatumHash): Promise<Datum> {
    const datum = await this.#read((tx) =>
      datumByHashIn(tx, Buffer.from(datumHash, "hex")),
    );
    if (datum === null) throw new L1CarriagePendingError("datum", datumHash);
    return datum.toString("hex");
  }

  /**
   * Where a transaction stands: confirmed or failed once the follower
   * applied its block (qualifying txs only, plan §5.2), pending while the
   * node's mempool holds it, otherwise not found.
   */
  override async getTransactionStatus(
    txHash: TxHash,
  ): Promise<TransactionStatus> {
    const landed = await this.#read(async (tx) => {
      const stored = await reads.txByHashIn(
        tx,
        this.#store.dialect,
        Buffer.from(txHash, "hex"),
      );
      if (stored === null) return null;
      const block = await reads.blockAtOrBeforeSlotIn(tx, stored.blockSlot);
      const status =
        block === null || block.slot !== stored.blockSlot
          ? null
          : await reads.pointStatusIn(tx, this.#store.dialect, block);
      return { stored, block, status };
    });
    if (landed !== null) {
      if (!landed.stored.isValid)
        return { status: "failed", txHash, reason: "phase_two_failure" };
      return {
        status: "confirmed",
        txHash,
        confirmation: {
          txHash,
          slot: landed.stored.blockSlot,
          ...(landed.block === null
            ? {}
            : {
                blockHash: landed.block.hash.toString("hex"),
                blockHeight: landed.block.height,
              }),
          ...(landed.status?.kind === "canonical"
            ? { confirmations: landed.status.depth }
            : {}),
        },
      };
    }
    const inMempool = await this.node(() => this.transport.hasTx(txHash));
    return { status: inMempool ? "pending" : "not_found", txHash };
  }

  /**
   * Resolves true once the follower applied the transaction's block (valid
   * or phase-2-failed); throws {@link L1AwaitTxTimeoutError} after the
   * configured bound. Only qualifying txs reach the facts (plan §5.2).
   */
  override async awaitTx(
    txHash: TxHash,
    checkInterval = DEFAULT_CHECK_INTERVAL_MS,
  ): Promise<boolean> {
    // Monotonic: a wall-clock step neither cuts the wait short nor stretches it.
    const deadline = performance.now() + this.awaitTxTimeoutMs;
    const hash = Buffer.from(txHash, "hex");
    let lastSeen: "in_mempool" | "absent" | "unknown" = "unknown";
    for (;;) {
      try {
        if (
          (await this.#read((tx) =>
            reads.txByHashIn(tx, this.#store.dialect, hash),
          )) !== null
        )
          return true;
        lastSeen = (await this.node(() => this.transport.hasTx(txHash)))
          ? "in_mempool"
          : "absent";
      } catch (error) {
        if (!(error instanceof L1ProviderTransientError)) throw error;
        lastSeen = "unknown";
      }
      const remaining = deadline - performance.now();
      if (remaining <= 0)
        throw new L1AwaitTxTimeoutError(
          txHash,
          this.awaitTxTimeoutMs,
          lastSeen,
        );
      await new Promise((resolve) =>
        setTimeout(resolve, Math.min(checkInterval, remaining)),
      );
    }
  }

  protected override async outputsOf(
    addressOrCredential: Address | Credential,
    query: string,
  ): Promise<readonly Readonly<{ outRef: OutRef; output: OutputSummary }>[]> {
    const subject = utxoSubject(addressOrCredential);
    const tracked = this.#store.trackedSet();
    if (subject.by === "payment_credential") {
      if (!tracked.paymentCredentials.has(subject.hash.toString("hex")))
        throw new L1ProviderScopeError(
          query,
          `payment credential ${subject.hash.toString("hex")} is not tracked`,
        );
      return await this.#factsUtxos(subject);
    }
    const payment = addressCredentials(subject.address).payment;
    if (
      tracked.addresses.has(subject.address.toString("hex")) ||
      (payment !== null &&
        tracked.paymentCredentials.has(payment.hash.toString("hex")))
    )
      return await this.#factsUtxos(subject);
    return await this.ledgerUtxos(
      [{ query: "utxo_by_address", addresses: [subject.address] }],
      "tip",
    );
  }

  /** Live tracked outputs at the follower's tip; refuses before initialization. */
  async #factsUtxos(
    filter: reads.UtxoFilter,
  ): Promise<readonly StoredOutput[]> {
    return await this.#read(async (tx) => {
      if ((await reads.tipIn(tx, this.#store.dialect)) === null)
        throw new L1ProviderTransientError("follower", "not_initialized");
      const result = await reads.liveUtxosIn(tx, this.#store.dialect, filter);
      if (result.kind !== "ok")
        throw new L1ProviderTransientError("follower", result.kind);
      return result.utxos;
    });
  }

  async #read<T>(run: (tx: SqlTx) => Promise<T>): Promise<T> {
    try {
      return await this.#store.transaction("read", run);
    } catch (error) {
      if (error instanceof L1ProviderError) throw error;
      throw new L1ProviderTransientError(
        "store",
        error instanceof Error ? error.message : String(error),
        { cause: error },
      );
    }
  }
}
