import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import type {
  Address,
  Credential,
  Datum,
  DatumHash,
  OutRef as LucidOutRef,
  PolicyId,
  TransactionStatus,
  TransactionStatusOptions,
  TxHash,
  Unit,
  UTxO,
} from "@lucid-evolution/lucid";

import type { SqlTx, TransactionOptions } from "../sql/backend.js";
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
import {
  applyPending,
  atSubject,
  isTrackedAddress,
  lucidHoldsPolicy,
  lucidHoldsUnit,
  overLedger,
  type PendingApplied,
  type PendingSource,
  utxoKey,
} from "./pending.js";
import {
  answerPoint,
  plainByDatum,
  plainExceptDatums,
  type PlainOutputsAnswer,
  type PlainQuery,
  readPlainOutputs,
} from "./plain-read.js";
import { addressBytes, holdsUnit, splitUnit, utxoSubject } from "./utxo.js";

export type L1FollowerProviderOptions = Readonly<{
  /** The role's fact store: tracked outputs, qualifying txs, blocks. */
  store: FactStore;
  /** The role's node transport (LSQ, LocalTxSubmission, LocalTxMonitor). */
  transport: L1NodeTransport;
  /** How long `awaitTx` waits for the tx to reach the facts (default 160 s). */
  awaitTxTimeoutMs?: number;
  /**
   * The node's mempool transactions, in mempool order. Given, every UTxO
   * answer is the facts with them applied: an output a pending
   * transaction spends is never listed, and an output one creates is
   * listed where its address or out-ref is asked for. Absent, answers are
   * the facts alone.
   */
  pending?: PendingSource;
}>;

/**
 * A Lucid provider over the L1 follower (plan §4.4): the node-ledger
 * provider ({@link LedgerProvider}) with the role's fact store over it.
 * UTxO, datum and transaction reads in the tracked scope come from the
 * store; protocol parameters, slot configuration, outrefs outside the facts
 * and reward accounts from local state query at the node's tip; submission through
 * LocalTxSubmission and mempool presence through LocalTxMonitor. It never
 * reads Kupo or Ogmios and never evaluates scripts remotely. Every failure
 * is a typed error; none ends the process.
 *
 * Scope: an address the role tracks (by address or payment credential) is
 * answered from the facts alone, at the follower's tip, seed rows included,
 * with the `pending` source's transactions applied when one is given. An
 * address or payment credential the role does not track is refused, never
 * scanned for on the ledger.
 */
export class L1FollowerProvider extends LedgerProvider {
  readonly #store: FactStore;
  readonly #pending: PendingSource | undefined;

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
    this.#pending = options.pending;
  }

  override async getUtxos(
    addressOrCredential: Address | Credential,
  ): Promise<UTxO[]> {
    return await this.#atSubject(
      addressOrCredential,
      async () => await super.getUtxos(addressOrCredential),
    );
  }

  override async getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit,
  ): Promise<UTxO[]> {
    return await this.#atSubject(
      addressOrCredential,
      async () => await super.getUtxosWithUnit(addressOrCredential, unit),
      (utxo) => lucidHoldsUnit(utxo, unit),
    );
  }

  override async getUtxosWithPolicy(
    addressOrCredential: Address | Credential,
    policyId: PolicyId,
  ): Promise<UTxO[]> {
    return await this.#atSubject(
      addressOrCredential,
      async () => await super.getUtxosWithPolicy(addressOrCredential, policyId),
      (utxo) => lucidHoldsPolicy(utxo, policyId),
    );
  }

  /** The one live tracked output holding `unit`; only tracked outputs are searched. */
  override async getUtxoByUnit(unit: Unit): Promise<UTxO> {
    const { policyId, assetName } = splitUnit(unit);
    const applied = await this.#applied();
    const ledger = (await this.#factsUtxos({ by: "unit", policyId, assetName }))
      .filter((row) => holdsUnit(row.output, unit))
      .map(lucidUtxo);
    const tracked = this.#store.trackedSet();
    const holders =
      applied === undefined
        ? ledger
        : overLedger(
            applied,
            ledger,
            (utxo) =>
              lucidHoldsUnit(utxo, unit) &&
              isTrackedAddress(tracked, addressBytes(utxo.address)),
          );
    if (holders.length !== 1) throw new L1UnitLookupError(unit, holders.length);
    return holders[0]!;
  }

  /**
   * The live outputs among `outRefs`, in request order. An outref the facts
   * hold is answered from the facts (a spent row is omitted); any other one
   * from the ledger at the node's tip, unless the ledger's output is in the
   * tracked scope, whose state is the follower's; one neither holds, from
   * the outputs pending transactions create. An outref a pending
   * transaction spends is omitted. `options.signal` ends
   * the read of the facts (`TransactionOptions`); a ledger read takes no
   * signal, and the transport bounds each of its requests, not the read.
   */
  override async getUtxosByOutRef(
    outRefs: Array<LucidOutRef>,
    options?: TransactionStatusOptions,
  ): Promise<UTxO[]> {
    const wanted = [
      ...new Map(
        outRefs.map((outRef) => {
          const parsed = toOutRef(outRef);
          return [outRefKey(parsed), parsed] as const;
        }),
      ).values(),
    ];
    const applied = await this.#applied();
    // One read transaction: every stored row at one snapshot.
    const stored = await this.#read(
      (tx) => reads.outputsIn(tx, this.#store.dialect, wanted),
      options,
    );
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
    if (applied === undefined)
      return wanted.flatMap((outRef) => found.get(outRefKey(outRef)) ?? []);
    const created = new Map(
      applied.created.map((utxo) => [utxoKey(utxo), utxo] as const),
    );
    return wanted.flatMap((outRef) => {
      const key = outRefKey(outRef);
      if (applied.spent.has(key)) return [];
      return found.get(key) ?? created.get(key) ?? [];
    });
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
   * node's mempool holds it, otherwise not found. `options.signal` ends the
   * read of the facts (`TransactionOptions`); the mempool read takes no
   * signal, and the transport bounds each step of it, not the whole.
   */
  override async getTransactionStatus(
    txHash: TxHash,
    options?: TransactionStatusOptions,
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
    }, options);
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
    if (subject.by === "payment_credential") {
      if (
        !this.#store
          .trackedSet()
          .paymentCredentials.has(subject.hash.toString("hex"))
      )
        throw new L1ProviderScopeError(
          query,
          `payment credential ${subject.hash.toString("hex")} is not tracked`,
        );
      return await this.#factsUtxos(subject);
    }
    return await this.#factsUtxos(this.#trackedAddress(subject.address, query));
  }

  /**
   * For each requested inline datum (hex), the first `limit` plain outputs
   * (ada only, no datum hash, no reference script) live at the follower's
   * tip at the tracked `address` with exactly that datum, by out-ref,
   * skipping `excluded`; with the point of the snapshot they were read at.
   * With a pending source, outputs pending transactions spend are skipped
   * and the plain outputs they create there are candidates too.
   */
  async plainOutputsByDatum({
    address,
    requests,
    excluded = [],
  }: Readonly<{
    address: Address;
    requests: readonly Readonly<{ datum: string; limit: number }>[];
    excluded?: readonly LucidOutRef[];
  }>): Promise<PlainOutputsAnswer> {
    return await this.#plainRead(
      { address, query: "plainOutputsByDatum", excluded },
      plainByDatum(requests),
    );
  }

  /**
   * The first `limit` plain outputs live at the follower's tip at the
   * tracked `address`, by out-ref, whose inline datum (if any) is none of
   * `datums` (hex), skipping `excluded`; with the point of the snapshot.
   * With a pending source, outputs pending transactions spend are skipped
   * and the plain outputs they create there are candidates too.
   */
  async plainOutputsExceptDatums({
    address,
    datums,
    excluded = [],
    limit,
  }: Readonly<{
    address: Address;
    datums: readonly string[];
    excluded?: readonly LucidOutRef[];
    limit: number;
  }>): Promise<PlainOutputsAnswer> {
    return await this.#plainRead(
      { address, query: "plainOutputsExceptDatums", excluded },
      plainExceptDatums(datums, limit),
    );
  }

  /**
   * The point a plain-output read made now would report (`PlainOutputsAnswer`),
   * from the cursor and the pending source alone; refuses before
   * initialization.
   */
  async factsPoint(): Promise<string> {
    const applied = await this.#applied();
    const cursor = await this.#read((tx) =>
      reads.tipIn(tx, this.#store.dialect),
    );
    if (cursor === null)
      throw new L1ProviderTransientError("follower", "not_initialized");
    return answerPoint(cursor, applied);
  }

  /** The tracked address `address`; refuses one the role does not track. */
  #trackedAddress(
    address: Buffer,
    query: string,
  ): Readonly<{ by: "address"; address: Buffer }> {
    if (isTrackedAddress(this.#store.trackedSet(), address))
      return { by: "address", address };
    throw new L1ProviderScopeError(
      query,
      `address ${address.toString("hex")} is not tracked`,
    );
  }

  /**
   * `read`'s answer at `addressOrCredential` with the pending source
   * applied: the outputs pending transactions create there that `holds`
   * keeps are listed too. The mempool is listed before the facts are read.
   */
  async #atSubject(
    addressOrCredential: Address | Credential,
    read: () => Promise<UTxO[]>,
    holds: (utxo: UTxO) => boolean = () => true,
  ): Promise<UTxO[]> {
    const applied = await this.#applied();
    const ledger = await read();
    if (applied === undefined) return ledger;
    const at = atSubject(utxoSubject(addressOrCredential));
    return overLedger(applied, ledger, (utxo) => at(utxo) && holds(utxo));
  }

  /** The pending source's transactions applied, or none without a source. */
  async #applied(): Promise<PendingApplied | undefined> {
    return this.#pending && applyPending(await this.#pending());
  }

  /**
   * One plain-output read at the tracked `address` with the pending source
   * applied (`plain-read.ts`); refuses an address the role does not track.
   */
  async #plainRead(
    target: Readonly<{
      address: Address;
      query: string;
      excluded: readonly LucidOutRef[];
    }>,
    query: PlainQuery,
  ): Promise<PlainOutputsAnswer> {
    return await readPlainOutputs({
      store: this.#store,
      read: async (use) => await this.#read(use),
      applied: await this.#applied(),
      address: this.#trackedAddress(addressBytes(target.address), target.query)
        .address,
      excluded: target.excluded.map(toOutRef),
      query,
    });
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

  async #read<T>(
    run: (tx: SqlTx) => Promise<T>,
    options?: TransactionOptions,
  ): Promise<T> {
    try {
      return await this.#store.transaction("read", run, options);
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
