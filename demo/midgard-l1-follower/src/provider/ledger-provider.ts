import {
  type BlockPoint,
  chainPoint,
  type L1NodeTransport,
  type LedgerQuery,
  type LedgerStateSession,
  queryRewardAccount,
} from "@al-ft/l1-node-transport";
import {
  type Address,
  type Credential,
  type Datum,
  type Delegation,
  type EvalRedeemer,
  getAddressDetails,
  type OutRef as LucidOutRef,
  type PolicyId,
  type ProtocolParameters,
  type Provider,
  type RewardAccountState,
  type RewardAddress,
  type SlotConfig,
  type Transaction,
  type TransactionStatus,
  type TxHash,
  type Unit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { decodeLedgerUtxos, type LedgerUtxo } from "../decode/utxo.js";
import type { OutputSummary, OutRef } from "../types.js";
import {
  fromTransportError,
  L1AwaitTxTimeoutError,
  L1LedgerScopeError,
  L1LocalEvaluationOnlyError,
  L1ProviderRequestError,
  L1ProviderTransientError,
  L1SubmitRejectedError,
  L1TxStatusUnknownError,
} from "./errors.js";
import {
  decodeEraHistory,
  decodeProtocolParameters,
  decodeSystemStart,
  slotConfigFrom,
} from "./ledger.js";
import {
  acquireRefusal,
  decodeBlockNo,
  decodeTipPoint,
  lucidUtxo,
  outputCount,
  outRefKey,
  poolBech32,
  toOutRef,
  transactionId,
} from "./ledger-answers.js";
import { holdsPolicy, holdsUnit, splitUnit, utxoSubject } from "./utxo.js";

export { lucidUtxo, outRefKey, toOutRef } from "./ledger-answers.js";

export type LedgerProviderOptions = Readonly<{
  /** The node transport (LSQ, LocalTxSubmission, LocalTxMonitor). */
  transport: L1NodeTransport;
  /** How long `awaitTx` waits for the tx's outputs (default 160 s). */
  awaitTxTimeoutMs?: number;
  /**
   * How long one acquired chain point answers UTxO reads, standing in for
   * one build step (default {@link DEFAULT_LEDGER_PIN_POINT_MS}; see the
   * class comment for the limit). 0 reads every query at the node's tip.
   */
  pinPointMs?: number;
  /** A monotonic clock for the pin window (default `performance.now`). */
  monotonicNowMs?: () => number;
}>;

/** The node's ledger tip: its chain point and block number, read together. */
export type LedgerTip = Readonly<{
  slot: number;
  /** The tip block's header hash, lowercase hex. */
  hash: string;
  blockNo: number;
}>;

/** Where a point stands on the node's chain, from an acquire at it. */
export type LedgerPointStatus = "on_chain" | "not_on_chain" | "immutable";

/** The pin window that stands in for one build step (class comment). */
export const DEFAULT_LEDGER_PIN_POINT_MS = 5_000;
/** The ledger state query takes at most this many items per request. */
export const MAX_QUERY_ITEMS = 4_096;
export const DEFAULT_AWAIT_TX_TIMEOUT_MS = 160_000;
export const DEFAULT_CHECK_INTERVAL_MS = 3_000;
/** Output indices probed for a tx this provider did not submit. */
const UNKNOWN_OUTPUT_PROBE = 32;
/** Output counts remembered from submissions (oldest dropped first). */
const REMEMBERED_SUBMISSIONS = 1_024;

type Pin = Readonly<{ point: BlockPoint; untilMs: number }>;

/**
 * A Lucid provider over the local node's ledger alone (option E's NodeLedger
 * adapter): UTxOs by address and by outref over local state query, protocol
 * parameters, slot configuration and reward accounts from the ledger,
 * submission through LocalTxSubmission, mempool presence through
 * LocalTxMonitor. It needs no store and no follower, and never reads Kupo,
 * Ogmios or a remote provider.
 *
 * One build step reads at one acquired point: the first UTxO read acquires
 * the node's tip and later reads reuse that point for `pinPointMs`. A
 * submission, a transaction-status read, a confirmed `awaitTx`,
 * `clearPin()` and `repin()` start a new step. A point the node no longer
 * holds is re-acquired once.
 *
 * Limit: the pin is a time window, not the build step itself. Lucid's
 * builder calls the provider without marking where a build begins or ends,
 * and binding the pin to the step would mean wrapping every command's
 * build in an acquire/release, so a build that takes longer than
 * `pinPointMs` reads its later UTxOs at a newer point. A mixed-point build
 * can only fail (an input it read is gone at submission), never produce an
 * unsound transaction: the ledger validates the submitted bytes.
 *
 * Confirmation is the tx's outputs in the ledger (`getTransactionStatus`).
 *
 * Scope: local state query cannot find outputs by payment credential or
 * unit and holds no datum preimages; those reads refuse with
 * {@link L1LedgerScopeError}.
 */
export class LedgerProvider implements Provider {
  protected readonly transport: L1NodeTransport;
  protected readonly awaitTxTimeoutMs: number;
  readonly #pinPointMs: number;
  readonly #now: () => number;
  #pin: Pin | undefined;
  readonly #outputCounts = new Map<string, number>();

  constructor(options: LedgerProviderOptions) {
    this.transport = options.transport;
    this.awaitTxTimeoutMs =
      options.awaitTxTimeoutMs ?? DEFAULT_AWAIT_TX_TIMEOUT_MS;
    this.#pinPointMs = options.pinPointMs ?? DEFAULT_LEDGER_PIN_POINT_MS;
    this.#now = options.monotonicNowMs ?? (() => performance.now());
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    return decodeProtocolParameters(
      await this.node(() => this.transport.query({ query: "protocol_params" })),
    );
  }

  /** Lucid's slot configuration from the ledger's era history and system start. */
  async slotConfig(): Promise<SlotConfig> {
    const [systemStart, eraHistory] = await this.node(() =>
      this.transport.withLedgerState("tip", async (state) => [
        await state.query({ query: "system_start" }),
        await state.query({ query: "era_history" }),
      ]),
    );
    return slotConfigFrom(
      decodeSystemStart(systemStart!),
      decodeEraHistory(eraHistory!),
    );
  }

  /** The node's ledger tip now: its point and block number from one acquired state. */
  async readTip(): Promise<LedgerTip> {
    const [point, blockNo] = await this.node(() =>
      this.transport.withLedgerState("tip", async (state) => [
        decodeTipPoint(await state.query({ query: "chain_point" })),
        decodeBlockNo(await state.query({ query: "chain_block_no" })),
      ]),
    );
    if (point === undefined || blockNo === undefined || point.kind !== "point")
      throw new L1ProviderTransientError("transport", "ledger_at_origin");
    return { slot: Number(point.slot), hash: point.hash, blockNo };
  }

  /** The point the current build step reads at, acquiring one when none is held. */
  async pinnedTip(): Promise<Readonly<{ slot: number; hash: string }>> {
    const point = await this.#buildPoint();
    if (point === "tip") {
      const tip = await this.readTip();
      return { slot: tip.slot, hash: tip.hash };
    }
    return { slot: Number(point.slot), hash: point.hash };
  }

  /** Starts a new build step at the node's tip now and returns its point. */
  async repin(): Promise<Readonly<{ slot: number; hash: string }>> {
    this.#pin = undefined;
    return await this.pinnedTip();
  }

  /** Ends the current build step: the next read acquires the tip again. */
  clearPin(): void {
    this.#pin = undefined;
  }

  /**
   * Where `point` stands: still on the node's chain, no longer on it, or
   * older than the node's volatile window (immutable, so on the chain it was
   * read on: no rollback reaches that deep).
   */
  async pointStatus(
    point: Readonly<{ slot: number; hash: string }>,
  ): Promise<LedgerPointStatus> {
    try {
      await this.transport.withLedgerState(
        chainPoint(BigInt(point.slot), point.hash),
        async () => undefined,
      );
      return "on_chain";
    } catch (error) {
      const refusal = acquireRefusal(error);
      if (refusal !== undefined) return refusal;
      throw fromTransportError(error);
    }
  }

  async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    return (await this.outputsOf(addressOrCredential, "getUtxos")).map(
      lucidUtxo,
    );
  }

  async getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit,
  ): Promise<UTxO[]> {
    splitUnit(unit);
    return (await this.outputsOf(addressOrCredential, "getUtxosWithUnit"))
      .filter((row) => holdsUnit(row.output, unit))
      .map(lucidUtxo);
  }

  async getUtxosWithPolicy(
    addressOrCredential: Address | Credential,
    policyId: PolicyId,
  ): Promise<UTxO[]> {
    return (await this.outputsOf(addressOrCredential, "getUtxosWithPolicy"))
      .filter((row) => holdsPolicy(row.output, policyId))
      .map(lucidUtxo);
  }

  getUtxoByUnit(unit: Unit): Promise<UTxO> {
    return Promise.reject(
      new L1LedgerScopeError(
        "getUtxoByUnit",
        `local state query cannot find the output holding ${unit}; read the address that holds it`,
      ),
    );
  }

  /** The live outputs among `outRefs`, in request order, at the build step's point. */
  async getUtxosByOutRef(outRefs: Array<LucidOutRef>): Promise<UTxO[]> {
    const wanted = [
      ...new Map(
        outRefs.map((outRef) => {
          const parsed = toOutRef(outRef);
          return [outRefKey(parsed), parsed] as const;
        }),
      ).values(),
    ];
    const found = new Map(
      (await this.ledgerUtxosByTxIn(wanted, "build")).map(
        (entry) => [outRefKey(entry.outRef), lucidUtxo(entry)] as const,
      ),
    );
    return wanted.flatMap((outRef) => found.get(outRefKey(outRef)) ?? []);
  }

  async getDelegation(rewardAddress: RewardAddress): Promise<Delegation> {
    const { poolId, rewards } = await this.getRewardAccount(rewardAddress);
    return { poolId, rewards };
  }

  async getRewardAccount(
    rewardAddress: RewardAddress,
  ): Promise<RewardAccountState> {
    const credential = getAddressDetails(rewardAddress).stakeCredential;
    if (credential === undefined)
      throw new L1ProviderRequestError(
        "invalid_reward_address",
        `${rewardAddress} carries no stake credential`,
      );
    const snapshot = await this.node(() =>
      queryRewardAccount(this.transport, credential),
    );
    return {
      registered: snapshot.registered,
      poolId:
        snapshot.poolIdHash === null ? null : poolBech32(snapshot.poolIdHash),
      rewards: snapshot.rewardsLovelace,
    };
  }

  getDatum(datumHash: string): Promise<Datum> {
    return Promise.reject(
      new L1LedgerScopeError(
        "getDatum",
        `the ledger holds no preimage of datum hash ${datumHash}; use an inline datum`,
      ),
    );
  }

  /**
   * Confirmed once any of the tx's outputs is in the ledger at the tip (no
   * block point: local state query does not say which block holds a tx),
   * pending while the node's mempool holds it, otherwise unknown: it throws
   * {@link L1TxStatusUnknownError}, never answering "not found", since a tx
   * whose every output is spent reads like one that never landed. A status
   * read ends the build step.
   */
  async getTransactionStatus(txHash: TxHash): Promise<TransactionStatus> {
    this.clearPin();
    if (await this.#landed(txHash))
      return { status: "confirmed", txHash, confirmation: { txHash } };
    if (await this.node(() => this.transport.hasTx(txHash)))
      return { status: "pending", txHash };
    throw new L1TxStatusUnknownError(txHash);
  }

  /**
   * Resolves true once any of the tx's outputs is in the ledger at the tip;
   * throws {@link L1AwaitTxTimeoutError} after the configured bound. A
   * transient transport failure is waited out within the bound.
   */
  async awaitTx(
    txHash: TxHash,
    checkInterval = DEFAULT_CHECK_INTERVAL_MS,
  ): Promise<boolean> {
    // Monotonic: a wall-clock step neither cuts the wait short nor stretches it.
    const deadline = performance.now() + this.awaitTxTimeoutMs;
    let lastSeen: "in_mempool" | "absent" | "unknown" = "unknown";
    for (;;) {
      try {
        if (await this.#landed(txHash)) {
          this.clearPin();
          return true;
        }
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
          "the node's ledger",
        );
      await new Promise((resolve) =>
        setTimeout(resolve, Math.min(checkInterval, remaining)),
      );
    }
  }

  /**
   * Submits through LocalTxSubmission; a ledger rejection carries its raw
   * bytes. A submission the transport stopped waiting for, or whose sidecar
   * exited, throws {@link L1SubmitOutcomeUnknownError} with the tx id. An
   * accepted submission ends the build step.
   */
  async submitTx(tx: Transaction): Promise<TxHash> {
    const bytes = Buffer.from(tx, "hex");
    const txHash = transactionId(bytes);
    const result = await this.node(
      () => this.transport.submit(bytes),
      "submit",
      txHash,
    );
    if (!result.accepted)
      throw new L1SubmitRejectedError(txHash, result.rejection);
    this.#rememberOutputs(txHash, outputCount(bytes));
    this.clearPin();
    return txHash;
  }

  /** Never remote: complete transactions with `localUPLCEval: true`. */
  evaluateTx(): Promise<EvalRedeemer[]> {
    return Promise.reject(new L1LocalEvaluationOnlyError());
  }

  /** The outputs at an address; a payment credential is out of scope. */
  protected async outputsOf(
    addressOrCredential: Address | Credential,
    query: string,
  ): Promise<readonly Readonly<{ outRef: OutRef; output: OutputSummary }>[]> {
    const subject = utxoSubject(addressOrCredential);
    if (subject.by === "payment_credential")
      throw new L1LedgerScopeError(
        query,
        `local state query cannot enumerate payment credential ${subject.hash.toString("hex")}; read an address`,
      );
    return await this.ledgerUtxos(
      [{ query: "utxo_by_address", addresses: [subject.address] }],
      "build",
    );
  }

  /** The ledger's outputs at `outRefs`, batched within one acquired state. */
  protected async ledgerUtxosByTxIn(
    outRefs: readonly OutRef[],
    at: "build" | "tip",
  ): Promise<readonly LedgerUtxo[]> {
    const queries: LedgerQuery[] = [];
    for (let start = 0; start < outRefs.length; start += MAX_QUERY_ITEMS)
      queries.push({
        query: "utxo_by_txin",
        txIns: outRefs.slice(start, start + MAX_QUERY_ITEMS).map((outRef) => ({
          txId: outRef.txHash.toString("hex"),
          index: outRef.index,
        })),
      });
    return queries.length === 0 ? [] : await this.ledgerUtxos(queries, at);
  }

  /** UTxO queries answered within one acquired state: the build step's point, or the tip. */
  protected async ledgerUtxos(
    queries: readonly LedgerQuery[],
    at: "build" | "tip",
  ): Promise<readonly LedgerUtxo[]> {
    return await this.#atPoint(at, async (session) => {
      const utxos: LedgerUtxo[] = [];
      for (const query of queries)
        utxos.push(...decodeLedgerUtxos(await session.query(query)));
      return utxos;
    });
  }

  protected async node<T>(
    run: () => Promise<T>,
    operation: "query" | "submit" = "query",
    txHash: string | null = null,
  ): Promise<T> {
    try {
      return await run();
    } catch (error) {
      throw fromTransportError(error, operation, txHash);
    }
  }

  async #landed(txHash: TxHash): Promise<boolean> {
    const count = this.#outputCounts.get(txHash) ?? UNKNOWN_OUTPUT_PROBE;
    const hash = Buffer.from(txHash, "hex");
    const outRefs = Array.from(
      { length: Math.min(Math.max(count, 1), MAX_QUERY_ITEMS) },
      (_, index) => ({ txHash: hash, index }),
    );
    return (await this.ledgerUtxosByTxIn(outRefs, "tip")).length > 0;
  }

  #rememberOutputs(txHash: string, count: number | undefined): void {
    if (count === undefined) return;
    this.#outputCounts.set(txHash, count);
    if (this.#outputCounts.size > REMEMBERED_SUBMISSIONS)
      this.#outputCounts.delete(this.#outputCounts.keys().next().value!);
  }

  async #buildPoint(): Promise<BlockPoint | "tip"> {
    if (this.#pinPointMs <= 0) return "tip";
    const now = this.#now();
    if (this.#pin !== undefined && now < this.#pin.untilMs)
      return this.#pin.point;
    const point = await this.node(() =>
      this.transport.withLedgerState("tip", async (state) =>
        decodeTipPoint(await state.query({ query: "chain_point" })),
      ),
    );
    // A ledger at the origin holds no outputs: read it at the tip.
    if (point === undefined) return "tip";
    this.#pin = { point, untilMs: now + this.#pinPointMs };
    return point;
  }

  async #atPoint<T>(
    at: "build" | "tip",
    use: (session: LedgerStateSession) => Promise<T>,
  ): Promise<T> {
    for (let attempt = 0; ; attempt += 1) {
      const point = at === "tip" ? "tip" : await this.#buildPoint();
      try {
        return await this.transport.withLedgerState(point, use);
      } catch (error) {
        // The node moved past the pinned point: start the step again, once.
        if (attempt === 0 && point !== "tip" && acquireRefusal(error)) {
          this.#pin = undefined;
          continue;
        }
        throw fromTransportError(error);
      }
    }
  }
}
