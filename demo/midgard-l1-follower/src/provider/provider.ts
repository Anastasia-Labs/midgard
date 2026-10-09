import {
  type L1NodeTransport,
  queryRewardAccount,
} from "@al-ft/l1-node-transport";
import {
  type Address,
  CML,
  type Credential,
  type Datum,
  type DatumHash,
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

import { readArray, skipItem, slice } from "../cbor/reader.js";
import { blake2b256 } from "../codec.js";
import { addressCredentials } from "../decode/output.js";
import { decodeLedgerUtxos, type LedgerUtxo } from "../decode/utxo.js";
import type { SqlTx } from "../sql/backend.js";
import { datumByHashIn } from "../store/datum.js";
import type { FactStore } from "../store/fact-store.js";
import { isTrackedOutput } from "../store/qualify.js";
import * as reads from "../store/reads.js";
import type { OutputSummary, OutRef, StoredOutput } from "../types.js";
import {
  fromTransportError,
  L1AwaitTxTimeoutError,
  L1CarriagePendingError,
  L1LocalEvaluationOnlyError,
  L1ProviderError,
  L1ProviderRequestError,
  L1ProviderScopeError,
  L1ProviderTransientError,
  L1SubmitRejectedError,
  L1UnitLookupError,
} from "./errors.js";
import {
  decodeEraHistory,
  decodeProtocolParameters,
  decodeSystemStart,
  slotConfigFrom,
} from "./ledger.js";
import {
  holdsPolicy,
  holdsUnit,
  splitUnit,
  toLucidUtxo,
  utxoSubject,
} from "./utxo.js";

export type L1FollowerProviderOptions = Readonly<{
  /** The role's fact store: tracked outputs, qualifying txs, blocks. */
  store: FactStore;
  /** The role's node transport (LSQ, LocalTxSubmission, LocalTxMonitor). */
  transport: L1NodeTransport;
  /** How long `awaitTx` waits for the tx to reach the facts (default 160 s). */
  awaitTxTimeoutMs?: number;
}>;

/** The ledger state query takes at most this many items per request. */
const MAX_QUERY_ITEMS = 4_096;
const DEFAULT_AWAIT_TX_TIMEOUT_MS = 160_000;
const DEFAULT_CHECK_INTERVAL_MS = 3_000;

const delay = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

const outRefKey = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index}`;

const toOutRef = (outRef: LucidOutRef): OutRef => ({
  txHash: Buffer.from(outRef.txHash, "hex"),
  index: outRef.outputIndex,
});

const lucidUtxo = (
  row: Readonly<{ outRef: OutRef; output: OutputSummary }>,
): UTxO => toLucidUtxo(row.outRef, row.output);

/** The transaction id: blake2b-256 of the body's exact bytes. */
const transactionId = (tx: Buffer): string => {
  try {
    const body = readArray(tx, 0).items[0];
    if (body === undefined) throw new Error("the transaction has no body");
    return blake2b256(slice(tx, body, skipItem(tx, body))).toString("hex");
  } catch (error) {
    throw new L1ProviderRequestError(
      "tx_undecodable",
      "the transaction is not a CBOR [body, witnesses, ...] array",
      { cause: error },
    );
  }
};

/**
 * A Lucid provider over the L1 follower (plan §4.4): UTxO, datum and
 * transaction reads from the fact store; protocol parameters, slot
 * configuration, untracked UTxOs and reward accounts from local state query;
 * submission through LocalTxSubmission and mempool presence through
 * LocalTxMonitor. It never reads Kupo or Ogmios and never evaluates scripts
 * remotely. Every failure is a typed error; none ends the process.
 *
 * Scope: an address the role tracks (by address or payment credential) is
 * answered from the facts alone, at the follower's tip, seed rows included.
 * An untracked address is answered from the ledger at the node's tip. A
 * payment credential the role does not track is refused: the ledger query
 * cannot enumerate by credential.
 */
export class L1FollowerProvider implements Provider {
  readonly #store: FactStore;
  readonly #transport: L1NodeTransport;
  readonly #awaitTxTimeoutMs: number;

  constructor(options: L1FollowerProviderOptions) {
    this.#store = options.store;
    this.#transport = options.transport;
    this.#awaitTxTimeoutMs =
      options.awaitTxTimeoutMs ?? DEFAULT_AWAIT_TX_TIMEOUT_MS;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    return decodeProtocolParameters(
      await this.#node(() =>
        this.#transport.query({ query: "protocol_params" }),
      ),
    );
  }

  /** Lucid's slot configuration from the ledger's era history and system start. */
  async slotConfig(): Promise<SlotConfig> {
    const [systemStart, eraHistory] = await this.#node(() =>
      this.#transport.withLedgerState("tip", async (state) => [
        await state.query({ query: "system_start" }),
        await state.query({ query: "era_history" }),
      ]),
    );
    return slotConfigFrom(
      decodeSystemStart(systemStart!),
      decodeEraHistory(eraHistory!),
    );
  }

  async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    return (await this.#outputsOf(addressOrCredential, "getUtxos")).map(
      lucidUtxo,
    );
  }

  async getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit,
  ): Promise<UTxO[]> {
    splitUnit(unit);
    return (await this.#outputsOf(addressOrCredential, "getUtxosWithUnit"))
      .filter((row) => holdsUnit(row.output, unit))
      .map(lucidUtxo);
  }

  async getUtxosWithPolicy(
    addressOrCredential: Address | Credential,
    policyId: PolicyId,
  ): Promise<UTxO[]> {
    return (await this.#outputsOf(addressOrCredential, "getUtxosWithPolicy"))
      .filter((row) => holdsPolicy(row.output, policyId))
      .map(lucidUtxo);
  }

  /** The one live tracked output holding `unit`; only tracked outputs are searched. */
  async getUtxoByUnit(unit: Unit): Promise<UTxO> {
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
  async getUtxosByOutRef(outRefs: Array<LucidOutRef>): Promise<UTxO[]> {
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
    for (let start = 0; start < missing.length; start += MAX_QUERY_ITEMS)
      for (const entry of await this.#ledgerUtxos({
        query: "utxo_by_txin",
        txIns: missing.slice(start, start + MAX_QUERY_ITEMS).map((outRef) => ({
          txId: outRef.txHash.toString("hex"),
          index: outRef.index,
        })),
      }))
        if (!isTrackedOutput(entry.output, tracked))
          found.set(outRefKey(entry.outRef), lucidUtxo(entry));
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
    const snapshot = await this.#node(() =>
      queryRewardAccount(this.#transport, credential),
    );
    return {
      registered: snapshot.registered,
      poolId:
        snapshot.poolIdHash === null ? null : poolBech32(snapshot.poolIdHash),
      rewards: snapshot.rewardsLovelace,
    };
  }

  /** A datum preimage from the stored witness sets; never an empty answer. */
  async getDatum(datumHash: DatumHash): Promise<Datum> {
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
  async getTransactionStatus(txHash: TxHash): Promise<TransactionStatus> {
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
    const inMempool = await this.#node(() => this.#transport.hasTx(txHash));
    return { status: inMempool ? "pending" : "not_found", txHash };
  }

  /**
   * Resolves true once the follower applied the transaction's block (valid
   * or phase-2-failed); throws {@link L1AwaitTxTimeoutError} after the
   * configured bound. Only qualifying txs reach the facts (plan §5.2).
   */
  async awaitTx(
    txHash: TxHash,
    checkInterval = DEFAULT_CHECK_INTERVAL_MS,
  ): Promise<boolean> {
    // Monotonic: a wall-clock step neither cuts the wait short nor stretches it.
    const deadline = performance.now() + this.#awaitTxTimeoutMs;
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
        lastSeen = (await this.#node(() => this.#transport.hasTx(txHash)))
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
          this.#awaitTxTimeoutMs,
          lastSeen,
        );
      await delay(Math.min(checkInterval, remaining));
    }
  }

  /**
   * Submits through LocalTxSubmission; a ledger rejection carries its raw
   * bytes. A submission the transport stopped waiting for, or whose sidecar
   * exited, throws {@link L1SubmitOutcomeUnknownError} with the tx id.
   */
  async submitTx(tx: Transaction): Promise<TxHash> {
    const bytes = Buffer.from(tx, "hex");
    const txHash = transactionId(bytes);
    const result = await this.#node(
      () => this.#transport.submit(bytes),
      "submit",
      txHash,
    );
    if (!result.accepted)
      throw new L1SubmitRejectedError(txHash, result.rejection);
    return txHash;
  }

  /** Never remote: complete transactions with `localUPLCEval: true`. */
  evaluateTx(): Promise<EvalRedeemer[]> {
    return Promise.reject(new L1LocalEvaluationOnlyError());
  }

  async #outputsOf(
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
    return await this.#ledgerUtxos({
      query: "utxo_by_address",
      addresses: [subject.address],
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

  async #ledgerUtxos(
    query: Parameters<L1NodeTransport["query"]>[0],
  ): Promise<readonly LedgerUtxo[]> {
    return decodeLedgerUtxos(
      await this.#node(() => this.#transport.query(query)),
    );
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

  async #node<T>(
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
}

const poolBech32 = (hashHex: string): string => {
  const hash = CML.Ed25519KeyHash.from_hex(hashHex);
  try {
    return hash.to_bech32("pool");
  } finally {
    hash.free();
  }
};
