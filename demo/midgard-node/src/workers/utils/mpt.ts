import { BatchDBOp } from "@ethereumjs/util";
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Data, Effect, Option } from "effect";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { SqlClient } from "@effect/sql";
import {
  CML,
  Data as LucidData,
  getAddressDetails,
  toHex,
  valueToAssets,
} from "@lucid-evolution/lucid";
import type { Assets, UTxO } from "@lucid-evolution/lucid";
import { Level } from "level";
import { Database, NodeConfig } from "@/services/index.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as MempoolTxDeltasDB from "@/database/mempoolTxDeltas.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as DepositsDB from "@/database/deposits.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { FileSystemError, findSpentAndProducedUTxOs } from "@/utils.js";
import { aikenSerialisedPlutusDataCbor } from "@/utils/plutus-data-cbor.js";
import * as FS from "fs";
import * as SDK from "@al-ft/midgard-sdk";
import { encodeMidgardTxOutput } from "@al-ft/lucid-midgard";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import {
  decodeMidgardTxOutput,
  midgardOutputAddressText,
  midgardValueToCmlValue,
} from "@/validation/midgard-output.js";

const LEVELDB_ENCODING_OPTS = {
  keyEncoding: ETH_UTILS.KeyEncoding.Bytes,
  valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
};

export const COMMIT_REJECT_CODE_DECODE_FAILED = "E_COMMIT_CBOR_DESERIALIZATION";
export const COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT =
  "E_COMMIT_WITHDRAWN_REFERENCE_INPUT";

export type ResolvedTxDeltaForCommit =
  | {
      readonly _tag: "Decoded";
      readonly spent: readonly Buffer[];
      readonly produced: readonly Ledger.MinimalEntry[];
    }
  | {
      readonly _tag: "Rejected";
      readonly rejection: TxRejectionsDB.EntryNoTimestamp;
    };

export const resolveTxDeltaForCommit = (
  entry: Tx.EntryWithTimeStamp,
  existingDelta: MempoolTxDeltasDB.TxDelta | undefined,
): Effect.Effect<ResolvedTxDeltaForCommit, never> =>
  Effect.gen(function* () {
    if (existingDelta !== undefined) {
      return {
        _tag: "Decoded",
        spent: existingDelta.spent.map((outRef) => Buffer.from(outRef)),
        produced: existingDelta.produced.map((deltaEntry) => ({
          [Ledger.Columns.OUTREF]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTREF],
          ),
          [Ledger.Columns.OUTPUT]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTPUT],
          ),
        })),
      };
    }

    const txId = entry[Tx.Columns.TX_ID];
    const txCbor = entry[Tx.Columns.TX];
    const decoded = yield* findSpentAndProducedUTxOs(txCbor, txId).pipe(
      Effect.either,
    );
    if (decoded._tag === "Left") {
      return {
        _tag: "Rejected",
        rejection: {
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(txId),
          [TxRejectionsDB.Columns.REJECT_CODE]:
            COMMIT_REJECT_CODE_DECODE_FAILED,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: decoded.left.message,
        },
      };
    }

    return {
      _tag: "Decoded",
      spent: decoded.right.spent,
      produced: decoded.right.produced,
    };
  });

export const makeMpts: Effect.Effect<
  { ledgerTrie: MidgardMpt; mempoolTrie: MidgardMpt },
  MptError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const mempoolTrie = yield* MidgardMpt.create(
    "mempool",
    nodeConfig.MEMPOOL_MPT_DB_PATH,
  );
  const ledgerTrie = yield* MidgardMpt.create(
    "ledger",
    nodeConfig.LEDGER_MPT_DB_PATH,
  );
  const ledgerRootIsEmpty = yield* ledgerTrie.rootIsEmpty();
  if (ledgerRootIsEmpty) {
    yield* Effect.logInfo(
      "🔹 No previous ledger trie root found - inserting genesis utxos",
    );
    const ops: ETH_UTILS.BatchDBOp[] = yield* Effect.allSuccesses(
      nodeConfig.GENESIS_UTXOS.map((u: UTxO) =>
        utxoToPutBatchOp(u).pipe(
          Effect.tapError((e) =>
            Effect.logError(`IGNORED ERROR WITH GENESIS UTXOS: ${e}`),
          ),
        ),
      ),
    );
    yield* ledgerTrie.batch(ops);
    const rootAfterGenesis = yield* ledgerTrie.getRootHex();
    yield* Effect.logInfo(
      `🔹 New ledger trie root after inserting genesis utxos: ${rootAfterGenesis}`,
    );
  }
  return {
    ledgerTrie,
    mempoolTrie,
  };
});

export const utxoToPutBatchOp = (
  utxo: UTxO,
): Effect.Effect<ETH_UTILS.BatchDBOp, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const input = yield* Effect.try({
      try: () =>
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(utxo.txHash),
          BigInt(utxo.outputIndex),
        ),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to convert UTxO outref to CML.TransactionInput",
          cause: e,
        }),
    });
    const output = yield* Effect.try({
      try: () =>
        encodeMidgardTxOutput(utxo.address, utxo.assets, {
          ...(utxo.datum == null
            ? {}
            : { datum: { kind: "inline" as const, data: utxo.datum } }),
        }),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to convert UTxO to Midgard output CBOR",
          cause: e,
        }),
    });
    const op: ETH_UTILS.BatchDBOp = {
      type: "put",
      key: Buffer.from(input.to_cbor_bytes()),
      value: output,
    };
    return op;
  });

export const deleteMempoolMpt = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMpt(config.MEMPOOL_MPT_DB_PATH, "mempool");
});

export const deleteLedgerMpt = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMpt(config.LEDGER_MPT_DB_PATH, "ledger");
});

export const deleteMpt = (
  path: string,
  name: string,
): Effect.Effect<void, FileSystemError> =>
  Effect.try({
    try: () => FS.rmSync(path, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: `Failed to delete ${name}'s LevelDB file from disk`,
        cause: e,
      }),
  }).pipe(Effect.withLogSpan(`Delete ${name} MPT`));

// Make mempool trie, and fill it with ledger trie with processed mempool txs
export type ProcessMptsConfig = {
  readonly currentBlockStartTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
  readonly withdrawalVisibilityBarrierTime?: Date;
};

type DecodedMempoolTxForCommit = {
  readonly entry: Tx.EntryWithTimeStamp;
  readonly txHash: Buffer;
  readonly txCbor: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.MinimalEntry[];
};

type ClassifiedWithdrawal = {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly validity: WithdrawalsDB.Validity;
  readonly validityDetail: unknown;
  readonly settlementEventInfo: Buffer;
  readonly shouldDeleteLedgerUtxo: boolean;
};

const LOVELACE_UNIT = "lovelace";
const ADA_POLICY_ID = "";
const ADA_ASSET_NAME = "";

const normalizeAssets = (assets: Assets): Assets => {
  const result: Record<string, bigint> = {};
  for (const [unit, quantity] of Object.entries(assets)) {
    if (quantity === 0n) {
      continue;
    }
    result[unit] = (result[unit] ?? 0n) + quantity;
  }
  return result as Assets;
};

const assetsToValue = (assets: Assets): SDK.Value => {
  const outer = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(normalizeAssets(assets))) {
    const policyId =
      unit === LOVELACE_UNIT ? ADA_POLICY_ID : unit.slice(0, 56).toLowerCase();
    const assetName =
      unit === LOVELACE_UNIT ? ADA_ASSET_NAME : unit.slice(56).toLowerCase();
    const inner = outer.get(policyId) ?? new Map<string, bigint>();
    inner.set(assetName, (inner.get(assetName) ?? 0n) + quantity);
    outer.set(policyId, inner);
  }
  return outer;
};

const withdrawalValidityToSdk = (
  validity: WithdrawalsDB.Validity,
  detail: unknown,
): SDK.WithdrawalValidity => {
  if (validity !== WithdrawalsDB.Validity.SpentWithdrawalUtxo) {
    return validity as SDK.WithdrawalValidity;
  }
  const detailRecord =
    typeof detail === "object" && detail !== null
      ? (detail as { readonly l2_tx_id?: unknown })
      : {};
  const l2TxId =
    typeof detailRecord.l2_tx_id === "string"
      ? detailRecord.l2_tx_id
      : "00".repeat(32);
  return {
    SpentWithdrawalUtxo: {
      l2_tx_id: l2TxId,
    },
  } as SDK.WithdrawalValidity;
};

const decodeWithdrawalInfo = (
  entry: WithdrawalsDB.Entry,
): Effect.Effect<SDK.WithdrawalInfo, DatabaseError, never> =>
  Effect.try({
    try: () =>
      LucidData.from(
        entry[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex"),
        SDK.WithdrawalInfo,
      ) as SDK.WithdrawalInfo,
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message: "Failed to decode withdrawal event info",
        cause,
      }),
  });

const encodeWithdrawalSettlementInfo = (
  entry: WithdrawalsDB.Entry,
  validity: WithdrawalsDB.Validity,
  validityDetail: unknown,
): Effect.Effect<Buffer, DatabaseError, never> =>
  Effect.gen(function* () {
    const rawInfo = yield* decodeWithdrawalInfo(entry);
    return yield* Effect.try({
      try: () =>
        Buffer.from(
          LucidData.to(
            {
              ...rawInfo,
              validity: withdrawalValidityToSdk(validity, validityDetail),
            } satisfies SDK.WithdrawalInfo,
            SDK.WithdrawalInfo,
          ),
          "hex",
        ),
      catch: (cause) =>
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message: "Failed to encode withdrawal settlement event info",
          cause,
        }),
    });
  });

const decodeLedgerUtxo = ({
  outRef,
  output,
}: {
  readonly outRef: Buffer;
  readonly output: Buffer;
}): Effect.Effect<UTxO, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const input = CML.TransactionInput.from_cbor_bytes(outRef);
      const decodedOutput = decodeMidgardTxOutput(output);
      const outputIndex = Number(input.index());
      if (!Number.isSafeInteger(outputIndex)) {
        throw new Error("output index exceeds JavaScript safe integer range");
      }
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex,
        address: midgardOutputAddressText(decodedOutput),
        assets: valueToAssets(midgardValueToCmlValue(decodedOutput.value)) as Assets,
        ...(decodedOutput.datum === undefined
          ? {}
          : { datum: decodedOutput.datum.cbor.toString("hex") }),
      } satisfies UTxO;
    },
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message: "Failed to decode ledger UTxO for withdrawal classification",
        cause,
      }),
  });

const valuesEqual = (
  left: SDK.Value,
  right: SDK.Value,
): Effect.Effect<boolean, DatabaseError, never> =>
  Effect.try({
    try: () =>
      Buffer.from(LucidData.to(left, SDK.Value), "hex").equals(
        Buffer.from(LucidData.to(right, SDK.Value), "hex"),
      ),
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message: "Failed to compare withdrawal value CBOR",
        cause,
      }),
  });

const classifyWithdrawal = ({
  entry,
  ledgerOutRef,
  ledgerOutput,
}: {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly ledgerOutput: Option.Option<Buffer>;
}): Effect.Effect<ClassifiedWithdrawal, DatabaseError, never> =>
  Effect.gen(function* () {
    let validity: WithdrawalsDB.Validity;
    let validityDetail: unknown = {};

    if (Option.isNone(ledgerOutput)) {
      validity = WithdrawalsDB.Validity.NonExistentWithdrawalUtxo;
    } else {
      const utxo = yield* decodeLedgerUtxo({
        outRef: ledgerOutRef,
        output: ledgerOutput.value,
      });
      const paymentCredential = getAddressDetails(utxo.address).paymentCredential;
      if (
        paymentCredential == null ||
        paymentCredential.hash.toLowerCase() !==
          entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex").toLowerCase()
      ) {
        validity = WithdrawalsDB.Validity.IncorrectWithdrawalOwner;
      } else {
        const requestedValue = yield* Effect.try({
          try: () =>
            LucidData.from(
              entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
              SDK.Value,
            ) as SDK.Value,
          catch: (cause) =>
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message: "Failed to decode withdrawal l2_value",
              cause,
            }),
        });
        const actualValue = assetsToValue(utxo.assets);
        const valueMatches = yield* valuesEqual(requestedValue, actualValue);
        if (!valueMatches) {
          validity = WithdrawalsDB.Validity.IncorrectWithdrawalValue;
          validityDetail = {
            requested_value_cbor:
              entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
            actual_assets: Object.fromEntries(
              Object.entries(normalizeAssets(utxo.assets)).map(
                ([unit, quantity]) => [unit, quantity.toString()],
              ),
            ),
          };
        } else if (Object.keys(normalizeAssets(utxo.assets)).length > 100) {
          validity = WithdrawalsDB.Validity.TooManyTokensInWithdrawal;
        } else {
          validity = WithdrawalsDB.Validity.WithdrawalIsValid;
        }
      }
    }

    const settlementEventInfo = yield* encodeWithdrawalSettlementInfo(
      entry,
      validity,
      validityDetail,
    );
    return {
      entry,
      ledgerOutRef,
      validity,
      validityDetail,
      settlementEventInfo,
      shouldDeleteLedgerUtxo:
        validity === WithdrawalsDB.Validity.WithdrawalIsValid,
    };
  });

export const resolveIncludedDepositEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
}): Effect.Effect<readonly DepositsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime);
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: DepositsDB.tableName,
              message:
                "Refusing to build a block because one or more deposits due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) => entry[DepositsDB.Columns.ID].toString("hex"))
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] !== DepositsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected deposit UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime(),
        );
        let normalizedCurrentWindowEntries: readonly DepositsDB.Entry[] = [];
        if (currentWindowEntries.length > 0) {
          const awaitingEntries = currentWindowEntries.filter(
            (entry) =>
              entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
          );
          if (awaitingEntries.length > 0) {
            const mempoolEntries = yield* Effect.forEach(
              awaitingEntries,
              DepositsDB.toMempoolLedgerEntry,
            );
            yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
            yield* DepositsDB.markAwaitingAsProjected(
              awaitingEntries.map((entry) => entry[DepositsDB.Columns.ID]),
            );
          }

          normalizedCurrentWindowEntries = currentWindowEntries.map((entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting
              ? {
                  ...entry,
                  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
                }
              : entry,
          );
        }

        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      DepositsDB.tableName,
      "Failed to resolve deposits for the current block window",
    ),
  );

export const resolveIncludedWithdrawalEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
}): Effect.Effect<readonly WithdrawalsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* WithdrawalsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          );
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.STATUS] ===
            WithdrawalsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to build a block because one or more withdrawals due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) =>
                  entry[WithdrawalsDB.Columns.ID].toString("hex"),
                )
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.STATUS] !==
            WithdrawalsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected withdrawal UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime(),
        );

        return [...replayableOverdueEntries, ...currentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      WithdrawalsDB.tableName,
      "Failed to resolve withdrawals for the current block window",
    ),
  );

export const processMpts = (
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly Tx.EntryWithTimeStamp[],
  config?: ProcessMptsConfig,
): Effect.Effect<
  {
    utxoRoot: string;
    txRoot: string;
    mempoolTxHashes: Buffer[];
    processedMempoolTxs: readonly Tx.EntryWithTimeStamp[];
    sizeOfProcessedTxs: number;
    rejectedMempoolTxsCount: number;
    includedDepositEntriesCount: number;
    includedDepositEntries: readonly DepositsDB.Entry[];
    includedDepositEventIds: readonly Buffer[];
    includedWithdrawalEntriesCount: number;
    includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
    includedWithdrawalEventIds: readonly Buffer[];
  },
  MptError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const processedMempoolTxs: Tx.EntryWithTimeStamp[] = [];
    const rejectedTxHashes: Buffer[] = [];
    const rejectionEntries: TxRejectionsDB.EntryNoTimestamp[] = [];
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    const decodedMempoolTxs: DecodedMempoolTxForCommit[] = [];
    let sizeOfProcessedTxs = 0;
    const txDeltasByTxHash = yield* MempoolTxDeltasDB.retrieveByTxIds(
      mempoolTxs.map((entry) => entry[Tx.Columns.TX_ID]),
    );
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(mempoolTxs, (entry: Tx.EntryWithTimeStamp) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        const txHashHex = txHash.toString("hex");
        const existingDelta = txDeltasByTxHash.get(txHashHex);
        const resolved = yield* resolveTxDeltaForCommit(
          entry,
          existingDelta,
        ).pipe(Effect.withSpan("resolveTxDeltaForCommit"));
        if (resolved._tag === "Rejected") {
          rejectedTxHashes.push(Buffer.from(txHash));
          rejectionEntries.push(resolved.rejection);
          yield* Effect.logWarning(
            `Skipping malformed mempool tx ${txHashHex}: ${resolved.rejection[TxRejectionsDB.Columns.REJECT_DETAIL]}`,
          );
          return;
        }
        const { spent, produced } = resolved;
        decodedMempoolTxs.push({
          entry,
          txHash,
          txCbor,
          spent,
          produced,
        });
      }),
    );

    const effectiveEndTime =
      decodedMempoolTxs[0]?.entry[Tx.Columns.TIMESTAMPTZ] ??
      config?.processedOnlyEndTime ??
      config?.depositOnlyEndTime;

    if (
      effectiveEndTime !== undefined &&
      config?.depositVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.depositVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DepositsDB.tableName,
          message:
            "Refusing to build a block because deposit ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},deposit_visibility_barrier_time=${config.depositVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    if (
      effectiveEndTime !== undefined &&
      config?.withdrawalVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() >
        config.withdrawalVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Refusing to build a block because withdrawal ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},withdrawal_visibility_barrier_time=${config.withdrawalVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    let includedDepositEntries: readonly DepositsDB.Entry[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedDepositEntries = yield* resolveIncludedDepositEntriesForWindow({
        currentBlockStartTime: config.currentBlockStartTime,
        effectiveEndTime,
      });
    }
    const includedDepositEntriesCount = includedDepositEntries.length;
    const includedDepositEventIds = includedDepositEntries.map((entry) =>
      Buffer.from(entry[DepositsDB.Columns.ID]),
    );
    const depositLedgerEntries = yield* Effect.forEach(
      includedDepositEntries,
      DepositsDB.toLedgerEntry,
    );
    const sameBlockDepositOutputsByOutRef = new Map(
      depositLedgerEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );

    let includedWithdrawalEntries: readonly WithdrawalsDB.Entry[] = [];
    let classifiedWithdrawals: readonly ClassifiedWithdrawal[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedWithdrawalEntries =
        yield* resolveIncludedWithdrawalEntriesForWindow({
          currentBlockStartTime: config.currentBlockStartTime,
          effectiveEndTime,
        });

      const seenWithdrawalTarget = new Map<string, Buffer>();
      const mutableClassifiedWithdrawals: ClassifiedWithdrawal[] = [];
      for (const entry of includedWithdrawalEntries) {
        const ledgerOutRef = yield* WithdrawalsDB.toLedgerOutRef(entry);
        const ledgerOutRefHex = ledgerOutRef.toString("hex");
        const priorWithdrawalEventId =
          seenWithdrawalTarget.get(ledgerOutRefHex);
        if (priorWithdrawalEventId !== undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to build a block because multiple withdrawals target the same L2 outref in one candidate window",
              cause: `l2_outref=${ledgerOutRefHex},first_event_id=${priorWithdrawalEventId.toString(
                "hex",
              )},duplicate_event_id=${entry[WithdrawalsDB.Columns.ID].toString(
                "hex",
              )}`,
            }),
          );
        }

        const sameBlockDepositOutput =
          sameBlockDepositOutputsByOutRef.get(ledgerOutRefHex);
        const trieLedgerOutput =
          sameBlockDepositOutput === undefined
            ? yield* ledgerTrie.get(ledgerOutRef)
            : Option.some(sameBlockDepositOutput);
        const classifiedWithdrawal = yield* classifyWithdrawal({
          entry,
          ledgerOutRef,
          ledgerOutput: trieLedgerOutput,
        });
        mutableClassifiedWithdrawals.push(classifiedWithdrawal);
        seenWithdrawalTarget.set(
          ledgerOutRefHex,
          entry[WithdrawalsDB.Columns.ID],
        );
      }
      classifiedWithdrawals = mutableClassifiedWithdrawals;

      yield* WithdrawalsDB.setSettlementInfoForEventIds(
        classifiedWithdrawals.map((classified) => ({
          eventId: classified.entry[WithdrawalsDB.Columns.ID],
          settlementEventInfo: classified.settlementEventInfo,
          validity: classified.validity,
          validityDetail: classified.validityDetail,
        })),
      );
      yield* WithdrawalsDB.markAwaitingAsProjected(
        classifiedWithdrawals.map(
          (classified) => classified.entry[WithdrawalsDB.Columns.ID],
        ),
      );

      const classifiedByEventId = new Map(
        classifiedWithdrawals.map(
          (classified) =>
            [
              classified.entry[WithdrawalsDB.Columns.ID].toString("hex"),
              classified,
            ] as const,
        ),
      );
      includedWithdrawalEntries = includedWithdrawalEntries.map((entry) => {
        const classified = classifiedByEventId.get(
          entry[WithdrawalsDB.Columns.ID].toString("hex"),
        );
        return classified === undefined
          ? entry
          : {
              ...entry,
              [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
                classified.settlementEventInfo,
              [WithdrawalsDB.Columns.VALIDITY]: classified.validity,
              [WithdrawalsDB.Columns.VALIDITY_DETAIL]:
                classified.validityDetail,
              [WithdrawalsDB.Columns.STATUS]:
                entry[WithdrawalsDB.Columns.STATUS] ===
                WithdrawalsDB.Status.Awaiting
                  ? WithdrawalsDB.Status.Projected
                  : entry[WithdrawalsDB.Columns.STATUS],
            };
      });
    }

    const withdrawalLedgerDelOps: ETH_UTILS.BatchDBOp[] = classifiedWithdrawals
      .filter((classified) => classified.shouldDeleteLedgerUtxo)
      .map((classified) => ({
        type: "del" as const,
        key: classified.ledgerOutRef,
      }));
    const withdrawnOutRefHexes = new Set(
      classifiedWithdrawals
        .filter((classified) => classified.shouldDeleteLedgerUtxo)
        .map((classified) => classified.ledgerOutRef.toString("hex")),
    );

    yield* Effect.forEach(decodedMempoolTxs, (decoded) =>
      Effect.gen(function* () {
        const txHashHex = decoded.txHash.toString("hex");
        const withdrawnOutRef = decoded.spent.find((outRef) =>
          withdrawnOutRefHexes.has(outRef.toString("hex")),
        );
        if (withdrawnOutRef !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]: `Transaction spends L2 outref ${withdrawnOutRef.toString(
              "hex",
            )}, which is consumed by a valid withdrawal in the same block window`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref consumed by a due withdrawal event.`,
          );
          return;
        }

        mempoolTxHashes.push(decoded.txHash);
        processedMempoolTxs.push(decoded.entry);
        sizeOfProcessedTxs += decoded.txCbor.length;
        const delOps: ETH_UTILS.BatchDBOp[] = decoded.spent.map((outRef) => ({
          type: "del",
          key: outRef,
        }));
        const putOps: ETH_UTILS.BatchDBOp[] = decoded.produced.map(
          (le: Ledger.MinimalEntry) => ({
            type: "put",
            key: le[Ledger.Columns.OUTREF],
            value: le[Ledger.Columns.OUTPUT],
          }),
        );
        mempoolBatchOps.push({
          type: "put",
          key: decoded.txHash,
          value: decoded.txCbor,
        });
        batchDBOps.push(...delOps);
        batchDBOps.push(...putOps);
      }),
    );

    if (depositLedgerEntries.length > 0) {
      yield* Effect.logInfo(
        `🔹 Applying ${depositLedgerEntries.length} projected deposit UTxO(s) to the block pre-state.`,
      );
      yield* Effect.sync(() =>
        batchDBOps.unshift(
          ...depositLedgerEntries.map((entry) => ({
            type: "put" as const,
            key: entry[Ledger.Columns.OUTREF],
            value: entry[Ledger.Columns.OUTPUT],
          })),
        ),
      );
    }

    if (withdrawalLedgerDelOps.length > 0) {
      yield* Effect.logInfo(
        `🔹 Applying ${withdrawalLedgerDelOps.length} valid withdrawal event(s) to the block UTxO state.`,
      );
      batchDBOps.push(...withdrawalLedgerDelOps);
    }

    if (rejectedTxHashes.length > 0) {
      yield* Effect.logWarning(
        `Dropping ${rejectedTxHashes.length} transaction(s) from MempoolDB`,
      );
      yield* Effect.all(
        [
          MempoolDB.clearTxs(rejectedTxHashes),
          TxRejectionsDB.insertMany(rejectionEntries),
        ],
        { concurrency: "unbounded" },
      );
    }

    yield* Effect.all(
      [mempoolTrie.batch(mempoolBatchOps), ledgerTrie.batch(batchDBOps)],
      { concurrency: "unbounded" },
    );

    const txRoot = yield* mempoolTrie.getRootHex();
    const utxoRoot = yield* ledgerTrie.getRootHex();

    yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

    const includedWithdrawalEntriesCount = includedWithdrawalEntries.length;
    const includedWithdrawalEventIds = includedWithdrawalEntries.map((entry) =>
      Buffer.from(entry[WithdrawalsDB.Columns.ID]),
    );

    return {
      utxoRoot,
      txRoot,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount: rejectedTxHashes.length,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
    };
  });

export const keyValueMptRoot = (
  keys: Buffer[],
  values: Buffer[],
): Effect.Effect<string, MptError, never> =>
  Effect.gen(function* () {
    const trie = yield* MidgardMpt.create("keyValueMPT");

    const ops: ETH_UTILS.BatchDBOp[] = yield* Effect.allSuccesses(
      keys.map((key: Buffer, i: number) =>
        Effect.gen(function* () {
          const op: ETH_UTILS.BatchDBOp = {
            type: "put",
            key: key,
            value: values[i], // Poor mans zip
          };
          return op;
        }),
      ),
    );

    yield* trie.batch(ops);
    return yield* trie.getRootHex();
  });

const toPhasTrieItem = (keyCbor: Buffer, valueCbor: Buffer) => ({
  key: Buffer.from(aikenSerialisedPlutusDataCbor(keyCbor.toString("hex")), "hex"),
  value: Buffer.from(
    aikenSerialisedPlutusDataCbor(valueCbor.toString("hex")),
    "hex",
  ),
});

export const keyValuePhasRoot = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Effect.Effect<string, MptError, never> =>
  Effect.tryPromise({
    try: async () => {
      if (keys.length !== values.length) {
        throw new Error(
          `Cannot build PHAS root for ${keys.length} keys and ${values.length} values`,
        );
      }
      if (keys.length === 0) {
        return SDK.EMPTY_MERKLE_TREE_ROOT;
      }
      const trie = await Trie.fromList(
        keys.map((key, index) => toPhasTrieItem(key, values[index]!)),
      );
      if (trie.hash === undefined || trie.hash === null) {
        throw new Error("PHAS trie returned an empty root for non-empty input");
      }
      return trie.hash.toString("hex");
    },
    catch: (e) => MptError.phasRoot(e),
  });

export const keyValuePhasProof = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
  key: Buffer,
): Effect.Effect<SDK.Proof, MptError, never> =>
  Effect.tryPromise({
    try: async () => {
      if (keys.length !== values.length) {
        throw new Error(
          `Cannot build PHAS proof for ${keys.length} keys and ${values.length} values`,
        );
      }
      if (keys.length === 0) {
        throw new Error("Cannot build a PHAS membership proof for an empty tree");
      }
      const trie = await Trie.fromList(
        keys.map((itemKey, index) => toPhasTrieItem(itemKey, values[index]!)),
      );
      const proof = await trie.prove(
        Buffer.from(aikenSerialisedPlutusDataCbor(key.toString("hex")), "hex"),
      );
      return LucidData.from(
        proof.toCBOR().toString("hex"),
        SDK.Proof as never,
      ) as SDK.Proof;
    },
    catch: (e) => MptError.phasRoot(e),
  });

export const withTrieTransaction = <A, E, R>(
  trie: MidgardMpt,
  eff: Effect.Effect<A, E, R>,
): Effect.Effect<void | A, E | MptError, R> =>
  Effect.gen(function* () {
    yield* trie.checkpoint();
    const res = yield* eff;
    yield* trie.commit();
    return res;
  }).pipe(
    Effect.catchAll((e) =>
      Effect.gen(function* () {
        yield* trie.revert();
        yield* Effect.fail(e);
      }),
    ),
  );

export class LevelDB {
  _leveldb: Level<string, Uint8Array>;
  _location?: string;

  constructor(leveldb: Level<string, Uint8Array>, location?: string) {
    this._leveldb = leveldb;
    this._location = location;
  }

  /**
   * Opens the underlying LevelDB database.
   */
  async open() {
    await this._leveldb.open();
  }

  /**
   * Closes the underlying LevelDB database.
   */
  async close() {
    await this._leveldb.close();
  }

  /**
   * Reads a value from the underlying LevelDB database.
   */
  async get(key: string) {
    return this._leveldb.get(key, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Writes a value into the underlying LevelDB database.
   */
  async put(key: string, val: Uint8Array) {
    await this._leveldb.put(key, val, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Deletes a value from the underlying LevelDB database.
   */
  async del(key: string) {
    await this._leveldb.del(key, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Executes a batch of LevelDB operations.
   */
  async batch(opStack: BatchDBOp<string, Uint8Array>[]) {
    await this._leveldb.batch(opStack, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Builds a lightweight copy of the LevelDB wrapper.
   */
  shallowCopy() {
    if (typeof this._location === "string") {
      return new LevelDB(
        new Level<string, Uint8Array>(this._location, LEVELDB_ENCODING_OPTS),
        this._location,
      );
    }
    return new LevelDB(this._leveldb);
  }

  /**
   * Returns the underlying LevelDB instance.
   */
  getDatabase() {
    return this._leveldb;
  }
}

export class MptError extends Data.TaggedError(
  "MptError",
)<SDK.GenericErrorFields> {
  /**
   * Builds an error for trie read failures.
   */
  static get(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred getting an entry from ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie insert/update failures.
   */
  static put(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred inserting a new entry in ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie batch-update failures.
   */
  static batch(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred during a batch operation on ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for Midgard PHAS user-event root/proof failures.
   */
  static phasRoot(cause: unknown) {
    return new MptError({
      message: "An error occurred building a Midgard PHAS root or proof",
      cause,
    });
  }

  /**
   * Builds an error for trie deletion failures.
   */
  static del(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred deleting an entry from ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie initialization failures.
   */
  static trieCreate(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred creating ${trie} trie: ${cause}`,
      cause,
    });
  }

  /**
   * Builds an error for trie commit failures.
   */
  static trieCommit(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred committing ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie close failures.
   */
  static trieClose(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred closing ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie rollback failures.
   */
  static trieRevert(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred reverting ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for missing trie-root state.
   */
  static rootNotSet(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred getting ${trie} trie root, the root is ${typeof cause}`,
      cause,
    });
  }
}

export class MidgardMpt {
  public readonly trie: ETH.MerklePatriciaTrie;
  public readonly EMPTY_TRIE_ROOT_HEX: string;
  public readonly trieName: string;
  public readonly databaseAndPath?: {
    database: LevelDB;
    databaseFilePath: string;
  };

  private constructor(
    trie: ETH.MerklePatriciaTrie,
    trieName: string,
    databaseAndPath?: { database: LevelDB; databaseFilePath: string },
  ) {
    this.trie = trie;
    this.trieName = trieName;
    this.databaseAndPath = databaseAndPath;
    this.EMPTY_TRIE_ROOT_HEX = toHex(trie.EMPTY_TRIE_ROOT);
  }

  /**
   * Create a Merkle Patricia Trie (MPT) with a LevelDB-backed database if
   * `levelDBFilePath` is provided, or an in-memory database otherwise.
   *
   * @param trieName - The name identifier for the trie instance.
   * @param levelDBFilePath - Optional file path for LevelDB persistence.
   * @returns An Effect that resolves to the created MidgardMpt instance or fails with MptError.
   */
  public static create(
    trieName: string,
    levelDBFilePath?: string,
  ): Effect.Effect<MidgardMpt, MptError> {
    return Effect.gen(function* () {
      let databaseAndPath:
        | { database: LevelDB; databaseFilePath: string }
        | undefined = undefined;
      let valueEncoding: ETH_UTILS.ValueEncoding | undefined = undefined;
      if (typeof levelDBFilePath === "string") {
        const level = new Level<string, Uint8Array>(
          levelDBFilePath,
          LEVELDB_ENCODING_OPTS,
        );
        const db = new LevelDB(level, levelDBFilePath);
        yield* Effect.tryPromise({
          try: () => db.open(),
          catch: (e) => MptError.trieCreate(trieName, e),
        });
        databaseAndPath = { database: db, databaseFilePath: levelDBFilePath };
        valueEncoding = LEVELDB_ENCODING_OPTS.valueEncoding;
      }
      const trie = yield* Effect.tryPromise({
        try: () =>
          ETH.createMPT({
            db: databaseAndPath?.database,
            useRootPersistence: Boolean(databaseAndPath),
            valueEncoding,
          }),
        catch: (e) => MptError.trieCreate(trieName, e),
      });
      return new MidgardMpt(trie, trieName, databaseAndPath);
    });
  }

  /**
   * Deletes the trie backing store when one exists.
   */
  public delete(): Effect.Effect<void, FileSystemError> {
    if (this.databaseAndPath) {
      return Effect.gen(this, function* () {
        yield* Effect.tryPromise({
          try: () => this.databaseAndPath!.database.close(),
          catch: (_e) => _e,
        }).pipe(Effect.catchAll(() => Effect.void));
        yield* deleteMpt(this.databaseAndPath!.databaseFilePath, this.trieName);
      });
    } else {
      return Effect.succeed(Effect.void);
    }
  }

  /**
   * Closes the trie backing store when one exists.
   */
  public close(): Effect.Effect<void, MptError> {
    if (this.databaseAndPath) {
      return Effect.tryPromise({
        try: () => this.databaseAndPath!.database.close(),
        catch: (e) => MptError.trieClose(this.trieName, e),
      });
    }
    return Effect.void;
  }

  /**
   * Executes a batch of LevelDB operations.
   */
  public batch(arg: ETH_UTILS.BatchDBOp[]): Effect.Effect<void, MptError> {
    const trieName = this.trieName;
    const trieBatch = this.trie.batch(arg);
    return Effect.tryPromise({
      try: () => trieBatch,
      catch: (e) => MptError.batch(trieName, e),
    });
  }

  /**
   * Reads one trie entry by raw key.
   */
  public get(key: Buffer): Effect.Effect<Option.Option<Buffer>, MptError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.get(key),
      catch: (e) => MptError.get(trieName, e),
    }).pipe(
      Effect.map((value) =>
        value === null || value === undefined
          ? Option.none()
          : Option.some(Buffer.from(value)),
      ),
    );
  }

  /**
   * Returns the current trie root as a normalized byte array.
   */
  public getRoot(): Effect.Effect<Uint8Array, MptError> {
    const trieName = this.trieName;
    const root = this.trie.root();
    return Effect.gen(function* () {
      if (root === undefined || root === null)
        return yield* Effect.fail(MptError.rootNotSet(trieName, root));
      // Normalize to pure Uint8Array for type consistency
      // trie.root() returns different constructor types depending on the source:
      //   - Fresh (computed): Uint8Array
      //   - Persisted (loaded from Level.js after some changes): Buffer
      return root instanceof Uint8Array && !Buffer.isBuffer(root)
        ? root
        : new Uint8Array(root.buffer, root.byteOffset, root.byteLength);
    });
  }

  /**
   * Returns the current trie root as hexadecimal text.
   */
  public getRootHex(): Effect.Effect<string, MptError> {
    return this.getRoot().pipe(Effect.map(toHex));
  }

  /**
   * Checks whether the trie root matches the empty-root constant.
   */
  public rootIsEmpty(): Effect.Effect<boolean, MptError> {
    const getRootHex = this.getRootHex();
    const emptyRootHex = toHex(this.trie.EMPTY_TRIE_ROOT);
    return Effect.gen(function* () {
      const rootHex = yield* getRootHex;
      return rootHex === emptyRootHex;
    });
  }

  /**
   * Creates a checkpoint on the trie stack.
   */
  public checkpoint(): Effect.Effect<void> {
    return Effect.sync(() => this.trie.checkpoint());
  }

  /**
   * Commits the latest trie checkpoint.
   */
  public commit(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.commit(),
      catch: (e) => MptError.trieCommit(this.trieName, e),
    });
  }

  /**
   * Reverts the latest trie checkpoint.
   */
  public revert(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.revert(),
      catch: (e) => MptError.trieRevert(this.trieName, e),
    });
  }

  /**
   * Returns the internal database statistics exposed by the trie backend.
   */
  public databaseStats() {
    return this.trie.database()._stats;
  }
}

export const emptyRootHexProgram: Effect.Effect<string, MptError> = Effect.gen(
  function* () {
    const tempMpt = yield* MidgardMpt.create("temp");
    return tempMpt.EMPTY_TRIE_ROOT_HEX;
  },
);
