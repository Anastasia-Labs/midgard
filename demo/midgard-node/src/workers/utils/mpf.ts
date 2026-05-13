import { Proof, Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Data, Effect, Option } from "effect";
import { SqlClient } from "@effect/sql";
import { CML, Data as LucidData, valueToAssets } from "@lucid-evolution/lucid";
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
import { verifyWithdrawalSignature } from "@/withdrawal-signature.js";
import {
  decodeMidgardAddressText,
  decodeMidgardNativeTxFull,
  encodeMidgardNativeTxCompact,
} from "@/midgard-tx-codec/index.js";

const ROOT_KEY = "__root__";
const JSON_LEVEL_ENCODING_OPTS = { valueEncoding: "json" as const };

export const MPF_EMPTY_ROOT_HEX = SDK.EMPTY_MERKLE_TREE_ROOT;
const MPF_EMPTY_ROOT = Buffer.from(MPF_EMPTY_ROOT_HEX, "hex");
const MPF_INTERNAL_NULL_ROOT_HEX = "00".repeat(32);

export type MpfStoreName = "ledger" | "transactions" | "scratch";

export type MpfBatchOp =
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer }
  | { readonly type: "delete"; readonly key: Buffer };

type MpfStoredValue = string | Record<string, unknown>;

type LevelBatchOp =
  | {
      readonly type: "put";
      readonly key: string;
      readonly value: MpfStoredValue;
    }
  | { readonly type: "del"; readonly key: string };

const normalizeStoredRootHex = (rootHex: string): string =>
  rootHex === MPF_INTERNAL_NULL_ROOT_HEX ? MPF_EMPTY_ROOT_HEX : rootHex;

const parseStoredRootHex = (rootHex: unknown): Buffer => {
  if (rootHex === undefined) {
    return MPF_EMPTY_ROOT;
  }
  if (typeof rootHex !== "string") {
    throw new Error("Persisted MPF root marker is not a string");
  }
  if (!/^[0-9a-fA-F]{64}$/.test(rootHex)) {
    throw new Error("Persisted MPF root marker is not a 32-byte hex root");
  }
  if (rootHex === MPF_INTERNAL_NULL_ROOT_HEX) {
    throw new Error(
      "Persisted MPF root marker uses the library internal null root instead of the canonical Midgard empty root",
    );
  }
  return Buffer.from(rootHex, "hex");
};

const applyPendingBatch = (
  key: string,
  value: MpfStoredValue | undefined,
  ops: readonly LevelBatchOp[] | undefined,
): MpfStoredValue | undefined =>
  (ops ?? []).reduce<MpfStoredValue | undefined>((current, op) => {
    if (op.key !== key) {
      return current;
    }
    return op.type === "put" ? op.value : undefined;
  }, value);

const encodeTransactionRootValue = (txEnvelopeCbor: Buffer): Buffer =>
  encodeMidgardNativeTxCompact(decodeMidgardNativeTxFull(txEnvelopeCbor).compact);

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

export const makeMpfs: Effect.Effect<
  { ledgerMpf: MidgardMpf; transactionsMpf: MidgardMpf },
  MpfError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const transactionsMpf = yield* MidgardMpf.create(
    "transactions",
    nodeConfig.TRANSACTIONS_MPF_DB_PATH,
  );
  const ledgerMpf = yield* MidgardMpf.create(
    "ledger",
    nodeConfig.LEDGER_MPF_DB_PATH,
  );
  const ledgerRootIsEmpty = yield* ledgerMpf.rootIsEmpty();
  if (ledgerRootIsEmpty) {
    yield* Effect.logInfo(
      "🔹 No previous ledger MPF root found - inserting genesis utxos",
    );
    const ops: MpfBatchOp[] = yield* Effect.forEach(
      nodeConfig.GENESIS_UTXOS,
      (u: UTxO) =>
        utxoToInsertBatchOp(u).pipe(
          Effect.mapError((e) => MpfError.rootBuild("ledger genesis", e)),
        ),
    );
    yield* ledgerMpf.applyBatch(ops);
    const rootAfterGenesis = yield* ledgerMpf.rootHex();
    yield* Effect.logInfo(
      `🔹 New ledger MPF root after inserting genesis utxos: ${rootAfterGenesis}`,
    );
  }
  return {
    ledgerMpf,
    transactionsMpf,
  };
});

export const utxoToInsertBatchOp = (
  utxo: UTxO,
): Effect.Effect<MpfBatchOp, SDK.CmlDeserializationError> =>
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
    const op: MpfBatchOp = {
      type: "insert",
      key: Buffer.from(input.to_cbor_bytes()),
      value: output,
    };
    return op;
  });

export const deleteMpfStore = (
  path: string,
  name: string,
): Effect.Effect<void, FileSystemError> =>
  Effect.try({
    try: () => FS.rmSync(path, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: `Failed to delete ${name}'s MPF LevelDB store from disk`,
        cause: e,
      }),
  }).pipe(Effect.withLogSpan(`Delete ${name} MPF store`));

export const deleteTransactionsMpfStore = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMpfStore(config.TRANSACTIONS_MPF_DB_PATH, "transactions");
});

export const deleteLedgerMpfStore = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMpfStore(config.LEDGER_MPF_DB_PATH, "ledger");
});

export type ProcessMpfsConfig = {
  readonly currentBlockStartTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
  readonly withdrawalVisibilityBarrierTime?: Date;
};

export type DecodedMempoolTxForCommit = {
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
        assets: valueToAssets(
          midgardValueToCmlValue(decodedOutput.value),
        ) as Assets,
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
      const paymentCredential = decodeMidgardAddressText(
        utxo.address,
      ).paymentCredential;
      if (
        paymentCredential.hash.toString("hex").toLowerCase() !==
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
          const withdrawalInfo = yield* decodeWithdrawalInfo(entry);
          const verification = verifyWithdrawalSignature(
            withdrawalInfo.body,
            withdrawalInfo.signature,
            entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex"),
          );
          if (!verification.valid) {
            validity = WithdrawalsDB.Validity.IncorrectWithdrawalSignature;
            validityDetail = {
              reason: verification.reason,
              ...(verification.publicKeyHash === undefined
                ? {}
                : { public_key_hash: verification.publicKeyHash }),
            };
          } else {
            validity = WithdrawalsDB.Validity.WithdrawalIsValid;
          }
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

export const orderDecodedMempoolTxsForLedgerApplication = (
  decodedMempoolTxs: readonly DecodedMempoolTxForCommit[],
): Effect.Effect<
  readonly DecodedMempoolTxForCommit[],
  DatabaseError,
  never
> =>
  Effect.gen(function* () {
    if (decodedMempoolTxs.length <= 1) {
      return decodedMempoolTxs;
    }

    const txByHash = new Map<string, DecodedMempoolTxForCommit>();
    const originalIndexByTxHash = new Map<string, number>();
    const producerByOutRef = new Map<string, string>();

    for (const [index, decoded] of decodedMempoolTxs.entries()) {
      const txHashHex = decoded.txHash.toString("hex");
      if (txByHash.has(txHashHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Refusing to build a block because the mempool candidate contains duplicate transaction ids",
            cause: `tx_id=${txHashHex}`,
          }),
        );
      }
      txByHash.set(txHashHex, decoded);
      originalIndexByTxHash.set(txHashHex, index);

      for (const produced of decoded.produced) {
        const outRefHex = produced[Ledger.Columns.OUTREF].toString("hex");
        const priorProducer = producerByOutRef.get(outRefHex);
        if (priorProducer !== undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because multiple mempool transactions produce the same outref",
              cause: `outref=${outRefHex},first_tx_id=${priorProducer},duplicate_tx_id=${txHashHex}`,
            }),
          );
        }
        producerByOutRef.set(outRefHex, txHashHex);
      }
    }

    const dependenciesByTxHash = new Map<string, Set<string>>();
    const dependentsByTxHash = new Map<string, Set<string>>();
    for (const txHashHex of txByHash.keys()) {
      dependenciesByTxHash.set(txHashHex, new Set());
      dependentsByTxHash.set(txHashHex, new Set());
    }

    for (const decoded of decodedMempoolTxs) {
      const txHashHex = decoded.txHash.toString("hex");
      const dependencies = dependenciesByTxHash.get(txHashHex)!;
      const spentByThisTx = new Set<string>();

      for (const spent of decoded.spent) {
        const spentHex = spent.toString("hex");
        if (spentByThisTx.has(spentHex)) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends the same outref more than once",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }
        spentByThisTx.add(spentHex);

        const producerTxHash = producerByOutRef.get(spentHex);
        if (producerTxHash === undefined) {
          continue;
        }
        if (producerTxHash === txHashHex) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends an outref it also produces",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }

        dependencies.add(producerTxHash);
        dependentsByTxHash.get(producerTxHash)!.add(txHashHex);
      }
    }

    const byOriginalIndex = (left: string, right: string) =>
      originalIndexByTxHash.get(left)! - originalIndexByTxHash.get(right)!;
    const ready = [...dependenciesByTxHash.entries()]
      .filter(([, dependencies]) => dependencies.size === 0)
      .map(([txHashHex]) => txHashHex)
      .sort(byOriginalIndex);
    const queued = new Set(ready);
    const ordered: DecodedMempoolTxForCommit[] = [];

    while (ready.length > 0) {
      const txHashHex = ready.shift()!;
      ordered.push(txByHash.get(txHashHex)!);

      for (const dependentTxHash of dependentsByTxHash.get(txHashHex)!) {
        const dependencies = dependenciesByTxHash.get(dependentTxHash)!;
        dependencies.delete(txHashHex);
        if (dependencies.size === 0 && !queued.has(dependentTxHash)) {
          ready.push(dependentTxHash);
          queued.add(dependentTxHash);
          ready.sort(byOriginalIndex);
        }
      }
    }

    if (ordered.length !== decodedMempoolTxs.length) {
      const blockedTxIds = [...dependenciesByTxHash.entries()]
        .filter(([, dependencies]) => dependencies.size > 0)
        .map(([txHashHex, dependencies]) => ({
          tx_id: txHashHex,
          depends_on: [...dependencies].sort(),
        }));
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolDB.tableName,
          message:
            "Refusing to build a block because same-block mempool dependencies are cyclic",
          cause: JSON.stringify(blockedTxIds),
        }),
      );
    }

    return ordered;
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
                .map((entry) => entry[WithdrawalsDB.Columns.ID].toString("hex"))
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

export const processMpfs = (
  ledgerMpf: MidgardMpf,
  transactionsMpf: MidgardMpf,
  mempoolTxs: readonly Tx.EntryWithTimeStamp[],
  config?: ProcessMpfsConfig,
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
  MpfError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const processedMempoolTxs: Tx.EntryWithTimeStamp[] = [];
    const rejectedTxHashes: Buffer[] = [];
    const rejectionEntries: TxRejectionsDB.EntryNoTimestamp[] = [];
    const mempoolTxHashes: Buffer[] = [];
    const transactionOps: MpfBatchOp[] = [];
    const ledgerOps: MpfBatchOp[] = [];
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
        const mpfLedgerOutput =
          sameBlockDepositOutput === undefined
            ? yield* ledgerMpf.get(ledgerOutRef)
            : Option.some(sameBlockDepositOutput);
        const classifiedWithdrawal = yield* classifyWithdrawal({
          entry,
          ledgerOutRef,
          ledgerOutput: mpfLedgerOutput,
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

    const withdrawalLedgerDeleteOps: MpfBatchOp[] = classifiedWithdrawals
      .filter((classified) => classified.shouldDeleteLedgerUtxo)
      .map((classified) => ({
        type: "delete" as const,
        key: classified.ledgerOutRef,
      }));
    const withdrawnOutRefHexes = new Set(
      classifiedWithdrawals
        .filter((classified) => classified.shouldDeleteLedgerUtxo)
        .map((classified) => classified.ledgerOutRef.toString("hex")),
    );

    const orderedDecodedMempoolTxs =
      yield* orderDecodedMempoolTxsForLedgerApplication(decodedMempoolTxs);

    yield* Effect.forEach(orderedDecodedMempoolTxs, (decoded) =>
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
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${withdrawnOutRef.toString(
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
        const deleteOps: MpfBatchOp[] = decoded.spent.map((outRef) => ({
          type: "delete",
          key: outRef,
        }));
        const insertOps: MpfBatchOp[] = decoded.produced.map(
          (le: Ledger.MinimalEntry) => ({
            type: "insert",
            key: le[Ledger.Columns.OUTREF],
            value: le[Ledger.Columns.OUTPUT],
          }),
        );
        transactionOps.push({
          type: "insert",
          key: decoded.txHash,
          value: encodeTransactionRootValue(decoded.txCbor),
        });
        ledgerOps.push(...deleteOps);
        ledgerOps.push(...insertOps);
      }),
    );

    if (depositLedgerEntries.length > 0) {
      yield* Effect.logInfo(
        `🔹 Applying ${depositLedgerEntries.length} projected deposit UTxO(s) to the block pre-state.`,
      );
      yield* Effect.sync(() =>
        ledgerOps.unshift(
          ...depositLedgerEntries.map((entry) => ({
            type: "insert" as const,
            key: entry[Ledger.Columns.OUTREF],
            value: entry[Ledger.Columns.OUTPUT],
          })),
        ),
      );
    }

    if (withdrawalLedgerDeleteOps.length > 0) {
      yield* Effect.logInfo(
        `🔹 Applying ${withdrawalLedgerDeleteOps.length} valid withdrawal event(s) to the block UTxO state.`,
      );
      ledgerOps.push(...withdrawalLedgerDeleteOps);
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

    const transactionRootBeforeApply = yield* transactionsMpf.root();
    const ledgerRootBeforeApply = yield* ledgerMpf.root();
    yield* Effect.all(
      [
        transactionsMpf.applyBatch(transactionOps),
        ledgerMpf.applyBatch(ledgerOps),
      ],
      { concurrency: "unbounded" },
    ).pipe(
      Effect.catchAll((error) =>
        Effect.gen(function* () {
          yield* transactionsMpf
            .resetToRoot(transactionRootBeforeApply)
            .pipe(Effect.catchAll(() => Effect.void));
          yield* ledgerMpf
            .resetToRoot(ledgerRootBeforeApply)
            .pipe(Effect.catchAll(() => Effect.void));
          return yield* Effect.fail(error);
        }),
      ),
    );

    const txRoot = yield* transactionsMpf.rootHex();
    const utxoRoot = yield* ledgerMpf.rootHex();

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

export const keyValueMpfRoot = (
  keys: Buffer[],
  values: Buffer[],
): Effect.Effect<string, MpfError, never> =>
  Effect.gen(function* () {
    if (keys.length !== values.length) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "key-value",
          new Error(
            `Cannot build MPF root for ${keys.length} keys and ${values.length} values`,
          ),
        ),
      );
    }
    const mpf = yield* MidgardMpf.createScratch("key-value");
    yield* mpf.applyBatch(
      keys.map((key, index) => ({
        type: "insert" as const,
        key,
        value: values[index]!,
      })),
    );
    return yield* mpf.rootHex();
  });

const toPhasTrieItem = (keyCbor: Buffer, valueCbor: Buffer) => ({
  key: Buffer.from(
    aikenSerialisedPlutusDataCbor(keyCbor.toString("hex")),
    "hex",
  ),
  value: Buffer.from(
    aikenSerialisedPlutusDataCbor(valueCbor.toString("hex")),
    "hex",
  ),
});

export const keyValuePhasRoot = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Effect.Effect<string, MpfError, never> =>
  Effect.gen(function* () {
    if (keys.length !== values.length) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Cannot build PHAS root for ${keys.length} keys and ${values.length} values`,
          ),
        ),
      );
    }
    if (keys.length === 0) {
      return SDK.EMPTY_MERKLE_TREE_ROOT;
    }
    const mpf = yield* MidgardMpf.createScratch("phas-root");
    yield* mpf.applyBatch(
      keys.map((key, index) => {
        const item = toPhasTrieItem(key, values[index]!);
        return {
          type: "insert" as const,
          key: item.key,
          value: item.value,
        };
      }),
    );
    return yield* mpf.rootHex();
  });

export const keyValuePhasProof = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
  key: Buffer,
): Effect.Effect<SDK.Proof, MpfError, never> =>
  Effect.gen(function* () {
    if (keys.length !== values.length) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Cannot build PHAS proof for ${keys.length} keys and ${values.length} values`,
          ),
        ),
      );
    }
    if (keys.length === 0) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error("Cannot build a PHAS membership proof for an empty tree"),
        ),
      );
    }
    const mpf = yield* MidgardMpf.createScratch("phas-proof");
    yield* mpf.applyBatch(
      keys.map((itemKey, index) => {
        const item = toPhasTrieItem(itemKey, values[index]!);
        return {
          type: "insert" as const,
          key: item.key,
          value: item.value,
        };
      }),
    );
    const proof = yield* mpf.prove(
      Buffer.from(aikenSerialisedPlutusDataCbor(key.toString("hex")), "hex"),
    );
    return yield* Effect.try({
      try: () =>
        LucidData.from(
          proof.cbor.toString("hex"),
          SDK.Proof as never,
        ) as SDK.Proof,
      catch: (e) => MpfError.phasRoot(e),
    });
  });

export const withMpfRootTransaction = <A, E, R>(
  mpf: MidgardMpf,
  eff: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    const beforeRoot = yield* mpf.root();
    return yield* eff.pipe(
      Effect.catchAll((e) =>
        Effect.gen(function* () {
          yield* mpf.resetToRoot(beforeRoot);
          return yield* Effect.fail(e);
        }),
      ),
    );
  });

export type MpfProof = {
  readonly key: Buffer;
  readonly proof: Proof;
  readonly cbor: Buffer;
  readonly json: unknown;
  readonly aiken: string;
};

class MidgardMpfRootViewStore extends Store {
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;
  private readonly persistRootMarker: boolean;
  private currentRoot: Buffer;
  private batchOps: LevelBatchOp[] | undefined;

  constructor({
    level,
    memory,
    root,
    persistRootMarker,
  }: {
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
  }) {
    super(undefined);
    this.level = level;
    this.memory = memory;
    this.currentRoot = Buffer.from(root);
    this.persistRootMarker = persistRootMarker;
  }

  async ready() {
    await this.level?.open();
  }

  async batch(callback: () => Promise<unknown>) {
    if (this.batchOps !== undefined) {
      throw new Error("MPF store batch already ongoing");
    }
    const rootBefore = Buffer.from(this.currentRoot);
    this.batchOps = [];
    let result: unknown;
    try {
      result = await callback();
    } catch (error) {
      this.currentRoot = rootBefore;
      this.batchOps = undefined;
      throw error;
    }
    const ops = this.batchOps;
    this.batchOps = undefined;
    try {
      if (ops.length > 0) {
        if (this.level !== undefined) {
          await this.level.batch(ops, JSON_LEVEL_ENCODING_OPTS);
        } else {
          for (const op of ops) {
            if (op.type === "put") {
              this.memory!.set(op.key, op.value);
            } else {
              this.memory!.delete(op.key);
            }
          }
        }
      }
    } catch (error) {
      this.currentRoot = rootBefore;
      throw error;
    }
    return result;
  }

  async get(key: unknown, deserialise: (...args: any[]) => unknown) {
    if (key === ROOT_KEY) {
      return deserialise(key, this.currentRoot.toString("hex"), this);
    }
    const storageKey = this.storageKey(key);
    const storedValue =
      this.level === undefined
        ? this.memory!.get(storageKey)
        : await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
    const value = applyPendingBatch(storageKey, storedValue, this.batchOps);
    return deserialise(key, value, this);
  }

  async put(key: unknown, value: { serialise: () => MpfStoredValue }) {
    const storageKey = this.storageKey(key);
    const rawSerialized = value.serialise();
    const serialized =
      storageKey === ROOT_KEY && typeof rawSerialized === "string"
        ? normalizeStoredRootHex(rawSerialized)
        : rawSerialized;
    if (storageKey === ROOT_KEY) {
      this.currentRoot = Buffer.from(serialized as string, "hex");
      if (!this.persistRootMarker) {
        return;
      }
    }
    const op: LevelBatchOp = {
      type: "put",
      key: storageKey,
      value: serialized,
    };
    if (this.batchOps !== undefined) {
      this.batchOps.push(op);
    } else if (this.level !== undefined) {
      await this.level.put(op.key, op.value, JSON_LEVEL_ENCODING_OPTS);
    } else {
      this.memory!.set(op.key, op.value);
    }
  }

  async del(key: unknown) {
    const storageKey = this.storageKey(key);
    if (storageKey !== ROOT_KEY) {
      return;
    }
    if (!this.persistRootMarker) {
      return;
    }
    const op: LevelBatchOp = { type: "del", key: storageKey };
    if (this.batchOps !== undefined) {
      this.batchOps.push(op);
    } else if (this.level !== undefined) {
      await this.level.del(op.key);
    } else {
      this.memory!.delete(op.key);
    }
  }

  async size() {
    if (this.level !== undefined) {
      return this.level
        .keys()
        .all()
        .then((keys) => keys.length);
    }
    return this.memory!.size;
  }

  root() {
    return Buffer.from(this.currentRoot);
  }

  setRoot(root: Buffer) {
    this.currentRoot = Buffer.from(root);
  }

  private storageKey(key: unknown): string {
    if (key === null || key === undefined) {
      return MPF_INTERNAL_NULL_ROOT_HEX;
    }
    if (typeof key === "string") {
      return key;
    }
    if (Buffer.isBuffer(key)) {
      return key.toString("hex");
    }
    if (key instanceof Uint8Array) {
      return Buffer.from(key).toString("hex");
    }
    throw new Error(`Unsupported MPF store key type: ${typeof key}`);
  }
}

export class MpfError extends Data.TaggedError(
  "MpfError",
)<SDK.GenericErrorFields> {
  static get(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred getting an entry from ${trie} MPF`,
      cause,
    });
  }

  static insert(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred inserting a new entry in ${trie} MPF`,
      cause,
    });
  }

  static delete(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred deleting an entry from ${trie} MPF`,
      cause,
    });
  }

  static batch(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred during a batch operation on ${trie} MPF`,
      cause,
    });
  }

  static phasRoot(cause: unknown) {
    return new MpfError({
      message: "An error occurred building a Midgard PHAS root or proof",
      cause,
    });
  }

  static rootBuild(rootName: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred building ${rootName} MPF root`,
      cause,
    });
  }

  static create(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred creating ${trie} MPF`,
      cause,
    });
  }

  static close(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred closing ${trie} MPF store`,
      cause,
    });
  }

  static prove(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred proving a key in ${trie} MPF`,
      cause,
    });
  }

  static verify(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred verifying a proof for ${trie} MPF`,
      cause,
    });
  }

  static rootNotSet(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred getting ${trie} MPF root, the root is ${typeof cause}`,
      cause,
    });
  }
}

export class MidgardMpf {
  public readonly trie: Trie;
  public readonly trieName: string;
  private readonly store: MidgardMpfRootViewStore;
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;

  private constructor({
    trie,
    trieName,
    store,
    level,
    memory,
  }: {
    readonly trie: Trie;
    readonly trieName: string;
    readonly store: MidgardMpfRootViewStore;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
  }) {
    this.trie = trie;
    this.trieName = trieName;
    this.store = store;
    this.level = level;
    this.memory = memory;
  }

  public static create(
    trieName: string,
    levelDBFilePath?: string,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      if (levelDBFilePath === undefined) {
        return yield* MidgardMpf.createScratch(trieName);
      }
      const level = new Level<string, MpfStoredValue>(
        levelDBFilePath,
        JSON_LEVEL_ENCODING_OPTS,
      );
      yield* Effect.tryPromise({
        try: () => level?.open() ?? Promise.resolve(),
        catch: (e) => MpfError.create(trieName, e),
      });
      const root = yield* readPersistedRoot(level);
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: level !== undefined,
      });
    });
  }

  public static createScratch(
    trieName: string,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromMemory({
      trieName,
      root: MPF_EMPTY_ROOT,
      memory: new Map(),
    });
  }

  public static load(
    trieName: string,
    levelDBFilePath: string,
    root: Buffer,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const level = new Level<string, MpfStoredValue>(
        levelDBFilePath,
        JSON_LEVEL_ENCODING_OPTS,
      );
      yield* Effect.tryPromise({
        try: () => level.open(),
        catch: (e) => MpfError.create(trieName, e),
      });
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: false,
      });
    });
  }

  private static loadFromMemory({
    trieName,
    root,
    memory,
  }: {
    readonly trieName: string;
    readonly root: Buffer;
    readonly memory: Map<string, MpfStoredValue>;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root,
      memory,
      persistRootMarker: false,
    });
  }

  private static loadFromLevel({
    trieName,
    level,
    root,
    persistRootMarker,
  }: {
    readonly trieName: string;
    readonly level?: Level<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root,
      level,
      persistRootMarker,
    });
  }

  private static loadFromRootView({
    trieName,
    root,
    level,
    memory,
    persistRootMarker,
  }: {
    readonly trieName: string;
    readonly root: Buffer;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly persistRootMarker: boolean;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const store = new MidgardMpfRootViewStore({
        level,
        memory,
        root,
        persistRootMarker,
      });
      const trie = yield* Effect.tryPromise({
        try: async () =>
          root.equals(MPF_EMPTY_ROOT)
            ? new Trie(store)
            : await Trie.load(store),
        catch: (e) => MpfError.create(trieName, e),
      });
      return new MidgardMpf({ trie, trieName, store, level, memory });
    });
  }

  public root(): Effect.Effect<Buffer, MpfError> {
    const hash = this.trie.hash;
    return Effect.gen(this, function* () {
      if (hash === null || hash === undefined) {
        return Buffer.from(MPF_EMPTY_ROOT);
      }
      if (!Buffer.isBuffer(hash)) {
        return Buffer.from(hash);
      }
      return Buffer.from(hash);
    });
  }

  public rootHex(): Effect.Effect<string, MpfError> {
    return this.root().pipe(Effect.map((root) => root.toString("hex")));
  }

  public rootIsEmpty(): Effect.Effect<boolean, MpfError> {
    return this.root().pipe(Effect.map((root) => root.equals(MPF_EMPTY_ROOT)));
  }

  public get(key: Buffer): Effect.Effect<Option.Option<Buffer>, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.get(key),
      catch: (e) => MpfError.get(trieName, e),
    }).pipe(
      Effect.map((value) =>
        value === null || value === undefined
          ? Option.none()
          : Option.some(Buffer.from(value)),
      ),
    );
  }

  public insert(key: Buffer, value: Buffer): Effect.Effect<Buffer, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.insert(key, value),
      catch: (e) => MpfError.insert(trieName, e),
    }).pipe(Effect.andThen(() => this.root()));
  }

  public delete(key: Buffer): Effect.Effect<Buffer, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.delete(key),
      catch: (e) => MpfError.delete(trieName, e),
    }).pipe(Effect.andThen(() => this.root()));
  }

  public applyBatch(
    ops: readonly MpfBatchOp[],
  ): Effect.Effect<Buffer, MpfError> {
    return Effect.gen(this, function* () {
      const rootBefore = yield* this.root();
      yield* Effect.gen(this, function* () {
        for (const op of ops) {
          if (op.type === "insert") {
            yield* this.insert(op.key, op.value);
          } else {
            yield* this.delete(op.key);
          }
        }
      }).pipe(
        Effect.catchAll((error) =>
          this.resetToRoot(rootBefore).pipe(
            Effect.flatMap(() => Effect.fail(error)),
          ),
        ),
      );
      return yield* this.root();
    }).pipe(
      Effect.mapError((cause) =>
        cause instanceof MpfError
          ? cause
          : MpfError.batch(this.trieName, cause),
      ),
    );
  }

  public prove(key: Buffer): Effect.Effect<MpfProof, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.prove(key),
      catch: (e) => MpfError.prove(trieName, e),
    }).pipe(
      Effect.map((proof: Proof) => ({
        key: Buffer.from(key),
        proof,
        cbor: proof.toCBOR(),
        json: proof.toJSON(),
        aiken: proof.toAiken(),
      })),
    );
  }

  public verify(
    proof:
      | MpfProof
      | Proof
      | { readonly verify: (includingItem?: boolean) => Buffer },
    includingItem: boolean,
  ): Effect.Effect<Buffer, MpfError> {
    return Effect.try({
      try: () => {
        const proofObject = "proof" in proof ? proof.proof : proof;
        return Buffer.from(proofObject.verify(includingItem));
      },
      catch: (e) => MpfError.verify(this.trieName, e),
    });
  }

  public resetToRoot(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
      });
      Object.assign(this, reloaded);
      yield* this.persistRootMarker(root);
    });
  }

  public resetToEmpty(): Effect.Effect<void, MpfError> {
    return this.resetToRoot(MPF_EMPTY_ROOT);
  }

  public close(): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: () => this.level?.close() ?? Promise.resolve(),
      catch: (e) => MpfError.close(this.trieName, e),
    });
  }

  public diagnostics(): Effect.Effect<{ readonly entries: number }, MpfError> {
    return Effect.tryPromise({
      try: async () => ({ entries: await this.store.size() }),
      catch: (e) => MpfError.get(this.trieName, e),
    });
  }

  private persistRootMarker(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: async () => {
        this.store.setRoot(root);
        if (this.level !== undefined) {
          await this.level.put(
            ROOT_KEY,
            normalizeStoredRootHex(root.toString("hex")),
            JSON_LEVEL_ENCODING_OPTS,
          );
        }
      },
      catch: (e) => MpfError.create(this.trieName, e),
    });
  }
}

const readPersistedRoot = (
  level: Level<string, MpfStoredValue> | undefined,
): Effect.Effect<Buffer, MpfError> =>
  Effect.tryPromise({
    try: async () => {
      if (level === undefined) {
        return MPF_EMPTY_ROOT;
      }
      const rootHex = await level.get(ROOT_KEY, JSON_LEVEL_ENCODING_OPTS);
      return parseStoredRootHex(rootHex);
    },
    catch: (e) => MpfError.rootNotSet("persisted", e),
  });

export const openMpfStore = (
  storeName: MpfStoreName,
): Effect.Effect<MidgardMpf, MpfError, NodeConfig> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    if (storeName === "ledger") {
      return yield* MidgardMpf.create("ledger", config.LEDGER_MPF_DB_PATH);
    }
    if (storeName === "transactions") {
      return yield* MidgardMpf.create(
        "transactions",
        config.TRANSACTIONS_MPF_DB_PATH,
      );
    }
    return yield* MidgardMpf.createScratch("scratch");
  });

export const loadMpf = (
  storeName: MpfStoreName,
  root: Buffer,
): Effect.Effect<MidgardMpf, MpfError, NodeConfig> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    if (storeName === "ledger") {
      return yield* MidgardMpf.load("ledger", config.LEDGER_MPF_DB_PATH, root);
    }
    if (storeName === "transactions") {
      return yield* MidgardMpf.load(
        "transactions",
        config.TRANSACTIONS_MPF_DB_PATH,
        root,
      );
    }
    return yield* MidgardMpf.createScratch("scratch");
  });

export const emptyRootHexProgram: Effect.Effect<string, MpfError> =
  Effect.succeed(MPF_EMPTY_ROOT_HEX);
