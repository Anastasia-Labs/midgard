import { Proof, Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardTxOutput } from "@al-ft/lucid-midgard";
import { encodeMidgardNativeTxCompact } from "@al-ft/midgard-core/codec";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import { decodeMidgardTxCommitmentsFromCanonicalCbor } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import type { UTxO } from "@lucid-evolution/lucid";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { Data, Effect, Option } from "effect";
import * as FS from "fs";
import { Level } from "level";

import * as ConfirmedLedgerDB from "@/database/confirmedLedger.js";
import * as DepositsDB from "@/database/deposits.js";
import * as ForcedTransactionsDB from "@/database/forcedTransactions.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as MempoolTxDeltasDB from "@/database/mempoolTxDeltas.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as Tx from "@/database/utils/tx.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { Database, NodeConfig } from "@/services/index.js";
import { FileSystemError, findSpentAndProducedUTxOs } from "@/utils.js";

import { keyValuePhasRoot, keyValuePhasRootWithCount } from "./mpf/phas.js";
import {
  type ClassifiedWithdrawal,
  classifyWithdrawal,
} from "./mpf/withdrawal-classification.js";

export {
  canonicalizeKeyValuePhasEntries,
  type KeyValuePhasEntry,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  type KeyValuePhasRoot,
  keyValuePhasRoot,
  keyValuePhasRootWithCount,
  rootFromPhasProof,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "./mpf/phas.js";

const ROOT_KEY = "__root__";
const JSON_LEVEL_ENCODING_OPTS = { valueEncoding: "json" as const };

export const MPF_EMPTY_ROOT_HEX = SDK.EMPTY_MERKLE_TREE_ROOT;
const MPF_EMPTY_ROOT = Buffer.from(MPF_EMPTY_ROOT_HEX, "hex");
const MPF_INTERNAL_NULL_ROOT_HEX = "00".repeat(32);

export type MpfBatchOp =
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer }
  | { readonly type: "delete"; readonly key: Buffer };

export type MpfInsertBatchOp = Extract<MpfBatchOp, { readonly type: "insert" }>;

type MpfStoredValue = string | Record<string, unknown>;

type LevelBatchOp =
  | {
      readonly type: "put";
      readonly key: string;
      readonly value: MpfStoredValue;
    }
  | { readonly type: "del"; readonly key: string };

const normalizeRootMarkerHex = (rootHex: string, fieldName: string): string =>
  normalizeHex(rootHex, { fieldName, byteLength: 32 });

const normalizeStoredRootHex = (rootHex: string): string => {
  const normalized = normalizeRootMarkerHex(rootHex, "MPF root marker");
  return normalized === MPF_INTERNAL_NULL_ROOT_HEX
    ? MPF_EMPTY_ROOT_HEX
    : normalized;
};

const parseStoredRootHex = (rootHex: unknown): Buffer => {
  if (rootHex === undefined) {
    return MPF_EMPTY_ROOT;
  }
  if (typeof rootHex !== "string") {
    throw new Error("Persisted MPF root marker is not a string");
  }
  const normalized = normalizeRootMarkerHex(
    rootHex,
    "Persisted MPF root marker",
  );
  if (normalized === MPF_INTERNAL_NULL_ROOT_HEX) {
    throw new Error(
      "Persisted MPF root marker uses the library internal null root instead of the canonical Midgard empty root",
    );
  }
  return Buffer.from(normalized, "hex");
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

export const encodeTransactionRootValue = (txCanonicalCbor: Buffer): Buffer =>
  encodeMidgardNativeTxCompact(
    decodeMidgardTxCommitmentsFromCanonicalCbor(txCanonicalCbor)
      .transactionCompact,
  );

export const COMMIT_REJECT_CODE_DECODE_FAILED = "E_COMMIT_CBOR_DESERIALIZATION";
export const COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT =
  "E_COMMIT_WITHDRAWN_REFERENCE_INPUT";
export const COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT =
  "E_COMMIT_SAME_BLOCK_DEPOSIT_INPUT";

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
  DatabaseError | MpfError,
  Database | NodeConfig
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
    const genesisEntries = yield* Effect.forEach(
      nodeConfig.GENESIS_UTXOS,
      (u: UTxO) =>
        utxoToInsertBatchOp(u).pipe(
          Effect.mapError((e) => MpfError.rootBuild("ledger genesis", e)),
          Effect.map((op) => ({
            op,
            ledgerEntry: {
              [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(u.txHash, "hex"),
              [MempoolLedgerDB.Columns.OUTREF]: op.key,
              [MempoolLedgerDB.Columns.OUTPUT]: op.value,
              [MempoolLedgerDB.Columns.ADDRESS]: u.address,
              [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
            } satisfies MempoolLedgerDB.EntryNoTimeStamp,
          })),
        ),
    );
    yield* MempoolLedgerDB.insert(
      genesisEntries.map(({ ledgerEntry }) => ledgerEntry),
    );
    const ops = genesisEntries.map(({ op }) => op);
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

export const ledgerEntryToInsertBatchOp = (
  entry: Ledger.MinimalEntry,
): MpfInsertBatchOp => ({
  type: "insert",
  key: Buffer.from(entry[Ledger.Columns.OUTREF]),
  value: Buffer.from(entry[Ledger.Columns.OUTPUT]),
});

export const computeLedgerMpfRootFromLedgerEntries = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  keyValuePhasRoot(
    entries.map((entry) => entry[Ledger.Columns.OUTREF]),
    entries.map((entry) => entry[Ledger.Columns.OUTPUT]),
  );

export const hydrateLedgerMpfFromLedgerEntries = (
  ledgerMpf: MidgardMpf,
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    yield* ledgerMpf.resetToEmpty();
    yield* ledgerMpf.applyBatch(entries.map(ledgerEntryToInsertBatchOp));
    return yield* ledgerMpf.rootHex();
  });

export const synchronizeCommitMpfStoresFromLedgerEntries = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<
  {
    readonly ledgerEntryCount: number;
    readonly ledgerRoot: string;
    readonly transactionsRoot: string;
  },
  MpfError,
  NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const ledgerMpf = yield* MidgardMpf.create(
      "ledger",
      nodeConfig.LEDGER_MPF_DB_PATH,
    );
    const transactionsMpf = yield* MidgardMpf.create(
      "transactions",
      nodeConfig.TRANSACTIONS_MPF_DB_PATH,
    );
    const closeMpfs = Effect.all(
      [
        ledgerMpf.close().pipe(Effect.catchAll(() => Effect.void)),
        transactionsMpf.close().pipe(Effect.catchAll(() => Effect.void)),
      ],
      { discard: true },
    );
    return yield* Effect.gen(function* () {
      const ledgerRoot = yield* hydrateLedgerMpfFromLedgerEntries(
        ledgerMpf,
        entries,
      );
      yield* transactionsMpf.resetToEmpty();
      const transactionsRoot = yield* transactionsMpf.rootHex();
      yield* Effect.logInfo(
        `Synchronized commit MPF stores from confirmed ledger: ledger_entries=${entries.length.toString()},ledger_root=${ledgerRoot},transactions_root=${transactionsRoot}`,
      );
      return {
        ledgerEntryCount: entries.length,
        ledgerRoot,
        transactionsRoot,
      };
    }).pipe(Effect.ensuring(closeMpfs));
  });

export const synchronizeCommitMpfStoresFromConfirmedLedger: Effect.Effect<
  {
    readonly ledgerEntryCount: number;
    readonly ledgerRoot: string;
    readonly transactionsRoot: string;
  },
  DatabaseError | MpfError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
  return yield* synchronizeCommitMpfStoresFromLedgerEntries(confirmedEntries);
});

export const utxoToInsertBatchOp = (
  utxo: UTxO,
): Effect.Effect<MpfInsertBatchOp, SDK.CmlDeserializationError> =>
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
    return {
      type: "insert",
      key: Buffer.from(input.to_cbor_bytes()),
      value: output,
    };
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

export type ProcessMpfsConfig = {
  readonly currentBlockStartTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
  readonly txOrderVisibilityBarrierTime?: Date;
  readonly withdrawalVisibilityBarrierTime?: Date;
  readonly initialLedgerEntries?: readonly Ledger.MinimalEntry[];
};

export type UtxoPayloadEntry = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

export type TransitionTraceSourceEvent = {
  readonly eventKey: SDK.EventKey;
  readonly phase: SDK.TransitionPhase;
  readonly ledgerOps: readonly MpfBatchOp[];
};

export type RetainedTransitionTraceMember = {
  readonly stepIndex: bigint;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.TransitionStep;
};

export type RetainedEventToStepMember = {
  readonly eventKey: SDK.EventKey;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.EventToStepValue;
};

export type TransitionTraceBuildResult = {
  readonly finalUtxosRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly totalEventCount: number;
  readonly transitionStepCount: number;
};

export type DecodedMempoolTxForCommit = {
  readonly entry: Tx.EntryWithTimeStamp;
  readonly txHash: Buffer;
  readonly txCbor: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.MinimalEntry[];
};

const outputReferenceFromCbor = (
  cbor: Buffer,
  label: string,
): Effect.Effect<SDK.OutputReference, MpfError> =>
  Effect.try({
    try: () =>
      LucidData.from(
        cbor.toString("hex"),
        SDK.OutputReference,
      ) as SDK.OutputReference,
    catch: (cause) =>
      MpfError.rootBuild(
        "transition trace event key",
        new Error(`Failed to decode ${label} as OutputReference CBOR`, {
          cause,
        }),
      ),
  });

const withdrawalTraceEventKey = (
  entry: WithdrawalsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[WithdrawalsDB.Columns.ID],
    "withdrawal event id",
  ).pipe(
    Effect.map((withdrawalId) => ({
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    })),
  );

const forcedTransactionTraceEventKey = (
  entry: ForcedTransactionsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
    "forced transaction tx_order_id",
  ).pipe(
    Effect.map((txOrderId) => ({
      ForcedTransactionEventKey: { tx_order_id: txOrderId },
    })),
  );

const depositTraceEventKey = (
  entry: DepositsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[DepositsDB.Columns.ID],
    "deposit event id",
  ).pipe(
    Effect.map((depositId) => ({
      DepositEventKey: { deposit_id: depositId },
    })),
  );

const l2TransactionTraceEventKey = (txHash: Buffer): SDK.EventKey => ({
  L2TransactionEventKey: { tx_id: txHash.toString("hex") },
});

export const orderDecodedMempoolTxsForLedgerApplication = (
  decodedMempoolTxs: readonly DecodedMempoolTxForCommit[],
): Effect.Effect<readonly DecodedMempoolTxForCommit[], DatabaseError, never> =>
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

const compareBufferHex = (left: Buffer, right: Buffer): number => {
  const leftHex = left.toString("hex");
  const rightHex = right.toString("hex");
  return leftHex < rightHex ? -1 : leftHex > rightHex ? 1 : 0;
};

const materializeUtxoPayloadEntries = (
  initialLedgerEntries: readonly Ledger.MinimalEntry[],
  ledgerOps: readonly MpfBatchOp[],
): readonly UtxoPayloadEntry[] => {
  const entries = new Map<string, UtxoPayloadEntry>();
  for (const entry of initialLedgerEntries) {
    entries.set(entry[Ledger.Columns.OUTREF].toString("hex"), {
      outref: Buffer.from(entry[Ledger.Columns.OUTREF]),
      output: Buffer.from(entry[Ledger.Columns.OUTPUT]),
    });
  }
  for (const op of ledgerOps) {
    const key = op.key.toString("hex");
    if (op.type === "delete") {
      entries.delete(key);
      continue;
    }
    entries.set(key, {
      outref: Buffer.from(op.key),
      output: Buffer.from(op.value),
    });
  }
  return [...entries.values()].sort((left, right) =>
    compareBufferHex(left.outref, right.outref),
  );
};

const computeUtxoPayloadRoot = (
  entries: readonly UtxoPayloadEntry[],
): Effect.Effect<string, MpfError> =>
  keyValuePhasRoot(
    entries.map((entry) => entry.outref),
    entries.map((entry) => entry.output),
  );

const encodePlutusData = <A>(
  value: A,
  schema: Parameters<typeof LucidData.Nullable>[0],
  label: string,
): Effect.Effect<Buffer, MpfError> =>
  Effect.try({
    try: () =>
      Buffer.from(LucidData.to(value as never, schema as never), "hex"),
    catch: (cause) =>
      MpfError.rootBuild(
        "transition trace",
        new Error(`Failed to encode ${label} as canonical Plutus data`, {
          cause,
        }),
      ),
  });

const countedRootFromEncodedEntries = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    const phas = yield* keyValuePhasRootWithCount(
      entries.map((entry) => entry.key),
      entries.map((entry) => entry.value),
    );
    return yield* SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.rootBuild(
          "count-bound transition commitment",
          new Error("Failed to commit count-bound root", { cause }),
        ),
      ),
    );
  });

export const buildTransactionsSourceRoot = (
  entries: readonly MpfInsertBatchOp[],
): Effect.Effect<string, MpfError> =>
  countedRootFromEncodedEntries(SDK.ROOT_DOMAINS.transactions, entries);

const eventKeyCbor = (
  eventKey: SDK.EventKey,
): Effect.Effect<Buffer, MpfError> =>
  encodePlutusData(eventKey, SDK.EventKeySchema, "transition event key");

const eventKeyFingerprint = (
  eventKey: SDK.EventKey,
): Effect.Effect<string, MpfError> =>
  eventKeyCbor(eventKey).pipe(Effect.map((encoded) => encoded.toString("hex")));

const assertUniqueTransitionSourceEvents = (
  sourceEvents: readonly TransitionTraceSourceEvent[],
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    const seen = new Set<string>();
    for (const [index, event] of sourceEvents.entries()) {
      const fingerprint = yield* eventKeyFingerprint(event.eventKey);
      if (seen.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Duplicate source event key at source index ${index.toString()}: ${fingerprint}`,
            ),
          ),
        );
      }
      seen.add(fingerprint);
    }
  });

const transitionPhaseRank = (phase: SDK.TransitionPhase): number => {
  switch (phase) {
    case "Withdrawal":
      return 0;
    case "ForcedTransaction":
      return 1;
    case "L2Transaction":
      return 2;
    case "Deposit":
      return 3;
  }
};

const assertCanonicalTransitionPhaseOrder = (
  sourceEvents: readonly TransitionTraceSourceEvent[],
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    let lastRank = -1;
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const rank = transitionPhaseRank(sourceEvent.phase);
      if (rank < lastRank) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Transition source events are not in canonical phase order at source index ${index.toString()}: phase=${sourceEvent.phase}`,
            ),
          ),
        );
      }
      lastRank = rank;
    }
  });

const materializeUtxoRootFromMap = (
  entries: ReadonlyMap<string, UtxoPayloadEntry>,
): Effect.Effect<string, MpfError> =>
  computeUtxoPayloadRoot([...entries.values()]);

const applyTraceLedgerOps = (
  workingUtxos: Map<string, UtxoPayloadEntry>,
  ops: readonly MpfBatchOp[],
  eventKeyDescription: string,
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    for (const op of ops) {
      const keyHex = op.key.toString("hex");
      if (op.type === "delete") {
        if (!workingUtxos.has(keyHex)) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "transition trace",
              new Error(
                `Transition event ${eventKeyDescription} deletes missing UTxO ${keyHex}`,
              ),
            ),
          );
        }
        workingUtxos.delete(keyHex);
        continue;
      }
      if (workingUtxos.has(keyHex)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Transition event ${eventKeyDescription} inserts duplicate UTxO ${keyHex}`,
            ),
          ),
        );
      }
      workingUtxos.set(keyHex, {
        outref: Buffer.from(op.key),
        output: Buffer.from(op.value),
      });
    }
  });

export const buildEventToStepMembersFromTrace = ({
  sourceEvents,
  transitionTraceMembers,
}: {
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
}): Effect.Effect<readonly RetainedEventToStepMember[], MpfError> =>
  Effect.gen(function* () {
    yield* assertUniqueTransitionSourceEvents(sourceEvents);
    if (sourceEvents.length !== transitionTraceMembers.length) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "event-to-step root",
          new Error(
            `Transition source event count does not match trace step count: source_events=${sourceEvents.length.toString()},trace_steps=${transitionTraceMembers.length.toString()}`,
          ),
        ),
      );
    }

    const sourceByKey = new Map<string, TransitionTraceSourceEvent>();
    for (const sourceEvent of sourceEvents) {
      sourceByKey.set(
        yield* eventKeyFingerprint(sourceEvent.eventKey),
        sourceEvent,
      );
    }

    const seenTraceEvents = new Set<string>();
    const members: RetainedEventToStepMember[] = [];
    for (const traceMember of transitionTraceMembers) {
      const step = traceMember.value;
      const fingerprint = yield* eventKeyFingerprint(step.event_key);
      const source = sourceByKey.get(fingerprint);
      if (source === undefined) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace step ${step.step_index.toString()} references an event key with no source-root member: ${fingerprint}`,
            ),
          ),
        );
      }
      if (seenTraceEvents.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace contains duplicate event key ${fingerprint}`,
            ),
          ),
        );
      }
      if (source.phase !== step.phase) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace step phase does not match source phase: step_index=${step.step_index.toString()},source_phase=${source.phase},step_phase=${step.phase}`,
            ),
          ),
        );
      }
      seenTraceEvents.add(fingerprint);
      const value: SDK.EventToStepValue = {
        step_index: step.step_index,
        phase: step.phase,
      };
      members.push({
        eventKey: step.event_key,
        keyCbor: yield* eventKeyCbor(step.event_key),
        valueCbor: yield* encodePlutusData(
          value,
          SDK.EventToStepValueSchema,
          "event-to-step value",
        ),
        value,
      });
    }

    if (seenTraceEvents.size !== sourceByKey.size) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "event-to-step root",
          new Error(
            `Event-to-step root omits source events: source_events=${sourceByKey.size.toString()},mapped_events=${seenTraceEvents.size.toString()}`,
          ),
        ),
      );
    }
    return members;
  });

export const buildTransitionTraceResult = ({
  initialUtxos,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly initialUtxos: readonly UtxoPayloadEntry[];
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<TransitionTraceBuildResult, MpfError> =>
  Effect.gen(function* () {
    const totalEventCount =
      withdrawalCount +
      forcedTransactionCount +
      l2TransactionCount +
      depositCount;
    if (
      expectedTotalEventCount !== undefined &&
      totalEventCount !== expectedTotalEventCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source count mismatch: expected=${expectedTotalEventCount.toString()},actual=${totalEventCount.toString()}`,
          ),
        ),
      );
    }
    if (sourceEvents.length !== totalEventCount) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source event array length does not match source counts: source_events=${sourceEvents.length.toString()},source_count_sum=${totalEventCount.toString()}`,
          ),
        ),
      );
    }
    yield* assertUniqueTransitionSourceEvents(sourceEvents);
    yield* assertCanonicalTransitionPhaseOrder(sourceEvents);

    const workingUtxos = new Map<string, UtxoPayloadEntry>();
    for (const entry of initialUtxos) {
      const keyHex = entry.outref.toString("hex");
      if (workingUtxos.has(keyHex)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Initial transition UTxO snapshot contains duplicate outref ${keyHex}`,
            ),
          ),
        );
      }
      workingUtxos.set(keyHex, {
        outref: Buffer.from(entry.outref),
        output: Buffer.from(entry.output),
      });
    }

    const transitionTraceMembers: RetainedTransitionTraceMember[] = [];
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const preUtxosRoot = yield* materializeUtxoRootFromMap(workingUtxos);
      const eventKeyDescription = yield* eventKeyFingerprint(
        sourceEvent.eventKey,
      );
      yield* applyTraceLedgerOps(
        workingUtxos,
        sourceEvent.ledgerOps,
        eventKeyDescription,
      );
      const postUtxosRoot = yield* materializeUtxoRootFromMap(workingUtxos);
      const value: SDK.TransitionStep = {
        schema_version: 1n,
        step_index: BigInt(index),
        event_key: sourceEvent.eventKey,
        phase: sourceEvent.phase,
        pre_utxos_root: preUtxosRoot,
        post_utxos_root: postUtxosRoot,
      };
      transitionTraceMembers.push({
        stepIndex: value.step_index,
        keyCbor: yield* encodePlutusData(
          value.step_index,
          LucidData.Integer(),
          "transition trace step index",
        ),
        valueCbor: yield* encodePlutusData(
          value,
          SDK.TransitionStepSchema,
          "transition trace step",
        ),
        value,
      });
    }

    const eventToStepMembers = yield* buildEventToStepMembersFromTrace({
      sourceEvents,
      transitionTraceMembers,
    });
    const transitionTraceRoot = yield* countedRootFromEncodedEntries(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTraceMembers.map((member) => ({
        key: member.keyCbor,
        value: member.valueCbor,
      })),
    );
    const eventToStepRoot = yield* countedRootFromEncodedEntries(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStepMembers.map((member) => ({
        key: member.keyCbor,
        value: member.valueCbor,
      })),
    );

    return {
      finalUtxosRoot: yield* materializeUtxoRootFromMap(workingUtxos),
      transitionTraceRoot,
      eventToStepRoot,
      transitionTraceMembers,
      eventToStepMembers,
      withdrawalCount,
      forcedTransactionCount,
      l2TransactionCount,
      depositCount,
      totalEventCount,
      transitionStepCount: transitionTraceMembers.length,
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
        if (currentWindowEntries.length <= 0) {
          return replayableOverdueEntries;
        }

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

        const normalizedCurrentWindowEntries = currentWindowEntries.map(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting
              ? {
                  ...entry,
                  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
                }
              : entry,
        );
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

export const resolveIncludedForcedTransactionEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
}): Effect.Effect<
  readonly ForcedTransactionsDB.Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          );
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: ForcedTransactionsDB.tableName,
              message:
                "Refusing to build a block because one or more tx-order events due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) =>
                  entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
                    "hex",
                  ),
                )
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] !==
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected tx-order event(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime(),
        );
        if (currentWindowEntries.length <= 0) {
          return replayableOverdueEntries;
        }

        const awaitingEntries = currentWindowEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (awaitingEntries.length > 0) {
          yield* ForcedTransactionsDB.markAwaitingAsProjected(
            awaitingEntries.map(
              (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
            ),
          );
        }

        const normalizedCurrentWindowEntries = currentWindowEntries.map(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting
              ? {
                  ...entry,
                  [ForcedTransactionsDB.Columns.STATUS]:
                    ForcedTransactionsDB.Status.Projected,
                }
              : entry,
        );
        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      ForcedTransactionsDB.tableName,
      "Failed to resolve forced transactions for the current block window",
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
    transitionTraceRoot: string;
    eventToStepRoot: string;
    transitionTraceMembers: readonly RetainedTransitionTraceMember[];
    eventToStepMembers: readonly RetainedEventToStepMember[];
    transitionStepCount: number;
    totalEventCount: number;
    utxoPayloadEntries: readonly UtxoPayloadEntry[];
    mempoolTxHashes: Buffer[];
    processedMempoolTxs: readonly Tx.EntryWithTimeStamp[];
    sizeOfProcessedTxs: number;
    rejectedMempoolTxsCount: number;
    includedDepositEntriesCount: number;
    includedDepositEntries: readonly DepositsDB.Entry[];
    includedDepositEventIds: readonly Buffer[];
    includedForcedTransactionEntriesCount: number;
    includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
    includedForcedTransactionEventIds: readonly Buffer[];
    includedWithdrawalEntriesCount: number;
    includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
    includedWithdrawalEventIds: readonly Buffer[];
    transitionTraceBuild: TransitionTraceBuildResult;
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
    const transactionSourceOps: MpfInsertBatchOp[] = [];
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

    if (
      effectiveEndTime !== undefined &&
      config?.txOrderVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.txOrderVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Refusing to build a block because tx-order ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},tx_order_visibility_barrier_time=${config.txOrderVisibilityBarrierTime.toISOString()}`,
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

    let includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[] =
      [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedForcedTransactionEntries =
        yield* resolveIncludedForcedTransactionEntriesForWindow({
          currentBlockStartTime: config.currentBlockStartTime,
          effectiveEndTime,
        });
    }
    const includedForcedTransactionEntriesCount =
      includedForcedTransactionEntries.length;
    const includedForcedTransactionEventIds =
      includedForcedTransactionEntries.map((entry) =>
        Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]),
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

        const mpfLedgerOutput = yield* ledgerMpf.get(ledgerOutRef);
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

      includedWithdrawalEntries = classifiedWithdrawals.map((classified) => ({
        ...classified.entry,
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
          classified.settlementEventInfo,
        [WithdrawalsDB.Columns.VALIDITY]: classified.validity,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: classified.validityDetail,
        [WithdrawalsDB.Columns.STATUS]:
          classified.entry[WithdrawalsDB.Columns.STATUS] ===
          WithdrawalsDB.Status.Awaiting
            ? WithdrawalsDB.Status.Projected
            : classified.entry[WithdrawalsDB.Columns.STATUS],
      }));
    }

    const validWithdrawalClassifications = classifiedWithdrawals.filter(
      (classified) => classified.shouldDeleteLedgerUtxo,
    );
    const withdrawnOutRefHexes = new Set(
      validWithdrawalClassifications.map((classified) =>
        classified.ledgerOutRef.toString("hex"),
      ),
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
        const sameBlockDepositInput = decoded.spent.find((outRef) =>
          sameBlockDepositOutputsByOutRef.has(outRef.toString("hex")),
        );
        if (sameBlockDepositInput !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${sameBlockDepositInput.toString(
                "hex",
              )}, which is produced by a deposit that executes later in the same block window`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref produced by a due deposit event that executes after L2 transactions.`,
          );
          return;
        }

        mempoolTxHashes.push(decoded.txHash);
        processedMempoolTxs.push(decoded.entry);
        sizeOfProcessedTxs += decoded.txCbor.length;
        const transactionInsertOp = {
          type: "insert",
          key: decoded.txHash,
          value: encodeTransactionRootValue(decoded.txCbor),
        } as const satisfies MpfInsertBatchOp;
        transactionOps.push(transactionInsertOp);
        transactionSourceOps.push(transactionInsertOp);
      }),
    );

    if (depositLedgerEntries.length > 0) {
      yield* Effect.logInfo(
        `🔹 Including ${depositLedgerEntries.length} projected deposit UTxO(s) in the deposit phase.`,
      );
    }

    if (validWithdrawalClassifications.length > 0) {
      yield* Effect.logInfo(
        `🔹 Including ${validWithdrawalClassifications.length} valid withdrawal event(s) in the withdrawal phase.`,
      );
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
    const initialLedgerEntries =
      config?.initialLedgerEntries ?? (yield* ConfirmedLedgerDB.retrieve);
    const initialUtxoPayloadEntries = materializeUtxoPayloadEntries(
      initialLedgerEntries,
      [],
    );
    const initialPayloadRoot = yield* computeUtxoPayloadRoot(
      initialUtxoPayloadEntries,
    );
    const ledgerRootBeforeApplyHex = ledgerRootBeforeApply.toString("hex");
    if (initialPayloadRoot !== ledgerRootBeforeApplyHex) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the transition trace base UTxO snapshot root does not match the ledger MPF root",
          cause: `payload_base_utxos_root=${initialPayloadRoot},ledger_mpf_root=${ledgerRootBeforeApplyHex}`,
        }),
      );
    }
    const withdrawalSourceEvents = yield* Effect.forEach(
      includedWithdrawalEntries,
      (entry) =>
        Effect.gen(function* () {
          const eventKey = yield* withdrawalTraceEventKey(entry);
          const valid = entry[WithdrawalsDB.Columns.VALIDITY];
          return {
            eventKey,
            phase: "Withdrawal" as const,
            ledgerOps:
              valid === WithdrawalsDB.Validity.WithdrawalIsValid
                ? [
                    {
                      type: "delete" as const,
                      key: yield* WithdrawalsDB.toLedgerOutRef(entry),
                    },
                  ]
                : [],
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const forcedTransactionSourceEvents = yield* Effect.forEach(
      includedForcedTransactionEntries,
      (entry) =>
        Effect.gen(function* () {
          if (
            entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY] ===
            "TxIsValid"
          ) {
            return yield* Effect.fail(
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message:
                  "Refusing to build a transition trace for an effectful forced transaction before forced transaction ledger deltas are available",
                cause: `tx_order_id=${entry[
                  ForcedTransactionsDB.Columns.TX_ORDER_ID
                ].toString("hex")}`,
              }),
            );
          }
          return {
            eventKey: yield* forcedTransactionTraceEventKey(entry),
            phase: "ForcedTransaction" as const,
            ledgerOps: [],
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const decodedByTxHash = new Map(
      orderedDecodedMempoolTxs.map((decoded) => [
        decoded.txHash.toString("hex"),
        decoded,
      ]),
    );
    const l2TransactionSourceEvents = yield* Effect.forEach(
      processedMempoolTxs,
      (entry, index) =>
        Effect.gen(function* () {
          const txHash = entry[Tx.Columns.TX_ID];
          const decoded = decodedByTxHash.get(txHash.toString("hex"));
          if (decoded === undefined) {
            return yield* Effect.fail(
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Refusing to build a transition trace because an included transaction is missing decoded ledger deltas",
                cause: `source_index=${index.toString()},tx_id=${txHash.toString(
                  "hex",
                )}`,
              }),
            );
          }
          return {
            eventKey: l2TransactionTraceEventKey(decoded.txHash),
            phase: "L2Transaction" as const,
            ledgerOps: [
              ...decoded.spent.map((outRef) => ({
                type: "delete" as const,
                key: outRef,
              })),
              ...decoded.produced.map((entry) => ({
                type: "insert" as const,
                key: entry[Ledger.Columns.OUTREF],
                value: entry[Ledger.Columns.OUTPUT],
              })),
            ],
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const depositSourceEvents = yield* Effect.forEach(
      includedDepositEntries,
      (entry) =>
        Effect.gen(function* () {
          const ledgerEntry = yield* DepositsDB.toLedgerEntry(entry);
          return {
            eventKey: yield* depositTraceEventKey(entry),
            phase: "Deposit" as const,
            ledgerOps: [
              {
                type: "insert" as const,
                key: ledgerEntry[Ledger.Columns.OUTREF],
                value: ledgerEntry[Ledger.Columns.OUTPUT],
              },
            ],
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const sourceEvents = [
      ...withdrawalSourceEvents,
      ...forcedTransactionSourceEvents,
      ...l2TransactionSourceEvents,
      ...depositSourceEvents,
    ];
    const transitionLedgerOps = sourceEvents.flatMap((event) =>
      event.ledgerOps.map((op) => ({ ...op })),
    );
    const transitionTraceBuild = yield* buildTransitionTraceResult({
      initialUtxos: initialUtxoPayloadEntries,
      sourceEvents,
      withdrawalCount: includedWithdrawalEntries.length,
      forcedTransactionCount: includedForcedTransactionEntries.length,
      l2TransactionCount: processedMempoolTxs.length,
      depositCount: includedDepositEntries.length,
    });
    const utxoPayloadEntries = materializeUtxoPayloadEntries(
      initialLedgerEntries,
      transitionLedgerOps,
    );
    yield* Effect.all(
      [
        transactionsMpf.applyBatch(transactionOps),
        ledgerMpf.applyBatch(transitionLedgerOps),
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

    const rawTxRoot = yield* transactionsMpf.rootHex();
    const txRoot = yield* buildTransactionsSourceRoot(transactionSourceOps);
    const utxoRoot = yield* ledgerMpf.rootHex();
    const payloadUtxoRoot = yield* computeUtxoPayloadRoot(utxoPayloadEntries);
    if (payloadUtxoRoot !== utxoRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the DA payload UTxO snapshot root does not match the computed ledger MPF root",
          cause: `payload_utxos_root=${payloadUtxoRoot},computed_utxos_root=${utxoRoot}`,
        }),
      );
    }
    if (transitionTraceBuild.finalUtxosRoot !== utxoRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the transition trace final UTxO root does not match the computed ledger MPF root",
          cause: `trace_final_utxos_root=${transitionTraceBuild.finalUtxosRoot},computed_utxos_root=${utxoRoot}`,
        }),
      );
    }

    yield* Effect.logInfo(
      `🔹 New raw transaction MPF root found: ${rawTxRoot}`,
    );
    yield* Effect.logInfo(`🔹 New transaction source root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
    yield* Effect.logInfo(
      `🔹 New transition trace root found: ${transitionTraceBuild.transitionTraceRoot}`,
    );
    yield* Effect.logInfo(
      `🔹 New event-to-step root found: ${transitionTraceBuild.eventToStepRoot}`,
    );

    const includedWithdrawalEntriesCount = includedWithdrawalEntries.length;
    const includedWithdrawalEventIds = includedWithdrawalEntries.map((entry) =>
      Buffer.from(entry[WithdrawalsDB.Columns.ID]),
    );

    return {
      utxoRoot,
      txRoot,
      transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
      eventToStepRoot: transitionTraceBuild.eventToStepRoot,
      transitionTraceMembers: transitionTraceBuild.transitionTraceMembers,
      eventToStepMembers: transitionTraceBuild.eventToStepMembers,
      transitionStepCount: transitionTraceBuild.transitionStepCount,
      totalEventCount: transitionTraceBuild.totalEventCount,
      utxoPayloadEntries,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount: rejectedTxHashes.length,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedForcedTransactionEntriesCount,
      includedForcedTransactionEntries,
      includedForcedTransactionEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
      transitionTraceBuild,
    };
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

export const withMpfRootTransactions = <A, E, R>(
  mpfs: readonly MidgardMpf[],
  eff: Effect.Effect<A, E, R>,
  shouldPreserveRoots: (value: A) => boolean,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    const beforeRoots = yield* Effect.forEach(mpfs, (mpf) => mpf.root(), {
      concurrency: "unbounded",
    });
    const resetRoots = Effect.forEach(
      mpfs,
      (mpf, index) => mpf.resetToRoot(beforeRoots[index]!),
      {
        discard: true,
        concurrency: "unbounded",
      },
    );

    const result = yield* Effect.either(eff);
    if (result._tag === "Left") {
      yield* resetRoots;
      return yield* Effect.fail(result.left);
    }
    if (!shouldPreserveRoots(result.right)) {
      yield* resetRoots;
    }
    return result.right;
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
    if (storageKey !== ROOT_KEY || !this.persistRootMarker) {
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
        try: () => level.open(),
        catch: (e) => MpfError.create(trieName, e),
      });
      const root = yield* readPersistedRoot(level);
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: true,
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
    return Effect.succeed(Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT));
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
      const rootAfter = yield* this.root();
      yield* this.persistRootMarker(rootAfter);
      return rootAfter;
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
        const verifiedRoot = proofObject.verify(includingItem);
        if (verifiedRoot === null || verifiedRoot === undefined) {
          return MPF_EMPTY_ROOT;
        }
        const normalizedRoot = Buffer.from(verifiedRoot);
        return normalizedRoot.equals(Buffer.alloc(32))
          ? MPF_EMPTY_ROOT
          : normalizedRoot;
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
  level: Level<string, MpfStoredValue>,
): Effect.Effect<Buffer, MpfError> =>
  Effect.tryPromise({
    try: async () => {
      const rootHex = await level.get(ROOT_KEY, JSON_LEVEL_ENCODING_OPTS);
      return parseStoredRootHex(rootHex);
    },
    catch: (e) => MpfError.rootNotSet("persisted", e),
  });

export const emptyRootHexProgram: Effect.Effect<string, MpfError> =
  Effect.succeed(MPF_EMPTY_ROOT_HEX);
