/**
 * Opening the MPF stores and hydrating the ledger trie from confirmed ledger entries.
 */

import { encodeMidgardTxOutput, outRefToCbor } from "@al-ft/lucid-midgard";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { validateMidgardConsensusV1TxCbor } from "@al-ft/midgard-core/consensus-validation-v1";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as FS from "fs";

import * as ConfirmedLedgerDB from "../database/confirmedLedger.js";
import * as MempoolLedgerDB from "../database/mempoolLedger.js";
import * as MpfEngineStateDB from "../database/mpfEngineState.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import { Database, NodeConfig } from "../services/index.js";
import { FileSystemError } from "../utils.js";
import { keyValuePhasRoot } from "../workers/utils/mpf/phas.js";
import { configureCommitMpfRuntime } from "./engine-config.js";
import { MpfError } from "./errors.js";
import {
  ledgerEntryToInsertBatchOp,
  ledgerOutputToInsertBatchOpV1,
} from "./ledger-delta.js";
import { MidgardMpf } from "./store.js";
import { type MpfInsertBatchOp } from "./types.js";

export const encodeTransactionRootValue = (
  txCanonicalCbor: Buffer,
  consensusProfile: MidgardConsensusProfileV1 = MIDGARD_CONSENSUS_PROFILE_V1,
): Buffer => {
  if (!isMidgardConsensusProfileV1(consensusProfile)) {
    throw new Error("Refusing transaction under a non-V1 consensus profile");
  }
  const violation = validateMidgardConsensusV1TxCbor(txCanonicalCbor);
  if (violation !== null) {
    throw new Error(
      `Refusing transaction outside the exact canonical V1 consensus profile: ${violation.code} ${violation.featureId} ${violation.detail}`,
    );
  }
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(txCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(txCanonicalCbor),
  );
  const value: SDK.L2TransactionSourceV1 = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return Buffer.from(
    aikenSerialisedPlutusDataCbor(
      LucidData.to(value, SDK.L2TransactionSourceV1),
    ),
    "hex",
  );
};

export const makeMpfs: Effect.Effect<
  { ledgerMpf: MidgardMpf; transactionsMpf: MidgardMpf },
  DatabaseError | MpfError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  if (nodeConfig.MPF_ENGINE === "architecture_g") {
    return yield* Effect.fail(
      MpfError.create(
        "architecture-g-owner",
        new Error(
          "Architecture G ledger ownership must be supplied by the main-process native owner; refusing to open its Level path in a commit worker",
        ),
      ),
    );
  }
  yield* configureCommitMpfRuntime(nodeConfig);
  const transactionsMpf = yield* MidgardMpf.create(
    "transactions",
    nodeConfig.TRANSACTIONS_MPF_DB_PATH,
    {
      engine: nodeConfig.MPF_ENGINE,
      spillThresholdBytes: nodeConfig.MPF_OVERLAY_SPILL_BYTES,
    },
  );
  const ledgerMpf = yield* MidgardMpf.create(
    "ledger",
    nodeConfig.LEDGER_MPF_DB_PATH,
    {
      engine: nodeConfig.MPF_ENGINE,
      spillThresholdBytes: nodeConfig.MPF_OVERLAY_SPILL_BYTES,
    },
  );
  const ledgerRootIsEmpty = yield* ledgerMpf.rootIsEmpty();
  if (ledgerRootIsEmpty) {
    yield* Effect.logInfo(
      "🔹 No previous ledger MPF root found - inserting genesis utxos",
    );
    const genesisEntries = yield* Effect.forEach(
      nodeConfig.GENESIS_UTXOS,
      (u: UTxO) =>
        utxoToLedgerInsertMaterialV1(u).pipe(
          Effect.mapError((e) => MpfError.rootBuild("ledger genesis", e)),
          Effect.map(({ ledgerOp, outputCbor }) => ({
            op: ledgerOp,
            ledgerEntry: {
              [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(u.txHash, "hex"),
              [MempoolLedgerDB.Columns.OUTREF]: ledgerOp.key,
              [MempoolLedgerDB.Columns.OUTPUT]: outputCbor,
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
  yield* MpfEngineStateDB.stampLedgerMigration(yield* ledgerMpf.rootHex());
  return {
    ledgerMpf,
    transactionsMpf,
  };
});

export const computeLedgerMpfRootFromLedgerEntries = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.try({
    try: () => entries.map(ledgerEntryToInsertBatchOp),
    catch: (cause) => MpfError.rootBuild("ledger descriptor root", cause),
  }).pipe(
    Effect.flatMap((ops) =>
      keyValuePhasRoot(
        ops.map((op) => op.key),
        ops.map((op) => op.value),
      ),
    ),
  );

export const hydrateLedgerMpfFromLedgerEntries = (
  ledgerMpf: MidgardMpf,
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    yield* ledgerMpf.resetToEmpty();
    const ops = yield* Effect.try({
      try: () => entries.map(ledgerEntryToInsertBatchOp),
      catch: (cause) =>
        MpfError.rootBuild("ledger descriptor hydration", cause),
    });
    yield* ledgerMpf.applyBatch(ops);
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
    if (nodeConfig.MPF_ENGINE === "architecture_g") {
      return yield* Effect.fail(
        MpfError.create(
          "architecture-g-owner",
          new Error(
            "Architecture G ledger ownership belongs to the live native owner; refusing to reopen its LevelDB path for persistent-store synchronization",
          ),
        ),
      );
    }
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

export const utxoToLedgerInsertMaterialV1 = (
  utxo: UTxO,
): Effect.Effect<
  {
    readonly ledgerOp: MpfInsertBatchOp;
    readonly outputCbor: Buffer;
  },
  SDK.CmlDeserializationError
> =>
  Effect.gen(function* () {
    // The MPF trie key is the §5.3 field-0/1 item encoding, byte-for-byte what
    // on-chain `ledger_outref_key` derives through `encode_midgard_tx_input`.
    // CML's minimal-index TransactionInput CBOR is 36 bytes for indices 0–23 and
    // would key the trie where the on-chain side never looks.
    const outRef = yield* Effect.try({
      try: () => outRefToCbor(utxo),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to encode UTxO outref as the §5.3 ledger key",
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
    const ledgerOp = yield* Effect.try({
      try: () => ledgerOutputToInsertBatchOpV1({ outRef, outputCbor: output }),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to derive the canonical V1 genesis descriptor",
          cause: e,
        }),
    });
    return { ledgerOp, outputCbor: output };
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
