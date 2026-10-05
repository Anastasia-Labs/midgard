import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  ConfirmedLedgerDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
  utxoToLedgerInsertMaterial,
} from "../mpf/index.js";
import { Database, NodeConfig } from "../services/index.js";
import { materializeConfirmedLedgerSnapshot } from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import { resolveVerifiedCommitBase } from "./commit-block-header.resolve-verified-commit-base.js";
import { type ResolvedCommitBaseLedgerEntries } from "./commit-block-header.select-authenticated-foreign-base-candidate.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
  WorkerInput,
} from "./utils/commit-block-header.js";

export const resolveCommitBaseLedgerEntries = ({
  availableConfirmedBlock,
  speculativeBase,
  nativeMpfRoot,
  requireEntries,
}: {
  readonly availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  readonly speculativeBase?: NonNullable<
    WorkerInput["data"]["speculativeBuild"]
  >["base"];
  readonly nativeMpfRoot: string;
  readonly requireEntries: boolean;
}): Effect.Effect<
  ResolvedCommitBaseLedgerEntries,
  unknown,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    if (speculativeBase !== undefined) {
      const currentLedgerRootHex = nativeMpfRoot;
      const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(speculativeBase.headerHash, "hex"),
      );
      if (Option.isNone(journal)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Refusing to speculate without the submitted base journal",
            cause: `base_header_hash=${speculativeBase.headerHash}`,
          }),
        );
      }
      if (
        journal.value[
          PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
        ] !== speculativeBase.utxosRoot
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Speculative base root does not match its submitted journal",
            cause: `base_header_hash=${speculativeBase.headerHash},input_root=${speculativeBase.utxosRoot},journal_root=${journal.value[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]}`,
          }),
        );
      }
      if (
        !requireEntries &&
        currentLedgerRootHex === speculativeBase.utxosRoot &&
        journal.value.utxoPayloadAggregate !== undefined
      ) {
        return {
          source: `speculative-parent:${speculativeBase.headerHash}`,
          root: speculativeBase.utxosRoot,
          utxoPayloadAggregate: journal.value.utxoPayloadAggregate,
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
      const snapshot = yield* materializeConfirmedLedgerSnapshot(journal.value);
      if (snapshot.root !== speculativeBase.utxosRoot) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Submitted journal post-state cannot reproduce the speculative base root",
            cause: `base_header_hash=${speculativeBase.headerHash},expected_root=${speculativeBase.utxosRoot},journal_root=${snapshot.root}`,
          }),
        );
      }
      return {
        source: `speculative-journal:${speculativeBase.headerHash}`,
        entries: snapshot.entries,
        root: snapshot.root,
        utxoPayloadAggregate:
          journal.value.utxoPayloadAggregate ??
          ledgerPayloadAggregateFromEntries(snapshot.entries),
      } satisfies ResolvedCommitBaseLedgerEntries;
    }
    if (availableConfirmedBlock !== "") {
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      if (latestBlock.datum.key === "Empty") {
        yield* Effect.logInfo(
          "🔹 Commit base state_queue tip is the confirmed-state root; using confirmed_ledger as base.",
        );
        const confirmedEntriesForGenesis = yield* ConfirmedLedgerDB.retrieve;
        if (confirmedEntriesForGenesis.length === 0) {
          const nodeConfig = yield* NodeConfig;
          const genesisEntries = yield* Effect.forEach(
            nodeConfig.GENESIS_UTXOS,
            (utxo) =>
              utxoToLedgerInsertMaterial(utxo).pipe(
                Effect.map(({ ledgerOp, outputCbor }) => ({
                  [Ledger.Columns.OUTREF]: ledgerOp.key,
                  [Ledger.Columns.OUTPUT]: outputCbor,
                })),
              ),
          );
          if (genesisEntries.length > 0) {
            const root =
              yield* computeLedgerMpfRootFromLedgerEntries(genesisEntries);
            yield* Effect.logInfo(
              `🔹 Commit base ledger snapshot resolved from configured genesis UTxOs (entries=${genesisEntries.length.toString()}).`,
            );
            return {
              source: "genesis",
              entries: genesisEntries,
              root,
              utxoPayloadAggregate:
                ledgerPayloadAggregateFromEntries(genesisEntries),
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
        }
      } else {
        const header = yield* SDK.getHeaderFromStateQueueDatum(
          latestBlock.datum,
        );
        const currentLedgerRootHex = nativeMpfRoot;
        const headerHash = yield* SDK.hashBlockHeader(header);
        const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(headerHash, "hex"),
        );

        if (Option.isSome(journal)) {
          // The tail is this node's block whose journal was abandoned by a
          // signed-intent replacement or a correction: local state does not
          // hold it (its members were reopened). Never build on it; a
          // replaced block that won its slot is revived first.
          if (
            journal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
              PendingBlockFinalizationsDB.Status.Abandoned &&
            journal.value[
              PendingBlockFinalizationsDB.Columns.CORRECTION_TRANSITION_DIGEST
            ] != null
          ) {
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Refusing to build on a state-queue tail whose local journal is abandoned; it must be revived or corrected first",
                cause: `header_hash=${headerHash}`,
              }),
            );
          }
          if (
            !requireEntries &&
            currentLedgerRootHex === header.utxosRoot &&
            journal.value.utxoPayloadAggregate !== undefined
          ) {
            return {
              source: "persistent-ledger-mpf",
              root: header.utxosRoot,
              utxoPayloadAggregate: journal.value.utxoPayloadAggregate,
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
          const snapshot = yield* materializeConfirmedLedgerSnapshot(
            journal.value,
          );
          if (snapshot.root !== header.utxosRoot) {
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Refusing to use pending-finalization journal as commit base because its UTxO snapshot root does not match the state-queue tip",
                cause: `header_hash=${headerHash},journal_root=${snapshot.root},state_queue_root=${header.utxosRoot}`,
              }),
            );
          }
          yield* Effect.logInfo(
            `🔹 Commit base ledger snapshot resolved from pending-finalization journal ${headerHash} (entries=${snapshot.entries.length.toString()}).`,
          );
          return {
            source: `pending-finalization:${headerHash}`,
            entries: snapshot.entries,
            root: snapshot.root,
            utxoPayloadAggregate:
              journal.value.utxoPayloadAggregate ??
              ledgerPayloadAggregateFromEntries(snapshot.entries),
          } satisfies ResolvedCommitBaseLedgerEntries;
        }
        // A root match cannot authenticate a foreign block. Preflight's
        // complete-prefix verifier is mandatory even for unchanged UTxO roots.
        const verified = yield* resolveVerifiedCommitBase(latestBlock);
        return {
          source: `verified-foreign:${verified.headerHash}`,
          entries: verified.entries,
          root: verified.root,
          utxoPayloadAggregate: ledgerPayloadAggregateFromEntries(
            verified.entries,
          ),
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
    }

    if (!requireEntries) {
      const currentLedgerRootHex = nativeMpfRoot;
      const aggregate =
        yield* MpfEngineStateDB.retrieveLedgerPayloadAggregate(
          currentLedgerRootHex,
        );
      if (aggregate !== undefined) {
        return {
          source: "persistent-ledger-mpf",
          root: currentLedgerRootHex,
          utxoPayloadAggregate: aggregate,
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
    }

    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    const confirmedRoot =
      yield* computeLedgerMpfRootFromLedgerEntries(confirmedEntries);
    yield* Effect.logInfo(
      `🔹 Commit base ledger snapshot resolved from confirmed_ledger (entries=${confirmedEntries.length.toString()}).`,
    );
    const utxoPayloadAggregate =
      ledgerPayloadAggregateFromEntries(confirmedEntries);
    yield* MpfEngineStateDB.stampLedgerPayloadAggregate({
      rootHex: confirmedRoot,
      aggregate: utxoPayloadAggregate,
    });
    return {
      source: "confirmed_ledger",
      entries: confirmedEntries,
      root: confirmedRoot,
      utxoPayloadAggregate,
    } satisfies ResolvedCommitBaseLedgerEntries;
  });
