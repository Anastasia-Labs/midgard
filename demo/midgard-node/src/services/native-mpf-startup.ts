import { SqlClient } from "@effect/sql";
import { Effect, Exit, Option, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import {
  MempoolLedgerDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  MidgardMpf,
  utxoToLedgerInsertMaterial,
} from "../mpf/index.js";
import type { NodeConfigDep } from "./config.js";
import type { Database } from "./database.js";
import { withHistoryWrite } from "./event-history-producer.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import type { Globals } from "./globals.js";
import { Lucid } from "./lucid.js";
import { MidgardContracts } from "./midgard-contracts.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";
import { fetchStateQueueSnapshotProgram } from "./state-queue-topology.js";

export const initializeArchitectureGOwner = (
  globals: Globals,
  nodeConfig: NodeConfigDep,
  preparation?: HistoryRecoveryPreparation,
): Effect.Effect<
  ProductionNativeMpfOwnerService | undefined,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    if (nodeConfig.MPF_ENGINE !== "architecture_g") return undefined;

    const writeSql = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      preparation === undefined
        ? withHistoryWrite(work)
        : Authority.withRecovery(
            preparation.token,
            preparation.assertCurrent.pipe(
              Effect.zipRight(work),
              Effect.tap(() => preparation.assertCurrent),
            ),
          );
    const sql = yield* SqlClient.SqlClient;
    const initializedStores = yield* sql<{ root_hex: string | null }>`
      SELECT root_hex FROM mpf_engine_state
      WHERE store_name = 'ledger' AND migration_version >= 1 AND root_hex IS NOT NULL`;
    const alreadyInitialized = initializedStores.length === 1;
    const bootstrap = yield* MidgardMpf.create(
      "architecture-g-bootstrap",
      nodeConfig.LEDGER_MPF_DB_PATH,
      {
        engine: "overlay",
        spillThresholdBytes: nodeConfig.MPF_OVERLAY_SPILL_BYTES,
      },
    );
    const initialized = yield* Effect.either(
      Effect.gen(function* () {
        if (
          alreadyInitialized &&
          (yield* bootstrap.persistedRootMarker()) === undefined
        )
          return yield* Effect.fail(
            new Error(
              "Initialized native ledger is missing its durable root marker; recovery is required",
            ),
          );
        // An initialized trie may legitimately become empty after its last
        // withdrawal. Replaying configured genesis would resurrect spent funds.
        if (!alreadyInitialized && (yield* bootstrap.rootIsEmpty())) {
          const genesisEntries = yield* Effect.forEach(
            nodeConfig.GENESIS_UTXOS,
            (utxo) =>
              utxoToLedgerInsertMaterial(utxo).pipe(
                Effect.map(({ ledgerOp, outputCbor }) => ({
                  op: ledgerOp,
                  ledgerEntry: {
                    [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(
                      utxo.txHash,
                      "hex",
                    ),
                    [MempoolLedgerDB.Columns.OUTREF]: ledgerOp.key,
                    [MempoolLedgerDB.Columns.OUTPUT]: outputCbor,
                    [MempoolLedgerDB.Columns.ADDRESS]: utxo.address,
                    [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
                  } satisfies MempoolLedgerDB.EntryNoTimeStamp,
                })),
              ),
          );
          const lucid = yield* Lucid;
          const contracts = yield* MidgardContracts;
          const snapshot = yield* fetchStateQueueSnapshotProgram(
            lucid.api,
            contracts.stateQueue,
            "startup",
          );
          if (snapshot.topology.parsedNodeCount !== 1)
            return yield* Effect.fail(
              new Error(
                "Native genesis bootstrap requires the authenticated clean state queue",
              ),
            );
          // Empty genesis is valid only if it matches this deployment's actual
          // committed root. The same equality also protects nonempty genesis.
          const genesisRoot = yield* computeLedgerMpfRootFromLedgerEntries(
            genesisEntries.map(({ ledgerEntry }) => ledgerEntry),
          );
          if (genesisRoot !== snapshot.tailCommitBase.roots.utxosRoot)
            return yield* Effect.fail(
              new Error(
                "Configured native genesis does not match the committed state-queue root",
              ),
            );
          yield* bootstrap.applyBatch(genesisEntries.map(({ op }) => op));
          yield* writeSql(
            MempoolLedgerDB.insert(
              genesisEntries.map(({ ledgerEntry }) => ledgerEntry),
            ),
          );
        }
        const root = yield* bootstrap.rootHex();
        if (!alreadyInitialized)
          yield* writeSql(MpfEngineStateDB.stampLedgerMigration(root));
      }).pipe(
        Effect.ensuring(
          bootstrap.close().pipe(Effect.catchAll(() => Effect.void)),
        ),
      ),
    );
    if (initialized._tag === "Left")
      return yield* Effect.fail(initialized.left);

    return yield* Effect.uninterruptibleMask((restore) =>
      Effect.gen(function* () {
        const owner = yield* Effect.tryPromise({
          try: () =>
            ProductionNativeMpfOwnerService.create({
              levelPath: nodeConfig.LEDGER_MPF_DB_PATH,
              binaryPath: nodeConfig.MPF_NATIVE_OWNER_BINARY_PATH,
              binarySha256: nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
              maxFrameBytes: nodeConfig.MPF_NATIVE_OWNER_MAX_FRAME_BYTES,
              maxChunkBytes: nodeConfig.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES,
              requestTimeoutMs: nodeConfig.MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS,
              restartLimit: nodeConfig.MPF_NATIVE_OWNER_RESTART_LIMIT,
              sidecarPath: nodeConfig.MPF_NATIVE_OWNER_SIDECAR_PATH,
            }),
          catch: (cause) => cause,
        });
        yield* restore(
          Effect.gen(function* () {
            const active = yield* PendingBlockFinalizationsDB.retrieveActive();
            if (Option.isSome(active)) {
              const journal = active.value;
              const replay = journal.nativeMpfReplay;
              const submitted =
                journal[
                  PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
                ] !== null;
              const intended =
                journal[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] !=
                null;
              // A signed intent is possible submission, not accepted state.
              // Retain replay for canonical reconciliation; do not promote it.
              if ((submitted || intended) && replay === undefined) {
                return yield* Effect.fail(
                  new Error(
                    `Architecture G signed or submitted journal is missing replay data: header_hash=${journal[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex")}`,
                  ),
                );
              }
              const canonicallyObserved =
                journal[PendingBlockFinalizationsDB.Columns.STATUS] ===
                PendingBlockFinalizationsDB.Status.ObservedWaitingStability;
              if ((submitted || canonicallyObserved) && replay !== undefined) {
                yield* Effect.tryPromise({
                  try: () =>
                    owner.recover({
                      schema: 1,
                      ownerBinarySha256:
                        replay.ownerBinarySha256.toString("hex"),
                      baseRoot: replay.baseRoot.toString("hex"),
                      candidateRoot: replay.candidateRoot.toString("hex"),
                      eventLog: replay.eventLog,
                      eventLogDigest: replay.eventLogDigest.toString("hex"),
                      eventRoots: replay.eventRoots,
                      eventCount: replay.eventCount,
                    }),
                  catch: (cause) => cause,
                });
              }
            }
          }),
        ).pipe(
          Effect.onExit((exit) =>
            Exit.isFailure(exit)
              ? Effect.promise(() => owner.close())
              : Effect.void,
          ),
        );
        // Publish ownership before leaving the acquisition mask. Shutdown and a
        // superseded preparation can find the resource even if its caller stops.
        yield* Ref.set(globals.NATIVE_MPF_OWNER, owner);
        return owner;
      }),
    );
  });
