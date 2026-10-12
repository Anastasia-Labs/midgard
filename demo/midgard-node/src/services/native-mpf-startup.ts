import { SqlClient } from "@effect/sql";
import { Effect, Exit, Option, Ref } from "effect";

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
import { withFollowerWrite } from "./follower-write-gate.js";
import type { Globals } from "./globals.js";
import { landedStateQueueSnapshot } from "./landed-state-queue.js";
import { Lucid } from "./lucid.js";
import { MidgardContracts } from "./midgard-contracts.js";
import { NativeMpfPromotionIndexCapExceeded } from "./mpf-native-owner/protocol.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";

/**
 * The node refuses to start the native owner from an unpinned binary: the
 * owner holds the ledger, so the operator must name the exact release build.
 * The refusal names where each supported run finds the pin, so the error alone
 * tells an operator what to set.
 */
export const NATIVE_OWNER_PIN_REMEDY =
  "The release image ships the pin as /app/native/architecture-g-owner.sha256; " +
  "read it with `docker compose run --rm --no-deps --entrypoint cat midgard-node /app/native/architecture-g-owner.sha256`. " +
  "For a host run, build the owner with `pnpm run native:mpf-owner:build` in demo/midgard-node, " +
  "set MPF_NATIVE_OWNER_BINARY_PATH to native/mpf-event-flat-wasm/target/release/architecture-g-owner " +
  "and pin its `sha256sum`.";

export const requirePinnedNativeOwnerBinary = (
  nodeConfig: Pick<
    NodeConfigDep,
    | "MPF_NATIVE_OWNER_BINARY_PATH"
    | "MPF_NATIVE_OWNER_BINARY_SHA256"
    | "MPF_NATIVE_OWNER_SIDECAR_PATH"
  >,
): Effect.Effect<void, Error> => {
  if (!/^[0-9a-f]{64}$/.test(nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256))
    return Effect.fail(
      new Error(
        `MPF_NATIVE_OWNER_BINARY_SHA256 must pin the native owner binary at ${nodeConfig.MPF_NATIVE_OWNER_BINARY_PATH} as 64 lowercase hex characters. ${NATIVE_OWNER_PIN_REMEDY}`,
      ),
    );
  if (nodeConfig.MPF_NATIVE_OWNER_BINARY_PATH.trim().length === 0)
    return Effect.fail(
      new Error(
        `MPF_NATIVE_OWNER_BINARY_PATH must name the native owner. ${NATIVE_OWNER_PIN_REMEDY}`,
      ),
    );
  if (nodeConfig.MPF_NATIVE_OWNER_SIDECAR_PATH.trim().length === 0)
    return Effect.fail(
      new Error("MPF_NATIVE_OWNER_SIDECAR_PATH must name the owner sidecar"),
    );
  return Effect.void;
};

export const initializeArchitectureGOwner = <R = never>(
  globals: Globals,
  nodeConfig: NodeConfigDep,
  /**
   * Re-checked around each SQL write when a driver recompute runs the
   * initialization: it fails once the recompute was superseded.
   */
  assertCurrent?: Effect.Effect<void, unknown>,
  beforeJournalReplay?: (
    owner: ProductionNativeMpfOwnerService,
  ) => Effect.Effect<void, unknown, R>,
): Effect.Effect<
  ProductionNativeMpfOwnerService,
  unknown,
  Database | Lucid | MidgardContracts | R
> =>
  Effect.gen(function* () {
    yield* requirePinnedNativeOwnerBinary(nodeConfig);

    const writeSql = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      withFollowerWrite(
        assertCurrent === undefined
          ? work
          : assertCurrent.pipe(
              Effect.zipRight(work),
              Effect.tap(() => assertCurrent),
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
        mode: "overlay",
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
          const contracts = yield* MidgardContracts;
          const snapshot = yield* landedStateQueueSnapshot(
            contracts.stateQueue,
            "startup",
          );
          if (snapshot.blockCount !== 0)
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
        } else if (!alreadyInitialized) {
          // An unstamped nonempty trie is trusted only at the committed tail
          // root. A store left from an earlier run must not be stamped onto a
          // fresh database, where it would first fail at the next commit.
          const contracts = yield* MidgardContracts;
          const snapshot = yield* landedStateQueueSnapshot(
            contracts.stateQueue,
            "startup",
          );
          const leftoverRoot = yield* bootstrap.rootHex();
          if (leftoverRoot !== snapshot.tailCommitBase.roots.utxosRoot)
            return yield* Effect.fail(
              new Error(
                `Stale native ledger store at LEDGER_MPF_DB_PATH: root=${leftoverRoot} differs from the committed state-queue root ${snapshot.tailCommitBase.roots.utxosRoot}; wipe it before starting on a fresh database`,
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
            if (beforeJournalReplay !== undefined)
              yield* beforeJournalReplay(owner);
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
                }).pipe(
                  // A replay the owner refuses over a full-index cap left it
                  // at its durable root: the owner starts there, holding the
                  // refusal (which readiness names), instead of failing
                  // startup into a crash loop. The journal stays active, and
                  // its next replay (local finalization) retries it.
                  Effect.catchIf(
                    (cause): cause is NativeMpfPromotionIndexCapExceeded =>
                      cause instanceof NativeMpfPromotionIndexCapExceeded,
                    (cause) =>
                      Effect.logWarning(
                        `${cause.message}. The native owner starts at its durable root and holds the journal's replay.`,
                      ),
                  ),
                );
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
