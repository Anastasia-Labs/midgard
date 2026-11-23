import { LedgerRootMismatchError } from "@al-ft/midgard-fault-proofs/ledger-at";
import { SqlClient } from "@effect/sql";
import { Effect, Exit, Option, Ref } from "effect";

import {
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { readLedgerOutputs } from "../landed-blocks/ledger-outputs.js";
import {
  MPF_EMPTY_ROOT_HEX,
  parseStoredRootHex,
  ROOT_KEY,
} from "../mpf/store-primitives.js";
import type { NodeConfigDep } from "./config.js";
import type { Database } from "./database.js";
import { withFollowerWrite } from "./follower-write-gate.js";
import type { Globals } from "./globals.js";
import { landedStateQueueSnapshot } from "./landed-state-queue.js";
import { Lucid } from "./lucid.js";
import { MidgardContracts } from "./midgard-contracts.make-midgard-contract-runtime.js";
import { NativeMpfPromotionIndexCapExceeded } from "./mpf-native-owner/protocol.js";
import {
  type NativeOwnerLedger,
  openNativeOwnerLedger,
  writeGenesisLedger,
} from "./mpf-native-owner/service.open-ledger.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.production-native-mpf-owner-service.js";

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
    const stateQueueSnapshot = Effect.gen(function* () {
      const contracts = yield* MidgardContracts;
      return yield* landedStateQueueSnapshot(contracts.stateQueue, "startup");
    });
    const initializeLedger = ({ level, store }: NativeOwnerLedger) =>
      Effect.gen(function* () {
        const marker = yield* Effect.tryPromise({
          try: () => level.get(ROOT_KEY),
          catch: (cause) => cause,
        });
        if (alreadyInitialized) {
          if (marker === undefined)
            return yield* Effect.fail(
              new Error(
                "Initialized native ledger is missing its durable root marker; recovery is required",
              ),
            );
          return;
        }
        const storedRoot = yield* Effect.try({
          try: () => parseStoredRootHex(marker).toString("hex"),
          catch: (cause) => cause,
        });
        const snapshot = yield* stateQueueSnapshot;
        const tailRoot = snapshot.tailCommitBase.roots.utxosRoot;
        // Genesis is the empty ledger on every network: an unstamped empty
        // store is admitted only on a queue with no block, at its root.
        if (storedRoot === MPF_EMPTY_ROOT_HEX) {
          if (snapshot.blockCount !== 0)
            return yield* Effect.fail(
              new Error(
                "Native genesis bootstrap requires the authenticated clean state queue",
              ),
            );
          yield* Effect.tryPromise({
            try: () => writeGenesisLedger(store, [], tailRoot),
            catch: (cause) =>
              cause instanceof LedgerRootMismatchError
                ? new Error(
                    "The empty native genesis does not match the committed state-queue root",
                    { cause },
                  )
                : cause,
          });
        } else if (storedRoot !== tailRoot) {
          // An unstamped nonempty trie is trusted only at the committed tail
          // root. A store left from an earlier run must not be stamped onto a
          // fresh database, where it would first fail at the next commit.
          return yield* Effect.fail(
            new Error(
              `Stale native ledger store at LEDGER_MPF_DB_PATH: root=${storedRoot} differs from the committed state-queue root ${tailRoot}; wipe it before starting on a fresh database`,
            ),
          );
        }
        yield* writeSql(MpfEngineStateDB.stampLedgerMigration(tailRoot));
      });

    return yield* Effect.uninterruptibleMask((restore) =>
      Effect.gen(function* () {
        // The ledger's one LevelDB handle and store; the owner takes them over.
        const ledger = yield* Effect.tryPromise({
          try: () =>
            openNativeOwnerLedger(nodeConfig.LEDGER_MPF_DB_PATH, (outRefs) =>
              // Only the client: a reader built inside a transaction must not
              // keep that transaction's connection.
              Effect.runPromise(
                Effect.provideService(
                  readLedgerOutputs(outRefs),
                  SqlClient.SqlClient,
                  sql,
                ),
              ),
            ),
          catch: (cause) => cause,
        });
        yield* restore(initializeLedger(ledger)).pipe(
          Effect.onExit((exit) =>
            Exit.isFailure(exit)
              ? Effect.promise(() =>
                  ledger.level.close().catch(() => undefined),
                )
              : Effect.void,
          ),
        );
        const owner = yield* Effect.tryPromise({
          try: () =>
            ProductionNativeMpfOwnerService.create(ledger, {
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
            const live = yield* PendingBlockFinalizationsDB.retrieveLive();
            if (Option.isSome(live)) {
              const journal = live.value;
              const replay = journal.nativeMpfReplay;
              // A signed live journal's commit may land; the working ledger
              // holds its block on the landed tip (the rebase target), so the
              // owner recovers its candidate. An unsigned one was never
              // broadcast and is discarded by the next commit attempt.
              const signed =
                journal[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] !=
                null;
              if (signed && replay === undefined) {
                return yield* Effect.fail(
                  new Error(
                    `Architecture G signed journal is missing replay data: header_hash=${journal[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex")}`,
                  ),
                );
              }
              if (signed && replay !== undefined) {
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
                  // startup into a crash loop. The journal stays live, and
                  // the landed-block rebase retries its replay.
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
