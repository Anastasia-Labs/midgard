/**
 * MidgardMpf: the ledger, transaction, deposit, withdrawal, and forced-transaction tries and their overlay handles.
 */

import { Proof, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect, Option } from "effect";
import { Level } from "level";

import {
  type AuthenticatedPackedMpfRecord,
  authenticatePackedMpfRecord,
  EventFlatMutationArena,
  type EventFlatMutationDiagnostics,
  type PackedMpfStoredValue,
  type ParkedEventFlatOverlayV1,
  ResumedEventFlatOverlayV1,
} from "../workers/utils/mpf-event-flat.js";
import {
  eventFlatDigest,
  prepareEventFlatDigest,
} from "../workers/utils/mpf-event-flat-digest.js";
import {
  type MpfArenaCheckpointDiagnostics,
  type MpfEngine,
  type MpfPathHydrationDiagnostics,
  type MpfStoreDiagnostics,
  type ParkedMpfOverlayV1,
} from "./engine-config.js";
import { MpfError } from "./errors.js";
import { estimateMpfStoredValueBytes } from "./payload-size.js";
import { MidgardMpfRootViewStore } from "./root-view-store.js";
import {
  JSON_LEVEL_ENCODING_OPTS,
  type LevelBatchOp,
  MPF_EMPTY_ROOT,
  MPF_EMPTY_ROOT_HEX,
  normalizeStoredRootHex,
  parseStoredRootHex,
  ROOT_KEY,
} from "./store-primitives.js";
import {
  type MpfBatchOp,
  type MpfProof,
  type MpfSerializableValue,
  type MpfStoredValue,
} from "./types.js";

export class MidgardMpf {
  public readonly trie: Trie;
  public readonly trieName: string;
  private eventFlatArena?: EventFlatMutationArena;
  private readonly store: MidgardMpfRootViewStore;
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;
  private readonly engine: MpfEngine;
  private readonly spillThresholdBytes: number;
  private readonly parentStore?: MidgardMpfRootViewStore;
  private readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;

  private constructor({
    trie,
    trieName,
    store,
    level,
    memory,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
    parentStore,
    parentOverlay,
  }: {
    readonly trie: Trie;
    readonly trieName: string;
    readonly store: MidgardMpfRootViewStore;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
    readonly parentStore?: MidgardMpfRootViewStore;
    readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  }) {
    this.trie = trie;
    this.trieName = trieName;
    this.store = store;
    this.level = level;
    this.memory = memory;
    this.engine = engine;
    this.spillThresholdBytes = spillThresholdBytes;
    this.parentStore = parentStore;
    this.parentOverlay = parentOverlay;
  }

  public static create(
    trieName: string,
    levelDBFilePath?: string,
    options: {
      readonly engine?: MpfEngine;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      if (levelDBFilePath === undefined) {
        return yield* MidgardMpf.createScratch(trieName, options);
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
        engine: options.engine ?? "legacy",
        spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
      });
    });
  }

  public static createScratch(
    trieName: string,
    options: {
      readonly engine?: MpfEngine;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root: MPF_EMPTY_ROOT,
      memory: new Map(),
      persistRootMarker: false,
      engine: options.engine ?? "legacy",
      spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
    });
  }

  public static createScratchFromList(
    trieName: string,
    entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
    options: { readonly engine?: MpfEngine } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const memory = new Map<string, MpfStoredValue>();
      const store = new MidgardMpfRootViewStore({
        memory,
        root: MPF_EMPTY_ROOT,
        persistRootMarker: false,
        engine: options.engine ?? "legacy",
      });
      const trie = yield* Effect.tryPromise({
        try: () =>
          Trie.fromList(
            entries.map(({ key, value }) => ({
              key: Buffer.from(key),
              value: Buffer.from(value),
            })),
            store,
          ),
        catch: (e) => MpfError.create(trieName, e),
      });
      return new MidgardMpf({
        trie,
        trieName,
        store,
        memory,
        engine: options.engine ?? "legacy",
      });
    });
  }

  /** Builds a deterministic Level-backed fixture with `fromList`, then reopens
   * only its root. This is intentionally benchmark-only: production bootstrap
   * continues to use the audited confirmed-ledger migration path. */
  public static createLevelFromListForBenchmark(
    trieName: string,
    levelDBFilePath: string,
    entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
    options: {
      readonly engine?: MpfEngine;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const memory = new Map<string, MpfStoredValue>();
      const stagingStore = new MidgardMpfRootViewStore({
        memory,
        root: MPF_EMPTY_ROOT,
        persistRootMarker: false,
        engine: "legacy",
      });
      const trie = yield* Effect.tryPromise({
        try: () =>
          Trie.fromList(
            entries.map(({ key, value }) => ({
              key: Buffer.from(key),
              value: Buffer.from(value),
            })),
            stagingStore,
          ),
        catch: (cause) => MpfError.create(trieName, cause),
      });
      const root = Buffer.from(trie.hash ?? MPF_EMPTY_ROOT);
      const level = new Level<string, MpfStoredValue>(
        levelDBFilePath,
        JSON_LEVEL_ENCODING_OPTS,
      );
      yield* Effect.tryPromise({
        try: async () => {
          await level.open();
          let batch: Extract<LevelBatchOp, { readonly type: "put" }>[] = [];
          for (const [key, value] of memory) {
            batch.push({ type: "put", key, value });
            if (batch.length === 10_000) {
              await level.batch(batch, JSON_LEVEL_ENCODING_OPTS);
              batch = [];
            }
          }
          if (batch.length > 0) {
            await level.batch(batch, JSON_LEVEL_ENCODING_OPTS);
          }
          await level.put(
            ROOT_KEY,
            normalizeStoredRootHex(root.toString("hex")),
            JSON_LEVEL_ENCODING_OPTS,
          );
        },
        catch: (cause) => MpfError.create(trieName, cause),
      });
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: true,
        engine: options.engine ?? "overlay",
        spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
      });
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

  private static loadFromLevel({
    trieName,
    level,
    root,
    persistRootMarker,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
  }: {
    readonly trieName: string;
    readonly level?: Level<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root,
      level,
      persistRootMarker,
      engine,
      spillThresholdBytes,
    });
  }

  private static loadFromRootView({
    trieName,
    root,
    level,
    memory,
    persistRootMarker,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
    parentStore,
    parentOverlay,
  }: {
    readonly trieName: string;
    readonly root: Buffer;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly persistRootMarker: boolean;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
    readonly parentStore?: MidgardMpfRootViewStore;
    readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const store = new MidgardMpfRootViewStore({
        level,
        memory,
        root,
        persistRootMarker,
        engine,
        spillThresholdBytes,
        parentStore,
        parentOverlay,
      });
      const trie = yield* Effect.tryPromise({
        try: async () =>
          root.equals(MPF_EMPTY_ROOT)
            ? new Trie(store)
            : await Trie.load(store),
        catch: (e) => MpfError.create(trieName, e),
      });
      return new MidgardMpf({
        trie,
        trieName,
        store,
        level,
        memory,
        engine,
        spillThresholdBytes,
        parentStore,
        parentOverlay,
      });
    });
  }

  public root(): Effect.Effect<Buffer, MpfError> {
    return Effect.try({
      try: () => {
        this.store.root();
        return (
          this.eventFlatArena?.rootHash() ??
          Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT)
        );
      },
      catch: (cause) => MpfError.get(this.trieName, cause),
    });
  }

  public rootHex(): Effect.Effect<string, MpfError> {
    return this.root().pipe(Effect.map((root) => root.toString("hex")));
  }

  public persistedRootHex(): Effect.Effect<string, MpfError> {
    return this.level === undefined
      ? this.rootHex()
      : readPersistedRoot(this.level).pipe(
          Effect.map((root) => root.toString("hex")),
        );
  }

  public rootIsEmpty(): Effect.Effect<boolean, MpfError> {
    return this.root().pipe(Effect.map((root) => root.equals(MPF_EMPTY_ROOT)));
  }

  public get(key: Buffer): Effect.Effect<Option.Option<Buffer>, MpfError> {
    if (this.eventFlatArena !== undefined) {
      return Effect.try({
        try: () => this.eventFlatArena!.get(key),
        catch: (cause) => MpfError.get(this.trieName, cause),
      }).pipe(
        Effect.map((value) =>
          value === undefined ? Option.none() : Option.some(value),
        ),
      );
    }
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

  public insert(key: Buffer, value: Buffer): Effect.Effect<void, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.insert(key, value),
      catch: (e) => MpfError.insert(trieName, e),
    });
  }

  public delete(key: Buffer): Effect.Effect<void, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.delete(key),
      catch: (e) => MpfError.delete(trieName, e),
    });
  }

  public applyBatch(
    ops: readonly MpfBatchOp[],
  ): Effect.Effect<Buffer, MpfError> {
    if (
      this.store.overlayIsActive() &&
      this.engine === "event_flat" &&
      this.eventFlatArena !== undefined
    ) {
      return Effect.gen(this, function* () {
        const mutated = yield* Effect.either(
          Effect.try({
            try: () => this.eventFlatArena!.applyEvent(ops),
            catch: (cause) => MpfError.batch(this.trieName, cause),
          }),
        );
        if (mutated._tag === "Right") {
          this.store.putRetainedRoot(mutated.right);
          this.trie.hash = Buffer.from(mutated.right);
          return mutated.right;
        }
        const poisoned = yield* Effect.either(
          Effect.tryPromise({
            try: () => this.store.poisonOverlayAfterMutationFailure(),
            catch: (cause) => MpfError.batch(this.trieName, cause),
          }),
        );
        this.eventFlatArena = undefined;
        if (poisoned._tag === "Right") {
          this.trie.hash = Buffer.from(poisoned.right);
        }
        return yield* Effect.fail(mutated.left);
      });
    }
    if (this.store.overlayIsActive() && this.engine === "overlay") {
      return Effect.tryPromise({
        try: async () => {
          const deferred = this.store.beginDeferredMutation();
          try {
            for (const op of ops) {
              try {
                if (op.type === "insert") {
                  await this.trie.insert(op.key, op.value);
                } else {
                  await this.trie.delete(op.key);
                }
              } catch (cause) {
                throw op.type === "insert"
                  ? MpfError.insert(
                      this.trieName,
                      new Error(
                        `Failed to insert MPF key ${op.key.toString("hex")}`,
                        { cause },
                      ),
                    )
                  : MpfError.delete(
                      this.trieName,
                      new Error(
                        `Failed to delete MPF key ${op.key.toString("hex")}`,
                        { cause },
                      ),
                    );
              }
            }
            if (this.store.deferMidgardBranchHashes) {
              this.trie.finalizeMidgardEventMutation();
            }
            if (deferred) await this.store.commitDeferredMutation();
            return Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
          } catch (error) {
            this.store.abortDeferredMutation();
            try {
              const baseRoot =
                await this.store.poisonOverlayAfterMutationFailure();
              this.trie.hash = Buffer.from(baseRoot);
            } catch {
              // Preserve the original mutation error; recovery will reopen from
              // the unchanged durable marker.
            }
            throw error;
          }
        },
        catch: (cause) =>
          cause instanceof MpfError
            ? cause
            : MpfError.batch(this.trieName, cause),
      });
    }
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
      if (!this.store.overlayIsActive()) {
        yield* this.persistRootMarker(rootAfter);
      }
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

  /**
   * Resets the working trie view. While a block overlay is active this is a
   * logical reset only: the overlay's original rollback root remains intact
   * and callers must explicitly flush/promote after their journal or recovery
   * boundary. Non-overlay callers persist the root marker immediately for
   * standalone migration and recovery tooling.
   */
  public resetToRoot(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      if (this.store.overlayIsActive()) {
        const workingRoot = yield* Effect.tryPromise({
          try: () => this.store.resetOverlayRoot(root),
          catch: (e) => MpfError.batch(this.trieName, e),
        });
        const trie = yield* Effect.tryPromise({
          try: async () =>
            workingRoot.equals(MPF_EMPTY_ROOT)
              ? new Trie(this.store)
              : await Trie.load(this.store),
          catch: (e) => MpfError.create(this.trieName, e),
        });
        Object.assign(this, { trie });
        return;
      }
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.parentStore,
        parentOverlay: this.parentOverlay,
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
      try: async () => {
        await this.store.waitForSpills();
        if (this.store.shouldCloseLevel()) {
          await (this.level?.close() ?? Promise.resolve());
        }
      },
      catch: (e) => MpfError.close(this.trieName, e),
    });
  }

  public diagnostics(): Effect.Effect<MpfStoreDiagnostics, MpfError> {
    return Effect.tryPromise({
      try: async () => ({
        entries: await this.store.size(),
        ...this.store.diagnostics(),
      }),
      catch: (e) => MpfError.get(this.trieName, e),
    });
  }

  public prefetchTouchedPaths(
    touched: readonly (Buffer | MpfBatchOp)[],
    concurrency = 64,
  ): Effect.Effect<MpfPathHydrationDiagnostics, MpfError> {
    if (
      this.level === undefined ||
      !this.store.overlayIsActive() ||
      touched.length === 0
    ) {
      return Effect.succeed({
        prefetchMs: 0,
        uniquePaths: new Set(
          touched.map((item) =>
            Buffer.isBuffer(item)
              ? item.toString("hex")
              : item.key.toString("hex"),
          ),
        ).size,
        nodesRequested: 0,
        hydrationHits: 0,
        hydrationMisses: 0,
        loadedNodes: 0,
        maxInFlight: 0,
        maxBatchKeys: 0,
        maxFrontierPaths: 0,
        retainedBytesEstimate: 0,
        chunkCount: 0,
        checkpointMs: 0,
        authenticationMs: 0,
        materializeMs: 0,
        collapseMs: 0,
        checkpointSerializedNodes: 0,
        checkpointSerializedBytes: 0,
        verifiedUpperNodes: 0,
        retainedUpperNodes: 0,
        collapsedNodes: 0,
        peakDecodedNodes: 0,
      });
    }
    const trie = this.trie as Trie & {
      hydratePaths?: (
        touchedItems: readonly (Buffer | MpfBatchOp)[],
        options: {
          readonly concurrency: number;
          readonly nativeBatchSize: number;
        },
      ) => Promise<
        Omit<
          MpfPathHydrationDiagnostics,
          | "prefetchMs"
          | "chunkCount"
          | "checkpointMs"
          | "authenticationMs"
          | "materializeMs"
          | "collapseMs"
          | "checkpointSerializedNodes"
          | "checkpointSerializedBytes"
          | "verifiedUpperNodes"
          | "retainedUpperNodes"
          | "collapsedNodes"
          | "peakDecodedNodes"
        >
      >;
    };
    return Effect.tryPromise({
      try: async () => {
        if (trie.hydratePaths === undefined) {
          throw new Error(
            "Patched MPF trie does not expose bounded touched-path hydration",
          );
        }
        const levelWritesBefore = this.store.diagnostics().levelBatchWrites;
        const startedAt = performance.now();
        const result = await trie.hydratePaths(touched, {
          concurrency: Math.max(1, Math.min(256, Math.floor(concurrency))),
          nativeBatchSize: 4_096,
        });
        const prefetchMs = performance.now() - startedAt;
        if (this.store.diagnostics().levelBatchWrites !== levelWritesBefore) {
          throw new Error(
            "MPF touched-path hydration performed a durable write",
          );
        }
        return {
          prefetchMs,
          ...result,
          chunkCount: 1,
          checkpointMs: 0,
          authenticationMs: 0,
          materializeMs: 0,
          collapseMs: 0,
          checkpointSerializedNodes: 0,
          checkpointSerializedBytes: 0,
          verifiedUpperNodes: 0,
          retainedUpperNodes: 0,
          collapsedNodes: 0,
          peakDecodedNodes: result.loadedNodes,
        };
      },
      catch: (cause) => MpfError.get(`${this.trieName} touched paths`, cause),
    });
  }

  public primeBlockPathArena(
    touched: readonly (Buffer | MpfBatchOp)[],
    retainDepth = 2,
    collapseDecodedArena = true,
  ): Effect.Effect<
    {
      readonly hydration: MpfPathHydrationDiagnostics;
      readonly authenticationMs: number;
      readonly verifiedNodes: number;
      readonly checkpoint: MpfArenaCheckpointDiagnostics;
    },
    MpfError
  > {
    return Effect.gen(this, function* () {
      if (this.engine === "event_flat") {
        const rawPrimed = yield* Effect.either(
          this.primeEventFlatRawPaths(touched),
        );
        if (rawPrimed._tag === "Right") return rawPrimed.right;
        this.eventFlatArena = undefined;
        if (this.store.overlayIsActive()) {
          yield* Effect.promise(async () => {
            try {
              const baseRoot =
                await this.store.poisonOverlayAfterMutationFailure();
              this.trie.hash = Buffer.from(baseRoot);
            } catch {
              // Preserve the raw hydration failure; restart uses the marker.
            }
          });
        }
        return yield* Effect.fail(rawPrimed.left);
      }
      const primed = yield* Effect.either(
        Effect.gen(this, function* () {
          yield* Effect.try({
            try: () => this.store.enableBlockPathArena(!collapseDecodedArena),
            catch: (cause) => MpfError.get(this.trieName, cause),
          });
          const hydration = yield* this.prefetchTouchedPaths(touched);
          // Each fetched raw node is authenticated once in hydratePaths before
          // attachment. Verify the already-resident root here; chunk reads then
          // reuse the sealed content-hash authentication set.
          const authenticated = yield* this.authenticateDecodedArena(0);
          const checkpoint = yield* this.checkpointAndCollapseDecodedArena(
            retainDepth,
            false,
            collapseDecodedArena,
          );
          yield* Effect.try({
            try: () => this.store.sealBlockPathCache(),
            catch: (cause) => MpfError.get(this.trieName, cause),
          });
          return {
            hydration,
            authenticationMs: authenticated.authenticationMs,
            verifiedNodes: authenticated.verifiedNodes,
            checkpoint,
          };
        }),
      );
      if (primed._tag === "Right") {
        return primed.right;
      }
      this.eventFlatArena = undefined;
      if (this.store.overlayIsActive()) {
        yield* Effect.promise(async () => {
          try {
            const baseRoot =
              await this.store.poisonOverlayAfterMutationFailure();
            this.trie.hash = Buffer.from(baseRoot);
          } catch {
            // Preserve the prime failure; restart reads the durable marker.
          }
        });
      }
      return yield* Effect.fail(primed.left);
    });
  }

  private primeEventFlatRawPaths(
    touched: readonly (Buffer | MpfBatchOp)[],
  ): Effect.Effect<
    {
      readonly hydration: MpfPathHydrationDiagnostics;
      readonly authenticationMs: number;
      readonly verifiedNodes: number;
      readonly checkpoint: MpfArenaCheckpointDiagnostics;
    },
    MpfError
  > {
    return Effect.tryPromise({
      try: async () => {
        await prepareEventFlatDigest();
        const startedAt = performance.now();
        const root = this.store.root();
        const overlayBase = this.store.overlayStartingRoot();
        const trieRoot = Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
        if (
          overlayBase === undefined ||
          !root.equals(overlayBase) ||
          !root.equals(trieRoot)
        ) {
          throw new Error(
            `Event-flat raw hydration base mismatch: root=${root.toString("hex")},overlay=${overlayBase?.toString("hex") ?? "absent"},trie=${trieRoot.toString("hex")}`,
          );
        }
        type PathState = {
          readonly id: number;
          readonly path: string;
          readonly deletePath: boolean;
          readonly ix: number;
        };
        type RawNode = {
          readonly hash: Buffer;
          readonly value: PackedMpfStoredValue;
          readonly states: readonly PathState[];
        };
        const uniqueByPath = new Map<
          string,
          { readonly key: Buffer; deletePath: boolean }
        >();
        for (const item of touched) {
          const key = Buffer.isBuffer(item) ? item : item.key;
          const path = eventFlatDigest(key).toString("hex");
          const previous = uniqueByPath.get(path);
          uniqueByPath.set(path, {
            key,
            deletePath:
              (previous?.deletePath ?? false) ||
              (!Buffer.isBuffer(item) && item.type === "delete"),
          });
        }
        const states = [...uniqueByPath].map(
          ([path, state], id): PathState => ({
            id,
            path,
            deletePath: state.deletePath,
            ix: 0,
          }),
        );
        const authenticated = new Map<string, AuthenticatedPackedMpfRecord>();
        let retainedBytesEstimate = 0;
        let authenticationMs = 0;
        const authenticate = (
          hash: Buffer,
          value: PackedMpfStoredValue,
        ): AuthenticatedPackedMpfRecord => {
          const hashHex = hash.toString("hex");
          const existing = authenticated.get(hashHex);
          if (existing !== undefined) return existing;
          const authenticationStartedAt = performance.now();
          const branchMerkleNodes = authenticatePackedMpfRecord(hash, value);
          authenticationMs += performance.now() - authenticationStartedAt;
          const record = { hash, value, branchMerkleNodes };
          authenticated.set(hashHex, record);
          retainedBytesEstimate +=
            Buffer.byteLength(hashHex) + estimateMpfStoredValueBytes(value);
          this.store.assertEventFlatRawCaps(
            authenticated.size,
            retainedBytesEstimate,
          );
          return record;
        };
        let frontier: RawNode[] = [];
        let loadedNodes = 0;
        let nodesRequested = 0;
        let hydrationHits = 0;
        let hydrationMisses = 0;
        let maxInFlight = 0;
        let maxBatchKeys = 0;
        let maxFrontierPaths = states.length;
        const resolved = new Set<number>();
        if (!root.equals(MPF_EMPTY_ROOT)) {
          const [rootValue] = await this.store.loadEventFlatRawRecords([root]);
          if (rootValue === undefined) {
            throw new Error(
              `Event-flat durable base is missing root ${root.toString("hex")}`,
            );
          }
          authenticate(root, rootValue);
          loadedNodes += 1;
          frontier = [{ hash: root, value: rootValue, states }];
        } else {
          for (const state of states) resolved.add(state.id);
        }
        while (frontier.length > 0) {
          const requests: {
            readonly hash: Buffer;
            readonly states: readonly PathState[];
          }[] = [];
          const next: RawNode[] = [];
          for (const node of frontier) {
            if (node.value.__kind === "Leaf") {
              for (const state of node.states) resolved.add(state.id);
              continue;
            }
            const branch = node.value;
            const groups = new Map<number, PathState[]>();
            const deleteTargetChildIndexes = new Set<number>();
            for (const state of node.states) {
              if (!state.path.slice(state.ix).startsWith(branch.prefix)) {
                resolved.add(state.id);
                continue;
              }
              const childIndex = Number.parseInt(
                state.path[state.ix + branch.prefix.length]!,
                16,
              );
              if (state.deletePath) {
                deleteTargetChildIndexes.add(childIndex);
              }
              const group = groups.get(childIndex);
              if (group === undefined) groups.set(childIndex, [state]);
              else group.push(state);
            }
            const nonEmptyChildIndexes = branch.children.flatMap(
              (child, childIndex) => (child == null ? [] : [childIndex]),
            );
            const targetedExistingChildren = [
              ...deleteTargetChildIndexes,
            ].filter(
              (childIndex) => branch.children[childIndex] != null,
            ).length;
            if (
              targetedExistingChildren > 0 &&
              nonEmptyChildIndexes.length - targetedExistingChildren <= 1
            ) {
              for (const siblingIndex of nonEmptyChildIndexes) {
                if (!groups.has(siblingIndex)) groups.set(siblingIndex, []);
              }
            }
            for (const [childIndex, childStates] of groups) {
              const childHashHex = branch.children[childIndex];
              if (childHashHex == null) {
                hydrationMisses += 1;
                for (const state of childStates) resolved.add(state.id);
                continue;
              }
              nodesRequested += 1;
              const nextStates = childStates.map((state) => ({
                ...state,
                ix: state.ix + branch.prefix.length + 1,
              }));
              const known = authenticated.get(childHashHex);
              if (known !== undefined) {
                hydrationHits += 1;
                if (nextStates.length > 0) {
                  next.push({
                    hash: known.hash,
                    value: known.value,
                    states: nextStates,
                  });
                }
                continue;
              }
              requests.push({
                hash: Buffer.from(childHashHex, "hex"),
                states: nextStates,
              });
            }
          }
          const uniqueRequests = new Map<
            string,
            { readonly hash: Buffer; readonly requests: typeof requests }
          >();
          for (const request of requests) {
            const hashHex = request.hash.toString("hex");
            const existing = uniqueRequests.get(hashHex);
            if (existing === undefined) {
              uniqueRequests.set(hashHex, {
                hash: request.hash,
                requests: [request],
              });
            } else {
              existing.requests.push(request);
            }
          }
          const pending = [...uniqueRequests.values()];
          maxInFlight = Math.max(maxInFlight, pending.length);
          for (let offset = 0; offset < pending.length; offset += 4_096) {
            const batch = pending.slice(offset, offset + 4_096);
            maxBatchKeys = Math.max(maxBatchKeys, batch.length);
            const values = await this.store.loadEventFlatRawRecords(
              batch.map((request) => request.hash),
            );
            for (const [index, request] of batch.entries()) {
              const value = values[index];
              if (value === undefined) {
                throw new Error(
                  `Event-flat touched path is missing node ${request.hash.toString("hex")}`,
                );
              }
              const record = authenticate(request.hash, value);
              loadedNodes += 1;
              for (const relation of request.requests) {
                if (relation.states.length > 0) {
                  next.push({
                    hash: record.hash,
                    value: record.value,
                    states: relation.states,
                  });
                }
              }
            }
          }
          maxFrontierPaths = Math.max(
            maxFrontierPaths,
            next.reduce((total, node) => total + node.states.length, 0),
          );
          frontier = next;
        }
        if (resolved.size !== states.length) {
          throw new Error(
            `Event-flat touched-path proof is incomplete: resolved=${resolved.size.toString()},expected=${states.length.toString()}`,
          );
        }
        const arena = EventFlatMutationArena.fromAuthenticatedRecords(root, [
          ...authenticated.values(),
        ]);
        this.store.assertEventFlatArenaCaps(
          arena.nodeCount(),
          arena.estimatedBytes(),
        );
        this.store.handoffRawPathsToEventFlat();
        this.eventFlatArena = arena;
        const elapsedMs = performance.now() - startedAt;
        const checkpoint: MpfArenaCheckpointDiagnostics = {
          checkpointMs: 0,
          authenticationMs: 0,
          materializeMs: 0,
          collapseMs: 0,
          serializedNodes: 0,
          serializedBytes: 0,
          verifiedUpperNodes: 0,
          retainedUpperNodes: 0,
          collapsedNodes: 0,
        };
        return {
          hydration: {
            prefetchMs: Math.max(0, elapsedMs - authenticationMs),
            uniquePaths: states.length,
            nodesRequested,
            hydrationHits,
            hydrationMisses,
            loadedNodes,
            maxInFlight,
            maxBatchKeys,
            maxFrontierPaths,
            retainedBytesEstimate,
            chunkCount: 1,
            checkpointMs: 0,
            authenticationMs: 0,
            materializeMs: 0,
            collapseMs: 0,
            checkpointSerializedNodes: 0,
            checkpointSerializedBytes: 0,
            verifiedUpperNodes: 0,
            retainedUpperNodes: 0,
            collapsedNodes: 0,
            peakDecodedNodes: loadedNodes,
          },
          authenticationMs,
          verifiedNodes: authenticated.size,
          checkpoint,
        };
      },
      catch: (cause) => MpfError.get(`${this.trieName} event-flat raw`, cause),
    });
  }

  public checkpointAndCollapseDecodedArena(
    retainDepth = 2,
    materialize = true,
    collapseDecodedArena = true,
  ): Effect.Effect<MpfArenaCheckpointDiagnostics, MpfError> {
    const trie = this.trie as Trie & {
      assertHydratedNodeHashes?: (maxDepth: number) => {
        readonly verifiedNodes: number;
      };
      collapseHydratedChildren?: (retainedDepth: number) => {
        readonly retainedNodes: number;
        readonly collapsedNodes: number;
      };
    };
    return Effect.tryPromise({
      try: async () => {
        try {
          if (
            trie.assertHydratedNodeHashes === undefined ||
            trie.collapseHydratedChildren === undefined
          ) {
            throw new Error(
              "Patched MPF trie does not expose authenticated arena collapse",
            );
          }
          const boundedRetainDepth = Math.max(
            0,
            Math.min(8, Math.floor(retainDepth)),
          );
          const checkpointStartedAt = performance.now();
          const rootBefore = Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
          const writesBefore = this.store.diagnostics().levelBatchWrites;
          const transientVerification = collapseDecodedArena
            ? this.store.authenticateDirtyLiveNodes(
                this.trie as unknown as MpfSerializableValue,
              )
            : { verifiedNodes: 0, authenticationMs: 0 };
          const authenticationStartedAt = performance.now();
          const verification = trie.assertHydratedNodeHashes(
            collapseDecodedArena ? boundedRetainDepth : 0,
          );
          const authenticationMs =
            transientVerification.authenticationMs +
            performance.now() -
            authenticationStartedAt;
          const checkpoint = materialize
            ? await this.store.checkpointDeferredNodes()
            : { serializedNodes: 0, serializedBytes: 0, checkpointMs: 0 };
          const collapseStartedAt = performance.now();
          const collapsed = collapseDecodedArena
            ? trie.collapseHydratedChildren(boundedRetainDepth)
            : { retainedNodes: verification.verifiedNodes, collapsedNodes: 0 };
          const collapseMs = collapseDecodedArena
            ? performance.now() - collapseStartedAt
            : 0;
          const rootAfter = Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
          if (!rootAfter.equals(rootBefore)) {
            throw new Error(
              `Decoded-arena collapse changed MPF root: before=${rootBefore.toString("hex")},after=${rootAfter.toString("hex")}`,
            );
          }
          if (this.store.diagnostics().levelBatchWrites !== writesBefore) {
            throw new Error(
              "Decoded-arena checkpoint performed a durable Level write",
            );
          }
          const checkpointMs = performance.now() - checkpointStartedAt;
          if (!materialize) {
            this.store.recordLiveArenaCheckpoint(checkpointMs);
          }
          return {
            checkpointMs,
            authenticationMs,
            materializeMs: checkpoint.checkpointMs,
            collapseMs,
            serializedNodes: checkpoint.serializedNodes,
            serializedBytes: checkpoint.serializedBytes,
            verifiedUpperNodes:
              transientVerification.verifiedNodes + verification.verifiedNodes,
            retainedUpperNodes: collapsed.retainedNodes,
            collapsedNodes: collapsed.collapsedNodes,
          };
        } catch (cause) {
          try {
            const baseRoot =
              await this.store.poisonOverlayAfterMutationFailure();
            this.trie.hash = Buffer.from(baseRoot);
          } catch {
            // Preserve the checkpoint error; recovery reopens from the durable marker.
          }
          throw cause;
        }
      },
      catch: (cause) =>
        MpfError.get(`${this.trieName} decoded arena checkpoint`, cause),
    });
  }

  public authenticateDecodedArena(
    retainDepth = 2,
  ): Effect.Effect<
    { readonly verifiedNodes: number; readonly authenticationMs: number },
    MpfError
  > {
    const trie = this.trie as Trie & {
      assertHydratedNodeHashes?: (maxDepth: number) => {
        readonly verifiedNodes: number;
      };
    };
    return Effect.tryPromise({
      try: async () => {
        try {
          if (trie.assertHydratedNodeHashes === undefined) {
            throw new Error(
              "Patched MPF trie does not expose authenticated arena verification",
            );
          }
          const startedAt = performance.now();
          const verification = trie.assertHydratedNodeHashes(
            Math.max(0, Math.min(64, Math.floor(retainDepth))),
          );
          return {
            verifiedNodes: verification.verifiedNodes,
            authenticationMs: performance.now() - startedAt,
          };
        } catch (cause) {
          try {
            const baseRoot =
              await this.store.poisonOverlayAfterMutationFailure();
            this.trie.hash = Buffer.from(baseRoot);
          } catch {
            // Preserve the authentication error; recovery uses the durable marker.
          }
          throw cause;
        }
      },
      catch: (cause) =>
        MpfError.get(`${this.trieName} decoded arena verification`, cause),
    });
  }

  public beginBlockOverlay(): Effect.Effect<void, MpfError> {
    return Effect.try({
      try: () => {
        this.eventFlatArena = undefined;
        this.store.beginOverlay();
      },
      catch: (e) => MpfError.batch(this.trieName, e),
    });
  }

  public flushBlockOverlay(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      yield* Effect.try({
        try: () => {
          if (this.eventFlatArena !== undefined) {
            if (!this.eventFlatArena.rootHash().equals(root)) {
              throw new Error("Event-flat flush root does not match its arena");
            }
            this.store.stageEventFlatCandidate(
              root,
              this.eventFlatArena.reachableDirtyRecords(),
            );
            return;
          }
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          );
        },
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      yield* Effect.tryPromise({
        try: () => this.store.flushOverlay(root),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
      });
      Object.assign(this, reloaded);
      this.eventFlatArena = undefined;
    });
  }

  public parkBlockOverlay(): Effect.Effect<ParkedMpfOverlayV1, MpfError> {
    return Effect.gen(this, function* () {
      const root = yield* this.root();
      yield* Effect.try({
        try: () => {
          if (this.eventFlatArena !== undefined) {
            this.store.stageEventFlatCandidate(
              root,
              this.eventFlatArena.reachableDirtyRecords(),
            );
            return;
          }
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          );
        },
        catch: (cause) => MpfError.batch(`${this.trieName}-park`, cause),
      });
      const artifact = yield* Effect.tryPromise({
        try: () => this.store.parkCurrentOverlay(root, this.trieName),
        catch: (cause) => MpfError.batch(`${this.trieName}-park`, cause),
      });
      yield* Effect.tryPromise({
        try: () =>
          this.store.shouldCloseLevel()
            ? (this.level?.close() ?? Promise.resolve())
            : Promise.resolve(),
        catch: (cause) => MpfError.close(`${this.trieName}-park`, cause),
      });
      this.eventFlatArena = undefined;
      return artifact;
    });
  }

  public parkEventFlatOverlayV1(
    shardCount = 4,
  ): Effect.Effect<ParkedEventFlatOverlayV1, MpfError> {
    return Effect.gen(this, function* () {
      if (this.engine !== "event_flat" || this.eventFlatArena === undefined) {
        return yield* Effect.fail(
          MpfError.batch(
            `${this.trieName}-event-flat-park`,
            new Error("Event-flat V1 park requires an active packed arena"),
          ),
        );
      }
      const baseRoot = this.store.overlayStartingRoot();
      if (baseRoot === undefined) {
        return yield* Effect.fail(
          MpfError.batch(
            `${this.trieName}-event-flat-park`,
            new Error("Event-flat V1 park requires an active overlay"),
          ),
        );
      }
      const writesBefore = this.store.diagnostics().levelBatchWrites;
      const frozen = yield* Effect.either(
        Effect.tryPromise({
          try: () =>
            this.eventFlatArena!.freezeParallel({
              trieName: this.trieName,
              baseRoot,
              shardCount,
            }),
          catch: (cause) =>
            MpfError.batch(`${this.trieName}-event-flat-park`, cause),
        }),
      );
      if (frozen._tag === "Left") {
        yield* Effect.tryPromise({
          try: () => this.store.poisonOverlayAfterMutationFailure(),
          catch: (cause) =>
            MpfError.batch(`${this.trieName}-event-flat-park`, cause),
        }).pipe(Effect.catchAll(() => Effect.void));
        return yield* Effect.fail(frozen.left);
      }
      if (this.store.diagnostics().levelBatchWrites !== writesBefore) {
        return yield* Effect.fail(
          MpfError.batch(
            `${this.trieName}-event-flat-park`,
            new Error("Event-flat V1 park performed a Level write"),
          ),
        );
      }
      yield* Effect.try({
        try: () => this.store.discardOverlay(),
        catch: (cause) =>
          MpfError.batch(`${this.trieName}-event-flat-park`, cause),
      });
      yield* Effect.tryPromise({
        try: () =>
          this.store.shouldCloseLevel()
            ? (this.level?.close() ?? Promise.resolve())
            : Promise.resolve(),
        catch: (cause) =>
          MpfError.close(`${this.trieName}-event-flat-park`, cause),
      });
      this.eventFlatArena = undefined;
      return frozen.right;
    });
  }

  public static resumeParkedEventFlatOverlayV1(
    trieName: string,
    levelDBFilePath: string | undefined,
    artifact: ParkedEventFlatOverlayV1,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      yield* Effect.tryPromise({
        try: () => prepareEventFlatDigest(),
        catch: (cause) =>
          MpfError.create(`${trieName}-event-flat-digest`, cause),
      });
      if (artifact.trieName !== trieName) {
        return yield* Effect.fail(
          MpfError.create(
            trieName,
            new Error(
              `Parked event-flat trie mismatch: expected=${trieName},actual=${artifact.trieName}`,
            ),
          ),
        );
      }
      const resumedView = yield* Effect.try({
        try: () => new ResumedEventFlatOverlayV1(artifact),
        catch: (cause) =>
          MpfError.create(`${trieName}-event-flat-resume`, cause),
      });
      const mpf = yield* MidgardMpf.create(trieName, levelDBFilePath, {
        engine: "overlay",
      });
      const resumed = yield* Effect.either(
        Effect.gen(function* () {
          const durableRoot = yield* mpf.root();
          const parkedBase = Buffer.from(artifact.baseRoot);
          if (!durableRoot.equals(parkedBase)) {
            return yield* Effect.fail(
              MpfError.create(
                trieName,
                new Error(
                  `Parked event-flat durable base changed: durable=${durableRoot.toString("hex")},parked=${parkedBase.toString("hex")}`,
                ),
              ),
            );
          }
          yield* mpf.beginBlockOverlay();
          const candidateRoot = resumedView.rootHash();
          yield* Effect.try({
            try: () =>
              mpf.store.stageEventFlatCandidate(
                candidateRoot,
                resumedView.flushReadyRecords(),
              ),
            catch: (cause) =>
              MpfError.batch(`${trieName}-event-flat-resume`, cause),
          });
          const candidateTrie = yield* Effect.tryPromise({
            try: () =>
              candidateRoot.equals(MPF_EMPTY_ROOT)
                ? Promise.resolve(new Trie(mpf.store))
                : Trie.load(mpf.store),
            catch: (cause) =>
              MpfError.create(`${trieName}-event-flat-resume`, cause),
          });
          Object.assign(mpf, { trie: candidateTrie });
          if (!(yield* mpf.root()).equals(candidateRoot)) {
            return yield* Effect.fail(
              MpfError.create(
                `${trieName}-event-flat-resume`,
                new Error("Parked event-flat candidate root is unreadable"),
              ),
            );
          }
          return mpf;
        }),
      );
      if (resumed._tag === "Right") return resumed.right;
      yield* mpf.close().pipe(Effect.catchAll(() => Effect.void));
      return yield* Effect.fail(resumed.left);
    });
  }

  public static resumeParkedOverlay(
    trieName: string,
    levelDBFilePath: string | undefined,
    artifact: ParkedMpfOverlayV1,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      if (artifact.trieName !== trieName) {
        return yield* Effect.fail(
          MpfError.create(
            trieName,
            new Error(
              `Parked MPF trie mismatch: expected=${trieName},actual=${artifact.trieName}`,
            ),
          ),
        );
      }
      const mpf = yield* MidgardMpf.create(trieName, levelDBFilePath, {
        engine: "overlay",
      });
      const resumed = yield* Effect.either(
        Effect.gen(function* () {
          const durableRoot = yield* mpf.root();
          const parkedBase = Buffer.from(artifact.baseRoot);
          if (!durableRoot.equals(parkedBase)) {
            return yield* Effect.fail(
              MpfError.create(
                trieName,
                new Error(
                  `Parked MPF durable base changed: durable=${durableRoot.toString("hex")},parked=${parkedBase.toString("hex")}`,
                ),
              ),
            );
          }
          yield* mpf.beginBlockOverlay();
          const candidateRoot = yield* Effect.tryPromise({
            try: () => mpf.store.importParkedOverlay(artifact),
            catch: (cause) => MpfError.batch(`${trieName}-resume`, cause),
          });
          const candidateTrie = yield* Effect.tryPromise({
            try: () =>
              candidateRoot.equals(MPF_EMPTY_ROOT)
                ? Promise.resolve(new Trie(mpf.store))
                : Trie.load(mpf.store),
            catch: (cause) => MpfError.create(`${trieName}-resume`, cause),
          });
          Object.assign(mpf, { trie: candidateTrie });
          if (!(yield* mpf.root()).equals(candidateRoot)) {
            return yield* Effect.fail(
              MpfError.create(
                `${trieName}-resume`,
                new Error("Parked MPF candidate root is unreadable"),
              ),
            );
          }
          return mpf;
        }),
      );
      if (resumed._tag === "Right") return resumed.right;
      yield* mpf.close().pipe(Effect.catchAll(() => Effect.void));
      return yield* Effect.fail(resumed.left);
    });
  }

  public static promoteParkedOverlay(
    trieName: string,
    levelDBFilePath: string | undefined,
    artifact: ParkedMpfOverlayV1,
  ): Effect.Effect<void, MpfError> {
    return Effect.gen(function* () {
      const mpf = yield* MidgardMpf.resumeParkedOverlay(
        trieName,
        levelDBFilePath,
        artifact,
      );
      const promoted = yield* Effect.either(
        mpf.flushBlockOverlay(Buffer.from(artifact.candidateRoot)),
      );
      yield* mpf.close().pipe(Effect.catchAll(() => Effect.void));
      if (promoted._tag === "Left") return yield* Effect.fail(promoted.left);
    });
  }

  public discardBlockOverlay(): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      this.eventFlatArena = undefined;
      yield* Effect.tryPromise({
        try: () => this.store.waitForSpills(),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      const root = yield* Effect.try({
        try: () => this.store.discardOverlay(),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      if (this.parentStore !== undefined) {
        this.trie.hash = Buffer.from(root);
        return;
      }
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.parentStore,
        parentOverlay: this.parentOverlay,
      });
      Object.assign(this, reloaded);
    });
  }

  public discardBlockOverlayIfActive(): Effect.Effect<void, MpfError> {
    return this.store.overlayIsActive()
      ? this.discardBlockOverlay()
      : Effect.void;
  }

  public blockOverlayIsActive(): boolean {
    return this.store.overlayIsActive();
  }

  public usesStrictOverlayMutations(): boolean {
    return this.engine !== "legacy" && this.store.overlayIsActive();
  }

  public usesEventFlatEngine(): boolean {
    return this.engine === "event_flat";
  }

  public eventFlatMutationDiagnostics():
    | EventFlatMutationDiagnostics
    | undefined {
    return this.eventFlatArena?.diagnostics();
  }

  public spillIfNeeded(): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: () => this.store.spillIfNeeded(),
      catch: (e) => MpfError.batch(this.trieName, e),
    });
  }

  public ledgerOverlayHandle(): Effect.Effect<LedgerOverlayHandle, MpfError> {
    return Effect.gen(this, function* () {
      if (!this.store.overlayIsActive()) {
        yield* this.beginBlockOverlay();
      }
      return new MidgardLedgerOverlayHandle(this);
    });
  }

  public forkBlockOverlay(): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(this, function* () {
      yield* Effect.try({
        try: () =>
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          ),
        catch: (e) => MpfError.batch(`${this.trieName}-fork`, e),
      });
      const root = yield* this.root();
      const rootAvailable = yield* Effect.tryPromise({
        try: () => this.store.hasStoredNode(root),
        catch: (e) => MpfError.get(this.trieName, e),
      });
      if (!rootAvailable) {
        return yield* Effect.fail(
          MpfError.create(
            `${this.trieName}-fork`,
            new Error(
              `Current overlay root ${root.toString("hex")} is not readable from its parent store`,
            ),
          ),
        );
      }
      const forkStore = new MidgardMpfRootViewStore({
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: false,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.store,
        parentOverlay: this.store.currentOverlayView(),
      });
      const forkTrie = Object.assign(
        Object.create(Object.getPrototypeOf(this.trie)) as Trie,
        this.trie,
        {
          hash: Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT),
          store: forkStore,
        },
      );
      const mutableForkTrie = forkTrie as Trie & {
        children?: Array<{ hash: Buffer } | undefined>;
        key?: Buffer;
        value?: Buffer;
        __midgardMerkleNodes?: unknown;
        __midgardDirtyChild?: unknown;
      };
      if (mutableForkTrie.children !== undefined) {
        mutableForkTrie.children = mutableForkTrie.children.map((child) =>
          child === undefined ? undefined : { hash: Buffer.from(child.hash) },
        );
      }
      if (mutableForkTrie.key !== undefined) {
        mutableForkTrie.key = Buffer.from(mutableForkTrie.key);
      }
      if (mutableForkTrie.value !== undefined) {
        mutableForkTrie.value = Buffer.from(mutableForkTrie.value);
      }
      delete mutableForkTrie.__midgardMerkleNodes;
      delete mutableForkTrie.__midgardDirtyChild;
      const fork = new MidgardMpf({
        trie: forkTrie,
        // A fork is a lifecycle view of the same logical trie. Parked
        // artifacts must retain the durable store identity so a fresh process
        // can resume them after the owning parent closes.
        trieName: this.trieName,
        store: forkStore,
        level: this.level,
        memory: this.memory,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.store,
        parentOverlay: this.store.currentOverlayView(),
      });
      yield* fork.beginBlockOverlay();
      return fork;
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

/**
 * Block-scoped ledger overlay contract consumed by Phase 4 pipelining.
 *
 * G1: after `promote`, the durable root marker and retained working root equal
 * the root returned by `rootHex`. G2: `fork` is O(1): the fork reads through
 * its immutable parent overlay and owns only its subsequent delta. G3: block
 * deltas are plain `MpfBatchOp` values and can cross a worker boundary.
 */
export interface LedgerOverlayHandle {
  rootHex(): Effect.Effect<string, MpfError>;
  fork(): Effect.Effect<LedgerOverlayHandle, MpfError>;
  applyBlockDelta(ops: readonly MpfBatchOp[]): Effect.Effect<string, MpfError>;
  promote(): Effect.Effect<void, MpfError>;
  discard(): Effect.Effect<void, MpfError>;
}

class MidgardLedgerOverlayHandle implements LedgerOverlayHandle {
  constructor(private readonly mpf: MidgardMpf) {}

  rootHex() {
    return this.mpf.rootHex();
  }

  fork(): Effect.Effect<LedgerOverlayHandle, MpfError> {
    return this.mpf
      .forkBlockOverlay()
      .pipe(Effect.map((fork) => new MidgardLedgerOverlayHandle(fork)));
  }

  applyBlockDelta(ops: readonly MpfBatchOp[]) {
    return this.mpf
      .applyBatch(ops)
      .pipe(Effect.map((root) => root.toString("hex")));
  }

  promote() {
    return Effect.gen(this, function* () {
      const root = yield* this.mpf.root();
      yield* this.mpf.flushBlockOverlay(root);
    });
  }

  discard() {
    return this.mpf.discardBlockOverlay();
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
