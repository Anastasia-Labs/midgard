import { Proof, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect, Option } from "effect";
import { Level } from "level";

import {
  type MpfArenaCheckpointDiagnostics,
  type MpfPathHydrationDiagnostics,
  type MpfStoreDiagnostics,
  type MpfStoreMode,
} from "./engine-config.js";
import { MpfError } from "./errors.js";
import { MidgardMpfRootViewStore } from "./root-view-store.js";
import { readPersistedRoot } from "./store.read-persisted-root.js";
import {
  JSON_LEVEL_ENCODING_OPTS,
  type LevelBatchOp,
  MPF_EMPTY_ROOT,
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
  private readonly store: MidgardMpfRootViewStore;
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;
  private readonly mode: MpfStoreMode;
  private readonly spillThresholdBytes: number;

  private constructor({
    trie,
    trieName,
    store,
    level,
    memory,
    mode = "direct",
    spillThresholdBytes = 512 * 1024 * 1024,
  }: {
    readonly trie: Trie;
    readonly trieName: string;
    readonly store: MidgardMpfRootViewStore;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly mode?: MpfStoreMode;
    readonly spillThresholdBytes?: number;
  }) {
    this.trie = trie;
    this.trieName = trieName;
    this.store = store;
    this.level = level;
    this.memory = memory;
    this.mode = mode;
    this.spillThresholdBytes = spillThresholdBytes;
  }

  public static create(
    trieName: string,
    levelDBFilePath?: string,
    options: {
      readonly mode?: MpfStoreMode;
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
        mode: options.mode ?? "direct",
        spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
      });
    });
  }

  public static createScratch(
    trieName: string,
    options: {
      readonly mode?: MpfStoreMode;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root: MPF_EMPTY_ROOT,
      memory: new Map(),
      persistRootMarker: false,
      mode: options.mode ?? "direct",
      spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
    });
  }

  public static createScratchFromList(
    trieName: string,
    entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
    options: { readonly mode?: MpfStoreMode } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const memory = new Map<string, MpfStoredValue>();
      const store = new MidgardMpfRootViewStore({
        memory,
        root: MPF_EMPTY_ROOT,
        persistRootMarker: false,
        mode: options.mode ?? "direct",
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
        mode: options.mode ?? "direct",
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
      readonly mode?: MpfStoreMode;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const memory = new Map<string, MpfStoredValue>();
      const stagingStore = new MidgardMpfRootViewStore({
        memory,
        root: MPF_EMPTY_ROOT,
        persistRootMarker: false,
        mode: "direct",
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
        mode: options.mode ?? "overlay",
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
    mode = "direct",
    spillThresholdBytes = 512 * 1024 * 1024,
  }: {
    readonly trieName: string;
    readonly level?: Level<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
    readonly mode?: MpfStoreMode;
    readonly spillThresholdBytes?: number;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root,
      level,
      persistRootMarker,
      mode,
      spillThresholdBytes,
    });
  }

  private static loadFromRootView({
    trieName,
    root,
    level,
    memory,
    persistRootMarker,
    mode = "direct",
    spillThresholdBytes = 512 * 1024 * 1024,
  }: {
    readonly trieName: string;
    readonly root: Buffer;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly persistRootMarker: boolean;
    readonly mode?: MpfStoreMode;
    readonly spillThresholdBytes?: number;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const store = new MidgardMpfRootViewStore({
        level,
        memory,
        root,
        persistRootMarker,
        mode,
        spillThresholdBytes,
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
        mode,
        spillThresholdBytes,
      });
    });
  }

  /** Distinguishes a durably empty trie from a missing or recreated store. */
  public persistedRootMarker(): Effect.Effect<Buffer | undefined, MpfError> {
    return Effect.tryPromise({
      try: async () => {
        if (this.level === undefined) return undefined;
        const marker = await this.level.get(ROOT_KEY, JSON_LEVEL_ENCODING_OPTS);
        return marker === undefined ? undefined : parseStoredRootHex(marker);
      },
      catch: (cause) => MpfError.rootNotSet(this.trieName, cause),
    });
  }

  public root(): Effect.Effect<Buffer, MpfError> {
    return Effect.try({
      try: () => {
        this.store.root();
        return Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
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
    if (this.store.overlayIsActive() && this.mode === "overlay") {
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
        mode: this.mode,
        spillThresholdBytes: this.spillThresholdBytes,
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
        await (this.level?.close() ?? Promise.resolve());
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
      try: () => this.store.beginOverlay(),
      catch: (e) => MpfError.batch(this.trieName, e),
    });
  }

  public flushBlockOverlay(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      yield* Effect.try({
        try: () =>
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          ),
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
        mode: this.mode,
        spillThresholdBytes: this.spillThresholdBytes,
      });
      Object.assign(this, reloaded);
    });
  }

  public discardBlockOverlay(): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      yield* Effect.tryPromise({
        try: () => this.store.waitForSpills(),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      const root = yield* Effect.try({
        try: () => this.store.discardOverlay(),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        mode: this.mode,
        spillThresholdBytes: this.spillThresholdBytes,
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
    return this.mode === "overlay" && this.store.overlayIsActive();
  }

  public spillIfNeeded(): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: () => this.store.spillIfNeeded(),
      catch: (e) => MpfError.batch(this.trieName, e),
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
