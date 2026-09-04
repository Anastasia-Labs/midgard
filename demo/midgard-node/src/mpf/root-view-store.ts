/**
 * The root-view store: a LevelDB-backed MPF store with parked overlays and arena checkpoints.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Level } from "level";

import { type PackedMpfStoredValue } from "../workers/utils/mpf-event-flat.js";
import {
  DEFAULT_MPF_ARENA_LIMITS,
  getMpfArenaLimits,
  type MpfArenaLimits,
  type MpfEngine,
  type MpfStoreDiagnostics,
  type ParkedMpfOverlay,
} from "./engine-config.js";
import { estimateMpfStoredValueBytes } from "./payload-size.js";
import {
  applyPendingBatch,
  consumeMpfMutationProof,
  exactArrayBuffer,
  JSON_LEVEL_ENCODING_OPTS,
  type LevelBatchOp,
  MPF_EMPTY_ROOT,
  MPF_INTERNAL_NULL_ROOT,
  MPF_INTERNAL_NULL_ROOT_HEX,
  normalizeStoredRootHex,
  parkedOverlayDigest,
  ROOT_KEY,
} from "./store-primitives.js";
import {
  type MpfReadableValue,
  type MpfSerializableValue,
  type MpfStoredValue,
} from "./types.js";

export class MidgardMpfRootViewStore extends Store {
  public readonly retainHydratedChildren: boolean;
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;
  private readonly persistRootMarker: boolean;
  private readonly engine: MpfEngine;
  private readonly spillThresholdBytes: number;
  private readonly parentStore?: MidgardMpfRootViewStore;
  private readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  private currentRoot: Buffer;
  private batchOps: LevelBatchOp[] | undefined;
  private deferredNodePuts: Map<string, MpfSerializableValue> | undefined;
  private deferredNodeDeletes: Set<string> | undefined;
  private deferredMutationRoot: Buffer | undefined;
  private blockDeferredNodePuts: Map<string, MpfSerializableValue> | undefined;
  private blockDeferredNodeDeletes: Set<string> | undefined;
  private blockDeferredNodeEstimates: Map<string, number> | undefined;
  private blockDeferredEstimatedBytes = 0;
  private overlay: Map<string, MpfStoredValue> | undefined;
  private overlayValueBytes = new Map<string, number>();
  private readonly spillingOverlays: Map<string, MpfStoredValue>[] = [];
  private spillChain: Promise<void> = Promise.resolve();
  private spillError: unknown | undefined;
  private pendingSpillBytes = 0;
  private overlayBaseRoot: Buffer | undefined;
  private overlayBytes = 0;
  private readonly readCache = new Map<string, MpfStoredValue>();
  private blockPathCache: Map<string, MpfStoredValue> | undefined;
  private blockPathCacheBytes = 0;
  private blockPathCacheSealed = false;
  private authenticatedNodeObjects = new WeakSet<object>();
  private readonly hydratedNodeSources = new WeakMap<object, object>();
  private liveArenaEnabled = false;
  private readonly transientLiveNodes = new Set<MpfSerializableValue>();
  private readonly transientLiveNodeEstimates = new Map<
    MpfSerializableValue,
    number
  >();
  private readonly transientDirtyNodes = new Set<MpfSerializableValue>();
  private readonly midgardDirtyNodes = new Set<Trie>();
  private transientLiveBytes = 0;
  private transientSnapshotsCaptured = 0;
  private transientCurrentRootEnabled = false;
  private midgardTransientArenaTokenValue: object = {};
  private eventAtomicFinalizations = 0;
  private eventAtomicDirtyNodes = 0;
  private eventAtomicMaxDirtyNodes = 0;
  private arenaLimits: MpfArenaLimits = { ...DEFAULT_MPF_ARENA_LIMITS };
  private levelGets = 0;
  private levelGetManyCalls = 0;
  private levelGetManyMaxKeys = 0;
  private storePuts = 0;
  private storeDels = 0;
  private serialiseCalls = 0;
  private serialiseMs = 0;
  private deferredMaterializedEstimatedBytes = 0;
  private deferredMaterializedActualBytes = 0;
  private deferredLazyReads = 0;
  private deferredLazySerialiseMs = 0;
  private deferredLazySerialisedBytes = 0;
  private arenaCheckpointCalls = 0;
  private arenaCheckpointMs = 0;
  private arenaCheckpointNodes = 0;
  private arenaCheckpointBytes = 0;
  private pathCacheHits = 0;
  private liveArenaPrunedNodes = 0;
  private liveArenaPromotedNodes = 0;
  private liveArenaPromotedBytes = 0;
  private retainedSnapshotAuthentications = 0;
  private retainedSnapshotAuthenticationMs = 0;
  private overlayHits = 0;
  private readCacheHits = 0;
  private levelBatchWrites = 0;
  private bytesFlushed = 0;
  private overlaySpills = 0;
  private levelGetMs = 0;
  private jsonCodecMs = 0;
  private overlaySpillMs = 0;
  private flushMs = 0;
  private openForks = 0;
  private forkClosed = false;
  private invalidatedByChildPromotion = false;
  private promotionReserved = false;
  private poisoned = false;
  private ownsLevelLifecycle: boolean;

  constructor({
    level,
    memory,
    root,
    persistRootMarker,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
    parentStore,
    parentOverlay,
  }: {
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
    readonly parentStore?: MidgardMpfRootViewStore;
    readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  }) {
    super(undefined);
    this.level = level;
    this.memory = memory;
    this.currentRoot = Buffer.from(root);
    this.persistRootMarker = persistRootMarker;
    this.engine = engine;
    this.retainHydratedChildren = engine !== "legacy";
    this.spillThresholdBytes = spillThresholdBytes;
    this.parentStore = parentStore;
    this.parentOverlay = parentOverlay;
    this.ownsLevelLifecycle = parentStore === undefined;
    this.parentStore?.registerFork();
  }

  async ready() {
    await this.level?.open();
  }

  async batch(callback: () => Promise<unknown>) {
    if (this.synchronousRetainedWrites) {
      return callback();
    }
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
        if (this.overlay !== undefined) {
          for (const op of ops) {
            if (op.key === ROOT_KEY) continue;
            if (op.type === "put") {
              this.setOverlayValue(op.key, op.value, op.encodedBytes);
            } else if (this.overlay.has(op.key)) {
              this.deleteOverlayValue(op.key);
            }
          }
          await this.spillIfNeeded();
        } else if (this.level !== undefined) {
          await this.level.batch(ops, JSON_LEVEL_ENCODING_OPTS);
          this.levelBatchWrites += 1;
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

  async get(key: unknown, deserialise: (...args: unknown[]) => unknown) {
    this.assertUsable();
    if (key === ROOT_KEY) {
      return deserialise(key, this.currentRoot.toString("hex"), this);
    }
    const storageKey = this.storageKey(key);
    let storedValue: MpfReadableValue | undefined;
    const activeMutationValue = this.deferredNodePuts?.get(storageKey);
    const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
    if (activeMutationValue !== undefined) {
      storedValue = activeMutationValue;
      if (this.liveArenaEnabled) {
        this.assertLiveArenaNode(storageKey, activeMutationValue);
      }
    } else if (this.liveArenaEnabled && deferredValue !== undefined) {
      storedValue = deferredValue;
      this.assertLiveArenaNode(storageKey, deferredValue);
    } else if (this.overlay?.has(storageKey)) {
      this.overlayHits += 1;
      storedValue = this.overlay.get(storageKey);
    } else {
      for (
        let index = this.spillingOverlays.length - 1;
        index >= 0;
        index -= 1
      ) {
        const spilling = this.spillingOverlays[index]!;
        if (spilling.has(storageKey)) {
          this.overlayHits += 1;
          storedValue = spilling.get(storageKey);
          break;
        }
      }
    }
    if (storedValue !== undefined) {
      // The active or in-flight overlay supplied the value.
    } else if (this.parentOverlay?.has(storageKey)) {
      storedValue = this.parentOverlay.get(storageKey);
    } else if (this.parentStore !== undefined) {
      storedValue = await this.parentStore.lookupStoredValue(storageKey);
    } else if (this.blockPathCache?.has(storageKey)) {
      this.pathCacheHits += 1;
      storedValue = this.blockPathCache.get(storageKey);
    } else if (this.readCache.has(storageKey)) {
      this.readCacheHits += 1;
      storedValue = this.readCache.get(storageKey);
    } else if (this.liveArenaEnabled && this.blockPathCacheSealed) {
      const durableValue =
        this.level === undefined
          ? this.memory?.get(storageKey)
          : await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
      throw new Error(
        `Sealed MPF block path cache missed node ${storageKey};durable_source=${durableValue === undefined ? "absent" : "present"}`,
      );
    } else if (this.level === undefined) {
      storedValue = this.memory!.get(storageKey);
    } else {
      const startedAt = performance.now();
      storedValue = await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
      this.levelGetMs += performance.now() - startedAt;
      this.levelGets += 1;
      if (storedValue !== undefined) {
        this.cacheRead(storageKey, storedValue);
        this.cacheBlockPathNode(storageKey, storedValue);
      }
    }
    const value = applyPendingBatch(storageKey, storedValue, this.batchOps);
    const decoded = await deserialise(key, value, this);
    this.rememberHydratedNodeSource(decoded, value);
    return decoded;
  }

  async getMany(
    keys: readonly unknown[],
    deserialise: (...args: unknown[]) => unknown,
  ) {
    this.assertUsable();
    const storageKeys = keys.map((key) => this.storageKey(key));
    const values = new Array<MpfReadableValue | undefined>(keys.length);
    const unresolvedIndexes: number[] = [];
    const unresolvedStorageKeys: string[] = [];

    for (let index = 0; index < storageKeys.length; index += 1) {
      const storageKey = storageKeys[index]!;
      let storedValue: MpfReadableValue | undefined;
      let resolved = false;
      const activeMutationValue = this.deferredNodePuts?.get(storageKey);
      const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
      if (storageKey === ROOT_KEY) {
        storedValue = this.currentRoot.toString("hex");
        resolved = true;
      } else if (activeMutationValue !== undefined) {
        storedValue = activeMutationValue;
        if (this.liveArenaEnabled) {
          this.assertLiveArenaNode(storageKey, activeMutationValue);
        }
        resolved = true;
      } else if (this.liveArenaEnabled && deferredValue !== undefined) {
        storedValue = deferredValue;
        this.assertLiveArenaNode(storageKey, deferredValue);
        resolved = true;
      } else if (this.overlay?.has(storageKey)) {
        this.overlayHits += 1;
        storedValue = this.overlay.get(storageKey);
        resolved = true;
      } else {
        for (
          let spillIndex = this.spillingOverlays.length - 1;
          spillIndex >= 0;
          spillIndex -= 1
        ) {
          const spilling = this.spillingOverlays[spillIndex]!;
          if (spilling.has(storageKey)) {
            this.overlayHits += 1;
            storedValue = spilling.get(storageKey);
            resolved = true;
            break;
          }
        }
      }
      if (!resolved && this.parentOverlay?.has(storageKey)) {
        storedValue = this.parentOverlay.get(storageKey);
        resolved = true;
      } else if (!resolved && this.parentStore !== undefined) {
        storedValue = await this.parentStore.lookupStoredValue(storageKey);
        resolved = true;
      } else if (!resolved && this.blockPathCache?.has(storageKey)) {
        this.pathCacheHits += 1;
        storedValue = this.blockPathCache.get(storageKey);
        resolved = true;
      } else if (!resolved && this.readCache.has(storageKey)) {
        this.readCacheHits += 1;
        storedValue = this.readCache.get(storageKey);
        resolved = true;
      } else if (!resolved && this.level === undefined) {
        storedValue = this.memory!.get(storageKey);
        resolved = true;
      }
      if (resolved) {
        values[index] = storedValue;
      } else {
        unresolvedIndexes.push(index);
        unresolvedStorageKeys.push(storageKey);
      }
    }

    if (unresolvedStorageKeys.length > 0) {
      if (this.liveArenaEnabled && this.blockPathCacheSealed) {
        const durableValues =
          this.level === undefined
            ? unresolvedStorageKeys.map((key) => this.memory?.get(key))
            : await this.level.getMany(
                unresolvedStorageKeys,
                JSON_LEVEL_ENCODING_OPTS,
              );
        throw new Error(
          `Sealed MPF block path cache missed ${unresolvedStorageKeys.length.toString()} nodes;durable_present=${durableValues.filter((value) => value !== undefined).length.toString()};keys=${unresolvedStorageKeys.join(",")}`,
        );
      }
      const startedAt = performance.now();
      const loaded = await this.level!.getMany(
        unresolvedStorageKeys,
        JSON_LEVEL_ENCODING_OPTS,
      );
      this.levelGetMs += performance.now() - startedAt;
      this.levelGets += unresolvedStorageKeys.length;
      this.levelGetManyCalls += 1;
      this.levelGetManyMaxKeys = Math.max(
        this.levelGetManyMaxKeys,
        unresolvedStorageKeys.length,
      );
      for (let offset = 0; offset < unresolvedIndexes.length; offset += 1) {
        const index = unresolvedIndexes[offset]!;
        const storedValue = loaded[offset];
        values[index] = storedValue;
        if (storedValue !== undefined) {
          this.cacheRead(storageKeys[index]!, storedValue);
          this.cacheBlockPathNode(storageKeys[index]!, storedValue);
        }
      }
    }

    const decoded = await Promise.all(
      values.map((storedValue, index) =>
        deserialise(
          keys[index],
          applyPendingBatch(storageKeys[index]!, storedValue, this.batchOps),
          this,
        ),
      ),
    );
    for (let index = 0; index < decoded.length; index += 1) {
      this.rememberHydratedNodeSource(decoded[index], values[index]);
    }
    return decoded;
  }

  async put(key: unknown, value: MpfSerializableValue) {
    this.assertUsable();
    this.storePuts += 1;
    const storageKey = this.storageKey(key);
    if (
      storageKey !== ROOT_KEY &&
      this.deferredNodePuts !== undefined &&
      this.deferredNodeDeletes !== undefined
    ) {
      this.deferredNodePuts.set(storageKey, value);
      this.deferredNodeDeletes.delete(storageKey);
      return;
    }
    const serialiseStartedAt = performance.now();
    const rawSerialized = value.serialise();
    this.serialiseMs += performance.now() - serialiseStartedAt;
    this.serialiseCalls += 1;
    const serialized =
      storageKey === ROOT_KEY && typeof rawSerialized === "string"
        ? normalizeStoredRootHex(rawSerialized)
        : rawSerialized;
    let encodedBytes: number | undefined;
    if (this.level !== undefined) {
      const jsonCodecStartedAt = performance.now();
      encodedBytes = Buffer.byteLength(JSON.stringify(serialized));
      this.jsonCodecMs += performance.now() - jsonCodecStartedAt;
    }
    if (storageKey === ROOT_KEY) {
      this.currentRoot = Buffer.from(serialized as string, "hex");
      if (!this.persistRootMarker || this.overlay !== undefined) {
        return;
      }
    }
    const op: LevelBatchOp = {
      type: "put",
      key: storageKey,
      value: serialized,
      encodedBytes,
    };
    if (this.batchOps !== undefined) {
      this.batchOps.push(op);
    } else if (this.overlay !== undefined) {
      this.setOverlayValue(op.key, op.value, op.encodedBytes);
      await this.spillIfNeeded();
    } else if (this.level !== undefined) {
      await this.level.put(op.key, op.value, JSON_LEVEL_ENCODING_OPTS);
    } else {
      this.memory!.set(op.key, op.value);
    }
  }

  get synchronousRetainedWrites() {
    return this.retainHydratedChildren && this.deferredNodePuts !== undefined;
  }

  get midgardTransientArenaToken(): object | undefined {
    return this.transientCurrentRootEnabled
      ? this.midgardTransientArenaTokenValue
      : undefined;
  }

  get deferMidgardBranchHashes(): boolean {
    return (
      this.transientCurrentRootEnabled && this.deferredNodePuts !== undefined
    );
  }

  recordMidgardDirtyNode(node: Trie): void {
    if (!this.deferMidgardBranchHashes || node.store !== this) {
      throw new Error("Cannot retain a foreign MPF event mutation node");
    }
    this.midgardDirtyNodes.add(node);
  }

  takeMidgardDirtyNodes(): readonly Trie[] {
    if (!this.deferMidgardBranchHashes) {
      throw new Error("Cannot finalize an inactive MPF event mutation");
    }
    const dirtyNodes = [...this.midgardDirtyNodes];
    this.midgardDirtyNodes.clear();
    this.eventAtomicFinalizations += 1;
    this.eventAtomicDirtyNodes += dirtyNodes.length;
    this.eventAtomicMaxDirtyNodes = Math.max(
      this.eventAtomicMaxDirtyNodes,
      dirtyNodes.length,
    );
    return dirtyNodes;
  }

  putRetainedNode(key: unknown, value: MpfSerializableValue) {
    this.assertUsable();
    if (
      this.deferredNodePuts === undefined ||
      this.deferredNodeDeletes === undefined
    ) {
      throw new Error("Synchronous retained MPF write outside a mutation");
    }
    this.storePuts += 1;
    const storageKey = this.storageKey(key);
    const proofCandidate = value as MpfSerializableValue & {
      readonly consumeMidgardMutationProof?: () => boolean;
    };
    const trustedMutation =
      this.liveArenaEnabled &&
      this.transientCurrentRootEnabled &&
      value instanceof Trie &&
      proofCandidate.consumeMidgardMutationProof === consumeMpfMutationProof &&
      consumeMpfMutationProof.call(value);
    if (trustedMutation) {
      const previousEstimate = this.transientLiveNodeEstimates.get(value) ?? 0;
      const estimate = this.estimateDeferredNodeBytes(storageKey, value);
      this.transientLiveNodes.add(value);
      this.transientLiveNodeEstimates.set(value, estimate);
      this.transientLiveBytes += estimate - previousEstimate;
      this.transientDirtyNodes.add(value);
      this.deferredNodeDeletes.delete(storageKey);
      this.assertLiveArenaCaps();
      return;
    }
    const retainedValue = this.liveArenaEnabled
      ? (
          value as MpfSerializableValue & {
            readonly cloneDetached?: () => MpfSerializableValue;
          }
        ).cloneDetached?.()
      : value;
    if (
      retainedValue === undefined ||
      (this.liveArenaEnabled && retainedValue === value)
    ) {
      throw new Error(
        "Live MPF arena requires an immutable detached node snapshot",
      );
    }
    if (this.liveArenaEnabled) {
      // A content key may already have been authenticated for a different
      // object. Verify every new detached snapshot before it can replace that
      // key; raw cache clones remain memoized by their immutable source object.
      const authenticationStartedAt = performance.now();
      this.authenticateHydratedNodeOnce(retainedValue);
      this.retainedSnapshotAuthenticationMs +=
        performance.now() - authenticationStartedAt;
      this.retainedSnapshotAuthentications += 1;
    }
    this.deferredNodePuts.set(storageKey, retainedValue);
    this.deferredNodeDeletes.delete(storageKey);
  }

  deleteRetainedNode(key: unknown) {
    this.assertUsable();
    if (
      this.deferredNodePuts === undefined ||
      this.deferredNodeDeletes === undefined
    ) {
      throw new Error("Synchronous retained MPF delete outside a mutation");
    }
    this.storeDels += 1;
    if (this.liveArenaEnabled) {
      // Content hashes can be shared by multiple parents. Retain immutable
      // snapshots until the final current-root mark/sweep proves them orphaned.
      return;
    }
    const storageKey = this.storageKey(key);
    this.deferredNodePuts.delete(storageKey);
    this.deferredNodeDeletes.add(storageKey);
  }

  putRetainedRoot(root: Buffer | null | undefined) {
    this.assertUsable();
    this.storePuts += 1;
    const candidate = Buffer.from(root ?? MPF_EMPTY_ROOT);
    this.currentRoot = candidate.equals(MPF_INTERNAL_NULL_ROOT)
      ? Buffer.from(MPF_EMPTY_ROOT)
      : candidate;
  }

  async del(key: unknown) {
    this.assertUsable();
    this.storeDels += 1;
    const storageKey = this.storageKey(key);
    if (
      storageKey !== ROOT_KEY &&
      this.deferredNodePuts !== undefined &&
      this.deferredNodeDeletes !== undefined
    ) {
      if (this.liveArenaEnabled) return;
      this.deferredNodePuts.delete(storageKey);
      this.deferredNodeDeletes.add(storageKey);
      return;
    }
    if (this.overlay !== undefined && storageKey !== ROOT_KEY) {
      if (this.batchOps !== undefined) {
        this.batchOps.push({ type: "del", key: storageKey });
      } else if (this.overlay.has(storageKey)) {
        this.deleteOverlayValue(storageKey);
      }
      return;
    }
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
    this.assertUsable();
    return Buffer.from(this.currentRoot);
  }

  setRoot(root: Buffer) {
    this.currentRoot = Buffer.from(root);
  }

  beginOverlay() {
    this.assertUsable();
    if (this.engine === "legacy") {
      throw new Error("Cannot begin an MPF overlay when MPF_ENGINE=legacy");
    }
    if (this.overlay !== undefined) {
      throw new Error("MPF block overlay already active");
    }
    this.overlay = new Map();
    this.overlayValueBytes = new Map();
    this.blockDeferredNodePuts = new Map();
    this.blockDeferredNodeDeletes = new Set();
    this.blockDeferredNodeEstimates = new Map();
    this.blockDeferredEstimatedBytes = 0;
    this.overlayBaseRoot = Buffer.from(this.currentRoot);
    this.overlayBytes = 0;
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
    this.transientSnapshotsCaptured = 0;
    this.eventAtomicFinalizations = 0;
    this.eventAtomicDirtyNodes = 0;
    this.eventAtomicMaxDirtyNodes = 0;
    this.arenaLimits = { ...getMpfArenaLimits() };
    this.authenticatedNodeObjects = new WeakSet<object>();
  }

  enableBlockPathArena(transientCurrentRoot = false) {
    this.assertUsable();
    if (this.overlay === undefined) {
      throw new Error("Cannot enable an MPF path arena without an overlay");
    }
    if (this.liveArenaEnabled) {
      throw new Error("MPF block path arena is already active");
    }
    this.liveArenaEnabled = true;
    this.transientCurrentRootEnabled = transientCurrentRoot;
    this.midgardTransientArenaTokenValue = {};
    this.clearTransientLiveArena();
    this.blockPathCache = new Map();
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.arenaLimits = { ...getMpfArenaLimits() };
    for (const [key, value] of this.readCache) {
      this.cacheBlockPathNode(key, value);
    }
  }

  sealBlockPathCache() {
    if (!this.liveArenaEnabled || this.blockPathCache === undefined) {
      throw new Error("Cannot seal an inactive MPF block path cache");
    }
    this.blockPathCacheSealed = true;
  }

  assertEventFlatArenaCaps(nodeCount: number, estimatedBytes: number) {
    this.assertUsable();
    if (
      nodeCount > this.arenaLimits.liveArenaMaxNodes ||
      estimatedBytes > this.arenaLimits.liveArenaMaxBytes
    ) {
      throw new Error(
        `Event-flat arena limit exceeded: nodes=${nodeCount.toString()}/${this.arenaLimits.liveArenaMaxNodes.toString()},bytes=${estimatedBytes.toString()}/${this.arenaLimits.liveArenaMaxBytes.toString()}`,
      );
    }
  }

  assertEventFlatRawCaps(nodeCount: number, estimatedBytes: number) {
    this.assertUsable();
    if (
      nodeCount > this.arenaLimits.pathCacheMaxNodes ||
      estimatedBytes > this.arenaLimits.pathCacheMaxBytes
    ) {
      throw new Error(
        `Event-flat raw path limit exceeded: nodes=${nodeCount.toString()}/${this.arenaLimits.pathCacheMaxNodes.toString()},bytes=${estimatedBytes.toString()}/${this.arenaLimits.pathCacheMaxBytes.toString()}`,
      );
    }
  }

  async loadEventFlatRawRecords(
    hashes: readonly Buffer[],
  ): Promise<readonly (PackedMpfStoredValue | undefined)[]> {
    this.assertUsable();
    const values = (await this.getMany(hashes, (_hash, value) =>
      value === undefined
        ? undefined
        : typeof value === "object" &&
            value !== null &&
            "serialise" in value &&
            typeof value.serialise === "function"
          ? value.serialise()
          : value,
    )) as readonly unknown[];
    return values.map((value) =>
      typeof value === "object" &&
      value !== null &&
      "__kind" in value &&
      (value.__kind === "Leaf" || value.__kind === "Branch")
        ? (value as PackedMpfStoredValue)
        : undefined,
    );
  }

  handoffRawPathsToEventFlat() {
    this.assertUsable();
    if (this.overlay === undefined || this.liveArenaEnabled) {
      throw new Error("Cannot hand off an inactive or mutable raw MPF view");
    }
    if (
      this.deferredNodePuts !== undefined ||
      this.deferredNodeDeletes !== undefined ||
      this.midgardDirtyNodes.size !== 0 ||
      this.transientDirtyNodes.size !== 0 ||
      this.transientLiveNodes.size !== 0 ||
      (this.blockDeferredNodePuts?.size ?? 0) !== 0 ||
      (this.blockDeferredNodeDeletes?.size ?? 0) !== 0
    ) {
      throw new Error("Cannot hand off dirty raw MPF paths to event-flat");
    }
    this.readCache.clear();
  }

  private rememberHydratedNodeSource(
    decoded: unknown,
    source: MpfReadableValue | undefined,
  ) {
    if (
      typeof decoded === "object" &&
      decoded !== null &&
      typeof source === "object" &&
      source !== null
    ) {
      this.hydratedNodeSources.set(decoded, source);
    }
  }

  authenticateHydratedNodeOnce(value: MpfSerializableValue) {
    const hash = (value as MpfSerializableValue & { readonly hash?: Buffer })
      .hash;
    if (hash === undefined) {
      throw new Error("Hydrated MPF node has no content hash");
    }
    const identity =
      this.hydratedNodeSources.get(value as object) ?? (value as object);
    if (this.authenticatedNodeObjects.has(identity)) return;
    const authenticate = (
      value as MpfSerializableValue & {
        readonly assertHydratedNodeHashes?: (maxDepth: number) => unknown;
      }
    ).assertHydratedNodeHashes;
    if (authenticate === undefined) {
      throw new Error("Hydrated MPF node cannot authenticate its hash");
    }
    authenticate.call(value, 0);
    this.authenticatedNodeObjects.add(identity);
  }

  authenticateDirtyLiveNodes(root?: MpfSerializableValue): {
    readonly verifiedNodes: number;
    readonly authenticationMs: number;
  } {
    if (
      !this.liveArenaEnabled ||
      !this.transientCurrentRootEnabled ||
      this.transientDirtyNodes.size === 0
    ) {
      return { verifiedNodes: 0, authenticationMs: 0 };
    }
    const startedAt = performance.now();
    const reachableNodes =
      root === undefined ? undefined : new Set<MpfSerializableValue>();
    const dirtyNodes: MpfSerializableValue[] = [];
    if (root === undefined) {
      dirtyNodes.push(...this.transientDirtyNodes);
    } else {
      const pending = [root];
      while (pending.length > 0) {
        const node = pending.pop()!;
        if (reachableNodes!.has(node)) continue;
        reachableNodes!.add(node);
        if (this.transientDirtyNodes.has(node)) dirtyNodes.push(node);
        const children = (
          node as MpfSerializableValue & {
            readonly children?: readonly unknown[];
          }
        ).children;
        for (const child of children ?? []) {
          if (
            typeof child === "object" &&
            child !== null &&
            "serialise" in child
          ) {
            pending.push(child as MpfSerializableValue);
          }
        }
      }
    }
    let verifiedNodes = 0;
    for (const node of dirtyNodes) {
      const authenticate = (
        node as MpfSerializableValue & {
          readonly assertHydratedNodeHashes?: (maxDepth: number) => unknown;
        }
      ).assertHydratedNodeHashes;
      if (authenticate === undefined) {
        throw new Error("Transient MPF node cannot authenticate its hash");
      }
      authenticate.call(node, 0);
      verifiedNodes += 1;
    }
    if (reachableNodes !== undefined) {
      for (const node of this.transientLiveNodes) {
        if (reachableNodes.has(node)) continue;
        this.transientLiveNodes.delete(node);
        this.transientLiveBytes -=
          this.transientLiveNodeEstimates.get(node) ?? 0;
        this.transientLiveNodeEstimates.delete(node);
        this.liveArenaPrunedNodes += 1;
      }
    }
    this.transientDirtyNodes.clear();
    const authenticationMs = performance.now() - startedAt;
    this.retainedSnapshotAuthentications += verifiedNodes;
    this.retainedSnapshotAuthenticationMs += authenticationMs;
    return { verifiedNodes, authenticationMs };
  }

  captureCurrentLiveTrie(root: MpfSerializableValue): void {
    try {
      this.captureCurrentLiveTrieUnchecked(root);
    } catch (cause) {
      try {
        this.abortDeferredMutation();
        const baseRoot = this.discardOverlay();
        this.currentRoot = Buffer.from(baseRoot);
        this.poisoned = true;
      } catch {
        // Preserve the authentication/capture failure. The durable marker is
        // unchanged and recovery reopens it in a fresh store.
      }
      throw cause;
    }
  }

  private captureCurrentLiveTrieUnchecked(root: MpfSerializableValue): void {
    if (
      !this.liveArenaEnabled ||
      !this.transientCurrentRootEnabled ||
      this.transientLiveNodes.size === 0
    ) {
      return;
    }
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeEstimates === undefined
    ) {
      throw new Error("Cannot capture an inactive transient MPF arena");
    }
    const pending = [root];
    const visited = new Set<object>();
    while (pending.length > 0) {
      const node = pending.pop()!;
      if (visited.has(node as object)) continue;
      visited.add(node as object);
      const children = (
        node as MpfSerializableValue & {
          readonly children?: readonly (
            | MpfSerializableValue
            | { readonly hash?: Buffer }
            | undefined
          )[];
        }
      ).children;
      for (const child of children ?? []) {
        if (
          typeof child === "object" &&
          child !== null &&
          "serialise" in child
        ) {
          pending.push(child as MpfSerializableValue);
        }
      }
      if (!this.transientLiveNodes.has(node)) continue;
      const hash = (node as MpfSerializableValue & { readonly hash?: Buffer })
        .hash;
      const clone = (
        node as MpfSerializableValue & {
          readonly cloneDetached?: () => MpfSerializableValue;
        }
      ).cloneDetached?.();
      if (hash === undefined || clone === undefined || clone === node) {
        throw new Error("Cannot capture a mutable transient MPF node");
      }
      const key = this.storageKey(hash);
      this.assertLiveArenaNode(key, clone);
      const priorEstimate = this.blockDeferredNodeEstimates.get(key) ?? 0;
      const estimate = this.estimateDeferredNodeBytes(key, clone);
      this.blockDeferredNodePuts.set(key, clone);
      this.blockDeferredNodeEstimates.set(key, estimate);
      this.blockDeferredEstimatedBytes += estimate - priorEstimate;
      this.transientSnapshotsCaptured += 1;
    }
    this.clearTransientLiveArena();
    this.assertLiveArenaCaps();
  }

  async parkCurrentOverlay(
    root: Buffer,
    trieName: string,
  ): Promise<ParkedMpfOverlay> {
    this.assertUsable();
    if (
      this.overlay === undefined ||
      this.overlayBaseRoot === undefined ||
      !root.equals(this.currentRoot)
    ) {
      throw new Error("Cannot park an inactive or mismatched MPF overlay");
    }
    await this.waitForSpills();
    const writesBefore = this.levelBatchWrites;
    if (this.parentStore !== undefined) {
      await this.waitForAncestorSpills();
      this.importReachableParentCandidates(root);
    }
    this.pruneLiveArenaToRoot(root);
    const parkedNodes = new Map<string, MpfStoredValue>();
    const visited = new Set<string>();
    const pending = root.equals(MPF_EMPTY_ROOT) ? [] : [this.storageKey(root)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (visited.has(key)) continue;
      visited.add(key);
      const live = this.blockDeferredNodePuts?.get(key);
      const serialized = this.overlay.get(key);
      const candidate = live ?? serialized;
      if (candidate === undefined) {
        // A missing local candidate is an unchanged durable subtree. A new
        // descendant cannot be reachable through an unchanged content hash.
        continue;
      }
      if (live !== undefined) this.assertLiveArenaNode(key, live);
      parkedNodes.set(key, live === undefined ? serialized! : live.serialise());
      for (const childKey of this.candidateChildKeys(candidate)) {
        pending.push(childKey);
      }
    }
    const ordered = [...parkedNodes].sort(([left], [right]) =>
      left.localeCompare(right),
    );
    const nodeHashes = new Uint8Array(ordered.length * 32);
    const encodedValues = ordered.map(([, value]) =>
      Buffer.from(JSON.stringify(value)),
    );
    const nodeValueOffsets = new Uint32Array(ordered.length * 2);
    const nodeValues = new Uint8Array(
      encodedValues.reduce((total, value) => total + value.length, 0),
    );
    let valueOffset = 0;
    for (const [index, [key]] of ordered.entries()) {
      const hash = Buffer.from(key, "hex");
      if (hash.length !== 32) {
        throw new Error(`Cannot park invalid MPF content key ${key}`);
      }
      nodeHashes.set(hash, index * 32);
      const encoded = encodedValues[index]!;
      nodeValues.set(encoded, valueOffset);
      nodeValueOffsets.set([valueOffset, encoded.length], index * 2);
      valueOffset += encoded.length;
    }
    const baseRoot = exactArrayBuffer(this.overlayBaseRoot);
    const candidateRoot = exactArrayBuffer(root);
    const hashesBuffer = exactArrayBuffer(nodeHashes);
    const valuesBuffer = exactArrayBuffer(nodeValues);
    const offsetsBuffer = exactArrayBuffer(
      new Uint8Array(nodeValueOffsets.buffer),
    );
    const digest = parkedOverlayDigest({
      trieName,
      baseRoot,
      candidateRoot,
      nodeCount: ordered.length,
      nodeHashes: hashesBuffer,
      nodeValues: valuesBuffer,
      nodeValueOffsets: offsetsBuffer,
    });
    if (this.levelBatchWrites !== writesBefore) {
      throw new Error("Parking an MPF overlay performed a Level write");
    }
    const artifact: ParkedMpfOverlay = {
      schemaVersion: 1,
      trieName,
      baseRoot,
      candidateRoot,
      closureDigest: exactArrayBuffer(digest),
      nodeCount: ordered.length,
      nodeHashes: hashesBuffer,
      nodeValues: valuesBuffer,
      nodeValueOffsets: offsetsBuffer,
      encodedBytes:
        hashesBuffer.byteLength +
        valuesBuffer.byteLength +
        offsetsBuffer.byteLength,
    };
    this.discardOverlay();
    return artifact;
  }

  async importParkedOverlay(artifact: ParkedMpfOverlay): Promise<Buffer> {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error("Cannot import a parked MPF without an active overlay");
    }
    const baseRoot = Buffer.from(artifact.baseRoot);
    if (!baseRoot.equals(this.overlayBaseRoot)) {
      throw new Error(
        `Parked MPF base mismatch: durable=${this.overlayBaseRoot.toString("hex")},parked=${baseRoot.toString("hex")}`,
      );
    }
    if (
      artifact.schemaVersion !== 1 ||
      !Number.isSafeInteger(artifact.nodeCount) ||
      artifact.nodeCount < 0 ||
      artifact.baseRoot.byteLength !== 32 ||
      artifact.candidateRoot.byteLength !== 32 ||
      artifact.closureDigest.byteLength !== 32 ||
      artifact.nodeHashes.byteLength !== artifact.nodeCount * 32 ||
      artifact.nodeValueOffsets.byteLength !== artifact.nodeCount * 8 ||
      artifact.encodedBytes !==
        artifact.nodeHashes.byteLength +
          artifact.nodeValues.byteLength +
          artifact.nodeValueOffsets.byteLength
    ) {
      throw new Error("Invalid parked MPF artifact shape");
    }
    const expectedDigest = parkedOverlayDigest({
      trieName: artifact.trieName,
      baseRoot: artifact.baseRoot,
      candidateRoot: artifact.candidateRoot,
      nodeCount: artifact.nodeCount,
      nodeHashes: artifact.nodeHashes,
      nodeValues: artifact.nodeValues,
      nodeValueOffsets: artifact.nodeValueOffsets,
    });
    if (!expectedDigest.equals(Buffer.from(artifact.closureDigest))) {
      throw new Error("Parked MPF closure digest mismatch");
    }
    const hashes = new Uint8Array(artifact.nodeHashes);
    const values = new Uint8Array(artifact.nodeValues);
    const offsets = new Uint32Array(artifact.nodeValueOffsets);
    const artifactKeys = new Set<string>();
    const deserialise = (
      Trie as unknown as {
        readonly deserialise: (
          hash: Buffer,
          value: MpfStoredValue,
          store: MidgardMpfRootViewStore,
        ) => Promise<MpfSerializableValue>;
      }
    ).deserialise;
    for (let index = 0; index < artifact.nodeCount; index += 1) {
      const hash = Buffer.from(hashes.subarray(index * 32, (index + 1) * 32));
      const key = hash.toString("hex");
      if (artifactKeys.has(key)) {
        throw new Error(`Parked MPF contains duplicate node ${key}`);
      }
      artifactKeys.add(key);
      const valueOffset = offsets[index * 2]!;
      const valueLength = offsets[index * 2 + 1]!;
      if (valueOffset + valueLength > values.length) {
        throw new Error("Parked MPF node value exceeds its packed arena");
      }
      const value = JSON.parse(
        Buffer.from(
          values.subarray(valueOffset, valueOffset + valueLength),
        ).toString(),
      ) as MpfStoredValue;
      const decoded = await deserialise(hash, value, this);
      this.assertLiveArenaNode(key, decoded);
      this.setOverlayValue(key, value);
    }
    const candidateRoot = Buffer.from(artifact.candidateRoot);
    this.currentRoot = Buffer.from(candidateRoot);
    const reachableArtifactKeys = new Set<string>();
    const visited = new Set<string>();
    const pending = candidateRoot.equals(MPF_EMPTY_ROOT)
      ? []
      : [this.storageKey(candidateRoot)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (visited.has(key)) continue;
      visited.add(key);
      const local = this.overlay.get(key);
      if (local !== undefined) {
        reachableArtifactKeys.add(key);
        for (const childKey of this.candidateChildKeys(local)) {
          pending.push(childKey);
        }
        continue;
      }
      if ((await this.lookupStoredValue(key)) === undefined) {
        throw new Error(`Parked MPF closure is missing node ${key}`);
      }
    }
    if (reachableArtifactKeys.size !== artifactKeys.size) {
      throw new Error(
        `Parked MPF contains unreachable nodes: reachable=${reachableArtifactKeys.size.toString()},packed=${artifactKeys.size.toString()}`,
      );
    }
    return candidateRoot;
  }

  private clearTransientLiveArena() {
    this.transientLiveNodes.clear();
    this.transientLiveNodeEstimates.clear();
    this.transientDirtyNodes.clear();
    this.transientLiveBytes = 0;
  }

  beginDeferredMutation() {
    if (!this.retainHydratedChildren || this.overlay === undefined) {
      return false;
    }
    if (this.deferredNodePuts !== undefined) {
      throw new Error("MPF deferred mutation already active");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot mutate an MPF overlay while a fork is active");
    }
    this.deferredNodePuts = new Map();
    this.deferredNodeDeletes = new Set();
    this.deferredMutationRoot = Buffer.from(this.currentRoot);
    this.midgardDirtyNodes.clear();
    return true;
  }

  async commitDeferredMutation() {
    if (
      this.deferredNodePuts === undefined ||
      this.deferredNodeDeletes === undefined
    ) {
      throw new Error("MPF deferred mutation is not active");
    }
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeDeletes === undefined ||
      this.blockDeferredNodeEstimates === undefined
    ) {
      throw new Error("MPF block deferred mutation is not active");
    }
    if (!this.liveArenaEnabled) {
      for (const key of this.deferredNodeDeletes) {
        this.blockDeferredNodePuts.delete(key);
        this.blockDeferredNodeDeletes.add(key);
        this.blockDeferredEstimatedBytes -=
          this.blockDeferredNodeEstimates.get(key) ?? 0;
        this.blockDeferredNodeEstimates.delete(key);
      }
    }
    for (const [key, value] of this.deferredNodePuts) {
      this.blockDeferredEstimatedBytes -=
        this.blockDeferredNodeEstimates.get(key) ?? 0;
      const estimate = this.estimateDeferredNodeBytes(key, value);
      this.blockDeferredNodePuts.set(key, value);
      this.blockDeferredNodeEstimates.set(key, estimate);
      this.blockDeferredEstimatedBytes += estimate;
      this.blockDeferredNodeDeletes.delete(key);
    }
    this.deferredNodePuts = undefined;
    this.deferredNodeDeletes = undefined;
    this.deferredMutationRoot = undefined;
    this.midgardDirtyNodes.clear();
    await this.spillIfNeeded();
  }

  abortDeferredMutation() {
    if (this.deferredNodePuts === undefined) return;
    if (this.deferredMutationRoot !== undefined) {
      this.currentRoot = Buffer.from(this.deferredMutationRoot);
    }
    this.deferredNodePuts = undefined;
    this.deferredNodeDeletes = undefined;
    this.deferredMutationRoot = undefined;
    this.midgardDirtyNodes.clear();
  }

  overlayIsActive() {
    return this.overlay !== undefined;
  }

  overlayStartingRoot() {
    return this.overlayBaseRoot === undefined
      ? undefined
      : Buffer.from(this.overlayBaseRoot);
  }

  stageEventFlatCandidate(
    root: Buffer,
    records: ReadonlyMap<string, PackedMpfStoredValue>,
  ) {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error(
        "Cannot stage an event-flat candidate without an overlay",
      );
    }
    if (this.openForks > 0) {
      throw new Error("Cannot stage an event-flat candidate with open forks");
    }
    this.abortDeferredMutation();
    this.overlay = new Map();
    this.overlayValueBytes = new Map();
    this.overlayBytes = 0;
    this.blockDeferredNodePuts?.clear();
    this.blockDeferredNodeDeletes?.clear();
    this.blockDeferredNodeEstimates?.clear();
    this.blockDeferredEstimatedBytes = 0;
    for (const [key, value] of records) this.setOverlayValue(key, value);
    this.currentRoot = Buffer.from(root);
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
  }

  async resetOverlayRoot(root: Buffer) {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error("Cannot reset an inactive MPF block overlay");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot reset an MPF overlay while a fork is active");
    }
    if (this.deferredNodePuts !== undefined) {
      throw new Error("Cannot reset an MPF overlay during a mutation");
    }
    // A requested root may have been created earlier in this overlay and still
    // be held as a deferred live node. Keep the overlay contents as harmless
    // content-addressed candidates so that root remains readable; only the
    // virtual working root changes. The original overlayBaseRoot continues to
    // define discard/rollback and no durable marker is written here.
    const candidate = Buffer.from(root);
    if (
      this.liveArenaEnabled &&
      !candidate.equals(MPF_EMPTY_ROOT) &&
      (await this.lookupStoredValue(this.storageKey(candidate))) === undefined
    ) {
      throw new Error(
        `Cannot reset live MPF arena to unreadable root ${candidate.toString("hex")}`,
      );
    }
    if (!this.liveArenaEnabled) {
      await this.materializeDeferredNodes();
    }
    this.currentRoot = candidate.equals(MPF_INTERNAL_NULL_ROOT)
      ? Buffer.from(MPF_EMPTY_ROOT)
      : candidate;
    return Buffer.from(this.currentRoot);
  }

  async flushOverlay(root: Buffer) {
    this.assertUsable();
    if (this.overlay === undefined) {
      throw new Error("Cannot flush an inactive MPF block overlay");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot promote an MPF overlay while a fork is active");
    }
    if (!root.equals(this.currentRoot)) {
      throw new Error(
        `Refusing to promote mismatched MPF root: requested=${root.toString("hex")},current=${this.currentRoot.toString("hex")}`,
      );
    }
    const promotionReservations = this.reservePromotionChain();
    try {
      if (this.parentStore !== undefined) {
        await this.waitForAncestorSpills();
        this.importReachableParentCandidates(root);
      }
      if (this.liveArenaEnabled) {
        this.pruneLiveArenaToRoot(root);
      }
      await this.materializeDeferredNodes();
      await this.waitForSpills();
      const ops: LevelBatchOp[] = [...this.overlay].map(([key, value]) => ({
        type: "put" as const,
        key,
        value,
        encodedBytes: this.overlayValueBytes.get(key),
      }));
      if (this.level !== undefined) {
        ops.push({
          type: "put",
          key: ROOT_KEY,
          value: normalizeStoredRootHex(root.toString("hex")),
        });
        const flushStartedAt = performance.now();
        await this.level.batch(ops, JSON_LEVEL_ENCODING_OPTS);
        this.flushMs += performance.now() - flushStartedAt;
        this.levelBatchWrites += 1;
        this.bytesFlushed += ops.reduce(
          (total, op) =>
            total +
            op.key.length +
            (op.type === "put"
              ? (op.encodedBytes ?? Buffer.byteLength(JSON.stringify(op.value)))
              : 0),
          0,
        );
      } else if (this.memory !== undefined) {
        for (const [key, value] of this.overlay) {
          this.memory.set(key, value);
        }
      }
      this.currentRoot = Buffer.from(root);
      this.overlay = undefined;
      this.overlayValueBytes = new Map();
      this.blockDeferredNodePuts = undefined;
      this.blockDeferredNodeDeletes = undefined;
      this.blockDeferredNodeEstimates = undefined;
      this.blockDeferredEstimatedBytes = 0;
      this.blockPathCache = undefined;
      this.blockPathCacheBytes = 0;
      this.blockPathCacheSealed = false;
      this.liveArenaEnabled = false;
      this.transientCurrentRootEnabled = false;
      this.clearTransientLiveArena();
      this.overlayBaseRoot = undefined;
      this.overlayBytes = 0;
      this.readCache.clear();
      if (this.parentStore !== undefined) {
        this.parentStore.transferOwnershipToChild(root);
        this.ownsLevelLifecycle = true;
      }
      this.closeFork();
    } finally {
      this.releasePromotionChain(promotionReservations);
    }
  }

  discardOverlay() {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error("Cannot discard an inactive MPF block overlay");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot discard an MPF overlay while a fork is active");
    }
    const baseRoot = Buffer.from(this.overlayBaseRoot);
    this.overlay = undefined;
    this.overlayValueBytes = new Map();
    this.blockDeferredNodePuts = undefined;
    this.blockDeferredNodeDeletes = undefined;
    this.blockDeferredNodeEstimates = undefined;
    this.blockDeferredEstimatedBytes = 0;
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
    this.overlayBaseRoot = undefined;
    this.overlayBytes = 0;
    this.readCache.clear();
    this.currentRoot = Buffer.from(baseRoot);
    this.closeFork();
    return baseRoot;
  }

  async poisonOverlayAfterMutationFailure() {
    this.abortDeferredMutation();
    await this.waitForSpills();
    const baseRoot = this.discardOverlay();
    this.poisoned = true;
    return baseRoot;
  }

  async spillIfNeeded() {
    if (this.liveArenaEnabled) {
      this.assertLiveArenaCaps();
      return;
    }
    if (
      this.level === undefined ||
      this.overlay === undefined ||
      this.overlayBytes + this.blockDeferredEstimatedBytes <=
        this.spillThresholdBytes
    ) {
      return;
    }
    await this.materializeDeferredNodes();
    const ops: LevelBatchOp[] = [...this.overlay].map(([key, value]) => ({
      type: "put" as const,
      key,
      value,
      encodedBytes: this.overlayValueBytes.get(key),
    }));
    const spillingOverlay = this.overlay;
    const spillBytes = this.overlayBytes;
    this.spillingOverlays.push(spillingOverlay);
    this.pendingSpillBytes += spillBytes;
    this.overlay = new Map();
    this.overlayValueBytes = new Map();
    this.overlayBytes = 0;
    this.overlaySpills += 1;
    this.spillChain = this.spillChain.then(async () => {
      try {
        if (this.spillError === undefined) {
          const spillStartedAt = performance.now();
          await this.level!.batch(ops, JSON_LEVEL_ENCODING_OPTS);
          this.overlaySpillMs += performance.now() - spillStartedAt;
          this.levelBatchWrites += 1;
          this.bytesFlushed += spillBytes;
        }
      } catch (error) {
        this.spillError = error;
      } finally {
        const index = this.spillingOverlays.indexOf(spillingOverlay);
        if (index >= 0) this.spillingOverlays.splice(index, 1);
        this.pendingSpillBytes -= spillBytes;
      }
    });
  }

  async waitForSpills() {
    await this.spillChain;
    if (this.spillError !== undefined) {
      throw this.spillError instanceof Error
        ? this.spillError
        : new Error("MPF spill failed", { cause: this.spillError });
    }
  }

  private async waitForAncestorSpills() {
    if (this.parentStore === undefined) return;
    await this.parentStore.waitForSpills();
    await this.parentStore.waitForAncestorSpills();
  }

  shouldCloseLevel() {
    return this.ownsLevelLifecycle;
  }

  diagnostics(): Omit<MpfStoreDiagnostics, "entries"> {
    return {
      storePuts: this.storePuts,
      storeDels: this.storeDels,
      serialiseCalls: this.serialiseCalls,
      serialiseMs: this.serialiseMs,
      deferredMaterializedEstimatedBytes:
        this.deferredMaterializedEstimatedBytes,
      deferredMaterializedActualBytes: this.deferredMaterializedActualBytes,
      deferredLazyReads: this.deferredLazyReads,
      deferredLazySerialiseMs: this.deferredLazySerialiseMs,
      deferredLazySerialisedBytes: this.deferredLazySerialisedBytes,
      arenaCheckpointCalls: this.arenaCheckpointCalls,
      arenaCheckpointMs: this.arenaCheckpointMs,
      arenaCheckpointNodes: this.arenaCheckpointNodes,
      arenaCheckpointBytes: this.arenaCheckpointBytes,
      pathCacheEntries: this.blockPathCache?.size ?? 0,
      pathCacheBytes: this.blockPathCacheBytes,
      pathCacheHits: this.pathCacheHits,
      liveArenaPrunedNodes: this.liveArenaPrunedNodes,
      liveArenaPromotedNodes: this.liveArenaPromotedNodes,
      liveArenaPromotedBytes: this.liveArenaPromotedBytes,
      retainedSnapshotAuthentications: this.retainedSnapshotAuthentications,
      retainedSnapshotAuthenticationMs: this.retainedSnapshotAuthenticationMs,
      transientLiveNodes: this.transientLiveNodes.size,
      transientLiveBytes: this.transientLiveBytes,
      transientDirtyNodes: this.transientDirtyNodes.size,
      transientSnapshotsCaptured: this.transientSnapshotsCaptured,
      eventAtomicFinalizations: this.eventAtomicFinalizations,
      eventAtomicDirtyNodes: this.eventAtomicDirtyNodes,
      eventAtomicMaxDirtyNodes: this.eventAtomicMaxDirtyNodes,
      levelGets: this.levelGets,
      levelGetManyCalls: this.levelGetManyCalls,
      levelGetManyMaxKeys: this.levelGetManyMaxKeys,
      levelGetMs: this.levelGetMs,
      jsonCodecMs: this.jsonCodecMs,
      overlayHits: this.overlayHits,
      readCacheHits: this.readCacheHits,
      levelBatchWrites: this.levelBatchWrites,
      bytesFlushed: this.bytesFlushed,
      overlayEntries:
        (this.overlay?.size ?? 0) +
        (this.blockDeferredNodePuts?.size ?? 0) +
        this.transientLiveNodes.size +
        this.spillingOverlays.reduce((total, view) => total + view.size, 0),
      overlayBytes:
        this.overlayBytes +
        this.pendingSpillBytes +
        this.blockDeferredEstimatedBytes +
        this.transientLiveBytes,
      overlaySpills: this.overlaySpills,
      overlaySpillMs: this.overlaySpillMs,
      flushMs: this.flushMs,
    };
  }

  async hasStoredNode(root: Buffer): Promise<boolean> {
    const storageKey = root.toString("hex");
    const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
    if (deferredValue !== undefined) {
      const liveHash = (deferredValue as { readonly hash?: Buffer }).hash;
      if (liveHash === undefined || this.storageKey(liveHash) !== storageKey) {
        throw new Error(
          `Deferred MPF root is not content-addressed: expected=${storageKey},actual=${liveHash === undefined ? "undefined" : this.storageKey(liveHash)}`,
        );
      }
      return true;
    }
    if (this.blockDeferredNodeDeletes?.has(storageKey)) return false;
    return (await this.lookupStoredValue(storageKey)) !== undefined;
  }

  async checkpointDeferredNodes(): Promise<{
    readonly serializedNodes: number;
    readonly serializedBytes: number;
    readonly checkpointMs: number;
  }> {
    const serializedNodes = this.blockDeferredNodePuts?.size ?? 0;
    const actualBytesBefore = this.deferredMaterializedActualBytes;
    const startedAt = performance.now();
    await this.materializeDeferredNodes();
    const checkpointMs = performance.now() - startedAt;
    const serializedBytes =
      this.deferredMaterializedActualBytes - actualBytesBefore;
    this.arenaCheckpointCalls += 1;
    this.arenaCheckpointMs += checkpointMs;
    this.arenaCheckpointNodes += serializedNodes;
    this.arenaCheckpointBytes += serializedBytes;
    return { serializedNodes, serializedBytes, checkpointMs };
  }

  recordLiveArenaCheckpoint(checkpointMs: number) {
    this.arenaCheckpointCalls += 1;
    this.arenaCheckpointMs += checkpointMs;
  }

  currentOverlayView(): ReadonlyMap<string, MpfStoredValue> | undefined {
    return this.overlay;
  }

  private setOverlayValue(
    key: string,
    value: MpfStoredValue,
    encodedBytes?: number,
  ) {
    const previous = this.overlay!.get(key);
    if (previous !== undefined) {
      this.overlayBytes -=
        Buffer.byteLength(key) + (this.overlayValueBytes.get(key) ?? 0);
    }
    this.overlay!.set(key, value);
    const valueBytes =
      encodedBytes ??
      (this.level === undefined ? 0 : Buffer.byteLength(JSON.stringify(value)));
    this.overlayValueBytes.set(key, valueBytes);
    this.overlayBytes += Buffer.byteLength(key) + valueBytes;
    this.readCache.delete(key);
  }

  private async materializeDeferredNodes() {
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeDeletes === undefined ||
      (this.blockDeferredNodePuts.size === 0 &&
        this.blockDeferredNodeDeletes.size === 0)
    ) {
      return;
    }
    const puts: Extract<LevelBatchOp, { readonly type: "put" }>[] = [];
    let actualBytes = 0;
    for (const [key, value] of this.blockDeferredNodePuts) {
      this.assertLiveArenaNode(key, value);
      const serialiseStartedAt = performance.now();
      const serialized = value.serialise();
      this.serialiseMs += performance.now() - serialiseStartedAt;
      this.serialiseCalls += 1;
      let encodedBytes: number | undefined;
      if (this.level !== undefined) {
        const jsonCodecStartedAt = performance.now();
        encodedBytes = Buffer.byteLength(JSON.stringify(serialized));
        this.jsonCodecMs += performance.now() - jsonCodecStartedAt;
      }
      actualBytes +=
        Buffer.byteLength(key) +
        (encodedBytes ?? Buffer.byteLength(JSON.stringify(serialized)));
      puts.push({ type: "put", key, value: serialized, encodedBytes });
    }
    for (const key of this.blockDeferredNodeDeletes) {
      if (this.overlay!.has(key)) this.deleteOverlayValue(key);
    }
    for (const put of puts) {
      this.setOverlayValue(put.key, put.value, put.encodedBytes);
    }
    this.deferredMaterializedEstimatedBytes += this.blockDeferredEstimatedBytes;
    this.deferredMaterializedActualBytes += actualBytes;
    if (this.liveArenaEnabled) {
      this.liveArenaPromotedNodes += puts.length;
      this.liveArenaPromotedBytes += actualBytes;
    }
    this.blockDeferredNodePuts.clear();
    this.blockDeferredNodeDeletes.clear();
    this.blockDeferredNodeEstimates?.clear();
    this.blockDeferredEstimatedBytes = 0;
  }

  private estimateDeferredNodeBytes(
    key: string,
    value: MpfSerializableValue,
  ): number {
    const node = value as {
      readonly prefix?: string;
      readonly key?: Buffer;
      readonly value?: Buffer;
      readonly children?: readonly unknown[];
    };
    const keyBytes = Buffer.byteLength(key);
    const prefixBytes = Buffer.byteLength(node.prefix ?? "");
    if (Buffer.isBuffer(node.key) && Buffer.isBuffer(node.value)) {
      return (
        keyBytes +
        prefixBytes +
        256 +
        2 * node.key.length +
        2 * node.value.length
      );
    }
    if (Array.isArray(node.children)) {
      return keyBytes + prefixBytes + 2_048;
    }
    return keyBytes + 4_096;
  }

  private deleteOverlayValue(key: string) {
    const previous = this.overlay!.get(key);
    if (previous !== undefined) {
      this.overlayBytes -=
        Buffer.byteLength(key) + (this.overlayValueBytes.get(key) ?? 0);
    }
    this.overlay!.delete(key);
    this.overlayValueBytes.delete(key);
  }

  private cacheRead(key: string, value: MpfStoredValue) {
    this.readCache.delete(key);
    this.readCache.set(key, value);
    if (this.readCache.size > 4_096) {
      const oldest = this.readCache.keys().next().value as string | undefined;
      if (oldest !== undefined) this.readCache.delete(oldest);
    }
  }

  private cacheBlockPathNode(key: string, value: MpfStoredValue) {
    if (!this.liveArenaEnabled || this.blockPathCache === undefined) return;
    if (this.blockPathCache.has(key)) return;
    const bytes = Buffer.byteLength(key) + estimateMpfStoredValueBytes(value);
    const nextNodes = this.blockPathCache.size + 1;
    const nextBytes = this.blockPathCacheBytes + bytes;
    if (
      nextNodes > this.arenaLimits.pathCacheMaxNodes ||
      nextBytes > this.arenaLimits.pathCacheMaxBytes
    ) {
      throw new Error(
        `MPF block path cache limit exceeded: nodes=${nextNodes.toString()}/${this.arenaLimits.pathCacheMaxNodes.toString()},bytes=${nextBytes.toString()}/${this.arenaLimits.pathCacheMaxBytes.toString()}`,
      );
    }
    this.blockPathCache.set(key, value);
    this.blockPathCacheBytes = nextBytes;
  }

  private assertLiveArenaNode(key: string, value: MpfSerializableValue) {
    const node = value as MpfSerializableValue & {
      readonly hash?: Buffer;
      readonly assertHydratedNodeHashes?: (maxDepth: number) => {
        readonly verifiedNodes: number;
      };
    };
    if (node.hash === undefined || this.storageKey(node.hash) !== key) {
      throw new Error(
        `Live MPF arena node is not content-addressed: expected=${key},actual=${node.hash === undefined ? "undefined" : this.storageKey(node.hash)}`,
      );
    }
    this.authenticateHydratedNodeOnce(node);
  }

  private assertLiveArenaCaps() {
    const nodes =
      (this.blockDeferredNodePuts?.size ?? 0) + this.transientLiveNodes.size;
    const bytes = this.blockDeferredEstimatedBytes + this.transientLiveBytes;
    if (
      nodes > this.arenaLimits.liveArenaMaxNodes ||
      bytes > this.arenaLimits.liveArenaMaxBytes
    ) {
      throw new Error(
        `MPF live arena limit exceeded: nodes=${nodes.toString()}/${this.arenaLimits.liveArenaMaxNodes.toString()},bytes=${bytes.toString()}/${this.arenaLimits.liveArenaMaxBytes.toString()}`,
      );
    }
  }

  private candidateChildKeys(value: MpfReadableValue): readonly string[] {
    if (typeof value === "string") return [];
    if ("serialise" in value) {
      const children = (
        value as MpfSerializableValue & {
          readonly children?: readonly (
            | { readonly hash?: Buffer }
            | undefined
          )[];
        }
      ).children;
      return (
        children?.flatMap((child) =>
          child?.hash === undefined ? [] : [this.storageKey(child.hash)],
        ) ?? []
      );
    }
    if (value.__kind !== "Branch" || !Array.isArray(value.children)) return [];
    return value.children.flatMap((child) =>
      typeof child === "string" ? [child] : [],
    );
  }

  private findNonDurableCandidate(key: string): MpfReadableValue | undefined {
    const live = this.blockDeferredNodePuts?.get(key);
    if (live !== undefined) {
      this.assertLiveArenaNode(key, live);
      return live;
    }
    const serialized = this.overlay?.get(key);
    if (serialized !== undefined) return serialized;
    for (let index = this.spillingOverlays.length - 1; index >= 0; index -= 1) {
      const spilling = this.spillingOverlays[index]!;
      const candidate = spilling.get(key);
      if (candidate !== undefined) return candidate;
    }
    return this.parentStore?.findNonDurableCandidate(key);
  }

  private importReachableParentCandidates(root: Buffer) {
    if (this.parentStore === undefined) return;
    const visited = new Set<string>();
    const pending = [this.storageKey(root)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (visited.has(key)) continue;
      visited.add(key);
      let candidate: MpfReadableValue | undefined =
        this.blockDeferredNodePuts?.get(key) ?? this.overlay?.get(key);
      if (candidate === undefined) {
        candidate = this.parentStore.findNonDurableCandidate(key);
        if (candidate === undefined) continue;
        if (typeof candidate !== "string" && "serialise" in candidate) {
          const clone = (
            candidate as MpfSerializableValue & {
              readonly cloneDetached?: () => MpfSerializableValue;
            }
          ).cloneDetached?.();
          if (clone === undefined || clone === candidate) {
            throw new Error(
              `Cannot import mutable MPF parent arena node ${key}`,
            );
          }
          const priorEstimate = this.blockDeferredNodeEstimates?.get(key) ?? 0;
          const estimate = this.estimateDeferredNodeBytes(key, clone);
          this.blockDeferredNodePuts!.set(key, clone);
          this.blockDeferredNodeEstimates!.set(key, estimate);
          this.blockDeferredEstimatedBytes += estimate - priorEstimate;
          candidate = clone;
        } else {
          this.setOverlayValue(key, candidate as MpfStoredValue);
        }
      }
      for (const childKey of this.candidateChildKeys(candidate)) {
        pending.push(childKey);
      }
    }
    if (this.liveArenaEnabled) this.assertLiveArenaCaps();
  }

  private pruneLiveArenaToRoot(root: Buffer) {
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeEstimates === undefined
    ) {
      return;
    }
    const reachable = new Set<string>();
    const pending = [this.storageKey(root)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (reachable.has(key)) continue;
      const value =
        this.blockDeferredNodePuts.get(key) ??
        this.overlay?.get(key) ??
        this.blockPathCache?.get(key);
      if (value === undefined) continue;
      if (typeof value !== "string" && "serialise" in value) {
        this.assertLiveArenaNode(key, value as MpfSerializableValue);
        reachable.add(key);
      }
      for (const childKey of this.candidateChildKeys(value)) {
        pending.push(childKey);
      }
    }
    for (const key of [...this.blockDeferredNodePuts.keys()]) {
      if (reachable.has(key)) continue;
      this.blockDeferredNodePuts.delete(key);
      this.blockDeferredNodeDeletes?.delete(key);
      this.blockDeferredEstimatedBytes -=
        this.blockDeferredNodeEstimates.get(key) ?? 0;
      this.blockDeferredNodeEstimates.delete(key);
      this.liveArenaPrunedNodes += 1;
    }
    this.assertLiveArenaCaps();
  }

  private async lookupStoredValue(
    storageKey: string,
  ): Promise<MpfReadableValue | undefined> {
    const activeMutationValue = this.deferredNodePuts?.get(storageKey);
    if (activeMutationValue !== undefined) {
      if (this.liveArenaEnabled) {
        this.assertLiveArenaNode(storageKey, activeMutationValue);
        return activeMutationValue;
      }
      return activeMutationValue.serialise();
    }
    const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
    if (deferredValue !== undefined) {
      this.assertLiveArenaNode(storageKey, deferredValue);
      if (this.liveArenaEnabled) return deferredValue;
      const startedAt = performance.now();
      const serialized = deferredValue.serialise();
      const elapsedMs = performance.now() - startedAt;
      const serializedBytes = Buffer.byteLength(JSON.stringify(serialized));
      this.deferredLazyReads += 1;
      this.deferredLazySerialiseMs += elapsedMs;
      this.deferredLazySerialisedBytes += serializedBytes;
      this.serialiseCalls += 1;
      this.serialiseMs += elapsedMs;
      return serialized;
    }
    if (this.overlay?.has(storageKey)) {
      return this.overlay.get(storageKey);
    }
    for (let index = this.spillingOverlays.length - 1; index >= 0; index -= 1) {
      const spilling = this.spillingOverlays[index]!;
      if (spilling.has(storageKey)) return spilling.get(storageKey);
    }
    if (this.parentOverlay?.has(storageKey)) {
      return this.parentOverlay.get(storageKey);
    }
    if (this.parentStore !== undefined) {
      return this.parentStore.lookupStoredValue(storageKey);
    }
    if (this.blockPathCache?.has(storageKey)) {
      this.pathCacheHits += 1;
      return this.blockPathCache.get(storageKey);
    }
    if (this.readCache.has(storageKey)) {
      return this.readCache.get(storageKey);
    }
    if (this.level !== undefined) {
      const startedAt = performance.now();
      const value = await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
      this.levelGetMs += performance.now() - startedAt;
      this.levelGets += 1;
      this.cacheRead(storageKey, value);
      this.cacheBlockPathNode(storageKey, value);
      return value;
    }
    return this.memory?.get(storageKey);
  }

  private registerFork() {
    this.assertUsable();
    if (this.promotionReserved) {
      throw new Error("Cannot fork an MPF overlay while promotion is reserved");
    }
    this.openForks += 1;
  }

  private reservePromotionChain(): readonly MidgardMpfRootViewStore[] {
    const reserved: MidgardMpfRootViewStore[] = [];
    // The traversal intentionally starts at this store and walks its parents.
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let current: MidgardMpfRootViewStore | undefined = this;
    try {
      while (current !== undefined) {
        if (current.promotionReserved) {
          throw new Error("MPF promotion chain is already reserved");
        }
        if (current !== this && current.openForks !== 1) {
          throw new Error(
            `Cannot promote an MPF fork while an ancestor has ${current.openForks.toString()} open children`,
          );
        }
        current.promotionReserved = true;
        reserved.push(current);
        current = current.parentStore;
      }
      return reserved;
    } catch (cause) {
      this.releasePromotionChain(reserved);
      throw cause;
    }
  }

  private releasePromotionChain(reserved: readonly MidgardMpfRootViewStore[]) {
    for (const store of reserved) store.promotionReserved = false;
  }

  private transferOwnershipToChild(root: Buffer) {
    if (this.openForks !== 1) {
      throw new Error(
        `Cannot transfer MPF ownership without exactly one active child: open_forks=${this.openForks.toString()}`,
      );
    }
    this.parentStore?.transferOwnershipToChild(root);
    this.closeFork();
    this.currentRoot = Buffer.from(root);
    this.overlay = undefined;
    this.overlayValueBytes = new Map();
    this.blockDeferredNodePuts = undefined;
    this.blockDeferredNodeDeletes = undefined;
    this.blockDeferredNodeEstimates = undefined;
    this.blockDeferredEstimatedBytes = 0;
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
    this.overlayBaseRoot = undefined;
    this.overlayBytes = 0;
    this.readCache.clear();
    this.invalidatedByChildPromotion = true;
    this.ownsLevelLifecycle = false;
  }

  private assertUsable() {
    if (this.poisoned) {
      throw new Error(
        "MPF overlay is poisoned after a failed mutation and must be reloaded",
      );
    }
    if (this.invalidatedByChildPromotion) {
      throw new Error(
        "MPF handle is stale because durable-root ownership transferred to a promoted child fork",
      );
    }
    if (this.promotionReserved) {
      throw new Error("MPF handle is reserved for promotion");
    }
  }

  private closeFork() {
    if (this.parentStore !== undefined && !this.forkClosed) {
      this.forkClosed = true;
      this.parentStore.openForks -= 1;
    }
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
