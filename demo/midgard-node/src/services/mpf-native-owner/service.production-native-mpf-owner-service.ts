import { timingSafeEqual } from "node:crypto";
import { getHeapStatistics } from "node:v8";
import { MessageChannel, type MessagePort } from "node:worker_threads";

import type {
  Ledger,
  LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type { Level } from "level";

import {
  createEventFlatDigest,
  prepareEventFlatDigest,
} from "../../workers/utils/mpf-event-flat-digest.js";
import {
  assertNativeMpfGenerationHandle,
  assertNativeMpfHashHex,
  NATIVE_MPF_APPLY_MAX_NODE_ROUNDS,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_REPLAY_SCHEMA,
  type NativeMpfApplyResult,
  type NativeMpfCanonicalRootRecovery,
  NativeMpfClosureIncomplete,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerDiagnostics,
  type NativeMpfOwnerService,
  NativeMpfRpcKind,
  type PersistedNativeMpfReplay,
} from "./protocol.js";
import { encodeStoredNode } from "./service.encode-stored-node.js";
import {
  createOwnerLedgerLock,
  type LedgerRetention,
} from "./service.ledger-lock.js";
import {
  assertPinnedOwnerBinary,
  NativeChildRpc,
} from "./service.native-child-rpc.js";
import {
  assertNativeOwnerRuntimeMemoryBudget,
  assertStoredHash,
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
  HASH_BYTES,
  type NativeMpfOwnerServiceOptions,
  type NormalizedNativeMpfOwnerServiceOptions,
  normalizeOwnerOptions,
  PROMOTION_DIGEST_DOMAIN,
  readCgroupMemoryBudget,
  type StoredValue,
  type WorkerGenerationLease,
} from "./service.normalize-owner-options.js";
import type { NativeOwnerLedger } from "./service.open-ledger.js";
import {
  parsePromotionRecords,
  restoreRefusal,
} from "./service.parse-promotion-records.js";
import { readWorkerNodes } from "./service.read-worker-nodes.js";
import {
  type NativeOwnerRestartHealth,
  NativeOwnerRestartPolicy,
} from "./service.restart-policy.js";
import {
  decodeDiagnostics,
  decodeNeedNodes,
  decodeNodesProvided,
  encodeProvideNodes,
} from "./service.rpc-payloads.js";
import {
  setChildRoot,
  startNativeChild,
} from "./service.start-native-child.js";
import { validateEventLog } from "./service.validate-event-log.js";

export class ProductionNativeMpfOwnerService implements NativeMpfOwnerService {
  private readonly workerPorts = new Set<MessagePort>();
  private readonly workerPortLastRequestId = new Map<MessagePort, number>();
  private readonly workerGenerationLeases = new Map<
    string,
    WorkerGenerationLease
  >();
  private childRestarts = 0;
  private readonly restartPolicy: NativeOwnerRestartPolicy;
  private restartPromise: Promise<void> | undefined;
  private closing = false;
  private activeOperations = 0;
  private restoration: Promise<void> | undefined;
  private recoveryFailure: Error | undefined;
  private lastChildError: Error | undefined;
  /** The last apply's settling, which the next apply waits for. */
  private applyTail: Promise<unknown> = Promise.resolve();
  private readonly ledgerLock: ReturnType<typeof createOwnerLedgerLock>;

  private constructor(
    private readonly db: Level<string, StoredValue>,
    private readonly store: LedgerStore,
    private rpc: NativeChildRpc,
    private readonly binarySha256: string,
    private durableRoot: string,
    private readonly options: NormalizedNativeMpfOwnerServiceOptions,
  ) {
    this.restartPolicy = new NativeOwnerRestartPolicy(options);
    this.ledgerLock = createOwnerLedgerLock(store);
  }

  /** Takes `ledger` over: it is closed when creation fails, and by `close`. */
  public static async create(
    { level: db, store }: NativeOwnerLedger,
    options: NativeMpfOwnerServiceOptions,
  ): Promise<ProductionNativeMpfOwnerService> {
    let rpc: NativeChildRpc | undefined;
    try {
      await prepareEventFlatDigest();
      const normalized = normalizeOwnerOptions(options);
      assertNativeOwnerRuntimeMemoryBudget({
        cgroup: await readCgroupMemoryBudget(),
        v8HeapLimitBytes: getHeapStatistics().heap_size_limit,
      });
      assertNativeMpfHashHex(normalized.binarySha256, "binarySha256");
      await assertPinnedOwnerBinary(
        normalized.binaryPath,
        normalized.binarySha256,
      );
      const marker = assertStoredHash(await db.get("__root__"), "durableRoot");
      rpc = await startNativeChild({ options: normalized, root: marker });
      const service = new ProductionNativeMpfOwnerService(
        db,
        store,
        rpc,
        normalized.binarySha256,
        marker,
        normalized,
      );
      service.installFailureHandler(rpc);
      return service;
    } catch (error) {
      await rpc?.close().catch(() => undefined);
      await db.close();
      throw error;
    }
  }

  public async fork(baseRoot: string): Promise<NativeMpfGenerationHandle> {
    return this.runOperation(async () => {
      const rpc = await this.ensureRpc();
      assertNativeMpfHashHex(baseRoot, "baseRoot");
      if (baseRoot !== this.durableRoot) {
        throw new Error(
          `Native MPF fork base is stale: requested=${baseRoot},durable=${this.durableRoot}`,
        );
      }
      const response = await rpc.request(
        NativeMpfRpcKind.Fork,
        Buffer.from(baseRoot, "hex"),
        new Set([NativeMpfRpcKind.Forked]),
      );
      const payload = Buffer.from(response.payload);
      if (
        payload.length !== 48 ||
        payload.subarray(16).toString("hex") !== baseRoot
      ) {
        throw new Error("Native MPF Forked payload is invalid");
      }
      return {
        ownerEpoch: rpc.epoch,
        generationId: Buffer.from(payload.subarray(0, 16)),
        baseRoot,
      };
    });
  }

  public async applyEvents(
    handle: NativeMpfGenerationHandle,
    eventLog: Uint8Array,
  ): Promise<NativeMpfApplyResult> {
    return this.runOperation(async () => {
      const rpc = await this.ensureRpc();
      this.assertOwnedHandle(handle, rpc);
      const log = Buffer.from(eventLog);
      const eventCount = validateEventLog(handle.baseRoot, log);
      // One apply at a time: the child trims its cache when an apply ends,
      // which between two rounds of another apply could drop the nodes that
      // apply was just given.
      const applied = this.applyTail.then(() =>
        this.applyWithNodes(rpc, handle, log),
      );
      this.applyTail = applied.catch(() => undefined);
      const payload = Buffer.from((await applied).payload);
      const rootsEnd = 84 + eventCount * HASH_BYTES;
      const expectedBytes = rootsEnd + 16;
      if (
        payload.length !== expectedBytes ||
        !timingSafeEqual(
          payload.subarray(0, 16),
          Buffer.from(handle.generationId),
        ) ||
        payload.readUInt32LE(80) !== eventCount
      ) {
        throw new Error("Native MPF Applied payload count/handle is invalid");
      }
      const candidateRoot = payload.subarray(16, 48).toString("hex");
      assertNativeMpfHashHex(candidateRoot, "candidateRoot");
      const expectedDigest = digest(EVENT_LOG_DIGEST_DOMAIN, log);
      if (!timingSafeEqual(payload.subarray(48, 80), expectedDigest)) {
        throw new Error("Native MPF Applied event-log digest mismatch");
      }
      const readDurationNs = (offset: number, field: string): number => {
        const value = payload.readBigUInt64LE(offset);
        if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
          throw new Error(`Native MPF Applied ${field} exceeds safe integer`);
        }
        return Number(value);
      };
      return {
        handle,
        candidateRoot,
        eventLogDigest: expectedDigest.toString("hex"),
        eventRoots: Array.from({ length: eventCount }, (_, index) =>
          payload
            .subarray(84 + index * HASH_BYTES, 84 + (index + 1) * HASH_BYTES)
            .toString("hex"),
        ),
        proofArenaDurationNs: readDurationNs(rootsEnd, "proofArenaDurationNs"),
        mutationDurationNs: readDurationNs(rootsEnd + 8, "mutationDurationNs"),
      };
    });
  }

  /** Sends ApplyEvents until the child answers Applied. Each NeedNodes is
   * answered with those nodes read from the store in one `getMany`, and the
   * same ApplyEvents is sent again. No lock is needed for the read: the
   * child walks only from its root, the durable root, which the store keeps
   * retained with every node under it; if the durable root moved meanwhile,
   * the child refuses the stale apply anyway. */
  private async applyWithNodes(
    rpc: NativeChildRpc,
    handle: NativeMpfGenerationHandle,
    log: Buffer,
  ) {
    const request = Buffer.concat([Buffer.from(handle.generationId), log]);
    for (let round = 0; ; round += 1) {
      const response = await rpc.request(
        NativeMpfRpcKind.ApplyEvents,
        request,
        new Set([NativeMpfRpcKind.Applied, NativeMpfRpcKind.NeedNodes]),
        false,
        NATIVE_MPF_OWNER_DEFAULT_CAPS.applyTimeoutMs,
      );
      if (response.kind === NativeMpfRpcKind.Applied) return response;
      if (round === NATIVE_MPF_APPLY_MAX_NODE_ROUNDS) {
        throw new Error(
          `Native MPF apply from ${handle.baseRoot} still needs nodes after ${NATIVE_MPF_APPLY_MAX_NODE_ROUNDS.toString()} rounds`,
        );
      }
      const hashes = decodeNeedNodes(response.payload, handle.generationId);
      const nodes = await this.db.getMany(hashes);
      const missing = hashes.filter((_, index) => nodes[index] === undefined);
      if (missing.length > 0) {
        if (handle.baseRoot !== this.durableRoot)
          throw new Error(
            `Native MPF apply base is stale: base=${handle.baseRoot},durable=${this.durableRoot}`,
          );
        throw new NativeMpfClosureIncomplete(handle.baseRoot, missing);
      }
      const records = hashes.map((hash, index) =>
        encodeStoredNode(hash, nodes[index]),
      );
      for (const { payload, count } of encodeProvideNodes(
        records,
        this.options.maxChunkBytes,
      )) {
        const provided = await rpc.request(
          NativeMpfRpcKind.ProvideNodes,
          payload,
          new Set([NativeMpfRpcKind.NodesProvided]),
        );
        if (decodeNodesProvided(provided.payload) !== count) {
          throw new Error("Native MPF NodesProvided count mismatch");
        }
      }
    }
  }

  public async discard(handle: NativeMpfGenerationHandle): Promise<void> {
    return this.runOperation(async () => {
      const rpc = await this.ensureRpc();
      this.assertOwnedHandle(handle, rpc);
      const response = await rpc.request(
        NativeMpfRpcKind.Discard,
        handle.generationId,
        new Set([NativeMpfRpcKind.Discarded]),
      );
      if (
        !timingSafeEqual(
          Buffer.from(response.payload),
          Buffer.from(handle.generationId),
        )
      ) {
        throw new Error("Native MPF Discarded handle mismatch");
      }
      this.workerGenerationLeases.delete(this.generationKey(handle));
    });
  }

  public async promote(handle: NativeMpfGenerationHandle): Promise<void> {
    return this.runOperation(async () => {
      const rpc = await this.ensureRpc();
      this.assertOwnedHandle(handle, rpc);
      if (handle.baseRoot !== this.durableRoot) {
        throw new Error("Native MPF promotion base is stale");
      }
      const { frame, bytes } = await rpc.promotion(
        handle.generationId,
        NATIVE_MPF_OWNER_DEFAULT_CAPS.promotionTimeoutMs,
      );
      const payload = Buffer.from(frame.payload);
      if (
        payload.length !== 116 ||
        !timingSafeEqual(
          payload.subarray(0, 16),
          Buffer.from(handle.generationId),
        ) ||
        payload.subarray(16, 48).toString("hex") !== handle.baseRoot
      ) {
        throw new Error("Native MPF PromotionEnd handle/base is invalid");
      }
      const candidateRoot = payload.subarray(48, 80).toString("hex");
      const recordCount = payload.readUInt32LE(80);
      const records = parsePromotionRecords(bytes);
      if (
        records.length !== recordCount ||
        recordCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxGeneratedNodes
      ) {
        throw new Error(
          "Native MPF promotion record count mismatch/cap breach",
        );
      }
      for (let index = 1; index < records.length; index += 1) {
        if (records[index - 1]!.hashHex >= records[index]!.hashHex) {
          throw new Error(
            "Native MPF promotion records are not uniquely hash-sorted",
          );
        }
      }
      const aggregate = createEventFlatDigest();
      aggregate
        .update(PROMOTION_DIGEST_DOMAIN)
        .update(Buffer.from(handle.baseRoot, "hex"))
        .update(Buffer.from(candidateRoot, "hex"));
      aggregate.update(bytes);
      if (!timingSafeEqual(aggregate.digest(), payload.subarray(84, 116))) {
        throw new Error("Native MPF promotion aggregate digest mismatch");
      }
      const marker = assertStoredHash(
        await this.db.get("__root__"),
        "durableRoot",
      );
      if (marker !== handle.baseRoot) {
        throw new Error(
          `Native MPF durable marker changed before promotion: expected=${handle.baseRoot},actual=${marker}`,
        );
      }
      // From the write to the durable root's move, under the ledger lock: a
      // `retain` reads the durable root before the write or after the move.
      await this.ledgerLock.run(async () => {
        await this.options.faultInjectionForTests?.("before_promotion_batch");
        // One write of nodes, counts, retained root and `__root__` marker.
        await this.store.adopt(candidateRoot, {
          nodes: new Map(
            records.map(({ hashHex, stored }) => [hashHex, stored]),
          ),
        });
        await this.options.faultInjectionForTests?.(
          "after_promotion_batch_before_ack",
        );
        const committed = await rpc.request(
          NativeMpfRpcKind.PromotionCommitted,
          handle.generationId,
          new Set([NativeMpfRpcKind.PromotionCommitted]),
        );
        if (Buffer.from(committed.payload).toString("hex") !== candidateRoot) {
          throw new Error("Native MPF PromotionCommitted root mismatch");
        }
        this.durableRoot = candidateRoot;
        this.workerGenerationLeases.delete(this.generationKey(handle));
      });
    });
  }

  /**
   * Keeps the ledgers of `roots`, of the durable root, of the `__root__`
   * marker (ahead of the durable root after a promotion or a restore whose
   * acknowledgement failed) and of every worker's leased fork base, and drops
   * every other one (`LedgerStore.retain`). It refuses only once the owner is
   * closed: a refusing owner still keeps every root it could restart from.
   */
  public retain(roots: ReadonlySet<string>): Promise<LedgerRetention> {
    const requested = new Set(roots);
    return this.ledgerLock.retain(async () => {
      if (this.closing) throw new Error("Native MPF owner service is closed");
      requested.add(this.durableRoot);
      requested.add(assertStoredHash(await this.db.get("__root__"), "marker"));
      for (const { handle } of this.workerGenerationLeases.values())
        requested.add(handle.baseRoot);
      return requested;
    });
  }

  public ledger(root: string): Promise<Ledger> {
    return this.store.at(root);
  }

  /** Outside the ledger lock: a persist moves no durable root, and the
   * store orders it with a promotion's adoption and a `retain`'s
   * replacement of the retained set on its own write lock. */
  public persistLedger(ledger: Ledger): Promise<void> {
    return this.store.persist(ledger);
  }

  public async recover(replay: PersistedNativeMpfReplay): Promise<void> {
    return this.runOperation(async () => {
      if (replay.schema !== NATIVE_MPF_REPLAY_SCHEMA) {
        throw new Error("Native MPF replay schema mismatch");
      }
      if (replay.ownerBinarySha256 !== this.binarySha256) {
        throw new Error("Native MPF replay binary SHA-256 mismatch");
      }
      const log = Buffer.from(replay.eventLog);
      const digestHex = digest(EVENT_LOG_DIGEST_DOMAIN, log).toString("hex");
      if (digestHex !== replay.eventLogDigest) {
        throw new Error("Native MPF replay event-log digest mismatch");
      }
      if (replay.baseRoot !== this.durableRoot) {
        if (replay.candidateRoot === this.durableRoot) return;
        throw new Error(
          "Native MPF replay marker is neither base nor candidate",
        );
      }
      const handle = await this.fork(replay.baseRoot);
      try {
        const applied = await this.applyEvents(handle, log);
        if (
          applied.candidateRoot !== replay.candidateRoot ||
          applied.eventRoots.length !== replay.eventCount ||
          !timingSafeEqual(
            Buffer.from(applied.eventRoots.join(""), "hex"),
            Buffer.from(replay.eventRoots),
          )
        ) {
          throw new Error(
            "Native MPF replay roots diverged from durable journal",
          );
        }
        await this.promote(handle);
      } catch (error) {
        await this.discard(handle).catch(() => undefined);
        throw error;
      }
    });
  }

  public async diagnostics(): Promise<NativeMpfOwnerDiagnostics> {
    // Not counted as an operation: a restore refuses under one, and a
    // readiness probe must not make it refuse.
    this.assertCanOperate();
    const rpc = await this.ensureRpc();
    const response = await rpc.request(
      NativeMpfRpcKind.Diagnostics,
      Buffer.alloc(0),
      new Set([NativeMpfRpcKind.DiagnosticsResult]),
    );
    return {
      ownerEpoch: rpc.epoch,
      ...decodeDiagnostics(response.payload),
      childRestarts: this.childRestarts,
    };
  }

  /** Restore only a root the store retains. The caller must persist and
   * authenticate the recovery plan and drain producers before this operation.
   * SQL reconciliation and Ready publication remain the caller's responsibility.
   * The child keeps its epoch; every handle from before the restore names a
   * generation the child no longer holds, since the restore requires none.
   * In-flight read-only diagnostics (readiness, audits) finish on the same
   * child; new operations are refused from this call until the restore settles.
   */
  public restoreCanonicalRoot(
    plan: NativeMpfCanonicalRootRecovery,
  ): Promise<void> {
    let captured: NativeMpfCanonicalRootRecovery;
    try {
      captured = {
        recoveryId: plan.recoveryId,
        expectedRoot: plan.expectedRoot,
        targetRoot: plan.targetRoot,
      };
      this.assertCanOperate();
      if (this.activeOperations !== 0 || this.restartPromise !== undefined)
        throw new Error(
          "Native MPF canonical recovery requires drained operations",
        );
      assertNativeMpfHashHex(captured.recoveryId, "recoveryId");
      assertNativeMpfHashHex(captured.expectedRoot, "expectedRoot");
      assertNativeMpfHashHex(captured.targetRoot, "targetRoot");
    } catch (error) {
      return Promise.reject(
        error instanceof Error ? error : new Error(String(error)),
      );
    }
    // Under the ledger lock once every earlier `retain` has replaced the
    // retained set: the target found retained stays so until the marker and
    // the durable root name it, since a later `retain` reads them after.
    const restore = this.ledgerLock.run(async () => {
      await this.ledgerLock.deleted();
      await this.restoreRetainedRoot(captured);
    });
    this.restoration = restore;
    void restore
      .finally(() => {
        if (this.restoration === restore) this.restoration = undefined;
      })
      .catch(() => undefined);
    return restore;
  }

  private async restoreRetainedRoot(
    plan: NativeMpfCanonicalRootRecovery,
  ): Promise<void> {
    const record = JSON.stringify({
      recoveryId: plan.recoveryId,
      expectedRoot: plan.expectedRoot,
      targetRoot: plan.targetRoot,
    });
    const rpc = await this.ensureRpc();
    const marker = assertStoredHash(
      await this.db.get("__root__"),
      "durableRoot",
    );
    const recordKey = `__canonical_recovery__:${plan.recoveryId}`;
    const previous = await this.db.get(recordKey);
    if (previous !== undefined && previous !== record)
      throw new Error(
        "Native MPF canonical recovery identifier conflicts with retained plan",
      );
    if (marker === plan.targetRoot && previous === record) {
      if (this.durableRoot !== marker)
        throw new Error(
          "Native MPF canonical recovery is committed but not installed yet",
        );
      return;
    }
    if (marker !== plan.expectedRoot || this.durableRoot !== plan.expectedRoot)
      throw new Error("Native MPF canonical recovery base changed");
    // A root `retain` dropped is refused even while its nodes are still
    // queued for deletion: the marker never names a root the store may delete.
    await this.store.at(plan.targetRoot).catch((cause: unknown) => {
      throw restoreRefusal(plan.targetRoot, cause);
    });
    const diagnostics = await rpc.request(
      NativeMpfRpcKind.Diagnostics,
      Buffer.alloc(0),
      new Set([NativeMpfRpcKind.DiagnosticsResult]),
    );
    if (decodeDiagnostics(diagnostics.payload).activeGenerations !== 0)
      throw new Error(
        "Native MPF canonical recovery requires drained generations",
      );
    let committed = false;
    try {
      await this.options.faultInjectionForTests?.("before_root_restore_batch");
      // The store retains the target (checked above), so moving only the
      // marker leaves its counts and retained roots consistent.
      await this.db.batch(
        [
          { type: "put", key: recordKey, value: record },
          { type: "put", key: "__root__", value: plan.targetRoot },
        ],
        { sync: true },
      );
      committed = true;
      await this.options.faultInjectionForTests?.(
        "after_root_restore_batch_before_ack",
      );
      // The child refuses while a generation is active; none can be, as
      // operations stay refused until the restore settles.
      await setChildRoot(rpc, plan.targetRoot);
      this.durableRoot = plan.targetRoot;
      this.workerGenerationLeases.clear();
    } catch (error) {
      // Committed but not installed: the owner refuses work until a restart
      // gives a new child the durable root, the target.
      if (committed)
        this.recoveryFailure =
          error instanceof Error ? error : new Error(String(error));
      throw error;
    }
  }

  /** Why every operation refuses right now, or undefined: the owner holds
   * until the node restarts (`NativeOwnerRestartPolicy`), or a committed
   * canonical recovery is not installed yet. Each call also starts a restart
   * that is due, which starts a new child at the durable root marker as a
   * process start does; one that succeeds clears a pending recovery. */
  public terminalFailure(): Error | undefined {
    this.resumeRestart();
    return this.refusal();
  }

  public restartHealth(): NativeOwnerRestartHealth {
    return this.restartPolicy.health();
  }

  private refusal(): Error | undefined {
    const held = this.restartPolicy.held();
    if (held !== undefined) return held;
    if (this.recoveryFailure !== undefined)
      return new Error(
        "Native MPF canonical recovery is not installed yet; the owner restarts from its durable root",
        { cause: this.recoveryFailure },
      );
    return undefined;
  }

  /** Starts a child restart from the durable root when one is due: the child
   * is gone or a committed recovery was not installed, and neither a restart,
   * a restoration nor a hold is in the way. */
  private resumeRestart(): void {
    if (
      this.closing ||
      this.restartPromise !== undefined ||
      this.restoration !== undefined ||
      (this.recoveryFailure === undefined && !this.rpc.isClosed) ||
      this.restartPolicy.held() !== undefined
    )
      return;
    void this.scheduleRestart().catch(() => undefined);
  }

  private assertCanOperate(): void {
    if (this.closing) throw new Error("Native MPF owner service is closed");
    const refusal = this.refusal();
    if (refusal !== undefined) {
      this.resumeRestart();
      throw refusal;
    }
    if (this.restoration !== undefined)
      throw new Error("Native MPF canonical recovery is in progress");
  }

  private async runOperation<A>(work: () => Promise<A>): Promise<A> {
    this.assertCanOperate();
    this.activeOperations += 1;
    try {
      return await work();
    } finally {
      this.activeOperations -= 1;
    }
  }

  public createWorkerPort(): MessagePort {
    this.assertCanOperate();
    const channel = new MessageChannel();
    this.workerPorts.add(channel.port1);
    this.workerPortLastRequestId.set(channel.port1, 0);
    channel.port1.on("message", (message: unknown) => {
      void this.handleWorkerMessage(channel.port1, message);
    });
    channel.port1.once("close", () => {
      this.workerPorts.delete(channel.port1);
      this.workerPortLastRequestId.delete(channel.port1);
      void this.releaseWorkerLeases(channel.port1);
    });
    channel.port1.start();
    return channel.port2;
  }

  public async close(): Promise<void> {
    this.closing = true;
    for (const port of this.workerPorts) port.close();
    this.workerPorts.clear();
    this.workerPortLastRequestId.clear();
    this.workerGenerationLeases.clear();
    await this.restoration?.catch(() => undefined);
    this.restartPolicy.cancel();
    await this.restartPromise?.catch(() => undefined);
    await this.rpc.close();
    await this.db.close();
  }

  private installFailureHandler(rpc: NativeChildRpc): void {
    rpc.setFailureHandler((error) => {
      this.lastChildError = error;
      this.workerGenerationLeases.clear();
      if (this.restartPolicy.recordFailure(error, "death")) return;
      if (this.restoration === undefined && this.recoveryFailure === undefined)
        void this.scheduleRestart().catch(() => undefined);
    });
  }

  /** Restarts the child from the durable root marker after the policy's
   * backoff; refused once the owner holds. */
  private scheduleRestart(): Promise<void> {
    if (this.closing) {
      return Promise.reject(new Error("Native MPF owner service is closing"));
    }
    if (this.restartPromise !== undefined) return this.restartPromise;
    const held = this.restartPolicy.held();
    if (held !== undefined) return Promise.reject(held);
    const delayMs = this.restartPolicy.startRestart();
    // A close during the backoff ends the wait; it must not start a child.
    const restart = this.restartPolicy.wait(delayMs).then(() => {
      if (this.closing)
        throw new Error(
          "Native MPF owner service closed during restart backoff",
        );
      return this.restartChild();
    });
    this.restartPromise = restart;
    void restart.then(
      () => {
        this.restartPolicy.recordStarted();
        if (this.restartPromise === restart) this.restartPromise = undefined;
      },
      (restartError: unknown) => {
        this.lastChildError =
          restartError instanceof Error
            ? restartError
            : new Error(String(restartError));
        this.restartPolicy.recordFailure(this.lastChildError, "restart");
        if (this.restartPromise === restart) this.restartPromise = undefined;
      },
    );
    return restart;
  }

  private async restartChild(): Promise<void> {
    if (!this.rpc.isClosed) await this.rpc.close().catch(() => undefined);
    const marker = assertStoredHash(
      await this.db.get("__root__"),
      "durableRoot",
    );
    const rpc = await startNativeChild({ options: this.options, root: marker });
    if (this.closing) {
      await rpc.close().catch(() => undefined);
      throw new Error("Native MPF owner service closed during child restart");
    }
    this.rpc = rpc;
    this.durableRoot = marker;
    this.workerGenerationLeases.clear();
    this.recoveryFailure = undefined;
    this.childRestarts += 1;
    this.installFailureHandler(rpc);
  }

  private async ensureRpc(): Promise<NativeChildRpc> {
    if (this.closing) throw new Error("Native MPF owner service is closed");
    if (!this.rpc.isClosed) return this.rpc;
    await this.scheduleRestart();
    if (this.rpc.isClosed) {
      throw this.lastChildError ?? new Error("Native MPF owner restart failed");
    }
    return this.rpc;
  }

  private assertOwnedHandle(
    handle: NativeMpfGenerationHandle,
    rpc: NativeChildRpc,
  ): void {
    assertNativeMpfGenerationHandle(handle);
    if (!timingSafeEqual(Buffer.from(handle.ownerEpoch), rpc.epoch)) {
      throw new Error(
        "Native MPF generation handle belongs to a stale owner epoch",
      );
    }
  }

  private async handleWorkerMessage(
    port: MessagePort,
    message: unknown,
  ): Promise<void> {
    if (
      typeof message !== "object" ||
      message === null ||
      !("requestId" in message) ||
      !("method" in message)
    ) {
      port.close();
      return;
    }
    const request = message as Record<string, unknown>;
    if (!Number.isSafeInteger(request.requestId)) {
      port.close();
      return;
    }
    const requestId = Number(request.requestId);
    const lastRequestId = this.workerPortLastRequestId.get(port);
    if (lastRequestId === undefined || requestId <= lastRequestId) {
      port.close();
      return;
    }
    this.workerPortLastRequestId.set(port, requestId);
    try {
      let value: unknown;
      if (request.method === "fork") {
        const handle = await this.fork(String(request.baseRoot));
        if (!this.workerPorts.has(port)) {
          await this.discard(handle).catch(() => undefined);
          return;
        }
        this.workerGenerationLeases.set(this.generationKey(handle), {
          port,
          handle,
          journalOwned: false,
        });
        value = handle;
      } else if (request.method === "applyEvents") {
        this.assertWorkerLease(
          port,
          request.handle as NativeMpfGenerationHandle,
        );
        value = await this.applyEvents(
          request.handle as NativeMpfGenerationHandle,
          request.eventLog as Uint8Array,
        );
      } else if (request.method === "discard") {
        this.assertWorkerLease(
          port,
          request.handle as NativeMpfGenerationHandle,
        );
        await this.discard(request.handle as NativeMpfGenerationHandle);
        value = undefined;
      } else if (request.method === "retainForJournal") {
        const handle = request.handle as NativeMpfGenerationHandle;
        const lease = this.assertWorkerLease(port, handle);
        lease.journalOwned = true;
        value = undefined;
      } else if (request.method === "readNodes") {
        value = await readWorkerNodes(this.db, request.hashes);
      } else {
        throw new Error(
          `Unknown native MPF worker method ${String(request.method)}`,
        );
      }
      port.postMessage({ requestId: request.requestId, ok: true, value });
    } catch (error) {
      port.postMessage({
        requestId: request.requestId,
        ok: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  private generationKey(handle: NativeMpfGenerationHandle): string {
    assertNativeMpfGenerationHandle(handle);
    return `${Buffer.from(handle.ownerEpoch).toString("hex")}:${Buffer.from(handle.generationId).toString("hex")}`;
  }

  private assertWorkerLease(
    port: MessagePort,
    handle: NativeMpfGenerationHandle,
  ): WorkerGenerationLease {
    const lease = this.workerGenerationLeases.get(this.generationKey(handle));
    if (lease === undefined || lease.port !== port) {
      throw new Error(
        "Native MPF generation is not leased to this worker port",
      );
    }
    return lease;
  }

  private async releaseWorkerLeases(port: MessagePort): Promise<void> {
    const abandoned: NativeMpfGenerationHandle[] = [];
    for (const [key, lease] of this.workerGenerationLeases) {
      if (lease.port !== port || lease.journalOwned) continue;
      this.workerGenerationLeases.delete(key);
      abandoned.push(lease.handle);
    }
    await Promise.all(
      abandoned.map((handle) => this.discard(handle).catch(() => undefined)),
    );
  }
}
