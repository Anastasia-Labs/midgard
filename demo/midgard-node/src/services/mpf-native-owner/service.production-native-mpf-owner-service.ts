import { timingSafeEqual } from "node:crypto";
import { getHeapStatistics } from "node:v8";
import { MessageChannel, type MessagePort } from "node:worker_threads";

import { Level } from "level";

import {
  createEventFlatDigest,
  prepareEventFlatDigest,
} from "../../workers/utils/mpf-event-flat-digest.js";
import {
  assertNativeMpfGenerationHandle,
  assertNativeMpfHashHex,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_RPC_SCHEMA,
  type NativeMpfApplyResult,
  type NativeMpfCanonicalRootRecovery,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerDiagnostics,
  type NativeMpfOwnerService,
  NativeMpfRpcKind,
  type PersistedNativeMpfReplay,
} from "./protocol.js";
import { assertStoredNode } from "./service.encode-stored-node.js";
import {
  assertPinnedOwnerBinary,
  NativeChildRpc,
} from "./service.native-child-rpc.js";
import {
  assertNativeOwnerRuntimeMemoryBudget,
  assertStoredHash,
  type DecodedPromotionRecord,
  digest,
  EMPTY_ROOT_HEX,
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
import {
  buildOrReadFullIndex,
  keyNibbles,
  parsePromotionRecords,
} from "./service.parse-promotion-records.js";
import {
  type NativeOwnerRestartHealth,
  NativeOwnerRestartPolicy,
} from "./service.restart-policy.js";
import { startNativeChild } from "./service.start-native-child.js";
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
  private activeReads = 0;
  private readDrainWaiters: (() => void)[] = [];
  private restoration: Promise<void> | undefined;
  private recoveryFailure: Error | undefined;
  private lastChildError: Error | undefined;

  private constructor(
    private readonly db: Level<string, StoredValue>,
    private rpc: NativeChildRpc,
    private readonly binarySha256: string,
    private durableRoot: string,
    private readonly options: NormalizedNativeMpfOwnerServiceOptions,
  ) {
    this.restartPolicy = new NativeOwnerRestartPolicy(options);
  }

  public static async create(
    options: NativeMpfOwnerServiceOptions,
  ): Promise<ProductionNativeMpfOwnerService> {
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
    const db = new Level<string, StoredValue>(normalized.levelPath, {
      valueEncoding: "json",
    });
    await db.open();
    let rpc: NativeChildRpc | undefined;
    try {
      const marker = assertStoredHash(await db.get("__root__"), "durableRoot");
      const fullIndex = await buildOrReadFullIndex({
        db,
        marker,
        options: normalized,
        binarySha256: normalized.binarySha256,
      });
      rpc = await startNativeChild({ options: normalized, fullIndex, marker });
      const service = new ProductionNativeMpfOwnerService(
        db,
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
      const response = await rpc.request(
        NativeMpfRpcKind.ApplyEvents,
        Buffer.concat([Buffer.from(handle.generationId), log]),
        new Set([NativeMpfRpcKind.Applied]),
        false,
        NATIVE_MPF_OWNER_DEFAULT_CAPS.applyTimeoutMs,
      );
      const payload = Buffer.from(response.payload);
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
        if (
          Buffer.compare(records[index - 1]!.hash, records[index]!.hash) >= 0
        ) {
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
      for (const record of records) aggregate.update(record.encoded);
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
      await this.validatePromotionClosure(
        candidateRoot,
        handle.baseRoot,
        records,
      );
      await this.options.faultInjectionForTests?.("before_promotion_batch");
      await this.db.batch([
        ...records.map((record) => ({
          type: "put" as const,
          key: record.hashHex,
          value: record.stored,
        })),
        { type: "put" as const, key: "__root__", value: candidateRoot },
      ]);
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
  }

  public async recover(replay: PersistedNativeMpfReplay): Promise<void> {
    return this.runOperation(async () => {
      if (replay.schema !== NATIVE_MPF_RPC_SCHEMA) {
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
    return this.runReadOperation(async () => {
      const rpc = await this.ensureRpc();
      await this.options.faultInjectionForTests?.("diagnostics_before_request");
      const response = await rpc.request(
        NativeMpfRpcKind.Diagnostics,
        Buffer.alloc(0),
        new Set([NativeMpfRpcKind.DiagnosticsResult]),
      );
      const payload = Buffer.from(response.payload);
      if (payload.length !== 96) {
        throw new Error("Native MPF diagnostics payload length is invalid");
      }
      const value = (offset: number): number => {
        const result = payload.readBigUInt64LE(offset);
        if (result > BigInt(Number.MAX_SAFE_INTEGER)) {
          throw new Error("Native MPF diagnostic exceeds safe integer");
        }
        return Number(result);
      };
      const diagnostics = {
        ownerEpoch: rpc.epoch,
        durableRoot: payload.subarray(0, 32).toString("hex"),
        residentNodes: value(32),
        residentEdges: value(40),
        residentBytes: value(48),
        activeGenerations: value(56),
        generatedNodes: value(64),
        generatedBytes: value(72),
        rssBytes: value(80) * 1024,
        peakRssBytes: value(88) * 1024,
        childRestarts: this.childRestarts,
      };
      if (
        diagnostics.residentNodes >
          NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentNodes ||
        diagnostics.residentBytes >
          NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
        diagnostics.rssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
        diagnostics.peakRssBytes >
          NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes
      ) {
        throw new Error(
          `Native MPF diagnostics cap exceeded: nodes=${diagnostics.residentNodes.toString()},resident_bytes=${diagnostics.residentBytes.toString()},rss_bytes=${diagnostics.rssBytes.toString()},peak_rss_bytes=${diagnostics.peakRssBytes.toString()}`,
        );
      }
      return diagnostics;
    });
  }

  /** Restore only a retained, hash-verified closure. The caller must persist and
   * authenticate the recovery plan and drain producers before this operation.
   * SQL reconciliation and Ready publication remain the caller's responsibility.
   * A new child epoch invalidates all handles from the displaced native state.
   * In-flight read-only diagnostics (readiness, audits) are awaited, not refused;
   * new operations are refused from this call until the restore settles.
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
    const restore = this.readsDrained().then(() =>
      this.restoreRetainedRoot(captured),
    );
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
    const diagnostics = await rpc.request(
      NativeMpfRpcKind.Diagnostics,
      Buffer.alloc(0),
      new Set([NativeMpfRpcKind.DiagnosticsResult]),
    );
    const payload = Buffer.from(diagnostics.payload);
    if (payload.length !== 96 || payload.readBigUInt64LE(56) !== 0n)
      throw new Error(
        "Native MPF canonical recovery requires drained generations",
      );
    // Never use a sidecar as authority for a different canonical root. Read the
    // retained content-addressed closure, then let the pinned native loader
    // verify every hash, path and child before changing the durable marker.
    // A target whose closure is not fully retained is refused here, before any
    // marker change: restoring onto a partial trie would commit on a wrong base.
    const fullIndex = await buildOrReadFullIndex({
      db: this.db,
      marker: plan.targetRoot,
      options: { ...this.options, sidecarPath: undefined },
      binarySha256: this.binarySha256,
    }).catch((cause: unknown) => {
      throw new Error(
        `Native MPF canonical recovery target root ${plan.targetRoot} is not retained in full; refusing to restore`,
        { cause },
      );
    });
    let replacement: NativeChildRpc | undefined;
    let committed = false;
    try {
      // Avoid keeping two resident native tries alive at production scale.
      await rpc.close();
      replacement = await startNativeChild({
        options: this.options,
        fullIndex,
        marker: plan.targetRoot,
      });
      if (this.closing)
        throw new Error("Native MPF owner closed during canonical recovery");
      const current = assertStoredHash(
        await this.db.get("__root__"),
        "durableRoot",
      );
      if (current !== plan.expectedRoot)
        throw new Error(
          "Native MPF canonical recovery marker changed before commit",
        );
      await this.options.faultInjectionForTests?.("before_root_restore_batch");
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
      this.rpc = replacement;
      this.durableRoot = plan.targetRoot;
      this.workerGenerationLeases.clear();
      this.childRestarts += 1;
      this.installFailureHandler(replacement);
      replacement = undefined;
    } catch (error) {
      if (committed)
        this.recoveryFailure =
          error instanceof Error ? error : new Error(String(error));
      throw error;
    } finally {
      await replacement?.close().catch(() => undefined);
    }
  }

  /** Why every operation refuses right now, or undefined: failed child
   * restarts have exhausted the window (see `NativeOwnerRestartPolicy`), or a
   * committed canonical recovery has not been installed yet. Neither is
   * terminal: each call also starts a restart that is due, which loads the
   * child from the durable root marker exactly as a process start does, and a
   * restart that succeeds clears both. */
  public terminalFailure(): Error | undefined {
    this.resumeRestart();
    return this.refusal();
  }

  public restartHealth(): NativeOwnerRestartHealth {
    return this.restartPolicy.health();
  }

  private refusal(): Error | undefined {
    const exhaustion = this.restartPolicy.exhaustion();
    if (exhaustion !== undefined) return exhaustion;
    if (this.recoveryFailure !== undefined)
      return new Error(
        "Native MPF canonical recovery is not installed yet; the owner restarts from its durable root",
        { cause: this.recoveryFailure },
      );
    return undefined;
  }

  /** Starts a child restart from the durable root when one is due: the child
   * is gone or a committed recovery was not installed, and neither a restart,
   * a restoration nor an exhausted window is in the way. */
  private resumeRestart(): void {
    if (
      this.closing ||
      this.restartPromise !== undefined ||
      this.restoration !== undefined ||
      (this.recoveryFailure === undefined && !this.rpc.isClosed) ||
      this.restartPolicy.exhaustion() !== undefined
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

  private async runReadOperation<A>(work: () => Promise<A>): Promise<A> {
    this.assertCanOperate();
    this.activeReads += 1;
    try {
      return await work();
    } finally {
      this.activeReads -= 1;
      if (this.activeReads === 0)
        for (const resolve of this.readDrainWaiters.splice(0)) resolve();
    }
  }

  private readsDrained(): Promise<void> {
    if (this.activeReads === 0) return Promise.resolve();
    return new Promise((resolve) => this.readDrainWaiters.push(resolve));
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
      if (this.restoration === undefined && this.recoveryFailure === undefined)
        void this.scheduleRestart().catch(() => undefined);
    });
  }

  /** Restarts the child from the durable root marker after the policy's
   * backoff; refused while failed restarts exhaust the window. */
  private scheduleRestart(): Promise<void> {
    if (this.closing) {
      return Promise.reject(new Error("Native MPF owner service is closing"));
    }
    if (this.restartPromise !== undefined) return this.restartPromise;
    const exhaustion = this.restartPolicy.exhaustion();
    if (exhaustion !== undefined) return Promise.reject(exhaustion);
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
        this.restartPolicy.recordSuccess();
        if (this.restartPromise === restart) this.restartPromise = undefined;
      },
      (restartError: unknown) => {
        this.lastChildError =
          restartError instanceof Error
            ? restartError
            : new Error(String(restartError));
        this.restartPolicy.recordFailure(this.lastChildError);
        if (this.restartPromise === restart) this.restartPromise = undefined;
      },
    );
    return restart;
  }

  private async restartChild(): Promise<void> {
    // Never keep two resident native tries alive at production scale.
    if (!this.rpc.isClosed) await this.rpc.close().catch(() => undefined);
    const marker = assertStoredHash(
      await this.db.get("__root__"),
      "durableRoot",
    );
    const fullIndex = await buildOrReadFullIndex({
      db: this.db,
      marker,
      options: this.options,
      binarySha256: this.binarySha256,
    });
    const rpc = await startNativeChild({
      options: this.options,
      fullIndex,
      marker,
    });
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

  private async validatePromotionClosure(
    candidateRoot: string,
    baseRoot: string,
    records: readonly DecodedPromotionRecord[],
  ): Promise<void> {
    assertNativeMpfHashHex(candidateRoot, "candidateRoot");
    const byHash = new Map(records.map((record) => [record.hashHex, record]));
    if (candidateRoot === baseRoot || candidateRoot === EMPTY_ROOT_HEX) {
      if (records.length !== 0) {
        throw new Error(
          "Empty or no-op native MPF promotion returned generated records",
        );
      }
      return;
    }
    if (!byHash.has(candidateRoot)) {
      throw new Error("Native MPF promotion closure is missing candidate root");
    }
    const visited = new Set<string>();
    const visit = async (hash: string, path: string): Promise<void> => {
      const record = byHash.get(hash);
      if (record === undefined) {
        try {
          assertStoredNode(await this.db.get(hash), hash);
          return;
        } catch (error) {
          throw new Error(
            `Native MPF promotion references missing durable child ${hash}: ${String(error)}`,
          );
        }
      }
      if (visited.has(hash)) return;
      visited.add(hash);
      const nodePath = path + record.stored.prefix;
      if (record.stored.__kind === "Leaf") {
        if (keyNibbles(record.stored.key) !== nodePath) {
          throw new Error(
            `Native MPF promoted leaf ${hash} is linked at the wrong path`,
          );
        }
        return;
      }
      await Promise.all(
        record.stored.children.map((child, branch) =>
          child === null
            ? undefined
            : visit(child, nodePath + branch.toString(16)),
        ),
      );
    };
    await visit(candidateRoot, "");
    if (visited.size !== records.length) {
      throw new Error(
        `Native MPF promotion contains unreachable generated records: reachable=${visited.size.toString()},records=${records.length.toString()}`,
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
