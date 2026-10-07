import { constants } from "node:fs";
import { access, realpath } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import {
  type ChainSyncEvent,
  type ChainSyncStream,
  sharedL1NodeTransport,
} from "@al-ft/l1-node-transport";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  deriveWatcherNativeGenesisIdentity,
  type NativeStreamInput,
  sha256,
} from "./native-chain-sync.derive-watcher-native-genesis-identity.js";
import {
  authorityDetails,
  authorityLiveness,
  eventReceiptBrand,
  eventReceipts,
  MAX_INTERSECTIONS,
  NativeChainSyncStartupFailure,
  type NativeOperation,
  parsePoint,
  receiptsByEvent,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
  type WatcherNativeChainSyncRuntime,
} from "./native-chain-sync.exact-record.js";
import {
  fromChainPoint,
  fromChainTip,
  startupFailure,
  toChainPoint,
  watcherEvent,
} from "./native-chain-sync.transport-event.js";

/**
 * Credit of a following stream: a deep window while it catches up, and one
 * outstanding block at the tip, so a closed stream frees its node connection
 * after at most one more block.
 */
const STREAM_CREDIT = Object.freeze({
  catchUpWindow: 50,
  tipWindow: 1,
  catchUpDistance: 10n,
});

/**
 * Intersection candidates in preference order: at most MAX_INTERSECTIONS,
 * no duplicates, and the Origin only as the final fallback.
 */
export const parseIntersectionCandidates = (
  candidates: readonly WatcherNativeChainSyncPoint[],
): readonly WatcherNativeChainSyncPoint[] => {
  if (candidates.length === 0 || candidates.length > MAX_INTERSECTIONS) {
    throw new Error(
      "native chain-sync intersection candidate bounds are invalid",
    );
  }
  const seen = new Set<string>();
  return Object.freeze(
    candidates.map((candidate, index) => {
      const parsed = parsePoint(candidate, "native intersection candidate");
      const key = watcherCanonicalJson(parsed);
      if (seen.has(key))
        throw new Error("native intersection candidate is duplicated");
      if (parsed.kind === "origin" && index !== candidates.length - 1) {
        throw new Error("native Origin candidate must be the final fallback");
      }
      seen.add(key);
      return parsed;
    }),
  );
};

/**
 * One native chain-sync read over the process's shared node transport: a
 * following stream, or one exact-point query. The node and the intersection
 * are admitted before the returned authority exists. Every delivered event is
 * checked for order and carries a receipt that a rollback, a stream failure
 * or close revokes.
 */
export const startNativeSupervisor = async (
  input: Omit<NativeStreamInput, "intersection"> & {
    readonly intersections: readonly WatcherNativeChainSyncPoint[];
    readonly operation: NativeOperation;
  },
): Promise<WatcherNativeChainSyncRuntime> => {
  input.signal?.throwIfAborted();
  if (input.watcherConfig.l1.source.sourceMode !== "local_node") {
    throw new Error(
      "native chain-sync requires the admitted local-node source",
    );
  }
  if (
    !Number.isSafeInteger(input.startupTimeoutMs) ||
    input.startupTimeoutMs < 100 ||
    input.startupTimeoutMs > 120_000
  ) {
    throw new Error("native chain-sync startup bounds are invalid");
  }
  const intersections = parseIntersectionCandidates(input.intersections);
  const binaryPath = input.binaryPath;
  if (!isAbsolute(binaryPath) || normalize(binaryPath) !== binaryPath) {
    throw new Error("native chain-sync binary path is not canonical");
  }
  if ((await realpath(binaryPath)) !== binaryPath) {
    throw new Error("native chain-sync binary path traverses a symlink");
  }
  input.signal?.throwIfAborted();
  await access(binaryPath, constants.X_OK);
  input.signal?.throwIfAborted();
  const source = input.watcherConfig.l1.source;
  const { genesisIdentitySha256, networkMagic } =
    await deriveWatcherNativeGenesisIdentity({
      watcherConfig: input.watcherConfig,
      ...(input.unsafeReadIdentityFileForTest === undefined
        ? {}
        : {
            unsafeReadIdentityFileForTest: input.unsafeReadIdentityFileForTest,
          }),
    });
  input.signal?.throwIfAborted();
  const startup = Object.freeze({
    authorityNodeId: source.authorityNodeId,
    genesisIdentitySha256,
    intersections,
    network: input.watcherConfig.targetNetwork,
    networkMagic,
    operation: input.operation,
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    socketPath: source.chainSync.socketPath,
  });
  const startupDigest = sha256(watcherCanonicalJson(startup));
  const exact = input.operation.kind === "exact_point";

  const eventProvenance = { active: true, generation: 0n };
  let revocationFailure: Error | undefined;
  const revokeEventProvenance = (): void => {
    const wasActive = eventProvenance.active;
    eventProvenance.active = false;
    eventProvenance.generation += 1n;
    if (wasActive) {
      try {
        input.onAuthorityRevoked?.();
      } catch (cause) {
        // Admission stays revoked and cleanup still runs; expose the lifecycle
        // integrity error through done/close rather than a listener throw.
        revocationFailure = new Error(
          "native read lifetime revocation failed",
          { cause },
        );
      }
    }
  };

  let stream: ChainSyncStream | undefined;
  let closing = false;
  const running: { done?: Promise<void> } = {};
  let closePromise: Promise<void> | undefined;
  const close = (): Promise<void> => {
    revokeEventProvenance();
    closePromise ??= (async () => {
      closing = true;
      await stream?.close();
      await running.done?.catch(() => undefined);
      if (revocationFailure !== undefined) throw revocationFailure;
    })();
    return closePromise;
  };

  const startupBoundMs = input.startupTimeoutMs;
  const opened = (async () => {
    const transport = sharedL1NodeTransport({
      binaryPath,
      socketPath: source.chainSync.socketPath,
      networkMagic,
    });
    await transport.whenReady(startupBoundMs);
    if (closing) throw new Error("native chain-sync read was closed");
    const owned = transport.openChainSync({
      points: intersections.map(toChainPoint),
      credit: exact ? 1 : STREAM_CREDIT,
      resume: false,
    });
    stream = owned;
    // A failed stream revokes admission at once, even while an event
    // callback is still awaited.
    void owned.ended.then((cause) => {
      if (cause !== null) revokeEventProvenance();
    });
    return await owned.opened;
  })();
  void opened.catch(() => undefined);
  // An abort rejects with the caller's own reason, whatever it is.
  let rejectStartup!: (reason: unknown) => void;
  const startupBound = new Promise<never>((_, reject) => {
    rejectStartup = reject;
  });
  const startupTimer = setTimeout(
    () => rejectStartup(new NativeChainSyncStartupFailure("startup_timed_out")),
    startupBoundMs,
  );
  const abortStartup = (): void => rejectStartup(input.signal?.reason);
  input.signal?.addEventListener("abort", abortStartup, { once: true });
  if (input.signal?.aborted === true) abortStartup();
  let selection: Awaited<typeof opened>;
  try {
    selection = await Promise.race([opened, startupBound]);
  } catch (error) {
    revokeEventProvenance();
    await close().catch(() => undefined);
    throw revocationFailure ?? startupFailure(error);
  } finally {
    clearTimeout(startupTimer);
    input.signal?.removeEventListener("abort", abortStartup);
  }
  const owned = stream!;
  const selectedIntersection = fromChainPoint(selection.intersection);
  const currentTip = fromChainTip(selection.tip);
  const details = Object.freeze({
    network: startup.network,
    authorityNodeId: startup.authorityNodeId,
    genesisIdentitySha256: startup.genesisIdentitySha256,
    socketPath: startup.socketPath,
    startupDigest,
    operation: startup.operation,
    selectedIntersection,
    currentTip,
  });
  const authority: WatcherNativeChainSyncAuthority = Object.freeze({
    schemaVersion: "midgard-watcher-native-chain-sync-authority-v1" as const,
    authorityDigest: sha256(watcherCanonicalJson(details)),
  });
  authorityDetails.set(authority, details);
  authorityLiveness.set(authority, { active: true });

  const knownPoints = new Map<
    string,
    Readonly<{ slot: bigint; blockNo: bigint }>
  >();
  if (selectedIntersection.kind === "point") {
    knownPoints.set(selectedIntersection.blockHash, {
      slot: BigInt(selectedIntersection.slot),
      blockNo: -1n,
    });
  }
  knownPoints.set("", { slot: 0n, blockNo: -1n });
  let current: Readonly<{
    hash: string;
    slot: bigint;
    blockNo: bigint;
  }> | null =
    selectedIntersection.kind === "point"
      ? Object.freeze({
          hash: selectedIntersection.blockHash,
          slot: BigInt(selectedIntersection.slot),
          blockNo: -1n,
        })
      : null;
  let queryAcknowledged = false;
  let queryCaptured = false;

  const checkExact = (event: WatcherNativeChainSyncEvent): void => {
    if (input.operation.kind !== "exact_point") return;
    if (queryCaptured) {
      throw new Error("native exact-point query emitted an extra event");
    }
    if (event.kind === "roll_backward") {
      if (
        queryAcknowledged ||
        watcherCanonicalJson(event.point) !==
          watcherCanonicalJson(selectedIntersection)
      ) {
        throw new Error("native exact-point query rolled back");
      }
      queryAcknowledged = true;
      return;
    }
    const target = input.operation.target;
    if (
      selectedIntersection.kind !== "point" ||
      event.blockHash !== target.blockHash ||
      event.slot !== target.slot ||
      event.blockNo !== target.blockNo ||
      event.prevHash !== selectedIntersection.blockHash
    ) {
      throw new Error("native exact-point query returned a different target");
    }
    queryCaptured = true;
  };

  const checkOrder = (event: WatcherNativeChainSyncEvent): void => {
    if (event.kind === "roll_forward") {
      const slot = BigInt(event.slot);
      const blockNo = BigInt(event.blockNo);
      if (
        (current !== null &&
          (event.prevHash !== current.hash ||
            slot <= current.slot ||
            blockNo <= current.blockNo)) ||
        (current === null && !knownPoints.has(event.prevHash))
      ) {
        throw new Error("native chain-sync roll-forward is out of order");
      }
      current = Object.freeze({ hash: event.blockHash, slot, blockNo });
      knownPoints.set(event.blockHash, { slot, blockNo });
      return;
    }
    const rollbackHash =
      event.point.kind === "origin" ? "" : event.point.blockHash;
    const rollbackSlot =
      event.point.kind === "origin" ? 0n : BigInt(event.point.slot);
    const rollback = knownPoints.get(rollbackHash);
    if (rollback === undefined) {
      // The node can roll back below the intersection. That ancestor has not
      // been delivered in this read; its block number is learned from the
      // next child. Durable recovery still proves both fork paths before
      // consumer authority can resume.
      if (
        selectedIntersection.kind !== "point" ||
        rollbackSlot >= BigInt(selectedIntersection.slot) ||
        (current !== null && rollbackSlot >= current.slot)
      )
        throw new Error(
          "native chain-sync rollback target is not durable history",
        );
      knownPoints.set(rollbackHash, { slot: rollbackSlot, blockNo: -1n });
    } else if (rollback.slot !== rollbackSlot) {
      throw new Error(
        "native chain-sync rollback target is not durable history",
      );
    }
    current = Object.freeze({
      hash: rollbackHash,
      slot: rollbackSlot,
      blockNo: rollback?.blockNo ?? -1n,
    });
    for (const [hash, point] of knownPoints) {
      if (point.slot > rollbackSlot) knownPoints.delete(hash);
    }
    eventProvenance.generation += 1n;
  };

  const deliver = async (event: WatcherNativeChainSyncEvent): Promise<void> => {
    checkExact(event);
    checkOrder(event);
    if (eventProvenance.active) {
      const generation = eventProvenance.generation;
      const receipt = Object.freeze({ [eventReceiptBrand]: true as const });
      eventReceipts.set(
        receipt,
        Object.freeze({
          value: Object.freeze({
            authority,
            startupDigest,
            event,
            eventDigest: sha256(watcherCanonicalJson(event)),
          }),
          isLive: () =>
            eventProvenance.active &&
            eventProvenance.generation === generation &&
            watcherNativeChainSyncAuthorityDetails(authority) !== null,
        }),
      );
      receiptsByEvent.set(event, receipt);
    }
    await input.onEvent(event);
  };

  const abortHandler = (): void => {
    revokeEventProvenance();
    void close().catch(() => undefined);
  };
  const done = (async () => {
    try {
      // The node's first reply acknowledges the intersection. The transport
      // consumes it, so the read delivers it as its first event.
      await deliver(
        Object.freeze({
          schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
          kind: "roll_backward",
          point: selectedIntersection,
          tip: currentTip,
        }),
      );
      for (;;) {
        let next: ChainSyncEvent | undefined;
        try {
          next = await owned.next();
        } catch (cause) {
          throw new Error(
            `native chain-sync runtime failed: ${(cause as Error).message}`,
            { cause },
          );
        }
        if (next === undefined) break;
        await deliver(watcherEvent(next));
        // An exact-point query holds its single credit: no block follows.
        if (!exact && !closing) owned.ack(next.seq);
      }
      if (!closing)
        throw new Error("native chain-sync stream ended unexpectedly");
    } finally {
      revokeEventProvenance();
      input.signal?.removeEventListener("abort", abortHandler);
      const liveness = authorityLiveness.get(authority);
      if (liveness !== undefined) liveness.active = false;
      if (!closing) await owned.close();
    }
  })()
    .then(() => {
      if (revocationFailure !== undefined) throw revocationFailure;
    })
    .catch((error: unknown) => {
      throw revocationFailure ?? error;
    });
  running.done = done;
  void done.catch(() => undefined);

  input.signal?.addEventListener("abort", abortHandler, { once: true });
  if (input.signal?.aborted === true) abortHandler();

  return Object.freeze({ authority, done, close });
};
