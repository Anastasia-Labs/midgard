import { constants } from "node:fs";
import { access, realpath } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import { watcherNativeChildDrain } from "./native-chain-sync.child-drain.js";
import {
  deriveWatcherNativeGenesisIdentity,
  lines,
  type NativeStreamInput,
  parseJsonLine,
  parseWatcherNativeChainSyncEvent,
  productionSpawn,
  sha256,
} from "./native-chain-sync.derive-watcher-native-genesis-identity.js";
import { exactPointServiceSession } from "./native-chain-sync.exact-point-service.js";
import {
  authorityDetails,
  authorityLiveness,
  eventReceiptBrand,
  eventReceipts,
  exactRecord,
  MAX_QUERY_STDOUT_BYTES,
  MAX_STDERR_BYTES,
  MAX_STDERR_DIAGNOSTIC_BYTES,
  NativeChainSyncStartupFailure,
  type NativeOperation,
  parsePoint,
  parseTip,
  receiptsByEvent,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncRuntime,
} from "./native-chain-sync.exact-record.js";

export const startNativeSupervisor = async (
  input: NativeStreamInput & {
    readonly operation: NativeOperation;
    readonly signal?: AbortSignal;
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
  const binaryPath = input.binaryPath;
  if (!isAbsolute(binaryPath) || normalize(binaryPath) !== binaryPath) {
    throw new Error("native chain-sync binary path is not canonical");
  }
  if (input.unsafeSpawnForTest === undefined) {
    if ((await realpath(binaryPath)) !== binaryPath) {
      throw new Error("native chain-sync binary path traverses a symlink");
    }
    input.signal?.throwIfAborted();
    await access(binaryPath, constants.X_OK);
    input.signal?.throwIfAborted();
  }
  const intersection = parsePoint(
    input.intersection,
    "native startup intersection",
  );
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
    intersection,
    network: input.watcherConfig.targetNetwork,
    networkMagic,
    operation: input.operation,
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    socketPath: source.chainSync.socketPath,
  });
  const startupJson = watcherCanonicalJson(startup);
  const startupDigest = sha256(startupJson);
  if (Buffer.byteLength(startupJson, "utf8") + 1 > 64 * 1024) {
    throw new Error("native chain-sync startup exceeds its byte bound");
  }
  input.signal?.throwIfAborted();
  // Exact-point queries are sessions of the persistent helper; a stream owns
  // its helper process for the stream's whole lifetime.
  const child = (
    input.unsafeSpawnForTest ??
    (input.operation.kind === "exact_point"
      ? exactPointServiceSession
      : productionSpawn)
  )(binaryPath);
  const drainage = watcherNativeChildDrain(child);
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
        // integrity error through done/close rather than an event-listener throw.
        revocationFailure = new Error(
          "native read lifetime revocation failed",
          { cause },
        );
      }
    }
  };
  // Lifecycle events remain observable while the ordered callback is awaiting.
  child.once("exit", revokeEventProvenance);
  child.once("error", revokeEventProvenance);
  child.stdin.end(`${startupJson}\n`, "utf8");

  let closing = false;
  const abortHandler = () => {
    revokeEventProvenance();
    rejectReady(input.signal?.reason);
    void close();
  };
  let resolveReady!: (authority: WatcherNativeChainSyncAuthority) => void;
  let rejectReady!: (error: unknown) => void;
  const ready = new Promise<WatcherNativeChainSyncAuthority>(
    (resolve, reject) => {
      resolveReady = resolve;
      rejectReady = reject;
    },
  );
  const knownPoints = new Map<
    string,
    Readonly<{ slot: bigint; blockNo: bigint }>
  >();
  if (intersection.kind === "point") {
    knownPoints.set(intersection.blockHash, {
      slot: BigInt(intersection.slot),
      blockNo: -1n,
    });
  }
  knownPoints.set("", { slot: 0n, blockNo: -1n });
  let current: Readonly<{
    hash: string;
    slot: bigint;
    blockNo: bigint;
  }> | null =
    intersection.kind === "point"
      ? Object.freeze({
          hash: intersection.blockHash,
          slot: BigInt(intersection.slot),
          blockNo: -1n,
        })
      : null;
  let sawReady = false;
  let queryEventCount = 0;
  let queryAcknowledged = false;
  let queryCaptured = false;
  let mintedAuthority: WatcherNativeChainSyncAuthority | undefined;

  let stderrTail = Buffer.alloc(0);
  let rejectedLine: string | undefined;
  const stderrDrain = (async () => {
    try {
      let total = 0;
      for await (const chunk of child.stderr) {
        total += chunk.byteLength;
        stderrTail = Buffer.concat([stderrTail, chunk]).subarray(
          -MAX_STDERR_DIAGNOSTIC_BYTES,
        );
        if (total > MAX_STDERR_BYTES) {
          child.kill("SIGKILL");
          throw new Error("native chain-sync stderr exceeded its bound");
        }
      }
    } catch (error) {
      revokeEventProvenance();
      throw error;
    }
  })();

  void stderrDrain.catch(() => undefined);
  const done = (async () => {
    try {
      for await (const line of lines(
        child.stdout,
        input.operation.kind === "exact_point"
          ? MAX_QUERY_STDOUT_BYTES
          : undefined,
      )) {
        rejectedLine = line;
        const value = parseJsonLine(line);
        if (
          typeof value === "object" &&
          value !== null &&
          (value as { kind?: unknown }).kind === "error"
        ) {
          const failure = exactRecord(
            value,
            ["code", "kind", "schemaVersion"],
            "native chain-sync failure",
          );
          if (
            failure.schemaVersion !==
              WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION ||
            typeof failure.code !== "string" ||
            !/^[a-z][a-z0-9_]{0,62}$/u.test(failure.code)
          )
            throw new Error("native chain-sync emitted an invalid failure");
          if (!sawReady) throw new NativeChainSyncStartupFailure(failure.code);
          throw new Error(`native chain-sync runtime failed: ${failure.code}`);
        }
        if (!sawReady) {
          const record = exactRecord(
            value,
            [
              "authorityNodeId",
              "currentTip",
              "genesisIdentitySha256",
              "kind",
              "network",
              "networkMagic",
              "operation",
              "schemaVersion",
              "selectedIntersection",
              "socketPath",
              "startupDigest",
            ],
            "native chain-sync ready event",
          );
          const selectedIntersection = parsePoint(
            record.selectedIntersection,
            "native selected intersection",
          );
          if (
            record.kind !== "ready" ||
            record.schemaVersion !== WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION ||
            record.authorityNodeId !== startup.authorityNodeId ||
            record.genesisIdentitySha256 !== startup.genesisIdentitySha256 ||
            record.network !== startup.network ||
            record.networkMagic !== startup.networkMagic ||
            watcherCanonicalJson(record.operation) !==
              watcherCanonicalJson(startup.operation) ||
            watcherCanonicalJson(selectedIntersection) !==
              watcherCanonicalJson(intersection) ||
            record.socketPath !== startup.socketPath ||
            record.startupDigest !== startupDigest
          ) {
            throw new Error(
              "native chain-sync ready identity differs from startup authority",
            );
          }
          const currentTip = parseTip(record.currentTip);
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
          const authority = Object.freeze({
            schemaVersion:
              "midgard-watcher-native-chain-sync-authority-v1" as const,
            authorityDigest: sha256(watcherCanonicalJson(details)),
          });
          authorityDetails.set(authority, details);
          authorityLiveness.set(authority, { active: true });
          mintedAuthority = authority;
          sawReady = true;
          resolveReady(authority);
          rejectedLine = undefined;
          continue;
        }
        const event = parseWatcherNativeChainSyncEvent(value);
        if (input.operation.kind === "exact_point") {
          queryEventCount += 1;
          if (queryCaptured || queryEventCount > 2) {
            throw new Error("native exact-point query emitted an extra event");
          }
          if (event.kind === "roll_backward") {
            if (
              queryAcknowledged ||
              watcherCanonicalJson(event.point) !==
                watcherCanonicalJson(intersection)
            ) {
              throw new Error("native exact-point query rolled back");
            }
            queryAcknowledged = true;
          } else {
            const target = input.operation.target;
            if (
              intersection.kind !== "point" ||
              event.blockHash !== target.blockHash ||
              event.slot !== target.slot ||
              event.blockNo !== target.blockNo ||
              event.prevHash !== intersection.blockHash
            ) {
              throw new Error(
                "native exact-point query returned a different target",
              );
            }
            queryCaptured = true;
          }
        }
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
        } else {
          const rollbackHash =
            event.point.kind === "origin" ? "" : event.point.blockHash;
          const rollbackSlot =
            event.point.kind === "origin" ? 0n : BigInt(event.point.slot);
          const rollback = knownPoints.get(rollbackHash);
          if (rollback === undefined) {
            // The authenticated node can roll back below FindIntersect. That
            // ancestor has not been delivered in this session; its block
            // number is learned from the next child. Durable recovery still
            // proves both fork paths before consumer authority can resume.
            if (
              intersection.kind !== "point" ||
              rollbackSlot >= BigInt(intersection.slot) ||
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
        }
        if (eventProvenance.active && mintedAuthority !== undefined) {
          const authority = mintedAuthority;
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
        rejectedLine = undefined;
        await input.onEvent(event);
      }
      await stderrDrain;
      if (!closing)
        throw new Error("native chain-sync process exited unexpectedly");
    } catch (error) {
      revokeEventProvenance();
      const failure = error instanceof Error ? error : new Error(String(error));
      rejectReady(failure);
      if (!closing) child.kill("SIGKILL");
      if (rejectedLine !== undefined) {
        // A terminal native error is stdout control data, not a chain event.
        // Drain its preceding stderr after process termination, with a bound
        // for a broken child; never include raw block payloads in diagnostics.
        let diagnosticTimer: ReturnType<typeof setTimeout> | undefined;
        await Promise.race([
          stderrDrain.catch(() => undefined),
          new Promise<void>((resolve) => {
            diagnosticTimer = setTimeout(resolve, 1000);
          }),
        ]);
        if (diagnosticTimer !== undefined) clearTimeout(diagnosticTimer);
        failure.message += `; nativePid=${child.pid ?? "unavailable"} nativeOperation=${watcherCanonicalJson(input.operation)} nativeStartupDigest=${startupDigest} nativeLineBytes=${Buffer.byteLength(rejectedLine)} nativeLineSha256=${sha256(rejectedLine)} stderrTail=${JSON.stringify(stderrTail.toString("utf8"))}`;
      }
      throw failure;
    } finally {
      revokeEventProvenance();
      if (abortHandler !== undefined) {
        input.signal?.removeEventListener("abort", abortHandler);
      }
      const liveness =
        mintedAuthority === undefined
          ? undefined
          : authorityLiveness.get(mintedAuthority);
      if (liveness !== undefined) liveness.active = false;
      await drainage.closed;
      await stderrDrain.catch(() => undefined);
    }
  })()
    .then(() => {
      if (revocationFailure !== undefined) throw revocationFailure;
    })
    .catch((error: unknown) => {
      throw revocationFailure ?? error;
    });
  void done.catch(() => undefined);

  let closePromise: Promise<void> | undefined;
  const close = (): Promise<void> => {
    revokeEventProvenance();
    closePromise ??= (async () => {
      closing = true;
      await drainage.terminate();
      await done.catch(() => undefined);
      if (revocationFailure !== undefined) throw revocationFailure;
    })();
    return closePromise;
  };

  input.signal?.addEventListener("abort", abortHandler, { once: true });
  if (input.signal?.aborted === true) abortHandler();

  let startupTimer: NodeJS.Timeout | undefined;
  const authority = await Promise.race([
    ready,
    new Promise<never>((_, reject) => {
      startupTimer = setTimeout(
        () => reject(new NativeChainSyncStartupFailure("startup_timed_out")),
        input.startupTimeoutMs,
      );
    }),
  ])
    .catch(async (error: unknown) => {
      revokeEventProvenance();
      closing = true;
      child.kill("SIGKILL");
      await close();
      throw revocationFailure ?? error;
    })
    .finally(() => {
      if (startupTimer !== undefined) clearTimeout(startupTimer);
    });

  return Object.freeze({ authority, done, close });
};
