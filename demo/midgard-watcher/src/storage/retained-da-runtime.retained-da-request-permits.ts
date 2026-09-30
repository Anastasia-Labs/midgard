import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";

import { assertWatcherL1AvailabilityPayloadSource } from "../availability/published-payload.js";
import { type WatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import {
  WatcherPublicDaLibp2pTransport,
  type WatcherPublicDaLibp2pTransportOptions,
} from "./public-da-libp2p-transport.js";

export const WATCHER_RETAINED_DA_RUNTIME =
  "midgard-watcher-production-retained-da-runtime-v1" as const;

export const operationsSinkByDeploymentIdentity = new WeakMap<
  VerifiedWatcherDeploymentIdentity,
  WatcherOperationsSink
>();

export const l1AvailabilitySourceByDeploymentIdentity = new WeakMap<
  VerifiedWatcherDeploymentIdentity,
  RetainedDaPayloadSource
>();

export const bindWatcherL1AvailabilityPayloadSource = (input: {
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  source: RetainedDaPayloadSource;
}): Readonly<{ close(): void }> => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherL1AvailabilityPayloadSource(
    input.source,
    input.deploymentIdentity,
  );
  if (l1AvailabilitySourceByDeploymentIdentity.has(input.deploymentIdentity))
    throw new Error("L1 availability source is already bound");
  l1AvailabilitySourceByDeploymentIdentity.set(
    input.deploymentIdentity,
    input.source,
  );
  return {
    close: () => {
      if (
        l1AvailabilitySourceByDeploymentIdentity.get(
          input.deploymentIdentity,
        ) === input.source
      ) {
        l1AvailabilitySourceByDeploymentIdentity.delete(
          input.deploymentIdentity,
        );
      }
    },
  };
};

export type WatcherRetainedDaOperationsBinding = Readonly<{
  close(): void;
}>;

/**
 * Installs the process-local, read-only diagnostics sink against the exact
 * module-admitted deployment identity. The retained-DA loader receives that
 * same opaque identity; neither application configuration nor callers can
 * select a different diagnostics authority.
 */
export const bindWatcherRetainedDaOperations = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly sink: WatcherOperationsSink;
}): WatcherRetainedDaOperationsBinding => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  if (operationsSinkByDeploymentIdentity.has(input.deploymentIdentity)) {
    throw new Error("production retained-DA operations sink is already bound");
  }
  operationsSinkByDeploymentIdentity.set(input.deploymentIdentity, input.sink);
  let closed = false;
  return Object.freeze({
    close: () => {
      if (closed) return;
      closed = true;
      if (
        operationsSinkByDeploymentIdentity.get(input.deploymentIdentity) ===
        input.sink
      ) {
        operationsSinkByDeploymentIdentity.delete(input.deploymentIdentity);
      }
    },
  });
};

export type WatcherRetainedDaRuntime = Readonly<{
  schemaVersion: typeof WATCHER_RETAINED_DA_RUNTIME;
  deploymentFingerprint: string;
  sources: readonly DaLibp2pRetainedDaSource[];
  close(): Promise<void>;
}>;

export type WatcherRetainedDaRuntimeOptions = Readonly<{
  /** Parsed again at this boundary so a caller cannot cast around config admission. */
  watcherConfig: unknown;
  /** Must already have passed the signed deployment-identity verifier. */
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  /** Unsafe test-only transport construction seam. Production omits it. */
  unsafeTransportOptionsForTest?: WatcherPublicDaLibp2pTransportOptions;
  unsafeTransportFactoryForTest?: (
    options?: WatcherPublicDaLibp2pTransportOptions,
  ) => Promise<WatcherPublicDaLibp2pTransport>;
}>;

/**
 * Where the owner's one shared transport stands. `idle` means no workflow has
 * needed it yet, which is healthy: the transport starts on the first
 * classification or launch, never by readiness. Starting builds the local
 * libp2p node and contacts no peer; peers are dialed per request under a
 * lease. A start failure is therefore local and deterministic, so `failed` is
 * sticky for the owner's lifetime and carries the start failure. The
 * classification that triggered it fails closed and ends the process, and a
 * restart owns a fresh transport; no in-process retry is intended.
 */
export type WatcherRetainedDaTransportStatus = Readonly<{
  state: "idle" | "opening" | "open" | "failed" | "closed";
  failure: string | null;
}>;

export type WatcherRetainedDaRuntimeOwner = Readonly<{
  createRuntime(watcherConfig: unknown): Promise<WatcherRetainedDaRuntime>;
  transportStatus(): WatcherRetainedDaTransportStatus;
  close(): Promise<void>;
}>;

export const runtimeOwnerIdentities = new WeakMap<
  WatcherRetainedDaRuntimeOwner,
  VerifiedWatcherDeploymentIdentity
>();

/** One bounded request queue for all leases sharing a public peer identity. */
export class RetainedDaRequestPermits {
  private active = 0;
  private readonly waiting = new Set<{
    grant(): void;
    signal: AbortSignal;
    onAbort(): void;
  }>();

  constructor(private readonly maximum: number) {}

  async acquire(signal: AbortSignal): Promise<() => void> {
    signal.throwIfAborted();
    if (this.active < this.maximum) {
      this.active += 1;
      return this.releaseOnce();
    }
    if (this.waiting.size >= 64) {
      throw new Error("retained-DA request queue is full");
    }
    return await new Promise<() => void>((resolve, reject) => {
      const waiter = {
        signal,
        grant: () => {
          this.active += 1;
          resolve(this.releaseOnce());
        },
        onAbort: () => {
          this.waiting.delete(waiter);
          reject(
            signal.reason instanceof Error
              ? signal.reason
              : new Error("retained-DA request aborted", {
                  cause: signal.reason,
                }),
          );
        },
      };
      this.waiting.add(waiter);
      signal.addEventListener("abort", waiter.onAbort, { once: true });
    });
  }

  private releaseOnce(): () => void {
    let released = false;
    return () => {
      if (released) return;
      released = true;
      this.active -= 1;
      for (const waiter of this.waiting) {
        this.waiting.delete(waiter);
        waiter.signal.removeEventListener("abort", waiter.onAbort);
        if (!waiter.signal.aborted) {
          waiter.grant();
          break;
        }
      }
    };
  }
}

export type RetainedDaTransportScope = Readonly<{
  open(): Promise<WatcherPublicDaLibp2pTransport>;
  signal: AbortSignal;
  acquireRequest(signal: AbortSignal): Promise<() => void>;
}>;

export type AdmittedRuntimeOptions = Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  unsafeTransportOptionsForTest?: WatcherPublicDaLibp2pTransportOptions;
  unsafeTransportFactoryForTest?: (
    options?: WatcherPublicDaLibp2pTransportOptions,
  ) => Promise<WatcherPublicDaLibp2pTransport>;
}>;

/**
 * Snapshots the caller-owned construction object before any asynchronous
 * boundary. The verified identity itself is immutable and module-admitted;
 * the test-only transport options are copied so later property replacement
 * cannot cross-bind a workflow invocation to another runtime authority.
 */
export const admitRuntimeOptions = (
  options: Omit<WatcherRetainedDaRuntimeOptions, "watcherConfig">,
): AdmittedRuntimeOptions => {
  const {
    deploymentIdentity,
    unsafeTransportFactoryForTest,
    unsafeTransportOptionsForTest,
  } = options;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const transportOptions =
    unsafeTransportOptionsForTest === undefined
      ? undefined
      : Object.freeze({
          ...(unsafeTransportOptionsForTest.libp2pFactory === undefined
            ? {}
            : {
                libp2pFactory: unsafeTransportOptionsForTest.libp2pFactory,
              }),
          ...(unsafeTransportOptionsForTest.maxFrameBytes === undefined
            ? {}
            : { maxFrameBytes: unsafeTransportOptionsForTest.maxFrameBytes }),
        });
  return Object.freeze({
    deploymentIdentity,
    ...(transportOptions === undefined
      ? {}
      : { unsafeTransportOptionsForTest: transportOptions }),
    ...(unsafeTransportFactoryForTest === undefined
      ? {}
      : { unsafeTransportFactoryForTest }),
  });
};

export type AdmittedPeer = WatcherConfig["da"]["peers"][number];
