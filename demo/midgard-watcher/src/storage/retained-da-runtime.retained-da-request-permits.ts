import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";

import { assertWatcherL1AvailabilityPayloadSource } from "../availability/published-payload.js";
import {
  type PerHeaderAlert,
  perHeaderAlert,
} from "../fault-proofs/per-header-alert.js";
import { type WatcherConfig } from "../runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.parse-trust-roots.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.verify-watcher-user-event-script-binding.js";
import { watcherDaFetchAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import {
  WatcherPublicDaLibp2pTransport,
  type WatcherPublicDaLibp2pTransportOptions,
} from "./public-da-libp2p-transport.js";

export const WATCHER_RETAINED_DA_RUNTIME =
  "midgard-watcher-production-retained-da-runtime-v1" as const;

/**
 * The diagnostics sink of a deployment, its `da_fetch_failure` alert, and
 * the readers its DA fetchers registered (`registerWatcherDaFetcher`).
 */
export type WatcherRetainedDaOperations = Readonly<{
  sink: WatcherOperationsSink;
  daFetchAlert: PerHeaderAlert;
  fetchers: Set<() => Iterable<string>>;
}>;

export const operationsByDeploymentIdentity = new WeakMap<
  VerifiedWatcherDeploymentIdentity,
  WatcherRetainedDaOperations
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

/**
 * Registers what a fetcher of the deployment's retained DA by header may
 * still fetch: `mayStillFetch` reads the headers whose DA it may fetch
 * again, the queued ones aside. A failed fetch's `da_fetch_failure` stands
 * while some fetcher still names its header. Every such fetcher registers
 * one for its lifetime; the returned function unregisters it. Without a
 * bound diagnostics sink nothing raises the alert, and nothing is kept.
 */
export const registerWatcherDaFetcher = (
  deploymentIdentity: VerifiedWatcherDeploymentIdentity,
  mayStillFetch: () => Iterable<string>,
): (() => void) => {
  const fetchers =
    operationsByDeploymentIdentity.get(deploymentIdentity)?.fetchers;
  fetchers?.add(mayStillFetch);
  return () => fetchers?.delete(mayStillFetch);
};

export type WatcherRetainedDaOperationsBinding = Readonly<{
  close(): void;
  /**
   * Clears the `da_fetch_failure` alert of every header a fetch failed for
   * that no registered fetcher may still fetch and that is released, not in
   * the observed queue (a rollback included, `perHeaderAlert`) or settled.
   * The fault decision bridge calls it with the headers of every observation.
   */
  reconcile: PerHeaderAlert["reconcile"];
  /**
   * Settles `headerHash`: a record of the fault decision bridge accounts for
   * its standing failed fetch, so the alert clears, at once unless a
   * registered fetcher still names the header, else once none does. The only
   * clear of a header's `da_fetch_failure` besides `reconcile` and a served
   * fetch.
   */
  settle(headerHash: string): void;
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
  if (operationsByDeploymentIdentity.has(input.deploymentIdentity)) {
    throw new Error("production retained-DA operations sink is already bound");
  }
  const operations: WatcherRetainedDaOperations = Object.freeze({
    sink: input.sink,
    daFetchAlert: perHeaderAlert({
      code: "da_fetch_failure",
      subjectOf: watcherDaFetchAlertSubject,
      operationsSink: input.sink,
      observedAtMs: () => Date.now().toString(),
    }),
    fetchers: new Set<() => Iterable<string>>(),
  });
  operationsByDeploymentIdentity.set(input.deploymentIdentity, operations);
  const fetchable = (): Set<string> => {
    const named = new Set<string>();
    for (const mayStillFetch of operations.fetchers)
      for (const headerHash of mayStillFetch()) named.add(headerHash);
    return named;
  };
  // Raised headers settled while a fetcher still named them, cleared once
  // none does; a later failure of one after its clear stands again.
  const settled = new Set<string>();
  let closed = false;
  return Object.freeze({
    close: () => {
      if (closed) return;
      closed = true;
      if (
        operationsByDeploymentIdentity.get(input.deploymentIdentity) ===
        operations
      ) {
        operationsByDeploymentIdentity.delete(input.deploymentIdentity);
      }
    },
    reconcile: (queued, released) => {
      const named = fetchable();
      operations.daFetchAlert.reconcile(
        new Set([
          ...[...queued].filter((hash) => !settled.has(hash)),
          ...named,
        ]),
        new Map([...released].filter(([hash]) => !named.has(hash))),
      );
      for (const hash of settled)
        if (!operations.daFetchAlert.raised(hash)) settled.delete(hash);
    },
    settle: (headerHash) => {
      if (fetchable().has(headerHash)) settled.add(headerHash);
      else operations.daFetchAlert.set(headerHash, false);
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
  /** Parsed once where the raw configuration entered the process. */
  watcherConfig: WatcherConfig;
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
  createRuntime(
    watcherConfig: WatcherConfig,
  ): Promise<WatcherRetainedDaRuntime>;
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
