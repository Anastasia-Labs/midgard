import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  daRequestResponseProtocolId,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import {
  DaLibp2pRetainedDaSource,
  type FamilyCommonInfrastructure,
  type FamilyReferenceScriptResolver,
  type RetainedDaLibp2pRequest,
  type RetainedDaLibp2pTransport,
  type RetainedDaPayloadSource,
  WORKFLOW_RUNTIME_CONFIG,
  type WorkflowAdapterReadinessInput,
  type WorkflowRuntimeLoader,
} from "@al-ft/midgard-fault-proofs";

import { assertWatcherL1AvailabilityPayloadSource } from "../availability/published-payload.js";
import {
  parseWatcherConfig,
  parseWatcherConfigJson,
  type WatcherConfig,
} from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import type { WatcherPublicDaRequest } from "./public-da-client.js";
import {
  createWatcherPublicDaLibp2pTransport,
  WatcherPublicDaLibp2pTransport,
  type WatcherPublicDaLibp2pTransportOptions,
} from "./public-da-libp2p-transport.js";

export const WATCHER_RETAINED_DA_RUNTIME =
  "midgard-watcher-production-retained-da-runtime-v1" as const;

const operationsSinkByDeploymentIdentity = new WeakMap<
  VerifiedWatcherDeploymentIdentity,
  WatcherOperationsSink
>();
const l1AvailabilitySourceByDeploymentIdentity = new WeakMap<
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

export type WatcherRetainedDaRuntimeOwner = Readonly<{
  createRuntime(watcherConfig: unknown): Promise<WatcherRetainedDaRuntime>;
  close(): Promise<void>;
}>;

const runtimeOwnerIdentities = new WeakMap<
  WatcherRetainedDaRuntimeOwner,
  VerifiedWatcherDeploymentIdentity
>();

/** One bounded request queue for all leases sharing a public peer identity. */
class RetainedDaRequestPermits {
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

type RetainedDaTransportScope = Readonly<{
  open(): Promise<WatcherPublicDaLibp2pTransport>;
  signal: AbortSignal;
  acquireRequest(signal: AbortSignal): Promise<() => void>;
}>;

/**
 * Keeps one client identity/connection alive across bounded workflow leases.
 * Ownership is explicit: lease close revokes its requests; owner close tears
 * down the node. Repeated classifications must not redial as new peers.
 */
export const createWatcherRetainedDaRuntimeOwner = (
  options: Omit<WatcherRetainedDaRuntimeOptions, "watcherConfig">,
): WatcherRetainedDaRuntimeOwner => {
  const admitted = admitRuntimeOptions(options);
  const controller = new AbortController();
  let configuration: string | undefined;
  let transport: Promise<WatcherPublicDaLibp2pTransport> | undefined;
  let permits: RetainedDaRequestPermits | undefined;
  let closePromise: Promise<void> | undefined;
  const owner: WatcherRetainedDaRuntimeOwner = Object.freeze({
    createRuntime: async (watcherConfig: unknown) => {
      controller.signal.throwIfAborted();
      const config = parseWatcherConfig(watcherConfig);
      return await createRuntimeFromAdmittedConfig(admitted, config, {
        signal: controller.signal,
        open: async () => {
          controller.signal.throwIfAborted();
          const binding = JSON.stringify({
            mode: config.mode,
            targetNetwork: config.targetNetwork,
            customNetwork: config.customNetwork,
            da: config.da,
          });
          if (configuration !== undefined && configuration !== binding) {
            throw new Error("retained-DA owner configuration changed");
          }
          if (transport === undefined) {
            configuration = binding;
            permits = new RetainedDaRequestPermits(config.da.maxConcurrency);
            transport = (
              admitted.unsafeTransportFactoryForTest ??
              createWatcherPublicDaLibp2pTransport
            )(admitted.unsafeTransportOptionsForTest);
          }
          const opened = await transport;
          controller.signal.throwIfAborted();
          return opened;
        },
        acquireRequest: async (signal) => await permits!.acquire(signal),
      });
    },
    close: () => {
      if (closePromise !== undefined) return closePromise;
      controller.abort(new Error("retained-DA runtime owner is closed"));
      closePromise = (async () => {
        await (await transport)?.stop();
      })();
      return closePromise;
    },
  });
  runtimeOwnerIdentities.set(owner, admitted.deploymentIdentity);
  return owner;
};

type AdmittedRuntimeOptions = Readonly<{
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
const admitRuntimeOptions = (
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

/** What the watcher builds for one invocation and every family draws from. */
export type WatcherWorkflowInfrastructure = Readonly<{
  infrastructure: FamilyCommonInfrastructure;
  resolveReferenceScript: FamilyReferenceScriptResolver;
}>;

export type WatcherWorkflowInfrastructureBuilder = (input: {
  readonly watcherConfig: WatcherConfig;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<WatcherWorkflowInfrastructure>;

type AdmittedPeer = WatcherConfig["da"]["peers"][number];

class WatcherRetainedDaLibp2pTransport implements RetainedDaLibp2pTransport {
  private readonly peerById: ReadonlyMap<string, AdmittedPeer>;

  constructor(
    private readonly deploymentFingerprint: string,
    peers: readonly AdmittedPeer[],
    private readonly transport: WatcherPublicDaLibp2pTransport,
    private readonly configuredTimeoutMs: number,
    private readonly operationsSink: WatcherOperationsSink | undefined,
    private readonly customNetwork: WatcherPublicDaRequest["customNetwork"],
    private readonly lifetime: AbortSignal,
    private readonly acquireRequest: (
      signal: AbortSignal,
    ) => Promise<() => void>,
  ) {
    this.peerById = new Map(peers.map((peer) => [peer.peerId, peer]));
  }

  async request(args: RetainedDaLibp2pRequest): Promise<Uint8Array> {
    const peer = this.peerById.get(args.peer.peerId);
    if (peer === undefined) {
      throw new Error(
        "retained-DA request selected a peer outside the admitted public configuration",
      );
    }
    if (
      !Number.isSafeInteger(args.timeoutMs) ||
      args.timeoutMs <= 0 ||
      args.timeoutMs !== this.configuredTimeoutMs
    ) {
      throw new Error(
        "retained-DA request timeout differs from the admitted watcher configuration",
      );
    }
    const subjectDigest = createHash("sha256")
      .update(args.payload)
      .digest("hex");
    const startedAtMs = Date.now().toString();
    const startedMonotonicMs = performance.now();
    const signal = AbortSignal.any([
      this.lifetime,
      AbortSignal.timeout(this.configuredTimeoutMs),
    ]);
    let release: (() => void) | undefined;
    try {
      release = await this.acquireRequest(signal);
      signal.throwIfAborted();
      const response = await this.transport.request({
        peerIdentity: peer.identity,
        peerId: peer.peerId,
        multiaddr: peer.multiaddr,
        protocol: args.protocol,
        protocolId: daRequestResponseProtocolId(
          this.deploymentFingerprint,
          args.protocol,
        ),
        requestCbor: args.payload,
        timeoutMs: this.configuredTimeoutMs,
        signal,
        ...(this.customNetwork === undefined
          ? {}
          : {
              customNetwork: this.customNetwork,
            }),
      });
      signal.throwIfAborted();
      const completedAtMs = Date.now().toString();
      this.operationsSink?.recordDaFetch({
        subjectDigest,
        startedAtMs,
        completedAtMs,
        elapsedMs: Math.ceil(performance.now() - startedMonotonicMs).toString(),
        outcome: "succeeded",
      });
      this.operationsSink?.setAlert({
        code: "da_fetch_failure",
        subjectDigest,
        active: false,
        observedAtMs: completedAtMs,
      });
      return response;
    } catch (error) {
      const completedAtMs = Date.now().toString();
      try {
        this.operationsSink?.recordDaFetch({
          subjectDigest,
          startedAtMs,
          completedAtMs,
          elapsedMs: Math.ceil(
            performance.now() - startedMonotonicMs,
          ).toString(),
          outcome:
            error instanceof DOMException && error.name === "TimeoutError"
              ? "timed_out"
              : "failed",
        });
        this.operationsSink?.setAlert({
          code: "da_fetch_failure",
          subjectDigest,
          active: true,
          observedAtMs: completedAtMs,
        });
      } catch (diagnosticError) {
        throw new AggregateError(
          [error, diagnosticError],
          "DA fetch and failure diagnostics failed",
          { cause: error },
        );
      }
      throw error;
    } finally {
      release?.();
    }
  }
}

export class WatcherRetainedDaSourceWithL1Fallback extends DaLibp2pRetainedDaSource {
  constructor(
    options: ConstructorParameters<typeof DaLibp2pRetainedDaSource>[0],
    private readonly l1Source: RetainedDaPayloadSource,
    private readonly lifetime?: AbortSignal,
  ) {
    super(options);
  }

  override async fetchPayloadByHeaderHash(headerHash: string) {
    this.lifetime?.throwIfAborted();
    const result = await super.fetchPayloadByHeaderHash(headerHash);
    this.lifetime?.throwIfAborted();
    if (result.ok) return result;
    const fallback = await this.l1Source.fetchPayloadByHeaderHash(headerHash);
    this.lifetime?.throwIfAborted();
    return {
      ...fallback,
      attempts: [...result.attempts, ...fallback.attempts],
    };
  }
}

/**
 * Compiled public-DA authority for production fault-proof workflows.
 *
 * The watcher config parser admits direct public DNS TCP peers, plus explicit
 * Custom-chain ip4 peers. The signed deployment identity supplies the
 * protocol namespace. One source is created per peer so the shared evidence
 * layer can preserve independent attempts instead of silently treating an
 * operator-private endpoint or local file as public evidence.
 */
const createRuntimeFromAdmittedConfig = async (
  options: AdmittedRuntimeOptions,
  config: WatcherConfig,
  shared?: RetainedDaTransportScope,
): Promise<WatcherRetainedDaRuntime> => {
  assertVerifiedWatcherDeploymentIdentity(options.deploymentIdentity);
  if (config.mode !== "acceptance") {
    throw new Error(
      "production retained-DA runtime requires an admitted acceptance-mode watcher configuration",
    );
  }
  const deploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    options.deploymentIdentity.manifestId,
  );
  if (
    options.deploymentIdentity.durableMarker.manifestId !==
    deploymentFingerprint
  ) {
    throw new Error(
      "production retained-DA deployment marker differs from the verified manifest",
    );
  }
  if (config.targetNetwork !== options.deploymentIdentity.network) {
    throw new Error(
      "production retained-DA watcher network differs from the verified deployment",
    );
  }
  const lifetime = new AbortController();
  const permits = new RetainedDaRequestPermits(config.da.maxConcurrency);
  const transport =
    shared === undefined
      ? await (
          options.unsafeTransportFactoryForTest ??
          createWatcherPublicDaLibp2pTransport
        )(options.unsafeTransportOptionsForTest)
      : await shared.open();
  try {
    const signal =
      shared === undefined
        ? lifetime.signal
        : AbortSignal.any([lifetime.signal, shared.signal]);
    const adapter = new WatcherRetainedDaLibp2pTransport(
      deploymentFingerprint,
      config.da.peers,
      transport,
      config.da.requestTimeoutMs,
      operationsSinkByDeploymentIdentity.get(options.deploymentIdentity),
      config.targetNetwork === "Custom"
        ? Object.freeze({
            watcherConfig: config,
            deploymentIdentity: options.deploymentIdentity,
          })
        : undefined,
      signal,
      shared?.acquireRequest ?? ((signal) => permits.acquire(signal)),
    );
    const l1Source = l1AvailabilitySourceByDeploymentIdentity.get(
      options.deploymentIdentity,
    );
    const publicSources = config.da.peers.map((peer, index) => {
      const options = {
        sourceId: `watcher-public-da/${peer.identity}`,
        deploymentFingerprint,
        peers: [{ peerId: peer.peerId }],
        transport: adapter,
        timeoutMs: config.da.requestTimeoutMs,
      };
      return l1Source !== undefined && index === config.da.peers.length - 1
        ? new WatcherRetainedDaSourceWithL1Fallback(options, l1Source, signal)
        : new DaLibp2pRetainedDaSource(options);
    });
    const sources = Object.freeze(publicSources);
    let closed = false;
    return Object.freeze({
      schemaVersion: WATCHER_RETAINED_DA_RUNTIME,
      deploymentFingerprint,
      sources,
      close: async (): Promise<void> => {
        if (closed) return;
        closed = true;
        lifetime.abort(new Error("retained-DA runtime lease is closed"));
        if (shared === undefined) await transport.stop();
      },
    });
  } catch (cause) {
    lifetime.abort(cause);
    if (shared === undefined) await transport.stop();
    throw cause;
  }
};

export const createWatcherRetainedDaRuntime = async (
  options: WatcherRetainedDaRuntimeOptions,
): Promise<WatcherRetainedDaRuntime> => {
  const { watcherConfig, ...runtimeOptions } = options;
  return await createRuntimeFromAdmittedConfig(
    admitRuntimeOptions(runtimeOptions),
    parseWatcherConfig(watcherConfig),
  );
};

/**
 * The watcher's one shared-workflow loader, the same for every family.
 *
 * `runtimeConfigPath` is the strict watcher configuration file. It may name
 * public network infrastructure and secret *sources*, never prepared proof
 * evidence. The common infrastructure (Lucid, signer, mutation lease
 * coordinator, the optional parts a family may require) and the reference
 * resolver are constructed by the application callback after this loader has
 * independently bound public DA to the verified deployment; each family's
 * record then resolves its own roster and binds its own config from them. The
 * shared runner owns and always invokes `close`.
 */
export const createWatcherWorkflowRuntimeLoader = (
  options: Omit<WatcherRetainedDaRuntimeOptions, "watcherConfig"> & {
    readonly buildInfrastructure: WatcherWorkflowInfrastructureBuilder;
    readonly runtimeOwner?: WatcherRetainedDaRuntimeOwner;
  },
): WorkflowRuntimeLoader => {
  const {
    buildInfrastructure,
    runtimeOwner,
    deploymentIdentity,
    unsafeTransportFactoryForTest,
    unsafeTransportOptionsForTest,
  } = options;
  if (typeof buildInfrastructure !== "function") {
    throw new Error(
      "production workflow runtime omitted its infrastructure builder",
    );
  }
  const admittedRuntimeOptions = admitRuntimeOptions({
    deploymentIdentity,
    ...(unsafeTransportOptionsForTest === undefined
      ? {}
      : { unsafeTransportOptionsForTest }),
    ...(unsafeTransportFactoryForTest === undefined
      ? {}
      : { unsafeTransportFactoryForTest }),
  });
  if (
    runtimeOwner !== undefined &&
    runtimeOwnerIdentities.get(runtimeOwner) !== deploymentIdentity
  ) {
    throw new Error("retained-DA owner belongs to another deployment identity");
  }
  return async ({ runtimeConfigPath, invocation }) => {
    if (
      !isAbsolute(runtimeConfigPath) ||
      runtimeConfigPath.trim() !== runtimeConfigPath
    ) {
      throw new Error(
        "production workflow runtime config path must be canonical and absolute",
      );
    }
    const canonicalRuntimeConfigPath = await realpath(runtimeConfigPath);
    if (canonicalRuntimeConfigPath !== runtimeConfigPath) {
      throw new Error(
        "production workflow runtime config path must not traverse a symlink or non-canonical segment",
      );
    }
    if (
      invocation.deploymentFingerprint !==
      admittedRuntimeOptions.deploymentIdentity.manifestId
    ) {
      throw new Error(
        "production workflow invocation deployment differs from verified watcher authority",
      );
    }
    const watcherConfig = parseWatcherConfigJson(
      await readFile(canonicalRuntimeConfigPath, "utf8"),
    );
    const retainedDa =
      runtimeOwner === undefined
        ? await createRuntimeFromAdmittedConfig(
            admittedRuntimeOptions,
            watcherConfig,
          )
        : await runtimeOwner.createRuntime(watcherConfig);
    try {
      const { infrastructure, resolveReferenceScript } =
        await buildInfrastructure({ watcherConfig, invocation });
      return {
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        infrastructure,
        resolveReferenceScript,
        retainedDaSources: retainedDa.sources,
        close: retainedDa.close,
      };
    } catch (cause) {
      await retainedDa.close();
      throw cause;
    }
  };
};
