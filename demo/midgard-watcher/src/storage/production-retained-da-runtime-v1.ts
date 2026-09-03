import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  daRequestResponseProtocolId,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaLibp2pRequest,
  type RetainedDaLibp2pTransport,
  WORKFLOW_RUNTIME_CONFIG,
  type WorkflowAdapterReadinessInput,
  type WorkflowRuntimeConfigLoader,
} from "@al-ft/midgard-fault-proofs";

import {
  parseWatcherConfig,
  parseWatcherConfigJson,
  type WatcherConfig,
} from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import type { WatcherOperationsSink } from "../runtime/production-operations-observability-v1.js";
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

export type WatcherWorkflowInfrastructureBuilder<Config> = (input: {
  readonly watcherConfig: WatcherConfig;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<Config>;

type AdmittedPeer = WatcherConfig["da"]["peers"][number];

class WatcherRetainedDaLibp2pTransport implements RetainedDaLibp2pTransport {
  private readonly peerById: ReadonlyMap<string, AdmittedPeer>;

  constructor(
    private readonly deploymentFingerprint: string,
    peers: readonly AdmittedPeer[],
    private readonly transport: WatcherPublicDaLibp2pTransport,
    private readonly configuredTimeoutMs: number,
    private readonly operationsSink: WatcherOperationsSink | undefined,
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
    try {
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
        signal: AbortSignal.timeout(this.configuredTimeoutMs),
      });
      const completedAtMs = Date.now().toString();
      this.operationsSink?.recordDaFetch({
        subjectDigest,
        startedAtMs,
        completedAtMs,
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
      this.operationsSink?.recordDaFetch({
        subjectDigest,
        startedAtMs,
        completedAtMs,
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
      throw error;
    }
  }
}

/**
 * Compiled public-DA authority for production fault-proof workflows.
 *
 * The watcher config parser admits only direct public DNS TCP multiaddrs with
 * embedded peer identities. The signed deployment identity supplies the
 * protocol namespace. One source is created per peer so the shared evidence
 * layer can preserve independent attempts instead of silently treating an
 * operator-private endpoint or local file as public evidence.
 */
const createRuntimeFromAdmittedConfig = async (
  options: AdmittedRuntimeOptions,
  config: WatcherConfig,
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
  const transport = await (
    options.unsafeTransportFactoryForTest ??
    createWatcherPublicDaLibp2pTransport
  )(options.unsafeTransportOptionsForTest);
  try {
    const adapter = new WatcherRetainedDaLibp2pTransport(
      deploymentFingerprint,
      config.da.peers,
      transport,
      config.da.requestTimeoutMs,
      operationsSinkByDeploymentIdentity.get(options.deploymentIdentity),
    );
    const sources = Object.freeze(
      config.da.peers.map(
        (peer) =>
          new DaLibp2pRetainedDaSource({
            sourceId: `watcher-public-da/${peer.identity}`,
            deploymentFingerprint,
            peers: [{ peerId: peer.peerId }],
            transport: adapter,
            timeoutMs: config.da.requestTimeoutMs,
          }),
      ),
    );
    let closed = false;
    return Object.freeze({
      schemaVersion: WATCHER_RETAINED_DA_RUNTIME,
      deploymentFingerprint,
      sources,
      close: async (): Promise<void> => {
        if (closed) return;
        closed = true;
        await transport.stop();
      },
    });
  } catch (cause) {
    await transport.stop();
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
 * Concrete shared-workflow loader for the watcher application.
 *
 * `runtimeConfigPath` is the strict watcher configuration file. It may name
 * public network infrastructure and secret *sources*, never prepared proof
 * evidence. Family-specific capabilities (Lucid, signer, manifest-bound
 * reference UTxOs and mutation lease coordinator) are constructed by the
 * application callback after this loader has independently bound public DA to
 * the verified deployment. The shared runner owns and always invokes `close`.
 */
export const createWatcherWorkflowRuntimeLoader = <Config>(
  options: Omit<WatcherRetainedDaRuntimeOptions, "watcherConfig"> & {
    readonly buildInfrastructureConfig: WatcherWorkflowInfrastructureBuilder<Config>;
  },
): WorkflowRuntimeConfigLoader<Config> => {
  const {
    buildInfrastructureConfig,
    deploymentIdentity,
    unsafeTransportFactoryForTest,
    unsafeTransportOptionsForTest,
  } = options;
  if (typeof buildInfrastructureConfig !== "function") {
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
    const retainedDa = await createRuntimeFromAdmittedConfig(
      admittedRuntimeOptions,
      watcherConfig,
    );
    try {
      return {
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        config: await buildInfrastructureConfig({
          watcherConfig,
          invocation,
        }),
        retainedDaSources: retainedDa.sources,
        close: retainedDa.close,
      };
    } catch (cause) {
      await retainedDa.close();
      throw cause;
    }
  };
};
