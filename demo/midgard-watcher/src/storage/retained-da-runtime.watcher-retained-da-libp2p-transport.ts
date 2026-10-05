import { createHash } from "node:crypto";

import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaAttestationsByHeaderRequestCbor,
  decodeDaEventToStepByEventRequestCbor,
  decodeDaPayloadByHeaderRequestCbor,
  decodeDaPayloadChunkRequestCbor,
  decodeDaProofBundleByHeaderRequestCbor,
  decodeDaTraceStepByIndexRequestCbor,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaLibp2pRequest,
  type RetainedDaLibp2pTransport,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";

import { type WatcherConfig } from "../runtime/config.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { watcherDaFetchAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import type { WatcherPublicDaRequest } from "./public-da-client.js";
import {
  createWatcherPublicDaLibp2pTransport,
  WatcherPublicDaLibp2pTransport,
} from "./public-da-libp2p-transport.js";
import { watcherRetainedDaReadScope } from "./retained-da-runtime.read-scope.js";
import {
  type AdmittedPeer,
  type AdmittedRuntimeOptions,
  l1AvailabilitySourceByDeploymentIdentity,
  operationsSinkByDeploymentIdentity,
  RetainedDaRequestPermits,
  type RetainedDaTransportScope,
  WATCHER_RETAINED_DA_RUNTIME,
  type WatcherRetainedDaRuntime,
} from "./retained-da-runtime.retained-da-request-permits.js";

const HEADER_REQUEST_DECODERS: Readonly<
  Partial<
    Record<
      DaRequestResponseProtocol,
      (payload: Uint8Array) => Readonly<{ headerHash: Buffer }>
    >
  >
> = Object.freeze({
  [DaRequestResponseProtocol.payloadByHeader]:
    decodeDaPayloadByHeaderRequestCbor,
  [DaRequestResponseProtocol.metadataByHeader]:
    decodeDaPayloadByHeaderRequestCbor,
  [DaRequestResponseProtocol.payloadChunk]: decodeDaPayloadChunkRequestCbor,
  [DaRequestResponseProtocol.proofBundleByHeader]:
    decodeDaProofBundleByHeaderRequestCbor,
  [DaRequestResponseProtocol.traceStepByIndex]:
    decodeDaTraceStepByIndexRequestCbor,
  [DaRequestResponseProtocol.eventToStepByEvent]:
    decodeDaEventToStepByEventRequestCbor,
  [DaRequestResponseProtocol.attestationsByHeader]:
    decodeDaAttestationsByHeaderRequestCbor,
});

/**
 * The `da_fetch_failure` subject of one request: its header, so any later
 * outcome for that header (a successful fetch of any kind, a decision, a
 * merge, a removal) clears it. A request that names no header keeps its own
 * payload digest.
 */
export const watcherDaFetchSubjectDigest = (
  args: Pick<RetainedDaLibp2pRequest, "protocol" | "payload">,
): string => {
  const decode = HEADER_REQUEST_DECODERS[args.protocol];
  if (decode !== undefined) {
    try {
      return watcherDaFetchAlertSubject(
        decode(args.payload).headerHash.toString("hex"),
      );
    } catch {
      // A request this watcher could not decode is still reported.
    }
  }
  return createHash("sha256").update(args.payload).digest("hex");
};

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
    const readScope = watcherRetainedDaReadScope(this.deploymentFingerprint);
    const timeoutMs = Math.min(
      this.configuredTimeoutMs,
      Math.max(
        1,
        Math.ceil(readScope?.scope.remainingMs() ?? this.configuredTimeoutMs),
      ),
    );
    const subjectDigest = watcherDaFetchSubjectDigest(args);
    const startedAtMs = Date.now().toString();
    const startedMonotonicMs = performance.now();
    const signal = AbortSignal.any([
      this.lifetime,
      AbortSignal.timeout(timeoutMs),
      ...(readScope === undefined ? [] : [readScope.scope.signal]),
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
        timeoutMs,
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
    const readScope = watcherRetainedDaReadScope();
    this.lifetime?.throwIfAborted();
    const result = await super.fetchPayloadByHeaderHash(headerHash);
    this.lifetime?.throwIfAborted();
    readScope?.scope.assertCurrent();
    if (result.ok) return result;
    const fallback = await (
      readScope?.l1Source ?? this.l1Source
    ).fetchPayloadByHeaderHash(headerHash);
    readScope?.scope.assertCurrent();
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
export const createRuntimeFromAdmittedConfig = async (
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
