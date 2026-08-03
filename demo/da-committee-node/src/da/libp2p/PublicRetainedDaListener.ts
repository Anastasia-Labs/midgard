import { withDaRequestDeadline } from "@al-ft/midgard-core/da-request-deadline";
import {
  DA_PUBLIC_RETAINED_DA_PROTOCOLS_V1,
  DA_TRANSPORT_LIMITS_V1,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import { peerIdFromPrivateKey } from "@libp2p/peer-id";
import { tcp } from "@libp2p/tcp";
import { createLibp2p, type Libp2pOptions } from "libp2p";

import type { PublicRetainedDaConfig } from "../../config.js";
import type { WatcherStore } from "../../store.js";
import type { DaLibp2pStream, DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import { createDaLibp2pPublicRetainedDaPayloadRequestHandlers } from "./payload-source.js";
import { createDaLibp2pProofRequestHandlers } from "./proof-protocols.js";

type PublicRetainedDaStore = Pick<
  WatcherStore,
  "getDaPayload" | "getStateQueueHeader"
>;
type PublicRetainedDaPrivateKey = NonNullable<Libp2pOptions["privateKey"]>;

type PublicRetainedDaRuntimeNode = {
  start(): Promise<void> | void;
  stop(): Promise<void> | void;
  handle(
    protocol: string,
    handler: (stream: unknown, connection: unknown) => Promise<void> | void,
    options?: {
      readonly maxInboundStreams?: number;
      readonly maxOutboundStreams?: number;
      readonly runOnLimitedConnection?: boolean;
    },
  ): Promise<void> | void;
  unhandle(protocol: string): Promise<void> | void;
  getProtocols?(): readonly string[];
  getMultiaddrs?(): readonly { toString(): string }[];
};

export type PublicRetainedDaLibp2pFactory = (
  options: Libp2pOptions,
) => Promise<PublicRetainedDaRuntimeNode>;

export type PublicRetainedDaListenerOptions = {
  readonly deploymentFingerprint: string;
  readonly config: PublicRetainedDaConfig;
  readonly store: PublicRetainedDaStore;
  readonly privateKey: PublicRetainedDaPrivateKey;
  readonly dataLimits: Pick<
    PublicRetainedDaConfig["limits"],
    "maxStreamsPerPeer" | "requestTimeoutMs"
  > & {
    readonly maxPayloadBytes: number;
    readonly maxInlineResponseBytes: number;
    readonly maxChunkBytes: number;
  };
  readonly libp2pFactory?: PublicRetainedDaLibp2pFactory;
};

/**
 * A separate, inbound-only Noise-authenticated process profile for retained
 * public data. It does not reuse the committee node's connection gater,
 * identity, gossip, mutation, signing, or attestation handlers.
 */
export class PublicRetainedDaListener {
  readonly protocols: readonly string[];

  private readonly handlers: ReadonlyMap<string, DaLibp2pStreamHandler>;
  private readonly globalPermits: AsyncPermitPool;
  private readonly proofPermits: AsyncPermitPool;
  private readonly peerPermits = new Map<string, AsyncPermitPool>();
  private readonly config: PublicRetainedDaConfig;
  private readonly libp2pFactory: PublicRetainedDaLibp2pFactory;
  private node?: PublicRetainedDaRuntimeNode;
  private started = false;

  constructor(options: PublicRetainedDaListenerOptions) {
    if (
      peerIdFromPrivateKey(options.privateKey).toString() !==
      options.config.peerId
    ) {
      throw new Error(
        "public retained DA private key does not match configured peer id",
      );
    }
    this.config = options.config;
    const limits = {
      maxPayloadBytes: options.dataLimits.maxPayloadBytes,
      maxInlineResponseBytes: options.dataLimits.maxInlineResponseBytes,
      maxChunkBytes: options.dataLimits.maxChunkBytes,
      maxStreamsPerPeer: options.config.limits.maxStreamsPerPeer,
      requestTimeoutMs: options.config.limits.requestTimeoutMs,
    };
    const payloadHandlers =
      createDaLibp2pPublicRetainedDaPayloadRequestHandlers({
        deploymentFingerprint: options.deploymentFingerprint,
        store: options.store,
        limits,
      });
    const proofHandlers = createDaLibp2pProofRequestHandlers({
      deploymentFingerprint: options.deploymentFingerprint,
      store: options.store,
      limits,
      accessPolicy: { kind: "any_noise_authenticated_peer" },
    });
    const expectedProtocols = DA_PUBLIC_RETAINED_DA_PROTOCOLS_V1.map(
      (protocol) =>
        daRequestResponseProtocolId(options.deploymentFingerprint, protocol),
    );
    this.handlers = new Map(
      expectedProtocols.map((protocolId) => {
        const handler =
          payloadHandlers.get(protocolId) ?? proofHandlers.get(protocolId);
        if (handler === undefined) {
          throw new Error(
            `public retained DA handler is missing ${protocolId}`,
          );
        }
        return [protocolId, handler] as const;
      }),
    );
    this.protocols = Object.freeze([...this.handlers.keys()]);
    this.globalPermits = new AsyncPermitPool(
      options.config.limits.maxInflightRequests,
    );
    this.proofPermits = new AsyncPermitPool(
      options.config.limits.maxInflightProofRequests,
    );
    this.libp2pFactory =
      options.libp2pFactory ?? defaultPublicRetainedDaFactory;
    this.privateKey = options.privateKey;
  }

  private readonly privateKey: PublicRetainedDaPrivateKey;

  isStarted(): boolean {
    return this.started;
  }

  /** Bound listener addresses, including the OS-selected port after startup. */
  getMultiaddrs(): readonly string[] {
    return (
      this.node?.getMultiaddrs?.().map((address) => address.toString()) ?? []
    );
  }

  /** Test-only diagnostic: idle peer keys must not survive public request churn. */
  getActivePeerPermitCountForTest(): number {
    return this.peerPermits.size;
  }

  async start(): Promise<void> {
    if (this.started) return;
    const node = await this.libp2pFactory({
      start: false,
      privateKey: this.privateKey,
      addresses: {
        listen: [...this.config.listenMultiaddrs],
        announce: [...this.config.announceMultiaddrs],
      },
      transports: [tcp()],
      connectionEncrypters: [noise()],
      streamMuxers: [
        yamux({
          maxInboundStreams: this.config.limits.maxStreamsPerPeer,
          maxMessageSize: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
        }),
      ],
      // Public input is accepted only after Noise authentication; outbound and
      // relayed paths are denied because this is a read-only listener.
      connectionGater: {
        denyDialPeer: () => true,
        denyDialMultiaddr: () => true,
        denyOutboundConnection: () => true,
        denyOutboundEncryptedConnection: () => true,
        denyOutboundUpgradedConnection: () => true,
        denyInboundRelayReservation: () => true,
        denyInboundRelayedConnection: () => true,
        denyOutboundRelayedConnection: () => true,
      },
    });
    this.node = node;
    for (const [protocolId, handler] of this.handlers) {
      await node.handle(
        protocolId,
        async (stream, connection) => {
          const remotePeerId = (
            connection as {
              readonly remotePeer?: { toString(): string };
            }
          ).remotePeer?.toString();
          if (remotePeerId === undefined || remotePeerId.length === 0) {
            (stream as DaLibp2pStream).abort?.(
              new Error(
                "public retained DA requires a Noise-authenticated peer",
              ),
            );
            return;
          }
          const typedStream = stream as DaLibp2pStream;
          try {
            await this.runBounded(
              protocolId,
              remotePeerId,
              typedStream,
              connection,
              handler,
            );
          } catch (cause) {
            if (cause instanceof PublicRetainedDaOverloadError) {
              // A rejected admission must tear down the public stream rather
              // than leaving an unconsumed peer-side writer alive. Deadlines
              // already abort their stream through withDaRequestDeadline.
              typedStream.abort?.(cause);
              await typedStream.close?.();
            }
            throw cause;
          }
        },
        {
          maxInboundStreams: this.config.limits.maxStreamsPerPeer,
          maxOutboundStreams: 0,
          runOnLimitedConnection: false,
        },
      );
    }
    await node.start();
    this.started = true;
  }

  async stop(): Promise<void> {
    const node = this.node;
    if (node === undefined) return;
    const failures: unknown[] = [];
    for (const protocolId of this.protocols) {
      try {
        await node.unhandle(protocolId);
      } catch (error) {
        failures.push(error);
      }
    }
    try {
      await node.stop();
    } catch (error) {
      failures.push(error);
    } finally {
      this.peerPermits.clear();
      this.node = undefined;
      this.started = false;
    }
    if (failures.length > 0) {
      throw new AggregateError(
        failures,
        "public retained DA listener shutdown failed",
      );
    }
  }

  private async runBounded(
    protocolId: string,
    remotePeerId: string,
    stream: DaLibp2pStream,
    connection: unknown,
    handler: DaLibp2pStreamHandler,
  ): Promise<void> {
    const execute = async (): Promise<void> =>
      withDaRequestDeadline({
        timeoutMs: this.config.limits.requestTimeoutMs,
        open: async () => stream,
        run: async (openedStream) =>
          handler({
            protocolId,
            protocolName: protocolId,
            stream: openedStream,
            connection: connection as never,
            remotePeerId,
          }),
        abort: (openedStream, error) => openedStream.abort?.(error),
      });
    await this.globalPermits.run(async () => {
      // Global admission happens before allocating peer state, bounding the
      // map to actively admitted public work even under Sybil churn.
      const peerPermits =
        this.peerPermits.get(remotePeerId) ??
        new AsyncPermitPool(this.config.limits.maxInflightRequestsPerPeer);
      this.peerPermits.set(remotePeerId, peerPermits);
      try {
        await peerPermits.run(async () =>
          isProofProtocol(protocolId)
            ? this.proofPermits.run(execute)
            : execute(),
        );
      } finally {
        if (
          peerPermits.isIdle &&
          this.peerPermits.get(remotePeerId) === peerPermits
        ) {
          this.peerPermits.delete(remotePeerId);
        }
      }
    });
  }
}

const isProofProtocol = (protocolId: string): boolean =>
  protocolId.endsWith("/proof-bundle-by-header/1") ||
  protocolId.endsWith("/trace-step-by-index/1") ||
  protocolId.endsWith("/event-to-step-by-event/1");

class AsyncPermitPool {
  private active = 0;

  constructor(private readonly limit: number) {
    if (!Number.isSafeInteger(limit) || limit <= 0) {
      throw new RangeError(
        "public retained DA permit limit must be a positive integer",
      );
    }
  }

  get isIdle(): boolean {
    return this.active === 0;
  }

  acquire(): () => void {
    if (this.active >= this.limit) {
      // Do not retain an unbounded queue of public requests. The caller's
      // stream deadline is not a memory-budget mechanism, so overload is a
      // deterministic rejection rather than delayed work.
      throw new PublicRetainedDaOverloadError();
    }
    this.active += 1;
    let released = false;
    return () => {
      if (released) return;
      released = true;
      this.active -= 1;
    };
  }

  async run<T>(operation: () => Promise<T>): Promise<T> {
    const release = this.acquire();
    try {
      return await operation();
    } finally {
      release();
    }
  }
}

const defaultPublicRetainedDaFactory: PublicRetainedDaLibp2pFactory = async (
  options,
): Promise<PublicRetainedDaRuntimeNode> =>
  createLibp2p(options) as Promise<PublicRetainedDaRuntimeNode>;

class PublicRetainedDaOverloadError extends Error {
  constructor() {
    super("public retained DA is overloaded");
    this.name = "PublicRetainedDaOverloadError";
  }
}
