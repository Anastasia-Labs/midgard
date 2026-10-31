import { withDaRequestDeadline } from "@al-ft/midgard-core/da-request-deadline";
import {
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DA_TRANSPORT_LIMITS,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { noise } from "@chainsafe/libp2p-noise";
import { yamux } from "@chainsafe/libp2p-yamux";
import { peerIdFromPrivateKey } from "@libp2p/peer-id";
import { ping } from "@libp2p/ping";
import { tcp } from "@libp2p/tcp";
import { createLibp2p, type Libp2pOptions } from "libp2p";

import type { PublicRetainedDaConfig } from "../../config.js";
import type { CommitteeStore } from "../../store.js";
import type { DaLibp2pStream, DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import { createDaLibp2pPublicRetainedDaPayloadRequestHandlers } from "./payload-source.js";
import { createDaLibp2pProofRequestHandlers } from "./proof-protocols.js";

type PublicRetainedDaStore = Pick<
  CommitteeStore,
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
  /**
   * How long a request at a full permit pool waits for a permit before it is
   * refused as overloaded. Defaults to a quarter of the request deadline,
   * capped at {@link PUBLIC_RETAINED_DA_ADMISSION_WAIT_MS}.
   */
  readonly admissionWaitMs?: number;
  readonly nowMs?: () => number;
};

/** Ceiling of the default admission wait at a full permit pool. */
export const PUBLIC_RETAINED_DA_ADMISSION_WAIT_MS = 1_000;

/** What the listener reports to its process's readiness probe. */
export type PublicRetainedDaListenerStatus = {
  readonly bound: boolean;
  readonly lastServedOkAtMs?: number;
  /** The last request that failed for a reason other than overload. */
  readonly lastServedErrorAtMs?: number;
  readonly lastServedError?: string;
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
  private readonly admissionWaitMs: number;
  private readonly nowMs: () => number;
  private node?: PublicRetainedDaRuntimeNode;
  private started = false;
  private lastServedOkAtMs?: number;
  private lastServedErrorAtMs?: number;
  private lastServedError?: string;

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
    const expectedProtocols = DA_PUBLIC_RETAINED_DA_PROTOCOLS.map((protocol) =>
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
    this.admissionWaitMs =
      options.admissionWaitMs ??
      Math.min(
        PUBLIC_RETAINED_DA_ADMISSION_WAIT_MS,
        Math.floor(options.config.limits.requestTimeoutMs / 4),
      );
    this.nowMs = options.nowMs ?? Date.now;
    this.globalPermits = new AsyncPermitPool(
      options.config.limits.maxInflightRequests,
      this.admissionWaitMs,
    );
    this.proofPermits = new AsyncPermitPool(
      options.config.limits.maxInflightProofRequests,
      this.admissionWaitMs,
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

  status(): PublicRetainedDaListenerStatus {
    return {
      bound: this.started && this.getMultiaddrs().length > 0,
      ...(this.lastServedOkAtMs === undefined
        ? {}
        : { lastServedOkAtMs: this.lastServedOkAtMs }),
      ...(this.lastServedErrorAtMs === undefined
        ? {}
        : {
            lastServedErrorAtMs: this.lastServedErrorAtMs,
            ...(this.lastServedError === undefined
              ? {}
              : { lastServedError: this.lastServedError }),
          }),
    };
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
          // Ping permits two inbound streams for asynchronous close/open
          // ordering. DA admission remains independently bounded below.
          maxInboundStreams: this.config.limits.maxStreamsPerPeer + 2,
          maxOutboundStreams: 1,
          maxMessageSize: DA_TRANSPORT_LIMITS.maxChunkBytes,
        }),
      ],
      services: { ping: ping() },
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
            this.lastServedOkAtMs = this.nowMs();
          } catch (cause) {
            if (!(cause instanceof PublicRetainedDaOverloadError)) {
              this.lastServedErrorAtMs = this.nowMs();
              this.lastServedError =
                cause instanceof Error ? cause.message : String(cause);
            }
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
        new AsyncPermitPool(
          Math.min(
            this.config.limits.maxStreamsPerPeer,
            this.config.limits.maxInflightRequestsPerPeer,
          ),
          this.admissionWaitMs,
        );
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

/**
 * A fixed number of permits. A request at a full pool waits a bounded time
 * for one (a burst slightly over the limit is served rather than refused),
 * and the wait queue is itself bounded by the limit, so memory stays bounded
 * under any load; past either bound the request is refused as overloaded.
 */
class AsyncPermitPool {
  private active = 0;
  private readonly waiters: (() => void)[] = [];

  constructor(
    private readonly limit: number,
    private readonly waitMs: number,
  ) {
    if (!Number.isSafeInteger(limit) || limit <= 0) {
      throw new RangeError(
        "public retained DA permit limit must be a positive integer",
      );
    }
  }

  get isIdle(): boolean {
    return this.active === 0 && this.waiters.length === 0;
  }

  async acquire(): Promise<() => void> {
    if (this.active < this.limit) {
      this.active += 1;
    } else if (this.waitMs <= 0 || this.waiters.length >= this.limit) {
      throw new PublicRetainedDaOverloadError();
    } else {
      // A released permit is handed straight to the first waiter, so the
      // active count does not change while it passes between them.
      await new Promise<void>((resolve, reject) => {
        const admit = (): void => {
          clearTimeout(timer);
          resolve();
        };
        const timer = setTimeout(() => {
          const index = this.waiters.indexOf(admit);
          if (index >= 0) this.waiters.splice(index, 1);
          reject(new PublicRetainedDaOverloadError());
        }, this.waitMs);
        this.waiters.push(admit);
      });
    }
    let released = false;
    return () => {
      if (released) return;
      released = true;
      const next = this.waiters.shift();
      if (next === undefined) this.active -= 1;
      else next();
    };
  }

  async run<T>(operation: () => Promise<T>): Promise<T> {
    const release = await this.acquire();
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
