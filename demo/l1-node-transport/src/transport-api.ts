import type {
  LedgerQuery,
  TransportReadiness,
  TransportUnreadyReason,
} from "./protocol.js";

export type L1NodeTransportOptions = Readonly<{
  /** The compiled `midgard-l1-node-transport` binary. */
  binaryPath: string;
  /** The node's local (node-to-client) socket. */
  socketPath: string;
  networkMagic: number;
  /**
   * Bound on one request's answer; a sidecar that misses it is restarted.
   * The sidecar itself refuses a ledger-state query or a submission the node
   * has not answered within half this bound (`node_timeout`), so a slow node
   * answer never costs the sidecar and its streams.
   */
  requestTimeoutMs?: number;
  /** Bound on waiting for a ready sidecar before a request is refused. */
  readyTimeoutMs?: number;
  helloTimeoutMs?: number;
  /** First and last restart delay; the delay doubles between them. */
  restartDelayMs?: Readonly<{ initial: number; max: number }>;
  onDiagnostic?: (line: string) => void;
  onReadiness?: (readiness: TransportReadiness) => void;
}>;

/** The transport is not ready; the reason is transient and retried. */
export class TransportUnavailableError extends Error {
  override readonly name = "TransportUnavailableError";
  constructor(
    readonly reason: TransportUnreadyReason,
    readonly detail: string,
  ) {
    super(`L1 node transport unavailable: ${reason}: ${detail}`);
  }
}

/** A request missed its bound; the sidecar was restarted. */
export class TransportTimeoutError extends Error {
  override readonly name = "TransportTimeoutError";
}

export type SubmitResult =
  | Readonly<{ accepted: true }>
  | Readonly<{ accepted: false; rejection: Uint8Array }>;

export type MempoolSizes = Readonly<{
  capacity: bigint;
  size: bigint;
  txCount: bigint;
}>;

/**
 * Per-request options. `deadline: "none"` takes the request out of the
 * request deadline on both sides: neither the client nor the sidecar abandons
 * it, however long the node takes to answer. It is for a read that is
 * unbounded by nature and runs once (the wallet seed's `utxo_by_address`,
 * a full scan of the node's UTxO set); every other request keeps its bound.
 * The request still ends when the node connection or the sidecar does.
 */
export type QueryOptions = Readonly<{ deadline?: "none" }>;

/** A ledger state acquired for a sequence of consistent queries. */
export type LedgerStateSession = Readonly<{
  query: (query: LedgerQuery, options?: QueryOptions) => Promise<Uint8Array>;
}>;
