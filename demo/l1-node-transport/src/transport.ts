import type { Frame } from "./frame.js";
import {
  type ChainPoint,
  encodePoint,
  encodeQuery,
  headerText,
  hexToBytes,
  isTransportFailedReason,
  type LedgerQuery,
  natural,
  type TransportReadiness,
  type TransportUnreadyReason,
} from "./protocol.js";
import {
  type SidecarExit,
  SidecarExitedError,
  SidecarProcess,
} from "./sidecar.js";
import { type ChainSyncOptions, ChainSyncStream } from "./stream.js";
import {
  TransportFailedError,
  transportFailureOf,
} from "./transport-failed.js";

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

/** A ledger state acquired for a sequence of consistent queries. */
export type LedgerStateSession = Readonly<{
  query: (query: LedgerQuery) => Promise<Uint8Array>;
}>;

const DEFAULT_REQUEST_TIMEOUT_MS = 120_000;
const DEFAULT_HELLO_TIMEOUT_MS = 30_000;
const STABLE_RUN_MS = 30_000;

const MAX_REQUEST_DEADLINE_MS = 24 * 60 * 60 * 1000;

/** The sidecar's own bound, below the client's: half of it. */
const requestDeadlineMs = (requestTimeoutMs: number): number =>
  Math.min(Math.floor(requestTimeoutMs / 2), MAX_REQUEST_DEADLINE_MS);

const unreadyReasonOf = (exit: SidecarExit): TransportUnreadyReason => {
  switch (exit.fatal?.code) {
    case "node_unreachable":
      return "node_unreachable";
    case "node_connection_lost":
    case "node_unresponsive":
      return "node_connection_lost";
    case undefined:
    default:
      return "sidecar_restarting";
  }
};

const describe = (exit: SidecarExit): string =>
  exit.fatal !== null
    ? `${exit.fatal.code}: ${exit.fatal.message}`
    : `exit code ${exit.code}, signal ${exit.signal}${
        exit.diagnostics.length === 0
          ? ""
          : `: ${exit.diagnostics.trim().split("\n").slice(-3).join(" | ")}`
      }`;

/**
 * One long-lived sidecar per process role: a supervisor that keeps it
 * running, restarts it with backoff, reports transient readiness, and never
 * ends the process. Chain-sync streams resume across restarts. A sidecar
 * that ends on a fault no restart repairs (`TRANSPORT_FAILED_REASONS`, such
 * as a refused N2C handshake) is not restarted: the readiness turns
 * `failed`, and every call and stream fails with `TransportFailedError`.
 */
export class L1NodeTransport {
  readonly #options: L1NodeTransportOptions;
  readonly #requestTimeoutMs: number;
  readonly #readyTimeoutMs: number;
  readonly #streams = new Set<ChainSyncStream>();
  readonly #readinessListeners = new Set<
    (readiness: TransportReadiness) => void
  >();
  #readyWaiters: Array<() => void> = [];
  #readiness: TransportReadiness = {
    ready: false,
    reason: "sidecar_starting",
    detail: "the sidecar has not started",
  };
  #sidecar: SidecarProcess | undefined;
  #restartTimer: NodeJS.Timeout | undefined;
  #restartDelay: number;
  #stopped = false;
  #nextStreamId = 1;
  #activity = 0;
  #ledgerLock: Promise<void> = Promise.resolve();

  constructor(options: L1NodeTransportOptions) {
    if (
      !Number.isSafeInteger(options.networkMagic) ||
      options.networkMagic < 0 ||
      options.networkMagic > 0xffff_ffff
    )
      throw new RangeError("network magic is not a 32-bit natural number");
    this.#options = options;
    this.#requestTimeoutMs =
      options.requestTimeoutMs ?? DEFAULT_REQUEST_TIMEOUT_MS;
    if (
      !Number.isSafeInteger(this.#requestTimeoutMs) ||
      this.#requestTimeoutMs < 2
    )
      throw new RangeError("requestTimeoutMs must be an integer of at least 2");
    this.#readyTimeoutMs = options.readyTimeoutMs ?? this.#requestTimeoutMs;
    this.#restartDelay = options.restartDelayMs?.initial ?? 250;
    if (options.onReadiness !== undefined)
      this.#readinessListeners.add(options.onReadiness);
    void this.#start();
  }

  get readiness(): TransportReadiness {
    return this.#readiness;
  }

  /** Subscribes to readiness changes; returns the unsubscribe function. */
  onReadiness(listener: (readiness: TransportReadiness) => void): () => void {
    this.#readinessListeners.add(listener);
    return () => this.#readinessListeners.delete(listener);
  }

  #setReadiness(readiness: TransportReadiness): void {
    this.#readiness = readiness;
    if (readiness.ready)
      for (const waiter of this.#readyWaiters.splice(0)) waiter();
    for (const listener of this.#readinessListeners) {
      try {
        listener(readiness);
      } catch {
        // A listener failure never stops the supervisor.
      }
    }
  }

  async #start(): Promise<void> {
    if (this.#stopped) return;
    const startedAt = Date.now();
    let sidecar: SidecarProcess;
    try {
      sidecar = await SidecarProcess.start({
        binaryPath: this.#options.binaryPath,
        socketPath: this.#options.socketPath,
        networkMagic: this.#options.networkMagic,
        requestDeadlineMs: requestDeadlineMs(this.#requestTimeoutMs),
        helloTimeoutMs:
          this.#options.helloTimeoutMs ?? DEFAULT_HELLO_TIMEOUT_MS,
        ...(this.#options.onDiagnostic === undefined
          ? {}
          : { onDiagnostic: this.#options.onDiagnostic }),
      });
    } catch (error) {
      const exit = error instanceof SidecarExitedError ? error.exit : undefined;
      if (exit !== undefined && this.#failOn(exit)) return;
      this.#scheduleRestart(
        exit === undefined || exit.fatal === null
          ? "sidecar_unavailable"
          : unreadyReasonOf(exit),
        exit === undefined ? (error as Error).message : describe(exit),
      );
      return;
    }
    if (this.#stopped) {
      void sidecar.close();
      return;
    }
    this.#sidecar = sidecar;
    sidecar.setReferenced(this.#activity > 0);
    sidecar.onExit((exit) => {
      if (this.#sidecar !== sidecar) return;
      this.#sidecar = undefined;
      for (const stream of this.#streams) stream.detach(exit);
      if (this.#stopped || this.#failOn(exit)) return;
      if (Date.now() - startedAt >= STABLE_RUN_MS)
        this.#restartDelay = this.#options.restartDelayMs?.initial ?? 250;
      this.#scheduleRestart(unreadyReasonOf(exit), describe(exit));
    });
    this.#setReadiness({
      ready: true,
      nodeToClientVersion: sidecar.nodeToClientVersion,
    });
    for (const stream of this.#streams) stream.attach(sidecar);
  }

  /**
   * Ends the supervisor on an exit no restart repairs: the readiness turns
   * `failed`, waiters wake to `TransportFailedError`, and every stream fails
   * with it. Returns whether the exit was one.
   */
  #failOn(exit: SidecarExit): boolean {
    const code = exit.fatal?.code;
    if (code === undefined || !isTransportFailedReason(code)) return false;
    const detail = describe(exit);
    this.#setReadiness({ ready: false, failed: true, reason: code, detail });
    for (const waiter of this.#readyWaiters.splice(0)) waiter();
    const failure = new TransportFailedError(code, detail);
    for (const stream of [...this.#streams]) stream.failWith(failure);
    return true;
  }

  #failure(): TransportFailedError | undefined {
    return transportFailureOf(this.#readiness);
  }

  #scheduleRestart(reason: TransportUnreadyReason, detail: string): void {
    if (this.#stopped) return;
    this.#setReadiness({ ready: false, reason, detail });
    const delay = this.#restartDelay;
    this.#restartDelay = Math.min(
      delay * 2,
      this.#options.restartDelayMs?.max ?? 30_000,
    );
    this.#restartTimer = setTimeout(() => {
      this.#restartTimer = undefined;
      void this.#start();
    }, delay);
    if (this.#activity === 0) this.#restartTimer.unref();
  }

  #activityDelta(delta: number): void {
    const before = this.#activity;
    this.#activity += delta;
    if ((before === 0) === (this.#activity === 0)) return;
    const referenced = this.#activity > 0;
    this.#sidecar?.setReferenced(referenced);
    if (referenced) this.#restartTimer?.ref();
    else this.#restartTimer?.unref();
  }

  /**
   * Resolves once a sidecar is ready, or rejects after the bound; rejects at
   * once with `TransportFailedError` once the transport failed.
   */
  async whenReady(timeoutMs = this.#readyTimeoutMs): Promise<void> {
    if (this.#stopped)
      throw new TransportUnavailableError("stopped", "the transport is closed");
    const failedBefore = this.#failure();
    if (failedBefore !== undefined) throw failedBefore;
    if (this.#readiness.ready && this.#sidecar !== undefined) return;
    let timer: NodeJS.Timeout | undefined;
    let waiter: (() => void) | undefined;
    try {
      await new Promise<void>((resolve, reject) => {
        waiter = resolve;
        this.#readyWaiters.push(resolve);
        timer = setTimeout(() => {
          const readiness = this.#readiness;
          reject(
            readiness.ready
              ? new TransportUnavailableError("sidecar_restarting", "")
              : "failed" in readiness
                ? new TransportFailedError(readiness.reason, readiness.detail)
                : new TransportUnavailableError(
                    readiness.reason,
                    readiness.detail,
                  ),
          );
        }, timeoutMs);
      });
    } finally {
      clearTimeout(timer);
      this.#readyWaiters = this.#readyWaiters.filter((w) => w !== waiter);
    }
    if (this.#stopped)
      throw new TransportUnavailableError("stopped", "the transport is closed");
    const failed = this.#failure();
    if (failed !== undefined) throw failed;
  }

  async #readySidecar(): Promise<SidecarProcess> {
    for (;;) {
      await this.whenReady();
      const sidecar = this.#sidecar;
      if (sidecar !== undefined && sidecar.exited === undefined) return sidecar;
    }
  }

  /** One request with a bound; a wedged sidecar is killed and restarted. */
  async #call(
    sidecar: SidecarProcess,
    header: Parameters<SidecarProcess["request"]>[0],
    payload?: Uint8Array,
  ): Promise<Frame> {
    this.#activityDelta(1);
    let timer: NodeJS.Timeout | undefined;
    try {
      return await Promise.race([
        sidecar.request(header, payload),
        new Promise<never>((_, reject) => {
          timer = setTimeout(() => {
            sidecar.kill();
            reject(
              new TransportTimeoutError(
                `${headerText(header.type)} had no answer within ${this.#requestTimeoutMs} ms`,
              ),
            );
          }, this.#requestTimeoutMs);
        }),
      ]);
    } finally {
      clearTimeout(timer);
      this.#activityDelta(-1);
    }
  }

  /**
   * Acquires one ledger state (the tip, or a recent point), runs `use` with
   * queries against it, and releases it. Sessions are serialized.
   */
  async withLedgerState<T>(
    at: ChainPoint | "tip",
    use: (session: LedgerStateSession) => Promise<T>,
  ): Promise<T> {
    const previous = this.#ledgerLock;
    let unlock!: () => void;
    this.#ledgerLock = new Promise<void>((resolve) => (unlock = resolve));
    this.#activityDelta(1);
    try {
      await previous;
      const sidecar = await this.#readySidecar();
      await this.#call(sidecar, {
        type: "lsq_acquire",
        point: at === "tip" ? undefined : encodePoint(at),
      });
      try {
        return await use({
          query: async (query) => {
            const answer = await this.#call(sidecar, encodeQuery(0, query));
            return answer.payload;
          },
        });
      } finally {
        if (sidecar.exited === undefined)
          await this.#call(sidecar, { type: "lsq_release" }).catch(
            () => undefined,
          );
      }
    } finally {
      this.#activityDelta(-1);
      unlock();
    }
  }

  /** One query at the current tip. */
  async query(query: LedgerQuery): Promise<Uint8Array> {
    return await this.withLedgerState("tip", (session) => session.query(query));
  }

  /** Submits a signed transaction; a rejection carries the node's raw reason. */
  async submit(tx: Uint8Array, era?: number): Promise<SubmitResult> {
    const sidecar = await this.#readySidecar();
    const answer = await this.#call(sidecar, { type: "submit", era }, tx);
    return answer.header.type === "submit_accepted"
      ? { accepted: true }
      : { accepted: false, rejection: answer.payload };
  }

  /** Whether the node's mempool holds the transaction (a fresh snapshot). */
  async hasTx(txId: string): Promise<boolean> {
    const sidecar = await this.#readySidecar();
    const answer = await this.#call(sidecar, {
      type: "monitor_has_tx",
      txId: hexToBytes(txId),
    });
    return answer.header.has === true;
  }

  async mempoolSizes(): Promise<MempoolSizes> {
    const sidecar = await this.#readySidecar();
    const answer = await this.#call(sidecar, { type: "monitor_sizes" });
    return {
      capacity: natural(answer.header.capacity, "mempool capacity"),
      size: natural(answer.header.size, "mempool size"),
      txCount: natural(answer.header.txCount, "mempool transaction count"),
    };
  }

  /** Opens a chain-sync stream; it attaches to every ready sidecar in turn. */
  openChainSync(options: ChainSyncOptions): ChainSyncStream {
    if (this.#stopped)
      throw new TransportUnavailableError("stopped", "the transport is closed");
    const failed = this.#failure();
    if (failed !== undefined) throw failed;
    const stream = new ChainSyncStream(options, {
      nextStreamId: () => this.#nextStreamId++,
      forget: (owned) => this.#streams.delete(owned),
      activity: (delta) => this.#activityDelta(delta),
    });
    this.#streams.add(stream);
    const sidecar = this.#sidecar;
    if (sidecar !== undefined && sidecar.exited === undefined)
      stream.attach(sidecar);
    return stream;
  }

  /** Stops the supervisor, closes every stream and ends the sidecar. */
  async close(): Promise<void> {
    if (this.#stopped) return;
    this.#stopped = true;
    clearTimeout(this.#restartTimer);
    await Promise.all([...this.#streams].map((stream) => stream.close()));
    this.#setReadiness({
      ready: false,
      reason: "stopped",
      detail: "the transport is closed",
    });
    for (const waiter of this.#readyWaiters.splice(0)) waiter();
    const sidecar = this.#sidecar;
    this.#sidecar = undefined;
    await sidecar?.close();
  }
}

const shared = new Map<string, L1NodeTransport>();

/**
 * The process-wide transport for one node: every caller in a process shares
 * one sidecar and one node connection. It idles unreferenced, so a CLI exits
 * once its work is done.
 */
export const sharedL1NodeTransport = (
  options: Pick<
    L1NodeTransportOptions,
    "binaryPath" | "socketPath" | "networkMagic" | "requestTimeoutMs"
  >,
): L1NodeTransport => {
  const key = JSON.stringify([
    options.binaryPath,
    options.socketPath,
    options.networkMagic,
    options.requestTimeoutMs ?? DEFAULT_REQUEST_TIMEOUT_MS,
  ]);
  let transport = shared.get(key);
  if (
    transport === undefined ||
    (transport.readiness.ready === false &&
      transport.readiness.reason === "stopped")
  ) {
    transport = new L1NodeTransport(options);
    shared.set(key, transport);
  }
  return transport;
};

/** Closes every shared transport (process shutdown, tests). */
export const closeSharedL1NodeTransports = async (): Promise<void> => {
  const transports = [...shared.values()];
  shared.clear();
  await Promise.all(transports.map((transport) => transport.close()));
};
