import {
  SidecarExitedError,
  TransportRequestError,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";

/**
 * Typed failures of the follower's Lucid provider. None of them ends the
 * process: a transient one is retried by the caller with backoff, every other
 * one names what the caller asked for that the provider cannot answer.
 */
export class L1ProviderError extends Error {
  override readonly name: string = "L1ProviderError";
}

/** Where a transient failure came from. */
export type TransientSource = "transport" | "store" | "follower";

/**
 * The node transport, the local state query or the fact store is not
 * available right now (a sidecar restart, a node reconnect, a lost database
 * connection, a follower that is not initialized yet). Retry with backoff.
 */
export class L1ProviderTransientError extends L1ProviderError {
  override readonly name = "L1ProviderTransientError";
  constructor(
    readonly source: TransientSource,
    readonly reason: string,
    options?: { cause?: unknown },
  ) {
    super(`L1 provider ${source} unavailable: ${reason}`, options);
  }
}

/** The sidecar refused a request for a reason a retry does not change. */
export class L1ProviderRequestError extends L1ProviderError {
  override readonly name = "L1ProviderRequestError";
  constructor(
    readonly code: string,
    message: string,
    options?: { cause?: unknown },
  ) {
    super(`L1 provider request refused: ${code}: ${message}`, options);
  }
}

/** The ledger rejected a submitted transaction; `rejection` is the node's raw `ApplyTxErr`. */
export class L1SubmitRejectedError extends L1ProviderError {
  override readonly name = "L1SubmitRejectedError";
  readonly rejectionHex: string;
  constructor(
    readonly txHash: string,
    readonly rejection: Uint8Array,
  ) {
    const rejectionHex = Buffer.from(rejection).toString("hex");
    super(`the ledger rejected transaction ${txHash}: ${rejectionHex}`);
    this.rejectionHex = rejectionHex;
  }
}

/**
 * By-hash content (a datum preimage) the facts do not hold yet. The §12
 * resolution may supply it later; it is never answered as empty.
 */
export class L1CarriagePendingError extends L1ProviderError {
  override readonly name = "L1CarriagePendingError";
  constructor(
    readonly kind: "datum",
    readonly hash: string,
  ) {
    super(`carriage pending: no ${kind} with hash ${hash} in the L1 facts`);
  }
}

/**
 * The query falls outside what the follower can answer completely: a payment
 * credential the tracked set does not hold (the ledger state query cannot
 * enumerate by credential).
 */
export class L1ProviderScopeError extends L1ProviderError {
  override readonly name = "L1ProviderScopeError";
  constructor(
    readonly query: string,
    readonly detail: string,
  ) {
    super(`${query} is outside the follower's tracked scope: ${detail}`);
  }
}

/** `getUtxoByUnit` found no live tracked output, or more than one, holding the unit. */
export class L1UnitLookupError extends L1ProviderError {
  override readonly name = "L1UnitLookupError";
  constructor(
    readonly unit: string,
    readonly found: number,
  ) {
    super(
      found === 0
        ? `unit ${unit} is held by no live tracked output`
        : `unit ${unit} is held by ${found} live tracked outputs, not one`,
    );
  }
}

/** `awaitTx` saw no landing within its bound. */
export class L1AwaitTxTimeoutError extends L1ProviderError {
  override readonly name = "L1AwaitTxTimeoutError";
  constructor(
    readonly txHash: string,
    readonly timeoutMs: number,
    readonly lastSeen: "in_mempool" | "absent" | "unknown",
  ) {
    super(
      `transaction ${txHash} did not land in the L1 facts within ${timeoutMs} ms (last seen: ${lastSeen})`,
    );
  }
}

/** Phase-two evaluation is local only: complete with `localUPLCEval: true`. */
export class L1LocalEvaluationOnlyError extends L1ProviderError {
  override readonly name = "L1LocalEvaluationOnlyError";
  constructor() {
    super(
      "the follower provider never evaluates scripts remotely; complete the transaction with localUPLCEval: true",
    );
  }
}

/** Sidecar refusals that clear on their own: retried like an outage. */
const TRANSIENT_REQUEST_CODES = new Set([
  "busy",
  "node_unavailable",
  "acquire_failed",
  "monitor_unavailable",
  "era_mismatch",
]);

/**
 * Maps a transport failure to the provider's typed errors: an outage, a
 * restart, a timeout or a transient refusal becomes
 * {@link L1ProviderTransientError}; any other refusal
 * {@link L1ProviderRequestError}. Anything else is returned unchanged.
 */
export const fromTransportError = (error: unknown): unknown => {
  if (error instanceof TransportUnavailableError)
    return new L1ProviderTransientError("transport", error.reason, {
      cause: error,
    });
  if (error instanceof TransportTimeoutError)
    return new L1ProviderTransientError("transport", "request_timeout", {
      cause: error,
    });
  if (error instanceof SidecarExitedError)
    return new L1ProviderTransientError(
      "transport",
      error.exit.fatal?.code ?? "sidecar_exited",
      { cause: error },
    );
  if (error instanceof TransportRequestError)
    return TRANSIENT_REQUEST_CODES.has(error.code)
      ? new L1ProviderTransientError("transport", error.code, { cause: error })
      : new L1ProviderRequestError(error.code, error.message, {
          cause: error,
        });
  return error;
};
