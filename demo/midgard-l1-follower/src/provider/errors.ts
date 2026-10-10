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
  override readonly name: string = "L1ProviderTransientError";
  constructor(
    readonly source: TransientSource,
    readonly reason: string,
    options?: { cause?: unknown },
  ) {
    super(`L1 provider ${source} unavailable: ${reason}`, options);
  }
}

/**
 * A submission whose outcome is unknown: the transport stopped waiting for
 * the node's answer, or the sidecar exited, while it was in flight. The node
 * may have taken the transaction. It is an {@link L1ProviderTransientError}
 * (`retryable`), so a caller that resends the same bytes behaves as before;
 * a caller that would build a replacement looks for `txHash` first (in the
 * mempool or the facts) instead of resubmitting blind. `reason` is the
 * transient reason the submission failed with.
 */
export class L1SubmitOutcomeUnknownError extends L1ProviderTransientError {
  override readonly name: string = "L1SubmitOutcomeUnknownError";
  readonly retryable = true;
  readonly outcomeUnknown = true;
  constructor(
    readonly txHash: string | null,
    reason: string,
    options?: { cause?: unknown },
  ) {
    super("transport", reason, options);
    this.message = `submission ${txHash === null ? "" : `of transaction ${txHash} `}has an unknown outcome (${reason}): the node may have taken it; look for it by id before building a replacement`;
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

/**
 * The query cannot be answered from the node's ledger alone (the
 * `LedgerProvider`, no store): local state query finds outputs by address
 * or outref, never by payment credential or unit, and holds no datum
 * preimages.
 */
export class L1LedgerScopeError extends L1ProviderError {
  override readonly name = "L1LedgerScopeError";
  constructor(
    readonly query: string,
    readonly detail: string,
  ) {
    super(`${query} cannot be answered from the node's ledger: ${detail}`);
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
    /** Where the landing was looked for. */
    observedIn = "the L1 facts",
  ) {
    super(
      `transaction ${txHash} did not land in ${observedIn} within ${timeoutMs} ms (last seen: ${lastSeen})`,
    );
  }
}

/**
 * The node's ledger cannot say whether a transaction landed: no output of it
 * is unspent at the tip and the node's mempool does not hold it. One that
 * never landed and one whose every output is already spent (or, for one the
 * provider did not submit, one with more outputs than it probes) read the
 * same, so the answer is unknown, never "not found". A chain index
 * (`--l1 kupmios`) reads the transaction itself.
 */
export class L1TxStatusUnknownError extends L1ProviderError {
  override readonly name = "L1TxStatusUnknownError";
  readonly reason = "ledger_tx_status_unknown";
  constructor(readonly txHash: string) {
    super(
      `the node's ledger cannot tell whether transaction ${txHash} landed: no output of it is unspent at the tip and the mempool does not hold it (it never landed, or every output is already spent); read its status from a chain index (--l1 kupmios)`,
    );
  }
}

/** Phase-two evaluation is local only: complete with `localUPLCEval: true`. */
export class L1LocalEvaluationOnlyError extends L1ProviderError {
  override readonly name = "L1LocalEvaluationOnlyError";
  constructor() {
    super(
      "the node-transport provider never evaluates scripts remotely; complete the transaction with localUPLCEval: true",
    );
  }
}

/**
 * Sidecar refusals that clear on their own: retried like an outage. An
 * `era_mismatch` is one: the sidecar re-reads the node's era for every
 * query, so it clears once the era boundary has passed.
 */
const TRANSIENT_REQUEST_CODES = new Set([
  "busy",
  "node_unavailable",
  "acquire_failed",
  "monitor_unavailable",
  "era_mismatch",
]);

/**
 * The node did not answer within the sidecar's bound. A query repeats
 * safely; a submission's outcome is unknown (the node may have taken it),
 * so a submit timeout stays a request error its caller decides on.
 */
const QUERY_TRANSIENT_REQUEST_CODES = new Set(["node_timeout"]);

/**
 * Maps a transport failure to the provider's typed errors: an outage, a
 * restart, a timeout or a transient refusal becomes
 * {@link L1ProviderTransientError}; any other refusal
 * {@link L1ProviderRequestError}. Anything else is returned unchanged.
 * `operation` is the request that failed: a node timeout is transient for a
 * query only, and a submission the transport stopped waiting for, or whose
 * sidecar exited, is {@link L1SubmitOutcomeUnknownError} for `txHash`.
 */
export const fromTransportError = (
  error: unknown,
  operation: "query" | "submit" = "query",
  txHash: string | null = null,
): unknown => {
  if (error instanceof TransportUnavailableError)
    return new L1ProviderTransientError("transport", error.reason, {
      cause: error,
    });
  if (error instanceof TransportTimeoutError)
    return operation === "submit"
      ? new L1SubmitOutcomeUnknownError(txHash, "request_timeout", {
          cause: error,
        })
      : new L1ProviderTransientError("transport", "request_timeout", {
          cause: error,
        });
  if (error instanceof SidecarExitedError) {
    const reason = error.exit.fatal?.code ?? "sidecar_exited";
    return operation === "submit"
      ? new L1SubmitOutcomeUnknownError(txHash, reason, { cause: error })
      : new L1ProviderTransientError("transport", reason, { cause: error });
  }
  if (error instanceof TransportRequestError)
    return TRANSIENT_REQUEST_CODES.has(error.code) ||
      (operation === "query" && QUERY_TRANSIENT_REQUEST_CODES.has(error.code))
      ? new L1ProviderTransientError("transport", error.code, { cause: error })
      : new L1ProviderRequestError(error.code, error.message, {
          cause: error,
        });
  return error;
};
