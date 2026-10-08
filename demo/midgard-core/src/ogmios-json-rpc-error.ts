/**
 * The one classification of an Ogmios JSON-RPC error answer, shared by every
 * Ogmios WebSocket client (midgard-node's ChainSync and ledger-state session,
 * da-committee-node's chain-sync session and its state-queue replay).
 *
 * An Ogmios answer is either a refusal of the request itself (malformed,
 * unknown method, no intersection, a misconfigured node) or a statement that
 * the node cannot answer it right now (its ledger is still in an earlier era
 * while it syncs, it is crossing an era, the acquired state left its volatile
 * window, the server failed to answer a well-formed request). Only the second
 * kind clears without a human, so only it is transient. A client turns a
 * transient answer into the same error it raises for a lost socket, so the
 * owning operation's bounded retry treats both alike; whether a cardano-node
 * restart reaches a client as a closed socket or as an error answer then no
 * longer decides whether the client recovers.
 *
 * Grounded in Ogmios v7.0.0 (the pinned image), CardanoSolutions/ogmios tag
 * v7.0.0:
 * - docs/static/ogmios.json: the error schemas (`AcquireLedgerStateFailure`,
 *   `QueryLedgerStateEraMismatch`, `QueryLedgerStateUnavailableInCurrentEra`,
 *   `QueryLedgerStateAcquiredExpired`, `QueryNetworkInvalidGenesis`,
 *   `FindIntersectionResponse`, `MustAcquireMempoolFirst`, `RpcError`);
 * - server/src/Ogmios/Data/Protocol/StateQuery.hs and ChainSync.hs: where each
 *   custom code is raised;
 * - server/src/Ogmios/App/Protocol/StateQuery.hs: 2002 is raised when the
 *   node's current era is not a Shelley-based one (`withCurrentEra`), and 2003
 *   also answers an unacquired query whose volatile-tip acquisition failed;
 * - server/src/Ogmios/App/Protocol.hs: -32603 is the catch-all for an
 *   exception while the server handled a well-formed request;
 * - server/src/Ogmios/App/Server/WebSocket.hs: a lost or failed node
 *   connection closes the socket with a -32000 reason;
 * - server/src/Ogmios/App/Server/Http.hs: the HTTP endpoint answers every
 *   JSON-RPC error with status 400;
 * - server/modules/json-rpc/src/Codec/Json/Rpc/Handler.hs: the JSON-RPC 2.0
 *   protocol codes -32700, -32600, -32601, -32602 and -32603.
 */

/** The fields of a JSON-RPC `error` member, as far as they could be read. */
export type OgmiosJsonRpcErrorAnswer = Readonly<{
  /** Undefined when the answer carries no integer code. */
  code: number | undefined;
  message: string | undefined;
  data: unknown;
}>;

/**
 * Codes that say the node cannot answer now, never that the request or the
 * chain is wrong. Every other code, an unknown one or a missing one included,
 * is a refusal.
 */
export const OGMIOS_TRANSIENT_JSON_RPC_ERROR_CODES: ReadonlyMap<
  number,
  string
> = new Map([
  // Unable to acquire the ledger state at the requested point: it left the
  // node's volatile window, or the node's selected chain does not (yet) hold
  // it. A caller pinned to the point waits for another point of its own.
  [2000, "acquire_failed"],
  // "An era mismatch between a client request and the era the ledger is in.
  // This may occur when running queries on a syncing node and/or when the
  // node is crossing an era." (ogmios.json)
  [2001, "era_mismatch"],
  // The node's current era is not a Shelley-based one: every query these
  // clients send exists in all Shelley-based eras, so this answers only while
  // a syncing node's ledger is still in Byron.
  [2002, "unavailable_in_current_era"],
  // "Previously acquired ledger state is no longer available", or the
  // volatile tip an unacquired query acquires could not be acquired.
  [2003, "acquired_state_expired"],
  // Ogmios's own code for "Connection with the node lost or failed."
  [-32000, "node_connection_lost"],
  // The server could not answer a well-formed request (RpcError: "when the
  // server was unable to reply to a well-formed request").
  [-32603, "internal_error"],
]);

const integerCode = (value: unknown): number | undefined => {
  if (typeof value === "number" && Number.isSafeInteger(value)) return value;
  if (
    typeof value === "bigint" &&
    value >= BigInt(Number.MIN_SAFE_INTEGER) &&
    value <= BigInt(Number.MAX_SAFE_INTEGER)
  )
    return Number(value);
  return undefined;
};

/** Reads a JSON-RPC `error` member of any shape without throwing. */
export const decodeOgmiosJsonRpcError = (
  error: unknown,
): OgmiosJsonRpcErrorAnswer => {
  const record =
    typeof error === "object" && error !== null && !Array.isArray(error)
      ? (error as Record<string, unknown>)
      : undefined;
  return Object.freeze({
    code: integerCode(record?.code),
    message: typeof record?.message === "string" ? record.message : undefined,
    data: record?.data,
  });
};

/** Whether an answer with this code clears without a human. */
export const isTransientOgmiosJsonRpcErrorCode = (
  code: number | undefined,
): boolean =>
  code !== undefined && OGMIOS_TRANSIENT_JSON_RPC_ERROR_CODES.has(code);

/** The `error` member as JSON text, losslessly for big integers. */
export const formatOgmiosJsonRpcError = (error: unknown): string =>
  JSON.stringify(error, (_key, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value,
  ) ?? String(error);

/**
 * An Ogmios JSON-RPC error answer, with its code and whether it is
 * transient. A client whose transient failures need a type of their own
 * (midgard-node's `L1SourceUnavailable`) defines its own transient class and
 * keeps this one for refusals.
 */
export class OgmiosJsonRpcError extends Error {
  readonly answer: OgmiosJsonRpcErrorAnswer;
  readonly transient: boolean;

  constructor(message: string, error: unknown, options?: ErrorOptions) {
    super(message, options);
    this.name = "OgmiosJsonRpcError";
    this.answer = decodeOgmiosJsonRpcError(error);
    this.transient = isTransientOgmiosJsonRpcErrorCode(this.answer.code);
  }
}

const recordOf = (value: unknown): Record<string, unknown> | undefined =>
  typeof value === "object" && value !== null
    ? (value as Record<string, unknown>)
    : undefined;

/**
 * The Ogmios code an error object carries, read by shape so that it works
 * across package copies: this module's `OgmiosJsonRpcError` (its `answer`),
 * and the Lucid Kupmios provider's `OgmiosJsonRpcError` (`kind: "json_rpc"`
 * with a numeric `code`). Any other value has none.
 */
export const ogmiosJsonRpcErrorCodeOf = (
  value: unknown,
): number | undefined => {
  const record = recordOf(value);
  if (record === undefined || record.name !== "OgmiosJsonRpcError") {
    return undefined;
  }
  const answer = recordOf(record.answer);
  if (answer !== undefined) return integerCode(answer.code);
  return record.kind === "json_rpc" ? integerCode(record.code) : undefined;
};

/**
 * Whether an error object is an Ogmios error answer whose code says the node
 * cannot answer now. The Kupmios provider's own `retryable` flag on such an
 * answer reflects only the HTTP status, and Ogmios's HTTP endpoint answers
 * every JSON-RPC error with HTTP 400 (`postRootR` in
 * server/src/Ogmios/App/Server/Http.hs), so that flag is false for every one
 * of them; only the code tells a syncing or era-crossing node apart from a
 * refused request.
 */
export const isTransientOgmiosJsonRpcFailure = (value: unknown): boolean =>
  isTransientOgmiosJsonRpcErrorCode(ogmiosJsonRpcErrorCodeOf(value));

const SPENT_INPUT_REJECTION =
  /BadInputsUTxO|UnknownInput|unknownOutputReferences|JSON-RPC error 3117\b|"code":\s*3117\b|does not exist or was already spent/u;

/**
 * Whether the ledger refused a submission because an input is spent or
 * unknown: Ogmios's JSON-RPC 3117 (its outrefs under
 * `data.unknownOutputReferences`), the ledger's `BadInputsUTxO` text as
 * Blockfrost and the submit API carry it, and the Lucid emulator's "does not
 * exist or was already spent". It is searched through the error's message,
 * its cause chain and its structured fields, because a provider wrapper can
 * carry the answer only as formatted text (an `Error.cause` is not
 * enumerable). Ogmios 3997 ("All inputs are spent", a mempool view of the
 * submitter's own transaction) is not this refusal and does not match.
 */
export const isSpentInputSubmitRejection = (error: unknown): boolean => {
  const seen = new Set<unknown>();
  const search = (value: unknown): boolean => {
    if (typeof value === "string") return SPENT_INPUT_REJECTION.test(value);
    if (typeof value !== "object" || value === null || seen.has(value))
      return false;
    seen.add(value);
    if (
      value instanceof Error &&
      (search(value.message) || search(value.cause))
    )
      return true;
    const record = value as Record<string, unknown>;
    if ("unknownOutputReferences" in record || "badInputs" in record)
      return true;
    return Object.values(record).some(search);
  };
  return search(error);
};
