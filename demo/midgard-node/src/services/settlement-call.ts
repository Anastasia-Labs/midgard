import { Cause, Data, Effect } from "effect";

/** How far a report follows an error's `cause` links. */
const MAX_CAUSE_DEPTH = 8;
/** Effect's placeholder text for a rejection it wrapped without a catch. */
const EFFECT_WRAPPER_MESSAGE =
  /^An unknown error occurred(?: in Effect\.\w+)?$/u;

const errorText = (error: unknown): string | undefined => {
  if (typeof error === "string") return error;
  if (error instanceof Error) {
    const message = error.message.trim();
    if (EFFECT_WRAPPER_MESSAGE.test(message)) return undefined;
    return message === "" ? error.name : message;
  }
  return undefined;
};

/** An error and its cause chain on one line, outermost first, each link
 * that adds text of its own. A provider's JSON-RPC error, wrapped by a
 * promise rejection and then by an operation label, reads as
 * "submit settlement transaction: Ogmios JSON-RPC error 3005: ...". */
export const describeSettlementError = (error: unknown): string => {
  const parts: string[] = [];
  const seen = new Set<unknown>();
  let current: unknown = error;
  for (
    let depth = 0;
    depth < MAX_CAUSE_DEPTH && current != null && !seen.has(current);
    depth++
  ) {
    seen.add(current);
    const text = errorText(current);
    if (text !== undefined && !parts.some((part) => part.includes(text)))
      parts.push(text);
    if (typeof current !== "object") break;
    const record = current as { cause?: unknown; error?: unknown };
    current = record.cause ?? record.error;
  }
  if (parts.length > 0) return parts.join(": ");
  try {
    return JSON.stringify(error)?.slice(0, 300) ?? String(error);
  } catch {
    return String(error);
  }
};

/** A provider, signer, indexer or journal-inspection call on the settlement
 * path failed. The message names the operation and carries the underlying
 * error chain, so a health report says which call failed and what the
 * provider answered instead of Effect's "An unknown error occurred". */
export class SettlementCallError extends Data.TaggedError(
  "SettlementCallError",
)<{
  readonly operation: string;
  readonly message: string;
  readonly cause: unknown;
}> {}

const callError = (operation: string, cause: unknown) =>
  new SettlementCallError({
    operation,
    message: `${operation}: ${describeSettlementError(cause)}`,
    cause,
  });

export const settlementCall = <A>(
  operation: string,
  call: () => PromiseLike<A>,
): Effect.Effect<A, SettlementCallError> =>
  Effect.tryPromise({
    try: () => call(),
    catch: (cause) => callError(operation, cause),
  });

export const settlementCheck = <A>(
  operation: string,
  check: () => A,
): Effect.Effect<A, SettlementCallError> =>
  Effect.try({
    try: check,
    catch: (cause) => callError(operation, cause),
  });

/** A settlement job's step failed; names the job so a report, and the
 * job's `last_error`, can be traced to the event it settles. */
export class SettlementJobError extends Data.TaggedError("SettlementJobError")<{
  readonly message: string;
  readonly cause: unknown;
}> {}

export const settlementJobError = (
  job: { readonly kind: string; readonly event_id: string },
  step: string,
  cause: unknown,
) =>
  new SettlementJobError({
    message: `settlement ${job.kind} ${job.event_id} ${step}: ${describeSettlementError(cause)}`,
    cause,
  });

/** A health report's detail for a failed tick or worker program: every
 * failure and defect with its cause chain, one per line. Effect's own
 * rendering prints only the outermost error and its stack, which for a
 * wrapped promise rejection is "UnknownException" with internal frames. */
export const settlementCauseDetail = (cause: Cause.Cause<unknown>): string => {
  const errors = [...Cause.failures(cause), ...Cause.defects(cause)];
  if (errors.length === 0)
    return Cause.isInterruptedOnly(cause)
      ? "settlement interrupted"
      : Cause.pretty(cause);
  return errors.map(describeSettlementError).join("\n").slice(0, 2000);
};

/** Ogmios refuses to resubmit a body whose inputs its ledger and mempool
 * view has already consumed: the node's mempool check ("All inputs are
 * spent. Transaction has probably already been included", JSON-RPC 3997) or
 * the ledger's unknown-input failure (3117). For the exact journaled body
 * that is the normal answer while it waits in the mempool or for the
 * indexer; the next reconcile reads its status. */
export const isSettlementInputsSpentRejection = (error: unknown): boolean =>
  /All inputs are spent|JSON-RPC error 3117\b|unknownOutputReferences|BadInputsUTxO/u.test(
    describeSettlementError(error),
  );
