import { SqlError } from "@effect/sql";
import { Cause, Runtime } from "effect";

import { L1SourceUnavailable } from "../l1-source-unavailable.js";
import { HistoryRecoverySuperseded } from "./event-history-recovery.js";

/** Lost connections, refused or timed-out connects, server shutdown, and
 * contention a retried transaction resolves (statement timeout,
 * serialization, deadlock, lock wait). None of them is an authority answer:
 * the history authority's own refusals carry no SQL error. */
const TRANSIENT_DATABASE_CODES = new Set([
  "CONNECTION_CLOSED",
  "CONNECTION_DESTROYED",
  "CONNECTION_ENDED",
  "CONNECT_TIMEOUT",
  "ECONNREFUSED",
  "ECONNRESET",
  "ETIMEDOUT",
  "EPIPE",
  "EHOSTUNREACH",
  "ENETUNREACH",
  "EAI_AGAIN",
  "ENOTFOUND",
  "57P01",
  "57P02",
  "57P03",
  "53300",
  "57014",
  "40001",
  "40P01",
  "55P03",
]);
const MAXIMUM_CAUSE_DEPTH = 8;

const codeOf = (value: object): string | undefined => {
  const code = (value as { readonly code?: unknown }).code;
  return typeof code === "string" ? code : undefined;
};

/** Whether a SQL error anywhere in this cause chain is transport-class. */
const transientSql = (error: SqlError.SqlError): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < MAXIMUM_CAUSE_DEPTH; depth += 1) {
    if (typeof current !== "object" || current === null) return false;
    const code = codeOf(current);
    if (
      code !== undefined &&
      (code.startsWith("08") || TRANSIENT_DATABASE_CODES.has(code))
    )
      return true;
    current = (current as { readonly cause?: unknown }).cause;
  }
  return false;
};

/** Whether the history owner may reconnect after this failure instead of
 * stopping. Only failures that say nothing about the chain or the authority
 * qualify: an unavailable or lagging L1 source, a deadline, a transport-class
 * SQL error, or this process's own recovery fence moving. A reconnect always
 * re-validates the lease, the source binding and a retained intersection
 * before it admits anything, so none of these can admit different history.
 * Everything else is terminal: another owner or generation, an expired or
 * suspended lease, a missing intersection, a genesis mismatch, broken
 * ancestry, or a malformed answer. */
export const isRecoverableHistorySourceFailure = (cause: unknown): boolean => {
  let current = cause;
  for (let depth = 0; depth < MAXIMUM_CAUSE_DEPTH; depth += 1) {
    if (Runtime.isFiberFailure(current)) {
      current = Cause.squash(current[Runtime.FiberFailureCauseId]);
      continue;
    }
    if (
      current instanceof L1SourceUnavailable ||
      current instanceof HistoryRecoverySuperseded
    )
      return true;
    if (current instanceof SqlError.SqlError) return transientSql(current);
    if (typeof current !== "object" || current === null) return false;
    if ((current as { readonly name?: unknown }).name === "TimeoutError")
      return true;
    current = (current as { readonly cause?: unknown }).cause;
  }
  return false;
};
