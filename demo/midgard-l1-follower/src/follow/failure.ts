/**
 * How the follow loop treats a failed store write (plan §7.5): `transient`
 * recovers by itself (a dropped or refused connection, a lock, a
 * serialization conflict) and never escalates; `deterministic` fails the
 * same way on every retry (a constraint or data error, a statement the
 * database refuses) and escalates at once; `unknown` escalates after N
 * consecutive failures on the same point.
 */
export type FailureClass = "transient" | "deterministic" | "unknown";

/**
 * Postgres SQLSTATE classes: 08 connection exception, 40 transaction
 * rollback (serialization, deadlock), 53 insufficient resources, 55 object
 * not in prerequisite state (lock not available), 57 operator intervention
 * (admin or crash shutdown, `pg_terminate_backend`).
 */
const TRANSIENT_SQLSTATE = new Set(["08", "40", "53", "55", "57"]);
/** 22 data exception, 23 integrity constraint violation, 42 syntax or access rule. */
const DETERMINISTIC_SQLSTATE = new Set(["22", "23", "42"]);

const TRANSIENT_ERRNO = new Set([
  "ECONNREFUSED",
  "ECONNRESET",
  "ECONNABORTED",
  "ETIMEDOUT",
  "EPIPE",
  "ENOTFOUND",
  "EAI_AGAIN",
  "EHOSTUNREACH",
  "ENETUNREACH",
]);

/** node:sqlite primary result codes: BUSY, LOCKED. */
const TRANSIENT_SQLITE = new Set([5, 6]);
/** node:sqlite primary result codes: CONSTRAINT, MISMATCH. */
const DETERMINISTIC_SQLITE = new Set([19, 20]);

/** pg's own connection failures carry no SQLSTATE. */
const TRANSIENT_MESSAGE =
  /connection terminated|connection error|not queryable|timeout exceeded when trying to connect/iu;

export const classifyFailure = (error: unknown): FailureClass => {
  if (!(error instanceof Error)) return "unknown";
  const { code, errcode } = error as { code?: unknown; errcode?: unknown };
  if (code === "ERR_SQLITE_ERROR" && typeof errcode === "number") {
    const primary = errcode & 0xff;
    if (TRANSIENT_SQLITE.has(primary)) return "transient";
    if (DETERMINISTIC_SQLITE.has(primary)) return "deterministic";
    return "unknown";
  }
  if (typeof code === "string") {
    if (TRANSIENT_ERRNO.has(code)) return "transient";
    if (/^[0-9A-Z]{5}$/u.test(code)) {
      const sqlClass = code.slice(0, 2);
      if (TRANSIENT_SQLSTATE.has(sqlClass)) return "transient";
      if (DETERMINISTIC_SQLSTATE.has(sqlClass)) return "deterministic";
      return "unknown";
    }
  }
  return TRANSIENT_MESSAGE.test(error.message) ? "transient" : "unknown";
};
