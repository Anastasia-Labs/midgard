/** node:sqlite primary result codes: SQLITE_BUSY, SQLITE_LOCKED. */
const BUSY_PRIMARY_CODES = new Set([5, 6]);
const MAX_CAUSE_DEPTH = 8;

/**
 * Whether `error`, or an error it wraps through `cause`, is a node:sqlite
 * SQLITE_BUSY or SQLITE_LOCKED (any extended code). Another connection held
 * the database past the busy timeout. The statement that met it committed
 * nothing, and running it again later can succeed. Whoever drives the work
 * holds it and requeues it; this never fails the process.
 */
export const isSqliteBusyError = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < MAX_CAUSE_DEPTH; depth += 1) {
    if (!(current instanceof Error)) return false;
    const { code, errcode } = current as { code?: unknown; errcode?: unknown };
    if (
      code === "ERR_SQLITE_ERROR" &&
      typeof errcode === "number" &&
      BUSY_PRIMARY_CODES.has(errcode & 0xff)
    )
      return true;
    current = current.cause;
  }
  return false;
};
