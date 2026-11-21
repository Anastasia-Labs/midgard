import type { RoleFailureClass } from "../role-failure.js";

/**
 * One conformance row of the shared failure classification
 * (`classifyRoleFailure`): an error as a dependency throws it, and the class
 * the owner ruling of 2026-10-09 gives it. Every role's adapter test runs
 * these rows and asserts that the adapter does what its class says.
 */
export type RoleFailureRow = Readonly<{
  name: string;
  error: () => unknown;
  expected: RoleFailureClass;
}>;

const coded = (message: string, code: string, syscall?: string): Error =>
  Object.assign(
    new Error(message),
    syscall === undefined ? { code } : { code, syscall },
  );

/** node:sqlite's error: the primary result code is the low byte. */
const sqlite = (message: string, errcode: number): Error =>
  Object.assign(new Error(message), { code: "ERR_SQLITE_ERROR", errcode });

const row = (
  name: string,
  expected: RoleFailureClass,
  error: () => unknown,
): RoleFailureRow => ({ name, error, expected });

/** One row per SQLSTATE, named by what it means. */
const sqlstateRows = (
  expected: RoleFailureClass,
  names: Readonly<Record<string, string>>,
): RoleFailureRow[] =>
  Object.entries(names).map(([sqlstate, name]) =>
    row(`${name} (${sqlstate})`, expected, () => coded(name, sqlstate)),
  );

export const ROLE_FAILURE_ROWS: readonly RoleFailureRow[] = [
  ...[
    "ECONNREFUSED",
    "ECONNRESET",
    "ECONNABORTED",
    "ETIMEDOUT",
    "EPIPE",
    "ENOTFOUND",
    "EAI_AGAIN",
    "EHOSTUNREACH",
    "ENETUNREACH",
    "ENETDOWN",
    "EHOSTDOWN",
  ].map((errno) =>
    row(`a socket failing ${errno}`, "transient", () =>
      coded(`connect ${errno}`, errno, "connect"),
    ),
  ),
  row("a fetch failure around a refused connection", "transient", () =>
    Object.assign(new TypeError("fetch failed"), {
      cause: coded("connect ECONNREFUSED", "ECONNREFUSED", "connect"),
    }),
  ),
  row(
    "an AggregateError holding a reset socket",
    "transient",
    () =>
      new AggregateError(
        [new Error("cleanup failed"), coded("read ECONNRESET", "ECONNRESET")],
        "startup",
      ),
  ),
  row("a Unix socket not created yet", "transient", () =>
    coded("connect ENOENT /run/node.socket", "ENOENT", "connect"),
  ),
  ...sqlstateRows("transient", {
    "08000": "a connection exception",
    "08001": "a connection not established",
    "08003": "a connection that does not exist",
    "08006": "a connection failure",
    "57P01": "an admin shutdown",
    "57P02": "a crash shutdown",
    "57P03": "a server that cannot connect now",
    "53300": "too many connections",
    "53100": "a disk full",
    "53200": "an out of memory",
  }),
  row(
    "pg's own dropped connection",
    "transient",
    () => new Error("Connection terminated unexpectedly"),
  ),
  row(
    "pg.Client's connect bound",
    "transient",
    () => new Error("timeout expired"),
  ),
  ...sqlstateRows("contention", {
    "40001": "a serialization failure",
    "40P01": "a deadlock",
    "55P03": "a lock not available",
    "57014": "a statement cancelled",
  }),
  row("SQLite busy", "contention", () => sqlite("database is locked", 5)),
  row("SQLite busy on a snapshot (extended code)", "contention", () =>
    sqlite("database is locked", 517),
  ),
  row("SQLite locked", "contention", () =>
    sqlite("database table is locked", 6),
  ),
  row("a port in use", "restart", () =>
    coded("listen EADDRINUSE: address already in use", "EADDRINUSE", "listen"),
  ),
  row("a port in use behind a dropped connection", "restart", () =>
    Object.assign(new Error("start failed"), {
      cause: Object.assign(coded("connect ECONNREFUSED", "ECONNREFUSED"), {
        cause: coded("listen EADDRINUSE", "EADDRINUSE", "listen"),
      }),
    }),
  ),
  row("libp2p's listen address in use (message only)", "restart", () =>
    Object.assign(
      new Error(
        "Some configured addresses failed to be listened on, you may need to remove one or more listen addresses from your configuration or set `transportManager.faultTolerance` to NO_FATAL:\n\n  /ip4/127.0.0.1/tcp/4001: listen EADDRINUSE: address already in use 127.0.0.1:4001\n",
      ),
      { name: "UnsupportedListenAddressesError" },
    ),
  ),
  row("libp2p's listen address refused for another reason", "unknown", () =>
    Object.assign(
      new Error(
        "Some configured addresses failed to be listened on:\n\n  /ip4/10.9.9.9/tcp/4001: listen EADDRNOTAVAIL: address not available 10.9.9.9:4001\n",
      ),
      { name: "UnsupportedListenAddressesError" },
    ),
  ),
  row(
    "EADDRINUSE in the message of an unrelated error",
    "unknown",
    () => new Error("peer reported EADDRINUSE on its side"),
  ),
  ...["EACCES", "EPERM", "EISDIR", "EINVAL"].map((errno) =>
    row(`a file failing ${errno}`, "deterministic", () =>
      coded(`open ${errno}`, errno, "open"),
    ),
  ),
  ...sqlstateRows("deterministic", {
    "23505": "a unique violation",
    "22P02": "an invalid text representation",
    "42P01": "an undefined table",
    "42501": "an insufficient privilege",
  }),
  row("a SQLite constraint (extended code)", "deterministic", () =>
    sqlite("UNIQUE constraint failed", 2067),
  ),
  row("a SQLite datatype mismatch", "deterministic", () =>
    sqlite("datatype mismatch", 20),
  ),
  row("a file not written yet (ENOENT)", "unknown", () =>
    coded("open ENOENT", "ENOENT", "open"),
  ),
  row("an I/O error (EIO)", "unknown", () => coded("read EIO", "EIO", "read")),
  row("a failed password (28P01)", "unknown", () =>
    coded("password authentication failed", "28P01"),
  ),
  row("a missing database (3D000)", "unknown", () =>
    coded("database does not exist", "3D000"),
  ),
  row("an unrecognised error", "unknown", () => new Error("boom")),
  row(
    "a message that only mentions a timeout",
    "unknown",
    () => new Error("statement timeout expired soon"),
  ),
  row("a non-Error value", "unknown", () => "timeout expired"),
];
