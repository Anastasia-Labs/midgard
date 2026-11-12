/**
 * The one failure classification every role shares (owner ruling
 * 2026-10-09, plan §7.5): a process exits only when a restart could
 * plausibly fix the failure. `classifyRoleFailure` sorts a failure into a
 * `RoleFailureClass` by the codes and messages in one table; each role's
 * adapter maps the class to what it does, and differs only where the role
 * does (which peer a transient is bounded on, what it retries in place).
 * `@al-ft/midgard-l1-follower/testing` holds the conformance rows every
 * role's adapter is tested against.
 */

/**
 * - `transient`: a dependency down, restarting or unreachable. Retried; the
 *   role bounds it by peer: Postgres, a driver or a store at 15 minutes,
 *   after which the process exits; the cardano-node or its sidecar without
 *   bound, named on `/readyz`.
 * - `contention`: another transaction or lock in the way (a serialization
 *   failure, a deadlock, a lock not available, a statement cancelled).
 *   Retried in place; it is not a dependency outage.
 * - `restart`: a system error a restart may clear (a port in use). The
 *   process exits non-zero.
 * - `deterministic`: fails the same way on every retry (a constraint or
 *   data error, a permission). The process holds, unready.
 * - `unknown`: not recognised. The process holds, as on a deterministic
 *   failure.
 */
export type RoleFailureClass =
  | "transient"
  | "contention"
  | "restart"
  | "deterministic"
  | "unknown";

/** Socket- and resolver-level errnos of a peer that is down, restarting or
 * briefly unroutable. */
export const CONNECTION_ERRNOS = [
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
] as const;

export type ConnectionErrno = (typeof CONNECTION_ERRNOS)[number];

/**
 * The table. A key is an exact code: an errno, optionally with its
 * `syscall`; a SQLSTATE, or its two-character class; a node:sqlite primary
 * result code as `SQLITE:<n>`. The exact code wins over its SQLSTATE class.
 */
const ROLE_FAILURE_TABLE: ReadonlyMap<string, RoleFailureClass> = new Map<
  string,
  RoleFailureClass
>([
  ...CONNECTION_ERRNOS.map((errno) => [errno, "transient"] as const),
  // A Unix socket whose server has not created it yet.
  ["ENOENT connect", "transient"],
  ["EADDRINUSE", "restart"],
  ["EACCES", "deterministic"],
  ["EPERM", "deterministic"],
  ["EISDIR", "deterministic"],
  ["EINVAL", "deterministic"],
  // SQLSTATE classes: 08 connection exception; 53 insufficient resources;
  // 57 operator intervention (admin or crash shutdown, starting up).
  ["08", "transient"],
  ["53", "transient"],
  ["57", "transient"],
  // 40 transaction rollback (serialization, deadlock); 55 object not in
  // prerequisite state (lock not available); query_canceled.
  ["40", "contention"],
  ["55", "contention"],
  ["57014", "contention"],
  // 22 data exception, 23 integrity constraint violation, 42 syntax or
  // access rule.
  ["22", "deterministic"],
  ["23", "deterministic"],
  ["42", "deterministic"],
  // node:sqlite: BUSY, LOCKED; CONSTRAINT, MISMATCH.
  ["SQLITE:5", "contention"],
  ["SQLITE:6", "contention"],
  ["SQLITE:19", "deterministic"],
  ["SQLITE:20", "deterministic"],
]);

/** Failures that carry their class only in their name and message. */
const MESSAGE_TABLE: readonly Readonly<{
  name?: string;
  message: RegExp;
  class: RoleFailureClass;
}>[] = [
  // pg's own connection failures carry no SQLSTATE; `timeout expired` is
  // pg.Client's connect bound (`connectionTimeoutMillis`).
  {
    message:
      /connection terminated|connection error|not queryable|timeout exceeded when trying to connect|^timeout expired$/iu,
    class: "transient",
  },
  // libp2p 3.3.4 reports a listen address in use as an
  // UnsupportedListenAddressesError with no `code` and no `cause`: the
  // EADDRINUSE is only in its message.
  {
    name: "UnsupportedListenAddressesError",
    message: /\bEADDRINUSE\b/u,
    class: "restart",
  },
];

/** The class one error says by itself, ignoring what it wraps. */
const classOfLink = (link: object): RoleFailureClass | undefined => {
  const { code, errcode, syscall, name, message } = link as {
    code?: unknown;
    errcode?: unknown;
    syscall?: unknown;
    name?: unknown;
    message?: unknown;
  };
  if (code === "ERR_SQLITE_ERROR" && typeof errcode === "number")
    return ROLE_FAILURE_TABLE.get(`SQLITE:${(errcode & 0xff).toString()}`);
  if (typeof code === "string") {
    const exact =
      (typeof syscall === "string"
        ? ROLE_FAILURE_TABLE.get(`${code} ${syscall}`)
        : undefined) ?? ROLE_FAILURE_TABLE.get(code);
    if (exact !== undefined) return exact;
    if (/^[0-9A-Z]{5}$/u.test(code))
      return ROLE_FAILURE_TABLE.get(code.slice(0, 2));
  }
  if (typeof message !== "string") return undefined;
  return MESSAGE_TABLE.find(
    (rule) =>
      (rule.name === undefined || rule.name === name) &&
      rule.message.test(message),
  )?.class;
};

/** When links disagree, the first class here that any link has wins. */
const PRECEDENCE: readonly RoleFailureClass[] = [
  "restart",
  "transient",
  "contention",
  "deterministic",
];

/**
 * The class of `error`, read from it and everything it wraps (`cause`, an
 * `AggregateError`'s `errors`, and whatever `unwrap` returns for a wrapper
 * the role knows, such as an Effect `Cause`): a port in use anywhere exits,
 * a dropped connection anywhere is transient, and a failure nothing
 * recognises is `unknown`.
 */
export const classifyRoleFailure = (
  error: unknown,
  unwrap: (link: object) => readonly unknown[] = () => [],
): RoleFailureClass => {
  const found = new Set<RoleFailureClass>();
  const seen = new Set<unknown>();
  const pending: unknown[] = [error];
  while (pending.length > 0 && seen.size < 64) {
    const link = pending.shift();
    if (typeof link !== "object" || link === null || seen.has(link)) continue;
    seen.add(link);
    const linkClass = classOfLink(link);
    if (linkClass !== undefined) found.add(linkClass);
    const { cause, errors } = link as { cause?: unknown; errors?: unknown };
    pending.push(cause, ...unwrap(link));
    if (Array.isArray(errors)) pending.push(...(errors as unknown[]));
  }
  return PRECEDENCE.find((candidate) => found.has(candidate)) ?? "unknown";
};
