/**
 * The adapter seam. Shared follower SQL is written once with `?`
 * placeholders in the subset Postgres and SQLite both support; each backend
 * runs it, and the dialect encodes the few column types that differ (§18.1
 * Q1: booleans, JSON, outref arrays, the row-lock clause).
 */

export type SqlValue =
  | null
  | number
  | bigint
  | string
  | boolean
  | Buffer
  | Buffer[];

export type SqlRow = Record<string, unknown>;

/** A statement executor bound to one open transaction. */
export type SqlTx = {
  query(sql: string, params?: readonly SqlValue[]): Promise<SqlRow[]>;
  /** Runs one or more statements with no parameters and no result. */
  exec(sql: string): Promise<void>;
};

export type DialectName = "postgres" | "sqlite";

export type Dialect = Readonly<{
  name: DialectName;
  bool(value: boolean): SqlValue;
  readBool(value: unknown): boolean;
  /** A JSON document column (`jsonb` / `TEXT`). */
  json(text: string): SqlValue;
  /** An outref list column (`bytea[]` / JSON array of hex `TEXT`). */
  outRefList(encoded: readonly Buffer[]): SqlValue;
  readOutRefList(value: unknown): Buffer[];
  /** `FOR UPDATE` / `FOR SHARE` on Postgres; SQLite locks by `BEGIN IMMEDIATE`. */
  lockClause(mode: "update" | "share"): string;
  /** A per-row identity usable in `WHERE <id> IN (SELECT <id> … LIMIT n)`. */
  rowId: string;
}>;

export type TransactionMode =
  /** Serialised writer: `BEGIN` + row locks / `BEGIN IMMEDIATE`. */
  | "write"
  /** A consistent read-only snapshot. */
  | "read";

/**
 * The store's writer lease: one holder per store across processes and hosts.
 * Postgres holds a session advisory lock on a dedicated connection; SQLite
 * holds core's `SqliteProcessMutex` on a sidecar file. Either is released
 * when its holder closes it or its process (or session) dies.
 */
export type WriterLease = {
  /** True once the lease's session ended without `release()`. */
  lost(): boolean;
  /**
   * Why a lost lease cannot be had now, when its source can name it (an
   * instance lock's refusal); undefined when it cannot or the lease is held.
   */
  refusal?(): string | undefined;
  release(): Promise<void>;
};

export type TransactionOptions = Readonly<{
  /**
   * Ends the transaction when it fires: the call rejects with the signal's
   * reason, and a statement still waiting for its answer is given up with
   * its connection, so a server or network that never answers holds
   * nothing past the abort. For reads: an abort during a write's `COMMIT`
   * leaves its outcome unknown, as a dropped connection does. On Postgres
   * a read given no signal ends at the backend's `readDeadlineMs`.
   */
  signal?: AbortSignal;
  /**
   * Marks a write transaction that only reads: one Postgres must run
   * read-write because it takes `FOR SHARE` (a view check), which a
   * read-only transaction refuses. Given no signal, it ends at the read
   * deadline as a read does. A real write never does: one the server is
   * still applying is progress, and one cut short at its `COMMIT` has an
   * unknown outcome.
   */
  onlyReads?: boolean;
}>;

export type SqlBackend = {
  readonly dialect: Dialect;
  transaction<T>(
    mode: TransactionMode,
    run: (tx: SqlTx) => Promise<T>,
    options?: TransactionOptions,
  ): Promise<T>;
  /** Takes the writer lease, or returns null while another holder has it. */
  acquireWriterLease(): Promise<WriterLease | null>;
  /** Releases connections this backend opened (never a caller's pool). */
  close(): Promise<void>;
};

/** Thrown inside a transaction to roll it back and return `value`. */
export class RollbackWith<T> extends Error {
  constructor(readonly value: T) {
    super("transaction rolled back with a typed result");
    this.name = "RollbackWith";
  }
}

/** Rewrites `?` placeholders to `$1…$n` (no `?` may appear in literals). */
export const toNumberedPlaceholders = (sql: string): string => {
  let index = 0;
  return sql.replace(/\?/gu, () => {
    index += 1;
    return `$${index}`;
  });
};

export const asNumber = (value: unknown): number => {
  if (typeof value === "number") return value;
  if (typeof value === "bigint" || typeof value === "string") {
    const number = Number(value);
    if (!Number.isSafeInteger(number))
      throw new Error(`integer ${String(value)} is not a safe number`);
    return number;
  }
  throw new Error(`expected an integer column, got ${typeof value}`);
};

export const asNullableNumber = (value: unknown): number | null =>
  value === null || value === undefined ? null : asNumber(value);

export const asBuffer = (value: unknown): Buffer => {
  if (Buffer.isBuffer(value)) return value;
  if (value instanceof Uint8Array) return Buffer.from(value);
  throw new Error(`expected a bytes column, got ${typeof value}`);
};

export const asNullableBuffer = (value: unknown): Buffer | null =>
  value === null || value === undefined ? null : asBuffer(value);

export const asBigInt = (value: unknown): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" || typeof value === "string")
    return BigInt(value);
  throw new Error(`expected a numeric column, got ${typeof value}`);
};

export const asNullableBigInt = (value: unknown): bigint | null =>
  value === null || value === undefined ? null : asBigInt(value);

export const asString = (value: unknown): string => {
  if (typeof value === "string") return value;
  throw new Error(`expected a text column, got ${typeof value}`);
};
