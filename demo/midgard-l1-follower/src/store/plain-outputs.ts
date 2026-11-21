/**
 * Bounded reads of plain outputs at one address: live, ada only, no datum
 * hash, no reference script. A reader that looks up published data by its
 * inline datum bytes reads only the copies it can use, a few per datum,
 * however many other outputs sit at the address.
 */
import type { Dialect, SqlTx, SqlValue } from "../sql/backend.js";
import type { Cursor, OutRef, StoredOutput } from "../types.js";
import {
  MAX_PARAMETERS,
  readCursor,
  STORED_OUTPUT_FROM,
  STORED_OUTPUT_SELECT,
  storedOutputFromRow,
} from "./rows.js";

/** A read with the cursor of its snapshot, or the store has no cursor yet. */
export type PlainOutputsRead =
  | Readonly<{
      kind: "ok";
      cursor: Cursor;
      utxos: readonly StoredOutput[];
    }>
  | Readonly<{ kind: "not_initialized"; detail: string }>;

/** One inline datum looked up, and how many copies of it to return. */
export type PlainDatumRequest = Readonly<{ datum: Buffer; limit: number }>;

const PLAIN_LIVE = `o.address = ? AND o.spent_slot IS NULL AND o.datum_hash IS NULL
  AND o.script_ref_hash IS NULL AND NOT EXISTS (SELECT 1 FROM l1_output_assets a
  WHERE a.tx_hash = o.tx_hash AND a.output_index = o.output_index)`;

const ORDER = "ORDER BY o.tx_hash, o.output_index";

const excludedSql = (
  excluded: readonly OutRef[],
): { where: string; params: SqlValue[] } =>
  excluded.length === 0
    ? { where: "", params: [] }
    : {
        where: ` AND NOT (${excluded.map(() => "(o.tx_hash = ? AND o.output_index = ?)").join(" OR ")})`,
        params: excluded.flatMap((outRef) => [outRef.txHash, outRef.index]),
      };

const checkParameters = (params: readonly SqlValue[]): void => {
  if (params.length > MAX_PARAMETERS)
    throw new RangeError(
      `a plain-output read binds ${params.length.toString()} parameters, over the ${MAX_PARAMETERS.toString()} one statement allows`,
    );
};

const assertLimit = (limit: number): void => {
  if (!Number.isSafeInteger(limit) || limit < 1)
    throw new RangeError(`a read limit must be a positive integer: ${limit}`);
};

/** One request per datum, at the largest limit asked for it. */
const mergedRequests = (
  requests: readonly PlainDatumRequest[],
): PlainDatumRequest[] => {
  const merged = new Map<string, PlainDatumRequest>();
  for (const request of requests) {
    assertLimit(request.limit);
    const key = request.datum.toString("hex");
    const seen = merged.get(key);
    if (seen === undefined || seen.limit < request.limit)
      merged.set(key, request);
  }
  return [...merged.values()];
};

const notInitialized = {
  kind: "not_initialized",
  detail: "no cursor row",
} as const;

/**
 * For each requested datum, the first `limit` plain outputs live at the tip
 * at `address` whose inline datum is exactly those bytes, ordered by
 * out-ref and skipping `excluded`. One statement, one snapshot with its
 * cursor; the rows returned are at most the sum of the limits.
 */
export const livePlainOutputsByDatumIn = async (
  tx: SqlTx,
  dialect: Dialect,
  {
    address,
    requests,
    excluded = [],
  }: Readonly<{
    address: Buffer;
    requests: readonly PlainDatumRequest[];
    excluded?: readonly OutRef[];
  }>,
): Promise<PlainOutputsRead> => {
  const cursor = await readCursor(tx, dialect);
  if (cursor === null) return notInitialized;
  const wanted = mergedRequests(requests);
  if (wanted.length === 0) return { kind: "ok", cursor, utxos: [] };
  const exclusion = excludedSql(excluded);
  const limitCase = wanted
    .map(() => "WHEN ranked.datum = ? THEN CAST(? AS INTEGER)")
    .join(" ");
  const params: SqlValue[] = [
    address,
    ...wanted.map(({ datum }) => datum),
    ...exclusion.params,
    ...wanted.flatMap(({ datum, limit }) => [datum, limit]),
  ];
  checkParameters(params);
  const rows = await tx.query(
    `SELECT * FROM (SELECT ${STORED_OUTPUT_SELECT},
      ROW_NUMBER() OVER (PARTITION BY o.datum ${ORDER}) AS datum_rank
      FROM ${STORED_OUTPUT_FROM}
      WHERE ${PLAIN_LIVE} AND o.datum IN (${wanted.map(() => "?").join(", ")})${exclusion.where}) ranked
    WHERE ranked.datum_rank <= CASE ${limitCase} ELSE 0 END
    ORDER BY ranked.tx_hash, ranked.output_index`,
    params,
  );
  return {
    kind: "ok",
    cursor,
    utxos: rows.map((row) => storedOutputFromRow(dialect, row)),
  };
};

/**
 * The first `limit` plain outputs live at the tip at `address`, ordered by
 * out-ref, whose inline datum (if any) is none of `datums`, skipping
 * `excluded`. One statement, one snapshot with its cursor.
 */
export const livePlainOutputsExceptDatumsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  {
    address,
    datums,
    excluded = [],
    limit,
  }: Readonly<{
    address: Buffer;
    datums: readonly Buffer[];
    excluded?: readonly OutRef[];
    limit: number;
  }>,
): Promise<PlainOutputsRead> => {
  assertLimit(limit);
  const cursor = await readCursor(tx, dialect);
  if (cursor === null) return notInitialized;
  const distinct = [
    ...new Map(datums.map((datum) => [datum.toString("hex"), datum])).values(),
  ];
  const exclusion = excludedSql(excluded);
  const params: SqlValue[] = [address, ...distinct, ...exclusion.params, limit];
  checkParameters(params);
  const rows = await tx.query(
    `SELECT ${STORED_OUTPUT_SELECT} FROM ${STORED_OUTPUT_FROM}
    WHERE ${PLAIN_LIVE}${
      distinct.length === 0
        ? ""
        : ` AND (o.datum IS NULL OR o.datum NOT IN (${distinct.map(() => "?").join(", ")}))`
    }${exclusion.where} ${ORDER} LIMIT ?`,
    params,
  );
  return {
    kind: "ok",
    cursor,
    utxos: rows.map((row) => storedOutputFromRow(dialect, row)),
  };
};
