import { PublicHttpReadLimitError } from "../database/http-read-byte-budget.js";
import { HTTP_RESPONSE_MAX_BYTES } from "./listen-http-ingress.js";

export type ReadPage = {
  readonly limit: number;
  readonly cursor: Buffer | undefined;
};
export const parseReadPage = (
  params: Readonly<Record<string, unknown>>,
  cursorBytes = 32,
): ReadPage => {
  const rawLimit = params.limit ?? "25";
  const rawCursor = params.cursor;
  if (
    typeof rawLimit !== "string" ||
    !/^[1-9][0-9]{0,2}$/u.test(rawLimit) ||
    Number(rawLimit) > 100
  )
    throw new Error("limit must be an integer from 1 to 100");
  if (
    rawCursor !== undefined &&
    (typeof rawCursor !== "string" ||
      rawCursor.length !== cursorBytes * 2 ||
      !/^(?:[0-9a-f]{2}){1,80}$/u.test(rawCursor))
  )
    throw new Error("cursor must be a bounded lowercase hex key");
  return {
    limit: Number(rawLimit),
    cursor:
      rawCursor === undefined
        ? undefined
        : Buffer.from(rawCursor as string, "hex"),
  };
};

/** Include only complete rows; never advance a cursor past a row omitted for bytes. */
export const encodeReadPage = <A>(
  rows: readonly A[],
  page: ReadPage,
  key: (row: A) => Buffer,
  encode: (row: A) => unknown,
) => {
  const entries: unknown[] = [];
  let bytes = 256;
  let last: Buffer | undefined;
  for (const row of rows.slice(0, page.limit)) {
    let entry: unknown;
    try {
      entry = encode(row);
    } catch (error) {
      if (!(error instanceof PublicHttpReadLimitError) || entries.length === 0)
        throw error;
      break;
    }
    const size = Buffer.byteLength(JSON.stringify(entry)) + 1;
    if (bytes + size > HTTP_RESPONSE_MAX_BYTES) break;
    entries.push(entry);
    bytes += size;
    last = key(row);
  }
  if (entries.length === 0 && rows.length > 0)
    throw new PublicHttpReadLimitError({ message: "E_HTTP_READ_DATA_LIMIT" });
  const hasMore = entries.length < rows.length;
  return { entries, nextCursor: hasMore ? last!.toString("hex") : null };
};
