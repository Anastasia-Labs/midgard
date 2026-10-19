/** A key over a row's primary key and every column its content digest does
 * not cover. Typed values keep text and integer columns apart. */
export const verifiedRowKey = (...columns: readonly unknown[]): string =>
  JSON.stringify(columns);

/**
 * Rows verified once, by the key above. A row's parse depends only on its own
 * columns and the digest covers the content, so a key seen again is the same
 * verified row: re-parsing it would repeat sha256 and canonical JSON over the
 * signed transaction on every read. Summaries keep what the audit checks of
 * every retained row; full values stay only for unreleased reservations, so
 * memory follows live work rather than the store's whole history.
 */
export const createVerifiedRowMemo = <
  Value extends Readonly<{ reservationId: string }>,
  Summary,
>(
  summarize: (value: Value) => Summary,
) => {
  const values = new Map<string, Value>();
  const summaries = new Map<
    string,
    Readonly<{ reservationId: string; summary: Summary }>
  >();
  const read = (key: string, parse: () => Value): Value => {
    const known = values.get(key);
    if (known !== undefined) return known;
    const value = parse();
    values.set(key, value);
    summaries.set(key, {
      reservationId: value.reservationId,
      summary: summarize(value),
    });
    return value;
  };
  return Object.freeze({
    read,
    summary: (key: string, parse: () => Value): Summary =>
      (summaries.get(key) ?? { summary: summarize(read(key, parse)) }).summary,
    /** Drops rows the audit no longer saw, and full values of released
     * reservations. */
    retain: (seen: ReadonlySet<string>, released: ReadonlySet<string>) => {
      for (const [key, { reservationId }] of summaries) {
        if (!seen.has(key)) summaries.delete(key);
        if (!seen.has(key) || released.has(reservationId)) values.delete(key);
      }
      for (const key of values.keys())
        if (!summaries.has(key)) values.delete(key);
    },
  });
};
