/**
 * The provider's plain-output reads (`plainOutputsByDatum`,
 * `plainOutputsExceptDatums`, `factsPoint`): plain outputs at one tracked
 * address in one snapshot of the facts, with the node's mempool applied
 * when the provider has a pending source.
 */
import type { UTxO } from "@lucid-evolution/lucid";

import type { Dialect, SqlTx } from "../sql/backend.js";
import type { FactStore } from "../store/fact-store.js";
import {
  livePlainOutputsByDatumIn,
  livePlainOutputsExceptDatumsIn,
  type PlainOutputsRead,
} from "../store/plain-outputs.js";
import * as reads from "../store/reads.js";
import type { Cursor, OutRef } from "../types.js";
import { L1ProviderTransientError } from "./errors.js";
import { lucidUtxo, outRefKey } from "./ledger-provider.js";
import {
  byOutRef,
  isPlain,
  overLedger,
  parseOutRef,
  type PendingApplied,
  utxoKey,
} from "./pending.js";
import { addressBytes } from "./utxo.js";

/**
 * Plain outputs read in one snapshot of the facts, with that snapshot's
 * point: `generation:slot:hash` of the follower's cursor, followed by
 * `:mempool:<digest>` of the pending transactions applied when there are
 * any. Any applied block, rewind or other mempool listing gives another
 * point.
 */
export type PlainOutputsAnswer = Readonly<{
  point: string;
  utxos: readonly UTxO[];
}>;

const cursorPoint = ({ point, generation }: Cursor): string =>
  `${generation.toString()}:${point.slot.toString()}:${point.hash.toString("hex")}`;

/** The point of an answer read at `cursor` with `applied` over it. */
export const answerPoint = (
  cursor: Cursor,
  applied?: PendingApplied,
): string =>
  applied === undefined || applied.digest === ""
    ? cursorPoint(cursor)
    : `${cursorPoint(cursor)}:mempool:${applied.digest}`;

/**
 * What one plain-output read asks the facts (`run`, skipping `skipped`) and
 * picks from the candidates with the mempool applied (`select`).
 */
export type PlainQuery = Readonly<{
  run: (
    tx: SqlTx,
    dialect: Dialect,
    address: Buffer,
    skipped: readonly OutRef[],
  ) => Promise<PlainOutputsRead>;
  select: (candidates: readonly UTxO[]) => readonly UTxO[];
}>;

/** The first `limit` copies of each requested inline datum (hex). */
export const plainByDatum = (
  requests: readonly Readonly<{ datum: string; limit: number }>[],
): PlainQuery => {
  const limits = new Map<string, number>();
  for (const { datum, limit } of requests) {
    const key = Buffer.from(datum, "hex").toString("hex");
    limits.set(key, Math.max(limit, limits.get(key) ?? 0));
  }
  return {
    run: async (tx, dialect, address, skipped) =>
      await livePlainOutputsByDatumIn(tx, dialect, {
        address,
        requests: requests.map(({ datum, limit }) => ({
          datum: Buffer.from(datum, "hex"),
          limit,
        })),
        excluded: skipped,
      }),
    select: (candidates) => {
      const taken = new Map<string, number>();
      return candidates.filter(({ datum }) => {
        const limit = datum == null ? undefined : limits.get(datum);
        if (datum == null || limit === undefined) return false;
        const count = taken.get(datum) ?? 0;
        taken.set(datum, count + 1);
        return count < limit;
      });
    },
  };
};

/** The first `limit` outputs whose inline datum (if any) is none of `datums` (hex). */
export const plainExceptDatums = (
  datums: readonly string[],
  limit: number,
): PlainQuery => {
  const unwanted = new Set(
    datums.map((datum) => Buffer.from(datum, "hex").toString("hex")),
  );
  return {
    run: async (tx, dialect, address, skipped) =>
      await livePlainOutputsExceptDatumsIn(tx, dialect, {
        address,
        datums: datums.map((datum) => Buffer.from(datum, "hex")),
        excluded: skipped,
        limit,
      }),
    select: (candidates) =>
      candidates
        .filter(({ datum }) => datum == null || !unwanted.has(datum))
        .slice(0, limit),
  };
};

/**
 * One plain-output read at `address` in one snapshot of the facts. With
 * `applied`, the read also skips the stored outputs there that pending
 * transactions spend, and the query's `select` picks the answer from the stored
 * outputs and the plain outputs pending transactions create there, in
 * out-ref order.
 */
export const readPlainOutputs = async ({
  store,
  read,
  applied,
  address,
  excluded,
  query: { run, select },
}: Readonly<{
  store: FactStore;
  /** One read transaction of the facts, as the provider runs it. */
  read: <T>(run: (tx: SqlTx) => Promise<T>) => Promise<T>;
  applied: PendingApplied | undefined;
  address: Buffer;
  excluded: readonly OutRef[];
  query: PlainQuery;
}>): Promise<PlainOutputsAnswer> => {
  const result = await read(async (tx) => {
    if (applied === undefined || applied.spent.size === 0)
      return await run(tx, store.dialect, address, excluded);
    const spentHere = (
      await reads.outputsIn(
        tx,
        store.dialect,
        [...applied.spent].map(parseOutRef),
      )
    ).flatMap((row) =>
      row !== null && row.output.address.equals(address) ? [row.outRef] : [],
    );
    return await run(tx, store.dialect, address, [...excluded, ...spentHere]);
  });
  if (result.kind !== "ok")
    throw new L1ProviderTransientError("follower", result.kind);
  const stored = result.utxos.map(lucidUtxo);
  if (applied === undefined)
    return Object.freeze({ point: answerPoint(result.cursor), utxos: stored });
  const excludedKeys = new Set(excluded.map(outRefKey));
  const candidates = overLedger(
    applied,
    stored,
    (utxo) =>
      isPlain(utxo) &&
      addressBytes(utxo.address).equals(address) &&
      !excludedKeys.has(utxoKey(utxo)),
  );
  return Object.freeze({
    point: answerPoint(result.cursor, applied),
    utxos: select(byOutRef(candidates)),
  });
};
