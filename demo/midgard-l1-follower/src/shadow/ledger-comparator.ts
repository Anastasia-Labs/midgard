import { readFile, rename, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { chainPoint, type L1NodeTransport } from "@al-ft/l1-node-transport";

import { encodeOutRef } from "../codec.js";
import { decodeLedgerUtxos } from "../decode/utxo.js";
import type { OutputSummary, OutRef, Point } from "../types.js";
import {
  reading,
  type ShadowComparator,
  type ShadowReading,
  unavailable,
} from "./comparator.js";

/** The ledger-state reads the comparator needs (an `L1NodeTransport`). */
export type LedgerStateReader = Pick<L1NodeTransport, "withLedgerState">;

type Row = Readonly<{
  outRef: string;
  address: string;
  lovelace: string;
  assets: Readonly<Record<string, Readonly<Record<string, string>>>>;
  datumHash: string | null;
  datum: string | null;
  scriptRef: string | null;
}>;

const row = (outRef: OutRef, output: OutputSummary): Row => ({
  outRef: encodeOutRef(outRef).toString("hex"),
  address: output.address.toString("hex"),
  lovelace: output.lovelace.toString(),
  assets: Object.fromEntries(
    [...output.assets].map(([policy, names]) => [
      policy,
      Object.fromEntries(
        [...names].map(([name, quantity]) => [name, quantity.toString()]),
      ),
    ]),
  ),
  datumHash: output.datumHash?.toString("hex") ?? null,
  datum: output.datum?.toString("hex") ?? null,
  scriptRef: output.scriptRef?.hash.toString("hex") ?? null,
});

const byOutRef = (a: Row, b: Row): number =>
  a.outRef < b.outRef ? -1 : a.outRef > b.outRef ? 1 : 0;

/** Decodes a `utxo_by_address` answer: `{[txHash, index] => output}`. */
export const decodeUtxoAnswer = (bytes: Uint8Array): Row[] =>
  decodeLedgerUtxos(bytes).map((utxo) => row(utxo.outRef, utxo.output));

type Baseline = Readonly<{ origin: string; outRefs: readonly string[] }>;

const pointText = (point: Point): string =>
  `${point.slot}:${point.hash.toString("hex")}`;

const isBaseline = (value: unknown): value is Baseline =>
  typeof value === "object" &&
  value !== null &&
  "origin" in value &&
  typeof value.origin === "string" &&
  "outRefs" in value &&
  Array.isArray(value.outRefs) &&
  (value.outRefs as unknown[]).every((item) => typeof item === "string");

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * The soak's stub comparator, role `follower`: the store's live tracked
 * outputs at the given addresses against the node's own UTxO set at the same
 * block (LSQ acquired at the store's cursor). Outputs that already existed
 * at the store's origin are not facts of the store (it was not seeded), so
 * the node's set at the origin is captured once into `<dir>/ledger-baseline.json`
 * and subtracted. It proves the soak end to end before any role comparator
 * exists; it is not old-code tooling and stays after the cutovers.
 */
export const ledgerComparator = (
  input: Readonly<{
    ledger: LedgerStateReader;
    addresses: readonly Buffer[];
    dir: string;
  }>,
): ShadowComparator => {
  const path = join(input.dir, "ledger-baseline.json");
  let baseline: Set<string> | undefined;
  const query = async (point: Point): Promise<Row[]> =>
    await input.ledger.withLedgerState(
      chainPoint(BigInt(point.slot), point.hash.toString("hex")),
      async (session) =>
        decodeUtxoAnswer(
          await session.query({
            query: "utxo_by_address",
            addresses: input.addresses,
          }),
        ),
    );
  const loadBaseline = async (origin: Point): Promise<Set<string> | string> => {
    if (baseline !== undefined) return baseline;
    try {
      const parsed = JSON.parse(await readFile(path, "utf8")) as unknown;
      if (!isBaseline(parsed) || parsed.origin !== pointText(origin))
        return `${path} does not belong to origin ${pointText(origin)}`;
      baseline = new Set(parsed.outRefs);
      return baseline;
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "ENOENT")
        return `baseline unreadable: ${message(error)}`;
    }
    let rows: Row[];
    try {
      rows = await query(origin);
    } catch (error) {
      return `baseline at the origin not captured: ${message(error)}`;
    }
    const record: Baseline = {
      origin: pointText(origin),
      outRefs: rows.map((r) => r.outRef).sort(),
    };
    await writeFile(`${path}.tmp`, JSON.stringify(record));
    await rename(`${path}.tmp`, path);
    baseline = new Set(record.outRefs);
    return baseline;
  };
  return {
    role: "follower",
    name: "ledger-utxos",
    projected: async ({ store }): Promise<ShadowReading> => {
      const rows: Row[] = [];
      for (const address of input.addresses) {
        const read = await store.liveUtxos({ by: "address", address });
        if (read.kind !== "ok") return unavailable(read.kind);
        for (const utxo of read.utxos) rows.push(row(utxo.outRef, utxo.output));
      }
      return reading(rows.sort(byOutRef));
    },
    current: async ({ store, at }): Promise<ShadowReading> => {
      const cursor = await store.cursor();
      if (cursor === null) return unavailable("the store has no cursor");
      const known = await loadBaseline(cursor.origin);
      if (typeof known === "string") return unavailable(known);
      try {
        return reading(
          (await query(at.point))
            .filter((r) => !known.has(r.outRef))
            .sort(byOutRef),
        );
      } catch (error) {
        return unavailable(`ledger state at the block: ${message(error)}`);
      }
    },
  };
};
