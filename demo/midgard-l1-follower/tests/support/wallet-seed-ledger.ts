import {
  type ChainPoint,
  type LedgerQuery,
  type QueryOptions,
  TransportRequestError,
} from "@al-ft/l1-node-transport";

import type { FactStore, WalletLedger } from "../../src/index.js";
import { encodeUtxoAnswer, type SimUtxo } from "../../src/testing/index.js";
import { fill } from "./small-chain.js";

/** A second own wallet (enterprise key address) the store does not track yet. */
export const WALLET_B = Buffer.concat([Buffer.of(0x60), fill(0x44, 28)]);

export const preOrigin = (
  byte: number,
  address: Buffer,
  index = 0,
): SimUtxo => ({
  outRef: { txHash: fill(byte), index },
  output: { address, lovelace: BigInt(byte) * 1_000_000n },
});

export const key = (utxo: SimUtxo): string =>
  `${utxo.outRef.txHash.toString("hex")}#${utxo.outRef.index}`;

/**
 * A node's ledger for the seed: the UTxO set it answers `utxo_by_address`
 * from, acquirable only at the points `acquirable` admits (the volatile
 * window). Records every acquisition and query.
 */
export const fakeLedger = (
  input: Readonly<{
    utxos: () => readonly SimUtxo[];
    acquirable: (at: ChainPoint | "tip") => boolean;
    duringQuery?: () => Promise<void>;
    tamper?: (utxos: SimUtxo[]) => SimUtxo[];
  }>,
): WalletLedger & {
  queries: string[][];
  acquired: string[];
  deadlines: (string | undefined)[];
} => {
  const queries: string[][] = [];
  const acquired: string[] = [];
  const deadlines: (string | undefined)[] = [];
  return {
    queries,
    acquired,
    deadlines,
    withLedgerState: async (at, use) => {
      if (!input.acquirable(at))
        throw new TransportRequestError(
          "acquire_point_too_old",
          "the point is not in the volatile window",
        );
      acquired.push(
        at === "tip" ? "tip" : at.kind === "point" ? `${at.slot}` : "origin",
      );
      return await use({
        query: async (query: LedgerQuery, options?: QueryOptions) => {
          if (query.query !== "utxo_by_address")
            throw new Error(`unexpected query ${query.query}`);
          deadlines.push(options?.deadline);
          const wanted = query.addresses.map((a) =>
            Buffer.from(a).toString("hex"),
          );
          queries.push(wanted);
          await input.duringQuery?.();
          const matching = input
            .utxos()
            .filter((u) => wanted.includes(u.output.address.toString("hex")));
          return encodeUtxoAnswer(input.tamper?.(matching) ?? matching);
        },
      });
    },
  };
};

export const atCursor =
  (store: FactStore) =>
  async (): Promise<(at: ChainPoint | "tip") => boolean> => {
    const cursor = await store.cursor();
    return (at) =>
      at !== "tip" &&
      at.kind === "point" &&
      cursor !== null &&
      at.slot === BigInt(cursor.point.slot) &&
      at.hash === cursor.point.hash.toString("hex");
  };

export const seedRows = async (store: FactStore) =>
  await store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT tx_hash, output_index, seed_slot, created_slot FROM l1_outputs WHERE created_slot IS NULL ORDER BY tx_hash, output_index",
      )
    ).map(
      (row) =>
        `${Buffer.from(row.tx_hash as Uint8Array).toString("hex")}#${Number(row.output_index)}@${Number(row.seed_slot)}`,
    ),
  );
