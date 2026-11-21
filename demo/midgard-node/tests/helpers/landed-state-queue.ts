/**
 * The follower facts the node's landed state queue (P1, N2) reads, written
 * directly for tests without a followed chain: the follower's cursor at a
 * tip and the live outputs at the state-queue address as seed rows. P1 reads
 * them exactly as it reads a followed chain's, so the node's code under
 * test is the production read.
 *
 * `seedLandedStateQueue` replaces the outputs at the queue address with
 * `utxos` (stub fixtures). An emulator suite's follower follows the
 * emulator's chain instead (`emulator-l1-follower.ts`).
 */
import {
  type OutputSummary,
  postgresDialect,
} from "@al-ft/midgard-l1-follower";
import { insertSeedRowsIn } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { followerSqlTx } from "../../src/database/follower-schema.js";
import { type StateQueueContract } from "../../src/services/landed-state-queue.js";
import { FOLLOWER_GENERATION, followerBlockHash } from "./follower-view.js";

/** A Lucid UTxO's output as the follower stores it (no reference script). */
export const outputSummaryOf = (utxo: UTxO): OutputSummary => {
  const details = getAddressDetails(utxo.address);
  const assets = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(utxo.assets)) {
    if (unit === "lovelace") continue;
    const policy = unit.slice(0, 56);
    const names = assets.get(policy) ?? new Map<string, bigint>();
    names.set(unit.slice(56), quantity);
    assets.set(policy, names);
  }
  return {
    address: Buffer.from(details.address.hex, "hex"),
    paymentCredential:
      details.paymentCredential === undefined
        ? null
        : {
            hash: Buffer.from(details.paymentCredential.hash, "hex"),
            isScript: details.paymentCredential.type === "Script",
          },
    stakeCredential:
      details.stakeCredential === undefined
        ? null
        : Buffer.from(details.stakeCredential.hash, "hex"),
    lovelace: utxo.assets.lovelace ?? 0n,
    assets,
    datumHash:
      utxo.datum == null && utxo.datumHash != null
        ? Buffer.from(utxo.datumHash, "hex")
        : null,
    datum: utxo.datum == null ? null : Buffer.from(utxo.datum, "hex"),
    scriptRef: null,
  };
};

/**
 * In one transaction: the follower's cursor at `slot` (its generation kept;
 * a cursor already past `slot` stays), and the outputs at `address` replaced
 * by `utxos` as seed rows.
 */
export const writeAddressFacts = (
  address: string,
  utxos: readonly UTxO[],
  slot: number,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const cursor = yield* sql<{
          slot: number | string;
          generation: number | string;
        }>`SELECT slot, generation FROM l1_follower_cursor FOR UPDATE`;
        const generation =
          cursor[0] === undefined
            ? FOLLOWER_GENERATION
            : Number(cursor[0].generation);
        const tipSlot =
          cursor[0] === undefined
            ? slot
            : Math.max(slot, Number(cursor[0].slot));
        // A block already stored at the tip slot (an earlier file's chain in
        // this worker's database) stays the tip: the cursor must name a
        // stored block, or P1 reads `point_not_canonical`. A new tip block
        // sits one height above the highest stored one, as on a followed
        // chain, so the block d below it (the commit horizon lag) is the
        // tip written d times earlier.
        const stored = yield* sql<{
          hash: Uint8Array;
          height: number | string;
        }>`SELECT hash, height FROM l1_blocks WHERE slot = ${tipSlot}`;
        const hash =
          stored[0] === undefined
            ? followerBlockHash(tipSlot, generation)
            : Buffer.from(stored[0].hash);
        let height =
          stored[0] === undefined ? tipSlot : Number(stored[0].height);
        if (stored[0] === undefined) {
          const [highest] = yield* sql<{
            height: string | null;
          }>`SELECT max(height)::text AS height FROM l1_blocks`;
          if (highest?.height != null) height = Number(highest.height) + 1;
          yield* sql`INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count)
            VALUES (${tipSlot}, ${hash}, ${height}, NULL, 0)`;
        }
        yield* sql`INSERT INTO l1_follower_cursor
            (id, slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot)
          VALUES (true, ${tipSlot}, ${hash}, ${height}, ${generation}, 0, ${Buffer.alloc(32)}, 0)
          ON CONFLICT (id) DO UPDATE SET slot = EXCLUDED.slot, hash = EXCLUDED.hash,
            height = EXCLUDED.height`;
        const tx = yield* followerSqlTx;
        yield* Effect.promise(async () => {
          await tx.query("DELETE FROM l1_outputs WHERE address = ?", [
            Buffer.from(getAddressDetails(address).address.hex, "hex"),
          ]);
          await insertSeedRowsIn(
            tx,
            postgresDialect,
            tipSlot,
            utxos.map((utxo) => ({
              outRef: {
                txHash: Buffer.from(utxo.txHash, "hex"),
                index: utxo.outputIndex,
              },
              output: outputSummaryOf(utxo),
            })),
          );
        });
      }),
    );
  });

/** The landed queue's facts: `utxos` at `stateQueue`'s address, the cursor at `slot`. */
export const seedLandedStateQueue = (
  stateQueue: Pick<StateQueueContract, "spendingScriptAddress">,
  utxos: readonly UTxO[],
  slot = 1,
) => writeAddressFacts(stateQueue.spendingScriptAddress, utxos, slot);
