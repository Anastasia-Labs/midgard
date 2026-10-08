/**
 * The follower facts the node's landed state queue (P1, N2) reads, written
 * directly for tests without a followed chain: the follower's cursor at a
 * tip and the live outputs at the state-queue address as seed rows. P1 reads
 * them exactly as it reads a followed chain's, so the node's code under
 * test is the production read.
 *
 * - `seedLandedStateQueue` replaces the outputs at the queue address with
 *   `utxos` (stub fixtures).
 * - `mirrorEmulatorStateQueue` does so from an emulator's live outputs at the
 *   address, at the emulator's slot; `followEmulatorStateQueue` repeats it in
 *   the background whenever the emulator moved, standing in for a follower
 *   that follows the emulator, and `withEmulatorStateQueue` runs an effect
 *   under it.
 * - `emulatorStateQueueSnapshot` / `emulatorStateQueueUTxOs` mirror once and
 *   read P1, for tests asserting on the queue.
 */
import {
  type OutputSummary,
  postgresDialect,
} from "@al-ft/midgard-l1-follower";
import { insertSeedRowsIn } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import {
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Duration, Effect, Schedule } from "effect";

import { followerSqlTx } from "../../src/database/follower-schema.js";
import {
  landedStateQueueSnapshot,
  landedStateQueueUTxOs,
  type StateQueueContract,
  type StateQueueSnapshotReason,
} from "../../src/services/landed-state-queue.js";
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
const writeQueueFacts = (
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
        const hash = followerBlockHash(tipSlot, generation);
        yield* sql`INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count)
          VALUES (${tipSlot}, ${hash}, ${tipSlot}, NULL, 0) ON CONFLICT DO NOTHING`;
        yield* sql`INSERT INTO l1_follower_cursor
            (id, slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot)
          VALUES (true, ${tipSlot}, ${hash}, ${tipSlot}, ${generation}, 0, ${Buffer.alloc(32)}, 0)
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
) => writeQueueFacts(stateQueue.spendingScriptAddress, utxos, slot);

/** The emulator's live outputs at the queue address, with the slot it is at. */
const emulatorQueueOutputs = (
  lucid: LucidEvolution,
  stateQueue: Pick<StateQueueContract, "spendingScriptAddress">,
) =>
  Effect.map(
    Effect.promise(() => lucid.utxosAt(stateQueue.spendingScriptAddress)),
    (utxos) => ({ utxos, slot: lucid.currentSlot() }),
  );

/** The landed queue's facts from the emulator's live outputs at the queue address. */
export const mirrorEmulatorStateQueue = (
  lucid: LucidEvolution,
  stateQueue: Pick<StateQueueContract, "spendingScriptAddress">,
) =>
  Effect.flatMap(emulatorQueueOutputs(lucid, stateQueue), ({ utxos, slot }) =>
    writeQueueFacts(stateQueue.spendingScriptAddress, utxos, slot),
  );

/**
 * `mirrorEmulatorStateQueue` now, then every `interval` the emulator moved,
 * for as long as the caller's scope is open: the emulator's follower, as
 * the node's fibers see it.
 */
export const followEmulatorStateQueue = (
  lucid: LucidEvolution,
  stateQueue: Pick<StateQueueContract, "spendingScriptAddress">,
  interval: Duration.DurationInput = Duration.millis(100),
) =>
  Effect.gen(function* () {
    let last = "";
    const step = Effect.flatMap(
      emulatorQueueOutputs(lucid, stateQueue),
      ({ utxos, slot }) => {
        const key = `${slot.toString()}:${utxos
          .map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`)
          .sort()
          .join(",")}`;
        if (key === last) return Effect.void;
        return writeQueueFacts(
          stateQueue.spendingScriptAddress,
          utxos,
          slot,
        ).pipe(Effect.tap(() => Effect.sync(() => (last = key))));
      },
    );
    yield* step;
    yield* Effect.forkScoped(
      Effect.repeat(
        step.pipe(Effect.catchAllCause((cause) => Effect.logDebug(cause))),
        Schedule.spaced(interval),
      ),
    );
  });

/** `effect` with the emulator's queue followed into P1 while it runs. */
export const withEmulatorStateQueue =
  (
    lucid: LucidEvolution,
    stateQueue: Pick<StateQueueContract, "spendingScriptAddress">,
  ) =>
  <A, E, R>(effect: Effect.Effect<A, E, R>) =>
    Effect.scoped(
      Effect.zipRight(followEmulatorStateQueue(lucid, stateQueue), effect),
    );

/** The landed queue's snapshot after mirroring the emulator's queue into P1. */
export const emulatorStateQueueSnapshot = (
  lucid: LucidEvolution,
  stateQueue: StateQueueContract,
  reason: StateQueueSnapshotReason = "manual_status",
) =>
  Effect.zipRight(
    mirrorEmulatorStateQueue(lucid, stateQueue),
    landedStateQueueSnapshot(stateQueue, reason),
  );

/** The landed queue's nodes, root first, after mirroring the emulator's queue. */
export const emulatorStateQueueUTxOs = (
  lucid: LucidEvolution,
  stateQueue: StateQueueContract,
) =>
  Effect.zipRight(
    mirrorEmulatorStateQueue(lucid, stateQueue),
    landedStateQueueUTxOs(stateQueue, "the test's queue read"),
  );
