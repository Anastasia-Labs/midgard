import { SqlClient } from "@effect/sql";
import { Emulator } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { Database } from "../../src/services/database.js";

export const snapshotEmulator = (emulator: Emulator) =>
  structuredClone({
    ledger: emulator.ledger,
    mempool: emulator.mempool,
    chain: emulator.chain,
    blockHeight: emulator.blockHeight,
    slot: emulator.slot,
    time: emulator.time,
    protocolParameters: emulator.protocolParameters,
    datumTable: emulator.datumTable,
    treasury: emulator.treasury,
    transactionHistory: emulator.transactionHistory,
  });

export const restoreEmulator = (
  emulator: Emulator,
  snapshot: ReturnType<typeof snapshotEmulator>,
) => {
  Object.assign(emulator, structuredClone(snapshot));
};

export const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));
