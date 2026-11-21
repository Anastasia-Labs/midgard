/**
 * The journal of a signed own commit as a separate connection reads it:
 * its row, the signed bytes the intent journal holds for it (its
 * `l1_intents` row, which `intended_tx_hash` links), its derived status
 * and whether it is dead (`deadJournals`). Reading on its own connection
 * proves the signed intent committed before the commit reached the
 * provider, rather than observing an inherited transaction.
 */
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { Database } from "../../src/services/database.js";

export const readSignedCommitJournal = (txHash: string) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<
        Pending.Row & {
          intent_tx_cbor: Buffer | null;
          own_block_status: Pending.OwnBlockStatus;
        }
      >`SELECT p.*, intent.tx_cbor AS intent_tx_cbor,
          ${Pending.ownBlockStatusSql(sql, "p")} AS own_block_status
        FROM pending_block_finalizations p
        LEFT JOIN l1_intents intent ON intent.tx_hash = p.intended_tx_hash
        WHERE p.intended_tx_hash = ${Buffer.from(txHash, "hex")}`;
      expect(rows).toHaveLength(1);
      const { intent_tx_cbor, own_block_status, ...row } = rows[0]!;
      const dead = yield* Pending.deadJournals;
      return {
        row,
        signedCbor: intent_tx_cbor?.toString("hex"),
        status: own_block_status,
        dead: dead.causes.has(row[Pending.Columns.HEADER_HASH].toString("hex")),
      };
    }).pipe(Effect.provide(Database.layer)),
  );

export type SignedCommitJournal = Awaited<
  ReturnType<typeof readSignedCommitJournal>
>;

/** `journal` holds `signedCbor`, whose body hash is `txHash`, and is not
 * dead. */
export const expectSignedCommit = (
  journal: SignedCommitJournal,
  txHash: string,
  signedCbor: string,
) => {
  const { row } = journal;
  expect(row[Pending.Columns.PREPARED_TX_HASH]?.toString("hex")).toBe(txHash);
  expect(row[Pending.Columns.INTENDED_TX_HASH]?.toString("hex")).toBe(txHash);
  expect(journal.signedCbor).toBe(signedCbor);
  expect(
    CML.hash_transaction(
      CML.Transaction.from_cbor_hex(signedCbor).body(),
    ).to_hex(),
  ).toBe(txHash);
  expect(journal.dead).toBe(false);
};
