/**
 * The orphan repair (`follower-orphan-repair.ts`) for withdrawals: a
 * withdrawal row whose follower admission left the chain (its key is gone
 * from `l1_event_keys`) is deleted when nothing holds it, and kept while it
 * is assigned to a header, finalized, or named by a block journal that is
 * not abandoned. A canonical withdrawal is never touched. Deleting one
 * re-runs classification of the unassigned withdrawals that stay.
 *
 * A held orphan is of the kind of its header, finalized or not: own-landed
 * while its header is a processed own landed block (it holds no follower
 * write gate), foreign-landed while it is a processed foreign one, and a
 * journal orphan otherwise. The recovery hold's detail names a foreign
 * landed header that holds orphans.
 */
import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  countForeignLandedOrphans,
  countJournalOrphans,
  countOrphansAwaitingRecovery,
  countOwnLandedOrphans,
  deleteUnheldOrphans,
  describeOrphansAwaitingRecovery,
} from "../src/database/follower-orphan-repair.js";
import { testWrite } from "./helpers/driver-recompute.js";
import {
  insertOwnJournal,
  JournalStatus,
} from "./helpers/landed-blocks-sim.own.js";
import {
  BLOCK,
  freshNative,
  land,
  processOf,
  R1,
  root,
  run,
} from "./landed-blocks-rebase.fixture.js";
import { resetApplicationTables } from "./utils.js";

const digest = (label: string) => createHash("sha256").update(label).digest();

/** Header hashes are 28 bytes. */
const JOURNAL = "c4".repeat(28);
const HEADER = Buffer.from("c5".repeat(28), "hex");

type Withdrawal = Readonly<{
  label: string;
  /** Whether the follower still holds its admission key. */
  canonical: boolean;
  header?: Buffer;
  status?: "awaiting" | "projected" | "finalized";
  /** Classified already (so a re-classification is visible). */
  classified?: boolean;
}>;

const keyOf = (label: string) => digest(`orphan-repair:key:${label}`);
const originOf = (label: string) =>
  Buffer.concat([digest(`orphan-repair:origin:${label}`), Buffer.alloc(2)]);

const insertWithdrawal = (row: Withdrawal) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bytes = Buffer.from("00", "hex");
    const id = digest(row.label);
    yield* sql`INSERT INTO withdrawal_utxos
      (event_id, raw_event_info, settlement_event_info, inclusion_time,
        withdrawal_l1_tx_hash, withdrawal_l1_output_index, asset_name, l2_outref,
        l2_owner, l2_value, l1_address, l1_datum, refund_address, refund_datum,
        validity, projected_header_hash, status, l1_event_key, l1_origin_outref)
      VALUES (${id}, ${bytes}, ${row.classified === true ? bytes : null},
        ${new Date(1_000)}, ${id}, 0, ${bytes}, ${bytes},
        ${Buffer.alloc(28, 4)}, ${bytes}, ${bytes}, ${bytes}, ${bytes}, ${bytes},
        ${row.classified === true ? "WithdrawalIsValid" : null},
        ${row.header ?? null}, ${row.status ?? "awaiting"},
        ${keyOf(row.label)}, ${originOf(row.label)})`;
    if (row.canonical)
      yield* sql`INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot)
        VALUES ('withdrawal', ${keyOf(row.label)}, ${originOf(row.label)}, 1)`;
  });

/** Names withdrawal `label` as a member of block journal JOURNAL. */
const insertJournalMember = (label: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const payload = Buffer.from(`orphan-member:${label}`);
    yield* sql`INSERT INTO pending_block_finalization_withdrawals ${sql.insert({
      header_hash: Buffer.from(JOURNAL, "hex"),
      member_id: digest(label),
      ordinal: 0,
      validity: "WithdrawalIsValid",
      validity_detail: "{}",
      classification_revision: 0,
      classification_sha256: digest(`classification:${label}`),
      payload_cbor: payload,
      payload_sha256: createHash("sha256").update(payload).digest(),
      source_table: "withdrawal_utxos",
      source_id: digest(label),
      source_time_stamp_tz: new Date(1_000),
      l1_event_key: keyOf(label),
      l1_origin_outref: originOf(label),
    } as never)}`;
  });

const rowsOf = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    event_id: Buffer;
    status: string;
    validity: string | null;
  }>`SELECT event_id, status::text AS status, validity::text AS validity
    FROM withdrawal_utxos ORDER BY event_id`;
  return new Map(
    rows.map((row) => [
      row.event_id.toString("hex"),
      { status: row.status, validity: row.validity },
    ]),
  );
});

const kinds = Effect.gen(function* () {
  return {
    journal: yield* countJournalOrphans,
    ownLanded: yield* countOwnLandedOrphans,
    foreignLanded: yield* countForeignLandedOrphans,
    awaiting: yield* countOrphansAwaitingRecovery,
  };
});

const arrange = async (rows: readonly Withdrawal[]) => {
  const globals = await processOf(freshNative());
  await run(globals, resetApplicationTables);
  await run(globals, Effect.forEach(rows, insertWithdrawal, { discard: true }));
  return globals;
};

describe("the orphan repair deletes orphaned withdrawals nothing holds", () => {
  it("deletes an unheld orphaned withdrawal, keeps a canonical one, and re-classifies the unassigned withdrawals that stay", async () => {
    const globals = await arrange([
      { label: "orphan", canonical: false },
      { label: "canonical", canonical: true, classified: true },
    ]);
    const deleted = await run(globals, deleteUnheldOrphans);
    expect(deleted).toBe(1);
    const rows = await run(globals, rowsOf);
    expect(rows.has(digest("orphan").toString("hex"))).toBe(false);
    // The canonical row stays, and classification runs again for it.
    expect(rows.get(digest("canonical").toString("hex"))).toEqual({
      status: "awaiting",
      validity: null,
    });
    expect(await run(globals, countJournalOrphans)).toBe(0);
    expect(await run(globals, countOrphansAwaitingRecovery)).toBe(0);
  });

  it("keeps an orphaned withdrawal assigned to a header or finalized, counting it as held", async () => {
    const header = HEADER;
    const globals = await arrange([
      {
        label: "headed",
        canonical: false,
        header,
        status: "projected",
        classified: true,
      },
      {
        label: "finalized",
        canonical: false,
        header,
        status: "finalized",
        classified: true,
      },
    ]);
    expect(await run(globals, deleteUnheldOrphans)).toBe(0);
    const rows = await run(globals, rowsOf);
    expect(rows.size).toBe(2);
    expect(rows.get(digest("headed").toString("hex"))?.status).toBe(
      "projected",
    );
    // Its header is no landed block: journal orphans, which hold the gate.
    expect(await run(globals, kinds)).toEqual({
      journal: 2,
      ownLanded: 0,
      foreignLanded: 0,
      awaiting: 2,
    });
    expect(await run(globals, describeOrphansAwaitingRecovery(2))).toBe(
      "2 orphaned event admission(s) wait for their block journal's disposition or the landed-block rebase",
    );
  });

  it("classifies a held orphan, finalized or not, by its header: own-landed holds no gate, foreign-landed does", async () => {
    const orphans: Withdrawal[] = [
      {
        label: "headed",
        canonical: false,
        header: HEADER,
        status: "projected",
        classified: true,
      },
      {
        label: "final",
        canonical: false,
        header: HEADER,
        status: "finalized",
        classified: true,
      },
      {
        label: "kept",
        canonical: true,
        header: HEADER,
        status: "projected",
        classified: true,
      },
    ];
    const own = await arrange(orphans);
    await land(own, { headerHash: HEADER.toString("hex"), kind: "own" });
    expect(await run(own, kinds)).toEqual({
      journal: 0,
      ownLanded: 2,
      foreignLanded: 0,
      awaiting: 0,
    });

    const foreign = await arrange(orphans);
    await land(foreign, { headerHash: HEADER.toString("hex") });
    expect(await run(foreign, kinds)).toEqual({
      journal: 0,
      ownLanded: 0,
      foreignLanded: 2,
      awaiting: 2,
    });
    // The hold names the foreign header: it includes events no longer on
    // L1, so the node does not build on it until it leaves the landed queue.
    expect(await run(foreign, describeOrphansAwaitingRecovery(2))).toBe(
      `foreign landed block ${HEADER.toString("hex")} (2) includes 2 event(s) no longer on L1: it is fault-provable (fabricated deposit or withdrawal) and the node does not build on it; the hold clears when a fault proof or an L1 rollback removes the header from the landed queue`,
    );
  });

  it("keeps an orphaned withdrawal a live block journal names, and deletes it once that journal is abandoned", async () => {
    const globals = await arrange([{ label: "member", canonical: false }]);
    await run(
      globals,
      testWrite(
        insertOwnJournal({
          headerHash: JOURNAL,
          baseHeaderHash: BLOCK,
          baseUtxosRoot: R1,
          expectedUtxosRoot: root(0x12),
          spent: [],
          produced: [],
          txIds: [],
          at: new Date(1_000),
        }),
      ),
    );
    await run(globals, insertJournalMember("member"));
    expect(await run(globals, deleteUnheldOrphans)).toBe(0);
    expect((await run(globals, rowsOf)).size).toBe(1);
    expect(await run(globals, countJournalOrphans)).toBe(1);

    await run(
      globals,
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) =>
          sql`UPDATE pending_block_finalizations SET status = ${JournalStatus.Abandoned}
            WHERE header_hash = ${Buffer.from(JOURNAL, "hex")}`,
      ),
    );
    expect(await run(globals, deleteUnheldOrphans)).toBe(1);
    expect((await run(globals, rowsOf)).size).toBe(0);
    expect(await run(globals, countJournalOrphans)).toBe(0);
  });
});
