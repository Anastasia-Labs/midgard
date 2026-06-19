import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "@/database/index.js";
import * as Ledger from "@/database/utils/ledger.js";
import { materializeConfirmedLedgerSnapshot } from "@/transactions/state-queue/confirmed-ledger-snapshot.js";

import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

const VALID_ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";

const txOutRefCbor = (txHashByte: string, outputIndex: bigint) =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHashByte.repeat(32)),
      outputIndex,
    ).to_cbor_bytes(),
  );

describe("confirmed ledger snapshot materialization", () => {
  it("decodes finalized journal UTxO members into confirmed ledger entries", async () => {
    const outref = txOutRefCbor("42", 3n);
    const output = makeMidgardTxOutput(
      CML.Address.from_bech32(VALID_ADDRESS),
      CML.Value.from_coin(2_000_000n),
    );
    const now = new Date("2026-06-19T00:00:00.000Z");
    const record = {
      [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: Buffer.alloc(28, 1),
      [PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]: Buffer.alloc(
        32,
        2,
      ),
      [PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN]: "lease",
      [PendingBlockFinalizationsDB.Columns.BASE_SNAPSHOT_ID]: "snapshot",
      [PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF]: "base#0",
      [PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH]:
        Buffer.alloc(28, 3),
      [PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR]: "d87980",
      [PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME]: now,
      [PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME]: now,
      [PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]:
        SDK.EMPTY_MERKLE_TREE_ROOT,
      [PendingBlockFinalizationsDB.Columns.STATUS]:
        PendingBlockFinalizationsDB.Status.Finalized,
      [PendingBlockFinalizationsDB.Columns.OBSERVED_CONFIRMED_AT_MS]: 1n,
      [PendingBlockFinalizationsDB.Columns.CREATED_AT]: now,
      [PendingBlockFinalizationsDB.Columns.UPDATED_AT]: now,
      depositEventIds: [],
      withdrawalEventIds: [],
      mempoolTxIds: [],
      depositMembers: [],
      withdrawalMembers: [],
      txMembers: [],
      utxoMembers: [
        {
          [PendingBlockFinalizationsDB.UtxoColumns.HEADER_HASH]:
            Buffer.alloc(28, 1),
          [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: outref,
          [PendingBlockFinalizationsDB.UtxoColumns.ORDINAL]: 0,
          [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]:
            output.to_cbor_bytes(),
        },
      ],
    } satisfies PendingBlockFinalizationsDB.Record;

    const snapshot = await Effect.runPromise(
      materializeConfirmedLedgerSnapshot(record),
    );

    expect(snapshot.root).not.toEqual(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(snapshot.entries).toHaveLength(1);
    expect(snapshot.entries[0]?.[Ledger.Columns.TX_ID].toString("hex")).toEqual(
      "42".repeat(32),
    );
    expect(snapshot.entries[0]?.[Ledger.Columns.OUTREF]).toEqual(outref);
    expect(snapshot.entries[0]?.[Ledger.Columns.ADDRESS]).toEqual(
      VALID_ADDRESS,
    );
  });
});
