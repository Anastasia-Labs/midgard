import "./utils.js";

import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { foreignReplayMaterial } from "../src/landed-blocks/replay-foreign.js";
import type { WithdrawalMembership } from "../src/landed-blocks/store.js";
import { type MidgardMpf, processMpfs } from "../src/mpf/index.js";
import { ledgerEntryToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { verifyAndImportBlock } from "../src/mpf/verified-block-import.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { insertWithdrawals } from "./helpers/event-rows.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * Two due withdrawal orders naming one L2 outref in one block window. Anyone
 * can submit both, so the block must still build: each withdrawal is
 * classified against the ledger the earlier payable ones leave, and foreign
 * replay must derive the same bytes.
 */

// The native owner is replaced by the in-process trace builder over the same
// base ledger; the withdrawal classification and trace sources stay real.
const base = vi.hoisted(() => ({
  entries: [] as { readonly key: Buffer; readonly value: Buffer }[],
}));
vi.mock("../src/mpf/transition-trace.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/mpf/transition-trace.js")
  >("../src/mpf/transition-trace.js");
  const { MidgardMpf: Mpf } = await vi.importActual<
    typeof import("../src/mpf/store.midgard-mpf.js")
  >("../src/mpf/store.midgard-mpf.js");
  return {
    ...actual,
    buildNativeTransitionTraceResult: ({
      nativeMpf,
      ...rest
    }: Parameters<typeof actual.buildNativeTransitionTraceResult>[0]) =>
      Effect.gen(function* () {
        const ledgerMpf = yield* Mpf.createScratchFromList(
          "duplicate-withdrawal-ledger",
          base.entries,
        );
        const result = yield* actual.buildTransitionTraceResult({
          ledgerMpf,
          ...rest,
        });
        nativeMpf.candidateRoot = result.finalUtxosRoot;
        nativeMpf.eventLog = Buffer.alloc(0);
        nativeMpf.eventLogDigest = "00".repeat(32);
        nativeMpf.eventRoots = [];
        return result;
      }),
  };
});

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const T0 = Date.parse("2026-01-01T00:00:00.000Z");
const at = (seconds: number) => new Date(T0 + seconds * 1_000);
const hex = (buffer: Buffer) => buffer.toString("hex");
const LOVELACE = 7_000_000n;
const ledgerEntry = (byte: number): LedgerUtils.Entry => ({
  [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, byte),
  [LedgerUtils.Columns.OUTREF]: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, byte),
    outputIndex: 0,
  }),
  [LedgerUtils.Columns.OUTPUT]: encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: { lovelace: LOVELACE, assets: new Map() },
  }),
  [LedgerUtils.Columns.ADDRESS]: wallet.address,
});
// U is the outref both orders name; V stays untouched.
const U = ledgerEntry(0xaa);
const V = ledgerEntry(0x11);
const BASE = [U, V];

const order = async (idByte: number): Promise<WithdrawalsDB.Entry> => {
  const value: SDK.Value = new Map([["", new Map([["", LOVELACE]])]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
    l2_owner: privateKey.to_public().hash().to_hex(),
    l2_value: value,
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(wallet.address),
    ),
    l1_datum: "NoDatum",
  };
  const info: SDK.WithdrawalInfo = {
    body,
    signature: SDK.signWithdrawalBody(privateKey, body),
    validity: "WithdrawalIsValid",
  };
  const id = { transactionId: idByte.toString(16).repeat(64), outputIndex: 0n };
  return {
    [WithdrawalsDB.Columns.ID]: Buffer.from(
      Data.to(id, SDK.OutputReference),
      "hex",
    ),
    raw_event_info: Buffer.from(Data.to(info, SDK.WithdrawalInfo), "hex"),
    settlement_event_info: null,
    // Both orders fall due at the same instant.
    inclusion_time: at(1),
    withdrawal_l1_tx_hash: Buffer.alloc(32, idByte),
    withdrawal_l1_output_index: 0,
    asset_name: Buffer.alloc(32, idByte),
    l2_outref: Buffer.from(Data.to(body.l2_outref, SDK.OutputReference), "hex"),
    l2_owner: Buffer.from(body.l2_owner, "hex"),
    l2_value: Buffer.from(Data.to(value, SDK.Value), "hex"),
    l1_address: Buffer.from(Data.to(body.l1_address, SDK.AddressData), "hex"),
    l1_datum: Buffer.from(Data.to(body.l1_datum, SDK.CardanoDatum), "hex"),
    refund_address: Buffer.alloc(0),
    refund_datum: Buffer.alloc(0),
    validity: null,
    validity_detail: {},
    classification_revision: 0,
    reopened_from_header_hash: null,
    projected_header_hash: null,
    status: WithdrawalsDB.Status.Awaiting,
  };
};

const run = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

const build = (orders: readonly WithdrawalsDB.Entry[], baseRoot: string) =>
  Effect.gen(function* () {
    const { MidgardMpf } = yield* Effect.promise(
      () => import("../src/mpf/store.midgard-mpf.js"),
    );
    yield* resetApplicationTables;
    yield* insertWithdrawals([...orders]);
    const transactionsMpf = yield* MidgardMpf.createScratch(
      "duplicate-withdrawal-transactions",
    );
    const produced = yield* processMpfs(
      transactionsMpf as unknown as MidgardMpf,
      [],
      {
        currentBlockStartTime: at(0),
        fixedBlockEndTime: at(10),
        initialLedgerEntries: BASE,
        nativeMpf: {
          handle: { baseRoot },
          ownerBinarySha256: "00".repeat(32),
        } as never,
      },
    );
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql`SELECT event_id, status, validity, settlement_event_info FROM withdrawal_utxos ORDER BY event_id`;
    return { produced, rows };
  });

type Produced = Effect.Effect.Success<ReturnType<typeof build>>["produced"];

/** The DA payload of the produced block, committing its post-state root. */
const payloadOf = async (produced: Produced, baseRoot: string) => {
  const entry = <K extends { readonly keyCbor: Buffer }>(
    member: K & { readonly valueCbor: Buffer },
  ): SDK.DaPayloadEntry => [hex(member.keyCbor), hex(member.valueCbor)];
  const withdrawals = produced.includedWithdrawalEntries.map(
    (row): SDK.DaPayloadEntry => [
      hex(row[WithdrawalsDB.Columns.ID]),
      hex(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]!),
    ],
  );
  const header = {
    ...headerFor(
      {} as Parameters<typeof headerFor>[0],
      countsFromLengths({ withdrawals: withdrawals.length }),
    ),
    prevUtxosRoot: baseRoot,
    utxosRoot: produced.utxoRoot,
  };
  return rebind({
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header,
      deposits: [],
      withdrawals,
      transactions: [],
      transaction_preimages: [],
      cek_program_material: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      transition_trace: produced.transitionTraceMembers.map(entry),
      event_to_step: produced.eventToStepMembers.map(entry),
      validation_traces: [],
      validation_trace_witnesses: [],
      counts: countsFromLengths({ withdrawals: withdrawals.length }),
    },
  });
};

/** Replays `payload` with the foreign replayer's own withdrawal checks. */
const replay = (
  payload: SDK.DaPayload,
  orders: readonly WithdrawalsDB.Entry[],
  baseRoot: string,
) => {
  const memberships: WithdrawalMembership[] = [];
  const known = new Map(
    orders.map((row) => [
      hex(row[WithdrawalsDB.Columns.ID]),
      {
        l2Outref: hex(row[WithdrawalsDB.Columns.L2_OUTREF]),
        l2Owner: hex(row[WithdrawalsDB.Columns.L2_OWNER]),
        l2Value: hex(row[WithdrawalsDB.Columns.L2_VALUE]),
        info: hex(row[WithdrawalsDB.Columns.RAW_EVENT_INFO]),
      },
    ]),
  );
  return Effect.runPromise(
    Effect.either(
      verifyAndImportBlock({
        ...foreignReplayMaterial(new Map(), known, new Map(), memberships),
        header: payload.block_body.header,
        headerHash: payload.block_body.header_hash,
        parentHeaderHash: payload.block_body.header.prevHeaderHash,
        parentUtxosRoot: baseRoot,
        parentEntries: BASE,
        payload,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 0n,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      }),
    ),
  ).then((result) => ({ result, memberships }));
};

describe("duplicate-outref withdrawals in one block window", () => {
  it("builds the block, drains the outref once and replays to the same bytes", async () => {
    const orders = [await order(0x01), await order(0x02)];
    base.entries = BASE.map(ledgerEntryToInsertBatchOp);
    const baseRoot = await Effect.runPromise(
      computeLedgerMpfRootFromLedgerEntries(BASE),
    );
    const { produced, rows } = await run(build(orders, baseRoot));

    // The first order drains U; the second finds it gone.
    expect(
      produced.includedWithdrawalEntries.map((row) => [
        hex(row[WithdrawalsDB.Columns.ID]),
        row[WithdrawalsDB.Columns.VALIDITY],
      ]),
    ).toEqual([
      [hex(orders[0]![WithdrawalsDB.Columns.ID]), "WithdrawalIsValid"],
      [hex(orders[1]![WithdrawalsDB.Columns.ID]), "NonExistentWithdrawalUtxo"],
    ]);
    expect(rows.map((row) => [row.validity, row.status])).toEqual([
      ["WithdrawalIsValid", WithdrawalsDB.Status.Projected],
      ["NonExistentWithdrawalUtxo", WithdrawalsDB.Status.Projected],
    ]);
    expect(rows.map((row) => hex(row.settlement_event_info as Buffer))).toEqual(
      produced.includedWithdrawalEntries.map((row) =>
        hex(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]!),
      ),
    );
    // Exactly one delete, of U; step 1 removes it and step 2 is a no-op.
    expect(produced.ledgerDelta.spent.map(hex)).toEqual([
      hex(U[LedgerUtils.Columns.OUTREF]),
    ]);
    expect(produced.ledgerDelta.produced).toEqual([]);
    const [first, second] = produced.transitionTraceMembers.map(
      (member) => member.value,
    );
    expect(produced.transitionTraceMembers).toHaveLength(2);
    expect(first!.pre_utxos_root).toBe(baseRoot);
    expect(first!.post_utxos_root).not.toBe(first!.pre_utxos_root);
    expect(second!.pre_utxos_root).toBe(first!.post_utxos_root);
    expect(second!.post_utxos_root).toBe(second!.pre_utxos_root);
    expect(second!.post_utxos_root).toBe(produced.utxoRoot);

    // Foreign replay derives the same verdicts, transitions and root.
    const honest = await payloadOf(produced, baseRoot);
    expect(honest.block_body.header.utxosRoot).toBe(produced.utxoRoot);
    const accepted = await replay(honest, orders, baseRoot);
    expect(accepted.result._tag).toBe("Right");
    expect(
      accepted.memberships.map(({ id, validity }) => [id, validity]),
    ).toEqual([
      [hex(orders[0]![WithdrawalsDB.Columns.ID]), "WithdrawalIsValid"],
      [hex(orders[1]![WithdrawalsDB.Columns.ID]), "NonExistentWithdrawalUtxo"],
    ]);

    // Tagging the duplicate payable instead is refused by the same replay.
    const info = Data.from(
      hex(orders[1]![WithdrawalsDB.Columns.RAW_EVENT_INFO]),
      SDK.WithdrawalInfo,
    ) as SDK.WithdrawalInfo;
    const payableDuplicate = SDK.committedWithdrawalValueBytes({
      ...info,
      validity: "WithdrawalIsValid",
    });
    const tampered = await rebind({
      ...honest,
      block_body: {
        ...honest.block_body,
        withdrawals: honest.block_body.withdrawals.map(([key, value]) => [
          key,
          key === hex(orders[1]![WithdrawalsDB.Columns.ID])
            ? payableDuplicate
            : value,
        ]),
      },
    });
    const refused = await replay(tampered, orders, baseRoot);
    expect(refused.result).toMatchObject({
      _tag: "Left",
      left: {
        kind: "invalid",
        detail: "a withdrawal's verdict differs from its replay",
      },
    });
  }, 120_000);
});
