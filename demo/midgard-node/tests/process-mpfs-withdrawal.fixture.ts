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

import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { foreignReplayMaterial } from "../src/landed-blocks/replay-foreign.js";
import type { WithdrawalMembership } from "../src/landed-blocks/store.js";
import { type MidgardMpf, processMpfs } from "../src/mpf/index.js";
import type { ProcessMpfsConfig } from "../src/mpf/process-config.js";
import { verifyAndImportBlock } from "../src/mpf/verified-block-import.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { insertWithdrawals } from "./helpers/event-rows.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * Withdrawal orders built, signed and replayed against a two-entry base
 * ledger. A test using {@link build} replaces the native transition-trace
 * owner (see `process-mpfs-duplicate-withdrawal.test.ts`).
 */

export const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
export const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const T0 = Date.parse("2026-01-01T00:00:00.000Z");
export const at = (seconds: number) => new Date(T0 + seconds * 1_000);
export const hex = (buffer: Buffer) => buffer.toString("hex");
export const LOVELACE = 7_000_000n;
export const ledgerEntry = (byte: number): LedgerUtils.Entry => ({
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
export const U = ledgerEntry(0xaa);
export const V = ledgerEntry(0x11);
export const BASE = [U, V];

export const order = async (
  idByte: number,
  signature?: SDK.WithdrawalInfo["signature"],
): Promise<WithdrawalsDB.Entry> => {
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
    signature: signature ?? SDK.signWithdrawalBody(privateKey, body),
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

export const run = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

export const build = (
  orders: readonly WithdrawalsDB.Entry[],
  baseRoot: string,
  onCommitBuildAlarm?: ProcessMpfsConfig["onCommitBuildAlarm"],
) =>
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
        onCommitBuildAlarm,
      },
    );
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql`SELECT event_id, status, validity, settlement_event_info FROM withdrawal_utxos ORDER BY event_id`;
    return { produced, rows };
  });

export type Produced = Effect.Effect.Success<
  ReturnType<typeof build>
>["produced"];

/** The DA payload of the produced block, committing its post-state root. */
export const payloadOf = async (produced: Produced, baseRoot: string) => {
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
export const replay = (
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
        // The clock the block above was built at.
        blockTimeMs: at(10).getTime(),
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      }),
    ),
  ).then((result) => ({ result, memberships }));
};
