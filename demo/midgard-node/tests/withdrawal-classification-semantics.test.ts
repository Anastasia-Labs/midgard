import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { classifyWithdrawal } from "../src/workers/utils/mpf/withdrawal-classification.js";

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const owner = privateKey.to_public().hash().to_hex();
const policy = "ab".repeat(28);

const fixture = async (
  options: {
    readonly missing?: boolean;
    readonly wrongOwner?: boolean;
    readonly wrongValue?: boolean;
    readonly badSignature?: boolean;
    readonly tokenCount?: number;
    readonly malformedValue?: boolean;
    readonly malformedEvent?: boolean;
    readonly malformedLedger?: boolean;
  } = {},
) => {
  const tokens = new Map(
    Array.from(
      { length: options.tokenCount ?? 0 },
      (_, index) => [index.toString(16).padStart(4, "0"), 1n] as const,
    ),
  );
  const assets = new Map(tokens.size === 0 ? [] : [[policy, tokens]]);
  const lovelace = 7_000_000n;
  const value: SDK.Value = new Map([
    ["", new Map([["", options.wrongValue ? lovelace + 1n : lovelace]])],
    ...assets,
  ]);
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
    l2_owner: options.wrongOwner ? "bb".repeat(28) : owner,
    l2_value: value,
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(wallet.address),
    ),
    l1_datum: "NoDatum",
  };
  const info: SDK.WithdrawalInfo = {
    body,
    signature: options.badSignature
      ? [
          Buffer.from(privateKey.to_public().to_raw_bytes()).toString("hex"),
          "00".repeat(64),
        ]
      : SDK.signWithdrawalBody(privateKey, body),
    validity: "WithdrawalIsValid",
  };
  const eventInfo = Buffer.from(Data.to(info, SDK.WithdrawalInfo), "hex");
  const entry: WithdrawalsDB.Entry = {
    [WithdrawalsDB.Columns.ID]: Buffer.from("cc".repeat(32), "hex"),
    raw_event_info: options.malformedEvent
      ? Buffer.from("00", "hex")
      : eventInfo,
    settlement_event_info: null,
    inclusion_time: new Date(0),
    withdrawal_l1_tx_hash: Buffer.from("dd".repeat(32), "hex"),
    withdrawal_l1_output_index: 0,
    asset_name: Buffer.from("ee".repeat(32), "hex"),
    l2_outref: Buffer.from(Data.to(body.l2_outref, SDK.OutputReference), "hex"),
    l2_owner: Buffer.from(body.l2_owner, "hex"),
    l2_value: options.malformedValue
      ? Buffer.from("00", "hex")
      : Buffer.from(Data.to(value, SDK.Value), "hex"),
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
  const ledgerOutRef = encodeMidgardSpendInputItem({
    txId: Buffer.from(body.l2_outref.transactionId, "hex"),
    outputIndex: 0,
  });
  const output = options.malformedLedger
    ? Buffer.from("00", "hex")
    : encodeMidgardTxOutput({
        address: midgardAddressFromText(wallet.address),
        value: { lovelace, assets },
      });
  return {
    input: {
      entry,
      ledgerOutRef,
      ledgerOutput: options.missing
        ? Option.none<Buffer>()
        : Option.some(output),
    },
    info,
  };
};

describe("withdrawal classification semantics", () => {
  const scenarios = [
    { name: "valid withdrawal", options: {}, validity: "WithdrawalIsValid" },
    {
      name: "absent selected-base output",
      options: { missing: true },
      validity: "NonExistentWithdrawalUtxo",
    },
    {
      name: "wrong owner",
      options: { wrongOwner: true },
      validity: "IncorrectWithdrawalOwner",
    },
    {
      name: "wrong value",
      options: { wrongValue: true },
      validity: "IncorrectWithdrawalValue",
    },
    {
      name: "invalid signature",
      options: { badSignature: true },
      validity: "IncorrectWithdrawalSignature",
    },
    {
      name: "100 asset entries including lovelace",
      options: { tokenCount: 99 },
      validity: "WithdrawalIsValid",
    },
    {
      name: "101 asset entries including lovelace",
      options: { tokenCount: 100 },
      validity: "TooManyTokensInWithdrawal",
    },
    {
      name: "missing output precedes owner, value and signature",
      options: {
        missing: true,
        wrongOwner: true,
        wrongValue: true,
        badSignature: true,
      },
      validity: "NonExistentWithdrawalUtxo",
    },
    {
      name: "owner precedes value and signature",
      options: { wrongOwner: true, wrongValue: true, badSignature: true },
      validity: "IncorrectWithdrawalOwner",
    },
    {
      name: "value precedes token count and signature",
      options: { wrongValue: true, tokenCount: 100, badSignature: true },
      validity: "IncorrectWithdrawalValue",
    },
    {
      name: "token count precedes signature",
      options: { tokenCount: 100, badSignature: true },
      validity: "TooManyTokensInWithdrawal",
    },
    {
      name: "missing output avoids projected-value decoding",
      options: { missing: true, malformedValue: true },
      validity: "NonExistentWithdrawalUtxo",
    },
    {
      name: "owner mismatch avoids projected-value decoding",
      options: { wrongOwner: true, malformedValue: true },
      validity: "IncorrectWithdrawalOwner",
    },
  ] as const;

  it.each(scenarios)("$name", async ({ options, validity }) => {
    const { input, info } = await fixture(options);
    const result = await Effect.runPromise(classifyWithdrawal(input));
    expect(result.entry).toBe(input.entry);
    expect(result.ledgerOutRef).toBe(input.ledgerOutRef);
    expect(result.validity).toBe(validity);
    expect(result.shouldDeleteLedgerUtxo).toBe(
      validity === "WithdrawalIsValid",
    );
    expect(result.settlementEventInfo.toString("hex")).toBe(
      SDK.committedWithdrawalValueBytes({ ...info, validity }),
    );
    if (validity === "IncorrectWithdrawalValue") {
      expect(result.validityDetail).toMatchObject({
        requested_value_cbor: input.entry.l2_value.toString("hex"),
        actual_assets: { lovelace: "7000000" },
      });
    } else if (validity === "IncorrectWithdrawalSignature") {
      expect(result.validityDetail).toEqual({
        reason: "invalid_signature",
        public_key_hash: owner,
      });
    } else {
      expect(result.validityDetail).toEqual({});
    }
  });

  it.each([
    {
      name: "malformed ledger",
      options: { malformedLedger: true, malformedEvent: true },
      message: "Failed to decode ledger UTxO for withdrawal classification",
    },
    {
      name: "malformed projected value",
      options: { malformedValue: true, malformedEvent: true },
      message: "Failed to decode withdrawal l2_value",
    },
    {
      name: "malformed originating event even when output is absent",
      options: { malformedEvent: true, missing: true },
      message: "Failed to decode withdrawal event info",
    },
  ])("preserves the error for $name", async ({ options, message }) => {
    const { input } = await fixture(options);
    const result = await Effect.runPromise(
      Effect.either(classifyWithdrawal(input)),
    );
    expect(result._tag).toBe("Left");
    if (result._tag === "Right")
      throw new Error("malformed input was classified");
    expect(result.left).toMatchObject({
      _tag: "DatabaseError",
      table: WithdrawalsDB.tableName,
      message,
    });
  });
});
