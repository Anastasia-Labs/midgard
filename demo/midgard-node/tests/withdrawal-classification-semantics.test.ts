import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import * as WithdrawalsDB from "../src/database/withdrawals.js";
import {
  classifyWithdrawal,
  classifyWithdrawalsInOrder,
} from "../src/workers/utils/mpf/withdrawal-classification.js";

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const owner = privateKey.to_public().hash().to_hex();
const policy = "ab".repeat(28);
const lowPolicy = "0a".repeat(28);
const lovelace = 7_000_000n;

type Assets = ReadonlyMap<string, ReadonlyMap<string, bigint>>;

/** One policy holding "ff" and "0000", given in descending order: both the L2
 * codec and the Plutus Data Value write "0000" first. */
const mixedWidthAssets: Assets = new Map([
  [
    policy,
    new Map([
      ["ff", 1n],
      ["0000", 2n],
    ]),
  ],
]);
const twoPolicyAssets: Assets = new Map([
  [lowPolicy, new Map([["01", 3n]])],
  [policy, new Map([["02", 4n]])],
]);
/** `twoPolicyAssets` with 7 ADA, its policies in the order high, ADA, low: an
 * order admission accepts (`from_asset_list` sorts policies) and the SDK's
 * encoder never writes. */
const twoPoliciesOutOfOrderCbor = [
  "a3",
  `581c${policy}a1410204`,
  "40a1401a006acfc0",
  `581c${lowPolicy}a1410103`,
].join("");
/** An inline l1_datum of 1,500 bytes (in Plutus Data's 64-byte chunks): the
 * L1 payout output it makes needs more than the 7 ADA withdrawn. */
const longInlineDatumCbor = `d87b9f5f${[
  ...Array.from({ length: 23 }, () => `5840${"cd".repeat(64)}`),
  `581c${"cd".repeat(28)}`,
].join("")}ffff`;
const ada = (quantity: bigint) =>
  ["", new Map([["", quantity]])] as [string, Map<string, bigint>];

const fixture = async (
  options: {
    readonly missing?: boolean;
    readonly wrongOwner?: boolean;
    readonly wrongValue?: boolean;
    readonly badSignature?: boolean;
    readonly tokenCount?: number;
    /** The ledger output's non-ADA assets (default: `tokenCount` names). */
    readonly assets?: Assets;
    /** The withdrawal's requested l2_value (default: the output's value). */
    readonly requested?: SDK.Value;
    /** The requested l2_value as these exact bytes, in the signed body. */
    readonly requestedCbor?: string;
    /** The l1_datum as these exact bytes, in the signed body. */
    readonly l1DatumCbor?: string;
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
  const assets =
    options.assets ?? new Map(tokens.size === 0 ? [] : [[policy, tokens]]);
  const value: SDK.Value =
    options.requested ??
    new Map([
      ada(options.wrongValue ? lovelace + 1n : lovelace),
      ...[...assets].map(([id, names]) => [id, new Map(names)] as const),
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
  const bodyCbor =
    options.requestedCbor === undefined && options.l1DatumCbor === undefined
      ? undefined
      : [
          [2, options.requestedCbor],
          [4, options.l1DatumCbor],
        ].reduce(
          (cbor, [field, replacement]) =>
            replacement === undefined
              ? cbor
              : replacePlutusConstrFieldCbor(
                  cbor,
                  [field as number],
                  replacement as string,
                ),
          Data.to(body, SDK.WithdrawalBody),
        );
  const info: SDK.WithdrawalInfo = {
    body,
    signature: options.badSignature
      ? [
          Buffer.from(privateKey.to_public().to_raw_bytes()).toString("hex"),
          "00".repeat(64),
        ]
      : bodyCbor === undefined
        ? SDK.signWithdrawalBody(privateKey, body)
        : SDK.signWithdrawalBodyCbor(privateKey, bodyCbor),
    validity: "WithdrawalIsValid",
  };
  const infoCbor =
    bodyCbor === undefined
      ? Data.to(info, SDK.WithdrawalInfo)
      : replacePlutusConstrFieldCbor(
          Data.to(info, SDK.WithdrawalInfo),
          [0],
          bodyCbor,
        );
  const eventInfo = Buffer.from(infoCbor, "hex");
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
      : Buffer.from(options.requestedCbor ?? Data.to(value, SDK.Value), "hex"),
    l1_address: Buffer.from(Data.to(body.l1_address, SDK.AddressData), "hex"),
    l1_datum: Buffer.from(
      options.l1DatumCbor ?? Data.to(body.l1_datum, SDK.CardanoDatum),
      "hex",
    ),
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
    /** The committed settlement bytes for a verdict, from the exact event. */
    settlement: (validity: SDK.WithdrawalValidity) =>
      bodyCbor === undefined
        ? SDK.committedWithdrawalValueBytes({ ...info, validity })
        : aikenSerialisedPlutusDataCborPreservingMapOrder(
            replacePlutusConstrFieldCbor(
              infoCbor,
              [2],
              Data.to(validity, SDK.WithdrawalValidity),
            ),
          ),
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
      // The chain counts non-ADA assets only (descriptor asset_count).
      name: "100 non-ADA assets besides lovelace",
      options: { tokenCount: 100 },
      validity: "WithdrawalIsValid",
    },
    {
      // Step-04: the L1 payout output must meet its min-ADA.
      name: "an inline datum whose payout output needs more ADA",
      options: { l1DatumCbor: longInlineDatumCbor },
      validity: "UnpayableWithdrawalValue",
    },
    {
      name: "101 non-ADA assets besides lovelace",
      options: { tokenCount: 101 },
      validity: "TooManyTokensInWithdrawal",
    },
    {
      // Admission accepts policies in any order (from_asset_list sorts).
      name: "policies requested out of byte order",
      options: {
        assets: twoPolicyAssets,
        requestedCbor: twoPoliciesOutOfOrderCbor,
      },
      validity: "WithdrawalIsValid",
    },
    {
      name: "policies out of byte order with a changed quantity",
      options: {
        assets: new Map([
          [lowPolicy, new Map([["01", 3n]])],
          [policy, new Map([["02", 5n]])],
        ]),
        requestedCbor: twoPoliciesOutOfOrderCbor,
      },
      validity: "IncorrectWithdrawalValue",
    },
    {
      name: "mixed-width asset names in byte order, as the SDK writes them",
      options: {
        assets: mixedWidthAssets,
        requested: SDK.assetsToValue({
          lovelace,
          [`${policy}ff`]: 1n,
          [`${policy}0000`]: 2n,
        }),
      },
      validity: "WithdrawalIsValid",
    },
    {
      name: "mixed-width asset names with a changed quantity",
      options: {
        assets: mixedWidthAssets,
        requested: SDK.assetsToValue({
          lovelace,
          [`${policy}ff`]: 1n,
          [`${policy}0000`]: 3n,
        }),
      },
      validity: "IncorrectWithdrawalValue",
    },
    {
      name: "a requested asset the output lacks",
      options: {
        assets: mixedWidthAssets,
        requested: SDK.assetsToValue({
          lovelace,
          [`${policy}ff`]: 1n,
          [`${policy}0000`]: 2n,
          [`${policy}01`]: 1n,
        }),
      },
      validity: "IncorrectWithdrawalValue",
    },
    {
      name: "an output asset the request lacks",
      options: {
        assets: mixedWidthAssets,
        requested: SDK.assetsToValue({ lovelace, [`${policy}ff`]: 1n }),
      },
      validity: "IncorrectWithdrawalValue",
    },
    {
      // The chain requires the ADA policy to hold only the empty name.
      name: "an extra asset name under the ADA policy",
      options: {
        requested: new Map([
          [
            "",
            new Map([
              ["", lovelace],
              ["01", 1n],
            ]),
          ],
        ]),
      },
      validity: "IncorrectWithdrawalValue",
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
      options: { wrongValue: true, tokenCount: 101, badSignature: true },
      validity: "IncorrectWithdrawalValue",
    },
    {
      name: "token count precedes signature",
      options: { tokenCount: 101, badSignature: true },
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
    const { input, settlement } = await fixture(options);
    const result = await Effect.runPromise(classifyWithdrawal(input));
    expect(result.entry).toBe(input.entry);
    expect(result.ledgerOutRef).toBe(input.ledgerOutRef);
    expect(result.validity).toBe(validity);
    expect(result.shouldDeleteLedgerUtxo).toBe(
      validity === "WithdrawalIsValid",
    );
    expect(result.settlementEventInfo.toString("hex")).toBe(
      settlement(validity),
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
    } else if (validity === "UnpayableWithdrawalValue") {
      expect(result.validityDetail).toEqual({
        cardano_value_size: "5",
        lovelace: "7000000",
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

describe("withdrawal classification in trace order", () => {
  type Options = Parameters<typeof fixture>[0];
  // Each order gets its own event id; all name the same L2 outref.
  const orders = async (...options: Options[]) => {
    const built = await Promise.all(options.map((option) => fixture(option)));
    const entries = built.map(({ input }, index) => ({
      ...input.entry,
      [WithdrawalsDB.Columns.ID]: Buffer.alloc(32, index + 1),
    }));
    const { ledgerOutRef, ledgerOutput } = built[0]!.input;
    const baseOutputs = new Map(
      Option.isSome(ledgerOutput)
        ? [[ledgerOutRef.toString("hex"), ledgerOutput.value]]
        : [],
    );
    return { entries, baseOutputs };
  };
  const verdicts = async (...options: Options[]) => {
    const { entries, baseOutputs } = await orders(...options);
    const classified = await Effect.runPromise(
      classifyWithdrawalsInOrder({ entries, baseOutputs }),
    );
    expect(classified.map(({ entry }) => entry)).toEqual(entries);
    for (const { validity, shouldDeleteLedgerUtxo } of classified)
      expect(shouldDeleteLedgerUtxo).toBe(validity === "WithdrawalIsValid");
    return classified.map(({ validity }) => validity);
  };

  it("finds the outref gone for a second valid order on it", async () => {
    expect(await verdicts({}, {})).toEqual([
      "WithdrawalIsValid",
      "NonExistentWithdrawalUtxo",
    ]);
  });

  it("lets a valid order follow a non-payable one on the same outref", async () => {
    expect(await verdicts({ badSignature: true }, {})).toEqual([
      "IncorrectWithdrawalSignature",
      "WithdrawalIsValid",
    ]);
  });

  it("leaves the outref for a valid order after an unpayable one", async () => {
    expect(
      await verdicts({ l1DatumCbor: longInlineDatumCbor }, {}, {}),
    ).toEqual([
      "UnpayableWithdrawalValue",
      "WithdrawalIsValid",
      "NonExistentWithdrawalUtxo",
    ]);
  });

  it("finds a missing outref missing for every order", async () => {
    expect(await verdicts({ missing: true }, { missing: true })).toEqual([
      "NonExistentWithdrawalUtxo",
      "NonExistentWithdrawalUtxo",
    ]);
  });

  it("drains the outref once across three orders", async () => {
    expect(await verdicts({ wrongValue: true }, {}, {})).toEqual([
      "IncorrectWithdrawalValue",
      "WithdrawalIsValid",
      "NonExistentWithdrawalUtxo",
    ]);
  });
});
