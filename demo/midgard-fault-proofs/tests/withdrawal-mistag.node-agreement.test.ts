/** The node's withdrawal classification agrees with the chain's withdrawal
 * value rule.
 *
 * Each case is a ledger output and a requested `l2_value` (raw CBOR, in the
 * order admission accepted it). The node judges it with
 * `classifyWithdrawalFromLedger`; the chain judges the same bytes with
 * withdrawal-mistag step-03's `output_value_matches_v1` in
 * `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-03.node-agreement.test.ak`.
 * Both read `fixtures/withdrawal-mistag-agreement-vectors.json` (`valueRule`):
 * the requested value, the output's ledger descriptor facts and the chain's
 * verdict, rendered into the Aiken tests by
 * `scripts/generate-withdrawal-mistag-agreement-goldens.mjs`. This test checks
 * that the vectors are this table's (same bytes, same descriptor facts) and
 * that the node's verdict equals the pinned one.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import VECTORS from "./fixtures/withdrawal-mistag-agreement-vectors.json" with { type: "json" };

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
const owner = key.to_public().hash().to_hex();

const P = "0a".repeat(28);
const Q = "ab".repeat(28);
const ADA = 7_000_000n;
const LARGE_ADA = 50_000_000n;

// Minimal definite-length CBOR, so a requested value can hold its entries in
// an order the SDK's encoder never writes.
const head = (major: number, length: number): string => {
  const base = major << 5;
  if (length < 24) return (base | length).toString(16).padStart(2, "0");
  if (length < 0x100)
    return `${(base | 24).toString(16)}${length.toString(16).padStart(2, "0")}`;
  return `${(base | 25).toString(16)}${length.toString(16).padStart(4, "0")}`;
};
const uint = (value: bigint): string =>
  value < 0x1_0000_0000n
    ? value < 0x1_0000n
      ? head(0, Number(value))
      : `1a${value.toString(16).padStart(8, "0")}`
    : `1b${value.toString(16).padStart(16, "0")}`;
const bytes = (hex: string): string => `${head(2, hex.length / 2)}${hex}`;
/** A value in exactly the given entry order. */
const rawValue = (
  entries: readonly (readonly [
    string,
    readonly (readonly [string, bigint])[],
  ])[],
): string =>
  `${head(5, entries.length)}${entries
    .map(
      ([policy, names]) =>
        `${bytes(policy)}${head(5, names.length)}${names
          .map(([name, quantity]) => `${bytes(name)}${uint(quantity)}`)
          .join("")}`,
    )
    .join("")}`;
const encoded = (value: SDK.Value): string => Data.to(value, SDK.Value);

type Assets = Map<string, Map<string, bigint>>;
/** `count` distinct one-unit assets under `P`. */
const tokens = (count: number): Assets =>
  new Map([
    [
      P,
      new Map(
        Array.from(
          { length: count },
          (_, index) => [index.toString(16).padStart(4, "0"), 1n] as const,
        ),
      ),
    ],
  ]);
const exactly = (lovelace: bigint, assets: Assets): string =>
  encoded(new Map([["", new Map([["", lovelace]])], ...assets]));

type Case = Readonly<{
  name: string;
  lovelace: bigint;
  assets: Assets;
  l2Value: string;
}>;

const mixed: Assets = new Map([
  [
    P,
    new Map([
      ["0000", 2n],
      ["ff", 1n],
    ]),
  ],
]);
const twoPolicies: Assets = new Map([
  [P, new Map([["01", 1n]])],
  [Q, new Map([["ff", 1n]])],
]);

const CASES: readonly Case[] = [
  {
    name: "ada_only",
    lovelace: ADA,
    assets: new Map(),
    l2Value: exactly(ADA, new Map()),
  },
  {
    name: "wrong_lovelace",
    lovelace: ADA,
    assets: new Map(),
    l2Value: exactly(ADA - 1n, new Map()),
  },
  {
    name: "policies_out_of_order",
    lovelace: ADA,
    assets: twoPolicies,
    l2Value: rawValue([
      [Q, [["ff", 1n]]],
      ["", [["", ADA]]],
      [P, [["01", 1n]]],
    ]),
  },
  {
    // Names given in descending order; the SDK writes them in byte order.
    name: "mixed_width_names",
    lovelace: ADA,
    assets: mixed,
    l2Value: encoded(
      SDK.assetsToValue({ lovelace: ADA, [`${P}ff`]: 1n, [`${P}0000`]: 2n }),
    ),
  },
  {
    name: "changed_quantity",
    lovelace: ADA,
    assets: mixed,
    l2Value: rawValue([
      ["", [["", ADA]]],
      [
        P,
        [
          ["0000", 2n],
          ["ff", 2n],
        ],
      ],
    ]),
  },
  {
    name: "missing_asset",
    lovelace: ADA,
    assets: mixed,
    l2Value: rawValue([
      ["", [["", ADA]]],
      [P, [["ff", 1n]]],
    ]),
  },
  {
    name: "extra_requested_asset",
    lovelace: ADA,
    assets: new Map(),
    l2Value: rawValue([
      ["", [["", ADA]]],
      [P, [["ff", 1n]]],
    ]),
  },
  {
    name: "second_ada_name",
    lovelace: ADA,
    assets: new Map(),
    l2Value: rawValue([
      [
        "",
        [
          ["", ADA],
          ["01", 1n],
        ],
      ],
    ]),
  },
  {
    name: "short_policy",
    lovelace: ADA,
    assets: new Map(),
    l2Value: rawValue([
      ["", [["", ADA]]],
      ["0a".repeat(27), [["ff", 1n]]],
    ]),
  },
  {
    name: "long_asset_name",
    lovelace: ADA,
    assets: new Map(),
    l2Value: rawValue([
      ["", [["", ADA]]],
      [P, [["cc".repeat(33), 1n]]],
    ]),
  },
  {
    name: "hundred_assets",
    lovelace: LARGE_ADA,
    assets: tokens(SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT),
    l2Value: exactly(
      LARGE_ADA,
      tokens(SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT),
    ),
  },
  {
    name: "hundred_and_one_assets",
    lovelace: LARGE_ADA,
    assets: tokens(SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT + 1),
    l2Value: exactly(
      LARGE_ADA,
      tokens(SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT + 1),
    ),
  },
];

const OUTREF = { transactionId: "ab".repeat(32), outputIndex: 0n };

const outputOf = (testCase: Case) =>
  encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: { lovelace: testCase.lovelace, assets: testCase.assets },
  });

/** The descriptor facts step-03 judges the output by, as the Aiken vector
 * states them. */
const chainVector = (testCase: Case) => {
  const { descriptor } = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: outputOf(testCase),
  });
  return {
    l2Value: testCase.l2Value,
    lovelace: descriptor.lovelace,
    assetCount: BigInt(descriptor.assetCount),
    frontier: Buffer.from(descriptor.assetFrontierCommitment).toString("hex"),
  };
};

/** The node's verdict on a withdrawal signed by the output's owner. */
const nodeValid = async (testCase: Case): Promise<boolean> => {
  const body: SDK.WithdrawalBody = {
    l2_outref: OUTREF,
    l2_owner: owner,
    l2_value: new Map(),
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(wallet.address),
    ),
    l1_datum: "NoDatum",
  };
  const bodyCbor = replacePlutusConstrFieldCbor(
    Data.to(body, SDK.WithdrawalBody),
    [2],
    testCase.l2Value,
  );
  const infoCbor = replacePlutusConstrFieldCbor(
    Data.to(
      {
        body,
        signature: SDK.signWithdrawalBodyCbor(key, bodyCbor),
        validity: "WithdrawalIsValid",
      },
      SDK.WithdrawalInfo,
    ),
    [0],
    bodyCbor,
  );
  const result = await Effect.runPromise(
    SDK.classifyWithdrawalFromLedger({
      l2Owner: owner,
      l2ValueCbor: testCase.l2Value,
      eventInfoCbor: infoCbor,
      ledgerOutRef: encodeMidgardSpendInputItem({
        txId: Buffer.from(OUTREF.transactionId, "hex"),
        outputIndex: 0,
      }),
      ledgerOutput: outputOf(testCase),
    }),
  );
  return result.validity === "WithdrawalIsValid";
};

const aikenVectors = () =>
  new Map(
    VECTORS.valueRule.map((vector) => [
      vector.name,
      {
        vector: {
          l2Value: vector.l2Value,
          lovelace: BigInt(vector.outputLovelace),
          assetCount: BigInt(vector.outputAssetCount),
          frontier: vector.outputAssetFrontierCommitment,
        },
        chainValid: vector.chainValid,
      },
    ]),
  );

describe("withdrawal value: node classification agrees with step-03", () => {
  it("covers every case and nothing else in the Aiken vectors", async () => {
    expect([...aikenVectors().keys()].sort()).toEqual(
      CASES.map(({ name }) => name).sort(),
    );
  });

  it.each(CASES)("$name", async (testCase) => {
    const chain = aikenVectors().get(testCase.name);
    expect(chain?.vector).toEqual(chainVector(testCase));
    expect(await nodeValid(testCase)).toBe(chain?.chainValid);
  });
});
