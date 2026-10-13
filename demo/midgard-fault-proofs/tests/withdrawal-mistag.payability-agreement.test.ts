/** The node's withdrawal payability verdict agrees with the chain's.
 *
 * Each case is a withdrawal body (raw CBOR, as the order carries it) and the
 * ledger output it withdraws. The chain judges payability with
 * withdrawal-mistag step-04's `payout_is_exactly_payable_v1` in
 * `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-04.node-agreement.test.ak`,
 * whose `agrees_<case>` tests pin the body bytes, the output's Cardano value
 * size and the chain's verdict (or that step-04 refuses the body outright).
 * This test checks that the Aiken vectors are this table's, that the SDK's
 * `withdrawalMistagPayable` returns the chain's verdict, and that the node's
 * `classifyWithdrawalFromLedger` tags the withdrawal valid exactly when the
 * chain judges it payable.
 */
import { readFile } from "node:fs/promises";

import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
  midgardCardanoValueCborLength,
} from "@al-ft/midgard-core/codec";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

const AIKEN_VECTORS = new URL(
  "../../../onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-04.node-agreement.test.ak",
  import.meta.url,
);

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
const owner = key.to_public().hash().to_hex();

const P = "0a".repeat(28);
const STAKE = "22".repeat(28);

const head = (major: number, length: number): string => {
  const base = major << 5;
  if (length < 24) return (base | length).toString(16).padStart(2, "0");
  if (length < 0x100)
    return `${(base | 24).toString(16)}${length.toString(16).padStart(2, "0")}`;
  return `${(base | 25).toString(16)}${length.toString(16).padStart(4, "0")}`;
};
const bytes = (hex: string): string => `${head(2, hex.length / 2)}${hex}`;

/** Field 4 of the body, a Cardano datum option, as raw Plutus Data CBOR. */
const NO_DATUM = "d87980";
const datumHash = (hash: string): string => `d87a9f${bytes(hash)}ff`;
const inlineDatum = (data: string): string => `d87b9f${data}ff`;

type Assets = Map<string, Map<string, bigint>>;
type L1Address = SDK.WithdrawalBody["l1_address"];

const enterprise: L1Address = {
  paymentCredential: { PublicKeyCredential: [owner] },
  stakeCredential: null,
};
const inlineStaked: L1Address = {
  paymentCredential: { PublicKeyCredential: [owner] },
  stakeCredential: { Inline: [{ PublicKeyCredential: [STAKE] }] },
};
const pointer: L1Address = {
  paymentCredential: { PublicKeyCredential: [owner] },
  stakeCredential: {
    Pointer: {
      slotNumber: 2n ** 40n,
      transactionIndex: 16_384n,
      certificateIndex: 0n,
    },
  },
};

const oneToken: Assets = new Map([[P, new Map([["ff", 1n]])]]);
/** One asset under each of `policies` policies, 32-byte names except the
 * last, whose name has `lastNameBytes` bytes. */
const wideValue = (policies: number, lastNameBytes: number): Assets =>
  new Map(
    Array.from({ length: policies }, (_, index) => [
      index.toString(16).padStart(56, "0"),
      new Map([
        [
          (index + 1)
            .toString(16)
            .padStart(2, "0")
            .repeat(index === policies - 1 ? lastNameBytes : 32),
          1n,
        ],
      ]),
    ]),
  );

/** A 300-byte inline bytestring, in the 64-byte chunks Plutus Data uses. */
const LONG_BYTES = `5f${[64, 64, 64, 64, 44]
  .map((length) => bytes("cd".repeat(length)))
  .join("")}ff`;
/** A definite list `[1, 2]`; `cbor.serialise` writes lists indefinite. */
const DEFINITE_LIST = "820102";
/** A map whose key repeats; a decoded Data map would keep one entry. */
const REPEATED_KEY_MAP = "a201020103";

type Case = Readonly<{
  name: string;
  lovelace: bigint;
  assets: Assets;
  address: L1Address;
  datum: string;
}>;

const CASES: readonly Case[] = [
  {
    name: "ada_exact_minimum",
    lovelace: 857_690n,
    assets: new Map(),
    address: enterprise,
    datum: NO_DATUM,
  },
  {
    name: "ada_one_below_minimum",
    lovelace: 857_689n,
    assets: new Map(),
    address: enterprise,
    datum: NO_DATUM,
  },
  {
    name: "token_exact_minimum",
    lovelace: 1_012_850n,
    assets: oneToken,
    address: enterprise,
    datum: NO_DATUM,
  },
  {
    name: "token_one_below_minimum",
    lovelace: 1_012_849n,
    assets: oneToken,
    address: enterprise,
    datum: NO_DATUM,
  },
  {
    name: "inline_stake_datum_hash_exact_minimum",
    lovelace: 1_137_840n,
    assets: new Map(),
    address: inlineStaked,
    datum: datumHash("33".repeat(32)),
  },
  {
    name: "inline_stake_datum_hash_one_below_minimum",
    lovelace: 1_137_839n,
    assets: new Map(),
    address: inlineStaked,
    datum: datumHash("33".repeat(32)),
  },
  {
    name: "pointer_exact_minimum",
    lovelace: 900_790n,
    assets: new Map(),
    address: pointer,
    datum: NO_DATUM,
  },
  {
    name: "pointer_one_below_minimum",
    lovelace: 900_789n,
    assets: new Map(),
    address: pointer,
    datum: NO_DATUM,
  },
  {
    name: "long_inline_datum_exact_minimum",
    lovelace: 2_236_890n,
    assets: new Map(),
    address: enterprise,
    datum: inlineDatum(LONG_BYTES),
  },
  {
    name: "long_inline_datum_one_below_minimum",
    lovelace: 2_236_889n,
    assets: new Map(),
    address: enterprise,
    datum: inlineDatum(LONG_BYTES),
  },
  {
    name: "definite_list_datum_one_below_minimum",
    lovelace: 900_789n,
    assets: new Map(),
    address: enterprise,
    datum: inlineDatum(DEFINITE_LIST),
  },
  {
    name: "repeated_key_map_datum_one_below_minimum",
    lovelace: 905_099n,
    assets: new Map(),
    address: enterprise,
    datum: inlineDatum(REPEATED_KEY_MAP),
  },
  {
    name: "value_size_at_limit",
    lovelace: 50_000_000n,
    assets: wideValue(76, 9),
    address: enterprise,
    datum: NO_DATUM,
  },
  {
    name: "value_size_above_limit",
    lovelace: 50_000_000n,
    assets: wideValue(76, 10),
    address: enterprise,
    datum: NO_DATUM,
  },
  {
    name: "short_datum_hash",
    lovelace: 50_000_000n,
    assets: new Map(),
    address: enterprise,
    datum: datumHash("33".repeat(24)),
  },
];

const OUTREF = { transactionId: "ab".repeat(32), outputIndex: 0n };

const valueOf = (testCase: Case) => ({
  lovelace: testCase.lovelace,
  assets: testCase.assets,
});

const outputOf = (testCase: Case) =>
  encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: valueOf(testCase),
  });

const bodyCborOf = (testCase: Case): string =>
  replacePlutusConstrFieldCbor(
    Data.to(
      {
        l2_outref: OUTREF,
        l2_owner: owner,
        l2_value: new Map([
          ["", new Map([["", testCase.lovelace]])],
          ...testCase.assets,
        ]),
        l1_address: testCase.address,
        l1_datum: "NoDatum",
      },
      SDK.WithdrawalBody,
    ),
    [4],
    testCase.datum,
  );

/** The body bytes and Cardano value size step-04 judges, as the Aiken vector
 * states them. */
const chainVector = (testCase: Case) => ({
  bodyCbor: bodyCborOf(testCase),
  cardanoValueSize: BigInt(midgardCardanoValueCborLength(valueOf(testCase))),
});

/** The node's verdict on a withdrawal signed by the output's owner. */
const nodeValidity = async (testCase: Case): Promise<string> => {
  const bodyCbor = bodyCborOf(testCase);
  const infoCbor = replacePlutusConstrFieldCbor(
    Data.to(
      {
        body: Data.from(bodyCbor, SDK.WithdrawalBody),
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
      l2ValueCbor: plutusConstrFieldCbor(bodyCbor, [2]),
      eventInfoCbor: infoCbor,
      ledgerOutRef: encodeMidgardSpendInputItem({
        txId: Buffer.from(OUTREF.transactionId, "hex"),
        outputIndex: 0,
      }),
      ledgerOutput: outputOf(testCase),
    }),
  );
  return result.validity;
};

const VECTOR =
  /test agrees_(\w+)\(\) (fail )?\{\s*chain_payable\(\s*#"([0-9a-f]*)",\s*([\d_]+),?\s*\)(?:\s*==\s*(True|False))?\s*\}/gu;

type ChainVerdict = "payable" | "unpayable" | "refused";

const aikenVectors = async () => {
  const source = await readFile(AIKEN_VECTORS, "utf8");
  return new Map(
    [...source.matchAll(VECTOR)].map((match) => [
      match[1]!,
      {
        vector: {
          bodyCbor: match[3]!,
          cardanoValueSize: BigInt(match[4]!.replaceAll("_", "")),
        },
        verdict: (match[2] === "fail "
          ? "refused"
          : match[5] === "True"
            ? "payable"
            : "unpayable") as ChainVerdict,
      },
    ]),
  );
};

describe("withdrawal payability: node classification agrees with step-04", () => {
  it("covers every case and nothing else in the Aiken vectors", async () => {
    expect([...(await aikenVectors()).keys()].sort()).toEqual(
      CASES.map(({ name }) => name).sort(),
    );
  });

  it.each(CASES)("$name", async (testCase) => {
    const chain = (await aikenVectors()).get(testCase.name);
    const vector = chainVector(testCase);
    expect(chain?.vector).toEqual(vector);
    const payable = chain?.verdict === "payable";
    expect(
      SDK.withdrawalMistagPayable({
        bodyCbor: vector.bodyCbor,
        lovelace: testCase.lovelace,
        cardanoValueSize: vector.cardanoValueSize,
      }),
    ).toBe(payable);
    expect(await nodeValidity(testCase)).toBe(
      payable ? "WithdrawalIsValid" : "UnpayableWithdrawalValue",
    );
  });
});
