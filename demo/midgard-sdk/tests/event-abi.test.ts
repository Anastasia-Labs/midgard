import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/active-operators.js";
import "../src/linked-list.js";
import "../src/payout.js";
import "../src/registered-operators.js";
import "../src/reserve.js";
import "../src/retired-operators.js";
import "../src/settlement.js";
import "../src/transition-trace.js";
import "../src/user-events/deposit.js";
import "../src/user-events/internals.js";
import "../src/user-events/withdrawal.js";
import "./event-abi.withdrawal-datum.js";
import "./event-abi.vectors.js";
import "./event-abi.expected-cbor-by-label.js";

import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  fetchActiveOperatorUTxOs,
  OperatorRemovalSchedulerSync,
  SlashingArguments,
  SlashingReason,
} from "../src/active-operators.js";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
} from "../src/linked-list.js";
import {
  PayoutDatum,
  PayoutMintRedeemer,
  PayoutSpendRedeemer,
} from "../src/payout.js";
import {
  DuplicateOperatorStatus,
  RegisteredOperatorMintRedeemer,
} from "../src/registered-operators.js";
import { ReserveSpendRedeemer } from "../src/reserve.js";
import {
  fetchRetiredOperatorUTxOs,
  RetiredOperatorDatum,
  RetiredOperatorMintRedeemer,
} from "../src/retired-operators.js";
import {
  EventType,
  SettlementDatum,
  SettlementMintRedeemer,
  SettlementSpendRedeemer,
} from "../src/settlement.js";
import { EventSettlementMembershipProof } from "../src/transition-trace.js";
import {
  DepositDatum,
  DepositSpendRedeemer,
} from "../src/user-events/deposit.js";
import {
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
} from "../src/user-events/internals.js";
import {
  WithdrawalOrderDatum,
  WithdrawalSpendPurpose,
  WithdrawalSpendRedeemer,
} from "../src/user-events/withdrawal.js";
import { EXPECTED_CBOR_BY_LABEL } from "./event-abi.expected-cbor-by-label.js";
import { vectors } from "./event-abi.vectors.js";
import {
  ACTIVE_OPERATOR_DATUM,
  H28_A,
  H28_B,
  H32_A,
  RETIRED_OPERATOR_DATUM,
} from "./event-abi.withdrawal-datum.js";

describe("canonical event and operator V1 ABI", () => {
  it("matches every TypeScript event/operator tag, arity, field, and nested shape", () => {
    expect(Object.keys(EXPECTED_CBOR_BY_LABEL)).toHaveLength(vectors.length);
    for (const vector of vectors) {
      const expected =
        EXPECTED_CBOR_BY_LABEL[
          vector.label as keyof typeof EXPECTED_CBOR_BY_LABEL
        ];
      expect(expected, vector.label).toBeDefined();
      expect(
        Data.to(vector.value as never, vector.schema as never),
        vector.label,
      ).toBe(expected);
      expect(Data.from(expected, vector.schema as never), vector.label).toEqual(
        vector.value,
      );
    }
  });

  it("rejects adjacent tags, wrong arities, decorative versions, and malformed nested values", () => {
    const activeStrikeWithMalformedLink = EXPECTED_CBOR_BY_LABEL[
      "active operator spend strike tag3/7"
    ].replace("d8799f41aaff", "01");
    const depositDatumWithDecorativeVersion = `${EXPECTED_CBOR_BY_LABEL[
      "deposit datum record/3"
    ].slice(0, -2)}00ff`;
    const shortOperatorKey = `d87a9f581b${"11".repeat(27)}01020304ff`;

    const invalid: readonly (readonly [string, string, unknown])[] = [
      ["user event mint adjacent tag", "d87b80", UserEventMintRedeemer],
      [
        "user event witness adjacent tag",
        "d87c80",
        UserEventWitnessPublishRedeemer,
      ],
      ["deposit datum wrong arity", "d87980", DepositDatum],
      [
        "deposit datum decorative V2 field",
        depositDatumWithDecorativeVersion,
        DepositDatum,
      ],
      [
        "deposit spend wrong arity",
        "d8799f010203040506ff",
        DepositSpendRedeemer,
      ],
      ["withdrawal datum wrong arity", "d87980", WithdrawalOrderDatum],
      ["withdrawal purpose adjacent tag", "d87b80", WithdrawalSpendPurpose],
      ["withdrawal purpose wrong arity", "d87a80", WithdrawalSpendPurpose],
      [
        "withdrawal spend wrong arity",
        "d8799f0102030405060708ff",
        WithdrawalSpendRedeemer,
      ],
      ["reserve adjacent tag", "d87a80", ReserveSpendRedeemer],
      ["reserve wrong arity", "d8799f010203ff", ReserveSpendRedeemer],
      ["slashing reason adjacent tag", "d87b80", SlashingReason],
      ["slashing arguments wrong arity", "d8799f01020304ff", SlashingArguments],
      [
        "operator scheduler sync adjacent tag",
        "d87b80",
        OperatorRemovalSchedulerSync,
      ],
      ["active spend adjacent tag", "d87d80", ActiveOperatorSpendRedeemer],
      [
        "active spend malformed linked-list Link",
        activeStrikeWithMalformedLink,
        ActiveOperatorSpendRedeemer,
      ],
      [
        "active spend short verification-key hash",
        shortOperatorKey,
        ActiveOperatorSpendRedeemer,
      ],
      ["active mint adjacent tag", "d87e80", ActiveOperatorMintRedeemer],
      ["active payload wrong arity", "d8799f01ff", ActiveOperatorDatum],
      ["registered duplicate adjacent tag", "d87c80", DuplicateOperatorStatus],
      [
        "registered mint adjacent tag",
        "d87f80",
        RegisteredOperatorMintRedeemer,
      ],
      ["retired mint adjacent tag", "d87e80", RetiredOperatorMintRedeemer],
      [
        "retired slash arbitrary legacy payload",
        "d87d9f01ff",
        RetiredOperatorMintRedeemer,
      ],
      ["payout datum wrong arity", "d8799f0102ff", PayoutDatum],
      ["payout spend adjacent tag", "d87b80", PayoutSpendRedeemer],
      ["payout mint adjacent tag", "d87b80", PayoutMintRedeemer],
      ["settlement datum wrong arity", "d8799f01020304ff", SettlementDatum],
      ["settlement event adjacent tag", "d87c80", EventType],
      [
        "settlement membership adjacent tag",
        "d87c80",
        EventSettlementMembershipProof,
      ],
      ["settlement spend adjacent tag", "d87c80", SettlementSpendRedeemer],
      ["settlement mint adjacent tag", "d87b80", SettlementMintRedeemer],
    ];

    for (const [label, cbor, schema] of invalid) {
      expect(() => Data.from(cbor, schema as never), label).toThrow();
    }
  });

  it("unwraps persisted active and retired operator nodes and rejects raw payload datums", async () => {
    const policyId = H28_A;
    const makeUtxo = (datum: string, assetName: string) =>
      ({
        txHash: H32_A,
        outputIndex: 0,
        address: "addr_test1_event_v1_abi",
        assets: {
          lovelace: 2_000_000n,
          [`${policyId}${assetName}`]: 1n,
        },
        datum,
      }) as never;
    const lucidWith = (utxos: readonly unknown[]) =>
      ({
        utxosAt: async () => utxos,
      }) as never;

    const activeAssetName = `${ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_B}`;
    const active = await Effect.runPromise(
      fetchActiveOperatorUTxOs(
        {
          activeOperatorAddress: "addr_test1_event_v1_abi",
          operator: H28_B,
          activeOperatorPolicyId: policyId,
        },
        lucidWith([
          makeUtxo(
            EXPECTED_CBOR_BY_LABEL["active operator persisted node envelope"],
            activeAssetName,
          ),
          makeUtxo(
            Data.to(ACTIVE_OPERATOR_DATUM, ActiveOperatorDatum),
            `${ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_A}`,
          ),
        ]),
      ),
    );
    expect(active).toHaveLength(1);
    expect(active[0]?.assetName).toBe(activeAssetName);
    expect(active[0]?.datum).toEqual(ACTIVE_OPERATOR_DATUM);

    const retiredAssetName = `${RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_B}`;
    const retired = await Effect.runPromise(
      fetchRetiredOperatorUTxOs(
        {
          retiredOperatorAddress: "addr_test1_event_v1_abi",
          operator: H28_B,
          retiredOperatorPolicyId: policyId,
        },
        lucidWith([
          makeUtxo(
            EXPECTED_CBOR_BY_LABEL["retired operator persisted node envelope"],
            retiredAssetName,
          ),
          makeUtxo(
            Data.to(RETIRED_OPERATOR_DATUM, RetiredOperatorDatum),
            `${RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_A}`,
          ),
        ]),
      ),
    );
    expect(retired).toHaveLength(1);
    expect(retired[0]?.assetName).toBe(retiredAssetName);
    expect(retired[0]?.datum).toEqual(RETIRED_OPERATOR_DATUM);
  });
});
