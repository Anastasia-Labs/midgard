import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";

type BlueprintConstructor = {
  readonly title: string;
  readonly index: number;
  readonly fields?: readonly {
    readonly title?: string;
    readonly $ref?: string;
  }[];
};

type BlueprintDefinition = {
  readonly anyOf?: readonly BlueprintConstructor[];
};

type Blueprint = {
  readonly definitions: Record<string, BlueprintDefinition>;
};

const testDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(testDir, "../../..");
const blueprint = JSON.parse(
  readFileSync(path.join(repoRoot, "onchain/aiken/plutus.json"), "utf8"),
) as Blueprint;

const definition = (name: string): BlueprintDefinition => {
  const found = blueprint.definitions[name];
  expect(found, `missing blueprint definition ${name}`).toBeDefined();
  return found;
};

const constructor = (
  definitionName: string,
  constructorName: string,
): BlueprintConstructor => {
  const found = definition(definitionName).anyOf?.find(
    (candidate) => candidate.title === constructorName,
  );
  expect(
    found,
    `missing ${constructorName} constructor in ${definitionName}`,
  ).toBeDefined();
  return found!;
};

const fields = (ctor: BlueprintConstructor): readonly string[] =>
  (ctor.fields ?? []).map((field) => field.title ?? "");

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);
const h64 = "33".repeat(64);
const address: SDK.AddressData = {
  paymentCredential: { PublicKeyCredential: [h28] },
  stakeCredential: null,
};
const value: SDK.Value = new Map([["", new Map([["", 1n]])]]);
const proof: SDK.Proof = [];

const roundTrip = <T>(value: T, schema: T): T =>
  Data.from(Data.to(value as any, schema as any), schema as any) as T;

describe("SDK canonical ABI fixtures", () => {
  it("tracks canonical Aiken datum and redeemer field names", () => {
    expect(
      fields(constructor("midgard/scheduler/Datum", "ActiveOperator")),
    ).toEqual(["operator", "start_time"]);
    expect(
      constructor("midgard/scheduler/Datum", "NoActiveOperators").index,
    ).toBe(0);
    expect(
      fields(constructor("midgard/ledger_state/DepositInfo", "DepositInfo")),
    ).toEqual(["l2_address", "l2_datum"]);
    expect(
      fields(
        constructor(
          "midgard/state_queue/MintRedeemer",
          "MergeToConfirmedState",
        ),
      ),
    ).toEqual([
      "header_node_key",
      "header_node_input_index",
      "confirmed_state_input_index",
      "confirmed_state_output_index",
      "m_settlement_redeemer_index",
      "merged_block_transactions_root",
      "merged_block_deposits_root",
      "merged_block_withdrawals_root",
    ]);
    expect(
      fields(constructor("midgard/settlement/MintRedeemer", "Spawn")),
    ).toEqual([
      "settlement_id",
      "output_index",
      "state_queue_merge_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(
      fields(constructor("midgard/user_events/deposit/Datum", "Datum")),
    ).toEqual(["event", "inclusion_time", "witness"]);
    expect(
      fields(
        constructor(
          "midgard/user_events/withdrawal/SpendRedeemer",
          "SpendRedeemer",
        ),
      ),
    ).toContain("purpose");
    expect(
      constructor(
        "midgard/ledger_state/WithdrawalValidity",
        "UnpayableWithdrawalValue",
      ).index,
    ).toBe(7);

    expect(
      fields(constructor("midgard/payout/MintRedeemer", "MintPayout")),
    ).toEqual([
      "withdrawal_utxo_out_ref",
      "withdrawal_input_index",
      "withdrawal_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(
      fields(constructor("midgard/payout/MintRedeemer", "BurnPayout")),
    ).toEqual([
      "payout_input_index",
      "payout_asset_name",
      "payout_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
    const addFundsFields = fields(
      constructor("midgard/payout/SpendRedeemer", "AddFunds"),
    );
    expect(addFundsFields).toEqual([
      "payout_input_index",
      "payout_output_index",
      "reserve_input_index",
      "reserve_change_output_index",
      "reserve_spend_redeemer_index",
      "payout_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(addFundsFields).not.toContain("settlement_ref_input_index");
    expect(addFundsFields).not.toContain("membership_proof");
    const concludeFields = fields(
      constructor("midgard/payout/SpendRedeemer", "ConcludeWithdrawal"),
    );
    expect(concludeFields).toEqual([
      "payout_input_index",
      "l1_output_index",
      "burn_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(concludeFields).not.toContain("settlement_ref_input_index");
    expect(concludeFields).not.toContain("membership_proof");
    expect(
      fields(constructor("midgard/reserve/SpendRedeemer", "Spend")),
    ).toEqual([
      "reserve_input_index",
      "payout_input_index",
      "payout_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
  });

  it("encodes scheduler, hub-oracle, state-queue, and operator redeemers", () => {
    expect(roundTrip("NoActiveOperators", SDK.SchedulerDatum)).toEqual(
      "NoActiveOperators",
    );
    expect(
      roundTrip(
        { ActiveOperator: { operator: h28, start_time: 10n } },
        SDK.SchedulerDatum,
      ),
    ).toEqual({ ActiveOperator: { operator: h28, start_time: 10n } });

    const hubOracleDatum: SDK.HubOracleDatum = {
      registered_operators: h28,
      active_operators: h28,
      retired_operators: h28,
      scheduler: h28,
      state_queue: h28,
      fraud_proof_catalogue: h28,
      fraud_proof: h28,
      deposit: h28,
      withdrawal: h28,
      tx_order: h28,
      settlement: h28,
      payout: h28,
      registered_operators_addr: address,
      active_operators_addr: address,
      retired_operators_addr: address,
      scheduler_addr: address,
      state_queue_addr: address,
      fraud_proof_catalogue_addr: address,
      fraud_proof_addr: address,
      deposit_addr: address,
      withdrawal_addr: address,
      tx_order_addr: address,
      settlement_addr: address,
      reserve_addr: address,
      payout_addr: address,
      reserve_observer: h28,
    };
    expect(Object.keys(roundTrip(hubOracleDatum, SDK.HubOracleDatum))).toEqual([
      "registered_operators",
      "active_operators",
      "retired_operators",
      "scheduler",
      "state_queue",
      "fraud_proof_catalogue",
      "fraud_proof",
      "deposit",
      "withdrawal",
      "tx_order",
      "settlement",
      "payout",
      "registered_operators_addr",
      "active_operators_addr",
      "retired_operators_addr",
      "scheduler_addr",
      "state_queue_addr",
      "fraud_proof_catalogue_addr",
      "fraud_proof_addr",
      "deposit_addr",
      "withdrawal_addr",
      "tx_order_addr",
      "settlement_addr",
      "reserve_addr",
      "payout_addr",
      "reserve_observer",
    ]);

    expect(
      roundTrip({ Init: { output_index: 2n } }, SDK.StateQueueRedeemer),
    ).toEqual({ Init: { output_index: 2n } });
    expect(
      roundTrip(
        {
          CommitBlockHeader: {
            latest_block_input_index: 0n,
            new_block_output_index: 1n,
            continued_latest_block_output_index: 2n,
            operator: h28,
            scheduler_ref_input_index: 0n,
            active_operators_input_index: 1n,
            active_operators_redeemer_index: 1n,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    ).toMatchObject({ CommitBlockHeader: { operator: h28 } });
    expect(
      roundTrip(
        {
          MergeToConfirmedState: {
            header_node_key: h28,
            header_node_input_index: 1n,
            confirmed_state_input_index: 0n,
            confirmed_state_output_index: 0n,
            m_settlement_redeemer_index: 2n,
            merged_block_transactions_root: h32,
            merged_block_deposits_root: h32,
            merged_block_withdrawals_root: h32,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    ).toMatchObject({ MergeToConfirmedState: { header_node_key: h28 } });

    expect(
      roundTrip(
        {
          RegisterOperator: {
            registering_operator: h28,
            root_input_index: 0n,
            root_output_index: 0n,
            registered_node_output_index: 1n,
            hub_oracle_ref_input_index: 0n,
            active_operators_element_ref_input_index: 1n,
            operator_origin: {
              NewOperator: { retired_operators_element_ref_input_index: 2n },
            },
          },
        },
        SDK.RegisteredOperatorMintRedeemer,
      ),
    ).toMatchObject({ RegisterOperator: { registering_operator: h28 } });
    expect(
      roundTrip(
        {
          ActivateOperator: {
            new_active_operator_key: h28,
            new_active_operator_bond_unlock_time: null,
            active_operator_anchor_element_input_index: 0n,
            active_operator_anchor_element_output_index: 0n,
            active_operator_inserted_node_output_index: 1n,
            registered_operators_redeemer_index: 2n,
          },
        },
        SDK.ActiveOperatorMintRedeemer,
      ),
    ).toMatchObject({ ActivateOperator: { new_active_operator_key: h28 } });
  });

  it("encodes user-event witness, user-event spend, settlement, and fraud-proof fixtures", () => {
    const aikenWitnessPrefix = readFileSync(
      path.join(repoRoot, "onchain/aiken/lib/midgard/user-events/witness.ak"),
      "utf8",
    ).match(/pub const witness_script_prefix: ByteArray =\n  #"([^"]+)"/)?.[1];
    expect(SDK.USER_EVENT_WITNESS_SCRIPT_PREFIX).toBe(aikenWitnessPrefix);

    const witnessValidator = SDK.buildUserEventWitnessCertificateValidator(h32);
    expect(SDK.userEventWitnessScriptHash(h32)).toBe(
      validatorToScriptHash(witnessValidator),
    );
    expect(
      Data.from(
        SDK.encodeUserEventWitnessMintOrBurnRedeemer(h28),
        SDK.UserEventWitnessPublishRedeemer,
      ),
    ).toEqual({ MintOrBurn: { targetPolicy: h28 } });
    expect(
      Data.from(
        SDK.encodeUserEventAuthenticateMintRedeemer({
          nonceInputIndex: 0n,
          eventOutputIndex: 1n,
          hubRefInputIndex: 0n,
          witnessRegistrationRedeemerIndex: 2n,
        }),
        SDK.UserEventMintRedeemer,
      ),
    ).toEqual({
      AuthenticateEvent: {
        nonce_input_index: 0n,
        event_output_index: 1n,
        hub_ref_input_index: 0n,
        witness_registration_redeemer_index: 2n,
      },
    });

    const depositDatum: SDK.DepositDatum = {
      event: {
        id: { transactionId: h32, outputIndex: 0n },
        info: { l2_address: address, l2_datum: null },
      },
      inclusion_time: 123n,
      witness: h28,
    };
    expect(roundTrip(depositDatum, SDK.DepositDatum)).toEqual(depositDatum);
    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          mint_redeemer_index: 3n,
          membership_proof: proof,
          inclusion_proof_script_withdraw_redeemer_index: 4n,
        },
        SDK.DepositSpendRedeemer,
      ),
    ).toMatchObject({ input_index: 0n });

    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          burn_redeemer_index: 3n,
          membership_proof: proof,
          inclusion_proof_script_withdraw_redeemer_index: 4n,
          validity_override: "TxIsValid",
        },
        SDK.TxOrderSpendRedeemer,
      ),
    ).toMatchObject({ validity_override: "TxIsValid" });

    const withdrawalDatum: SDK.WithdrawalOrderDatum = {
      event: {
        id: { transactionId: h32, outputIndex: 0n },
        info: {
          body: {
            l2_outref: { transactionId: h32, outputIndex: 1n },
            l2_owner: h28,
            l2_value: value,
            l1_address: address,
            l1_datum: "NoDatum",
          },
          signature: [h32, h64],
          validity: "WithdrawalIsValid",
        },
      },
      inclusion_time: 123n,
      witness: h28,
      refund_address: address,
      refund_datum: "NoDatum",
    };
    expect(roundTrip(withdrawalDatum, SDK.WithdrawalOrderDatum)).toEqual(
      withdrawalDatum,
    );
    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          burn_redeemer_index: 3n,
          payout_mint_redeemer_index: 4n,
          membership_proof: proof,
          inclusion_proof_script_withdraw_redeemer_index: 5n,
          purpose: {
            Refund: {
              validity_override: {
                SpentWithdrawalUtxo: { l2_tx_id: h32 },
              },
            },
          },
        },
        SDK.WithdrawalSpendRedeemer,
      ),
    ).toMatchObject({ purpose: { Refund: expect.any(Object) } });
    expect(roundTrip("UnpayableWithdrawalValue", SDK.WithdrawalValidity)).toBe(
      "UnpayableWithdrawalValue",
    );

    expect(
      roundTrip(
        {
          deposits_root: h32,
          withdrawals_root: h32,
          transactions_root: h32,
          resolution_claim: null,
        },
        SDK.SettlementDatum,
      ),
    ).toMatchObject({ resolution_claim: null });
    expect(
      roundTrip(
        {
          Spawn: {
            settlement_id: h28,
            output_index: 0n,
            state_queue_merge_redeemer_index: 1n,
            hub_ref_input_index: 2n,
          },
        },
        SDK.SettlementMintRedeemer,
      ),
    ).toMatchObject({ Spawn: { settlement_id: h28 } });
    expect(roundTrip("Init", SDK.FraudProofCatalogueMintRedeemer)).toBe("Init");
  });

  it("encodes reserve and datum-based payout fixtures", () => {
    const payoutDatum: SDK.PayoutDatum = {
      l2_value: value,
      l1_address: address,
      l1_datum: "NoDatum",
    };
    expect(roundTrip(payoutDatum, SDK.PayoutDatum)).toEqual(payoutDatum);

    expect(
      roundTrip(
        {
          MintPayout: {
            withdrawal_utxo_out_ref: { transactionId: h32, outputIndex: 0n },
            withdrawal_input_index: 1n,
            withdrawal_spend_redeemer_index: 2n,
            hub_ref_input_index: 3n,
          },
        },
        SDK.PayoutMintRedeemer,
      ),
    ).toEqual({
      MintPayout: {
        withdrawal_utxo_out_ref: { transactionId: h32, outputIndex: 0n },
        withdrawal_input_index: 1n,
        withdrawal_spend_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
    });

    expect(
      roundTrip(
        {
          BurnPayout: {
            payout_input_index: 0n,
            payout_asset_name: h32,
            payout_spend_redeemer_index: 1n,
            hub_ref_input_index: 2n,
          },
        },
        SDK.PayoutMintRedeemer,
      ),
    ).toEqual({
      BurnPayout: {
        payout_input_index: 0n,
        payout_asset_name: h32,
        payout_spend_redeemer_index: 1n,
        hub_ref_input_index: 2n,
      },
    });

    expect(
      roundTrip(
        {
          AddFunds: {
            payout_input_index: 0n,
            payout_output_index: 1n,
            reserve_input_index: 2n,
            reserve_change_output_index: null,
            reserve_spend_redeemer_index: 3n,
            payout_spend_redeemer_index: 4n,
            hub_ref_input_index: 5n,
          },
        },
        SDK.PayoutSpendRedeemer,
      ),
    ).toEqual({
      AddFunds: {
        payout_input_index: 0n,
        payout_output_index: 1n,
        reserve_input_index: 2n,
        reserve_change_output_index: null,
        reserve_spend_redeemer_index: 3n,
        payout_spend_redeemer_index: 4n,
        hub_ref_input_index: 5n,
      },
    });

    expect(
      roundTrip(
        {
          ConcludeWithdrawal: {
            payout_input_index: 0n,
            l1_output_index: 1n,
            burn_redeemer_index: 2n,
            hub_ref_input_index: 3n,
          },
        },
        SDK.PayoutSpendRedeemer,
      ),
    ).toEqual({
      ConcludeWithdrawal: {
        payout_input_index: 0n,
        l1_output_index: 1n,
        burn_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
    });

    expect(
      roundTrip(
        {
          reserve_input_index: 0n,
          payout_input_index: 1n,
          payout_spend_redeemer_index: 2n,
          hub_ref_input_index: 3n,
        },
        SDK.ReserveSpendRedeemer,
      ),
    ).toEqual({
      reserve_input_index: 0n,
      payout_input_index: 1n,
      payout_spend_redeemer_index: 2n,
      hub_ref_input_index: 3n,
    });
  });
});
