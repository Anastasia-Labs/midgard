import "node:crypto";
import "node:fs";
import "node:path";
import "node:url";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "effect";
import "vitest";
import "./sdk-abi-fixtures.header-fixture.js";
import "./sdk-abi-fixtures.build-transition-trace-abi-fixtures.js";

import { readFileSync, writeFileSync } from "node:fs";
import path from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildTransitionTraceAbiFixtures } from "./sdk-abi-fixtures.build-transition-trace-abi-fixtures.js";
import {
  address,
  constructor,
  encodedFixture,
  eventKeys,
  expectGoldenFixture,
  expectRoundTrip,
  fields,
  forcedInclusionTxFixture,
  type GoldenAbiFixtureFile,
  h28,
  h32,
  h64,
  headerFixture,
  ledgerStateSource,
  outputReference,
  proof,
  repoRoot,
  roundTrip,
  testnetIntegerConst,
  transitionPhases,
  transitionTraceAbiGolden,
  value,
} from "./sdk-abi-fixtures.header-fixture.js";

describe("SDK canonical ABI fixtures", () => {
  it("keeps SDK protocol timing constants aligned with canonical Aiken values", () => {
    expect(SDK.SHIFT_DURATION_MS).toBe(testnetIntegerConst("shift_duration"));
    expect(SDK.REGISTRATION_DURATION_MS).toBe(
      testnetIntegerConst("registration_duration"),
    );
    // ledger-state.ak re-exports the profile's env value, so the literal is
    // read from the env module the same way as its siblings.
    expect(ledgerStateSource).toMatch(
      /^pub const block_maturity_duration_v1: Int = env\.block_maturity_duration_v1$/m,
    );
    expect(SDK.MATURITY_DURATION_MS).toBe(
      testnetIntegerConst("block_maturity_duration_v1"),
    );
    expect(SDK.USER_EVENTS_NEGLIGENCE_TIMEOUT_MS).toBe(
      testnetIntegerConst("user_events_negligence_timeout"),
    );
    expect(SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS).toBe(
      testnetIntegerConst("new_shift_inactivity_grace_period"),
    );
    expect(SDK.MAX_VALIDITY_RANGE_LENGTH_MS).toBe(
      testnetIntegerConst("max_validity_range_length"),
    );
    expect(SDK.MAX_INACTIVITY_STRIKES).toBe(
      testnetIntegerConst("max_inactivity_strikes"),
    );
    expect(BigInt(SDK.EVENT_WAIT_DURATION_MS)).toBe(
      testnetIntegerConst("event_wait_duration"),
    );
  });

  it("tracks canonical Aiken datum and redeemer field names", () => {
    expect(
      fields(constructor("midgard/scheduler/SchedDatum", "ActiveOperator")),
    ).toEqual(["operator", "start_time"]);
    expect(
      constructor("midgard/scheduler/SchedDatum", "NoActiveOperators").index,
    ).toBe(0);
    expect(
      fields(constructor("midgard/ledger_state/DepositInfo", "DepositInfo")),
    ).toEqual(["l2_address", "l2_network_id", "l2_datum"]);
    expect(
      fields(
        constructor(
          "midgard/state_queue/MintRedeemer",
          "MergeToConfirmedStateV1",
        ),
      ),
    ).toEqual([
      "yield_to_ref_input_index",
      "header_node_key",
      "confirmed_state_input_outref",
      "confirmed_state_output_index",
      "m_settlement_redeemer_index",
      "merged_block_withdrawals_root",
      "merged_block_forced_transactions_root",
      "merged_block_transactions_root",
      "merged_block_deposits_root",
      "merged_block_transition_trace_root",
      "merged_block_event_to_step_root",
      "merged_block_validation_traces_root",
      "merged_block_withdrawal_count",
      "merged_block_forced_transaction_count",
      "merged_block_l2_transaction_count",
      "merged_block_deposit_count",
      "merged_block_total_event_count",
      "merged_block_transition_step_count",
      "merged_block_validation_trace_count",
    ]);
    expect(
      fields(constructor("midgard/settlement/MintRedeemer", "Spawn")),
    ).toEqual([
      "settlement_id",
      "output_index",
      "state_queue_merge_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(fields(constructor("midgard/settlement/Datum", "Datum"))).toEqual([
      "deposits_root",
      "withdrawals_root",
      "forced_transactions_root",
      "transactions_root",
      "resolution_claim",
    ]);
    expect(
      fields(
        constructor("midgard/user_events/deposit/DepositDatum", "DepositDatum"),
      ),
    ).toEqual(["event", "inclusion_time", "witness"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/TransitionFaultProof",
          "TransitionFaultProof",
        ),
      ),
    ).toEqual(["challenged_header_hash", "header", "fault"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/TransitionFault",
          "TraceBoundaryFault",
        ),
      ),
    ).toEqual(["side", "trace_proof"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/SourceMembershipMismatchWitness",
          "MappedEventMissingFromSource",
        ),
      ),
    ).toEqual(["trace_proof", "event_to_step", "source_non_membership"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/InvalidOneStepTransitionWitness",
          "ValidDepositTransition",
        ),
      ),
    ).toEqual([
      "trace_proof",
      "event_to_step",
      "source_membership",
      "projected_utxo",
    ]);
    for (const [definition, variant, sourceField] of [
      [
        "OmittedDueL1EventWitness",
        "OmittedDueDeposit",
        "source_non_membership",
      ],
      [
        "OmittedDueL1EventWitness",
        "OmittedDueWithdrawal",
        "source_non_membership",
      ],
      [
        "OutOfWindowSourceEventWitness",
        "OutOfWindowDeposit",
        "source_membership",
      ],
      [
        "OutOfWindowSourceEventWitness",
        "OutOfWindowWithdrawal",
        "source_membership",
      ],
    ] as const) {
      expect(
        fields(
          constructor(
            `midgard/fraud_proofs/transition_trace/proof/${definition}`,
            variant,
          ),
        ),
      ).toEqual([sourceField]);
    }
    expect(
      fields(
        constructor("fraud_proofs/transition_trace/route_v1/Args", "Args"),
      ),
    ).toEqual(["input_index", "output_index", "proof", "proof_ref_indices"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/final_v1/Args",
          "Args",
        ),
      ),
    ).toEqual([
      "input_index",
      "output_index",
      "hub_ref_input_index",
      "fraud_proof_mint_redeemer_index",
    ]);
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
      "retirement_withdraw_redeemer_index",
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

  it("rejects the obsolete deposit transition pointer fields", () => {
    const fixture =
      buildTransitionTraceAbiFixtures()[
        "transition-fault.valid-deposit-transition.fault"
      ]!;
    const raw = Data.from(Data.to(fixture.value, fixture.schema));
    if (!(raw instanceof Constr) || !(raw.fields[0] instanceof Constr))
      throw new Error("Expected deposit fault witness");
    expect(raw.fields[0].fields).toHaveLength(4);
    raw.fields[0].fields.splice(3, 0, 999n, "00");
    expect(() => Data.from(Data.to(raw), SDK.TransitionFault)).toThrow();
  });

  it("matches transition trace golden ABI fixture files", () => {
    expect(transitionTraceAbiGolden.version).toBe(1);
    expect(transitionTraceAbiGolden.encoding).toBe(
      "lucid-plutus-data-cbor-hex",
    );

    const fixtures = buildTransitionTraceAbiFixtures();
    // Emit mode belongs to scripts/generate-transition-trace-abi-fixture.mjs:
    // it hands us a scratch path, diffs the result against the checked-in
    // golden itself and regenerates the Aiken golden from the same bytes. A
    // normal run has no way to rewrite the golden it is asserting against.
    const emitPath = process.env.MIDGARD_TRANSITION_TRACE_ABI_EMIT_PATH;
    if (emitPath !== undefined) {
      const emitted: GoldenAbiFixtureFile = {
        version: 1,
        encoding: "lucid-plutus-data-cbor-hex",
        fixtures: Object.fromEntries(
          Object.entries(fixtures).map(([name, fixture]) => {
            try {
              return [
                name,
                {
                  schema: fixture.schemaName,
                  ...encodedFixture(fixture.value, fixture.schema),
                },
              ];
            } catch (error) {
              throw new Error(
                `failed to encode transition trace ABI fixture ${name}`,
                { cause: error },
              );
            }
          }),
        ),
      };
      writeFileSync(emitPath, `${JSON.stringify(emitted, null, 2)}\n`);
      return;
    }

    expect(Object.keys(transitionTraceAbiGolden.fixtures).sort()).toEqual(
      Object.keys(fixtures).sort(),
    );
    for (const [name, fixture] of Object.entries(fixtures)) {
      expectGoldenFixture({
        name,
        schemaName: fixture.schemaName,
        value: fixture.value,
        schema: fixture.schema,
      });
    }
  });

  it("encodes scheduler, hub-oracle, state-queue, and operator redeemers", () => {
    expectRoundTrip("NoActiveOperators", SDK.SchedulerDatum);
    expectRoundTrip(
      { ActiveOperator: { operator: h28, start_time: 10n } },
      SDK.SchedulerDatum,
    );

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

    expectRoundTrip({ InitV1: { output_index: 2n } }, SDK.StateQueueRedeemer);
    expectRoundTrip("LinkedListMutation", SDK.StateQueueSpendRedeemer);
    expect(
      roundTrip(
        {
          CommitBlockHeader: {
            yield_to_ref_input_index: 0n,
            new_block_output_index: 1n,
            continued_latest_block_output_index: 2n,
            operator: h28,
            scheduler_ref_input_index: 0n,
            active_operators_input_index: 1n,
            active_operators_redeemer_index: 1n,
            m_confirmed_state_ref_input_index: null,
            m_head_state_queue_node_ref_input_index: null,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    ).toMatchObject({ CommitBlockHeader: { operator: h28 } });
    expect(
      roundTrip(
        {
          MergeToConfirmedStateV1: {
            yield_to_ref_input_index: 0n,
            header_node_key: h28,
            confirmed_state_input_outref: outputReference,
            confirmed_state_output_index: 0n,
            m_settlement_redeemer_index: 2n,
            merged_block_withdrawals_root: h32,
            merged_block_forced_transactions_root: h32,
            merged_block_transactions_root: h32,
            merged_block_deposits_root: h32,
            merged_block_transition_trace_root: h32,
            merged_block_event_to_step_root: h32,
            merged_block_validation_traces_root: h32,
            merged_block_withdrawal_count: 1n,
            merged_block_forced_transaction_count: 2n,
            merged_block_l2_transaction_count: 3n,
            merged_block_deposit_count: 4n,
            merged_block_total_event_count: 10n,
            merged_block_transition_step_count: 10n,
            merged_block_validation_trace_count: 10n,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    ).toMatchObject({ MergeToConfirmedStateV1: { header_node_key: h28 } });

    expect(
      roundTrip(
        {
          RegisterOperator: {
            registering_operator: h28,
            root_output_index: 0n,
            registered_node_output_index: 1n,
            hub_oracle_ref_input_index: 0n,
            active_operators_element_ref_input_index: 1n,
            retired_operators_element_ref_input_index: 2n,
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
            active_operator_anchor_element_output_index: 0n,
            active_operator_inserted_node_output_index: 1n,
            registered_operators_redeemer_index: 2n,
            active_operators_set_was_empty: true,
          },
        },
        SDK.ActiveOperatorMintRedeemer,
      ),
    ).toMatchObject({ ActivateOperator: { new_active_operator_key: h28 } });
  });

  it("encodes user-event witness, user-event spend, settlement, and fraud-proof fixtures", () => {
    const aikenWitnessPrefix = readFileSync(
      path.join(repoRoot, "onchain/aiken/env/testnet.ak"),
      "utf8",
    ).match(
      /pub const user_events_witness_script_prefix: ByteArray =\n {2}#"([^"]+)"/,
    )?.[1];
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
    const depositDatum: SDK.DepositDatum = {
      event: {
        id: { transactionId: h32, outputIndex: 0n },
        info: { l2_address: address, l2_network_id: 0n, l2_datum: null },
      },
      inclusion_time: 123n,
      witness: h28,
    };
    expectRoundTrip(depositDatum, SDK.DepositDatum);
    const depositMembershipProof: SDK.RawRootMembershipProof = {
      domain: SDK.ROOT_DOMAINS.deposits,
      root: "44".repeat(32),
      phas_root: "55".repeat(32),
      count: 1n,
      key: Data.to(depositDatum.event.id, SDK.OutputReference),
      value: Data.to(depositDatum.event.info, SDK.DepositInfo),
      proof,
    };
    expectRoundTrip(
      {
        domain: depositMembershipProof.domain,
        root: depositMembershipProof.root,
        phas_root: depositMembershipProof.phas_root,
        count: depositMembershipProof.count,
      },
      SDK.RootCountProof,
    );
    expectRoundTrip(depositMembershipProof, SDK.RawRootMembershipProof);
    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          mint_redeemer_index: 3n,
          membership_proof: depositMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index: 4n,
        },
        SDK.DepositSpendRedeemer,
      ),
    ).toMatchObject({ input_index: 0n });

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
    expectRoundTrip(withdrawalDatum, SDK.WithdrawalOrderDatum);
    const withdrawalMembershipValue: SDK.WithdrawalInfo = {
      ...withdrawalDatum.event.info,
      validity: {
        SpentWithdrawalUtxo: { l2_tx_id: h32 },
      },
    };
    const withdrawalMembershipProof: SDK.RawRootMembershipProof = {
      domain: SDK.ROOT_DOMAINS.withdrawals,
      root: "88".repeat(32),
      phas_root: "99".repeat(32),
      count: 1n,
      key: Data.to(withdrawalDatum.event.id, SDK.OutputReference),
      value: Data.to(withdrawalMembershipValue, SDK.WithdrawalInfo),
      proof,
    };
    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          burn_redeemer_index: 3n,
          payout_mint_redeemer_index: 4n,
          membership_proof: withdrawalMembershipProof,
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
    expectRoundTrip("UnpayableWithdrawalValue", SDK.WithdrawalValidity);

    expect(
      roundTrip(
        {
          deposits_root: h32,
          withdrawals_root: h32,
          forced_transactions_root: h32,
          transactions_root: h32,
          resolution_claim: null,
        },
        SDK.SettlementDatum,
      ),
    ).toMatchObject({ resolution_claim: null });
    expectRoundTrip(
      {
        ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
        prevUtxosRoot: h32,
        utxosRoot: h32,
        withdrawalsRoot: h32,
        transactionsRoot: h32,
        depositsRoot: h32,
        startTime: 1n,
        endTime: 2n,
        blockSlot: 0n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: h28,
        operatorVkey: h28,
        protocolVersion: 1n,
      },
      SDK.Header,
    );
    const forcedInclusionTx: SDK.ForcedInclusionTxV1 = {
      ...forcedInclusionTxFixture,
      verdict: "ForcedTxValid",
    };
    expectRoundTrip(forcedInclusionTx, SDK.ForcedInclusionTxV1);
    expect(
      Object.keys(roundTrip(forcedInclusionTx, SDK.ForcedInclusionTxV1)),
    ).toEqual(["tx_id", "submitted_source", "verdict"]);
    for (const phase of transitionPhases) {
      expectRoundTrip(phase, SDK.TransitionPhase);
      expectRoundTrip({ step_index: 1n, phase }, SDK.EventToStepValue);
    }
    for (const [index, eventKey] of eventKeys.entries()) {
      const phase = transitionPhases[index]!;
      expectRoundTrip(eventKey, SDK.EventKey);
      expectRoundTrip(
        {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: eventKey,
          phase,
          pre_utxos_root: h32,
          post_utxos_root: "44".repeat(32),
        },
        SDK.TransitionStep,
      );
    }
    const transitionStep: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKeys[0]!,
      phase: "Withdrawal",
      pre_utxos_root: h32,
      post_utxos_root: "44".repeat(32),
    };
    const traceProof: SDK.IndexedTraceProof = {
      domain: SDK.ROOT_DOMAINS.transitionTrace,
      root: "55".repeat(32),
      phas_root: "66".repeat(32),
      count: 1n,
      key: 0n,
      value: transitionStep,
      proof,
    };
    const transitionHeader: SDK.Header = {
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
      prevUtxosRoot: h32,
      utxosRoot: "44".repeat(32),
      withdrawalsRoot: "77".repeat(32),
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: traceProof.root,
      eventToStepRoot: "88".repeat(32),
      withdrawalCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28,
      operatorVkey: h28,
      protocolVersion: 1n,
    };
    const transitionFaultProof = SDK.makeTransitionFaultProof({
      challengedHeaderHash: h28,
      header: transitionHeader,
      fault: SDK.traceBoundaryFault({
        side: "TraceStart",
        traceProof,
      }),
    });
    expectRoundTrip(transitionFaultProof, SDK.TransitionFaultProof);
    expectRoundTrip(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 1n,
            proof: transitionFaultProof,
            proof_ref_indices: [],
          },
        ],
      },
      SDK.TransitionTraceRouteSpendRedeemer,
    );
    expect(
      SDK.transitionTraceThreadAssetName({
        fraudCategoryId: "00000004",
        challengedHeaderHash: h28,
      }),
    ).toBe(`00000004${h28}`);
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
    expectRoundTrip("Init", SDK.FraudProofCatalogueMintRedeemer);
  });

  // The baseline digest is derived, never transcribed: the preimage is the
  // checked-in transition-trace golden `HeaderV1` entry (the same bytes the
  // generated Aiken golden decodes), and the digest is recomputed here with
  // @noble/hashes instead of with the SDK helper under test.
  it("commits every transition field into the block header hash", async () => {
    const goldenHeaderCborHex =
      transitionTraceAbiGolden.fixtures.HeaderV1!.cborHex;
    expect(Data.to(headerFixture, SDK.Header)).toBe(goldenHeaderCborHex);
    const expectedBaselineHash = Buffer.from(
      blake2b(Buffer.from(goldenHeaderCborHex, "hex"), { dkLen: 28 }),
    ).toString("hex");

    const baselineHash = await Effect.runPromise(
      SDK.hashBlockHeader(headerFixture),
    );
    expect(baselineHash).toBe(expectedBaselineHash);

    const differentRoot = "aa".repeat(32);
    const differentDigest28 = "bb".repeat(28);
    // Keyed by header field, so adding a header field without deciding whether
    // it is committed fails to compile. `null` means "cannot be varied": the
    // protocol version is pinned by the encoder and is covered by the refusal
    // assertion below.
    const mutations: Record<keyof SDK.Header, SDK.Header | null> = {
      prevUtxosRoot: { ...headerFixture, prevUtxosRoot: differentRoot },
      utxosRoot: { ...headerFixture, utxosRoot: differentRoot },
      withdrawalsRoot: { ...headerFixture, withdrawalsRoot: differentRoot },
      forcedTransactionsRoot: {
        ...headerFixture,
        forcedTransactionsRoot: differentRoot,
      },
      transactionsRoot: { ...headerFixture, transactionsRoot: differentRoot },
      depositsRoot: { ...headerFixture, depositsRoot: differentRoot },
      transitionTraceRoot: {
        ...headerFixture,
        transitionTraceRoot: differentRoot,
      },
      eventToStepRoot: { ...headerFixture, eventToStepRoot: differentRoot },
      validationTracesRoot: {
        ...headerFixture,
        validationTracesRoot: differentRoot,
      },
      withdrawalCount: {
        ...headerFixture,
        withdrawalCount: headerFixture.withdrawalCount + 1n,
      },
      forcedTransactionCount: {
        ...headerFixture,
        forcedTransactionCount: headerFixture.forcedTransactionCount + 1n,
      },
      l2TransactionCount: {
        ...headerFixture,
        l2TransactionCount: headerFixture.l2TransactionCount + 1n,
      },
      depositCount: {
        ...headerFixture,
        depositCount: headerFixture.depositCount + 1n,
      },
      totalEventCount: {
        ...headerFixture,
        totalEventCount: headerFixture.totalEventCount + 1n,
      },
      transitionStepCount: {
        ...headerFixture,
        transitionStepCount: headerFixture.transitionStepCount + 1n,
      },
      validationTraceCount: {
        ...headerFixture,
        validationTraceCount: headerFixture.validationTraceCount + 1n,
      },
      startTime: { ...headerFixture, startTime: headerFixture.startTime + 1n },
      endTime: { ...headerFixture, endTime: headerFixture.endTime + 1n },
      blockSlot: { ...headerFixture, blockSlot: headerFixture.blockSlot + 1n },
      expectedNetworkId: {
        ...headerFixture,
        expectedNetworkId: headerFixture.expectedNetworkId === 0n ? 1n : 0n,
      },
      minFeeA: { ...headerFixture, minFeeA: headerFixture.minFeeA + 1n },
      minFeeB: { ...headerFixture, minFeeB: headerFixture.minFeeB + 1n },
      prevHeaderHash: {
        ...headerFixture,
        prevHeaderHash: differentDigest28,
      },
      operatorVkey: { ...headerFixture, operatorVkey: differentDigest28 },
      protocolVersion: null,
    };
    expect(Object.keys(mutations).sort()).toEqual(
      Object.keys(headerFixture).sort(),
    );

    const mutatedHashes = await Promise.all(
      Object.entries(mutations)
        .filter((entry): entry is [string, SDK.Header] => entry[1] !== null)
        .map(async ([field, mutation]) => {
          const hash = await Effect.runPromise(SDK.hashBlockHeader(mutation));
          expect(hash, `header field ${field} is not committed`).not.toBe(
            baselineHash,
          );
          return hash;
        }),
    );
    // Every field must move the digest to its own value, so two fields cannot
    // share one commitment slot.
    expect(new Set(mutatedHashes).size).toBe(mutatedHashes.length);

    // Reject pair: the encoder refuses a header that claims a protocol version
    // other than the one this ABI describes, so an unrecognised version can
    // never be hashed into a block commitment.
    expect(() =>
      SDK.hashBlockHeader({
        ...headerFixture,
        protocolVersion: headerFixture.protocolVersion + 1n,
      }),
    ).toThrow(/protocol version/i);
  });

  it("validates transition commitment count and root invariants", async () => {
    await expect(
      Effect.runPromise(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 0n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    ).resolves.toEqual(SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS);

    const nonEmptyDepositWithoutTrace = await Effect.runPromise(
      Effect.either(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: h32,
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 1n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    );
    expect(nonEmptyDepositWithoutTrace._tag).toBe("Left");
    if (nonEmptyDepositWithoutTrace._tag === "Left") {
      expect(nonEmptyDepositWithoutTrace.left.message).toContain(
        "empty transition_trace_root",
      );
    }

    await expect(
      Effect.runPromise(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: h32,
          transitionTraceRoot: "44".repeat(32),
          eventToStepRoot: "55".repeat(32),
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 1n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    ).resolves.toMatchObject({
      depositCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
    });

    const zeroCountForNonEmptyRoot = await Effect.runPromise(
      Effect.either(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: h32,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 0n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    );
    expect(zeroCountForNonEmptyRoot._tag).toBe("Left");

    const negativeCount = await Effect.runPromise(
      Effect.either(
        SDK.validateHeaderTransitionCommitmentsProgram({
          ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalCount: -1n,
        }),
      ),
    );
    expect(negativeCount._tag).toBe("Left");
  });

  it("encodes reserve and datum-based payout fixtures", () => {
    const payoutDatum: SDK.PayoutDatum = {
      l2_value: value,
      l1_address: address,
      l1_datum: "NoDatum",
    };
    expectRoundTrip(payoutDatum, SDK.PayoutDatum);

    expectRoundTrip(
      {
        MintPayout: {
          withdrawal_utxo_out_ref: { transactionId: h32, outputIndex: 0n },
          withdrawal_input_index: 1n,
          retirement_withdraw_redeemer_index: 2n,
          hub_ref_input_index: 3n,
        },
      },
      SDK.PayoutMintRedeemer,
    );

    expectRoundTrip(
      {
        BurnPayout: {
          payout_input_index: 0n,
          payout_asset_name: h32,
          payout_spend_redeemer_index: 1n,
          hub_ref_input_index: 2n,
        },
      },
      SDK.PayoutMintRedeemer,
    );

    expectRoundTrip(
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
    );

    expectRoundTrip(
      {
        ConcludeWithdrawal: {
          payout_input_index: 0n,
          l1_output_index: 1n,
          burn_redeemer_index: 2n,
          hub_ref_input_index: 3n,
        },
      },
      SDK.PayoutSpendRedeemer,
    );

    expectRoundTrip(
      {
        reserve_input_index: 0n,
        payout_input_index: 1n,
        payout_spend_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
      SDK.ReserveSpendRedeemer,
    );
  });
});
