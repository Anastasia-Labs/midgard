import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeStateQueueYieldRedeemer,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
} from "../src/index.js";
import { roundTrip } from "./state-queue.build-test-contracts.js";
import { outputReference } from "./state-queue.state-queue-operator-funding-inputs.js";

describe("state-queue ABI", () => {
  it("encodes YieldStateQueueV1 as the canonical fieldless constructor", () => {
    expect(encodeStateQueueYieldRedeemer()).toBe("d87980");
  });

  it("uses the exact sole L04 InitV1 and MergeToConfirmedStateV1 language", () => {
    const init = { InitV1: { output_index: 2n } } as const;
    const initCbor = Data.to(init, StateQueueRedeemer);
    expect(initCbor).toBe("d8799f02ff");
    expect(roundTrip(init, StateQueueRedeemer)).toEqual(init);

    const proofMerge = {
      MergeToConfirmedStateV1: {
        yield_to_ref_input_index: 0n,
        header_node_key: "11".repeat(28),
        confirmed_state_input_outref: outputReference,
        confirmed_state_output_index: 0n,
        m_settlement_redeemer_index: 1n,
        merged_block_withdrawals_root: "21".repeat(32),
        merged_block_forced_transactions_root: "22".repeat(32),
        merged_block_transactions_root: "23".repeat(32),
        merged_block_deposits_root: "24".repeat(32),
        merged_block_transition_trace_root: "25".repeat(32),
        merged_block_event_to_step_root: "26".repeat(32),
        merged_block_validation_traces_root: "27".repeat(32),
        merged_block_withdrawal_count: 1n,
        merged_block_forced_transaction_count: 2n,
        merged_block_l2_transaction_count: 3n,
        merged_block_deposit_count: 4n,
        merged_block_total_event_count: 10n,
        merged_block_transition_step_count: 10n,
        merged_block_validation_trace_count: 5n,
      },
    } as const;
    const mergeCbor = Data.to(proofMerge, StateQueueRedeemer);
    expect(mergeCbor).toBe(
      "d87f9f00581c11111111111111111111111111111111111111111111111111111111d8799f5820444444444444444444444444444444444444444444444444444444444444444400ff00d8799f01ff58202121212121212121212121212121212121212121212121212121212121212121582022222222222222222222222222222222222222222222222222222222222222225820232323232323232323232323232323232323232323232323232323232323232358202424242424242424242424242424242424242424242424242424242424242424582025252525252525252525252525252525252525252525252525252525252525255820262626262626262626262626262626262626262626262626262626262626262658202727272727272727272727272727272727272727272727272727272727272727010203040a0a05ff",
    );
    expect(roundTrip(proofMerge, StateQueueRedeemer)).toEqual(proofMerge);
    expect(initCbor.startsWith("d8799f")).toBe(true);
    expect(mergeCbor.startsWith("d87f9f")).toBe(true);
    expect(() =>
      Data.to({ InitV2: { output_index: 2n } } as never, StateQueueRedeemer),
    ).toThrow();
    expect(() =>
      Data.to(
        {
          MergeToConfirmedStateV2: proofMerge.MergeToConfirmedStateV1,
        } as never,
        StateQueueRedeemer,
      ),
    ).toThrow();
    expect(() => Data.from("d87f80", StateQueueRedeemer)).toThrow();
  });

  it("round-trips CommitBlockHeader and RemoveFraudulentBlockHeader", () => {
    expect(
      roundTrip(
        {
          CommitBlockHeader: {
            yield_to_ref_input_index: 0n,
            new_block_output_index: 1n,
            continued_latest_block_output_index: 2n,
            operator: "11".repeat(28),
            scheduler_ref_input_index: 3n,
            active_operators_input_index: 4n,
            active_operators_redeemer_index: 5n,
            m_confirmed_state_ref_input_index: null,
            m_head_state_queue_node_ref_input_index: null,
          },
        },
        StateQueueRedeemer,
      ),
    ).toMatchObject({
      CommitBlockHeader: { active_operators_redeemer_index: 5n },
    });
    expect(roundTrip("LinkedListMutation", StateQueueSpendRedeemer)).toBe(
      "LinkedListMutation",
    );

    const removeRedeemer = {
      RemoveFraudulentBlockHeader: {
        yield_to_ref_input_index: 0n,
        fraudulent_operator: "22".repeat(28),
        fraudulent_blocks_header_hash: "33".repeat(28),
        slashing_approach: {
          OperatorAlreadySlashed: {
            active_operators_element_ref_input_index: 0n,
            retired_operators_element_ref_input_index: 1n,
          },
        },
        fraud_proof_ref_input_index: 3n,
        block_removal_approach: {
          RemoveLastFraudulentBlock: {
            anchor_element_input_outref: outputReference,
            anchor_element_output_index: 5n,
          },
        },
      },
    };
    expect(roundTrip(removeRedeemer, StateQueueRedeemer)).toEqual(
      removeRedeemer,
    );

    expect(
      roundTrip(
        {
          RemoveFraudulentBlockHeader: {
            ...removeRedeemer.RemoveFraudulentBlockHeader,
            slashing_approach: {
              SlashActiveOperator: {
                active_operators_redeemer_index: 6n,
                m_fraud_prover_reward_output_index: 8n,
              },
            },
            block_removal_approach: {
              RemoveFraudulentBlocksLink: {
                fraudulent_node_input_outref: outputReference,
                fraudulent_node_output_index: 7n,
              },
            },
          },
        },
        StateQueueRedeemer,
      ),
    ).toMatchObject({
      RemoveFraudulentBlockHeader: {
        slashing_approach: {
          SlashActiveOperator: {
            active_operators_redeemer_index: 6n,
            m_fraud_prover_reward_output_index: 8n,
          },
        },
      },
    });

    // D3: a bond-consuming slash that routes no reward encodes the index as
    // `null`, which the on-chain guard accepts only while the compiled
    // `env.fraud_prover_reward` is zero.
    const rewardlessRetiredSlash = {
      RemoveFraudulentBlockHeader: {
        ...removeRedeemer.RemoveFraudulentBlockHeader,
        slashing_approach: {
          SlashRetiredOperator: {
            retired_operators_redeemer_index: 9n,
            m_fraud_prover_reward_output_index: null,
          },
        },
      },
    };
    expect(roundTrip(rewardlessRetiredSlash, StateQueueRedeemer)).toEqual(
      rewardlessRetiredSlash,
    );
  });
});
