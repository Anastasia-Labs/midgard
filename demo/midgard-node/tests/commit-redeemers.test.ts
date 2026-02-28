import { describe, expect, it } from "vitest";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import {
  ActiveOperatorSpendRedeemer,
  DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
  deriveStateQueueCommitLayout,
  encodeActiveOperatorCommitRedeemer,
  encodeStateQueueCommitRedeemer,
  makeActiveOperatorCommitRedeemer,
  makeStateQueueCommitRedeemer,
} from "@/workers/utils/commit-redeemers.js";

describe("commit redeemer shapes", () => {
  it("builds state_queue CommitBlockHeader with expected index layout", () => {
    const operator = "11".repeat(28);
    const redeemer = makeStateQueueCommitRedeemer(operator);
    expect(redeemer).toEqual({
      CommitBlockHeader: {
        operator,
        scheduler_ref_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.schedulerRefInputIndex,
        active_node_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.activeNodeInputIndex,
        header_node_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.headerNodeOutputIndex,
        previous_header_node_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.previousHeaderNodeOutputIndex,
        active_operators_redeemer_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.activeOperatorsRedeemerIndex,
      },
    });
  });

  it("encodes non-void state_queue commit redeemer cbor", () => {
    const encoded = encodeStateQueueCommitRedeemer("22".repeat(28));
    expect(encoded).not.toEqual(Data.void());
    expect(Data.from(encoded, SDK.StateQueueRedeemer)).toEqual(
      makeStateQueueCommitRedeemer("22".repeat(28)),
    );
  });

  it("encodes active-operators UpdateBondHoldNewState redeemer", () => {
    const redeemer = makeActiveOperatorCommitRedeemer();
    expect(redeemer).toEqual({
      UpdateBondHoldNewState: {
        active_node_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.activeNodeOutputIndex,
        hub_oracle_ref_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.hubOracleRefInputIndex,
        state_queue_redeemer_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.stateQueueRedeemerIndex,
      },
    });
    const encoded = encodeActiveOperatorCommitRedeemer();
    expect(encoded).not.toEqual(Data.void());
    expect(Data.from(encoded, ActiveOperatorSpendRedeemer)).toEqual(redeemer);
  });

  it("derives dynamic input/redeemer indexes from out-ref ordering", () => {
    const latest = {
      txHash: "bb".repeat(32),
      outputIndex: 1,
    };
    const active = {
      txHash: "bb".repeat(32),
      outputIndex: 3,
    };
    const fee = {
      txHash: "aa".repeat(32),
      outputIndex: 0,
    };
    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      txInputs: [latest, active, fee],
    });
    expect(layout.activeNodeInputIndex).toEqual(2n);
    expect(layout.activeOperatorsRedeemerIndex).toEqual(1n);
  });
});
