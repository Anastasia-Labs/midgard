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
        latest_block_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.latestBlockInputIndex,
        new_block_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.newBlockOutputIndex,
        continued_latest_block_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.continuedLatestBlockOutputIndex,
        scheduler_ref_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.schedulerRefInputIndex,
        active_operators_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.activeOperatorsInputIndex,
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
    const operator = "11".repeat(28);
    const redeemer = makeActiveOperatorCommitRedeemer(operator);
    expect(redeemer).toEqual({
      UpdateBondHoldNewState: {
        active_operator: operator,
        active_node_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.activeOperatorsInputIndex,
        active_node_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.activeOperatorOutputIndex,
        hub_oracle_ref_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.hubOracleRefInputIndex,
        state_queue_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.latestBlockInputIndex,
        state_queue_redeemer_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.stateQueueSpendRedeemerIndex,
      },
    });
    const encoded = encodeActiveOperatorCommitRedeemer(operator);
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
    const schedulerRef = {
      txHash: "cc".repeat(32),
      outputIndex: 1,
    };
    const hubOracleRef = {
      txHash: "aa".repeat(32),
      outputIndex: 9,
    };
    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      schedulerRefInput: schedulerRef,
      hubOracleRefInput: hubOracleRef,
      txInputs: [latest, active, fee],
    });
    expect(layout.latestBlockInputIndex).toEqual(1n);
    expect(layout.activeOperatorsInputIndex).toEqual(2n);
    expect(layout.stateQueueSpendRedeemerIndex).toEqual(0n);
    expect(layout.activeOperatorsRedeemerIndex).toEqual(1n);
    expect(layout.schedulerRefInputIndex).toEqual(1n);
    expect(layout.hubOracleRefInputIndex).toEqual(0n);
  });

  it("derives active-operators redeemer index as 0 when active input sorts before latest block input", () => {
    const latest = {
      txHash: "bb".repeat(32),
      outputIndex: 1,
    };
    const active = {
      txHash: "aa".repeat(32),
      outputIndex: 9,
    };
    const fee = {
      txHash: "aa".repeat(32),
      outputIndex: 0,
    };
    const schedulerRef = {
      txHash: "aa".repeat(32),
      outputIndex: 4,
    };
    const hubOracleRef = {
      txHash: "ff".repeat(32),
      outputIndex: 0,
    };

    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      schedulerRefInput: schedulerRef,
      hubOracleRefInput: hubOracleRef,
      txInputs: [latest, fee, active],
    });

    expect(layout.latestBlockInputIndex).toEqual(2n);
    expect(layout.activeOperatorsInputIndex).toEqual(1n);
    expect(layout.stateQueueSpendRedeemerIndex).toEqual(1n);
    expect(layout.activeOperatorsRedeemerIndex).toEqual(0n);
    expect(layout.schedulerRefInputIndex).toEqual(0n);
    expect(layout.hubOracleRefInputIndex).toEqual(1n);

    const operator = "33".repeat(28);
    const stateQueueRedeemer = makeStateQueueCommitRedeemer(operator, layout);
    expect(stateQueueRedeemer).toEqual({
      CommitBlockHeader: {
        operator,
        latest_block_input_index: 2n,
        new_block_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.newBlockOutputIndex,
        continued_latest_block_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.continuedLatestBlockOutputIndex,
        scheduler_ref_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.schedulerRefInputIndex,
        active_operators_input_index: 1n,
        active_operators_redeemer_index: 0n,
      },
    });
  });

  it("derives input ordering with same tx hash using output index ordering", () => {
    const latest = {
      txHash: "cc".repeat(32),
      outputIndex: 2,
    };
    const active = {
      txHash: "cc".repeat(32),
      outputIndex: 1,
    };
    const fee = {
      txHash: "bb".repeat(32),
      outputIndex: 0,
    };
    const schedulerRef = {
      txHash: "cc".repeat(32),
      outputIndex: 5,
    };
    const hubOracleRef = {
      txHash: "cc".repeat(32),
      outputIndex: 4,
    };

    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      schedulerRefInput: schedulerRef,
      hubOracleRefInput: hubOracleRef,
      txInputs: [latest, active, fee],
    });

    expect(layout.latestBlockInputIndex).toEqual(2n);
    expect(layout.activeOperatorsInputIndex).toEqual(1n);
    expect(layout.stateQueueSpendRedeemerIndex).toEqual(1n);
    expect(layout.activeOperatorsRedeemerIndex).toEqual(0n);
    expect(layout.schedulerRefInputIndex).toEqual(1n);
    expect(layout.hubOracleRefInputIndex).toEqual(0n);
  });

  it("throws when active operator input is missing from tx inputs", () => {
    const latest = {
      txHash: "dd".repeat(32),
      outputIndex: 0,
    };
    const active = {
      txHash: "ee".repeat(32),
      outputIndex: 1,
    };
    const fee = {
      txHash: "ff".repeat(32),
      outputIndex: 2,
    };
    const schedulerRef = {
      txHash: "11".repeat(32),
      outputIndex: 0,
    };
    const hubOracleRef = {
      txHash: "22".repeat(32),
      outputIndex: 0,
    };

    expect(() =>
      deriveStateQueueCommitLayout({
        latestBlockInput: latest,
        activeOperatorInput: active,
        schedulerRefInput: schedulerRef,
        hubOracleRefInput: hubOracleRef,
        txInputs: [latest, fee],
      }),
    ).toThrow(
      `Active operator input ${active.txHash}#${active.outputIndex} missing from tx input set`,
    );
  });

  it("encodes both redeemers with indices from the provided layout", () => {
    const customLayout = {
      ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
      schedulerRefInputIndex: 1n,
      latestBlockInputIndex: 1n,
      activeOperatorsInputIndex: 2n,
      newBlockOutputIndex: 2n,
      continuedLatestBlockOutputIndex: 0n,
      activeOperatorsRedeemerIndex: 0n,
      activeOperatorOutputIndex: 3n,
      hubOracleRefInputIndex: 1n,
      stateQueueSpendRedeemerIndex: 1n,
    } as const;

    const operator = "44".repeat(28);
    const encodedStateQueue = encodeStateQueueCommitRedeemer(
      operator,
      customLayout,
    );
    const encodedActiveOperator = encodeActiveOperatorCommitRedeemer(
      operator,
      customLayout,
    );

    expect(Data.from(encodedStateQueue, SDK.StateQueueRedeemer)).toEqual(
      makeStateQueueCommitRedeemer(operator, customLayout),
    );
    expect(Data.from(encodedActiveOperator, ActiveOperatorSpendRedeemer)).toEqual(
      makeActiveOperatorCommitRedeemer(operator, customLayout),
    );
  });
});
