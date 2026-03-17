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

    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      txInputs: [latest, fee, active],
    });

    expect(layout.activeNodeInputIndex).toEqual(1n);
    expect(layout.activeOperatorsRedeemerIndex).toEqual(0n);

    const operator = "33".repeat(28);
    const stateQueueRedeemer = makeStateQueueCommitRedeemer(operator, layout);
    expect(stateQueueRedeemer).toEqual({
      CommitBlockHeader: {
        operator,
        scheduler_ref_input_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.schedulerRefInputIndex,
        active_node_input_index: 1n,
        header_node_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.headerNodeOutputIndex,
        previous_header_node_output_index:
          DEFAULT_STATE_QUEUE_COMMIT_LAYOUT.previousHeaderNodeOutputIndex,
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

    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      txInputs: [latest, active, fee],
    });

    expect(layout.activeNodeInputIndex).toEqual(1n);
    expect(layout.activeOperatorsRedeemerIndex).toEqual(0n);
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

    expect(() =>
      deriveStateQueueCommitLayout({
        latestBlockInput: latest,
        activeOperatorInput: active,
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
      activeNodeInputIndex: 2n,
      headerNodeOutputIndex: 2n,
      previousHeaderNodeOutputIndex: 0n,
      activeOperatorsRedeemerIndex: 0n,
      activeNodeOutputIndex: 3n,
      hubOracleRefInputIndex: 1n,
      stateQueueRedeemerIndex: 0n,
    } as const;

    const operator = "44".repeat(28);
    const encodedStateQueue = encodeStateQueueCommitRedeemer(
      operator,
      customLayout,
    );
    const encodedActiveOperator = encodeActiveOperatorCommitRedeemer(customLayout);

    expect(Data.from(encodedStateQueue, SDK.StateQueueRedeemer)).toEqual(
      makeStateQueueCommitRedeemer(operator, customLayout),
    );
    expect(Data.from(encodedActiveOperator, ActiveOperatorSpendRedeemer)).toEqual(
      makeActiveOperatorCommitRedeemer(customLayout),
    );
  });
});
