import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

const {
  DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
  deriveStateQueueCommitLayout,
  encodeActiveOperatorCommitRedeemer,
  encodeStateQueueCommitRedeemer,
  makeActiveOperatorCommitRedeemer,
  makeStateQueueCommitRedeemer,
} = SDK;

const ref = (byte: string, outputIndex: number) => ({
  txHash: byte.repeat(32),
  outputIndex,
});

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
    expect(Data.from(encoded, SDK.ActiveOperatorSpendRedeemer)).toEqual(
      redeemer,
    );
  });

  it("derives dynamic input/redeemer indexes from out-ref ordering", () => {
    const latest = ref("bb", 1);
    const active = ref("bb", 3);
    const fee = ref("aa", 0);
    const schedulerRef = ref("cc", 1);
    const hubOracleRef = ref("aa", 9);
    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      schedulerRefInput: schedulerRef,
      hubOracleRefInput: hubOracleRef,
      txInputs: [latest, active, fee],
    });
    expect(layout).toMatchObject({
      latestBlockInputIndex: 1n,
      activeOperatorsInputIndex: 2n,
      stateQueueSpendRedeemerIndex: 0n,
      activeOperatorsRedeemerIndex: 1n,
      schedulerRefInputIndex: 1n,
      hubOracleRefInputIndex: 0n,
    });
  });

  it("derives active-operators redeemer index as 0 when active input sorts before latest block input", () => {
    const latest = ref("bb", 1);
    const active = ref("aa", 9);
    const fee = ref("aa", 0);
    const schedulerRef = ref("aa", 4);
    const hubOracleRef = ref("ff", 0);

    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      schedulerRefInput: schedulerRef,
      hubOracleRefInput: hubOracleRef,
      txInputs: [latest, fee, active],
    });

    expect(layout).toMatchObject({
      latestBlockInputIndex: 2n,
      activeOperatorsInputIndex: 1n,
      stateQueueSpendRedeemerIndex: 1n,
      activeOperatorsRedeemerIndex: 0n,
      schedulerRefInputIndex: 0n,
      hubOracleRefInputIndex: 1n,
    });

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
    const latest = ref("cc", 2);
    const active = ref("cc", 1);
    const fee = ref("bb", 0);
    const schedulerRef = ref("cc", 5);
    const hubOracleRef = ref("cc", 4);

    const layout = deriveStateQueueCommitLayout({
      latestBlockInput: latest,
      activeOperatorInput: active,
      schedulerRefInput: schedulerRef,
      hubOracleRefInput: hubOracleRef,
      txInputs: [latest, active, fee],
    });

    expect(layout).toMatchObject({
      latestBlockInputIndex: 2n,
      activeOperatorsInputIndex: 1n,
      stateQueueSpendRedeemerIndex: 1n,
      activeOperatorsRedeemerIndex: 0n,
      schedulerRefInputIndex: 1n,
      hubOracleRefInputIndex: 0n,
    });
  });

  it("throws when active operator input is missing from tx inputs", () => {
    const latest = ref("dd", 0);
    const active = ref("ee", 1);
    const fee = ref("ff", 2);
    const schedulerRef = ref("11", 0);
    const hubOracleRef = ref("22", 0);

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
    expect(
      Data.from(encodedActiveOperator, SDK.ActiveOperatorSpendRedeemer),
    ).toEqual(makeActiveOperatorCommitRedeemer(operator, customLayout));
  });
});
