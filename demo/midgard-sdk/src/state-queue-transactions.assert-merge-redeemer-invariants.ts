import { Data } from "@lucid-evolution/lucid";

import type { OutputReference } from "./common.js";
import { type Header } from "./ledger-state.js";
import {
  SettlementMintRedeemer,
  type SettlementMintRedeemer as SettlementMintRedeemerType,
} from "./settlement.js";
import {
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "./state-queue.js";
import { type MergeRedeemerLayout } from "./state-queue-transactions.build-commit-block-header-tx-program.js";

export const assertMergeRedeemerInvariants = ({
  layout,
  headerNodeKey,
  blockHeader,
  confirmedStateInputOutRef,
  encodedStateQueueMergeRedeemer,
  encodedSettlementSpawnRedeemer,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly confirmedStateInputOutRef: OutputReference;
  readonly encodedStateQueueMergeRedeemer: string;
  readonly encodedSettlementSpawnRedeemer: string;
}): void => {
  const decodedStateQueue = Data.from(
    encodedStateQueueMergeRedeemer,
    StateQueueRedeemer,
  ) as StateQueueRedeemerType;
  const decodedSettlement = Data.from(
    encodedSettlementSpawnRedeemer,
    SettlementMintRedeemer,
  ) as SettlementMintRedeemerType;
  const mismatches: string[] = [];

  const stateQueueMerge =
    typeof decodedStateQueue !== "object" || decodedStateQueue === null
      ? undefined
      : "MergeToConfirmedStateV1" in decodedStateQueue
        ? decodedStateQueue.MergeToConfirmedStateV1
        : undefined;
  if (stateQueueMerge === undefined) {
    mismatches.push("state_queue variant mismatch");
  } else {
    if (
      stateQueueMerge.yield_to_ref_input_index !==
      BigInt(layout.yieldToRefInputIndex)
    ) {
      mismatches.push("state_queue.yield_to_ref_input_index mismatch");
    }
    if (stateQueueMerge.header_node_key !== headerNodeKey) {
      mismatches.push("state_queue.header_node_key mismatch");
    }
    if (
      stateQueueMerge.confirmed_state_input_outref.transactionId !==
      confirmedStateInputOutRef.transactionId
    ) {
      mismatches.push("state_queue.confirmed_state_input_outref tx mismatch");
    }
    if (
      stateQueueMerge.confirmed_state_input_outref.outputIndex !==
      confirmedStateInputOutRef.outputIndex
    ) {
      mismatches.push(
        "state_queue.confirmed_state_input_outref index mismatch",
      );
    }
    if (
      stateQueueMerge.confirmed_state_output_index !==
      BigInt(layout.confirmedStateOutputIndex)
    ) {
      mismatches.push("state_queue.confirmed_state_output_index mismatch");
    }
    if (
      stateQueueMerge.m_settlement_redeemer_index !==
      BigInt(layout.settlementRedeemerIndex)
    ) {
      mismatches.push("state_queue.m_settlement_redeemer_index mismatch");
    }
    if (
      stateQueueMerge.merged_block_withdrawals_root !==
      blockHeader.withdrawalsRoot
    ) {
      mismatches.push("state_queue.withdrawals_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_forced_transactions_root !==
      blockHeader.forcedTransactionsRoot
    ) {
      mismatches.push("state_queue.forced_transactions_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_transactions_root !==
      blockHeader.transactionsRoot
    ) {
      mismatches.push("state_queue.transactions_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_deposits_root !== blockHeader.depositsRoot
    ) {
      mismatches.push("state_queue.deposits_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_transition_trace_root !==
      blockHeader.transitionTraceRoot
    ) {
      mismatches.push("state_queue.transition_trace_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_event_to_step_root !==
      blockHeader.eventToStepRoot
    ) {
      mismatches.push("state_queue.event_to_step_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_withdrawal_count !==
      blockHeader.withdrawalCount
    ) {
      mismatches.push("state_queue.withdrawal_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_forced_transaction_count !==
      blockHeader.forcedTransactionCount
    ) {
      mismatches.push("state_queue.forced_transaction_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_l2_transaction_count !==
      blockHeader.l2TransactionCount
    ) {
      mismatches.push("state_queue.l2_transaction_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_deposit_count !== blockHeader.depositCount
    ) {
      mismatches.push("state_queue.deposit_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_total_event_count !==
      blockHeader.totalEventCount
    ) {
      mismatches.push("state_queue.total_event_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_transition_step_count !==
      blockHeader.transitionStepCount
    ) {
      mismatches.push("state_queue.transition_step_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_validation_traces_root !==
      blockHeader.validationTracesRoot
    ) {
      mismatches.push("state_queue.validation_traces_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_validation_trace_count !==
      blockHeader.validationTraceCount
    ) {
      mismatches.push("state_queue.validation_trace_count mismatch");
    }
  }

  if (!("Spawn" in decodedSettlement)) {
    mismatches.push("settlement variant mismatch");
  } else {
    const settlementSpawn = decodedSettlement.Spawn;
    if (settlementSpawn.settlement_id !== headerNodeKey) {
      mismatches.push("settlement.settlement_id mismatch");
    }
    if (settlementSpawn.output_index !== BigInt(layout.settlementOutputIndex)) {
      mismatches.push("settlement.output_index mismatch");
    }
    if (
      settlementSpawn.state_queue_merge_redeemer_index !==
      BigInt(layout.stateQueueRedeemerIndex)
    ) {
      mismatches.push("settlement.state_queue_merge_redeemer_index mismatch");
    }
    if (
      settlementSpawn.hub_ref_input_index !==
      BigInt(layout.hubOracleRefInputIndex)
    ) {
      mismatches.push("settlement.hub_ref_input_index mismatch");
    }
  }

  if (mismatches.length > 0) {
    throw new Error(JSON.stringify({ mismatches, layout }));
  }
};
