import { assetsEqual } from "@al-ft/midgard-core/assets";
import {
  type Assets,
  type BuildTxWithRedeemer,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { OutputReference } from "./common.js";
import { type Header } from "./ledger-state.js";
import { type SettlementMintRedeemer as SettlementMintRedeemerType } from "./settlement.js";
import {
  type StateQueueRedeemer as StateQueueRedeemerType,
  type StateQueueUTxO,
} from "./state-queue.js";
import { type MergeRedeemerLayout } from "./state-queue-transactions.build-commit-block-header-tx-program.js";
import { requireUniqueContextOutputIndex } from "./state-queue-transactions.commit-layout-fields.js";
import {
  requireMintRedeemerIndex as requireContextMintRedeemerIndex,
  requireReferenceInputIndex as requireContextReferenceInputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

export const makeStateQueueMergeRedeemer = ({
  layout,
  headerNodeKey,
  blockHeader,
  confirmedStateInputOutRef,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly confirmedStateInputOutRef: OutputReference;
}): StateQueueRedeemerType => {
  const common = {
    yield_to_ref_input_index: BigInt(layout.yieldToRefInputIndex),
    header_node_key: headerNodeKey,
    confirmed_state_input_outref: confirmedStateInputOutRef,
    confirmed_state_output_index: BigInt(layout.confirmedStateOutputIndex),
    m_settlement_redeemer_index: BigInt(layout.settlementRedeemerIndex),
    merged_block_withdrawals_root: blockHeader.withdrawalsRoot,
    merged_block_forced_transactions_root: blockHeader.forcedTransactionsRoot,
    merged_block_transactions_root: blockHeader.transactionsRoot,
    merged_block_deposits_root: blockHeader.depositsRoot,
    merged_block_transition_trace_root: blockHeader.transitionTraceRoot,
    merged_block_event_to_step_root: blockHeader.eventToStepRoot,
    merged_block_withdrawal_count: blockHeader.withdrawalCount,
    merged_block_forced_transaction_count: blockHeader.forcedTransactionCount,
    merged_block_l2_transaction_count: blockHeader.l2TransactionCount,
    merged_block_deposit_count: blockHeader.depositCount,
    merged_block_total_event_count: blockHeader.totalEventCount,
    merged_block_transition_step_count: blockHeader.transitionStepCount,
    merged_block_validation_traces_root: blockHeader.validationTracesRoot,
    merged_block_validation_trace_count: blockHeader.validationTraceCount,
  };
  return { MergeToConfirmedStateV1: common };
};

export const makeSettlementSpawnRedeemer = ({
  layout,
  headerNodeKey,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
}): SettlementMintRedeemerType => ({
  Spawn: {
    settlement_id: headerNodeKey,
    output_index: BigInt(layout.settlementOutputIndex),
    state_queue_merge_redeemer_index: BigInt(layout.stateQueueRedeemerIndex),
    hub_ref_input_index: BigInt(layout.hubOracleRefInputIndex),
  },
});

export const deriveMergeLayoutFromRedeemerContext = ({
  ctx,
  confirmedUTxO,
  hubOracleRefInput,
  stateQueueMergeYieldRefInput,
  stateQueuePolicyId,
  stateQueueAddress,
  encodedConfirmedNodeDatum,
  settlementPolicyId,
  settlementAddress,
  encodedSettlementDatum,
  settlementOutputAssets,
}: {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly confirmedUTxO: StateQueueUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly stateQueueMergeYieldRefInput: UTxO;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly encodedConfirmedNodeDatum: string;
  readonly settlementPolicyId: string;
  readonly settlementAddress: string;
  readonly encodedSettlementDatum: string;
  readonly settlementOutputAssets: Assets;
}): MergeRedeemerLayout => ({
  yieldToRefInputIndex: Number(
    requireContextReferenceInputIndex(
      ctx,
      stateQueueMergeYieldRefInput,
      "state-queue merge yield target",
    ),
  ),
  confirmedStateOutputIndex: Number(
    requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === stateQueueAddress &&
        outputDatumCborMatches(output, encodedConfirmedNodeDatum) &&
        assetsEqual(output.assets, confirmedUTxO.utxo.assets),
      "state-queue merge confirmed state",
    ),
  ),
  settlementOutputIndex: Number(
    requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === settlementAddress &&
        outputDatumCborMatches(output, encodedSettlementDatum) &&
        assetsEqual(output.assets, settlementOutputAssets),
      "state-queue merge settlement",
    ),
  ),
  stateQueueRedeemerIndex: Number(
    requireContextMintRedeemerIndex(
      ctx,
      stateQueuePolicyId,
      "state-queue merge state_queue mint",
    ),
  ),
  settlementRedeemerIndex: Number(
    requireContextMintRedeemerIndex(
      ctx,
      settlementPolicyId,
      "state-queue merge settlement mint",
    ),
  ),
  hubOracleRefInputIndex: Number(
    requireContextReferenceInputIndex(
      ctx,
      hubOracleRefInput,
      "state-queue merge hub oracle",
    ),
  ),
});
