import {
  type BuildTxWithRedeemer,
  Data,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  ActiveOperatorSpendRedeemer as ActiveOperatorSpendRedeemerSchema,
  type ActiveOperatorSpendRedeemer as ActiveOperatorSpendRedeemerType,
} from "../active-operators.js";
import {
  type SchedulerSpendRedeemer,
  SchedulerSpendRedeemer as SchedulerSpendRedeemerSchema,
} from "../scheduler.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import type { NodeWithDatum } from "./layout.js";
import {
  outputMatchesElement,
  requirePolicyNftUnit,
} from "./output-selectors.js";
import {
  type BuildStrikeInactiveOperatorTxConfig,
  schedulerError,
  type StrikeDatums,
  type StrikeInactiveOperatorLayout,
} from "./strike.plan-inactivity-takeover.js";

const safeTimeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw schedulerError(
      `${label} is outside the safe Lucid time range`,
      value.toString(),
    );
  }
  return Number(value);
};

const nodeLink = (node: NodeWithDatum): string | null =>
  node.datum.next === "Empty" ? null : node.datum.next.Key.key;

const witnessReferenceInputs = (
  config: BuildStrikeInactiveOperatorTxConfig,
): readonly UTxO[] => {
  const witnessNodes =
    config.witnesses.tier === "GoToNext"
      ? [config.witnesses.newOperatorNode.utxo]
      : [
          config.witnesses.activeRootNode.utxo,
          ...(config.witnesses.activeTailNode === null
            ? []
            : [config.witnesses.activeTailNode.utxo]),
          config.witnesses.registeredWitnessNode.utxo,
        ];
  return [
    config.hubOracleRefInput,
    config.stateQueueTailRefInput,
    ...witnessNodes,
    ...(config.neglectedEvent === undefined
      ? []
      : [config.neglectedEvent.utxo]),
  ];
};

export const deriveStrikeLayout = ({
  config,
  ctx,
  refreshedSchedulerDatumCbor,
  struckNodeDatumCbor,
  schedulerWitnessUnit,
}: {
  readonly config: BuildStrikeInactiveOperatorTxConfig;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly refreshedSchedulerDatumCbor: string;
  readonly struckNodeDatumCbor: string;
  readonly schedulerWitnessUnit: string;
}): StrikeInactiveOperatorLayout => {
  const shared = {
    schedulerInputIndex: requireInputIndex(
      ctx,
      config.schedulerInput,
      "inactivity strike scheduler",
    ),
    schedulerOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: config.scheduler.spendingScriptAddress,
          datum: refreshedSchedulerDatumCbor,
          unit: schedulerWitnessUnit,
        }),
      "inactivity strike scheduler",
    ),
    schedulerRedeemerIndex: requireSpendRedeemerIndex(
      ctx,
      config.schedulerInput,
      "inactivity strike scheduler",
    ),
    activeNodeInputIndex: requireInputIndex(
      ctx,
      config.skippedOperatorNode.utxo,
      "inactivity strike active node",
    ),
    activeNodeOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: config.activeOperators.spendingScriptAddress,
          datum: struckNodeDatumCbor,
          unit: requirePolicyNftUnit(
            config.skippedOperatorNode.utxo.assets,
            config.activeOperators.policyId,
            "inactivity strike active node assets",
          ),
        }),
      "inactivity strike active node",
    ),
    activeOperatorsSpendRedeemerIndex: requireSpendRedeemerIndex(
      ctx,
      config.skippedOperatorNode.utxo,
      "inactivity strike active node",
    ),
    hubOracleRefInputIndex: requireReferenceInputIndex(
      ctx,
      config.hubOracleRefInput,
      "inactivity strike hub oracle",
    ),
    stateQueueRefInputIndex: requireReferenceInputIndex(
      ctx,
      config.stateQueueTailRefInput,
      "inactivity strike state-queue tail",
    ),
    neglectedUserEventRefInputIndex:
      config.adversarialOverrides?.neglectedUserEventRefInputIndex ??
      (config.neglectedEvent === undefined
        ? undefined
        : requireReferenceInputIndex(
            ctx,
            config.neglectedEvent.utxo,
            `inactivity strike neglected ${config.neglectedEvent.kind}`,
          )),
  };
  if (config.witnesses.tier === "GoToNext") {
    return {
      ...shared,
      tier: "GoToNext",
      newOperatorNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        config.witnesses.newOperatorNode.utxo,
        "inactivity strike new operator node",
      ),
    };
  }
  const { activeRootNode, activeTailNode, registeredWitnessNode } =
    config.witnesses;
  return {
    ...shared,
    tier: "Rewind",
    activeRootRefInputIndex: requireReferenceInputIndex(
      ctx,
      activeRootNode.utxo,
      "inactivity strike active root",
    ),
    activeTailRefInputIndex:
      activeTailNode === null
        ? null
        : requireReferenceInputIndex(
            ctx,
            activeTailNode.utxo,
            "inactivity strike active tail",
          ),
    registeredWitnessRefInputIndex: requireReferenceInputIndex(
      ctx,
      registeredWitnessNode.utxo,
      "inactivity strike registered witness",
    ),
  };
};

const encodeNeglectedUserEvent = (
  config: BuildStrikeInactiveOperatorTxConfig,
  layout: StrikeInactiveOperatorLayout,
): unknown => {
  if (config.neglectedEvent === undefined) {
    return "NoNeglectedUserEvent";
  }
  const index = layout.neglectedUserEventRefInputIndex;
  if (index === undefined) {
    throw schedulerError(
      "Inactivity strike resolved no reference-input index for its neglected user event",
      config.neglectedEvent.kind,
    );
  }
  switch (config.neglectedEvent.kind) {
    case "Deposit":
      return { NeglectedDeposit: { deposit_ref_input_index: index } };
    case "Withdrawal":
      return { NeglectedWithdrawal: { withdrawal_ref_input_index: index } };
    case "TxOrder":
      return { NeglectedTxOrder: { tx_order_ref_input_index: index } };
  }
};

export const encodeSchedulerStrikeRedeemer = (
  config: BuildStrikeInactiveOperatorTxConfig,
  layout: StrikeInactiveOperatorLayout,
): string => {
  const neglected_user_event = encodeNeglectedUserEvent(config, layout);
  const advancing_approach =
    layout.tier === "GoToNext"
      ? {
          GoToNextDueToSkippedOperator: {
            new_shifts_operator_node_ref_input_index:
              layout.newOperatorNodeRefInputIndex,
            skipped_operator_node_input_index: layout.activeNodeInputIndex,
            active_operators_spend_redeemer_index:
              layout.activeOperatorsSpendRedeemerIndex,
            state_queue_ref_input_index: layout.stateQueueRefInputIndex,
            hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
            neglected_user_event,
          },
        }
      : {
          RewindDueToSkippedOperator: {
            active_operators_root_ref_input_index:
              layout.activeRootRefInputIndex,
            skipped_operator_node_input_index: layout.activeNodeInputIndex,
            active_operators_spend_redeemer_index:
              layout.activeOperatorsSpendRedeemerIndex,
            state_queue_ref_input_index: layout.stateQueueRefInputIndex,
            hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
            m_active_operators_last_node_ref_input_index:
              layout.activeTailRefInputIndex,
            registered_element_ref_input_index:
              layout.registeredWitnessRefInputIndex,
            neglected_user_event,
          },
        };
  const redeemer = {
    scheduler_input_index: layout.schedulerInputIndex,
    scheduler_output_index: layout.schedulerOutputIndex,
    advancing_approach,
  } as unknown as SchedulerSpendRedeemer;
  return Data.to(redeemer as never, SchedulerSpendRedeemerSchema as never);
};

export const encodeActiveOperatorStrikeRedeemer = (
  config: BuildStrikeInactiveOperatorTxConfig,
  layout: StrikeInactiveOperatorLayout,
): string => {
  const redeemer = {
    StrikeForInactivity: {
      active_node_input_index: layout.activeNodeInputIndex,
      active_node_output_index: layout.activeNodeOutputIndex,
      operator: config.skippedOperatorKeyHash,
      active_node_link:
        config.adversarialOverrides?.activeNodeLink === undefined
          ? nodeLink(config.skippedOperatorNode)
          : config.adversarialOverrides.activeNodeLink,
      scheduler_input_index: layout.schedulerInputIndex,
      scheduler_redeemer_index: layout.schedulerRedeemerIndex,
      hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    },
  } as unknown as ActiveOperatorSpendRedeemerType;
  return Data.to(redeemer as never, ActiveOperatorSpendRedeemerSchema as never);
};

/**
 * Assembles the strike transaction. Both script inputs receive either a
 * `BuildTxWithRedeemer` callback (first pass, resolving indices from the final
 * transaction context) or the resolved redeemer CBOR (second pass).
 */
export const buildStrikeInactiveOperatorTx = (
  config: BuildStrikeInactiveOperatorTxConfig,
  { struckNodeDatumCbor, refreshedSchedulerDatumCbor }: StrikeDatums,
  redeemers: {
    readonly scheduler: BuildTxWithRedeemer | string;
    readonly activeOperators: BuildTxWithRedeemer | string;
  },
): TxBuilder => {
  const scriptRefs = [
    ...(config.schedulerSpendingScriptRef === undefined
      ? []
      : [config.schedulerSpendingScriptRef]),
    ...(config.activeOperatorsSpendingScriptRef === undefined
      ? []
      : [config.activeOperatorsSpendingScriptRef]),
  ];
  let tx = config.lucid
    .newTx()
    .validFrom(safeTimeNumber(config.validFrom, "inactivity strike validFrom"))
    .validTo(safeTimeNumber(config.validTo, "inactivity strike validTo"))
    .readFrom([...witnessReferenceInputs(config), ...scriptRefs])
    .collectFrom([config.schedulerInput], redeemers.scheduler)
    .collectFrom([config.skippedOperatorNode.utxo], redeemers.activeOperators)
    .pay.ToContract(
      config.scheduler.spendingScriptAddress,
      { kind: "inline", value: refreshedSchedulerDatumCbor },
      config.schedulerInput.assets,
    )
    .pay.ToContract(
      config.activeOperators.spendingScriptAddress,
      { kind: "inline", value: struckNodeDatumCbor },
      config.skippedOperatorNode.utxo.assets,
    );
  if (config.schedulerSpendingScriptRef === undefined) {
    tx = tx.attach.Script(config.scheduler.spendingScript);
  }
  if (config.activeOperatorsSpendingScriptRef === undefined) {
    tx = tx.attach.Script(config.activeOperators.spendingScript);
  }
  return config.extraSignerKeyHash === undefined
    ? tx
    : tx.addSignerKey(config.extraSignerKeyHash);
};
