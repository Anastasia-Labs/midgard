import { Data as LucidData } from "@lucid-evolution/lucid";

import {
  ActiveOperatorMintRedeemer,
  type ActiveOperatorMintRedeemer as ActiveOperatorMintRedeemerType,
  type OperatorRemovalSchedulerSync,
} from "../active-operators.js";
import { outputReferenceFromUTxO } from "../common.js";
import {
  RetiredOperatorMintRedeemer,
  type RetiredOperatorMintRedeemer as RetiredOperatorMintRedeemerType,
} from "../retired-operators.js";
import {
  type SchedulerSpendRedeemer,
  SchedulerSpendRedeemer as SchedulerSpendRedeemerSchema,
} from "../scheduler.js";
import {
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  type AdvancingSchedulerSync,
  exitError,
  type RedeemerContext,
} from "./exit.complete-in-two-passes.js";
import {
  deriveRetireSchedulerSyncLayout,
  type RetireOperatorTxConfig,
  type RetirePlan,
  type RetireRedeemerLayout,
  type RetireSchedulerSyncLayout,
} from "./exit.derive-retire-scheduler-sync-layout.js";
import {
  outputMatchesElement,
  requirePolicyNftUnit,
} from "./output-selectors.js";

export const deriveRetireLayout = (
  config: RetireOperatorTxConfig,
  ctx: RedeemerContext,
  plan: RetirePlan,
): RetireRedeemerLayout => {
  const { activeOperators, retiredOperators } = config.contracts;
  return {
    hubOracleRefInputIndex: requireReferenceInputIndex(
      ctx,
      config.hubOracleRefInput,
      "operator retirement hub oracle",
    ),
    activeOperatorsRedeemerIndex: requireMintRedeemerIndex(
      ctx,
      activeOperators.policyId,
      "operator retirement active mint",
    ),
    retiredOperatorsRedeemerIndex: requireMintRedeemerIndex(
      ctx,
      retiredOperators.policyId,
      "operator retirement retired mint",
    ),
    activeAnchorNodeInputOutRef: outputReferenceFromUTxO(
      config.activeAnchor.utxo,
    ),
    activeAnchorNodeOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: activeOperators.spendingScriptAddress,
          datum: plan.updatedActiveAnchorDatumCbor,
          unit: requirePolicyNftUnit(
            config.activeAnchor.utxo.assets,
            activeOperators.policyId,
            "operator retirement active anchor assets",
          ),
        }),
      "operator retirement updated active anchor",
    ),
    retiredAnchorNodeOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: retiredOperators.spendingScriptAddress,
          datum: plan.updatedRetiredAnchorDatumCbor,
          unit: requirePolicyNftUnit(
            config.retiredInsertionAnchor.utxo.assets,
            retiredOperators.policyId,
            "operator retirement retired anchor assets",
          ),
        }),
      "operator retirement updated retired anchor",
    ),
    retiredInsertedNodeOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: retiredOperators.spendingScriptAddress,
          datum: plan.insertedRetiredNodeDatumCbor,
          unit: config.retiredNodeUnit,
        }),
      "operator retirement inserted retired node",
    ),
    schedulerSync: deriveRetireSchedulerSyncLayout(config, ctx, plan.scheduler),
  };
};

/**
 * The advancing scheduler route and its indices together. The layout and the
 * config each carry one half; they only disagree when a caller pins a layout
 * derived for a different route, which is refused rather than encoded wrong.
 */
const advancingSchedulerRoute = (
  config: RetireOperatorTxConfig,
  layout: RetireRedeemerLayout,
): {
  readonly sync: AdvancingSchedulerSync;
  readonly indices: Extract<
    RetireSchedulerSyncLayout,
    { kind: "SchedulerIsAdvancing" }
  >;
} => {
  const sync = config.schedulerSync;
  const indices = layout.schedulerSync;
  if (
    sync.kind !== "SchedulerIsAdvancing" ||
    indices.kind !== "SchedulerIsAdvancing"
  ) {
    throw exitError(
      "Operator retirement layout and scheduler sync disagree on the scheduler route",
      config.operatorKeyHash,
    );
  }
  return { sync, indices };
};

const encodeRemovalSchedulerSync = (
  config: RetireOperatorTxConfig,
  layout: RetireRedeemerLayout,
): OperatorRemovalSchedulerSync => {
  if (layout.schedulerSync.kind === "OperatorIsInactive") {
    return {
      ShowOperatorIsInactive: {
        scheduler_ref_input_index: layout.schedulerSync.schedulerRefInputIndex,
      },
    };
  }
  const { sync, indices } = advancingSchedulerRoute(config, layout);
  return {
    ShowSchedulerIsAdvancing: {
      scheduler_input_index: indices.schedulerInputIndex,
      scheduler_redeemer_index: indices.schedulerRedeemerIndex,
      removing_operators_anchor_element_key:
        sync.removingOperatorsAnchorElementKey,
      removing_operator_is_the_last_member:
        sync.removingOperatorIsTheLastMember,
    },
  };
};

export const encodeActiveRetireRedeemer = (
  config: RetireOperatorTxConfig,
  layout: RetireRedeemerLayout,
): string => {
  const redeemer: ActiveOperatorMintRedeemerType = {
    RetireOperator: {
      active_operator_key: config.operatorKeyHash,
      hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
      active_operator_anchor_element_input_outref:
        layout.activeAnchorNodeInputOutRef,
      active_operator_anchor_element_output_index:
        layout.activeAnchorNodeOutputIndex,
      retired_operators_redeemer_index: layout.retiredOperatorsRedeemerIndex,
      penalize_for_inactivity: config.mode === "forced-inactivity",
      operator_removal_scheduler_sync: encodeRemovalSchedulerSync(
        config,
        layout,
      ),
    },
  };
  return LucidData.to(redeemer, ActiveOperatorMintRedeemer);
};

export const encodeRetiredRetireRedeemer = (
  config: RetireOperatorTxConfig,
  layout: RetireRedeemerLayout,
): string => {
  const redeemer: RetiredOperatorMintRedeemerType = {
    RetireOperator: {
      new_retired_operator_key: config.operatorKeyHash,
      bond_unlock_time: config.bondUnlockTime,
      hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
      retired_operator_anchor_element_output_index:
        layout.retiredAnchorNodeOutputIndex,
      retired_operator_inserted_node_output_index:
        layout.retiredInsertedNodeOutputIndex,
      active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
    },
  };
  return LucidData.to(redeemer, RetiredOperatorMintRedeemer);
};

export const encodeRetireSchedulerRedeemer = (
  config: RetireOperatorTxConfig,
  layout: RetireRedeemerLayout,
): string => {
  const { sync, indices } = advancingSchedulerRoute(config, layout);
  let advancingApproach: SchedulerSpendRedeemer["advancing_approach"];
  if (sync.removingOperatorsAnchorElementKey !== null) {
    advancingApproach = {
      GoToNextDueToOperatorRemoval: {
        active_operators_mint_redeemer_index:
          indices.activeOperatorsMintRedeemerIndex,
        removal_reason: "OperatorRetirement",
      },
    };
  } else {
    if (indices.registeredWitnessRefInputIndex === null) {
      throw exitError(
        "Rewinding the scheduler on retirement needs a registered-list witness",
        config.operatorKeyHash,
      );
    }
    advancingApproach = {
      RewindDueToOperatorRemoval: {
        active_operators_mint_redeemer_index:
          indices.activeOperatorsMintRedeemerIndex,
        m_active_operators_last_node_ref_input_index:
          indices.activeTailRefInputIndex,
        removal_reason: "OperatorRetirement",
        registered_element_ref_input_index:
          indices.registeredWitnessRefInputIndex,
      },
    };
  }
  const redeemer: SchedulerSpendRedeemer = {
    scheduler_input_index: indices.schedulerInputIndex,
    scheduler_output_index: indices.schedulerOutputIndex,
    advancing_approach: advancingApproach,
  };
  return LucidData.to(redeemer as never, SchedulerSpendRedeemerSchema as never);
};

export const forcedRetirementPenalty = (
  config: RetireOperatorTxConfig,
): bigint => {
  if (config.inactivitySlashingPenaltyLovelace === undefined) {
    throw exitError(
      "A forced retirement must reserve the inactivity penalty as its fee",
      config.operatorKeyHash,
    );
  }
  return config.inactivitySlashingPenaltyLovelace;
};
