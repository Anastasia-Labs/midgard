import {
  type ActiveOperatorMintRedeemer as ActiveOperatorMintRedeemerData,
  outputReferenceFromUTxO,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  type RetiredOperatorMintRedeemer as RetiredOperatorMintRedeemerData,
  type SchedulerSpendRedeemer as SchedulerSpendRedeemerData,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type OperatorSlashingLayout } from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import {
  type OperatorSlashingLayoutContext,
  type RemoveFraudulentBlockSlashing,
  type RemoveTransactionResult,
  requireOutputIndexByUnit,
  type SchedulerRemovalPlan,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";

export type SlashingTxPlan = {
  readonly approach: RemoveTransactionResult["slashingApproach"];
  readonly removedOperatorNodeOutRef: string | null;
  readonly registeredOperatorsElementOutRef: string | null;
  readonly buildSlashing: (
    schedulerStartTime: bigint,
  ) => RemoveFraudulentBlockSlashing;
  readonly additionalRefInputs: readonly UTxO[];
};

const requireLayoutIndex = (
  value: bigint | undefined,
  label: string,
): bigint => {
  if (value === undefined) {
    throw new Error(`Missing ${label} in remove-fraudulent-block layout.`);
  }
  return value;
};

export const requireLayoutUtxo = (
  utxo: UTxO | undefined,
  label: string,
): UTxO => {
  if (utxo === undefined) {
    throw new Error(
      `Missing ${label} in remove-fraudulent-block layout context.`,
    );
  }
  return utxo;
};

export const makeLayoutRedeemer = <T>({
  layoutContext,
  encode,
  schema,
  bindOwnPurpose,
}: {
  readonly layoutContext: OperatorSlashingLayoutContext;
  readonly encode: (layout: OperatorSlashingLayout) => T;
  readonly schema: T;
  readonly bindOwnPurpose: (ctx: Parameters<BuildTxWithRedeemer>[0]) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    bindOwnPurpose(ctx);
    const layout = deriveOperatorSlashingLayoutFromRedeemerContext({
      ctx,
      ...layoutContext,
    });
    return Data.to(encode(layout) as never, schema as never);
  }) satisfies BuildTxWithRedeemer;

export const makeActiveOperatorsMintRedeemerFromPlan =
  ({
    operator,
    schedulerPlan,
    activeOperatorAnchorKey,
  }: {
    readonly operator: string;
    readonly schedulerPlan: SchedulerRemovalPlan;
    readonly activeOperatorAnchorKey: string | null;
  }) =>
  (layout: OperatorSlashingLayout): ActiveOperatorMintRedeemerData => {
    const operatorRemovalSchedulerSync =
      schedulerPlan.kind === "inactive"
        ? {
            ShowOperatorIsInactive: {
              scheduler_ref_input_index: requireLayoutIndex(
                layout.schedulerRefInputIndex,
                "schedulerRefInputIndex",
              ),
            },
          }
        : {
            ShowSchedulerIsAdvancing: {
              scheduler_input_index: requireLayoutIndex(
                layout.schedulerInputIndex,
                "schedulerInputIndex",
              ),
              scheduler_redeemer_index: requireLayoutIndex(
                layout.schedulerRedeemerTxInfoIndex,
                "schedulerRedeemerTxInfoIndex",
              ),
              removing_operators_anchor_element_key: activeOperatorAnchorKey,
              removing_operator_is_the_last_member:
                schedulerPlan.removedNodeIsLast,
            },
          };

    return {
      SlashOperator: {
        slashing_arguments: {
          slashed_operator: operator,
          hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
          slashed_operator_anchor_element_input_outref:
            layout.operatorDirectoryAnchorInputOutRef,
          slashed_operator_anchor_element_output_index:
            layout.operatorDirectoryAnchorOutputIndex,
          slashing_reason: {
            SlashOperatorForBadState: {
              state_queue_redeemer_index: layout.stateQueueRedeemerTxInfoIndex,
            },
          },
        },
        operator_removal_scheduler_sync: operatorRemovalSchedulerSync,
      },
    };
  };

export const makeRetiredOperatorsMintRedeemerFromPlan =
  ({ operator }: { readonly operator: string }) =>
  (layout: OperatorSlashingLayout): RetiredOperatorMintRedeemerData => ({
    SlashOperator: {
      slashing_arguments: {
        slashed_operator: operator,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        slashed_operator_anchor_element_input_outref:
          layout.operatorDirectoryAnchorInputOutRef,
        slashed_operator_anchor_element_output_index:
          layout.operatorDirectoryAnchorOutputIndex,
        slashing_reason: {
          SlashOperatorForBadState: {
            state_queue_redeemer_index: layout.stateQueueRedeemerTxInfoIndex,
          },
        },
      },
    },
  });

export const makeSchedulerSpendRedeemerFromPlan =
  ({
    schedulerPlan,
  }: {
    readonly schedulerPlan: Exclude<SchedulerRemovalPlan, { kind: "inactive" }>;
  }) =>
  (layout: OperatorSlashingLayout): SchedulerSpendRedeemerData => {
    const scheduler_input_index = requireLayoutIndex(
      layout.schedulerInputIndex,
      "schedulerInputIndex",
    );
    const scheduler_output_index = requireLayoutIndex(
      layout.schedulerOutputIndex,
      "schedulerOutputIndex",
    );
    return {
      scheduler_input_index,
      scheduler_output_index,
      advancing_approach:
        schedulerPlan.kind === "goToAnchor"
          ? {
              GoToNextDueToOperatorRemoval: {
                active_operators_mint_redeemer_index: requireLayoutIndex(
                  layout.activeOperatorsRedeemerTxInfoIndex,
                  "activeOperatorsRedeemerTxInfoIndex",
                ),
                removal_reason: "OperatorSlashing",
              },
            }
          : {
              RewindDueToOperatorRemoval: {
                active_operators_mint_redeemer_index: requireLayoutIndex(
                  layout.activeOperatorsRedeemerTxInfoIndex,
                  "activeOperatorsRedeemerTxInfoIndex",
                ),
                m_active_operators_last_node_ref_input_index:
                  layout.activeOperatorsLastNodeRefInputIndex ?? null,
                removal_reason: "OperatorSlashing",
                registered_element_ref_input_index: requireLayoutIndex(
                  layout.registeredOperatorsElementRefInputIndex,
                  "registeredOperatorsElementRefInputIndex",
                ),
              },
            },
    };
  };

const deriveOperatorSlashingLayoutFromRedeemerContext = ({
  ctx,
  ...layoutContext
}: OperatorSlashingLayoutContext & {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
}): OperatorSlashingLayout => {
  const {
    operatorDirectoryAnchor,
    hubOracle,
    operatorDirectoryAnchorUnit,
    slashedOperatorDirectory,
    contracts,
  } = layoutContext;
  const slashingPolicyId =
    slashedOperatorDirectory === "active"
      ? contracts.activeOperatorsPolicyId
      : contracts.retiredOperatorsPolicyId;
  const slashingDirectoryLabel =
    slashedOperatorDirectory === "active"
      ? "active-operators"
      : "retired-operators";
  const slashingRedeemerTxInfoIndex = requireMintRedeemerIndex(
    ctx,
    slashingPolicyId,
    `${slashingDirectoryLabel} slash`,
  );
  const baseLayout: OperatorSlashingLayout = {
    ...(slashedOperatorDirectory === "active"
      ? { activeOperatorsRedeemerTxInfoIndex: slashingRedeemerTxInfoIndex }
      : { retiredOperatorsRedeemerTxInfoIndex: slashingRedeemerTxInfoIndex }),
    stateQueueRedeemerTxInfoIndex: requireMintRedeemerIndex(
      ctx,
      contracts.stateQueuePolicyId,
      "state-queue remove fraudulent block",
    ),
    operatorDirectoryAnchorInputOutRef: outputReferenceFromUTxO(
      operatorDirectoryAnchor,
    ),
    operatorDirectoryAnchorOutputIndex: requireOutputIndexByUnit({
      outputs: ctx.outputs,
      address:
        slashedOperatorDirectory === "active"
          ? contracts.activeOperatorsAddress
          : contracts.retiredOperatorsAddress,
      unit: operatorDirectoryAnchorUnit,
      label: `${slashingDirectoryLabel} anchor continuation`,
    }),
    hubOracleRefInputIndex: requireReferenceInputIndex(
      ctx,
      hubOracle,
      "hub-oracle reference input",
    ),
  };

  if (slashedOperatorDirectory === "retired") {
    return baseLayout;
  }

  const {
    scheduler,
    schedulerPlan,
    registeredOperatorsElement,
    activeOperatorsLastNode,
    schedulerUnit,
  } = layoutContext;
  return {
    ...baseLayout,
    ...(schedulerPlan.kind === "inactive"
      ? {
          schedulerRefInputIndex: requireReferenceInputIndex(
            ctx,
            scheduler,
            "scheduler reference input",
          ),
        }
      : {
          schedulerInputIndex: requireInputIndex(
            ctx,
            scheduler,
            "scheduler input",
          ),
          schedulerOutputIndex: requireOutputIndexByUnit({
            outputs: ctx.outputs,
            address: contracts.schedulerAddress,
            unit: schedulerUnit,
            label: "scheduler continuation",
          }),
          schedulerRedeemerTxInfoIndex: requireSpendRedeemerIndex(
            ctx,
            scheduler,
            "scheduler spend redeemer",
          ),
        }),
    ...(activeOperatorsLastNode === undefined
      ? {}
      : {
          activeOperatorsLastNodeRefInputIndex: requireReferenceInputIndex(
            ctx,
            activeOperatorsLastNode,
            "active-operators last-node reference input",
          ),
        }),
    ...(schedulerPlan.kind === "rewind"
      ? {
          registeredOperatorsElementRefInputIndex: requireReferenceInputIndex(
            ctx,
            requireLayoutUtxo(
              registeredOperatorsElement,
              "registered-operators terminal element",
            ),
            "registered-operators terminal element reference input",
          ),
        }
      : {}),
  };
};
