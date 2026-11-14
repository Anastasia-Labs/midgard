import {
  Data as LucidData,
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  bountyClaimantAddress,
  requireExactBountyRouting,
} from "../bounty-routing.js";
import { type MidgardValidators, outputReferenceFromUTxO } from "../common.js";
import { encodeLinkedListNodeView } from "../linked-list.js";
import {
  type DuplicateOperatorStatus,
  RegisteredOperatorMintRedeemer,
  type RegisteredOperatorMintRedeemer as RegisteredOperatorMintRedeemerType,
} from "../registered-operators.js";
import {
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  type ExactFeeBalancePlan,
  exactFeeCompleteOptions,
  type ExactFeeOutputPlan,
  withSettledLovelace,
} from "./exact-fee.js";
import {
  applyExactFeePlan,
  completeInTwoPasses,
  type ContractOutput,
  exitError,
  failExit,
  type LayoutOptions,
  LIST_STATE_TRANSITION_VOID_REDEEMER,
  OperatorExitError,
  payContractOutputs,
  planExactFee,
  type RedeemerContext,
  redeemerEncoder,
  requireExplicitWalletInputs,
  safeTimeNumber,
} from "./exit.complete-in-two-passes.js";
import {
  type DuplicateProof,
  type SlashDuplicateOperatorRedeemerLayout,
} from "./exit.derive-retire-operator-witnesses.js";
import type { NodeWithDatum, ReferenceScriptPublication } from "./layout.js";
import {
  outputMatchesElement,
  requirePolicyNftUnit,
} from "./output-selectors.js";

export type SlashDuplicateOperatorTxConfig =
  LayoutOptions<SlashDuplicateOperatorRedeemerLayout> & {
    readonly lucid: LucidEvolution;
    readonly contracts: MidgardValidators;
    /** The duplicated operator key. */
    readonly operatorKeyHash: string;
    readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
    /** The duplicate REGISTERED node being removed. */
    readonly duplicateRegisteredNode: NodeWithDatum;
    readonly registeredAnchor: NodeWithDatum;
    readonly duplicateRegisteredNodeUnit: string;
    /** The pre-existing membership that makes the removed node a duplicate. */
    readonly duplicateProof: DuplicateProof;
    /** On-chain the fee must equal the slashing penalty exactly. */
    readonly slashingPenaltyLovelace: bigint;
    /**
     * The key the bounty is paid to. It must sign the transaction, and the
     * bounty goes to its enterprise address.
     */
    readonly bountyClaimantKeyHash: string;
    /** The compiled `fraud_prover_reward`, paid to the claimant exactly. */
    readonly fraudProverRewardLovelace: bigint;
    /**
     * Set by `buildUnsignedSlashDuplicateOperatorTxProgram`. Callers that
     * drive `buildSlashDuplicateOperatorTx` themselves have to balance the
     * pinned fee with `planExactFeeBalance` and pass the plan here.
     */
    readonly exactFeePlan?: ExactFeeBalancePlan;
    /**
     * The submitter's coins, as the caller's view holds them. When given, the
     * pinned-fee balance and its collateral use exactly these and the
     * provider is never read.
     */
    readonly walletInputs?: readonly UTxO[];
    readonly validFrom?: bigint;
    readonly validTo?: bigint;
  };

const updatedRegisteredAnchorDatumCbor = (
  config: SlashDuplicateOperatorTxConfig,
): string =>
  encodeLinkedListNodeView({
    ...config.registeredAnchor.datum,
    next: config.duplicateRegisteredNode.datum.next,
  });

/**
 * The continued registered anchor. The removed node's bond pays the penalty
 * as the fee and the bounty to the claimant (`slashBountyOutputs`); a bond
 * holding exactly the required amount leaves nothing over.
 */
const slashDeclaredOutputs = (
  config: SlashDuplicateOperatorTxConfig,
  anchorDatumCbor: string,
): readonly ContractOutput[] => [
  withSettledLovelace(
    config.lucid,
    {
      address: config.contracts.registeredOperators.spendingScriptAddress,
      datumCbor: anchorDatumCbor,
      assets: config.registeredAnchor.utxo.assets,
    },
    "Duplicate-operator slashing's declared output",
  ),
];

const claimantAddress = (config: SlashDuplicateOperatorTxConfig): string => {
  const network = config.lucid.config().network;
  if (network === undefined) {
    throw exitError(
      "Duplicate-operator slashing needs the network to derive the bounty address",
      config.operatorKeyHash,
    );
  }
  return bountyClaimantAddress(network, config.bountyClaimantKeyHash);
};

/**
 * The bounty output: exactly the compiled reward, ADA only, to the claimant's
 * enterprise address. A zero reward pays no output at all, which is the only
 * case the validator accepts without one.
 */
const slashBountyOutputs = (
  config: SlashDuplicateOperatorTxConfig,
): readonly ExactFeeOutputPlan[] => {
  const reward = config.fraudProverRewardLovelace;
  if (reward === 0n) {
    return [];
  }
  const output = {
    address: claimantAddress(config),
    assets: { lovelace: reward },
  };
  const settled = withSettledLovelace(
    config.lucid,
    output,
    "Duplicate-operator slashing bounty",
  );
  if (settled.assets["lovelace"] !== reward) {
    throw exitError(
      `Duplicate-operator slashing bounty of ${reward.toString()} lovelace is below the minimum output`,
      config.operatorKeyHash,
    );
  }
  return [output];
};

const deriveSlashLayout = (
  config: SlashDuplicateOperatorTxConfig,
  ctx: RedeemerContext,
  anchorDatumCbor: string,
): SlashDuplicateOperatorRedeemerLayout => {
  const { registeredOperators } = config.contracts;
  if (config.fraudProverRewardLovelace !== 0n) {
    requireExactBountyRouting(
      ctx.outputs,
      {
        claimantAddress: claimantAddress(config),
        lovelace: config.fraudProverRewardLovelace,
      },
      "duplicate-operator slashing bounty",
    );
  }
  return {
    registeredAnchorNodeInputOutRef: outputReferenceFromUTxO(
      config.registeredAnchor.utxo,
    ),
    registeredAnchorNodeOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: registeredOperators.spendingScriptAddress,
          datum: anchorDatumCbor,
          unit: requirePolicyNftUnit(
            config.registeredAnchor.utxo.assets,
            registeredOperators.policyId,
            "duplicate-operator slashing registered anchor assets",
          ),
        }),
      "duplicate-operator slashing updated registered anchor",
    ),
    duplicateNodeRefInputIndex: requireReferenceInputIndex(
      ctx,
      config.duplicateProof.node.utxo,
      "duplicate-operator slashing proof node",
    ),
    hubOracleRefInputIndex:
      config.duplicateProof.kind === "active"
        ? requireReferenceInputIndex(
            ctx,
            config.duplicateProof.hubOracleRefInput,
            "duplicate-operator slashing hub oracle",
          )
        : null,
  };
};

const encodeDuplicateOperatorStatus = (
  config: SlashDuplicateOperatorTxConfig,
  layout: SlashDuplicateOperatorRedeemerLayout,
): DuplicateOperatorStatus => {
  switch (config.duplicateProof.kind) {
    case "registered":
      return "DuplicateIsRegistered";
    case "retired":
      return "DuplicateIsRetired";
    case "active": {
      if (layout.hubOracleRefInputIndex === null) {
        throw exitError(
          "Slashing a duplicate of an active operator needs the hub oracle reference input",
          config.operatorKeyHash,
        );
      }
      return {
        DuplicateIsActive: {
          hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        },
      };
    }
  }
};

const encodeSlashDuplicateRedeemer = (
  config: SlashDuplicateOperatorTxConfig,
  layout: SlashDuplicateOperatorRedeemerLayout,
): string => {
  const redeemer: RegisteredOperatorMintRedeemerType = {
    SlashDuplicateOperator: {
      duplicate_operator: config.operatorKeyHash,
      anchor_element_input_outref: layout.registeredAnchorNodeInputOutRef,
      anchor_element_output_index: layout.registeredAnchorNodeOutputIndex,
      duplicate_node_ref_input_index: layout.duplicateNodeRefInputIndex,
      duplicate_operator_status: encodeDuplicateOperatorStatus(config, layout),
      bounty_claimant: config.bountyClaimantKeyHash,
    },
  };
  return LucidData.to(redeemer, RegisteredOperatorMintRedeemer);
};

/**
 * Removes a duplicate registration, pays exactly the slashing penalty as the
 * transaction fee and the bounty exactly to the named claimant, who signs.
 */
export const buildSlashDuplicateOperatorTx = (
  config: SlashDuplicateOperatorTxConfig,
): TxBuilder => {
  const anchorDatumCbor = updatedRegisteredAnchorDatumCbor(config);
  const redeemer = redeemerEncoder(config, (ctx) =>
    deriveSlashLayout(config, ctx, anchorDatumCbor),
  );
  const proof = config.duplicateProof;
  let tx = config.lucid
    .newTx()
    .collectFrom(
      [config.registeredAnchor.utxo, config.duplicateRegisteredNode.utxo],
      LIST_STATE_TRANSITION_VOID_REDEEMER,
    )
    .readFrom([
      ...config.registeredOperatorScriptRefs.map(({ utxo }) => utxo),
      proof.node.utxo,
      ...(proof.kind === "active" ? [proof.hubOracleRefInput] : []),
    ])
    .mintAssets(
      { [config.duplicateRegisteredNodeUnit]: -1n },
      redeemer(
        (ctx) =>
          requireOwnMintPurpose(
            ctx,
            config.contracts.registeredOperators.policyId,
            "duplicate-operator slashing registered mint",
          ),
        (layout) => encodeSlashDuplicateRedeemer(config, layout),
      ),
    )
    .addSignerKey(config.bountyClaimantKeyHash)
    .setMinFee(config.slashingPenaltyLovelace);
  tx = payContractOutputs(tx, slashDeclaredOutputs(config, anchorDatumCbor));
  for (const bounty of slashBountyOutputs(config)) {
    tx = tx.pay.ToAddress(bounty.address, bounty.assets);
  }
  if (config.validFrom !== undefined) {
    tx = tx.validFrom(
      safeTimeNumber(config.validFrom, "duplicate-operator slashing validFrom"),
    );
  }
  if (config.validTo !== undefined) {
    tx = tx.validTo(
      safeTimeNumber(config.validTo, "duplicate-operator slashing validTo"),
    );
  }
  return config.exactFeePlan === undefined
    ? tx
    : applyExactFeePlan(tx, config.exactFeePlan);
};

export type SlashDuplicateOperatorTxResult = {
  readonly tx: TxSignBuilder;
  readonly layout: SlashDuplicateOperatorRedeemerLayout;
};

export const buildUnsignedSlashDuplicateOperatorTxProgram = (
  config: SlashDuplicateOperatorTxConfig,
): Effect.Effect<SlashDuplicateOperatorTxResult, OperatorExitError> =>
  Effect.gen(function* () {
    yield* requireExplicitWalletInputs(
      config.walletInputs,
      "a duplicate-operator slashing",
    );
    const bounty = yield* Effect.try({
      try: () => ({
        address: claimantAddress(config),
        outputs: slashBountyOutputs(config),
      }),
      catch: (cause) =>
        cause instanceof OperatorExitError
          ? cause
          : exitError(
              `Failed to plan the duplicate-operator slashing bounty: ${String(cause)}`,
              cause,
            ),
    });
    const plan =
      config.exactFeePlan ??
      (yield* planExactFee({
        lucid: config.lucid,
        label: "Duplicate-operator slashing",
        walletInputs: config.walletInputs,
        feeLovelace: config.slashingPenaltyLovelace,
        scriptInputs: [
          config.registeredAnchor.utxo,
          config.duplicateRegisteredNode.utxo,
        ],
        declaredOutputs: [
          ...slashDeclaredOutputs(
            config,
            updatedRegisteredAnchorDatumCbor(config),
          ),
          ...bounty.outputs,
        ],
      }));
    // The validator accepts exactly one output to the claimant, so a
    // remainder paid to the same address would be refused on chain.
    if (
      plan.remainderLovelace !== 0n &&
      plan.remainderAddress === bounty.address
    ) {
      return yield* failExit(
        `Duplicate-operator slashing would pay ${plan.remainderLovelace.toString()} lovelace of change to the bounty claimant's address; the claimant may receive only the bounty output`,
        config.operatorKeyHash,
      );
    }
    return yield* completeInTwoPasses({
      label: "duplicate-operator slashing",
      operatorKeyHash: config.operatorKeyHash,
      onLayout: config.onLayout,
      build: (layout) =>
        buildSlashDuplicateOperatorTx({
          ...config,
          exactFeePlan: plan,
          ...layout,
        }),
      options: exactFeeCompleteOptions(plan),
      exactFee: {
        plan,
        violation:
          "Duplicate-operator slashing must pay exactly the slashing penalty as its fee",
      },
    });
  });
