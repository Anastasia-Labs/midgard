/**
 * Operator exit: retirement (voluntary or forced by inactivity), bond
 * recovery, and duplicate-registration slashing.
 *
 * These are the removal counterparts of `buildRegisterOperatorTx` /
 * `buildActivateOperatorTx`, and they follow the same shape: a synchronous
 * `TxBuilder` builder whose redeemers may be resolved from the redeemer
 * context, plus an Effect program that builds twice — once with callbacks to
 * learn the layout, once with a static layout — because the redeemer's own
 * size moves the fee and therefore the indices.
 *
 * The forced retirement and the slashing pin their fee to a protocol penalty,
 * so their programs also balance the transaction themselves; see
 * `./exact-fee.ts`.
 */
import {
  type BuildTxWithRedeemer,
  Data as LucidData,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  ActiveOperatorMintRedeemer,
  type ActiveOperatorMintRedeemer as ActiveOperatorMintRedeemerType,
  ActiveOperatorSpendRedeemer,
  type OperatorRemovalSchedulerSync,
} from "../active-operators.js";
import {
  type MidgardValidators,
  type OutputReference,
  outputReferenceFromUTxO,
} from "../common.js";
import type { GenericErrorFields } from "../errors.js";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  encodeLinkedListNodeView,
  type LinkedListNodeView,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
} from "../linked-list.js";
import {
  type DuplicateOperatorStatus,
  RegisteredOperatorMintRedeemer,
  type RegisteredOperatorMintRedeemer as RegisteredOperatorMintRedeemerType,
} from "../registered-operators.js";
import {
  castRetiredOperatorDatumToData,
  RetiredOperatorMintRedeemer,
  type RetiredOperatorMintRedeemer as RetiredOperatorMintRedeemerType,
} from "../retired-operators.js";
import {
  SCHEDULER_ASSET_NAME,
  type SchedulerDatum,
  type SchedulerSpendRedeemer,
  SchedulerSpendRedeemer as SchedulerSpendRedeemerSchema,
} from "../scheduler.js";
import { encodeSchedulerDatumForChain } from "../scheduler-refresh.js";
import {
  completeOptionsWithLocalEval,
  type TxCompleteOptions,
} from "../tx-completion.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  findAnchorNodeForKey,
  findNodeByKey,
  findTailNode,
  nodeKeyHex,
  type OperatorDirectorySnapshot,
  registeredNodeKeyToPosixTime,
  schedulerCurrentOperator,
} from "./directory.js";
import {
  type ExactFeeBalancePlan,
  exactFeeCompleteOptions,
  type ExactFeeOutputPlan,
  exactFeeViolation,
  planExactFeeBalance,
  withSettledLovelace,
} from "./exact-fee.js";
import type { NodeWithDatum, ReferenceScriptPublication } from "./layout.js";
import { orderedNotMemberWitness } from "./layout.js";
import {
  outputMatchesElement,
  requirePolicyNftUnit,
} from "./output-selectors.js";

export class OperatorExitError extends EffectData.TaggedError(
  "OperatorExitError",
)<GenericErrorFields> {}

const exitError = (message: string, cause: unknown): OperatorExitError =>
  new OperatorExitError({ message, cause });

const failExit = (
  message: string,
  cause: unknown,
): Effect.Effect<never, OperatorExitError> =>
  Effect.fail(exitError(message, cause));

const ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER = LucidData.to(
  "ListStateTransition",
  ActiveOperatorSpendRedeemer,
);

/** The retired/registered spend validators only require that the list mints. */
const LIST_STATE_TRANSITION_VOID_REDEEMER = LucidData.void();

const safeTimeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw exitError(
      `${label} is outside the safe Lucid time range`,
      value.toString(),
    );
  }
  return Number(value);
};

export const activeOperatorNodeUnit = (
  activeOperatorsPolicyId: string,
  operatorKeyHash: string,
): string =>
  toUnit(
    activeOperatorsPolicyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );

export const retiredOperatorNodeUnit = (
  retiredOperatorsPolicyId: string,
  operatorKeyHash: string,
): string =>
  toUnit(
    retiredOperatorsPolicyId,
    RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );

// ---------------------------------------------------------------------------
// Build plumbing shared by the three endpoints
// ---------------------------------------------------------------------------

type RedeemerContext = Parameters<BuildTxWithRedeemer>[0];

/** A contract output the builder declares: address, inline datum, value. */
type ContractOutput = ExactFeeOutputPlan & { readonly datumCbor: string };

/**
 * `layout` pins the redeemer indices; without it every redeemer is resolved
 * from the redeemer context and reported through `onLayout`.
 */
type LayoutOptions<L> = {
  readonly layout?: L;
  readonly onLayout?: (layout: L) => void;
};

/**
 * Returns the redeemer factory for one build. With a pinned layout each
 * redeemer encodes statically; otherwise each is a callback that checks its
 * own purpose, derives the layout from the context, and reports it.
 */
const redeemerEncoder = <L>(
  options: LayoutOptions<L>,
  derive: (ctx: RedeemerContext) => L,
) => {
  const pinned = options.layout;
  if (pinned !== undefined) {
    options.onLayout?.(pinned);
  }
  return (
    guard: (ctx: RedeemerContext) => void,
    encode: (layout: L) => string,
  ): string | BuildTxWithRedeemer =>
    pinned === undefined
      ? (ctx) => {
          guard(ctx);
          const layout = derive(ctx);
          options.onLayout?.(layout);
          return encode(layout);
        }
      : encode(pinned);
};

const payContractOutputs = (
  tx: TxBuilder,
  outputs: readonly ContractOutput[],
): TxBuilder =>
  outputs.reduce(
    (acc, output) =>
      acc.pay.ToContract(
        output.address,
        { kind: "inline", value: output.datumCbor },
        output.assets,
      ),
    tx,
  );

/** Spends the plan's funding coins and pays the leftover it computed. */
const applyExactFeePlan = (
  tx: TxBuilder,
  plan: ExactFeeBalancePlan,
): TxBuilder => {
  const funded =
    plan.fundingInputs.length === 0
      ? tx
      : tx.collectFrom([...plan.fundingInputs]);
  return plan.remainderLovelace === 0n
    ? funded
    : funded.pay.ToAddress(plan.remainderAddress, {
        lovelace: plan.remainderLovelace,
      });
};

/**
 * Balances a pinned-fee build against the submitting wallet's plain coins.
 * The endpoint's own inputs and declared outputs rarely add up to the fee on
 * their own (a list anchor whose link grew needs a min-ADA top-up), so the
 * wallet makes up the difference and takes the leftover back in one output.
 */
/**
 * Plans a pinned-fee balance from the wallet's coins as the provider holds
 * them. The wallet's own view may be a cached prediction of an earlier
 * transaction's change, and an exact-fee transaction spends explicit inputs
 * with no coin selection, so a coin the ledger no longer holds would fail at
 * submission rather than be re-selected.
 */
const planExactFee = (input: {
  readonly lucid: LucidEvolution;
  readonly label: string;
  readonly feeLovelace: bigint;
  readonly scriptInputs: readonly UTxO[];
  readonly declaredOutputs: readonly ContractOutput[];
}): Effect.Effect<ExactFeeBalancePlan, OperatorExitError> =>
  Effect.tryPromise({
    try: async () => {
      const remainderAddress = await input.lucid.wallet().address();
      return planExactFeeBalance({
        ...input,
        walletUtxos: await input.lucid.utxosAt(remainderAddress),
        remainderAddress,
      });
    },
    catch: (cause) =>
      exitError(`Failed to balance the pinned fee: ${String(cause)}`, cause),
  });

/**
 * Lucid's first pass resolves the layout from the redeemer context; the
 * second pins it, because the redeemer's own encoded size feeds back into the
 * fee and therefore into the indices. For a pinned-fee build the completed
 * transaction is then checked against its plan: on chain the fee is an
 * equality, so a fee Lucid raised would make the transaction unsubmittable.
 */
const completeInTwoPasses = <L>({
  label,
  operatorKeyHash,
  onLayout,
  build,
  options,
  exactFee,
}: {
  readonly label: string;
  readonly operatorKeyHash: string;
  readonly onLayout: ((layout: L) => void) | undefined;
  readonly build: (layout: LayoutOptions<L>) => TxBuilder;
  readonly options: TxCompleteOptions;
  readonly exactFee?: {
    readonly plan: ExactFeeBalancePlan;
    readonly violation: string;
  };
}): Effect.Effect<
  { readonly tx: TxSignBuilder; readonly layout: L },
  OperatorExitError
> =>
  Effect.gen(function* () {
    const complete = (layout: LayoutOptions<L>, verb: string) =>
      Effect.tryPromise({
        try: () => build(layout).complete(options),
        catch: (cause) =>
          exitError(
            `Failed to ${verb} the ${label} tx: ${String(cause)}`,
            cause,
          ),
      });
    let resolved: L | undefined;
    yield* complete(
      {
        onLayout: (layout) => {
          resolved = layout;
          onLayout?.(layout);
        },
      },
      "build",
    );
    if (resolved === undefined) {
      return yield* failExit(
        `The ${label} did not resolve a redeemer layout`,
        operatorKeyHash,
      );
    }
    const layout = resolved;
    const tx = yield* complete({ layout }, "rebuild");
    if (exactFee !== undefined) {
      const violation = exactFeeViolation(tx, exactFee.plan);
      if (violation !== null) {
        return yield* failExit(exactFee.violation, violation);
      }
    }
    return { tx, layout };
  });

// ---------------------------------------------------------------------------
// Retirement
// ---------------------------------------------------------------------------

export type OperatorBondParameters = {
  readonly requiredBondLovelace: bigint;
  readonly inactivitySlashingPenaltyLovelace: bigint;
};

/**
 * `voluntary` needs the operator's signature and a strike count below the
 * maximum; `forced-inactivity` needs the strike count at or above the maximum,
 * may be submitted by anyone, and burns exactly the inactivity penalty as the
 * transaction fee.
 */
export type RetirementMode = "voluntary" | "forced-inactivity";

/**
 * Lovelace the retired node must hold. On-chain
 * (`transferred_operator_bond_tranche_is_exact_v1`) this is exact: the full
 * bond for a voluntary retirement, the bond less the inactivity penalty when
 * the retirement is forced (the penalty leaves as the transaction fee).
 */
export const retiredOperatorBondTranche = (
  mode: RetirementMode,
  params: OperatorBondParameters,
): bigint =>
  mode === "forced-inactivity"
    ? params.requiredBondLovelace - params.inactivitySlashingPenaltyLovelace
    : params.requiredBondLovelace;

/**
 * How the retirement keeps the scheduler consistent.
 *
 * `OperatorIsInactive` only has to reference the scheduler, and applies when
 * the scheduler names nobody or names somebody else. When the scheduler names
 * the retiring operator, the shift has to move in the same transaction, which
 * means spending the scheduler with `GoToNextDueToOperatorRemoval` (the
 * removed node had an anchor node, which becomes the new shift's operator) or
 * `RewindDueToOperatorRemoval` (the removed node's anchor was the root).
 */
export type RetireSchedulerSync =
  | {
      readonly kind: "OperatorIsInactive";
      readonly schedulerRefInput: UTxO;
    }
  | {
      readonly kind: "SchedulerIsAdvancing";
      readonly schedulerInput: UTxO;
      readonly refreshedDatum: SchedulerDatum;
      /**
       * The removed node's anchor key, or `null` when the anchor is the root.
       * `null` selects `RewindDueToOperatorRemoval`, `Some` selects
       * `GoToNextDueToOperatorRemoval`.
       */
      readonly removingOperatorsAnchorElementKey: string | null;
      /** Whether the removed node was the last one in the active list. */
      readonly removingOperatorIsTheLastMember: boolean;
      /**
       * Rewind only, and only when another active node survives: the list tail
       * whose key becomes the new shift's operator.
       */
      readonly activeTailRefNode?: NodeWithDatum;
      /**
       * Rewind only: the registered-list tail, proving no registration is
       * eligible to activate instead.
       */
      readonly registeredWitnessNode?: NodeWithDatum;
      readonly schedulerSpendingScriptRef?: UTxO;
    };

type AdvancingSchedulerSync = Extract<
  RetireSchedulerSync,
  { kind: "SchedulerIsAdvancing" }
>;

export type RetireSchedulerSyncLayout =
  | {
      readonly kind: "OperatorIsInactive";
      readonly schedulerRefInputIndex: bigint;
    }
  | {
      readonly kind: "SchedulerIsAdvancing";
      readonly schedulerInputIndex: bigint;
      readonly schedulerRedeemerIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeOperatorsMintRedeemerIndex: bigint;
      readonly activeTailRefInputIndex: bigint | null;
      readonly registeredWitnessRefInputIndex: bigint | null;
    };

export type RetireRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly retiredOperatorsRedeemerIndex: bigint;
  readonly activeAnchorNodeInputOutRef: OutputReference;
  readonly activeAnchorNodeOutputIndex: bigint;
  readonly retiredAnchorNodeOutputIndex: bigint;
  readonly retiredInsertedNodeOutputIndex: bigint;
  readonly schedulerSync: RetireSchedulerSyncLayout;
};

export type RetireOperatorTxConfig = LayoutOptions<RetireRedeemerLayout> & {
  readonly lucid: LucidEvolution;
  readonly contracts: MidgardValidators;
  readonly operatorKeyHash: string;
  readonly activeOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly retiredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  /** The active node being retired. */
  readonly activeNode: NodeWithDatum;
  /** The active-list element linking to `activeNode` (possibly the root). */
  readonly activeAnchor: NodeWithDatum;
  /** The retired-list element the new node is inserted after. */
  readonly retiredInsertionAnchor: NodeWithDatum;
  readonly activeNodeUnit: string;
  readonly retiredNodeUnit: string;
  /** Copied verbatim from the removed active node's datum. */
  readonly bondUnlockTime: bigint | null;
  /** Lovelace the inserted retired node must hold. */
  readonly retiredNodeLovelace: bigint;
  readonly mode: RetirementMode;
  /**
   * Required for `forced-inactivity`: the transaction fee must equal the
   * inactivity penalty exactly.
   */
  readonly inactivitySlashingPenaltyLovelace?: bigint;
  readonly schedulerSync: RetireSchedulerSync;
  /**
   * The active-operators validator requires the retiring operator's signature
   * for a voluntary retirement (a forced one is permissionless), so this
   * defaults to `true`. Setting it to `false` builds a transaction the
   * validator refuses; only a negative test does that, to reach the on-chain
   * check instead of a wallet-side witness error.
   */
  readonly requireOperatorSignature?: boolean;
  /**
   * Set by `buildUnsignedRetireOperatorTxProgram` for a forced retirement.
   * Callers that drive `buildRetireOperatorTx` themselves have to balance the
   * pinned fee with `planExactFeeBalance` and pass the plan here.
   */
  readonly exactFeePlan?: ExactFeeBalancePlan;
  /** The on-chain check needs a closed, short validity range. */
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

/**
 * What the retirement encodes once and reads from several places: the three
 * list datums it pays, and the scheduler route with its refreshed datum.
 */
type RetirePlan = {
  readonly updatedActiveAnchorDatumCbor: string;
  readonly updatedRetiredAnchorDatumCbor: string;
  readonly insertedRetiredNodeDatumCbor: string;
  readonly scheduler:
    | { readonly kind: "OperatorIsInactive"; readonly refInput: UTxO }
    | {
        readonly kind: "SchedulerIsAdvancing";
        readonly sync: AdvancingSchedulerSync;
        readonly refreshedDatumCbor: string;
      };
};

const retirePlan = (config: RetireOperatorTxConfig): RetirePlan => {
  const operatorNodeKey = { Key: { key: config.operatorKeyHash } } as const;
  const sync = config.schedulerSync;
  return {
    updatedActiveAnchorDatumCbor: encodeLinkedListNodeView({
      ...config.activeAnchor.datum,
      next: config.activeNode.datum.next,
    }),
    updatedRetiredAnchorDatumCbor: encodeLinkedListNodeView({
      ...config.retiredInsertionAnchor.datum,
      next: operatorNodeKey,
    }),
    insertedRetiredNodeDatumCbor: encodeLinkedListNodeView({
      key: operatorNodeKey,
      next: config.retiredInsertionAnchor.datum.next,
      data: castRetiredOperatorDatumToData({
        bond_unlock_time: config.bondUnlockTime,
      }) as LinkedListNodeView["data"],
    }),
    scheduler:
      sync.kind === "OperatorIsInactive"
        ? { kind: "OperatorIsInactive", refInput: sync.schedulerRefInput }
        : {
            kind: "SchedulerIsAdvancing",
            sync,
            refreshedDatumCbor: encodeSchedulerDatumForChain(
              sync.refreshedDatum,
            ),
          },
  };
};

/**
 * The outputs a retirement declares, in the order the builder pays them. The
 * redeemer layout resolves its indices from the final transaction, but the
 * exact-fee balance has to know what these outputs cost before the
 * transaction exists, so both read the same list.
 */
const retireDeclaredOutputs = (
  config: RetireOperatorTxConfig,
  plan: RetirePlan,
): readonly ContractOutput[] => {
  const { activeOperators, retiredOperators, scheduler } = config.contracts;
  const outputs: ContractOutput[] = [
    {
      address: activeOperators.spendingScriptAddress,
      datumCbor: plan.updatedActiveAnchorDatumCbor,
      assets: config.activeAnchor.utxo.assets,
    },
    {
      address: retiredOperators.spendingScriptAddress,
      datumCbor: plan.updatedRetiredAnchorDatumCbor,
      assets: config.retiredInsertionAnchor.utxo.assets,
    },
    {
      address: retiredOperators.spendingScriptAddress,
      datumCbor: plan.insertedRetiredNodeDatumCbor,
      assets: {
        lovelace: config.retiredNodeLovelace,
        [config.retiredNodeUnit]: 1n,
      },
    },
  ];
  if (plan.scheduler.kind === "SchedulerIsAdvancing") {
    outputs.push({
      address: scheduler.spendingScriptAddress,
      datumCbor: plan.scheduler.refreshedDatumCbor,
      assets: plan.scheduler.sync.schedulerInput.assets,
    });
  }
  if (config.mode !== "forced-inactivity") {
    return outputs;
  }
  // A pinned fee leaves no room for Lucid's own min-ADA top-up: an anchor
  // whose link grew needs more lovelace than it carried, and a top-up the
  // balance did not reserve would land in the fee.
  return outputs.map((output) =>
    withSettledLovelace(
      config.lucid,
      output,
      "A forced retirement's declared output",
    ),
  );
};

const deriveRetireSchedulerSyncLayout = (
  config: RetireOperatorTxConfig,
  ctx: RedeemerContext,
  scheduler: RetirePlan["scheduler"],
): RetireSchedulerSyncLayout => {
  if (scheduler.kind === "OperatorIsInactive") {
    return {
      kind: "OperatorIsInactive",
      schedulerRefInputIndex: requireReferenceInputIndex(
        ctx,
        scheduler.refInput,
        "operator retirement scheduler witness",
      ),
    };
  }
  const { sync, refreshedDatumCbor } = scheduler;
  return {
    kind: "SchedulerIsAdvancing",
    schedulerInputIndex: requireInputIndex(
      ctx,
      sync.schedulerInput,
      "operator retirement scheduler",
    ),
    schedulerRedeemerIndex: requireSpendRedeemerIndex(
      ctx,
      sync.schedulerInput,
      "operator retirement scheduler",
    ),
    schedulerOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: config.contracts.scheduler.spendingScriptAddress,
          datum: refreshedDatumCbor,
          unit: toUnit(
            config.contracts.scheduler.policyId,
            SCHEDULER_ASSET_NAME,
          ),
        }),
      "operator retirement refreshed scheduler",
    ),
    activeOperatorsMintRedeemerIndex: requireMintRedeemerIndex(
      ctx,
      config.contracts.activeOperators.policyId,
      "operator retirement active mint",
    ),
    activeTailRefInputIndex:
      sync.activeTailRefNode === undefined
        ? null
        : requireReferenceInputIndex(
            ctx,
            sync.activeTailRefNode.utxo,
            "operator retirement active tail",
          ),
    registeredWitnessRefInputIndex:
      sync.registeredWitnessNode === undefined
        ? null
        : requireReferenceInputIndex(
            ctx,
            sync.registeredWitnessNode.utxo,
            "operator retirement registered witness",
          ),
  };
};

const deriveRetireLayout = (
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

const encodeActiveRetireRedeemer = (
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

const encodeRetiredRetireRedeemer = (
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

const encodeRetireSchedulerRedeemer = (
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

const forcedRetirementPenalty = (config: RetireOperatorTxConfig): bigint => {
  if (config.inactivitySlashingPenaltyLovelace === undefined) {
    throw exitError(
      "A forced retirement must reserve the inactivity penalty as its fee",
      config.operatorKeyHash,
    );
  }
  return config.inactivitySlashingPenaltyLovelace;
};

/**
 * Builds the retirement transaction: the active node is burned and removed
 * through its anchor, a retired node carrying the same `bond_unlock_time` is
 * inserted with the correct bond tranche, and the scheduler is either
 * referenced or advanced in the same transaction.
 */
export const buildRetireOperatorTx = (
  config: RetireOperatorTxConfig,
): TxBuilder => {
  const plan = retirePlan(config);
  const redeemer = redeemerEncoder(config, (ctx) =>
    deriveRetireLayout(config, ctx, plan),
  );
  const { activeOperators, retiredOperators, scheduler } = config.contracts;
  const schedulerRefInputs: readonly UTxO[] =
    plan.scheduler.kind === "OperatorIsInactive"
      ? [plan.scheduler.refInput]
      : [
          plan.scheduler.sync.activeTailRefNode?.utxo,
          plan.scheduler.sync.registeredWitnessNode?.utxo,
          plan.scheduler.sync.schedulerSpendingScriptRef,
        ].filter((utxo): utxo is UTxO => utxo !== undefined);

  let tx = config.lucid
    .newTx()
    .validFrom(
      safeTimeNumber(config.validFrom, "operator retirement validFrom"),
    )
    .validTo(safeTimeNumber(config.validTo, "operator retirement validTo"))
    .collectFrom(
      [config.activeAnchor.utxo, config.activeNode.utxo],
      ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER,
    )
    .collectFrom(
      [config.retiredInsertionAnchor.utxo],
      LIST_STATE_TRANSITION_VOID_REDEEMER,
    )
    .readFrom([
      ...config.activeOperatorScriptRefs.map(({ utxo }) => utxo),
      ...config.retiredOperatorScriptRefs.map(({ utxo }) => utxo),
      config.hubOracleRefInput,
      ...schedulerRefInputs,
    ])
    .mintAssets(
      { [config.activeNodeUnit]: -1n },
      redeemer(
        (ctx) =>
          requireOwnMintPurpose(
            ctx,
            activeOperators.policyId,
            "operator retirement active mint",
          ),
        (layout) => encodeActiveRetireRedeemer(config, layout),
      ),
    )
    .mintAssets(
      { [config.retiredNodeUnit]: 1n },
      redeemer(
        (ctx) =>
          requireOwnMintPurpose(
            ctx,
            retiredOperators.policyId,
            "operator retirement retired mint",
          ),
        (layout) => encodeRetiredRetireRedeemer(config, layout),
      ),
    );

  if (plan.scheduler.kind === "SchedulerIsAdvancing") {
    const { sync } = plan.scheduler;
    tx = tx.collectFrom(
      [sync.schedulerInput],
      redeemer(
        (ctx) =>
          requireOwnSpendPurpose(
            ctx,
            sync.schedulerInput,
            "operator retirement scheduler",
          ),
        (layout) => encodeRetireSchedulerRedeemer(config, layout),
      ),
    );
    if (sync.schedulerSpendingScriptRef === undefined) {
      tx = tx.attach.Script(scheduler.spendingScript);
    }
  }

  tx = payContractOutputs(tx, retireDeclaredOutputs(config, plan));

  if (config.mode === "voluntary") {
    return config.requireOperatorSignature === false
      ? tx
      : tx.addSignerKey(config.operatorKeyHash);
  }
  tx = tx.setMinFee(forcedRetirementPenalty(config));
  return config.exactFeePlan === undefined
    ? tx
    : applyExactFeePlan(tx, config.exactFeePlan);
};

export type RetireOperatorTxResult = {
  readonly tx: TxSignBuilder;
  readonly layout: RetireRedeemerLayout;
};

/**
 * Two-pass retirement build (see `completeInTwoPasses`). A forced retirement
 * is balanced against the submitter's wallet first, and its completed fee is
 * checked against the inactivity penalty.
 */
export const buildUnsignedRetireOperatorTxProgram = (
  config: RetireOperatorTxConfig,
): Effect.Effect<RetireOperatorTxResult, OperatorExitError> =>
  Effect.gen(function* () {
    if (config.validTo <= config.validFrom) {
      return yield* failExit(
        "Operator retirement needs a closed validity range",
        `validFrom=${config.validFrom.toString()},validTo=${config.validTo.toString()}`,
      );
    }
    let exactFee: Parameters<typeof completeInTwoPasses>[0]["exactFee"];
    if (config.mode === "forced-inactivity") {
      const penalty = yield* Effect.try({
        try: () => forcedRetirementPenalty(config),
        catch: (cause) =>
          cause instanceof OperatorExitError
            ? cause
            : exitError(String(cause), cause),
      });
      const sync = config.schedulerSync;
      const plan =
        config.exactFeePlan ??
        (yield* planExactFee({
          lucid: config.lucid,
          label: "A forced retirement",
          feeLovelace: penalty,
          scriptInputs: [
            config.activeAnchor.utxo,
            config.activeNode.utxo,
            config.retiredInsertionAnchor.utxo,
            ...(sync.kind === "SchedulerIsAdvancing"
              ? [sync.schedulerInput]
              : []),
          ],
          declaredOutputs: retireDeclaredOutputs(config, retirePlan(config)),
        }));
      exactFee = {
        plan,
        violation:
          "A forced retirement must pay exactly the inactivity penalty as its fee",
      };
    }
    return yield* completeInTwoPasses({
      label: "operator retirement",
      operatorKeyHash: config.operatorKeyHash,
      onLayout: config.onLayout,
      build: (layout) =>
        buildRetireOperatorTx({
          ...config,
          exactFeePlan: exactFee?.plan,
          ...layout,
        }),
      options:
        exactFee === undefined
          ? completeOptionsWithLocalEval()
          : exactFeeCompleteOptions(exactFee.plan),
      exactFee,
    });
  });

// ---------------------------------------------------------------------------
// Retirement witnesses
// ---------------------------------------------------------------------------

export type RetireOperatorWitnesses = {
  readonly activeNode: NodeWithDatum;
  readonly activeAnchor: NodeWithDatum;
  readonly retiredInsertionAnchor: NodeWithDatum;
  readonly activeNodeUnit: string;
  readonly retiredNodeUnit: string;
  readonly bondUnlockTime: bigint | null;
  readonly inactivityStrikes: bigint;
  readonly schedulerSync: RetireSchedulerSync;
};

/**
 * The retired list is ascending by operator key, so the insertion anchor is
 * the element with the greatest key below the operator's, or the root when
 * no key precedes it.
 */
const retiredInsertionAnchorFor = (
  retired: readonly NodeWithDatum[],
  operatorKeyHash: string,
): NodeWithDatum | undefined =>
  retired.find((node) => orderedNotMemberWitness(node.datum, operatorKeyHash));

const rewindSchedulerSync = ({
  snapshot,
  operatorKeyHash,
  isLastMember,
  validTo,
  schedulerSpendingScriptRef,
}: {
  readonly snapshot: OperatorDirectorySnapshot;
  readonly operatorKeyHash: string;
  readonly isLastMember: boolean;
  readonly validTo: bigint;
  readonly schedulerSpendingScriptRef?: UTxO;
}): RetireSchedulerSync => {
  const registeredWitnessNode = findTailNode(snapshot.registered);
  if (registeredWitnessNode === undefined) {
    throw exitError(
      "Found no registered-operators tail to witness that nobody can activate",
      operatorKeyHash,
    );
  }
  const registeredWitnessActivationTime = registeredNodeKeyToPosixTime(
    registeredWitnessNode.datum.key,
  );
  if (
    registeredWitnessActivationTime !== undefined &&
    registeredWitnessActivationTime <= validTo - 1n
  ) {
    throw exitError(
      "The earliest registered operator is already eligible to activate, so the scheduler cannot rewind",
      `activation_time=${registeredWitnessActivationTime.toString()},inclusive_upper_bound=${(validTo - 1n).toString()}`,
    );
  }
  const base = {
    kind: "SchedulerIsAdvancing" as const,
    schedulerInput: snapshot.scheduler.utxo,
    removingOperatorsAnchorElementKey: null,
    removingOperatorIsTheLastMember: isLastMember,
    registeredWitnessNode,
    schedulerSpendingScriptRef,
  };
  if (isLastMember) {
    return { ...base, refreshedDatum: "NoActiveOperators" };
  }
  // The retiring node is not the tail (`isLastMember` is false), but the
  // two may share a transaction hash: an insertion writes the continued
  // anchor and the new node in one transaction. Compare keys, not hashes.
  const activeTailRefNode = findTailNode(snapshot.active);
  const tailKey =
    activeTailRefNode === undefined
      ? null
      : nodeKeyHex(activeTailRefNode.datum.key);
  if (activeTailRefNode === undefined || tailKey === operatorKeyHash) {
    throw exitError(
      "Found no surviving active-operators tail to take over the shift",
      operatorKeyHash,
    );
  }
  if (tailKey === null) {
    throw exitError(
      "The active-operators tail is the root, so no operator can take over the shift",
      operatorKeyHash,
    );
  }
  return {
    ...base,
    activeTailRefNode,
    refreshedDatum: {
      ActiveOperator: { operator: tailKey, start_time: validTo - 1n },
    },
  };
};

/**
 * Resolves everything the retirement builder needs from a directory snapshot,
 * including which scheduler route applies.
 *
 * `validTo` is Lucid's exclusive upper bound; the scheduler requires the new
 * shift to start exactly at Aiken's inclusive upper bound, which is
 * `validTo - 1`.
 */
export const deriveRetireOperatorWitnesses = ({
  snapshot,
  contracts,
  operatorKeyHash,
  validTo,
  schedulerSpendingScriptRef,
}: {
  readonly snapshot: OperatorDirectorySnapshot;
  readonly contracts: Pick<
    MidgardValidators,
    "activeOperators" | "retiredOperators"
  >;
  readonly operatorKeyHash: string;
  readonly validTo: bigint;
  readonly schedulerSpendingScriptRef?: UTxO;
}): RetireOperatorWitnesses => {
  const activeNode = findNodeByKey(snapshot.active, operatorKeyHash);
  if (activeNode === undefined) {
    throw exitError(
      "Operator is not in the active-operators list",
      operatorKeyHash,
    );
  }
  const activeAnchor = findAnchorNodeForKey(snapshot.active, operatorKeyHash);
  if (activeAnchor === undefined) {
    throw exitError(
      "Found no active-operators element linking to the retiring operator",
      operatorKeyHash,
    );
  }
  const retiredInsertionAnchor = retiredInsertionAnchorFor(
    snapshot.retired,
    operatorKeyHash,
  );
  if (retiredInsertionAnchor === undefined) {
    throw exitError(
      "Found no retired-operators insertion anchor",
      operatorKeyHash,
    );
  }
  const anchorKey = nodeKeyHex(activeAnchor.datum.key);
  const isLastMember = activeNode.datum.next === "Empty";
  const scheduled = schedulerCurrentOperator(snapshot.scheduler);

  const schedulerSync: RetireSchedulerSync =
    scheduled === null || scheduled.operator !== operatorKeyHash
      ? {
          kind: "OperatorIsInactive",
          schedulerRefInput: snapshot.scheduler.utxo,
        }
      : anchorKey !== null
        ? {
            kind: "SchedulerIsAdvancing",
            schedulerInput: snapshot.scheduler.utxo,
            refreshedDatum: {
              ActiveOperator: {
                operator: anchorKey,
                start_time: validTo - 1n,
              },
            },
            removingOperatorsAnchorElementKey: anchorKey,
            removingOperatorIsTheLastMember: isLastMember,
            schedulerSpendingScriptRef,
          }
        : rewindSchedulerSync({
            snapshot,
            operatorKeyHash,
            isLastMember,
            validTo,
            schedulerSpendingScriptRef,
          });

  return {
    activeNode,
    activeAnchor,
    retiredInsertionAnchor,
    activeNodeUnit: activeOperatorNodeUnit(
      contracts.activeOperators.policyId,
      operatorKeyHash,
    ),
    retiredNodeUnit: retiredOperatorNodeUnit(
      contracts.retiredOperators.policyId,
      operatorKeyHash,
    ),
    bondUnlockTime: activeNode.active?.bond_unlock_time ?? null,
    inactivityStrikes: activeNode.active?.inactivity_strikes ?? 0n,
    schedulerSync,
  };
};

// ---------------------------------------------------------------------------
// Bond recovery
// ---------------------------------------------------------------------------

export type RecoverOperatorBondRedeemerLayout = {
  readonly retiredAnchorNodeInputOutRef: OutputReference;
  readonly retiredAnchorNodeOutputIndex: bigint;
};

export type RecoverOperatorBondTxConfig =
  LayoutOptions<RecoverOperatorBondRedeemerLayout> & {
    readonly lucid: LucidEvolution;
    readonly contracts: MidgardValidators;
    readonly operatorKeyHash: string;
    readonly retiredOperatorScriptRefs: readonly ReferenceScriptPublication[];
    readonly retiredNode: NodeWithDatum;
    readonly retiredAnchor: NodeWithDatum;
    readonly retiredNodeUnit: string;
    /**
     * Must be strictly after `bond_unlock_time` when the node carries one: the
     * on-chain check is `is_entirely_after`, which is strict.
     */
    readonly validFrom: bigint;
    readonly validTo: bigint;
    /**
     * The retired-operators validator requires the operator's signature so
     * that the bond's destination is approved, so this defaults to `true`.
     * Setting it to `false` builds a transaction the validator refuses; only
     * a negative test does that.
     */
    readonly requireOperatorSignature?: boolean;
  };

const encodeRecoverBondRedeemer = (
  config: RecoverOperatorBondTxConfig,
  layout: RecoverOperatorBondRedeemerLayout,
): string => {
  const redeemer: RetiredOperatorMintRedeemerType = {
    RecoverOperatorBond: {
      retired_operator_key: config.operatorKeyHash,
      retired_operator_anchor_element_input_outref:
        layout.retiredAnchorNodeInputOutRef,
      retired_operator_anchor_element_output_index:
        layout.retiredAnchorNodeOutputIndex,
    },
  };
  return LucidData.to(redeemer, RetiredOperatorMintRedeemer);
};

/**
 * Burns the retired node and releases its bond. The anchor's own lovelace is
 * preserved; the released bond reaches the submitting wallet as change.
 */
export const buildRecoverOperatorBondTx = (
  config: RecoverOperatorBondTxConfig,
): TxBuilder => {
  const { retiredOperators } = config.contracts;
  const updatedRetiredAnchorDatumCbor = encodeLinkedListNodeView({
    ...config.retiredAnchor.datum,
    next: config.retiredNode.datum.next,
  });
  const redeemer = redeemerEncoder(
    config,
    (ctx): RecoverOperatorBondRedeemerLayout => ({
      retiredAnchorNodeInputOutRef: outputReferenceFromUTxO(
        config.retiredAnchor.utxo,
      ),
      retiredAnchorNodeOutputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        (output) =>
          outputMatchesElement({
            output,
            address: retiredOperators.spendingScriptAddress,
            datum: updatedRetiredAnchorDatumCbor,
            unit: requirePolicyNftUnit(
              config.retiredAnchor.utxo.assets,
              retiredOperators.policyId,
              "operator bond recovery retired anchor assets",
            ),
          }),
        "operator bond recovery updated retired anchor",
      ),
    }),
  );
  const tx = config.lucid
    .newTx()
    .validFrom(
      safeTimeNumber(config.validFrom, "operator bond recovery validFrom"),
    )
    .validTo(safeTimeNumber(config.validTo, "operator bond recovery validTo"))
    .collectFrom(
      [config.retiredAnchor.utxo, config.retiredNode.utxo],
      LIST_STATE_TRANSITION_VOID_REDEEMER,
    )
    .readFrom(config.retiredOperatorScriptRefs.map(({ utxo }) => utxo))
    .mintAssets(
      { [config.retiredNodeUnit]: -1n },
      redeemer(
        (ctx) =>
          requireOwnMintPurpose(
            ctx,
            retiredOperators.policyId,
            "operator bond recovery retired mint",
          ),
        (layout) => encodeRecoverBondRedeemer(config, layout),
      ),
    )
    .pay.ToContract(
      retiredOperators.spendingScriptAddress,
      { kind: "inline", value: updatedRetiredAnchorDatumCbor },
      config.retiredAnchor.utxo.assets,
    );
  return config.requireOperatorSignature === false
    ? tx
    : tx.addSignerKey(config.operatorKeyHash);
};

export type RecoverOperatorBondTxResult = {
  readonly tx: TxSignBuilder;
  readonly layout: RecoverOperatorBondRedeemerLayout;
};

export const buildUnsignedRecoverOperatorBondTxProgram = (
  config: RecoverOperatorBondTxConfig,
): Effect.Effect<RecoverOperatorBondTxResult, OperatorExitError> =>
  completeInTwoPasses({
    label: "operator bond recovery",
    operatorKeyHash: config.operatorKeyHash,
    onLayout: config.onLayout,
    build: (layout) => buildRecoverOperatorBondTx({ ...config, ...layout }),
    options: completeOptionsWithLocalEval(),
  });

/**
 * Resolves the bond-recovery witnesses from a directory snapshot.
 */
export const deriveRecoverOperatorBondWitnesses = ({
  snapshot,
  contracts,
  operatorKeyHash,
}: {
  readonly snapshot: Pick<OperatorDirectorySnapshot, "retired">;
  readonly contracts: Pick<MidgardValidators, "retiredOperators">;
  readonly operatorKeyHash: string;
}): {
  readonly retiredNode: NodeWithDatum;
  readonly retiredAnchor: NodeWithDatum;
  readonly retiredNodeUnit: string;
  readonly bondUnlockTime: bigint | null;
  readonly bondLovelace: bigint;
} => {
  const retiredNode = findNodeByKey(snapshot.retired, operatorKeyHash);
  if (retiredNode === undefined) {
    throw exitError(
      "Operator is not in the retired-operators list",
      operatorKeyHash,
    );
  }
  const retiredAnchor = findAnchorNodeForKey(snapshot.retired, operatorKeyHash);
  if (retiredAnchor === undefined) {
    throw exitError(
      "Found no retired-operators element linking to the operator",
      operatorKeyHash,
    );
  }
  return {
    retiredNode,
    retiredAnchor,
    retiredNodeUnit: retiredOperatorNodeUnit(
      contracts.retiredOperators.policyId,
      operatorKeyHash,
    ),
    bondUnlockTime: retiredNode.retired?.bond_unlock_time ?? null,
    bondLovelace: retiredNode.utxo.assets["lovelace"] ?? 0n,
  };
};

// ---------------------------------------------------------------------------
// Duplicate-registration slashing
// ---------------------------------------------------------------------------

/**
 * Where the operator's other membership lives. The proof node is referenced,
 * never spent, and its list determines which finalization the registered
 * minting policy applies.
 */
export type DuplicateProof =
  | { readonly kind: "registered"; readonly node: NodeWithDatum }
  | {
      readonly kind: "active";
      readonly node: NodeWithDatum;
      readonly hubOracleRefInput: UTxO;
    }
  | { readonly kind: "retired"; readonly node: NodeWithDatum };

export type SlashDuplicateOperatorRedeemerLayout = {
  readonly registeredAnchorNodeInputOutRef: OutputReference;
  readonly registeredAnchorNodeOutputIndex: bigint;
  readonly duplicateNodeRefInputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint | null;
};

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
     * Set by `buildUnsignedSlashDuplicateOperatorTxProgram`. Callers that
     * drive `buildSlashDuplicateOperatorTx` themselves have to balance the
     * pinned fee with `planExactFeeBalance` and pass the plan here.
     */
    readonly exactFeePlan?: ExactFeeBalancePlan;
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
 * The single output a slashing declares: the continued registered anchor. The
 * removed node's bond pays the penalty as the fee, and its remainder goes to
 * the submitter in the exact-fee balance's own output.
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

const deriveSlashLayout = (
  config: SlashDuplicateOperatorTxConfig,
  ctx: RedeemerContext,
  anchorDatumCbor: string,
): SlashDuplicateOperatorRedeemerLayout => {
  const { registeredOperators } = config.contracts;
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
    },
  };
  return LucidData.to(redeemer, RegisteredOperatorMintRedeemer);
};

/**
 * Removes a duplicate registration and pays exactly the slashing penalty as
 * the transaction fee. Anyone may submit this, and on-chain nothing
 * constrains where the rest of the removed node's bond goes: the exact-fee
 * plan pays it to the submitter.
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
    .setMinFee(config.slashingPenaltyLovelace);
  tx = payContractOutputs(tx, slashDeclaredOutputs(config, anchorDatumCbor));
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
    const plan =
      config.exactFeePlan ??
      (yield* planExactFee({
        lucid: config.lucid,
        label: "Duplicate-operator slashing",
        feeLovelace: config.slashingPenaltyLovelace,
        scriptInputs: [
          config.registeredAnchor.utxo,
          config.duplicateRegisteredNode.utxo,
        ],
        declaredOutputs: slashDeclaredOutputs(
          config,
          updatedRegisteredAnchorDatumCbor(config),
        ),
      }));
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
