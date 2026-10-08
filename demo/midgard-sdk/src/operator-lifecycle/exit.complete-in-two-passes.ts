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

import { ActiveOperatorSpendRedeemer } from "../active-operators.js";
import type { GenericErrorFields } from "../errors.js";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
} from "../linked-list.js";
import { type SchedulerDatum } from "../scheduler.js";
import {
  EMPTY_WALLET_INPUTS_CAUSE,
  emptyWalletInputsRefusal,
  type TxCompleteOptions,
} from "../tx-completion.js";
import {
  type ExactFeeBalancePlan,
  type ExactFeeOutputPlan,
  exactFeeViolation,
  planExactFeeBalance,
} from "./exact-fee.js";
import type { NodeWithDatum } from "./layout.js";

export class OperatorExitError extends EffectData.TaggedError(
  "OperatorExitError",
)<GenericErrorFields> {}

export const exitError = (message: string, cause: unknown): OperatorExitError =>
  new OperatorExitError({ message, cause });

export const failExit = (
  message: string,
  cause: unknown,
): Effect.Effect<never, OperatorExitError> =>
  Effect.fail(exitError(message, cause));

export const ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER = LucidData.to(
  "ListStateTransition",
  ActiveOperatorSpendRedeemer,
);

/** The retired/registered spend validators only require that the list mints. */
export const LIST_STATE_TRANSITION_VOID_REDEEMER = LucidData.void();

export const safeTimeNumber = (value: bigint, label: string): number => {
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

export type RedeemerContext = Parameters<BuildTxWithRedeemer>[0];

/** A contract output the builder declares: address, inline datum, value. */
export type ContractOutput = ExactFeeOutputPlan & {
  readonly datumCbor: string;
};

/**
 * `layout` pins the redeemer indices; without it every redeemer is resolved
 * from the redeemer context and reported through `onLayout`.
 */
export type LayoutOptions<L> = {
  readonly layout?: L;
  readonly onLayout?: (layout: L) => void;
};

/**
 * Returns the redeemer factory for one build. With a pinned layout each
 * redeemer encodes statically; otherwise each is a callback that checks its
 * own purpose, derives the layout from the context, and reports it.
 */
export const redeemerEncoder = <L>(
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

export const payContractOutputs = (
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
export const applyExactFeePlan = (
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
 * Refuses an explicit wallet-input set that holds nothing. An operator-exit
 * builder handed `walletInputs` selects coins, collateral and any pinned-fee
 * balance from exactly those inputs and never reads the provider.
 */
export const requireExplicitWalletInputs = (
  walletInputs: readonly UTxO[] | undefined,
  label: string,
): Effect.Effect<void, OperatorExitError> => {
  const refusal = emptyWalletInputsRefusal(walletInputs, label);
  return refusal === null
    ? Effect.void
    : failExit(refusal, EMPTY_WALLET_INPUTS_CAUSE);
};

/**
 * Balances a pinned-fee build against the submitting wallet's plain coins.
 * The endpoint's own inputs and declared outputs rarely add up to the fee on
 * their own (a list anchor whose link grew needs a min-ADA top-up), so the
 * wallet makes up the difference and takes the leftover back in one output,
 * at the selected wallet's address.
 *
 * With `walletInputs` the plan spends exactly those coins: the caller's view
 * (live facts less what its live intents spend, plus their predicted change)
 * is the authority, and the provider is not read. Without them the coins are
 * read from the provider as the ledger holds them, because Lucid's own wallet
 * cache may predict change that never landed, and an exact-fee transaction
 * spends explicit inputs with no coin selection to fall back on.
 */
export const planExactFee = (input: {
  readonly lucid: LucidEvolution;
  readonly label: string;
  readonly feeLovelace: bigint;
  readonly scriptInputs: readonly UTxO[];
  readonly declaredOutputs: readonly ContractOutput[];
  readonly walletInputs?: readonly UTxO[];
}): Effect.Effect<ExactFeeBalancePlan, OperatorExitError> =>
  Effect.gen(function* () {
    yield* requireExplicitWalletInputs(input.walletInputs, input.label);
    return yield* Effect.tryPromise({
      try: async () => {
        const remainderAddress = await input.lucid.wallet().address();
        return planExactFeeBalance({
          lucid: input.lucid,
          label: input.label,
          feeLovelace: input.feeLovelace,
          scriptInputs: input.scriptInputs,
          declaredOutputs: input.declaredOutputs,
          walletUtxos:
            input.walletInputs ?? (await input.lucid.utxosAt(remainderAddress)),
          remainderAddress,
        });
      },
      catch: (cause) =>
        exitError(`Failed to balance the pinned fee: ${String(cause)}`, cause),
    });
  });

/**
 * Lucid's first pass resolves the layout from the redeemer context; the
 * second pins it, because the redeemer's own encoded size feeds back into the
 * fee and therefore into the indices. For a pinned-fee build the completed
 * transaction is then checked against its plan: on chain the fee is an
 * equality, so a fee Lucid raised would make the transaction unsubmittable.
 */
export const completeInTwoPasses = <L>({
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

export type AdvancingSchedulerSync = Extract<
  RetireSchedulerSync,
  { kind: "SchedulerIsAdvancing" }
>;
