import {
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { completeOptionsWithLocalEval } from "../tx-completion.js";
import {
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
} from "../tx-context-redeemer.js";
import {
  findTailNode,
  nodeKeyHex,
  type OperatorDirectorySnapshot,
  registeredNodeKeyToPosixTime,
} from "./directory.js";
import { exactFeeCompleteOptions } from "./exact-fee.js";
import {
  ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER,
  applyExactFeePlan,
  completeInTwoPasses,
  exitError,
  failExit,
  LIST_STATE_TRANSITION_VOID_REDEEMER,
  OperatorExitError,
  payContractOutputs,
  planExactFee,
  redeemerEncoder,
  type RetireSchedulerSync,
  safeTimeNumber,
} from "./exit.complete-in-two-passes.js";
import {
  deriveRetireLayout,
  encodeActiveRetireRedeemer,
  encodeRetiredRetireRedeemer,
  encodeRetireSchedulerRedeemer,
  forcedRetirementPenalty,
} from "./exit.derive-retire-layout.js";
import {
  retireDeclaredOutputs,
  type RetireOperatorTxConfig,
  retirePlan,
  type RetireRedeemerLayout,
} from "./exit.derive-retire-scheduler-sync-layout.js";
import type { NodeWithDatum } from "./layout.js";
import { orderedNotMemberWitness } from "./layout.js";

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
export const retiredInsertionAnchorFor = (
  retired: readonly NodeWithDatum[],
  operatorKeyHash: string,
): NodeWithDatum | undefined =>
  retired.find((node) => orderedNotMemberWitness(node.datum, operatorKeyHash));

export const rewindSchedulerSync = ({
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
