import {
  Data as LucidData,
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type MidgardValidators,
  type OutputReference,
  outputReferenceFromUTxO,
} from "../common.js";
import { encodeLinkedListNodeView } from "../linked-list.js";
import {
  RetiredOperatorMintRedeemer,
  type RetiredOperatorMintRedeemer as RetiredOperatorMintRedeemerType,
} from "../retired-operators.js";
import { completeOptionsWithLocalEval } from "../tx-completion.js";
import {
  requireOwnMintPurpose,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  findAnchorNodeForKey,
  findNodeByKey,
  nodeKeyHex,
  type OperatorDirectorySnapshot,
  schedulerCurrentOperator,
} from "./directory.js";
import {
  retiredInsertionAnchorFor,
  type RetireOperatorWitnesses,
  rewindSchedulerSync,
} from "./exit.build-retire-operator-tx.js";
import {
  activeOperatorNodeUnit,
  completeInTwoPasses,
  exitError,
  type LayoutOptions,
  LIST_STATE_TRANSITION_VOID_REDEEMER,
  OperatorExitError,
  redeemerEncoder,
  retiredOperatorNodeUnit,
  type RetireSchedulerSync,
  safeTimeNumber,
} from "./exit.complete-in-two-passes.js";
import type { NodeWithDatum, ReferenceScriptPublication } from "./layout.js";
import {
  outputMatchesElement,
  requirePolicyNftUnit,
} from "./output-selectors.js";

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
