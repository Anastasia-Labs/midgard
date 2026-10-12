import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";

import { type MidgardValidators, type OutputReference } from "../common.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "../linked-list.js";
import { castRetiredOperatorDatumToData } from "../retired-operators.js";
import { SCHEDULER_ASSET_NAME } from "../scheduler.js";
import { encodeSchedulerDatumForChain } from "../scheduler-refresh.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import { type ExactFeeBalancePlan, withSettledLovelace } from "./exact-fee.js";
import {
  type AdvancingSchedulerSync,
  type ContractOutput,
  type LayoutOptions,
  type RedeemerContext,
  type RetirementMode,
  type RetireSchedulerSync,
} from "./exit.complete-in-two-passes.js";
import type { NodeWithDatum, ReferenceScriptPublication } from "./layout.js";
import { outputMatchesElement } from "./output-selectors.js";

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
  /**
   * The submitting wallet's coins, as the caller's view holds them. When
   * given, coin selection, collateral and the forced retirement's pinned-fee
   * balance use exactly these and the provider is never read.
   */
  readonly walletInputs?: readonly UTxO[];
  /** The on-chain check needs a closed, short validity range. */
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

/**
 * What the retirement encodes once and reads from several places: the three
 * list datums it pays, and the scheduler route with its refreshed datum.
 */
export type RetirePlan = {
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

export const retirePlan = (config: RetireOperatorTxConfig): RetirePlan => {
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
export const retireDeclaredOutputs = (
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

export const deriveRetireSchedulerSyncLayout = (
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
