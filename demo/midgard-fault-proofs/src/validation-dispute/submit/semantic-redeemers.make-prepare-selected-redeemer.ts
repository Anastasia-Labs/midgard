import {
  deriveCekSelectionFacts,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type ValidationAuxiliaryWitness as ValidationAuxiliaryWitnessData,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, type UTxO } from "@lucid-evolution/lucid";

import {
  type LedgerOutputProofFinalizePlan,
  type LedgerOutputProofStepPlan,
} from "../../ledger-output-proof-plan.js";
import { computationThreadOutputPredicate } from "../../tx-layout.js";
import {
  encodeWithRuntimeSchema,
  validationCanonicalDecodePrepareSelectedSpendRedeemerRuntimeSchema,
  validationPrepareSelectedSpendRedeemerRuntimeSchema,
} from "./evidence.js";
import { type ContinueLayout } from "./redeemers.js";

export const makePrepareSelectedRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  semanticResolverIndex,
  transition,
  auxiliary,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transition: ValidationOneStepWitness;
  readonly auxiliary: ValidationAuxiliaryWitnessData;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute prepare selected semantic resolver",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute prepare selected semantic resolver",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute prepare selected semantic resolver",
      ),
    };
    onLayout(layout);
    const base = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      semantic_resolver_index: BigInt(semanticResolverIndex),
      transition,
    };
    // Option B (#620): the canonical-decode resolver's `PrepareSelected` is
    // transition-only — the validator computes the evidence hash on-chain from
    // `(transition, NoAuxiliaryWitness)`, so neither the auxiliary nor a
    // prover-supplied hash rides in the redeemer. Every other resolver keeps
    // the five-field aux-bearing shape.
    return resolverIndex === 0
      ? encodeWithRuntimeSchema(
          { Continue: [base] },
          validationCanonicalDecodePrepareSelectedSpendRedeemerRuntimeSchema,
        )
      : encodeWithRuntimeSchema(
          { Continue: [{ ...base, auxiliary }] },
          validationPrepareSelectedSpendRedeemerRuntimeSchema,
        );
  }) satisfies BuildTxWithRedeemer;

export type ScriptSourcesObserverInvocation = {
  readonly observerHash: string;
  readonly activeCount: bigint;
  readonly indices: readonly bigint[];
};

export type ScriptSourcesMiddleInvocation = {
  readonly kind: number;
  readonly referenceInputIndex: bigint;
};

export type CekSelectionInvocation = {
  readonly beginTraversal?: boolean;
  readonly indices: readonly bigint[];
  readonly facts: ReturnType<typeof deriveCekSelectionFacts>;
};

export type LedgerOutputProofStepInvocation = {
  readonly plan: LedgerOutputProofStepPlan;
  /** Stage yield first, then the plan's attestation roles, in order. */
  readonly indices: readonly bigint[];
};

export type LedgerOutputProofFinalizeInvocation = {
  readonly plan: LedgerOutputProofFinalizePlan;
  /** The four descriptor yields, in `descriptor_roles` order. */
  readonly indices: readonly bigint[];
};

/** The two shared-LOP step dispatchers (ResolveInputs 7/3, ScriptSources 8/2). */
export const isLedgerOutputProofStepResolver = (
  resolverIndex: number,
  semanticResolverIndex: number,
): boolean =>
  (resolverIndex === 7 && semanticResolverIndex === 3) ||
  (resolverIndex === 8 && semanticResolverIndex === 2);

/** The two shared-LOP finalize dispatchers (ResolveInputs 7/4, ScriptSources 8/3). */
export const isLedgerOutputProofFinalizeResolver = (
  resolverIndex: number,
  semanticResolverIndex: number,
): boolean =>
  (resolverIndex === 7 && semanticResolverIndex === 4) ||
  (resolverIndex === 8 && semanticResolverIndex === 3);
