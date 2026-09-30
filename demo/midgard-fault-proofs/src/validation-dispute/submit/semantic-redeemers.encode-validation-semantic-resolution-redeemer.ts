import {
  deriveCekSelectionFacts,
  type ValidationCekMaterialRoute,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  deriveLedgerOutputProofFinalizePlan,
  deriveLedgerOutputProofStepPlan,
} from "../../ledger-output-proof-plan.js";
import {
  validationCekMaterialRouteData,
  type ValidationOneStepSubmissionArgument,
} from "./evidence.js";
import { type ContinueLayout } from "./redeemers.js";
import { requireStagedOneStepArgument } from "./reference-scripts.js";
import {
  isLedgerOutputProofStepResolver,
  type ScriptSourcesMiddleInvocation,
  type ScriptSourcesObserverInvocation,
} from "./semantic-redeemers.make-prepare-selected-redeemer.js";
import { semanticActionFields } from "./semantic-redeemers.semantic-action-fields.js";

export const encodeValidationSemanticResolutionRedeemer = ({
  oneStepArgument,
  inputIndex,
  outputIndex,
  materialRoute,
  assetFoldYieldReferenceInputIndex,
  phaseANativeItemInvocation,
  scriptSourcesMiddleInvocation,
  scriptSourcesObserverInvocation,
  scriptSourcesDescriptorInvocation,
  cekSelectionYieldReferenceInputIndices,
  ledgerOutputProofYieldReferenceInputIndices,
}: {
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  /** Required by, and only by, the CEK execution-selection resolver (11/1). */
  readonly materialRoute?: ValidationCekMaterialRoute;
  readonly assetFoldYieldReferenceInputIndex?: bigint;
  readonly scriptSourcesDescriptorInvocation?: {
    readonly claim: Constr<Data>;
    readonly referenceInputIndex: bigint;
  };
  readonly scriptSourcesObserverInvocation?: ScriptSourcesObserverInvocation;
  readonly scriptSourcesMiddleInvocation?: ScriptSourcesMiddleInvocation;
  readonly phaseANativeItemInvocation?: {
    readonly referenceInputIndex: bigint;
    readonly kind: 0 | 1;
  };
  readonly cekSelectionYieldReferenceInputIndices?: readonly bigint[];
  /**
   * The yield reference-input indices of a shared-LOP step (stage yield
   * first, then its attestation roles) or finalize (the four descriptor
   * yields in `descriptor_roles` order) resolution; the claims themselves are
   * re-derived from the argument's own evidence.
   */
  readonly ledgerOutputProofYieldReferenceInputIndices?: readonly bigint[];
}): Buffer => {
  if (inputIndex < 0n || outputIndex < 0n) {
    throw new Error(
      "Validation semantic redeemer indexes must be non-negative",
    );
  }
  // Option B (#620): the item-semantic `Verify` is transition-only — no
  // carriage field and no retired `VerifyReference` arm — so the CanonicalDecode
  // complete item flows through the generic `semanticActionFields` shape like
  // every other semantic action.
  const staged = requireStagedOneStepArgument(oneStepArgument);
  const fields = semanticActionFields({
    resolverIndex: oneStepArgument.resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    inputIndex,
    outputIndex,
    transition: staged.transitionData,
    auxiliary: staged.auxiliary,
    ...(cekSelectionYieldReferenceInputIndices === undefined
      ? {}
      : {
          cekSelectionInvocation: {
            indices: cekSelectionYieldReferenceInputIndices,
            facts: deriveCekSelectionFacts(staged.cekRouteMaterial),
          },
        }),
    ...(assetFoldYieldReferenceInputIndex === undefined
      ? {}
      : { assetFoldYieldReferenceInputIndex }),
    ...(phaseANativeItemInvocation === undefined
      ? {}
      : { phaseANativeItemInvocation }),
    ...(scriptSourcesMiddleInvocation === undefined
      ? {}
      : { scriptSourcesMiddleInvocation }),
    ...(scriptSourcesObserverInvocation === undefined
      ? {}
      : { scriptSourcesObserverInvocation }),
    ...(scriptSourcesDescriptorInvocation === undefined
      ? {}
      : { scriptSourcesDescriptorInvocation }),
    ...(ledgerOutputProofYieldReferenceInputIndices === undefined
      ? {}
      : isLedgerOutputProofStepResolver(
            oneStepArgument.resolverIndex,
            staged.semanticResolverIndex,
          )
        ? {
            ledgerOutputProofStepInvocation: {
              plan: deriveLedgerOutputProofStepPlan({
                resolverIndex: oneStepArgument.resolverIndex,
                semanticResolverIndex: staged.semanticResolverIndex,
                transitionCbor: oneStepArgument.transitionCbor,
                auxiliaryCbor: oneStepArgument.auxiliaryCbor,
                ...(oneStepArgument.ledgerOutputProofSuccessorWorkWitnessCbor ===
                undefined
                  ? {}
                  : {
                      ledgerOutputProofSuccessorWorkWitnessCbor:
                        oneStepArgument.ledgerOutputProofSuccessorWorkWitnessCbor,
                    }),
              }),
              indices: ledgerOutputProofYieldReferenceInputIndices,
            },
          }
        : {
            ledgerOutputProofFinalizeInvocation: {
              plan: deriveLedgerOutputProofFinalizePlan({
                resolverIndex: oneStepArgument.resolverIndex,
                semanticResolverIndex: staged.semanticResolverIndex,
                transitionCbor: oneStepArgument.transitionCbor,
              }),
              indices: ledgerOutputProofYieldReferenceInputIndices,
            },
          }),
    ...(materialRoute === undefined
      ? {}
      : { materialRoute: validationCekMaterialRouteData(materialRoute) }),
  });
  return Buffer.from(
    Data.to(
      new Constr(1, [
        new Constr(
          oneStepArgument.resolverIndex === 8 &&
          [19, 21].includes(staged.semanticResolverIndex) &&
          staged.auxiliary.index === 10
            ? 1
            : 0,
          [...fields],
        ),
      ]),
    ),
    "hex",
  );
};

/**
 * The semantic-resolution spend layout: the thread input, the award output
 * and — for the CEK execution selection only — the canonical indices of the
 * CEK program-material reference inputs, in the supplied (root) order.
 */
export type SemanticResolutionLayout = ContinueLayout & {
  readonly materialReferenceInputIndices: readonly bigint[];
};
