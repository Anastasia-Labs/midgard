import {
  deriveCekSelectionFacts,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type ValidationCekMaterialRoute,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type LedgerOutputProofFinalizePlan,
  type LedgerOutputProofStepPlan,
} from "../../ledger-output-proof-plan.js";
import { computationThreadOutputPredicate } from "../../tx-layout.js";
import {
  type PlutusDataValue,
  validationCekMaterialRouteData,
} from "./evidence.js";
import { type ContinueLayout } from "./redeemers.js";
import { type SemanticResolutionLayout } from "./semantic-redeemers.encode-validation-semantic-resolution-redeemer.js";
import { semanticActionFields } from "./semantic-redeemers.semantic-action-fields.js";

export const makeSemanticResolutionRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  semanticResolverIndex,
  transition,
  auxiliary,
  materialReferenceUtxos = [],
  materialRoute,
  assetFoldYieldReferenceUtxo,
  phaseANativeItemYield,
  scriptSourcesMiddleYield,
  scriptSourcesObserverYields,
  scriptSourcesDescriptorYield,
  ledgerOutputProofStepYield,
  ledgerOutputProofFinalizeYield,
  cekSelection,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
  /** CEK program-material UTxOs the route names, in root order. */
  readonly materialReferenceUtxos?: readonly UTxO[];
  readonly assetFoldYieldReferenceUtxo?: UTxO;
  readonly scriptSourcesDescriptorYield?: {
    readonly utxo: UTxO;
    readonly claim: Constr<Data>;
  };
  readonly scriptSourcesObserverYields?: {
    readonly utxos: readonly UTxO[];
    readonly observerHash: string;
    readonly activeCount: bigint;
  };
  readonly scriptSourcesMiddleYield?: {
    readonly utxo: UTxO;
    readonly kind: number;
  };
  readonly phaseANativeItemYield?: {
    readonly utxo: UTxO;
    readonly kind: 0 | 1;
  };
  /** Stage yield first, then the plan's attestation roles, in order. */
  readonly ledgerOutputProofStepYield?: {
    readonly plan: LedgerOutputProofStepPlan;
    readonly utxos: readonly UTxO[];
  };
  /** The four descriptor yields, in `descriptor_roles` order. */
  readonly ledgerOutputProofFinalizeYield?: {
    readonly plan: LedgerOutputProofFinalizePlan;
    readonly utxos: readonly UTxO[];
  };
  readonly cekSelection?: {
    readonly referenceUtxos: readonly UTxO[];
    readonly beginTraversal?: boolean;
    readonly facts: ReturnType<typeof deriveCekSelectionFacts>;
  };
  /** Builds the CEK material route once the reference-input indices are known. */
  readonly materialRoute?: (
    layout: SemanticResolutionLayout,
  ) => ValidationCekMaterialRoute;
  readonly onLayout: (layout: SemanticResolutionLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute semantic resolution",
    );
    const layout: SemanticResolutionLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute semantic resolution",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute semantic resolution",
      ),
      materialReferenceInputIndices: materialReferenceUtxos.map((utxo) =>
        requireReferenceInputIndex(
          ctx,
          utxo,
          "validation dispute semantic resolution CEK material",
        ),
      ),
    };
    onLayout(layout);
    // Option B (#620): the item-semantic `Verify` is transition-only, so the
    // CanonicalDecode complete item takes the generic shape below and the
    // retired proof-item reference route has no arm to target.
    const fields = semanticActionFields({
      resolverIndex,
      semanticResolverIndex,
      inputIndex: layout.inputIndex,
      outputIndex: layout.outputIndex,
      transition,
      auxiliary,
      ...(cekSelection === undefined
        ? {}
        : {
            cekSelectionInvocation: {
              indices: cekSelection.referenceUtxos.map((utxo) =>
                requireReferenceInputIndex(ctx, utxo, "CEK selection yield"),
              ),
              facts: cekSelection.facts,
              beginTraversal: cekSelection.beginTraversal ?? false,
            },
          }),
      ...(phaseANativeItemYield === undefined
        ? {}
        : {
            phaseANativeItemInvocation: {
              kind: phaseANativeItemYield.kind,
              referenceInputIndex: requireReferenceInputIndex(
                ctx,
                phaseANativeItemYield.utxo,
                "phase-A item yield",
              ),
            },
          }),
      ...(scriptSourcesDescriptorYield === undefined
        ? {}
        : {
            scriptSourcesDescriptorInvocation: {
              claim: scriptSourcesDescriptorYield.claim,
              referenceInputIndex: requireReferenceInputIndex(
                ctx,
                scriptSourcesDescriptorYield.utxo,
                "descriptor yield",
              ),
            },
          }),
      ...(scriptSourcesObserverYields === undefined
        ? {}
        : {
            scriptSourcesObserverInvocation: {
              observerHash: scriptSourcesObserverYields.observerHash,
              activeCount: scriptSourcesObserverYields.activeCount,
              indices: scriptSourcesObserverYields.utxos.map((utxo) =>
                requireReferenceInputIndex(ctx, utxo, "observer yield"),
              ),
            },
          }),
      ...(scriptSourcesMiddleYield === undefined
        ? {}
        : {
            scriptSourcesMiddleInvocation: {
              kind: scriptSourcesMiddleYield.kind,
              referenceInputIndex: requireReferenceInputIndex(
                ctx,
                scriptSourcesMiddleYield.utxo,
                "ScriptSources middle yield",
              ),
            },
          }),
      ...(ledgerOutputProofStepYield === undefined
        ? {}
        : {
            ledgerOutputProofStepInvocation: {
              plan: ledgerOutputProofStepYield.plan,
              indices: ledgerOutputProofStepYield.utxos.map((utxo) =>
                requireReferenceInputIndex(
                  ctx,
                  utxo,
                  "ledger output proof yield",
                ),
              ),
            },
          }),
      ...(ledgerOutputProofFinalizeYield === undefined
        ? {}
        : {
            ledgerOutputProofFinalizeInvocation: {
              plan: ledgerOutputProofFinalizeYield.plan,
              indices: ledgerOutputProofFinalizeYield.utxos.map((utxo) =>
                requireReferenceInputIndex(
                  ctx,
                  utxo,
                  "ledger output descriptor yield",
                ),
              ),
            },
          }),
      ...(assetFoldYieldReferenceUtxo === undefined
        ? {}
        : {
            assetFoldYieldReferenceInputIndex: requireReferenceInputIndex(
              ctx,
              assetFoldYieldReferenceUtxo,
              "asset fold yield",
            ),
          }),
      ...(materialRoute === undefined
        ? {}
        : {
            materialRoute: validationCekMaterialRouteData(
              materialRoute(layout),
            ),
          }),
    });
    return Data.to(
      new Constr(1, [
        new Constr(
          resolverIndex === 8 &&
          [19, 21].includes(semanticResolverIndex) &&
          auxiliary.index === 10
            ? 1
            : 0,
          [...fields],
        ),
      ]),
    );
  }) satisfies BuildTxWithRedeemer;

export const makeIndexedValidationStageRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  proofItemReferenceUtxo,
  label,
  encode,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly proofItemReferenceUtxo?: UTxO;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly referenceInputIndex?: bigint;
  }) => string;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        label,
      ),
    };
    onLayout(layout);
    return encode({
      ...layout,
      ...(proofItemReferenceUtxo === undefined
        ? {}
        : {
            referenceInputIndex: requireReferenceInputIndex(
              ctx,
              proofItemReferenceUtxo,
              "validation complete proof item",
            ),
          }),
    });
  }) satisfies BuildTxWithRedeemer;

export type ValidationFinalizationResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly materialReferenceInputOutRefs: readonly string[];
  readonly materialReferenceInputIndices: readonly number[];
  readonly awaitedConfirmation: boolean;
};
