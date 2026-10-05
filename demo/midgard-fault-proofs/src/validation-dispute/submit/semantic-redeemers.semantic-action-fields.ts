import {
  AssetFoldClaim,
  CekSelectionEnvelopeFacts,
  CekSelectionMaterialFacts,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import { buildValidationAssetFoldClaim } from ".././asset-fold.js";
import { type PlutusDataValue, requireConstr } from "./evidence.js";
import {
  hasValidationAuxiliaryShape,
  VALIDATION_AUXILIARY_SHAPES,
  VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES,
  VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES,
} from "./reference-scripts.js";
import {
  type CekSelectionInvocation,
  isLedgerOutputProofFinalizeResolver,
  isLedgerOutputProofStepResolver,
  type LedgerOutputProofFinalizeInvocation,
  type LedgerOutputProofStepInvocation,
  type ScriptSourcesMiddleInvocation,
  type ScriptSourcesObserverInvocation,
} from "./semantic-redeemers.make-prepare-selected-redeemer.js";

export const semanticActionFields = ({
  resolverIndex,
  semanticResolverIndex,
  inputIndex,
  outputIndex,
  transition,
  auxiliary,
  materialRoute,
  assetFoldYieldReferenceInputIndex,
  phaseANativeItemInvocation,
  scriptSourcesMiddleInvocation,
  scriptSourcesObserverInvocation,
  scriptSourcesDescriptorInvocation,
  cekSelectionInvocation,
  ledgerOutputProofStepInvocation,
  ledgerOutputProofFinalizeInvocation,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
  /**
   * The CEK execution-selection material route
   * (`validationCekMaterialRouteData`); required by, and only by, resolver
   * 11 semantic resolver 1.
   */
  readonly materialRoute?: PlutusDataValue;
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
  readonly cekSelectionInvocation?: CekSelectionInvocation;
  readonly ledgerOutputProofStepInvocation?: LedgerOutputProofStepInvocation;
  readonly ledgerOutputProofFinalizeInvocation?: LedgerOutputProofFinalizeInvocation;
}): readonly PlutusDataValue[] => {
  const base: readonly PlutusDataValue[] = [
    inputIndex,
    outputIndex,
    transition,
  ];
  if (resolverIndex !== 11 || semanticResolverIndex !== 1) {
    if (materialRoute !== undefined) {
      throw new Error(
        "CEK material route is permitted only for the CEK execution-selection semantic resolver",
      );
    }
  }
  if (resolverIndex === 8 && [19, 21, 22].includes(semanticResolverIndex)) {
    if (auxiliary.index === 10 && semanticResolverIndex !== 22)
      return [...base, ...auxiliary.fields];
    if (
      scriptSourcesDescriptorInvocation === undefined ||
      scriptSourcesDescriptorInvocation.referenceInputIndex < 0n
    )
      throw new Error("Descriptor scan requires its authenticated yield");
    return [
      ...base,
      scriptSourcesDescriptorInvocation.claim,
      scriptSourcesDescriptorInvocation.referenceInputIndex,
    ];
  }
  if (resolverIndex === 8 && semanticResolverIndex === 25) {
    if (
      scriptSourcesObserverInvocation === undefined ||
      scriptSourcesObserverInvocation.indices.length !== 2 ||
      scriptSourcesObserverInvocation.indices.some((index) => index < 0n)
    )
      throw new Error("Observer resolution requires both authenticated yields");
    return [
      ...base,
      ...auxiliary.fields,
      scriptSourcesObserverInvocation.observerHash,
      scriptSourcesObserverInvocation.activeCount,
      ...scriptSourcesObserverInvocation.indices,
    ];
  }
  if (resolverIndex === 8 && semanticResolverIndex === 0) {
    if (
      scriptSourcesMiddleInvocation === undefined ||
      scriptSourcesMiddleInvocation.referenceInputIndex < 0n ||
      !Number.isInteger(scriptSourcesMiddleInvocation.kind) ||
      scriptSourcesMiddleInvocation.kind < 0 ||
      scriptSourcesMiddleInvocation.kind > 7
    )
      throw new Error(
        "ScriptSources middle resolution requires its exact authenticated yield",
      );
    return [
      ...base,
      auxiliary,
      BigInt(scriptSourcesMiddleInvocation.kind),
      scriptSourcesMiddleInvocation.referenceInputIndex,
    ];
  }
  if (isLedgerOutputProofStepResolver(resolverIndex, semanticResolverIndex)) {
    if (
      !hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofStep,
      )
    )
      throw new Error(
        "Ledger output proof step auxiliary witness cannot construct the selected semantic redeemer",
      );
    if (
      ledgerOutputProofStepInvocation === undefined ||
      ledgerOutputProofStepInvocation.indices.length !==
        1 + ledgerOutputProofStepInvocation.plan.attestationRoles.length ||
      ledgerOutputProofStepInvocation.indices.some((index) => index < 0n)
    )
      throw new Error(
        "Ledger output proof step requires its exact authenticated yields",
      );
    // `VerifyOutputProofStep` field order: the auxiliary's `proof_witness`,
    // then control, successor control, claimed scalar, the stage role and
    // the yield reference-input indices (stage yield first, then
    // `stage_attestation_roles` order).
    return [
      ...base,
      ...auxiliary.fields,
      ledgerOutputProofStepInvocation.plan.controlCbor,
      ledgerOutputProofStepInvocation.plan.nextControlCbor,
      ledgerOutputProofStepInvocation.plan.claimedScalar,
      BigInt(ledgerOutputProofStepInvocation.plan.roleIndex),
      [...ledgerOutputProofStepInvocation.indices],
    ];
  }
  if (
    isLedgerOutputProofFinalizeResolver(resolverIndex, semanticResolverIndex)
  ) {
    if (
      !hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofFinalize,
      )
    )
      throw new Error(
        "Ledger output proof finalize auxiliary witness cannot construct the selected semantic redeemer",
      );
    if (
      ledgerOutputProofFinalizeInvocation === undefined ||
      ledgerOutputProofFinalizeInvocation.indices.length !==
        ledgerOutputProofFinalizeInvocation.plan.attachRoles.length ||
      ledgerOutputProofFinalizeInvocation.indices.some((index) => index < 0n)
    )
      throw new Error(
        "Ledger output proof finalize requires exactly its attach group's authenticated descriptor yields",
      );
    // `VerifyOutputProofFinalize` field order: the control's own descriptor,
    // the auxiliary's `signer_proof`, control, the two claimed leaf summaries,
    // this step's fact-group attach roles (empty at the thin terminal) and
    // the descriptor yield reference-input indices in the same order.
    return [
      ...base,
      ledgerOutputProofFinalizeInvocation.plan.descriptorCbor,
      ...auxiliary.fields,
      ledgerOutputProofFinalizeInvocation.plan.controlCbor,
      ledgerOutputProofFinalizeInvocation.plan.claimedValueSummary,
      ledgerOutputProofFinalizeInvocation.plan.claimedDatumSummary,
      ledgerOutputProofFinalizeInvocation.plan.attachRoles.map((role) =>
        BigInt(role),
      ),
      [...ledgerOutputProofFinalizeInvocation.indices],
    ];
  }
  if (resolverIndex === 5 && semanticResolverIndex === 1) {
    if (
      phaseANativeItemInvocation === undefined ||
      phaseANativeItemInvocation.referenceInputIndex < 0n
    )
      throw new Error(
        "Phase-A item resolution requires its authenticated yield reference index and kind",
      );
    return [
      ...base,
      ...auxiliary.fields,
      phaseANativeItemInvocation.referenceInputIndex,
      BigInt(phaseANativeItemInvocation.kind),
    ];
  }
  if (resolverIndex === 11) {
    // `cek_v1` prepare order: finish (no auxiliary), execution selection
    // (`VerifyExecutionSelection { …, auxiliary, material_route }`), context
    // step (`VerifyContextStep { …, auxiliary }`) and core step
    // (`VerifyCoreStep { …, step }`).
    if (
      semanticResolverIndex === 0 &&
      hasValidationAuxiliaryShape(auxiliary, VALIDATION_AUXILIARY_SHAPES.none)
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.nativeExecutionScan,
      )
    ) {
      if (materialRoute === undefined) {
        throw new Error(
          "CEK execution-selection semantic redeemer requires a material route",
        );
      }
      if (
        cekSelectionInvocation === undefined ||
        cekSelectionInvocation.indices.length !==
          (auxiliary.fields[1] === 0n || cekSelectionInvocation.beginTraversal
            ? 2
            : 4) ||
        cekSelectionInvocation.indices.some((index) => index < 0n)
      ) {
        throw new Error(
          "CEK selection requires exact authenticated yield reference indices",
        );
      }
      return [
        ...base,
        auxiliary,
        materialRoute,
        [...cekSelectionInvocation.indices],
        Data.from(
          Data.to(
            cekSelectionInvocation.facts.envelope,
            CekSelectionEnvelopeFacts,
          ),
        ),
        Data.from(
          Data.to(
            cekSelectionInvocation.facts.material,
            CekSelectionMaterialFacts,
          ),
        ),
        new Constr(cekSelectionInvocation.beginTraversal ? 1 : 0, []),
      ];
    }
    if (
      semanticResolverIndex === 2 &&
      VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES.some((shape) =>
        hasValidationAuxiliaryShape(auxiliary, shape),
      )
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 3 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.cekCoreStep,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "Cek auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 12) {
    // `value_and_mint_v1` prepare order; every item resolver flattens its
    // witness into the action (`ledger_output_index` stands in for the
    // witness's `output_index` on the output-descriptor and output-asset
    // actions, which is a field rename on the wire-identical position).
    const expected =
      VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES[semanticResolverIndex];
    if (
      expected !== undefined &&
      hasValidationAuxiliaryShape(auxiliary, expected)
    ) {
      if ([3, 6, 8].includes(semanticResolverIndex)) {
        if (
          assetFoldYieldReferenceInputIndex === undefined ||
          assetFoldYieldReferenceInputIndex < 0n
        ) {
          throw new Error(
            "Asset fold requires an authenticated yield reference index",
          );
        }
        const claim = Data.from(
          Data.to(
            buildValidationAssetFoldClaim(transition, auxiliary),
            AssetFoldClaim,
          ),
        );
        const coordinates =
          semanticResolverIndex === 3
            ? auxiliary.fields.slice(0, 3)
            : semanticResolverIndex === 6
              ? auxiliary.fields.slice(0, 1)
              : [auxiliary.fields[0]!, auxiliary.fields[4]!];
        return [
          claim,
          ...base,
          ...coordinates,
          assetFoldYieldReferenceInputIndex,
        ];
      }
      return expected[0] === 0 ? base : [...base, ...auxiliary.fields];
    }
    throw new Error(
      "ValueAndMint auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 0) {
    if (
      semanticResolverIndex === 0 &&
      hasValidationAuxiliaryShape(auxiliary, VALIDATION_AUXILIARY_SHAPES.none)
    ) {
      return base;
    }
    // Option B (#620): the item-semantic stage re-checks the transition-only
    // commitment and takes no carriage in any form — the carriage is
    // dereferenced once, at the observe stage's §8.8 door. The retired
    // four-field `Verify` was the only wire a chunk-shaped auxiliary could
    // ever have ridden, so a chunk here is now a refusal, not a route.
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionFieldItem,
      )
    ) {
      return base;
    }
    throw new Error(
      "CanonicalDecode auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 13) {
    if (
      (semanticResolverIndex === 2 ||
        semanticResolverIndex === 4 ||
        semanticResolverIndex === 6 ||
        semanticResolverIndex === 7) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 0 &&
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.ledgerDeltaOperation,
        )) ||
      (semanticResolverIndex === 1 &&
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.ledgerDeltaReplay,
        )) ||
      (semanticResolverIndex === 3 &&
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.ledgerDeltaOutput,
        )) ||
      (semanticResolverIndex === 5 &&
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.ledgerDeltaProofFrame,
        ))
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "LedgerDelta auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 7) {
    if (
      (semanticResolverIndex === 0 || semanticResolverIndex === 1) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    // The step (3) and finalize (4) dispatchers are handled by the shared-LOP
    // arms above, which append the yield claims and indices.
    if (
      (semanticResolverIndex === 2 &&
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.scheduledLedgerMembership,
        )) ||
      (semanticResolverIndex === 5 &&
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.scheduledLedgerNonMembership,
        ))
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "ResolveInputs auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 8) {
    if (semanticResolverIndex === 0) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofBegin,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    // The step (2) and finalize (3) dispatchers are handled by the shared-LOP
    // arms above, which append the yield claims and indices.
    if (
      semanticResolverIndex === 4 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 5 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 6 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 7 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.scriptSourceHashBlock,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      (semanticResolverIndex === 8 || semanticResolverIndex === 9) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 10 || semanticResolverIndex === 12) &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.scriptSourceScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 11 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.scriptSourceScan,
      )
    ) {
      return [
        ...base,
        auxiliary.fields[0]!,
        auxiliary.fields[1]!,
        auxiliary.fields[2]!,
        auxiliary.fields[4]!,
        auxiliary.fields[5]!,
        auxiliary.fields[6]!,
        auxiliary.fields[7]!,
      ];
    }
    if (
      semanticResolverIndex === 13 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 14 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 15 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionRedeemerItemBegin,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 16 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 17 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.scriptSourceScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 18 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 19 &&
      (hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.redeemerScanBegin,
      ) ||
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
        ))
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 20 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 21 || semanticResolverIndex === 22) &&
      (hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.redeemerScanBegin,
      ) ||
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
        ))
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 23 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 24 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.scriptPurposeScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 25 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 26 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.scriptPurposeScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 27 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    throw new Error(
      "ScriptSources auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 9) {
    if (
      semanticResolverIndex === 0 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.nativeExecutionDescriptor,
      )
    ) {
      if (semanticResolverIndex === 1) {
        const firstChunk = requireConstr({
          value: auxiliary.fields[15]!,
          index: 0,
          fields: 1,
          label: "validation NativeScripts native first chunk",
        });
        if (auxiliary.fields[1] !== 0n) {
          throw new Error(
            "NativeScripts native semantic route requires language tag 0",
          );
        }
        return [
          ...base,
          auxiliary.fields[0]!,
          ...auxiliary.fields.slice(2, 15),
          firstChunk.fields[0]!,
          auxiliary.fields[16]!,
        ];
      }
      if (semanticResolverIndex === 2) {
        const languageTag = auxiliary.fields[1];
        const noFirstChunk = requireConstr({
          value: auxiliary.fields[15]!,
          index: 1,
          fields: 0,
          label: "validation NativeScripts effectful first chunk",
        });
        const signerPeaks = auxiliary.fields[16];
        if (
          (languageTag !== 3n && languageTag !== 128n) ||
          noFirstChunk.fields.length !== 0 ||
          !Array.isArray(signerPeaks) ||
          signerPeaks.length !== 0
        ) {
          throw new Error(
            "NativeScripts effectful semantic route has native-only evidence",
          );
        }
        return [...base, ...auxiliary.fields.slice(0, 15)];
      }
    }
    throw new Error(
      "NativeScripts auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (
    hasValidationAuxiliaryShape(auxiliary, VALIDATION_AUXILIARY_SHAPES.none)
  ) {
    return base;
  }
  if (
    hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
    )
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.requiredSignerItem,
    )
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex >= 2 &&
    semanticResolverIndex <= 7 &&
    hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.nativeScriptToken,
    )
  ) {
    return [...base, auxiliary.fields[0]!, auxiliary.fields[1]!];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex >= 8 &&
    semanticResolverIndex <= 12 &&
    hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.nativeScriptToken,
    )
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex === 13 &&
    hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.nativeScriptFrame,
    )
  ) {
    return [...base, auxiliary.fields[0]!];
  }
  if (resolverIndex === 6) {
    if (
      semanticResolverIndex === 0 &&
      hasValidationAuxiliaryShape(auxiliary, VALIDATION_AUXILIARY_SHAPES.none)
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
  }
  throw new Error(
    "Validation auxiliary witness cannot construct the selected semantic redeemer",
  );
};
