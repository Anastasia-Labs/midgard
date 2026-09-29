import {
  AssetFoldClaim,
  CekSelectionEnvelopeFacts,
  CekSelectionMaterialFacts,
  deriveCekSelectionFacts,
  FraudProofTokenDatum,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type ValidationAuxiliaryWitness as ValidationAuxiliaryWitnessData,
  type ValidationCekMaterialRoute,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  toUnit,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  deriveLedgerOutputProofFinalizePlan,
  deriveLedgerOutputProofStepPlan,
  type LedgerOutputProofFinalizePlan,
  type LedgerOutputProofStepPlan,
} from "../../ledger-output-proof-plan.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  outRefLabel,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../step-support.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { buildValidationAssetFoldClaim } from ".././asset-fold.js";
import {
  encodeWithRuntimeSchema,
  type PlutusDataValue,
  requireConstr,
  validationCanonicalDecodePrepareSelectedSpendRedeemerRuntimeSchema,
  validationCekMaterialRouteData,
  type ValidationOneStepSubmissionArgument,
  validationPrepareSelectedSpendRedeemerRuntimeSchema,
} from "./evidence.js";
import {
  type ContinueLayout,
  type FinalizeLayout,
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
} from "./redeemers.js";
import {
  hasValidationAuxiliaryShape,
  requireStagedOneStepArgument,
  VALIDATION_AUXILIARY_SHAPES,
  VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES,
  VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES,
} from "./reference-scripts.js";
import { requireL1ProofEnvelope } from "./transaction-material.js";
import {
  reachOptionalPreSubmitBoundary,
  type ValidationDisputeValidityRange,
} from "./validity.js";

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

type ScriptSourcesObserverInvocation = {
  readonly observerHash: string;
  readonly activeCount: bigint;
  readonly indices: readonly bigint[];
};
type ScriptSourcesMiddleInvocation = {
  readonly kind: number;
  readonly referenceInputIndex: bigint;
};

type CekSelectionInvocation = {
  readonly beginTraversal?: boolean;
  readonly indices: readonly bigint[];
  readonly facts: ReturnType<typeof deriveCekSelectionFacts>;
};

type LedgerOutputProofStepInvocation = {
  readonly plan: LedgerOutputProofStepPlan;
  /** Stage yield first, then the plan's attestation roles, in order. */
  readonly indices: readonly bigint[];
};

type LedgerOutputProofFinalizeInvocation = {
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

const semanticActionFields = ({
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
    // `VerifyOutputProofFinalize` field order: the auxiliary's
    // `descriptor_cbor` and `signer_proof`, then control, the two claimed
    // leaf summaries, the attach roles of this step's fact group (empty at
    // the thin terminal) and the descriptor yield reference-input indices in
    // the same order.
    return [
      ...base,
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

type ValidationFinalizingSpendLayout = Omit<
  FinalizeLayout,
  "computationThreadMintRedeemerIndex"
> & {
  /** Supplied order is semantic (root order); values are canonical tx indices. */
  readonly materialReferenceInputIndices: readonly bigint[];
};

const makeValidationFinalizingSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  materialReferenceUtxos,
  label,
  encodeRedeemer,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly materialReferenceUtxos: readonly UTxO[];
  readonly label: string;
  readonly encodeRedeemer: (layout: ValidationFinalizingSpendLayout) => string;
  readonly onLayout: (layout: ValidationFinalizingSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        `${label} fraud proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        `${label} fraud-proof mint`,
      ),
      materialReferenceInputIndices: materialReferenceUtxos.map((utxo) =>
        requireReferenceInputIndex(ctx, utxo, `${label} CEK material`),
      ),
    };
    onLayout(layout);
    return encodeRedeemer(layout);
  }) satisfies BuildTxWithRedeemer;

type ValidationFinalizationTransactionParams = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly contracts: Awaited<
    ReturnType<typeof resolveValidationTraceDisputeDeploymentContracts>
  >["contracts"];
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadOutRef: string;
  readonly token: ReturnType<typeof requireComputationThreadToken>;
  readonly spendingScript: {
    readonly spendingScript: Script;
  };
  /**
   * Published authenticated reference-script UTxO carrying the spending
   * validator. When present the transaction consumes the validator through
   * `readFrom` and must not embed the validator body inside the L1 proof
   * envelope.
   */
  readonly spendingScriptReferenceUtxo?: UTxO;
  /** Required published shared minting witnesses for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly spendLabel: string;
  readonly encodeSpendRedeemer: (
    layout: ValidationFinalizingSpendLayout,
  ) => string;
  readonly materialReferenceUtxos?: readonly UTxO[];
  readonly validityRange: ValidationDisputeValidityRange;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
};

type PreparedValidationFinalizationTransaction = {
  readonly lucid: LucidEvolution;
  readonly signed: TxSigned;
  readonly threadOutRef: string;
  readonly fraudProofUnit: string;
  readonly layout: FinalizeLayout;
  readonly materialReferenceInputOutRefs: readonly string[];
  readonly materialReferenceInputIndices: readonly number[];
};

const prepareValidationFinalizationTransaction = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadOutRef,
  token,
  spendingScript,
  spendingScriptReferenceUtxo,
  witnessReferenceScripts,
  spendLabel,
  encodeSpendRedeemer,
  materialReferenceUtxos = [],
  validityRange,
}: ValidationFinalizationTransactionParams): Promise<PreparedValidationFinalizationTransaction> => {
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, token.assetName);
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  let partialLayout: ValidationFinalizingSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const materialOutRefs = materialReferenceUtxos.map(outRefLabel);
  if (new Set(materialOutRefs).size !== materialOutRefs.length) {
    throw new Error(`${spendLabel} CEK material references must be unique`);
  }
  const spendingScriptCarriage = witnessSpendingValidatorCarriage({
    script: spendingScript.spendingScript,
    referenceUtxo: spendingScriptReferenceUtxo,
    label: `${spendLabel} spending validator`,
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${spendLabel} computation-thread mint`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${spendLabel} fraud-proof mint`,
  });
  const referenceInputs = [
    ...materialReferenceUtxos,
    ...spendingScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const referenceOutRefs = referenceInputs.map(outRefLabel);
  if (new Set(referenceOutRefs).size !== referenceOutRefs.length) {
    throw new Error(`${spendLabel} reference inputs must be unique`);
  }
  let withReferenceInputs = lucid.newTx().collectFrom([feeInput]);
  if (referenceInputs.length > 0) {
    withReferenceInputs = withReferenceInputs.readFrom(referenceInputs);
  }
  const base = withReferenceInputs
    .collectFrom(
      [threadUtxo],
      makeValidationFinalizingSpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        materialReferenceUtxos,
        label: spendLabel,
        encodeRedeemer: encodeSpendRedeemer,
        onLayout: (layout) => {
          partialLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [token.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .validFrom(validityRange.validFrom)
    .validTo(validityRange.validTo);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(spendingScriptCarriage.attach(base)),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    partialLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(`BuildTxWithRedeemer did not resolve ${spendLabel} layout`);
  }
  const layout: FinalizeLayout = {
    ...partialLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), spendLabel);
  return {
    lucid,
    signed,
    threadOutRef,
    fraudProofUnit,
    layout,
    materialReferenceInputOutRefs: materialOutRefs,
    materialReferenceInputIndices:
      partialLayout.materialReferenceInputIndices.map(Number),
  };
};

const submitPreparedValidationFinalizationTransaction = async ({
  prepared,
  awaitConfirmation,
  preSubmitBoundary,
  referenceScriptCandidates,
}: {
  readonly prepared: PreparedValidationFinalizationTransaction;
  readonly awaitConfirmation: boolean;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly referenceScriptCandidates?: readonly {
    readonly role: string;
    readonly utxo: UTxO | undefined;
    readonly expectedScript?: Script;
  }[];
}): Promise<ValidationFinalizationResult> => {
  await reachOptionalPreSubmitBoundary({
    signed: prepared.signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates,
  });
  const txHash = await prepared.signed.submit();
  if (awaitConfirmation) {
    await prepared.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef: prepared.threadOutRef,
    fraudProofOutRef: `${txHash}#${prepared.layout.outputIndex.toString()}`,
    fraudProofUnit: prepared.fraudProofUnit,
    inputIndex: Number(prepared.layout.inputIndex),
    outputIndex: Number(prepared.layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      prepared.layout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      prepared.layout.fraudProofMintRedeemerIndex,
    ),
    materialReferenceInputOutRefs: prepared.materialReferenceInputOutRefs,
    materialReferenceInputIndices: prepared.materialReferenceInputIndices,
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitValidationFinalizationTransaction = async (
  params: ValidationFinalizationTransactionParams & {
    readonly awaitConfirmation: boolean;
  },
): Promise<ValidationFinalizationResult> =>
  submitPreparedValidationFinalizationTransaction({
    prepared: await prepareValidationFinalizationTransaction(params),
    awaitConfirmation: params.awaitConfirmation,
    preSubmitBoundary: params.preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: `${params.spendLabel} spending validator`,
        utxo: params.spendingScriptReferenceUtxo,
      },
      {
        role: "V1 fraud-proof computation-thread minting",
        utxo: params.witnessReferenceScripts?.computationThreadMint,
      },
      {
        role: "V1 fraud-proof token minting",
        utxo: params.witnessReferenceScripts?.fraudProofMint,
      },
    ],
  });
