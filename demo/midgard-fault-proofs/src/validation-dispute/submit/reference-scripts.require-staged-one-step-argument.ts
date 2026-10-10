import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeHash32,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  PreparedValidationResolutionDatum,
  PreparedValidationResolutionState,
  type SharedRedeemerItemStages,
  ValidationAuxiliaryWitness,
  type ValidationAuxiliaryWitness as ValidationAuxiliaryWitnessData,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import {
  type CekProgramMaterialNecessityReceiptSet,
  type CekRouteMaterial,
} from "@al-ft/midgard-validation";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";

import { deriveScriptSourcesRedeemerItemPlan } from "../../redeemer-item-plan.js";
import {
  exactPlutusDataFromCbor,
  type PlutusDataValue,
  validateCekSubmissionEvidence,
  validationOneStepEvidenceHashFromData,
  type ValidationOneStepSubmissionArgument,
} from "./evidence.js";
import {
  auxiliaryShape,
  validationSemanticResolverGlobalIndex,
} from "./reference-scripts.auxiliary-shape.js";
import {
  hasValidationAuxiliaryShape,
  VALIDATION_AUXILIARY_SHAPES,
  VALIDATION_SEMANTIC_RESOLVER_COUNTS,
} from "./reference-scripts.validation-auxiliary-shapes.js";

export const requireStagedOneStepArgument = (
  argument: ValidationOneStepSubmissionArgument,
): {
  readonly transition: ValidationOneStepWitness;
  readonly transitionData: PlutusDataValue;
  readonly auxiliaryData: PlutusDataValue;
  readonly auxiliaryWitness: ValidationAuxiliaryWitnessData;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly evidenceHash: string;
  readonly cekContextSuccessorWorkWitnessCbor?: Uint8Array;
  readonly cekRouteMaterial?: CekRouteMaterial;
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSet;
} => {
  const validatedCekEvidence = validateCekSubmissionEvidence(argument);
  if (
    !Number.isSafeInteger(argument.resolverIndex) ||
    argument.resolverIndex < 0 ||
    argument.resolverIndex >= VALIDATION_SEMANTIC_RESOLVER_COUNTS.length
  ) {
    throw new Error(
      "Staged validation one-step argument must select a prepare resolver",
    );
  }
  const semanticResolverIndex = argument.semanticResolverIndex;
  const semanticResolverCount =
    VALIDATION_SEMANTIC_RESOLVER_COUNTS[argument.resolverIndex]!;
  if (
    !Number.isSafeInteger(semanticResolverIndex) ||
    semanticResolverIndex < 0 ||
    semanticResolverIndex >= semanticResolverCount
  ) {
    throw new Error(
      "Validation one-step argument selects an unavailable semantic resolver",
    );
  }
  const transitionData = exactPlutusDataFromCbor(
    argument.transitionCbor,
    "validation transition",
  );
  const auxiliaryData = exactPlutusDataFromCbor(
    argument.auxiliaryCbor,
    "validation auxiliary witness",
  );
  const auxiliaryWitness = Data.from(
    Buffer.from(argument.auxiliaryCbor).toString("hex"),
    ValidationAuxiliaryWitness,
  );
  const transition = Data.from(
    Buffer.from(argument.transitionCbor).toString("hex"),
    ValidationOneStepWitness,
  );
  const auxiliary = auxiliaryShape({
    resolverIndex: argument.resolverIndex,
    semanticResolverIndex,
    auxiliary: auxiliaryData,
  });
  return {
    transition,
    transitionData,
    auxiliaryData,
    auxiliaryWitness,
    auxiliary,
    semanticResolverIndex,
    semanticResolverGlobalIndex: validationSemanticResolverGlobalIndex(
      argument.resolverIndex,
      semanticResolverIndex,
    ),
    // Option B (#620): the canonical-decode resolver commits to the transition
    // alone — the auxiliary hashed into `evidence_hash` is `NoAuxiliaryWitness`
    // whatever carriage the auxiliary witness names, because the carriage is
    // dereferenced (and content-checked) only at the observe stage's §8.8 door.
    // Every other resolver still freezes its auxiliary into the commitment.
    evidenceHash: validationOneStepEvidenceHashFromData(
      transitionData,
      argument.resolverIndex === 0 ? new Constr(0, []) : auxiliaryData,
    ),
    ...validatedCekEvidence,
  };
};

/**
 * The ScriptSources stage-one route (prepare resolver 8, semantic resolver 28)
 * whose redeemer-item step is split over the shared redeemer-item stages: a
 * chain of transactions, each resumed against the preparation its entry
 * stage consumed.
 */
export const isSplitScriptSourcesItemRoute = (
  argument: Pick<ValidationOneStepSubmissionArgument, "resolverIndex">,
  staged: Pick<
    ReturnType<typeof requireStagedOneStepArgument>,
    "semanticResolverIndex" | "auxiliary"
  >,
): boolean =>
  argument.resolverIndex === 8 &&
  staged.semanticResolverIndex === 28 &&
  hasValidationAuxiliaryShape(
    staged.auxiliary,
    VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
  );

/** Reconstruct immutable output bindings from the exact retained preparation and claim. */
export const deriveScriptSourcesItemSubmissionPlan = ({
  preparedCbor,
  oneStepArgument,
  stages,
  deploymentId,
}: {
  readonly preparedCbor: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  readonly stages: SharedRedeemerItemStages;
  readonly deploymentId: string;
}) => {
  if (!/^(?:[0-9a-f]{2})+$/.test(preparedCbor))
    throw new Error(
      "ScriptSources prepared datum is not exact hexadecimal CBOR",
    );
  aikenSerialisedPlutusDataCborPreservingMapOrder(preparedCbor);
  const prepared = Data.from(preparedCbor, PreparedValidationResolutionDatum);
  const staged = requireStagedOneStepArgument(oneStepArgument);
  if (
    prepared.data === null ||
    prepared.data.resolution.pre_state.phase !== "ScriptSources" ||
    oneStepArgument.resolverIndex !== 8 ||
    staged.semanticResolverIndex !== 28 ||
    staged.evidenceHash !== prepared.data.evidence_hash
  )
    throw new Error(
      "Retained ScriptSources item evidence differs from its authenticated preparation",
    );
  const bindings = deriveScriptSourcesRedeemerItemPlan({
    preparedResolution: Data.from(
      Data.to(prepared.data, PreparedValidationResolutionState),
    ),
    transition: staged.transitionData,
    auxiliary: staged.auxiliary,
    stages,
    deploymentId,
  });
  const datum = (state: PlutusDataValue) =>
    Data.to(new Constr(0, [prepared.fraud_prover, new Constr(0, [state])]));
  const identity = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardScriptSourcesItemSubmissionV1", "ascii"),
      encodeCbor([
        Buffer.from(preparedCbor, "hex"),
        Buffer.from(oneStepArgument.transitionCbor),
        Buffer.from(oneStepArgument.auxiliaryCbor),
        Buffer.from(deploymentId, "hex"),
        bindings.map((binding) =>
          Buffer.from(binding.validator.spendingScriptHash, "hex"),
        ),
      ]),
    ]),
  ).toString("hex");
  return {
    identity,
    preparedCbor,
    fraudProver: prepared.fraud_prover,
    bindings: bindings.map((binding) => ({
      ...binding,
      inputDatumCbor: datum(binding.inputState),
      outputDatumCbor: datum(binding.outputState),
    })),
  };
};

/** A checkpoint is resumable only at one exact canonical address-and-datum pair. */
export const scriptSourcesItemResumeIndex = ({
  plan,
  thread,
}: {
  readonly plan: ReturnType<typeof deriveScriptSourcesItemSubmissionPlan>;
  readonly thread: Pick<UTxO, "address" | "datum">;
}): number => {
  if (thread.datum == null)
    throw new Error("ScriptSources item checkpoint has no inline datum");
  const actual = aikenSerialisedPlutusDataCborPreservingMapOrder(thread.datum);
  const matching = plan.bindings.flatMap((binding, index) =>
    binding.validator.spendingScriptAddress === thread.address &&
    aikenSerialisedPlutusDataCborPreservingMapOrder(binding.inputDatumCbor) ===
      actual
      ? [index]
      : [],
  );
  if (matching.length !== 1)
    throw new Error(
      "ScriptSources item checkpoint is not an exact canonical stage",
    );
  return matching[0]!;
};
