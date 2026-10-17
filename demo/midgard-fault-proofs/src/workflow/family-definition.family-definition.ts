import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "./complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FamilyAdapterArm,
  type FamilyAssemblyContext,
  type FamilyCategory,
  type FamilyFieldCarriageRequirement,
  type FamilyStepTuple,
  type FamilyTransactionPort,
  type FaultProofWitnessRole,
  LINEAR_FAMILY_DEFINITION_VERSION,
  type LinearFamilyPrerequisiteInput,
  type LinearFamilyStepDatumSchema,
} from "./family-definition.family-adapter-arm.js";
import type { FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import {
  type LinearFamilyCategory,
  linearFamilySpec,
} from "./linear-family-spec.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";

export type LinearFamilyAdapterArm<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = FamilyAdapterArm<Category, Witness, Certificate>;

/**
 * An assembled workflow. It is not parameterised by witness role: the stored
 * definition is widened to every role so a workflow assembled from a narrow
 * definition is assignable wherever the category's workflow is expected.
 */
export type ManifestBoundFamilyWorkflow<
  Category extends FamilyCategory,
  Certificate extends boolean = boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
> = Readonly<{
  definition: FamilyDefinition<
    Category,
    FaultProofWitnessRole,
    Certificate,
    StepCount,
    Runtime
  >;
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  l1: FraudProofFamilyL1ObservationPort<Category>;
  transactions: FamilyTransactionPort<Category>;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  replayer: CompleteCanonicalReplay;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export type ManifestBoundLinearFamilyWorkflow<
  Category extends LinearFamilyCategory,
  Certificate extends boolean = boolean,
> = ManifestBoundFamilyWorkflow<Category, Certificate>;

export type FamilyDefinition<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole = FaultProofWitnessRole,
  Certificate extends boolean = boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
> = Readonly<{
  definitionVersion: typeof LINEAR_FAMILY_DEFINITION_VERSION;
  category: Category;
  /** Per-step thread datum schemas the deployment binding decodes with. */
  stepDatumSchemas: FamilyStepTuple<
    Category,
    StepCount,
    LinearFamilyStepDatumSchema
  >;
  /** Witness scripts whose published references this family binds. */
  witnessRoles: readonly Witness[];
  /** Additional published script roles mapped to finalized manifest contracts. */
  auxiliaryReferenceScripts?: Readonly<Record<string, string>>;
  /**
   * Whether the assembly binds the field-preimage certificate minting
   * reference script and refuses a manifest without the certificate policy.
   * A port that only reads the policy id into a verdict leaves this false.
   */
  fieldPreimageCertificate: Certificate;
  /** The exact closed replay bundle `runOrResume` launches with. */
  replayer: (
    context: FamilyAssemblyContext<
      Category,
      Witness,
      Certificate,
      StepCount,
      Runtime
    >,
  ) => CompleteCanonicalReplay;
  adapter: FamilyAdapterArm<Category, Witness, Certificate, StepCount, Runtime>;
  /** Applied in declared order, before the proof-chunk prerequisite. */
  fieldCarriage?: readonly FamilyFieldCarriageRequirement<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  >[];
  /** Proof CBOR an action publishes as chunks, or null for none. */
  proofChunk?: (
    context: FamilyAssemblyContext<
      Category,
      Witness,
      Certificate,
      StepCount,
      Runtime
    >,
    input: LinearFamilyPrerequisiteInput & { readonly headerHash: string },
  ) => string | null | Promise<string | null>;
  /** Extra members of the assembled workflow object. */
  extend?: (
    context: FamilyAssemblyContext<
      Category,
      Witness,
      Certificate,
      StepCount,
      Runtime
    >,
    workflow: ManifestBoundFamilyWorkflow<
      Category,
      Certificate,
      StepCount,
      Runtime
    >,
  ) => Readonly<Record<string, unknown>>;
}>;

export type LinearFamilyDefinition<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole = FaultProofWitnessRole,
  Certificate extends boolean = boolean,
> = FamilyDefinition<Category, Witness, Certificate>;

/**
 * The definition shape a heterogeneous table row admits: any witness-role
 * set, and either certificate polarity. Kept as a union over the polarity so
 * the context's `certificate` member stays exact in each arm.
 */
export type LinearFamilyDefinitionOf<Category extends LinearFamilyCategory> =
  | LinearFamilyDefinition<Category, FaultProofWitnessRole, true>
  | LinearFamilyDefinition<Category, FaultProofWitnessRole, false>;

/**
 * Freezes a definition and infers its exact category, roles, polarity and,
 * for a cursor arm, the step count of its spec.
 */
export const defineFamily = <
  Category extends FamilyCategory,
  const Witness extends FaultProofWitnessRole,
  const Certificate extends boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
>(
  definition: Omit<
    FamilyDefinition<Category, Witness, Certificate, StepCount, Runtime>,
    "definitionVersion"
  >,
): FamilyDefinition<Category, Witness, Certificate, StepCount, Runtime> =>
  Object.freeze({
    definitionVersion: LINEAR_FAMILY_DEFINITION_VERSION,
    ...definition,
  });

/** `defineFamily` for a linear category. */
export const defineLinearFamily = <
  Category extends LinearFamilyCategory,
  const Witness extends FaultProofWitnessRole,
  const Certificate extends boolean,
>(
  definition: Omit<
    LinearFamilyDefinition<Category, Witness, Certificate>,
    "definitionVersion"
  >,
): LinearFamilyDefinition<Category, Witness, Certificate> =>
  defineFamily(definition);

/**
 * The manifest contract names of a definition's chain steps, in step order:
 * the linear spec's for a linear arm, the arm's own for a cursor arm. Typed
 * on the two members it reads so a heterogeneous table row fits.
 */
export const familyStepContractNames = (
  definition: Readonly<{
    category: FamilyCategory;
    adapter: Readonly<
      | { kind: "linear" }
      | { kind: "cursor"; stepContractNames: readonly string[] }
    >;
  }>,
): readonly string[] =>
  definition.adapter.kind === "linear"
    ? linearFamilySpec(definition.category as LinearFamilyCategory).steps.map(
        (step) => step.manifestContractName,
      )
    : definition.adapter.stepContractNames;
