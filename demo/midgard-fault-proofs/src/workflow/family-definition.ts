/**
 * Family definition: the frozen, per-family data that the manifest-bound
 * family assembly (`manifest-bound-family-assembly.ts`) turns into a running
 * workflow. It is separate from the family's spec, which stays pure data for
 * the spec-driven state machine: a linear family's `LinearFamilySpec` names
 * its step contracts and chain shape, a cursor family's `CursorFamilySpec`
 * fixes only its chain topology.
 *
 * A definition carries what differs between families and nothing else: the
 * step datum schemas, the witness reference-script roles, whether the
 * field-preimage certificate is required, the complete replayer, the adapter
 * arm (a linear transaction port, or a cursor spec with its step contract
 * names, action refiner and cursor transaction port) and the optional
 * prerequisites. The assembly owns everything the families used to restate:
 * deployment binding, signer assertion, certificate requirement,
 * reference-script resolution over the step contract names, L1 observation
 * port, adapter, prerequisite decoration order, terminal verifier and
 * release-finality authority.
 *
 * The `Linear*` names are the original, linear-only spellings; each is an
 * alias over the family-wide type with the same meaning.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "./complete-replay.js";
import type {
  createCursorFamilyWorkflowAdapter,
  CursorFamilyTransactionPort,
} from "./cursor-family-adapter.js";
import type { CursorFamilySpec } from "./cursor-family-state.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import type { FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type { PreimageCarriageRequirement } from "./field-carriage-prerequisite.js";
import type { JournalJsonObject } from "./journal.js";
import type { LinearFamilyTransactionPort } from "./linear-family-adapter.js";
import {
  type LinearFamilyCategory,
  linearFamilySpec,
  type LinearFamilySpecOf,
} from "./linear-family-spec.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.js";
import type { FraudProofRawL1FamilyDefinition } from "./raw-l1-family-derivation.js";
import type { FraudProofRawL1SnapshotAuthority } from "./raw-l1-snapshot.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";

/** The identity every definition carries, linear or cursor. */
export const LINEAR_FAMILY_DEFINITION_VERSION =
  "midgard-production-linear-family-definition-v1" as const;

/**
 * A category the assembly can build a workflow for. The definition's adapter
 * arm decides whether its steps come from the linear spec or a cursor spec.
 */
export type FamilyCategory = FraudProofCatalogueCategoryName;

/** The Lucid datum schema one computation-thread step's datum decodes with. */
export type LinearFamilyStepDatumSchema = NonNullable<
  FraudProofRawL1FamilyDefinition["computationThread"]["steps"][number]["datumSchema"]
>;

/** A shared witness script a family's transactions execute by reference. */
export type FaultProofWitnessRole = keyof FaultProofWitnessReferenceScripts;

type PerStep<Steps extends readonly unknown[], Value> = {
  readonly [Index in keyof Steps]: Value;
};

type TupleOfLength<
  Count extends number,
  Value,
  Accumulated extends readonly Value[] = readonly [],
> = Accumulated["length"] extends Count
  ? Accumulated
  : TupleOfLength<Count, Value, readonly [...Accumulated, Value]>;

/** A tuple of `StepCount` values; a plain array when the count is unknown. */
type CursorStepTuple<StepCount extends number, Value> = number extends StepCount
  ? readonly Value[]
  : StepCount extends number
    ? TupleOfLength<StepCount, Value>
    : never;

/**
 * One `Value` per chain step, in step order. A linear category's step count
 * is its spec's tuple length; any other category's is the `StepCount` its
 * cursor arm's spec declares.
 */
export type FamilyStepTuple<
  Category extends FamilyCategory,
  StepCount extends number,
  Value,
> = Category extends LinearFamilyCategory
  ? PerStep<LinearFamilySpecOf<Category>["steps"], Value>
  : CursorStepTuple<StepCount, Value>;

/** One published reference-script UTxO per spec step, in step order. */
export type LinearFamilyStepReferenceScripts<
  Category extends LinearFamilyCategory,
> = PerStep<LinearFamilySpecOf<Category>["steps"], UTxO>;

/** One datum schema per spec step, in step order. */
export type LinearFamilyStepDatumSchemas<
  Category extends LinearFamilyCategory,
> = PerStep<LinearFamilySpecOf<Category>["steps"], LinearFamilyStepDatumSchema>;

/**
 * The published reference scripts a family needs: its own step scripts, the
 * witness scripts for its declared roles, and the field-preimage certificate
 * minting policy when the definition requires the certificate.
 */
export type FamilyReferenceScripts<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
> = Readonly<
  {
    steps: FamilyStepTuple<Category, StepCount, UTxO>;
    witnesses: FaultProofWitnessReferenceScripts & {
      readonly [Role in Witness]: UTxO;
    };
  } & (Certificate extends true
    ? { fieldPreimageCertificateMint: UTxO }
    : unknown)
>;

export type LinearFamilyReferenceScripts<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = FamilyReferenceScripts<Category, Witness, Certificate>;

export type ManifestBoundFamilyWorkflowConfig<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
> = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: FamilyReferenceScripts<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundLinearFamilyWorkflowConfig<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = ManifestBoundFamilyWorkflowConfig<Category, Witness, Certificate>;

export type FieldPreimageCertificateBinding = Readonly<{
  policyId: string;
  mintingScript: Script;
}>;

/**
 * Everything the assembly has bound before it asks the family for its
 * transaction port, replayer and prerequisites. The L1 port is guaranteed to
 * carry the raw-L1 and publication authorities.
 */
export type FamilyAssemblyContext<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
> = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  references: FamilyReferenceScripts<Category, Witness, Certificate, StepCount>;
  l1: FraudProofFamilyL1ObservationPort<Category> & {
    readonly rawL1: FraudProofRawL1SnapshotAuthority;
  };
  certificate: Certificate extends true
    ? FieldPreimageCertificateBinding
    : null;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type LinearFamilyAssemblyContext<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = FamilyAssemblyContext<Category, Witness, Certificate>;

export type LinearFamilyPrerequisiteInput = Readonly<{
  action: FraudProofWorkflowAction;
  artifact: JournalJsonObject;
}>;

/**
 * One authenticated field-carriage prerequisite. A family lists as many as it
 * has field plans; the assembly applies them in declared order, all before
 * the proof-chunk prerequisite.
 */
export type FamilyFieldCarriageRequirement<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
> = Readonly<{
  /** Raw-datum carriage instead of a compact-field opening. */
  rawDatum?: boolean;
  requirementForAction: (
    context: FamilyAssemblyContext<Category, Witness, Certificate, StepCount>,
    input: LinearFamilyPrerequisiteInput,
  ) =>
    | PreimageCarriageRequirement
    | null
    | Promise<PreimageCarriageRequirement | null>;
}>;

export type LinearFamilyFieldCarriageRequirement<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = FamilyFieldCarriageRequirement<Category, Witness, Certificate>;

/**
 * The transaction port an assembled workflow exposes: the linear port for a
 * linear category (a category outside the linear list has no linear port),
 * or the cursor port.
 */
export type FamilyTransactionPort<Category extends FamilyCategory> =
  | LinearFamilyTransactionPort<Category & LinearFamilyCategory>
  | CursorFamilyTransactionPort<Category>;

export type FamilyAdapterArm<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
> =
  | Readonly<{
      /** Steps come from the category's linear spec. */
      kind: "linear";
      transactionPort: (
        context: FamilyAssemblyContext<
          Category,
          Witness,
          Certificate,
          StepCount
        >,
      ) => LinearFamilyTransactionPort<Category & LinearFamilyCategory>;
    }>
  | Readonly<{
      /**
       * Cursor families share the assembly's head and tail but drive a
       * cursor-family adapter. The cursor spec fixes only the chain topology,
       * so the arm also names the step contracts; the spec's step count sizes
       * the definition's step tuples.
       */
      kind: "cursor";
      spec: CursorFamilySpec<Category> & Readonly<{ stepCount: StepCount }>;
      /** Manifest contract names of the chain's steps, in step order. */
      stepContractNames: FamilyStepTuple<Category, StepCount, string>;
      refineAction?: NonNullable<
        Parameters<
          typeof createCursorFamilyWorkflowAdapter<Category>
        >[0]["refineAction"]
      >;
      transactionPort: (
        context: FamilyAssemblyContext<
          Category,
          Witness,
          Certificate,
          StepCount
        >,
      ) => CursorFamilyTransactionPort<Category>;
    }>;

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
> = Readonly<{
  definition: FamilyDefinition<
    Category,
    FaultProofWitnessRole,
    Certificate,
    StepCount
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
  /**
   * Whether the assembly binds the field-preimage certificate minting
   * reference script and refuses a manifest without the certificate policy.
   * A port that only reads the policy id into a verdict leaves this false.
   */
  fieldPreimageCertificate: Certificate;
  /** The exact closed replay bundle `runOrResume` launches with. */
  replayer: (
    context: FamilyAssemblyContext<Category, Witness, Certificate, StepCount>,
  ) => CompleteCanonicalReplay;
  adapter: FamilyAdapterArm<Category, Witness, Certificate, StepCount>;
  /** Applied in declared order, before the proof-chunk prerequisite. */
  fieldCarriage?: readonly FamilyFieldCarriageRequirement<
    Category,
    Witness,
    Certificate,
    StepCount
  >[];
  /** Proof CBOR an action publishes as chunks, or null for none. */
  proofChunk?: (
    context: FamilyAssemblyContext<Category, Witness, Certificate, StepCount>,
    input: LinearFamilyPrerequisiteInput & { readonly headerHash: string },
  ) => string | null | Promise<string | null>;
  /** Extra members of the assembled workflow object. */
  extend?: (
    context: FamilyAssemblyContext<Category, Witness, Certificate, StepCount>,
    workflow: ManifestBoundFamilyWorkflow<Category, Certificate, StepCount>,
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
>(
  definition: Omit<
    FamilyDefinition<Category, Witness, Certificate, StepCount>,
    "definitionVersion"
  >,
): FamilyDefinition<Category, Witness, Certificate, StepCount> =>
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
