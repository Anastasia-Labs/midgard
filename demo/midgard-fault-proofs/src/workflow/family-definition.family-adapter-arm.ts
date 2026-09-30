import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import type {
  createCursorFamilyWorkflowAdapter,
  CursorFamilyTransactionPort,
} from "./cursor-family-adapter.js";
import type { CursorFamilySpec } from "./cursor-family-state.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import type { FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type {
  FieldCarriagePrerequisitePort,
  PreimageCarriageRequirement,
} from "./field-carriage-prerequisite.js";
import type { JournalJsonObject } from "./journal.js";
import type { LinearFamilyTransactionPort } from "./linear-family-adapter.js";
import {
  type LinearFamilyCategory,
  type LinearFamilySpecOf,
} from "./linear-family-spec.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { FraudProofRawL1FamilyDefinition } from "./raw-l1-family-derivation.js";
import type { FraudProofRawL1SnapshotAuthority } from "./raw-l1-snapshot.js";

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
export type FaultProofWitnessRole =
  | keyof FaultProofWitnessReferenceScripts
  | "stateQueueSpend";

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
  /** Family-specific published scripts, keyed by the definition's role names. */
  auxiliaryReferenceScripts?: Readonly<Record<string, UTxO>>;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundLinearFamilyWorkflowConfig<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = Omit<
  ManifestBoundFamilyWorkflowConfig<Category, Witness, Certificate>,
  "auxiliaryReferenceScripts"
>;

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
  Runtime extends object = Readonly<Record<never, never>>,
> = Readonly<{
  /** Family-specific invocation inputs, supplied explicitly by its constructor/runner. */
  runtime: Runtime;
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  references: FamilyReferenceScripts<Category, Witness, Certificate, StepCount>;
  /** Exact manifest-bound scripts declared in addition to steps and witnesses. */
  auxiliaryReferences: Readonly<Record<string, UTxO>>;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  /** Shared handles used by both the transaction port and adapter decorators. */
  fieldCarriagePrerequisites: readonly FieldCarriagePrerequisitePort<Category>[];
  l1: FraudProofFamilyL1ObservationPort<Category> & {
    readonly rawL1: FraudProofRawL1SnapshotAuthority;
  };
  certificate: Certificate extends true
    ? FieldPreimageCertificateBinding
    : null;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/** Authenticated deployment inputs shared by independently assembled runs. */
export type FamilyDeploymentContext<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
> = Omit<
  FamilyAssemblyContext<Category, Witness, Certificate, StepCount>,
  "runtime" | "fieldCarriagePrerequisites"
>;

/** Runtime inputs are mandatory only for definitions that declare them. */
export type FamilyRuntimeArguments<Runtime extends object> =
  keyof Runtime extends never
    ? readonly [runtime?: Runtime]
    : readonly [runtime: Runtime];

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
  Runtime extends object = Readonly<Record<never, never>>,
> = Readonly<{
  /** Raw-datum carriage instead of a compact-field opening. */
  rawDatum?: boolean;
  requirementForAction: (
    context: FamilyAssemblyContext<
      Category,
      Witness,
      Certificate,
      StepCount,
      Runtime
    >,
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
  Category extends LinearFamilyCategory
    ? LinearFamilyTransactionPort<Category>
    : CursorFamilyTransactionPort<Category>;

export type FamilyAdapterArm<
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
> =
  | Readonly<{
      /** Steps come from the category's linear spec. */
      kind: "linear";
      transactionPort: (
        context: FamilyAssemblyContext<
          Category,
          Witness,
          Certificate,
          StepCount,
          Runtime
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
      /** Bind a refiner to this workflow's authenticated runtime context. */
      createRefineAction?: (
        context: FamilyAssemblyContext<
          Category,
          Witness,
          Certificate,
          StepCount,
          Runtime
        >,
      ) => NonNullable<
        Parameters<
          typeof createCursorFamilyWorkflowAdapter<Category>
        >[0]["refineAction"]
      >;
      transactionPort: (
        context: FamilyAssemblyContext<
          Category,
          Witness,
          Certificate,
          StepCount,
          Runtime
        >,
      ) => CursorFamilyTransactionPort<Category>;
    }>;
