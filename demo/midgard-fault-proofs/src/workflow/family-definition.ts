/**
 * Family definition: the frozen, per-family data that the manifest-bound
 * family assembly (`manifest-bound-family-assembly.ts`) turns into a running
 * workflow. It is separate from `LinearFamilySpec`, which stays pure data for
 * the spec-driven state machine; the two are keyed by category.
 *
 * A definition carries what differs between families and nothing else: the
 * step datum schemas, the witness reference-script roles, whether the
 * field-preimage certificate is required, the complete replayer, the
 * transaction port (the `linear` arm) and the optional prerequisites. The
 * assembly owns everything the families used to restate: deployment binding,
 * signer assertion, certificate requirement, reference-script resolution over
 * the spec's step names, L1 observation port, adapter, prerequisite
 * decoration order, terminal verifier and release-finality authority.
 */
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
import type {
  LinearFamilyCategory,
  LinearFamilySpecOf,
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

export const LINEAR_FAMILY_DEFINITION_VERSION =
  "midgard-production-linear-family-definition-v1" as const;

/** The Lucid datum schema one computation-thread step's datum decodes with. */
export type LinearFamilyStepDatumSchema = NonNullable<
  FraudProofRawL1FamilyDefinition["computationThread"]["steps"][number]["datumSchema"]
>;

/** A shared witness script a family's transactions execute by reference. */
export type FaultProofWitnessRole = keyof FaultProofWitnessReferenceScripts;

type PerStep<Steps extends readonly unknown[], Value> = {
  readonly [Index in keyof Steps]: Value;
};

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
export type LinearFamilyReferenceScripts<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = Readonly<
  {
    steps: LinearFamilyStepReferenceScripts<Category>;
    witnesses: FaultProofWitnessReferenceScripts & {
      readonly [Role in Witness]: UTxO;
    };
  } & (Certificate extends true
    ? { fieldPreimageCertificateMint: UTxO }
    : unknown)
>;

export type ManifestBoundLinearFamilyWorkflowConfig<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: LinearFamilyReferenceScripts<
    Category,
    Witness,
    Certificate
  >;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type FieldPreimageCertificateBinding = Readonly<{
  policyId: string;
  mintingScript: Script;
}>;

/**
 * Everything the assembly has bound before it asks the family for its
 * transaction port, replayer and prerequisites. The L1 port is guaranteed to
 * carry the raw-L1 and publication authorities.
 */
export type LinearFamilyAssemblyContext<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  references: LinearFamilyReferenceScripts<Category, Witness, Certificate>;
  l1: FraudProofFamilyL1ObservationPort<Category> & {
    readonly rawL1: FraudProofRawL1SnapshotAuthority;
  };
  certificate: Certificate extends true
    ? FieldPreimageCertificateBinding
    : null;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type LinearFamilyPrerequisiteInput = Readonly<{
  action: FraudProofWorkflowAction;
  artifact: JournalJsonObject;
}>;

/**
 * One authenticated field-carriage prerequisite. A family lists as many as it
 * has field plans; the assembly applies them in declared order, all before
 * the proof-chunk prerequisite.
 */
export type LinearFamilyFieldCarriageRequirement<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> = Readonly<{
  /** Raw-datum carriage instead of a compact-field opening. */
  rawDatum?: boolean;
  requirementForAction: (
    context: LinearFamilyAssemblyContext<Category, Witness, Certificate>,
    input: LinearFamilyPrerequisiteInput,
  ) =>
    | PreimageCarriageRequirement
    | null
    | Promise<PreimageCarriageRequirement | null>;
}>;

export type LinearFamilyAdapterArm<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
> =
  | Readonly<{
      kind: "linear";
      transactionPort: (
        context: LinearFamilyAssemblyContext<Category, Witness, Certificate>,
      ) => LinearFamilyTransactionPort<Category>;
    }>
  | Readonly<{
      /**
       * Cursor families share the assembly's head and tail but drive a
       * cursor-family adapter. Typed here so the definition is sized for
       * them; the assembly implements this arm with the cursor probe.
       */
      kind: "cursor";
      spec: CursorFamilySpec<Category>;
      refineAction?: NonNullable<
        Parameters<
          typeof createCursorFamilyWorkflowAdapter<Category>
        >[0]["refineAction"]
      >;
      transactionPort: (
        context: LinearFamilyAssemblyContext<Category, Witness, Certificate>,
      ) => CursorFamilyTransactionPort<Category>;
    }>;

/**
 * An assembled workflow. It is not parameterised by witness role: the stored
 * definition is widened to every role so a workflow assembled from a narrow
 * definition is assignable wherever the category's workflow is expected.
 */
export type ManifestBoundLinearFamilyWorkflow<
  Category extends LinearFamilyCategory,
  Certificate extends boolean = boolean,
> = Readonly<{
  definition: LinearFamilyDefinition<
    Category,
    FaultProofWitnessRole,
    Certificate
  >;
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  l1: FraudProofFamilyL1ObservationPort<Category>;
  transactions: LinearFamilyTransactionPort<Category>;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  replayer: CompleteCanonicalReplay;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export type LinearFamilyDefinition<
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole = FaultProofWitnessRole,
  Certificate extends boolean = boolean,
> = Readonly<{
  definitionVersion: typeof LINEAR_FAMILY_DEFINITION_VERSION;
  category: Category;
  /** Per-step thread datum schemas the deployment binding decodes with. */
  stepDatumSchemas: LinearFamilyStepDatumSchemas<Category>;
  /** Witness scripts whose published references this family binds. */
  witnessRoles: readonly Witness[];
  /** Whether the manifest must publish the field-preimage certificate policy. */
  fieldPreimageCertificate: Certificate;
  /** The exact closed replay bundle `runOrResume` launches with. */
  replayer: (
    context: LinearFamilyAssemblyContext<Category, Witness, Certificate>,
  ) => CompleteCanonicalReplay;
  adapter: LinearFamilyAdapterArm<Category, Witness, Certificate>;
  /** Applied in declared order, before the proof-chunk prerequisite. */
  fieldCarriage?: readonly LinearFamilyFieldCarriageRequirement<
    Category,
    Witness,
    Certificate
  >[];
  /** Proof CBOR an action publishes as chunks, or null for none. */
  proofChunk?: (
    context: LinearFamilyAssemblyContext<Category, Witness, Certificate>,
    input: LinearFamilyPrerequisiteInput & { readonly headerHash: string },
  ) => string | null | Promise<string | null>;
  /** Extra members of the assembled workflow object. */
  extend?: (
    context: LinearFamilyAssemblyContext<Category, Witness, Certificate>,
    workflow: ManifestBoundLinearFamilyWorkflow<Category, Certificate>,
  ) => Readonly<Record<string, unknown>>;
}>;

/**
 * The definition shape a heterogeneous table row admits: any witness-role
 * set, and either certificate polarity. Kept as a union over the polarity so
 * the context's `certificate` member stays exact in each arm.
 */
export type LinearFamilyDefinitionOf<Category extends LinearFamilyCategory> =
  | LinearFamilyDefinition<Category, FaultProofWitnessRole, true>
  | LinearFamilyDefinition<Category, FaultProofWitnessRole, false>;

/** Freezes a definition and infers its exact category, roles and polarity. */
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
  Object.freeze({
    definitionVersion: LINEAR_FAMILY_DEFINITION_VERSION,
    ...definition,
  });
