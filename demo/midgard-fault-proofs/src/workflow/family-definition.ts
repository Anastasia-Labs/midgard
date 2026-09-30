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

import "./linear-family-spec.js";
import "./family-definition.family-adapter-arm.js";
import "./family-definition.family-definition.js";
export {
  type FamilyAdapterArm,
  type FamilyAssemblyContext,
  type FamilyCategory,
  type FamilyDeploymentContext,
  type FamilyFieldCarriageRequirement,
  type FamilyReferenceScripts,
  type FamilyRuntimeArguments,
  type FamilyStepTuple,
  type FamilyTransactionPort,
  type FaultProofWitnessRole,
  type FieldPreimageCertificateBinding,
  LINEAR_FAMILY_DEFINITION_VERSION,
  type LinearFamilyAssemblyContext,
  type LinearFamilyFieldCarriageRequirement,
  type LinearFamilyPrerequisiteInput,
  type LinearFamilyReferenceScripts,
  type LinearFamilyStepDatumSchema,
  type LinearFamilyStepDatumSchemas,
  type LinearFamilyStepReferenceScripts,
  type ManifestBoundFamilyWorkflowConfig,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.family-adapter-arm.js";
export {
  defineFamily,
  defineLinearFamily,
  type FamilyDefinition,
  familyStepContractNames,
  type LinearFamilyAdapterArm,
  type LinearFamilyDefinition,
  type LinearFamilyDefinitionOf,
  type ManifestBoundFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflow,
} from "./family-definition.family-definition.js";
