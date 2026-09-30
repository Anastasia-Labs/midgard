import "./family-application-registry.bundle-cursor-family-application-record.js";

import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  createManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflowConfig,
  runOrResumeManifestBoundNetworkIdWorkflow,
} from "../network-id/workflow-adapter.js";
import { REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES } from "../remove-fraudulent-block.js";
import { NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY } from "../runtime.js";
import {
  VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES,
} from "../validation-dispute/workflow-family.js";
import {
  createManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflowConfig,
  runOrResumeManifestBoundValidationTraceDisputeWorkflow,
} from "../validation-dispute/workflow-v1.js";
import {
  CONSERVATION_POSITIONS,
  conservationManifestName,
} from "../value-not-preserved/contracts.js";
import {
  createManifestBoundValueConservationWorkflow,
  type ManifestBoundValueConservationWorkflow,
  type ManifestBoundValueConservationWorkflowConfig,
  runOrResumeManifestBoundValueConservationWorkflow,
} from "../value-not-preserved/workflow.js";
import type { ValidationTraceChallenge } from "./challenge-authority.js";
import { STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS } from "./cursor-family-runtime.js";
import {
  defineFamilyApplication,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FamilyCommonInfrastructure,
} from "./family-application.js";
import {
  commonBoundConfigFields,
  requiredDecisionDigest,
} from "./family-application-registry.linear-family-application-record.js";
import {
  requiredReference,
  resolvedRoles,
  resolvedSteps,
  rolesOf,
  stepRoster,
  suppliedReplayContext,
} from "./family-application-registry.resolve-roster-parts.js";
import { type FamilyCategory } from "./family-definition.js";
import {
  createManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflowConfig,
  runOrResumeManifestBoundMissingSignatureWorkflow,
} from "./missing-signature.js";

/**
 * The witness roles the four hand-written records below spell out, in the
 * order the shared `FaultProofWitnessReferenceScripts` type declares them.
 */
const CORE_WITNESS_ROLES = Object.freeze([
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const);

const MEMBERSHIP_WITNESS_ROLES = Object.freeze([
  ...CORE_WITNESS_ROLES,
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const);

const MISSING_SIGNATURE_STEP_CONTRACT_NAMES = Object.freeze([
  "fraudProofMissingSignature",
  "fraudProofMissingSignatureStep02",
  "fraudProofMissingSignatureStep03",
  "fraudProofMissingSignatureStep04",
] as const);

/**
 * Missing-signature has no `FamilyDefinition`: its four-step accepted chain
 * is joined by the forced (wrongful-rejection) door, signer and witness
 * scripts, which its config makes optional so a deployment without the
 * forced direction still binds. The roster resolves all three always — a
 * role is never optional here — so the forced direction is installed
 * whenever the family is.
 */
export const MISSING_SIGNATURE_FAMILY_APPLICATION_RECORD =
  defineFamilyApplication<
    "missingSignature",
    ManifestBoundMissingSignatureWorkflowConfig,
    ManifestBoundMissingSignatureWorkflow
  >({
    category: "missingSignature",
    roster: Object.freeze({
      ...stepRoster(MISSING_SIGNATURE_STEP_CONTRACT_NAMES),
      forcedStep: "fraudProofMissingSignatureForcedStep",
      forcedSigner: "fraudProofMissingSignatureForcedSigner",
      forcedWitness: "fraudProofMissingSignatureForcedWitness",
      ...Object.fromEntries(CORE_WITNESS_ROLES.map((role) => [role, role])),
      fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
    }),
    requires: [],
    bindConfig: ({ infrastructure, references }) =>
      Object.freeze({
        ...commonBoundConfigFields(infrastructure),
        referenceScripts: Object.freeze({
          steps: resolvedSteps(references, 4),
          forced: Object.freeze({
            bind: requiredReference(references, "forcedStep"),
            signer: requiredReference(references, "forcedSigner"),
            witness: requiredReference(references, "forcedWitness"),
          }),
          witnesses: resolvedRoles(references, CORE_WITNESS_ROLES),
          fieldPreimageCertificateMint: requiredReference(
            references,
            "fieldPreimageCertificateMint",
          ),
        }),
      }),
    constructWorkflow: createManifestBoundMissingSignatureWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await runOrResumeManifestBoundMissingSignatureWorkflow({
        workflow,
        sources,
        journal,
      }),
    bindsDecisionDigest: false,
  });

/**
 * Network-id has no `FamilyDefinition` and lays its reference scripts at the
 * top of its config rather than in a bundle: the two-step accepted chain, the
 * forced door and the resumable forced outputs scan (optional in the config,
 * always resolved here), the certificate, and the five membership witnesses.
 * Its state-queue removal spends the removal set through the deployment
 * binding, so the roster carries no removal roles; the lease coordinator is
 * the one removal setting the config takes.
 */
export const NETWORK_ID_FAMILY_APPLICATION_RECORD = defineFamilyApplication<
  "networkId",
  ManifestBoundNetworkIdWorkflowConfig,
  ManifestBoundNetworkIdWorkflow
>({
  category: "networkId",
  roster: Object.freeze({
    step01: "fraudProofNetworkId",
    step02: "fraudProofNetworkIdStep02",
    forcedStep: "fraudProofNetworkIdForcedStep",
    forcedScan: NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
    ...Object.fromEntries(MEMBERSHIP_WITNESS_ROLES.map((role) => [role, role])),
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  }),
  requires: [],
  bindConfig: ({ infrastructure, references }) => {
    const { stateQueueMutationLeaseCoordinator, ...common } =
      commonBoundConfigFields(infrastructure);
    return Object.freeze({
      ...common,
      stepReferenceScripts: resolvedSteps(references, 2),
      forcedStepReferenceScript: requiredReference(references, "forcedStep"),
      forcedScanReferenceScript: requiredReference(references, "forcedScan"),
      fieldPreimageCertificateReferenceScript: requiredReference(
        references,
        "fieldPreimageCertificateMint",
      ),
      witnessReferenceScripts: resolvedRoles(
        references,
        MEMBERSHIP_WITNESS_ROLES,
      ),
      removal: Object.freeze({ stateQueueMutationLeaseCoordinator }),
    });
  },
  constructWorkflow: createManifestBoundNetworkIdWorkflow,
  execute: async ({ workflow, sources, journal }) =>
    await runOrResumeManifestBoundNetworkIdWorkflow({
      workflow,
      sources,
      journal,
    }),
  bindsDecisionDigest: false,
});

const VALUE_NOT_PRESERVED_STEP_CONTRACT_NAMES = Object.freeze([
  "fraudProofValueNotPreserved",
  "fraudProofValueNotPreservedStep02",
  "fraudProofValueNotPreservedStep03",
  "fraudProofValueNotPreservedStep04",
] as const);

/**
 * Value-conservation has no `FamilyDefinition`: beside its four-step legacy
 * chain it publishes one union contract per conservation position, read from
 * the family's own position table so the roster cannot drift from the
 * contracts the union plan walks. It replays the predecessor when the
 * classifier admitted one, so it binds the replay context the host supplies
 * without requiring it, and follows its proof token with the state-queue
 * removal set.
 */
export const VALUE_NOT_PRESERVED_FAMILY_APPLICATION_RECORD =
  defineFamilyApplication<
    "valueNotPreserved",
    ManifestBoundValueConservationWorkflowConfig,
    ManifestBoundValueConservationWorkflow
  >({
    category: "valueNotPreserved",
    roster: Object.freeze({
      ...stepRoster(VALUE_NOT_PRESERVED_STEP_CONTRACT_NAMES),
      ...Object.fromEntries(
        CONSERVATION_POSITIONS.map((position) => [
          position,
          conservationManifestName(position),
        ]),
      ),
      ...Object.fromEntries(
        MEMBERSHIP_WITNESS_ROLES.map((role) => [role, role]),
      ),
      fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      ...STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
    }),
    requires: [],
    bindConfig: ({ infrastructure, references }) =>
      Object.freeze({
        ...commonBoundConfigFields(infrastructure),
        ...suppliedReplayContext(infrastructure),
        referenceScripts: Object.freeze({
          steps: resolvedSteps(references, 4),
          union: resolvedRoles(references, CONSERVATION_POSITIONS),
          witnesses: resolvedRoles(references, MEMBERSHIP_WITNESS_ROLES),
          fieldPreimageCertificateMint: requiredReference(
            references,
            "fieldPreimageCertificateMint",
          ),
          removal: resolvedRoles(
            references,
            REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES,
          ),
        }),
      }),
    constructWorkflow: createManifestBoundValueConservationWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await runOrResumeManifestBoundValueConservationWorkflow({
        workflow,
        sources,
        journal,
      }),
    bindsDecisionDigest: false,
  });

/**
 * The freshly admitted validation-trace challenge for this decision, obtained
 * from the host's port, or nothing when the invocation carries no port. The
 * shared loop refuses a missing port before `bindConfig` runs unless the
 * invocation is reconciliation-only, which never reaches a dispute move; the
 * family's own execution fail-closes challenge-free.
 */
const optionalValidationChallenge = async (
  category: FamilyCategory,
  infrastructure: FamilyCommonInfrastructure,
): Promise<Readonly<{ challenge?: ValidationTraceChallenge }>> => {
  if (infrastructure.validationChallenge === undefined) return {};
  return {
    challenge: await infrastructure.validationChallenge.currentChallenge({
      headerHash: infrastructure.headerHash,
      decisionDigest: requiredDecisionDigest(category, infrastructure),
    }),
  };
};

/**
 * The sole interactive family, and the one record that requires the host's
 * validation-challenge port. Its roster is the family's own three contract
 * tables: the six dispute control scripts, the three witnesses the dispute
 * submitters attach by reference, and the state-queue removal set.
 */
export const VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD =
  defineFamilyApplication<
    "validationTraceDispute",
    ManifestBoundValidationTraceDisputeWorkflowConfig,
    ManifestBoundValidationTraceDisputeWorkflow
  >({
    category: "validationTraceDispute",
    roster: Object.freeze({
      ...VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES,
      ...VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES,
      ...VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
    }),
    requires: ["validationChallenge"],
    bindConfig: async ({ infrastructure, references }) =>
      Object.freeze({
        ...commonBoundConfigFields(infrastructure),
        decisionDigest: requiredDecisionDigest(
          "validationTraceDispute",
          infrastructure,
        ),
        ...(await optionalValidationChallenge(
          "validationTraceDispute",
          infrastructure,
        )),
        referenceScripts: Object.freeze({
          control: resolvedRoles(
            references,
            rolesOf(VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES),
          ),
          witnesses: resolvedRoles(
            references,
            rolesOf(VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES),
          ),
          removal: resolvedRoles(
            references,
            rolesOf(VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES),
          ),
        }),
      }),
    constructWorkflow: createManifestBoundValidationTraceDisputeWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await runOrResumeManifestBoundValidationTraceDisputeWorkflow({
        workflow,
        sources,
        journal,
      }),
    bindsDecisionDigest: true,
  });

/**
 * A record as the registry states it for every family at once. A family's own
 * config and workflow types are tied together at its definition site, where
 * `bindConfig`'s result is what `constructWorkflow` consumes; the registry
 * erases them so that one mapped type describes all fifty-five rows. A
 * consumer that runs a record imports it by name and keeps its full type.
 */
export type FamilyApplicationRegistryEntry<
  Category extends FraudProofCatalogueCategoryName,
> = Omit<
  FamilyApplicationRecord<
    Category,
    unknown,
    FamilyApplicationWorkflowIdentity<Category>
  >,
  "constructWorkflow" | "execute"
> &
  Readonly<{
    constructWorkflow: (
      config: never,
    ) => Promise<FamilyApplicationWorkflowIdentity<Category>>;
    execute: (input: never) => Promise<unknown>;
  }>;
