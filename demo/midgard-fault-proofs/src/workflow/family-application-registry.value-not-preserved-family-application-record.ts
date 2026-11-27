import "./family-application-registry.bundle-cursor-family-application-record.js";

import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import {
  createManifestBoundNetworkIdWorkflow,
  runOrResumeManifestBoundNetworkIdWorkflow,
} from "../network-id/workflow-adapter.create-manifest-bound-network-id-workflow.js";
import type {
  ManifestBoundNetworkIdWorkflow,
  ManifestBoundNetworkIdWorkflowConfig,
} from "../network-id/workflow-adapter.seal-manifest-bound-network-id-runtime.js";
import { REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES } from "../remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY } from "../runtime.js";
import {
  CONSERVATION_POSITIONS,
  conservationManifestName,
} from "../value-not-preserved/contracts.js";
import { createManifestBoundValueConservationWorkflow } from "../value-not-preserved/workflow.create-manifest-bound-value-conservation-workflow.js";
import {
  type ManifestBoundValueConservationWorkflow,
  runOrResumeManifestBoundValueConservationWorkflow,
} from "../value-not-preserved/workflow.run-or-resume-manifest-bound-value-conservation-workflow.js";
import type { ManifestBoundValueConservationWorkflowConfig } from "../value-not-preserved/workflow.value-conservation-references.js";
import { STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS } from "./cursor-family-runtime.js";
import {
  defineFamilyApplication,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
} from "./family-application.assert-family-definition-roster.js";
import { commonBoundConfigFields } from "./family-application-registry.linear-family-application-record.js";
import {
  requiredReference,
  resolvedRoles,
  resolvedSteps,
  stepRoster,
  suppliedReplayContext,
} from "./family-application-registry.resolve-roster-parts.js";
import {
  CORE_WITNESS_ROLES,
  MEMBERSHIP_WITNESS_ROLES,
} from "./family-definition.family-adapter-arm.js";
import type {
  ManifestBoundMissingSignatureWorkflow,
  ManifestBoundMissingSignatureWorkflowConfig,
} from "./missing-signature.create-bound-transaction-port.js";
import {
  createManifestBoundMissingSignatureWorkflow,
  runOrResumeManifestBoundMissingSignatureWorkflow,
} from "./missing-signature.create-manifest-bound-missing-signature-workflow.js";
import {
  withMissingSignatureOwnThreadReclaim,
  withValueConservationOwnThreadReclaim,
} from "./own-thread-reclaim.special-families.js";

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
      ...stepRoster(deploymentFamilyStepNames("missingSignature")),
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
    constructWorkflow: async (config) =>
      withMissingSignatureOwnThreadReclaim(
        await createManifestBoundMissingSignatureWorkflow(config),
        config,
      ),
    execute: async ({ workflow, sources, run }) =>
      await runOrResumeManifestBoundMissingSignatureWorkflow({
        workflow,
        sources,
        run,
      }),
    bindsDecisionDigest: false,
  });

/**
 * Network-id has no `FamilyDefinition` and lays its reference scripts at the
 * top of its config rather than in a bundle: the two-step accepted chain, the
 * forced door and the resumable forced outputs scan (optional in the config,
 * always resolved here), the certificate, the field grammar certificate's
 * reference script, and the five membership witnesses.
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
    ...stepRoster(deploymentFamilyStepNames("networkId")),
    forcedStep: "fraudProofNetworkIdForcedStep",
    forcedScan: NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
    ...Object.fromEntries(MEMBERSHIP_WITNESS_ROLES.map((role) => [role, role])),
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
    fieldGrammarCertificateSpend: "fieldGrammarCertificateSpend",
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
      fieldGrammarCertificateReferenceScript: requiredReference(
        references,
        "fieldGrammarCertificateSpend",
      ),
      witnessReferenceScripts: resolvedRoles(
        references,
        MEMBERSHIP_WITNESS_ROLES,
      ),
      removal: Object.freeze({ stateQueueMutationLeaseCoordinator }),
    });
  },
  constructWorkflow: createManifestBoundNetworkIdWorkflow,
  execute: async ({ workflow, sources, run }) =>
    await runOrResumeManifestBoundNetworkIdWorkflow({
      workflow,
      sources,
      run,
    }),
  bindsDecisionDigest: false,
});

/**
 * Value-conservation has no `FamilyDefinition`: beside its four-step chain
 * it publishes one union contract per conservation position, read from
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
      ...stepRoster(deploymentFamilyStepNames("valueNotPreserved")),
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
    constructWorkflow: async (config) =>
      withValueConservationOwnThreadReclaim(
        await createManifestBoundValueConservationWorkflow(config),
        config,
      ),
    execute: async ({ workflow, sources, run }) =>
      await runOrResumeManifestBoundValueConservationWorkflow({
        workflow,
        sources,
        run,
      }),
    bindsDecisionDigest: false,
  });

/**
 * A record as the registry states it for every family at once. A family's own
 * config and workflow types are tied together at its definition site, where
 * `bindConfig`'s result is what `constructWorkflow` consumes; the registry
 * erases them so that one mapped type describes every installed row. A
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
