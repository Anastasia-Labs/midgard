import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  createManifestBoundMintDeclaredAssetLimitWorkflow,
  executeManifestBoundMintDeclaredAssetLimitWorkflow,
  type ManifestBoundMintDeclaredAssetLimitWorkflow,
  type ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
  MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION,
} from "../mint-declared-asset-limit/v1.js";
import {
  createManifestBoundObserverOrderInvalidWorkflow,
  executeManifestBoundObserverOrderInvalidWorkflow,
  type ManifestBoundObserverOrderInvalidWorkflow,
  type ManifestBoundObserverOrderInvalidWorkflowConfig,
  OBSERVER_ORDER_INVALID_FAMILY_DEFINITION,
} from "../observer-order-invalid/v1.js";
import {
  createManifestBoundObserversForbiddenWorkflow,
  executeManifestBoundObserversForbiddenWorkflow,
  type ManifestBoundObserversForbiddenWorkflow,
  type ManifestBoundObserversForbiddenWorkflowConfig,
  OBSERVERS_FORBIDDEN_FAMILY_DEFINITION,
} from "../observers-forbidden-on-untagged-network/v1.js";
import {
  createManifestBoundRedeemerCanonicityWorkflow,
  executeManifestBoundRedeemerCanonicityWorkflow,
  type ManifestBoundRedeemerCanonicityWorkflow,
  type ManifestBoundRedeemerCanonicityWorkflowConfig,
  REDEEMER_CANONICITY_FAMILY_DEFINITION,
} from "../redeemer-canonicity/runtime.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import {
  createManifestBoundScriptIntegrityHashMissingWorkflow,
  executeManifestBoundScriptIntegrityHashMissingWorkflow,
  type ManifestBoundScriptIntegrityHashMissingWorkflow,
  type ManifestBoundScriptIntegrityHashMissingWorkflowConfig,
  SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION,
} from "../script-integrity-hash-missing/v1.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  assertFamilyDefinitionRoster,
  defineFamilyApplication,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FamilyCommonInfrastructure,
  familyDefinitionRoster,
  type FamilyResolvedReferenceScripts,
  type FamilyRosterDefinition,
  familyStepRole,
} from "./family-application.js";
import {
  commonBoundConfigFields,
  type DecisionDigestCursorFamilyConfig,
  requiredDecisionDigest,
} from "./family-application-registry.linear-family-application-record.js";
import {
  referenceScriptBundle,
  requiredReference,
  resolveRosterParts,
} from "./family-application-registry.resolve-roster-parts.js";
import { type FamilyCategory } from "./family-definition.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import type { FraudProofL1Source } from "./l1-source.js";

/**
 * Derives the record of a cursor family that binds the admitted decision
 * digest and follows its proof token with a state-queue removal. The roster
 * is the definition's: chain steps, witness roles, the certificate when the
 * definition binds it, and the removal set the definition declares as its
 * auxiliary reference scripts. `bindConfig` lays those same roles into the
 * family's config, so a script cannot be in the roster and absent from the
 * config or the reverse.
 */
const decisionDigestCursorFamilyApplicationRecord = <
  Category extends FamilyCategory,
  Config extends DecisionDigestCursorFamilyConfig,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>(
  definition: FamilyRosterDefinition & Readonly<{ category: Category }>,
  family: Readonly<{
    constructWorkflow: (config: Config) => Promise<Workflow>;
    execute: (input: {
      readonly workflow: Workflow;
      readonly sources: readonly RetainedDaPayloadSource[];
      readonly journal: FraudProofWorkflowJournalStore;
    }) => Promise<unknown>;
  }>,
): FamilyApplicationRecord<Category, Config, Workflow> => {
  const { category } = definition;
  const removalRoles = definition.auxiliaryReferenceScripts;
  if (removalRoles === undefined || Object.keys(removalRoles).length === 0) {
    throw new Error(
      `${category} follows its proof token with a state-queue removal but declares no auxiliary reference scripts`,
    );
  }
  const roster = familyDefinitionRoster(definition);
  assertFamilyDefinitionRoster(definition, roster);
  return defineFamilyApplication<Category, Config, Workflow>({
    category,
    roster,
    requires: [],
    bindConfig: ({ infrastructure, references }): Config => {
      const parts = resolveRosterParts(definition, references);
      const bound: DecisionDigestCursorFamilyConfig = Object.freeze({
        ...commonBoundConfigFields(infrastructure),
        decisionDigest: requiredDecisionDigest(category, infrastructure),
        referenceScripts: Object.freeze({
          ...referenceScriptBundle(parts),
          removal: parts.auxiliary,
        }),
      });
      // The step list has the definition's step count and every role resolved
      // or threw above, so this is the family's exact config shape.
      return bound as Config;
    },
    constructWorkflow: family.constructWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await family.execute({ workflow, sources, journal }),
    bindsDecisionDigest: true,
  });
};

/**
 * The decision-digest families on the state-queue-removal witness set.
 * Each row names its exact config and workflow types so the record's
 * `bindConfig` result is what its `constructWorkflow` consumes.
 */
export const MINT_DECLARED_ASSET_LIMIT_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "mintDeclaredAssetLimit",
    ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
    ManifestBoundMintDeclaredAssetLimitWorkflow
  >(MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundMintDeclaredAssetLimitWorkflow,
    execute: executeManifestBoundMintDeclaredAssetLimitWorkflow,
  });

export const OBSERVER_ORDER_INVALID_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "observerOrderInvalid",
    ManifestBoundObserverOrderInvalidWorkflowConfig,
    ManifestBoundObserverOrderInvalidWorkflow
  >(OBSERVER_ORDER_INVALID_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundObserverOrderInvalidWorkflow,
    execute: executeManifestBoundObserverOrderInvalidWorkflow,
  });

export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "observersForbiddenOnUntaggedNetwork",
    ManifestBoundObserversForbiddenWorkflowConfig,
    ManifestBoundObserversForbiddenWorkflow
  >(OBSERVERS_FORBIDDEN_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundObserversForbiddenWorkflow,
    execute: executeManifestBoundObserversForbiddenWorkflow,
  });

export const REDEEMER_CANONICITY_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "redeemerCanonicity",
    ManifestBoundRedeemerCanonicityWorkflowConfig,
    ManifestBoundRedeemerCanonicityWorkflow
  >(REDEEMER_CANONICITY_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundRedeemerCanonicityWorkflow,
    execute: executeManifestBoundRedeemerCanonicityWorkflow,
  });

export const SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "scriptIntegrityHashMissing",
    ManifestBoundScriptIntegrityHashMissingWorkflowConfig,
    ManifestBoundScriptIntegrityHashMissingWorkflow
  >(SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundScriptIntegrityHashMissingWorkflow,
    execute: executeManifestBoundScriptIntegrityHashMissingWorkflow,
  });

/**
 * The config shape the decision-digest families on the authenticated
 * certificate shape share: the common infrastructure, the admitted decision
 * digest, and a reference-script bundle that keys every chain step at the top
 * level beside the field-preimage certificate minting policy and the witness
 * roster. Each family's own config type narrows the step keys and the witness
 * roster; the builders below lay the roster in at this shape and assert the
 * family's exact type, which the definition's step count and role set make
 * sound.
 */
export type AuthenticatedCertificateFamilyConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  l1Source: FraudProofL1Source;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: Readonly<{
    fieldPreimageCertificateMint: UTxO;
    witnesses: Readonly<Record<string, UTxO>>;
  }> &
    Readonly<Record<string, unknown>>;
}>;

/**
 * Lays a resolved roster into the authenticated certificate shape: step `i`
 * under its family's config key, the certificate under its own role, and
 * every witness role inside `witnesses`.
 */
export const bindAuthenticatedCertificateFamilyConfig = ({
  category,
  infrastructure,
  references,
  stepConfigKeys,
  witnessRoles,
}: {
  readonly category: FamilyCategory;
  readonly infrastructure: FamilyCommonInfrastructure;
  readonly references: FamilyResolvedReferenceScripts;
  /** The family's config key for each step, in step order. */
  readonly stepConfigKeys: readonly string[];
  readonly witnessRoles: readonly string[];
}): AuthenticatedCertificateFamilyConfig =>
  Object.freeze({
    ...commonBoundConfigFields(infrastructure),
    decisionDigest: requiredDecisionDigest(category, infrastructure),
    referenceScripts: Object.freeze({
      ...Object.fromEntries(
        stepConfigKeys.map((key, index) => [
          key,
          requiredReference(references, familyStepRole(index + 1)),
        ]),
      ),
      fieldPreimageCertificateMint: requiredReference(
        references,
        "fieldPreimageCertificateMint",
      ),
      witnesses: Object.freeze(
        Object.fromEntries(
          witnessRoles.map((role) => [
            role,
            requiredReference(references, role),
          ]),
        ),
      ),
    }),
  });
