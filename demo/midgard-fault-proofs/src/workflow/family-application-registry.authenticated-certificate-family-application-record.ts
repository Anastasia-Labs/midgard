import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  createManifestBoundFieldItemWidthIllegalWorkflow,
  executeManifestBoundFieldItemWidthIllegalWorkflow,
  FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION,
  FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS,
  type ManifestBoundFieldItemWidthIllegalWorkflow,
  type ManifestBoundFieldItemWidthIllegalWorkflowConfig,
} from "../field-item-width-illegal/workflow.js";
import {
  createManifestBoundFieldPreimageLengthWorkflow,
  executeManifestBoundFieldPreimageLengthWorkflow,
  FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION,
  type ManifestBoundFieldPreimageLengthWorkflow,
  type ManifestBoundFieldPreimageLengthWorkflowConfig,
} from "../field-preimage-length-mismatch/authenticated-workflow.js";
import { FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS } from "../field-preimage-length-mismatch/config.js";
import {
  createManifestBoundOutputReferenceScriptDecodingWorkflow,
  executeManifestBoundOutputReferenceScriptDecodingWorkflow,
  type ManifestBoundOutputReferenceScriptDecodingWorkflow,
  type ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
  OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
  OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
} from "../output-reference-script-decoding/authenticated-workflow.js";
import {
  createManifestBoundProtectedOutputSignerMissingWorkflow,
  executeManifestBoundProtectedOutputSignerMissingWorkflow,
  type ManifestBoundProtectedOutputSignerMissingWorkflow,
  type ManifestBoundProtectedOutputSignerMissingWorkflowConfig,
  PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
  PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
} from "../protected-output-signer-missing/authenticated-workflow.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import {
  createManifestBoundTransactionOutputNonCanonicalWorkflow,
  executeManifestBoundTransactionOutputNonCanonicalWorkflow,
  type ManifestBoundTransactionOutputNonCanonicalWorkflow,
  type ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
  TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
  TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
} from "../transaction-output-non-canonical/workflow.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  createManifestBoundWitnessScriptDecodingWorkflow,
  executeManifestBoundWitnessScriptDecodingWorkflow,
  type ManifestBoundWitnessScriptDecodingWorkflow,
  type ManifestBoundWitnessScriptDecodingWorkflowConfig,
  WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
  WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS,
} from "../witness-script-decoding/workflow.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import {
  assertFamilyDefinitionRoster,
  defineFamilyApplication,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  familyDefinitionRoster,
  type FamilyResolvedReferenceScripts,
  type FamilyRosterDefinition,
} from "./family-application.js";
import {
  type AuthenticatedCertificateFamilyConfig,
  bindAuthenticatedCertificateFamilyConfig,
} from "./family-application-registry.decision-digest-cursor-family-application-record.js";
import { commonBoundConfigFields } from "./family-application-registry.linear-family-application-record.js";
import {
  type CursorFamilyRequirements,
  requiredInfrastructureFields,
  type ResolvedRosterParts,
  resolveRosterParts,
} from "./family-application-registry.resolve-roster-parts.js";
import {
  type FamilyCategory,
  familyStepContractNames,
} from "./family-definition.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import type { FraudProofL1Source } from "./l1-source.js";

/**
 * The config key a family on the authenticated certificate shape reads each
 * step from: the one entry of the family's manifest-contracts map that names
 * that step's contract. Most families spell the keys `step01`…; the
 * field-preimage-length family names its two second steps by direction, so
 * the key is looked up rather than assumed. The map must name exactly the
 * roster's contracts, or the family's config and its roster have drifted.
 */
const authenticatedCertificateStepConfigKeys = (
  category: FamilyCategory,
  roster: Readonly<Record<string, string>>,
  stepContractNames: readonly string[],
  contracts: Readonly<Record<string, string>>,
): readonly string[] => {
  const rosterContracts = new Set(Object.values(roster));
  const configContracts = new Set(Object.values(contracts));
  for (const contractName of rosterContracts) {
    if (!configContracts.has(contractName)) {
      throw new Error(
        `${category} manifest contracts omit ${contractName}, which its roster resolves`,
      );
    }
  }
  for (const contractName of configContracts) {
    if (!rosterContracts.has(contractName)) {
      throw new Error(
        `${category} manifest contracts name ${contractName}, which its roster does not resolve`,
      );
    }
  }
  return stepContractNames.map((contractName) => {
    const keys = Object.keys(contracts).filter(
      (key) => contracts[key] === contractName,
    );
    if (keys.length !== 1) {
      throw new Error(
        `${category} manifest contracts must name ${contractName} under exactly one key`,
      );
    }
    return keys[0]!;
  });
};

/**
 * Derives the record of a decision-digest cursor family on the authenticated
 * certificate shape. The roster is the definition's: chain steps, witness
 * roles and the certificate the definition binds. `bindConfig` lays those
 * same roles into the family's config under the keys its manifest-contracts
 * map declares, so a script cannot be in the roster and absent from the
 * config or the reverse.
 */
export const authenticatedCertificateFamilyApplicationRecord = <
  Category extends FamilyCategory,
  Config extends AuthenticatedCertificateFamilyConfig,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>(
  definition: FamilyRosterDefinition & Readonly<{ category: Category }>,
  family: Partial<CursorFamilyRequirements> &
    Readonly<{
      contracts: Readonly<Record<string, string>>;
      constructWorkflow: (config: Config) => Promise<Workflow>;
      execute: (input: {
        readonly workflow: Workflow;
        readonly sources: readonly RetainedDaPayloadSource[];
        readonly journal: FraudProofWorkflowJournalStore;
      }) => Promise<unknown>;
    }>,
): FamilyApplicationRecord<Category, Config, Workflow> => {
  const { category } = definition;
  const requires = family.requires ?? [];
  const infrastructureFields = requiredInfrastructureFields({ requires });
  if (!definition.fieldPreimageCertificate) {
    throw new Error(
      `${category} is on the authenticated certificate shape but its definition does not bind the certificate`,
    );
  }
  if (definition.auxiliaryReferenceScripts !== undefined) {
    throw new Error(
      `${category} declares auxiliary reference scripts, which the authenticated certificate shape has no place for`,
    );
  }
  const roster = familyDefinitionRoster(definition);
  assertFamilyDefinitionRoster(definition, roster);
  const stepConfigKeys = authenticatedCertificateStepConfigKeys(
    category,
    roster,
    familyStepContractNames(definition),
    family.contracts,
  );
  return defineFamilyApplication<Category, Config, Workflow>({
    category,
    roster,
    requires,
    bindConfig: ({ infrastructure, references }): Config =>
      // Every step key was found above and every role resolves or throws, so
      // this is the family's exact config shape.
      Object.freeze({
        ...bindAuthenticatedCertificateFamilyConfig({
          category,
          infrastructure,
          references,
          stepConfigKeys,
          witnessRoles: definition.witnessRoles,
        }),
        ...infrastructureFields(infrastructure),
      }) as Config,
    constructWorkflow: family.constructWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await family.execute({ workflow, sources, journal }),
    bindsDecisionDigest: true,
  });
};

/**
 * The six decision-digest families on the authenticated certificate shape
 * that derive from a definition. Each row names its exact config and workflow
 * types so the record's `bindConfig` result is what its `constructWorkflow`
 * consumes.
 */
export const FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "fieldItemWidthIllegal",
    ManifestBoundFieldItemWidthIllegalWorkflowConfig,
    ManifestBoundFieldItemWidthIllegalWorkflow
  >(FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION, {
    contracts: FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS,
    constructWorkflow: createManifestBoundFieldItemWidthIllegalWorkflow,
    execute: executeManifestBoundFieldItemWidthIllegalWorkflow,
  });

export const FIELD_PREIMAGE_LENGTH_MISMATCH_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "fieldPreimageLengthMismatch",
    ManifestBoundFieldPreimageLengthWorkflowConfig,
    ManifestBoundFieldPreimageLengthWorkflow
  >(FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION, {
    contracts: FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS,
    constructWorkflow: createManifestBoundFieldPreimageLengthWorkflow,
    execute: executeManifestBoundFieldPreimageLengthWorkflow,
  });

export const OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "outputReferenceScriptDecoding",
    ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
    ManifestBoundOutputReferenceScriptDecodingWorkflow
  >(OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION, {
    contracts: OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
    constructWorkflow: createManifestBoundOutputReferenceScriptDecodingWorkflow,
    execute: executeManifestBoundOutputReferenceScriptDecodingWorkflow,
  });

export const PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "protectedOutputSignerMissing",
    ManifestBoundProtectedOutputSignerMissingWorkflowConfig,
    ManifestBoundProtectedOutputSignerMissingWorkflow
  >(PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION, {
    contracts: PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
    constructWorkflow: createManifestBoundProtectedOutputSignerMissingWorkflow,
    execute: executeManifestBoundProtectedOutputSignerMissingWorkflow,
  });

export const TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "transactionOutputNonCanonical",
    ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
    ManifestBoundTransactionOutputNonCanonicalWorkflow
  >(TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION, {
    contracts: TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
    constructWorkflow: createManifestBoundTransactionOutputNonCanonicalWorkflow,
    execute: executeManifestBoundTransactionOutputNonCanonicalWorkflow,
  });

export const WITNESS_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "witnessScriptDecoding",
    ManifestBoundWitnessScriptDecodingWorkflowConfig,
    ManifestBoundWitnessScriptDecodingWorkflow
  >(WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION, {
    contracts: WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS,
    constructWorkflow: createManifestBoundWitnessScriptDecodingWorkflow,
    execute: executeManifestBoundWitnessScriptDecodingWorkflow,
  });

/**
 * Derives the record of a cursor family whose config lays the roster out in
 * its own shape. The roster is the definition's; `bindConfig` receives it
 * resolved into its parts, beside the common fields and the fields read from
 * the infrastructure the record requires, and lays them into the family's
 * config, so a script cannot be in the roster and absent from the config or
 * the reverse.
 */
export const cursorFamilyApplicationRecord = <
  Category extends FamilyCategory,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>(
  definition: FamilyRosterDefinition & Readonly<{ category: Category }>,
  family: CursorFamilyRequirements &
    Readonly<{
      bindsDecisionDigest: boolean;
      bindConfig: (input: {
        /** The common fields and the fields read from required infrastructure. */
        readonly common: ReturnType<typeof commonBoundConfigFields> &
          Readonly<Record<string, unknown>>;
        readonly roster: Readonly<Record<string, string>>;
        readonly references: FamilyResolvedReferenceScripts;
        readonly parts: ResolvedRosterParts;
      }) => Config;
      constructWorkflow: (config: Config) => Promise<Workflow>;
      execute: (input: {
        readonly workflow: Workflow;
        readonly sources: readonly RetainedDaPayloadSource[];
        readonly journal: FraudProofWorkflowJournalStore;
      }) => Promise<unknown>;
    }>,
): FamilyApplicationRecord<Category, Config, Workflow> => {
  const { category } = definition;
  const roster = familyDefinitionRoster(definition);
  assertFamilyDefinitionRoster(definition, roster);
  const infrastructureFields = requiredInfrastructureFields(family);
  return defineFamilyApplication<Category, Config, Workflow>({
    category,
    roster,
    requires: family.requires,
    bindConfig: ({ infrastructure, references }): Config =>
      family.bindConfig({
        common: Object.freeze({
          ...commonBoundConfigFields(infrastructure),
          ...infrastructureFields(infrastructure),
        }),
        roster,
        references,
        parts: resolveRosterParts(definition, references),
      }),
    constructWorkflow: family.constructWorkflow,
    execute: family.execute,
    bindsDecisionDigest: family.bindsDecisionDigest,
  });
};

/**
 * The config shape the cursor families without a decision digest share: the
 * common infrastructure, the replay context and the retained-DA sources when
 * required, and a reference-script bundle
 * of the chain steps, the witness roster, the certificate when the definition
 * binds it, and the definition's auxiliary scripts under the family's own key.
 * Each family's own config type narrows the step tuple, the witness roster and
 * the extra fields; the builder lays the roster in at this shape and asserts
 * the family's exact type, which the definition's step count and role set make
 * sound.
 */
export type BundleCursorFamilyConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  replayContext?: CompleteCanonicalReplayContext;
  referenceScripts: Readonly<{
    steps: readonly UTxO[];
    witnesses: Readonly<Record<string, UTxO>>;
    fieldPreimageCertificateMint?: UTxO;
  }> &
    Readonly<Record<string, unknown>>;
}> &
  Readonly<Record<string, unknown>>;
