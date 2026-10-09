import {
  createManifestBoundCrossBlockDuplicateEventWorkflow,
  CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_DEFINITION,
  type ManifestBoundCrossBlockDuplicateEventWorkflow,
  type ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
  runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow,
} from "../cross-block-duplicate-event/workflow.js";
import {
  createManifestBoundExecutionNativeScriptInvalidWorkflow,
  EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
  type ManifestBoundExecutionNativeScriptInvalidWorkflow,
  type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
  runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow,
} from "../execution-native-script-invalid/v1.js";
import {
  createManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflowConfig,
  MIN_ADA_FAMILY_DEFINITION,
  runOrResumeManifestBoundMinAdaWorkflow,
} from "../min-ada/workflow.js";
import {
  createManifestBoundMintAuthorizationWorkflow,
  type ManifestBoundMintAuthorizationWorkflow,
  type ManifestBoundMintAuthorizationWorkflowConfig,
  MINT_AUTHORIZATION_FAMILY_DEFINITION,
  runOrResumeManifestBoundMintAuthorizationWorkflow,
} from "../mint-authorization/workflow.js";
import {
  createManifestBoundMintItemNonCanonicalWorkflow,
  executeManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflowConfig,
  MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS,
} from "../mint-item-non-canonical/workflow.js";
import {
  createManifestBoundNativeScriptDecodingWorkflow,
  type ManifestBoundNativeScriptDecodingWorkflow,
  type ManifestBoundNativeScriptDecodingWorkflowConfig,
  NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION,
  runOrResumeManifestBoundNativeScriptDecodingWorkflow,
} from "../native-script-decoding/workflow.js";
import {
  createManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflowConfig,
  NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
  runOrResumeManifestBoundNativeScriptInvalidWorkflow,
} from "../native-script-invalid/workflow.js";
import {
  createManifestBoundResolvedOutputNonCanonicalWorkflow,
  executeManifestBoundResolvedOutputNonCanonicalWorkflow,
  type ManifestBoundResolvedOutputNonCanonicalWorkflow,
  type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
  RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
  RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
} from "../resolved-output-non-canonical/authenticated-workflow.js";
import {
  createManifestBoundSpendInputSignerMissingWorkflow,
  executeManifestBoundSpendInputSignerMissingWorkflow,
  type ManifestBoundSpendInputSignerMissingWorkflow,
  type ManifestBoundSpendInputSignerMissingWorkflowConfig,
  SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
  SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
} from "../spend-input-signer-missing/authenticated-workflow.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  createManifestBoundTransitionTraceWorkflow,
  type ManifestBoundTransitionTraceWorkflow,
  type ManifestBoundTransitionTraceWorkflowConfig,
  runOrResumeManifestBoundTransitionTraceWorkflow,
  TRANSITION_TRACE_FAMILY_DEFINITION,
} from "../transition-trace/workflow.js";
import {
  createManifestBoundWithdrawalMistagWorkflow,
  type ManifestBoundWithdrawalMistagWorkflow,
  type ManifestBoundWithdrawalMistagWorkflowConfig,
  runOrResumeManifestBoundWithdrawalMistagWorkflow,
  WITHDRAWAL_MISTAG_FAMILY_DEFINITION,
} from "../withdrawal-mistag/workflow.js";
import {
  defineFamilyApplication,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FamilyRosterDefinition,
} from "./family-application.js";
import {
  authenticatedCertificateFamilyApplicationRecord,
  type BundleCursorFamilyConfig,
  cursorFamilyApplicationRecord,
} from "./family-application-registry.authenticated-certificate-family-application-record.js";
import { bindAuthenticatedCertificateFamilyConfig } from "./family-application-registry.decision-digest-cursor-family-application-record.js";
import {
  type CursorFamilyRequirements,
  referenceScriptBundle,
  requiredReference,
} from "./family-application-registry.resolve-roster-parts.js";
import { type FamilyCategory } from "./family-definition.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";

/**
 * Derives the record of a cursor family without a decision digest whose
 * config reads its reference scripts from the shared bundle. The roster is
 * the definition's; the record declares the infrastructure it requires and
 * the fields it reads from it.
 */
const bundleCursorFamilyApplicationRecord = <
  Category extends FamilyCategory,
  Config extends BundleCursorFamilyConfig,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>(
  definition: FamilyRosterDefinition & Readonly<{ category: Category }>,
  family: CursorFamilyRequirements &
    Readonly<{
      /**
       * The bundle key the definition's auxiliary reference scripts are laid
       * under. Present exactly when the definition declares any.
       */
      auxiliaryReferenceScriptsKey?: string;
      constructWorkflow: (config: Config) => Promise<Workflow>;
      execute: (input: {
        readonly workflow: Workflow;
        readonly sources: readonly RetainedDaPayloadSource[];
        readonly journal: FraudProofWorkflowJournalStore;
      }) => Promise<unknown>;
    }>,
): FamilyApplicationRecord<Category, Config, Workflow> => {
  const { category } = definition;
  const declaresAuxiliary =
    Object.keys(definition.auxiliaryReferenceScripts ?? {}).length > 0;
  if (
    declaresAuxiliary !==
    (family.auxiliaryReferenceScriptsKey !== undefined)
  ) {
    throw new Error(
      `${category} must lay its auxiliary reference scripts into the bundle exactly when its definition declares them`,
    );
  }
  const { auxiliaryReferenceScriptsKey } = family;
  return cursorFamilyApplicationRecord<Category, Config, Workflow>(definition, {
    requires: family.requires,
    bindsDecisionDigest: false,
    bindConfig: ({ common, parts }): Config => {
      const bound: BundleCursorFamilyConfig = Object.freeze({
        ...common,
        referenceScripts: Object.freeze({
          ...referenceScriptBundle(parts),
          ...(auxiliaryReferenceScriptsKey === undefined
            ? {}
            : { [auxiliaryReferenceScriptsKey]: parts.auxiliary }),
        }),
      });
      // The step list has the definition's step count and every role resolved
      // or threw above, so this is the family's exact config shape.
      return bound as Config;
    },
    constructWorkflow: family.constructWorkflow,
    execute: family.execute,
  });
};

/**
 * The four cursor families without a decision digest on the plain and
 * replay-context shapes. Each row names its exact config and workflow types
 * so the record's `bindConfig` result is what its `constructWorkflow`
 * consumes.
 */
export const NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "nativeScriptInvalid",
    ManifestBoundNativeScriptInvalidWorkflowConfig,
    ManifestBoundNativeScriptInvalidWorkflow
  >(NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION, {
    requires: [],
    constructWorkflow: createManifestBoundNativeScriptInvalidWorkflow,
    execute: runOrResumeManifestBoundNativeScriptInvalidWorkflow,
  });

export const NATIVE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "nativeScriptDecoding",
    ManifestBoundNativeScriptDecodingWorkflowConfig,
    ManifestBoundNativeScriptDecodingWorkflow
  >(NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundNativeScriptDecodingWorkflow,
    execute: runOrResumeManifestBoundNativeScriptDecodingWorkflow,
  });

export const MINT_AUTHORIZATION_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "mintAuthorization",
    ManifestBoundMintAuthorizationWorkflowConfig,
    ManifestBoundMintAuthorizationWorkflow
  >(MINT_AUTHORIZATION_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundMintAuthorizationWorkflow,
    execute: runOrResumeManifestBoundMintAuthorizationWorkflow,
  });

export const WITHDRAWAL_MISTAG_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "withdrawalMistag",
    ManifestBoundWithdrawalMistagWorkflowConfig,
    ManifestBoundWithdrawalMistagWorkflow
  >(WITHDRAWAL_MISTAG_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundWithdrawalMistagWorkflow,
    execute: runOrResumeManifestBoundWithdrawalMistagWorkflow,
  });

/**
 * The families that replay the challenged block against its authenticated
 * predecessor read it from the classifier-admitted replay context.
 */
export const MIN_ADA_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "minAda",
    ManifestBoundMinAdaWorkflowConfig,
    ManifestBoundMinAdaWorkflow
  >(MIN_ADA_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    auxiliaryReferenceScriptsKey: "yields",
    constructWorkflow: createManifestBoundMinAdaWorkflow,
    execute: runOrResumeManifestBoundMinAdaWorkflow,
  });

/**
 * crossBlockDuplicateEvent fetches each settled block's payload from the same
 * public retained-DA sources as the challenged block.
 */
export const CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "crossBlockDuplicateEvent",
    ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
    ManifestBoundCrossBlockDuplicateEventWorkflow
  >(CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_DEFINITION, {
    requires: ["retainedDaSources"],
    constructWorkflow: createManifestBoundCrossBlockDuplicateEventWorkflow,
    execute: runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow,
  });

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "executionNativeScriptInvalid",
    ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
    ManifestBoundExecutionNativeScriptInvalidWorkflow
  >(EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    auxiliaryReferenceScriptsKey: "removal",
    constructWorkflow: createManifestBoundExecutionNativeScriptInvalidWorkflow,
    execute: runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow,
  });

/**
 * Transition-trace reads every reference script from one map keyed by
 * contract name — its chain steps, its witnesses and its yield entries alike —
 * so its record lays the derived roster out by the contract each role names.
 */
export const TRANSITION_TRACE_FAMILY_APPLICATION_RECORD =
  cursorFamilyApplicationRecord<
    "transitionTrace",
    ManifestBoundTransitionTraceWorkflowConfig,
    ManifestBoundTransitionTraceWorkflow
  >(TRANSITION_TRACE_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    bindsDecisionDigest: false,
    bindConfig: ({ common, roster, references }) =>
      Object.freeze({
        ...common,
        referenceScripts: Object.freeze(
          Object.fromEntries(
            Object.entries(roster).map(([role, contractName]) => [
              contractName,
              requiredReference(references, role),
            ]),
          ),
        ),
      }) as ManifestBoundTransitionTraceWorkflowConfig,
    constructWorkflow: createManifestBoundTransitionTraceWorkflow,
    execute: runOrResumeManifestBoundTransitionTraceWorkflow,
  });

export const RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "resolvedOutputNonCanonical",
    ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
    ManifestBoundResolvedOutputNonCanonicalWorkflow
  >(RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION, {
    contracts: RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundResolvedOutputNonCanonicalWorkflow,
    execute: executeManifestBoundResolvedOutputNonCanonicalWorkflow,
  });

export const SPEND_INPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD =
  authenticatedCertificateFamilyApplicationRecord<
    "spendInputSignerMissing",
    ManifestBoundSpendInputSignerMissingWorkflowConfig,
    ManifestBoundSpendInputSignerMissingWorkflow
  >(SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION, {
    contracts: SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundSpendInputSignerMissingWorkflow,
    execute: executeManifestBoundSpendInputSignerMissingWorkflow,
  });

/**
 * The second hand-written record. Mint-item-non-canonical has no
 * `FamilyDefinition`: it binds its four-step chain through its own loader.
 * Its manifest-contracts map is already role to contract, so it is the roster
 * as declared, and its config is the authenticated certificate shape.
 */
/**
 * mintItemNonCanonical has no family definition, so its roster is its
 * manifest-contracts map and its config keys are read from the same map:
 * the `stepNN` keys in step order, and every other key but the certificate
 * as a witness role. Nothing about the family is restated here.
 */
const MINT_ITEM_NON_CANONICAL_STEP_CONFIG_KEYS = Object.freeze(
  Object.keys(MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS)
    .filter((key) => /^step\d{2}$/u.test(key))
    .sort(),
);

const MINT_ITEM_NON_CANONICAL_WITNESS_ROLES = Object.freeze(
  Object.keys(MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS).filter(
    (key) =>
      !MINT_ITEM_NON_CANONICAL_STEP_CONFIG_KEYS.includes(key) &&
      key !== "fieldPreimageCertificateMint",
  ),
);

export const MINT_ITEM_NON_CANONICAL_FAMILY_APPLICATION_RECORD =
  defineFamilyApplication<
    "mintItemNonCanonical",
    ManifestBoundMintItemNonCanonicalWorkflowConfig,
    ManifestBoundMintItemNonCanonicalWorkflow
  >({
    category: "mintItemNonCanonical",
    roster: MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS,
    requires: [],
    bindConfig: ({ infrastructure, references }) =>
      bindAuthenticatedCertificateFamilyConfig({
        category: "mintItemNonCanonical",
        infrastructure,
        references,
        stepConfigKeys: MINT_ITEM_NON_CANONICAL_STEP_CONFIG_KEYS,
        witnessRoles: MINT_ITEM_NON_CANONICAL_WITNESS_ROLES,
      }) as ManifestBoundMintItemNonCanonicalWorkflowConfig,
    constructWorkflow: createManifestBoundMintItemNonCanonicalWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await executeManifestBoundMintItemNonCanonicalWorkflow({
        workflow,
        sources,
        journal,
      }),
    bindsDecisionDigest: true,
  });
