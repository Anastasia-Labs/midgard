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
  createManifestBoundMissingRedeemerWorkflow,
  type ManifestBoundMissingRedeemerWorkflow,
  MISSING_REDEEMER_FAMILY_DEFINITION,
  runOrResumeManifestBoundMissingRedeemerWorkflow,
} from "../missing-redeemer/workflow.js";
import type { ManifestBoundMissingRedeemerWorkflowConfig } from "../missing-redeemer/workflow.transaction-port.js";
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
  createManifestBoundScriptIntegrityHashMismatchWorkflow,
  type ManifestBoundScriptIntegrityHashMismatchWorkflow,
  runOrResumeManifestBoundScriptIntegrityHashMismatchWorkflow,
  SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_DEFINITION,
} from "../script-integrity-hash-mismatch/workflow.js";
import type { ManifestBoundScriptIntegrityHashMismatchWorkflowConfig } from "../script-integrity-hash-mismatch/workflow.transaction-port.js";
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
  CANONICAL_DECODABILITY_FAMILY_DEFINITION,
  createManifestBoundCanonicalDecodabilityWorkflow,
  type ManifestBoundCanonicalDecodabilityWorkflow,
  type ManifestBoundCanonicalDecodabilityWorkflowConfig,
  runOrResumeManifestBoundCanonicalDecodabilityWorkflow,
} from "./canonical-decodability.js";
import {
  COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION,
  createManifestBoundCommittedFieldShapeWorkflow,
  type ManifestBoundCommittedFieldShapeWorkflow,
  type ManifestBoundCommittedFieldShapeWorkflowConfig,
  runOrResumeManifestBoundCommittedFieldShapeWorkflow,
} from "./committed-field-shape.js";
import {
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FamilyRosterDefinition,
} from "./family-application.js";
import {
  authenticatedCertificateFamilyApplicationRecord,
  type BundleCursorFamilyConfig,
  cursorFamilyApplicationRecord,
} from "./family-application-registry.authenticated-certificate-family-application-record.js";
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
export const bundleCursorFamilyApplicationRecord = <
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
 * The six cursor families without a decision digest on the plain and
 * replay-context shapes. Each row names its exact config and workflow types
 * so the record's `bindConfig` result is what its `constructWorkflow`
 * consumes.
 */
export const CANONICAL_DECODABILITY_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "canonicalDecodability",
    ManifestBoundCanonicalDecodabilityWorkflowConfig,
    ManifestBoundCanonicalDecodabilityWorkflow
  >(CANONICAL_DECODABILITY_FAMILY_DEFINITION, {
    requires: [],
    constructWorkflow: createManifestBoundCanonicalDecodabilityWorkflow,
    execute: runOrResumeManifestBoundCanonicalDecodabilityWorkflow,
  });

export const COMMITTED_FIELD_SHAPE_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "committedFieldShape",
    ManifestBoundCommittedFieldShapeWorkflowConfig,
    ManifestBoundCommittedFieldShapeWorkflow
  >(COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION, {
    requires: [],
    constructWorkflow: createManifestBoundCommittedFieldShapeWorkflow,
    execute: runOrResumeManifestBoundCommittedFieldShapeWorkflow,
  });

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

export const MISSING_REDEEMER_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "missingRedeemer",
    ManifestBoundMissingRedeemerWorkflowConfig,
    ManifestBoundMissingRedeemerWorkflow
  >(MISSING_REDEEMER_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundMissingRedeemerWorkflow,
    execute: runOrResumeManifestBoundMissingRedeemerWorkflow,
  });

export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "scriptIntegrityHashMismatch",
    ManifestBoundScriptIntegrityHashMismatchWorkflowConfig,
    ManifestBoundScriptIntegrityHashMismatchWorkflow
  >(SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundScriptIntegrityHashMismatchWorkflow,
    execute: runOrResumeManifestBoundScriptIntegrityHashMismatchWorkflow,
  });

export const MIN_ADA_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "minAda",
    ManifestBoundMinAdaWorkflowConfig,
    ManifestBoundMinAdaWorkflow
  >(MIN_ADA_FAMILY_DEFINITION, {
    requires: [],
    auxiliaryReferenceScriptsKey: "yields",
    constructWorkflow: createManifestBoundMinAdaWorkflow,
    execute: runOrResumeManifestBoundMinAdaWorkflow,
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
