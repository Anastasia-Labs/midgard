/**
 * The family application registry: one record per fault family, keyed by
 * catalogue category. Its keys are the installed set; a host installs every
 * registered family through the one shared loop in `family-application.ts`
 * rather than branching per family.
 *
 * A linear family's record is derived from its `FamilyDefinition`: the step
 * contract names come from its linear spec, the witness roles and the
 * field-preimage certificate flag from the definition itself, so nothing is
 * restated. A decision-digest cursor family's record is derived the same way,
 * with the state-queue removal set read from the definition's auxiliary
 * reference scripts; a decision-digest family on the authenticated certificate
 * shape derives its record the same way, laying each step into the config key
 * that names that step's contract. A cursor family without a decision digest
 * derives its record from its definition too, declaring beside its roster the
 * optional infrastructure it requires — the predecessor replay context or the
 * historical native-script authority — and the config fields it reads from
 * it. A family whose config shape is its own writes a short record by hand:
 * `doubleSpend`, `transitionTrace`, `mintItemNonCanonical`, `missingSignature`,
 * `networkId`, `valueNotPreserved` and `validationTraceDispute`, the last of
 * which is the one family that requires the host's validation-challenge port.
 *
 * The `satisfies` guard requires exactly one record per catalogue category,
 * with the record's own category as its key, so an omitted or misnamed family
 * fails typecheck.
 */
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  createManifestBoundCrossBlockDuplicateEventWorkflow,
  CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_DEFINITION,
  type ManifestBoundCrossBlockDuplicateEventWorkflow,
  type ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
  runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow,
} from "../cross-block-duplicate-event/workflow.js";
import {
  createManifestBoundDistinctAssetAccumulationWorkflow,
  DISTINCT_ASSET_ACCUMULATION_FAMILY_DEFINITION,
  executeManifestBoundDistinctAssetAccumulationWorkflow,
  type ManifestBoundDistinctAssetAccumulationWorkflow,
  type ManifestBoundDistinctAssetAccumulationWorkflowConfig,
} from "../distinct-asset-accumulation-limit/v1.js";
import {
  createManifestBoundExecutionNativeScriptInvalidWorkflow,
  EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
  type ManifestBoundExecutionNativeScriptInvalidWorkflow,
  type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
  runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow,
} from "../execution-native-script-invalid/v1.js";
import {
  createManifestBoundExecutionSourceScriptDecodingWorkflow,
  executeManifestBoundExecutionSourceScriptDecodingWorkflow,
  EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION,
  type ManifestBoundExecutionSourceScriptDecodingWorkflow,
  type ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
} from "../execution-source-script-decoding/v1.js";
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
  createManifestBoundMintDeclaredAssetLimitWorkflow,
  executeManifestBoundMintDeclaredAssetLimitWorkflow,
  type ManifestBoundMintDeclaredAssetLimitWorkflow,
  type ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
  MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION,
} from "../mint-declared-asset-limit/v1.js";
import {
  createManifestBoundMintItemNonCanonicalWorkflow,
  executeManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflowConfig,
  MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS,
} from "../mint-item-non-canonical/workflow.js";
import {
  createManifestBoundMissingNativeScriptTxWorkflow,
  type ManifestBoundMissingNativeScriptTxWorkflow,
  type ManifestBoundMissingNativeScriptTxWorkflowConfig,
  MISSING_NATIVE_SCRIPT_TX_FAMILY_DEFINITION,
  runOrResumeManifestBoundMissingNativeScriptTxWorkflow,
} from "../missing-native-script-tx/workflow.js";
import {
  createManifestBoundMissingNativeScriptUtxoWorkflow,
  type ManifestBoundMissingNativeScriptUtxoWorkflow,
  type ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
  MISSING_NATIVE_SCRIPT_UTXO_FAMILY_DEFINITION,
  runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow,
} from "../missing-native-script-utxo/workflow.js";
import {
  createManifestBoundMissingRedeemerWorkflow,
  executeManifestBoundMissingRedeemerWorkflow,
  type ManifestBoundMissingRedeemerWorkflow,
  type ManifestBoundMissingRedeemerWorkflowConfig,
  MISSING_REDEEMER_FAMILY_DEFINITION,
} from "../missing-redeemer/v1.js";
import {
  createManifestBoundMissingScriptSourceWorkflow,
  executeManifestBoundMissingScriptSourceWorkflow,
  type ManifestBoundMissingScriptSourceWorkflow,
  type ManifestBoundMissingScriptSourceWorkflowConfig,
  MISSING_SCRIPT_SOURCE_FAMILY_DEFINITION,
} from "../missing-script-source/v1.js";
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
  createManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflowConfig,
  runOrResumeManifestBoundNetworkIdWorkflow,
} from "../network-id/workflow-adapter.js";
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
import {
  createManifestBoundReceivePurposeLanguageWorkflow,
  executeManifestBoundReceivePurposeLanguageWorkflow,
  type ManifestBoundReceivePurposeLanguageWorkflow,
  type ManifestBoundReceivePurposeLanguageWorkflowConfig,
  RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION,
} from "../receive-purpose-language/manifest-workflow.js";
import {
  createManifestBoundRedeemerCanonicityWorkflow,
  executeManifestBoundRedeemerCanonicityWorkflow,
  type ManifestBoundRedeemerCanonicityWorkflow,
  type ManifestBoundRedeemerCanonicityWorkflowConfig,
  REDEEMER_CANONICITY_FAMILY_DEFINITION,
} from "../redeemer-canonicity/runtime.js";
import {
  REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES,
  type StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import {
  createManifestBoundResolvedOutputNonCanonicalWorkflow,
  executeManifestBoundResolvedOutputNonCanonicalWorkflow,
  type ManifestBoundResolvedOutputNonCanonicalWorkflow,
  type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
  RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
  RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
} from "../resolved-output-non-canonical/authenticated-workflow.js";
import {
  NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  createManifestBoundScriptIntegrityHashMismatchWorkflow,
  executeManifestBoundScriptIntegrityHashMismatchWorkflow,
  type ManifestBoundScriptIntegrityHashMismatchWorkflow,
  type ManifestBoundScriptIntegrityHashMismatchWorkflowConfig,
  SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_DEFINITION,
} from "../script-integrity-hash-mismatch/manifest-workflow.js";
import {
  createManifestBoundScriptIntegrityHashMissingWorkflow,
  executeManifestBoundScriptIntegrityHashMissingWorkflow,
  type ManifestBoundScriptIntegrityHashMissingWorkflow,
  type ManifestBoundScriptIntegrityHashMissingWorkflowConfig,
  SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION,
} from "../script-integrity-hash-missing/v1.js";
import {
  createManifestBoundSpendInputSignerMissingWorkflow,
  executeManifestBoundSpendInputSignerMissingWorkflow,
  type ManifestBoundSpendInputSignerMissingWorkflow,
  type ManifestBoundSpendInputSignerMissingWorkflowConfig,
  SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
  SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
} from "../spend-input-signer-missing/authenticated-workflow.js";
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
  createManifestBoundTransitionTraceWorkflow,
  type ManifestBoundTransitionTraceWorkflow,
  type ManifestBoundTransitionTraceWorkflowConfig,
  runOrResumeManifestBoundTransitionTraceWorkflow,
  TRANSITION_TRACE_FAMILY_DEFINITION,
} from "../transition-trace/workflow.js";
import {
  createManifestBoundUnusedRedeemerWorkflow,
  executeManifestBoundUnusedRedeemerWorkflow,
  type ManifestBoundUnusedRedeemerWorkflow,
  type ManifestBoundUnusedRedeemerWorkflowConfig,
  UNUSED_REDEEMER_FAMILY_DEFINITION,
} from "../unused-redeemer/v1.js";
import {
  createManifestBoundUnusedScriptWitnessWorkflow,
  executeManifestBoundUnusedScriptWitnessWorkflow,
  type ManifestBoundUnusedScriptWitnessWorkflow,
  type ManifestBoundUnusedScriptWitnessWorkflowConfig,
  UNUSED_SCRIPT_WITNESS_FAMILY_DEFINITION,
} from "../unused-script-witness/v1.js";
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
import {
  createManifestBoundWithdrawalMistagWorkflow,
  type ManifestBoundWithdrawalMistagWorkflow,
  type ManifestBoundWithdrawalMistagWorkflowConfig,
  runOrResumeManifestBoundWithdrawalMistagWorkflow,
  WITHDRAWAL_MISTAG_FAMILY_DEFINITION,
} from "../withdrawal-mistag/workflow.js";
import {
  createManifestBoundWitnessScriptDecodingWorkflow,
  executeManifestBoundWitnessScriptDecodingWorkflow,
  type ManifestBoundWitnessScriptDecodingWorkflow,
  type ManifestBoundWitnessScriptDecodingWorkflowConfig,
  WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
  WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS,
} from "../witness-script-decoding/workflow.js";
import type { ValidationTraceChallenge } from "./challenge-authority.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import { STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS } from "./cursor-family-runtime.js";
import {
  type ManifestBoundDaHashPreimageWorkflow,
  runOrResumeManifestBoundDaHashPreimageWorkflow,
} from "./da-hash-preimage.js";
import {
  createManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflowConfig,
  runOrResumeManifestBoundDoubleSpendWorkflow,
} from "./double-spend-adapter.js";
import {
  assertFamilyDefinitionRoster,
  defineFamilyApplication,
  type FamilyApplicationRecord,
  type FamilyApplicationRequirement,
  type FamilyApplicationWorkflowIdentity,
  type FamilyCommonInfrastructure,
  familyDefinitionRoster,
  type FamilyHistoricalNativeScriptAuthority,
  type FamilyResolvedReferenceScripts,
  type FamilyRosterDefinition,
  familyStepRole,
  type LinearFamilyApplicationRecord,
} from "./family-application.js";
import {
  type FamilyCategory,
  type FamilyDefinition,
  familyStepContractNames,
  type FaultProofWitnessRole,
  type ManifestBoundFamilyWorkflow,
  type ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import {
  type AnyLinearFamilyDefinition,
  LINEAR_FAMILY_DEFINITIONS,
} from "./linear-family-definitions.js";
import {
  type LinearFamilyCategory,
  linearFamilySpec,
} from "./linear-family-spec.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  createManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflowConfig,
  runOrResumeManifestBoundMissingSignatureWorkflow,
} from "./missing-signature.js";

const requiredReference = (
  references: FamilyResolvedReferenceScripts,
  role: string,
): UTxO => {
  const reference = references[role];
  if (reference === undefined) {
    throw new Error(`family application roster omitted ${role}`);
  }
  return reference;
};

/**
 * The given roster roles resolved into a record keyed by those roles, for a
 * hand-written record whose config groups a fixed set of roles under one key.
 */
const resolvedRoles = <Role extends string>(
  references: FamilyResolvedReferenceScripts,
  roles: readonly Role[],
): Readonly<Record<Role, UTxO>> =>
  Object.freeze(
    Object.fromEntries(
      roles.map((role) => [role, requiredReference(references, role)]),
    ) as Record<Role, UTxO>,
  );

/** The role names of a contract-name table, typed as its keys. */
const rolesOf = <Table extends Readonly<Record<string, string>>>(
  table: Table,
): readonly (keyof Table & string)[] =>
  Object.keys(table) as (keyof Table & string)[];

/**
 * A hand-written record's step roster: `step01`… in the order of the family's
 * own step contract-name list.
 */
const stepRoster = (
  contractNames: readonly string[],
): Readonly<Record<string, string>> =>
  Object.freeze(
    Object.fromEntries(
      contractNames.map((contractName, index) => [
        familyStepRole(index + 1),
        contractName,
      ]),
    ),
  );

/**
 * The resolved step references of a hand-written record, as the tuple its
 * family's config declares (a two- or four-step chain).
 */
function resolvedSteps(
  references: FamilyResolvedReferenceScripts,
  count: 2,
): readonly [UTxO, UTxO];
function resolvedSteps(
  references: FamilyResolvedReferenceScripts,
  count: 4,
): readonly [UTxO, UTxO, UTxO, UTxO];
function resolvedSteps(
  references: FamilyResolvedReferenceScripts,
  count: 2 | 4,
): readonly UTxO[] {
  return Object.freeze(
    Array.from({ length: count }, (_, index) =>
      requiredReference(references, familyStepRole(index + 1)),
    ),
  );
}

/**
 * The predecessor replay context, laid into a family's config only when its
 * record requires it and the host supplied one. The shared loop refuses a
 * required context that is absent before `bindConfig` runs; here the field
 * stays optional so a reconciliation-only invocation, which is exempt from
 * the requirement, still binds.
 */
const optionalReplayContext = (
  requires: readonly FamilyApplicationRequirement[],
  infrastructure: FamilyCommonInfrastructure,
): Readonly<{ replayContext?: CompleteCanonicalReplayContext }> =>
  requires.includes("replayContext") &&
  infrastructure.replayContext !== undefined
    ? { replayContext: infrastructure.replayContext }
    : {};

/**
 * The predecessor replay context whenever the host supplied one, for a family
 * that replays the predecessor only when the classifier admitted one and
 * proves from the challenged block alone otherwise.
 */
const suppliedReplayContext = (
  infrastructure: FamilyCommonInfrastructure,
): Readonly<{ replayContext?: CompleteCanonicalReplayContext }> =>
  infrastructure.replayContext === undefined
    ? {}
    : { replayContext: infrastructure.replayContext };

/**
 * The historical native-script authority, or a refusal naming the family
 * without one. The shared loop refuses first; this is the guard `bindConfig`
 * keeps for a caller that reaches it another way.
 */
const requiredHistoricalNativeScriptAuthority = (
  category: FamilyCategory,
  infrastructure: FamilyCommonInfrastructure,
): FamilyHistoricalNativeScriptAuthority => {
  if (infrastructure.historicalNativeScriptAuthority === undefined) {
    throw new Error(
      `${category} reconstructs historical native scripts, which this invocation carries no authority for`,
    );
  }
  return infrastructure.historicalNativeScriptAuthority;
};

/**
 * The historical authority under the `historicalNativeScript*` spelling the
 * min-ADA, missing-native-script and transition-trace families read.
 */
const historicalNativeScriptPrefixedFields = (
  authority: FamilyHistoricalNativeScriptAuthority,
) =>
  ({
    historicalNativeScriptCheckpointStore: authority.checkpointStore,
    historicalNativeScriptHistorySource: authority.historySource,
  }) as const;

/**
 * The historical authority under the `historical*` spelling the
 * certificate-shape signer and output families and the execution
 * native-script family read.
 */
const historicalPrefixedFields = (
  authority: FamilyHistoricalNativeScriptAuthority,
) =>
  ({
    historicalCheckpointStore: authority.checkpointStore,
    historicalSource: authority.historySource,
  }) as const;

/**
 * How a cursor family reads the optional infrastructure it requires. The
 * replay context has one spelling in every config, so it is laid in
 * generically; the historical authority's two parts are spelled differently
 * across the families, so each record names the fields it reads.
 */
type CursorFamilyRequirements = Readonly<{
  requires: readonly FamilyApplicationRequirement[];
  /**
   * The config fields the family reads from the historical native-script
   * authority. Present exactly when `requires` names the authority.
   */
  historicalNativeScriptAuthority?: (
    authority: FamilyHistoricalNativeScriptAuthority,
  ) => Readonly<Record<string, unknown>>;
}>;

/**
 * The config fields a cursor family reads from the infrastructure its record
 * requires. Refuses at import a layout without its flag or a flag without its
 * layout, so a family cannot require the authority and never read it, or read
 * it without the shared loop having checked that the host supplied it.
 */
const requiredInfrastructureFields = (
  category: FamilyCategory,
  family: CursorFamilyRequirements,
): ((
  infrastructure: FamilyCommonInfrastructure,
) => Readonly<Record<string, unknown>>) => {
  const requiresAuthority = family.requires.includes(
    "historicalNativeScriptAuthority",
  );
  if (
    requiresAuthority !==
    (family.historicalNativeScriptAuthority !== undefined)
  ) {
    throw new Error(
      `${category} must lay the historical native-script authority into its config exactly when it requires it`,
    );
  }
  return (infrastructure) =>
    Object.freeze({
      ...optionalReplayContext(family.requires, infrastructure),
      ...(family.historicalNativeScriptAuthority === undefined
        ? {}
        : family.historicalNativeScriptAuthority(
            requiredHistoricalNativeScriptAuthority(category, infrastructure),
          )),
    });
};

/**
 * A definition's roster resolved into the parts a config lays out: the chain
 * steps in step order, the witness roles, the certificate when the definition
 * binds it, and the declared auxiliary reference scripts.
 */
type ResolvedRosterParts = Readonly<{
  steps: readonly UTxO[];
  witnesses: Readonly<Record<string, UTxO>>;
  fieldPreimageCertificateMint?: UTxO;
  auxiliary: Readonly<Record<string, UTxO>>;
}>;

const resolveRosterParts = (
  definition: FamilyRosterDefinition,
  references: FamilyResolvedReferenceScripts,
): ResolvedRosterParts =>
  Object.freeze({
    steps: Object.freeze(
      familyStepContractNames(definition).map((_, index) =>
        requiredReference(references, familyStepRole(index + 1)),
      ),
    ),
    witnesses: Object.freeze(
      Object.fromEntries(
        definition.witnessRoles.map((role) => [
          role,
          requiredReference(references, role),
        ]),
      ),
    ),
    ...(definition.fieldPreimageCertificate
      ? {
          fieldPreimageCertificateMint: requiredReference(
            references,
            "fieldPreimageCertificateMint",
          ),
        }
      : {}),
    auxiliary: Object.freeze(
      Object.fromEntries(
        Object.keys(definition.auxiliaryReferenceScripts ?? {}).map((role) => [
          role,
          requiredReference(references, role),
        ]),
      ),
    ),
  });

/**
 * The `{steps, witnesses, certificate}` bundle most cursor families read
 * their reference scripts from; a family with auxiliary scripts adds them
 * under its own key.
 */
const referenceScriptBundle = ({
  steps,
  witnesses,
  fieldPreimageCertificateMint,
}: ResolvedRosterParts) =>
  Object.freeze({
    steps,
    witnesses,
    ...(fieldPreimageCertificateMint === undefined
      ? {}
      : { fieldPreimageCertificateMint }),
  });

/**
 * The linear families that may only execute against a classifier-admitted
 * predecessor replay context. Stated once, here, and read by both the
 * load-time and the decision-time check.
 */
const LINEAR_FAMILY_REQUIREMENTS: Readonly<
  Partial<Record<LinearFamilyCategory, readonly FamilyApplicationRequirement[]>>
> = Object.freeze({
  nonExistentInput: Object.freeze(["replayContext"] as const),
  noReferenceInput: Object.freeze(["replayContext"] as const),
});

type WidenedLinearConfig = ManifestBoundFamilyWorkflowConfig<
  LinearFamilyCategory,
  FaultProofWitnessRole,
  boolean
>;

type WidenedLinearWorkflow = ManifestBoundFamilyWorkflow<
  LinearFamilyCategory,
  boolean
>;

/**
 * Q44 is the one linear family that does not launch through the generic
 * retained-DA runner: it fetches its raw source leaf through the dedicated
 * orchestrator entry, so no canonical replayer is consulted.
 *
 * The runtime's own derived factory row still selects the same two routes for
 * the live runner table; that row is what a later slice of the parent spec
 * replaces with these records, and the duplication ends there.
 */
const linearFamilyExecute = (
  category: LinearFamilyCategory,
): FamilyApplicationRecord<
  LinearFamilyCategory,
  WidenedLinearConfig,
  WidenedLinearWorkflow
>["execute"] =>
  category === "daHashPreimage"
    ? async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundDaHashPreimageWorkflow({
          workflow: workflow as ManifestBoundDaHashPreimageWorkflow,
          sources,
          journal,
        })
    : async ({ workflow, sources, journal }) =>
        await runOrResumeManifestBoundFamilyWorkflow({
          workflow,
          sources,
          journal,
        });

const linearFamilyApplicationRecord = (
  definition: AnyLinearFamilyDefinition,
): FamilyApplicationRecord<
  LinearFamilyCategory,
  WidenedLinearConfig,
  WidenedLinearWorkflow
> => {
  const { category } = definition;
  // A derived roster carries the family's chain steps, its witness roles and
  // its certificate policy; it has no place for auxiliary reference scripts.
  // `FamilyDefinition` declares them optional on every member, so this cannot
  // be a parameter constraint; a linear definition that grows them fails the
  // registry at import rather than resolving an incomplete roster.
  if (definition.auxiliaryReferenceScripts !== undefined) {
    throw new Error(
      `${category} declares auxiliary reference scripts, which a derived roster cannot carry`,
    );
  }
  const roster = familyDefinitionRoster(definition);
  assertFamilyDefinitionRoster(definition, roster);
  const stepRoles = linearFamilySpec(category).steps.map((step) =>
    familyStepRole(step.ordinal),
  );
  const requires = LINEAR_FAMILY_REQUIREMENTS[category] ?? [];
  return defineFamilyApplication({
    category,
    roster,
    requires,
    bindConfig: ({ infrastructure, references }): WidenedLinearConfig =>
      Object.freeze({
        ...commonBoundConfigFields(infrastructure),
        ...optionalReplayContext(requires, infrastructure),
        referenceScripts: Object.freeze({
          steps: Object.freeze(
            stepRoles.map((role) => requiredReference(references, role)),
          ),
          witnesses: Object.freeze(
            Object.fromEntries(
              definition.witnessRoles.map((role) => [
                role,
                requiredReference(references, role),
              ]),
            ),
          ),
          ...(definition.fieldPreimageCertificate
            ? {
                fieldPreimageCertificateMint: requiredReference(
                  references,
                  "fieldPreimageCertificateMint",
                ),
              }
            : {}),
        }),
        // The step list is built from the same spec that fixes the family's
        // step arity, and every role resolves or throws above, so its length
        // is the tuple length the widened config union expects.
      }) as WidenedLinearConfig,
    constructWorkflow: async (config) =>
      await assembleManifestBoundFamilyWorkflow(
        definition as FamilyDefinition<
          LinearFamilyCategory,
          FaultProofWitnessRole,
          boolean
        >,
        config,
      ),
    execute: linearFamilyExecute(category),
    bindsDecisionDigest: false,
  });
};

type LinearFamilyApplicationRecords = {
  readonly [Category in LinearFamilyCategory]: LinearFamilyApplicationRecord<Category>;
};

/**
 * The derived rows, one per linear definition. Each record is built at the
 * widened linear types and asserted back into its exact row position, the same
 * way the runtime's derived runner-factory table does and for the same reason:
 * a union member's polarity cannot be correlated with its own callbacks. The
 * `satisfies` guard on the registry below therefore proves completeness and
 * keying, not row shape — the roster table test proves that a derived row's
 * step roles are its spec's step contract names and its witness roles its
 * definition's.
 */
const LINEAR_FAMILY_APPLICATION_RECORDS = Object.freeze(
  Object.fromEntries(
    Object.values(LINEAR_FAMILY_DEFINITIONS).map(
      (definition): readonly [LinearFamilyCategory, unknown] => [
        definition.category,
        linearFamilyApplicationRecord(definition),
      ],
    ),
  ),
) as LinearFamilyApplicationRecords;

const DOUBLE_SPEND_STEP_CONTRACT_NAMES = Object.freeze([
  "fraudProofDoubleSpend",
  "fraudProofDoubleSpendStep02",
  "fraudProofDoubleSpendStep03",
  "fraudProofDoubleSpendStep04",
] as const);

/**
 * The first hand-written record. Double-spend has no `FamilyDefinition`: it
 * keeps its own four-step adapter and takes the field-preimage certificate as
 * a bare reference script rather than inside its reference-script bundle.
 */
export const DOUBLE_SPEND_FAMILY_APPLICATION_RECORD = defineFamilyApplication<
  "doubleSpend",
  ManifestBoundDoubleSpendWorkflowConfig,
  ManifestBoundDoubleSpendWorkflow
>({
  category: "doubleSpend",
  roster: Object.freeze({
    ...stepRoster(DOUBLE_SPEND_STEP_CONTRACT_NAMES),
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  }),
  requires: [],
  bindConfig: ({ infrastructure, references }) =>
    Object.freeze({
      ...commonBoundConfigFields(infrastructure),
      referenceScripts: Object.freeze({
        steps: resolvedSteps(references, 4),
        witnesses: Object.freeze({
          computationThreadMint: requiredReference(
            references,
            "computationThreadMint",
          ),
          fraudProofMint: requiredReference(references, "fraudProofMint"),
          phasMembershipWithdraw: requiredReference(
            references,
            "phasMembershipWithdraw",
          ),
          chunkedVerifyWithdraw: requiredReference(
            references,
            "chunkedVerifyWithdraw",
          ),
        }),
      }),
      fieldPreimageCertificateReferenceScript: requiredReference(
        references,
        "fieldPreimageCertificateMint",
      ),
    }),
  constructWorkflow: createManifestBoundDoubleSpendWorkflow,
  execute: async ({ workflow, sources, journal }) =>
    await runOrResumeManifestBoundDoubleSpendWorkflow({
      workflow,
      sources,
      journal,
    }),
  bindsDecisionDigest: false,
});

/**
 * The config shape every decision-digest cursor family shares: the common
 * infrastructure, the admitted decision digest, and a reference-script bundle
 * whose `removal` member is the state-queue removal set the family spends by
 * reference after its proof token is minted. Each family's own config type
 * narrows the step tuple and the witness roster; the record builder lays the
 * roster in at this shape and asserts the family's exact type, which the
 * derived roster's step count and role set make sound.
 */
type DecisionDigestCursorFamilyConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: Readonly<{
    steps: readonly UTxO[];
    witnesses: Readonly<Record<string, UTxO>>;
    fieldPreimageCertificateMint?: UTxO;
    removal: Readonly<Record<string, UTxO>>;
  }>;
}>;

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
 * The twelve decision-digest families on the state-queue-removal witness set.
 * Each row names its exact config and workflow types so the record's
 * `bindConfig` result is what its `constructWorkflow` consumes.
 */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "distinctAssetAccumulationLimit",
    ManifestBoundDistinctAssetAccumulationWorkflowConfig,
    ManifestBoundDistinctAssetAccumulationWorkflow
  >(DISTINCT_ASSET_ACCUMULATION_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundDistinctAssetAccumulationWorkflow,
    execute: executeManifestBoundDistinctAssetAccumulationWorkflow,
  });

export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "executionSourceScriptDecoding",
    ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
    ManifestBoundExecutionSourceScriptDecodingWorkflow
  >(EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundExecutionSourceScriptDecodingWorkflow,
    execute: executeManifestBoundExecutionSourceScriptDecodingWorkflow,
  });

export const MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "missingScriptSource",
    ManifestBoundMissingScriptSourceWorkflowConfig,
    ManifestBoundMissingScriptSourceWorkflow
  >(MISSING_SCRIPT_SOURCE_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundMissingScriptSourceWorkflow,
    execute: executeManifestBoundMissingScriptSourceWorkflow,
  });

export const RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "receivePurposeLanguage",
    ManifestBoundReceivePurposeLanguageWorkflowConfig,
    ManifestBoundReceivePurposeLanguageWorkflow
  >(RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundReceivePurposeLanguageWorkflow,
    execute: executeManifestBoundReceivePurposeLanguageWorkflow,
  });

export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "scriptIntegrityHashMismatch",
    ManifestBoundScriptIntegrityHashMismatchWorkflowConfig,
    ManifestBoundScriptIntegrityHashMismatchWorkflow
  >(SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundScriptIntegrityHashMismatchWorkflow,
    execute: executeManifestBoundScriptIntegrityHashMismatchWorkflow,
  });

export const UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "unusedRedeemer",
    ManifestBoundUnusedRedeemerWorkflowConfig,
    ManifestBoundUnusedRedeemerWorkflow
  >(UNUSED_REDEEMER_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundUnusedRedeemerWorkflow,
    execute: executeManifestBoundUnusedRedeemerWorkflow,
  });

export const MINT_DECLARED_ASSET_LIMIT_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "mintDeclaredAssetLimit",
    ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
    ManifestBoundMintDeclaredAssetLimitWorkflow
  >(MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundMintDeclaredAssetLimitWorkflow,
    execute: executeManifestBoundMintDeclaredAssetLimitWorkflow,
  });

export const MISSING_REDEEMER_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "missingRedeemer",
    ManifestBoundMissingRedeemerWorkflowConfig,
    ManifestBoundMissingRedeemerWorkflow
  >(MISSING_REDEEMER_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundMissingRedeemerWorkflow,
    execute: executeManifestBoundMissingRedeemerWorkflow,
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

export const UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD =
  decisionDigestCursorFamilyApplicationRecord<
    "unusedScriptWitness",
    ManifestBoundUnusedScriptWitnessWorkflowConfig,
    ManifestBoundUnusedScriptWitnessWorkflow
  >(UNUSED_SCRIPT_WITNESS_FAMILY_DEFINITION, {
    constructWorkflow: createManifestBoundUnusedScriptWitnessWorkflow,
    execute: executeManifestBoundUnusedScriptWitnessWorkflow,
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
type AuthenticatedCertificateFamilyConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: Readonly<{
    fieldPreimageCertificateMint: UTxO;
    witnesses: Readonly<Record<string, UTxO>>;
  }> &
    Readonly<Record<string, unknown>>;
}>;

/**
 * The common infrastructure every family's manifest-bound config carries
 * verbatim: the parts that are not a reference script or the decision digest.
 */
const commonBoundConfigFields = (infrastructure: FamilyCommonInfrastructure) =>
  ({
    manifest: infrastructure.manifest,
    blueprintJson: infrastructure.blueprintJson,
    deploymentInfo: infrastructure.deploymentInfo,
    headerHash: infrastructure.headerHash,
    lucid: infrastructure.lucid,
    signer: infrastructure.signer,
    source: infrastructure.source,
    stateQueueMutationLeaseCoordinator:
      infrastructure.stateQueueMutationLeaseCoordinator,
  }) as const;

/** The admitted decision digest, or a refusal naming the family without one. */
const requiredDecisionDigest = (
  category: FamilyCategory,
  infrastructure: FamilyCommonInfrastructure,
): string => {
  if (infrastructure.decisionDigest === undefined) {
    throw new Error(
      `${category} binds the admitted decision digest, which this invocation does not carry`,
    );
  }
  return infrastructure.decisionDigest;
};

/**
 * Lays a resolved roster into the authenticated certificate shape: step `i`
 * under its family's config key, the certificate under its own role, and
 * every witness role inside `witnesses`.
 */
const bindAuthenticatedCertificateFamilyConfig = ({
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
const authenticatedCertificateFamilyApplicationRecord = <
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
  const infrastructureFields = requiredInfrastructureFields(category, {
    ...family,
    requires,
  });
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
const cursorFamilyApplicationRecord = <
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
  const infrastructureFields = requiredInfrastructureFields(category, family);
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
 * common infrastructure, the replay context when required, the fields read
 * from the historical authority when required, and a reference-script bundle
 * of the chain steps, the witness roster, the certificate when the definition
 * binds it, and the definition's auxiliary scripts under the family's own key.
 * Each family's own config type narrows the step tuple, the witness roster and
 * the extra fields; the builder lays the roster in at this shape and asserts
 * the family's exact type, which the definition's step count and role set make
 * sound.
 */
type BundleCursorFamilyConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
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
    ...(family.historicalNativeScriptAuthority === undefined
      ? {}
      : {
          historicalNativeScriptAuthority:
            family.historicalNativeScriptAuthority,
        }),
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
 * The eight families that reconstruct pre-genesis native scripts through the
 * historical authority. Each record names the fields its config reads the
 * authority's history source and checkpoint store from; the requirement flag
 * and that layout are checked against each other at import.
 */
export const MIN_ADA_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "minAda",
    ManifestBoundMinAdaWorkflowConfig,
    ManifestBoundMinAdaWorkflow
  >(MIN_ADA_FAMILY_DEFINITION, {
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: historicalNativeScriptPrefixedFields,
    auxiliaryReferenceScriptsKey: "yields",
    constructWorkflow: createManifestBoundMinAdaWorkflow,
    execute: runOrResumeManifestBoundMinAdaWorkflow,
  });

export const MISSING_NATIVE_SCRIPT_TX_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "missingNativeScriptTx",
    ManifestBoundMissingNativeScriptTxWorkflowConfig,
    ManifestBoundMissingNativeScriptTxWorkflow
  >(MISSING_NATIVE_SCRIPT_TX_FAMILY_DEFINITION, {
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: (authority) => ({
      ...historicalNativeScriptPrefixedFields(authority),
      historicalNativeScriptL1Roster: authority.l1SourceRoster,
    }),
    constructWorkflow: createManifestBoundMissingNativeScriptTxWorkflow,
    execute: runOrResumeManifestBoundMissingNativeScriptTxWorkflow,
  });

export const MISSING_NATIVE_SCRIPT_UTXO_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "missingNativeScriptUtxo",
    ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
    ManifestBoundMissingNativeScriptUtxoWorkflow
  >(MISSING_NATIVE_SCRIPT_UTXO_FAMILY_DEFINITION, {
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: historicalNativeScriptPrefixedFields,
    constructWorkflow: createManifestBoundMissingNativeScriptUtxoWorkflow,
    execute: runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow,
  });

export const CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "crossBlockDuplicateEvent",
    ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
    ManifestBoundCrossBlockDuplicateEventWorkflow
  >(CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_DEFINITION, {
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: (authority) => ({
      historySource: authority.historySource,
      checkpointStore: authority.checkpointStore,
    }),
    constructWorkflow: createManifestBoundCrossBlockDuplicateEventWorkflow,
    execute: runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow,
  });

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "executionNativeScriptInvalid",
    ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
    ManifestBoundExecutionNativeScriptInvalidWorkflow
  >(EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION, {
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: historicalPrefixedFields,
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
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: historicalNativeScriptPrefixedFields,
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
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: historicalPrefixedFields,
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
    requires: ["historicalNativeScriptAuthority"],
    historicalNativeScriptAuthority: historicalPrefixedFields,
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

/**
 * Every catalogue category's record, keyed by the record's own category: an
 * omitted or misnamed family fails typecheck here.
 */
export const FAMILY_APPLICATION_REGISTRY: {
  readonly [Category in FraudProofCatalogueCategoryName]: FamilyApplicationRegistryEntry<Category>;
} = Object.freeze({
  ...LINEAR_FAMILY_APPLICATION_RECORDS,
  doubleSpend: DOUBLE_SPEND_FAMILY_APPLICATION_RECORD,
  distinctAssetAccumulationLimit:
    DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD,
  executionSourceScriptDecoding:
    EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  missingScriptSource: MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD,
  receivePurposeLanguage: RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD,
  scriptIntegrityHashMismatch:
    SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD,
  unusedRedeemer: UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD,
  mintDeclaredAssetLimit: MINT_DECLARED_ASSET_LIMIT_FAMILY_APPLICATION_RECORD,
  missingRedeemer: MISSING_REDEEMER_FAMILY_APPLICATION_RECORD,
  observerOrderInvalid: OBSERVER_ORDER_INVALID_FAMILY_APPLICATION_RECORD,
  observersForbiddenOnUntaggedNetwork:
    OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAMILY_APPLICATION_RECORD,
  redeemerCanonicity: REDEEMER_CANONICITY_FAMILY_APPLICATION_RECORD,
  scriptIntegrityHashMissing:
    SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_APPLICATION_RECORD,
  unusedScriptWitness: UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD,
  fieldItemWidthIllegal: FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_APPLICATION_RECORD,
  fieldPreimageLengthMismatch:
    FIELD_PREIMAGE_LENGTH_MISMATCH_FAMILY_APPLICATION_RECORD,
  outputReferenceScriptDecoding:
    OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  protectedOutputSignerMissing:
    PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD,
  transactionOutputNonCanonical:
    TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  witnessScriptDecoding: WITNESS_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  mintItemNonCanonical: MINT_ITEM_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  nativeScriptInvalid: NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD,
  nativeScriptDecoding: NATIVE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  mintAuthorization: MINT_AUTHORIZATION_FAMILY_APPLICATION_RECORD,
  withdrawalMistag: WITHDRAWAL_MISTAG_FAMILY_APPLICATION_RECORD,
  minAda: MIN_ADA_FAMILY_APPLICATION_RECORD,
  missingNativeScriptTx: MISSING_NATIVE_SCRIPT_TX_FAMILY_APPLICATION_RECORD,
  missingNativeScriptUtxo: MISSING_NATIVE_SCRIPT_UTXO_FAMILY_APPLICATION_RECORD,
  transitionTrace: TRANSITION_TRACE_FAMILY_APPLICATION_RECORD,
  crossBlockDuplicateEvent:
    CROSS_BLOCK_DUPLICATE_EVENT_FAMILY_APPLICATION_RECORD,
  resolvedOutputNonCanonical:
    RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  spendInputSignerMissing: SPEND_INPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD,
  executionNativeScriptInvalid:
    EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD,
  missingSignature: MISSING_SIGNATURE_FAMILY_APPLICATION_RECORD,
  networkId: NETWORK_ID_FAMILY_APPLICATION_RECORD,
  valueNotPreserved: VALUE_NOT_PRESERVED_FAMILY_APPLICATION_RECORD,
  validationTraceDispute: VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD,
});
