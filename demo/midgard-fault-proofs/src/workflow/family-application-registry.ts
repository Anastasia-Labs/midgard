/**
 * The family application registry: one record per fault family, keyed by
 * catalogue category. Its keys are the installed set; a host installs every
 * registered family through the one shared loop in `family-application.ts`
 * rather than branching per family.
 *
 * A linear family's record is derived from its `FamilyDefinition`: the step
 * contract names come from its linear spec, the witness roles and the
 * field-preimage certificate flag from the definition itself, so nothing is
 * restated. A family whose config shape is its own writes a short record by
 * hand; `doubleSpend` is the first.
 *
 * `NOT_YET_REGISTERED_FAMILY_CATEGORIES` is a shrinking allow-list of
 * catalogue categories that have no record yet, following the pattern the
 * linear definitions table used during its own migration. The `satisfies`
 * guard requires exactly one record per remaining category, with the record's
 * own category as its key, so an omitted or misnamed family fails typecheck.
 */
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

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
  familyDefinitionRoster,
  type FamilyResolvedReferenceScripts,
  familyStepRole,
  type LinearFamilyApplicationRecord,
} from "./family-application.js";
import type {
  FamilyDefinition,
  FaultProofWitnessRole,
  ManifestBoundFamilyWorkflow,
  ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  type AnyLinearFamilyDefinition,
  LINEAR_FAMILY_DEFINITIONS,
} from "./linear-family-definitions.js";
import {
  type LinearFamilyCategory,
  linearFamilySpec,
} from "./linear-family-spec.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

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
  const bindsReplayContext = requires.includes("replayContext");
  return defineFamilyApplication({
    category,
    roster,
    requires,
    bindConfig: ({ infrastructure, references }): WidenedLinearConfig =>
      Object.freeze({
        manifest: infrastructure.manifest,
        blueprintJson: infrastructure.blueprintJson,
        deploymentInfo: infrastructure.deploymentInfo,
        headerHash: infrastructure.headerHash,
        lucid: infrastructure.lucid,
        signer: infrastructure.signer,
        source: infrastructure.source,
        stateQueueMutationLeaseCoordinator:
          infrastructure.stateQueueMutationLeaseCoordinator,
        ...(bindsReplayContext && infrastructure.replayContext !== undefined
          ? { replayContext: infrastructure.replayContext }
          : {}),
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
    ...Object.fromEntries(
      DOUBLE_SPEND_STEP_CONTRACT_NAMES.map((contractName, index) => [
        familyStepRole(index + 1),
        contractName,
      ]),
    ),
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  }),
  requires: [],
  bindConfig: ({ infrastructure, references }) =>
    Object.freeze({
      manifest: infrastructure.manifest,
      blueprintJson: infrastructure.blueprintJson,
      deploymentInfo: infrastructure.deploymentInfo,
      headerHash: infrastructure.headerHash,
      lucid: infrastructure.lucid,
      signer: infrastructure.signer,
      source: infrastructure.source,
      stateQueueMutationLeaseCoordinator:
        infrastructure.stateQueueMutationLeaseCoordinator,
      referenceScripts: Object.freeze({
        steps: Object.freeze([
          requiredReference(references, familyStepRole(1)),
          requiredReference(references, familyStepRole(2)),
          requiredReference(references, familyStepRole(3)),
          requiredReference(references, familyStepRole(4)),
        ] as const),
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
 * Catalogue categories that have no application record yet. Every entry is
 * removed as its family's record lands; the list reaching empty is what makes
 * the registry the complete installed set.
 */
export const NOT_YET_REGISTERED_FAMILY_CATEGORIES = Object.freeze([
  "transitionTrace",
  "validationTraceDispute",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawalMistag",
  "crossBlockDuplicateEvent",
  "valueNotPreserved",
  "mintAuthorization",
  "networkId",
  "missingNativeScriptUtxo",
  "nativeScriptInvalid",
  "minAda",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "witnessScriptDecoding",
  "scriptIntegrityHashMissing",
  "transactionOutputNonCanonical",
  "resolvedOutputNonCanonical",
  "mintDeclaredAssetLimit",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "outputReferenceScriptDecoding",
  "executionSourceScriptDecoding",
  "receivePurposeLanguage",
  "unusedScriptWitness",
  "missingScriptSource",
  "missingRedeemer",
  "unusedRedeemer",
  "executionNativeScriptInvalid",
  "scriptIntegrityHashMismatch",
  "distinctAssetAccumulationLimit",
  "mintItemNonCanonical",
] as const satisfies readonly FraudProofCatalogueCategoryName[]);

export type NotYetRegisteredFamilyCategory =
  (typeof NOT_YET_REGISTERED_FAMILY_CATEGORIES)[number];

/** A catalogue category the registry carries a record for. */
export type RegisteredFamilyCategory = Exclude<
  FraudProofCatalogueCategoryName,
  NotYetRegisteredFamilyCategory
>;

export const FAMILY_APPLICATION_REGISTRY = Object.freeze({
  ...LINEAR_FAMILY_APPLICATION_RECORDS,
  doubleSpend: DOUBLE_SPEND_FAMILY_APPLICATION_RECORD,
} satisfies {
  // Completeness only: every remaining catalogue category has exactly one
  // row, keyed by the record's own category. A record's config and workflow
  // types are tied together at its own definition site, where `bindConfig`'s
  // result is what `constructWorkflow` consumes.
  readonly [Category in RegisteredFamilyCategory]: Readonly<{
    category: Category;
  }>;
});

/** Registered categories in catalogue order. */
export const REGISTERED_FAMILY_CATEGORIES = Object.freeze(
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
    (category): category is RegisteredFamilyCategory =>
      category in FAMILY_APPLICATION_REGISTRY,
  ),
);
