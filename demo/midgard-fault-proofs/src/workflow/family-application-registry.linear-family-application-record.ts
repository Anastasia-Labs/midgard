import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
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
  type FamilyCommonInfrastructure,
  familyDefinitionRoster,
  familyStepRole,
  type LinearFamilyApplicationRecord,
} from "./family-application.js";
import {
  LINEAR_FAMILY_REQUIREMENTS,
  optionalReplayContext,
  requiredReference,
  resolvedSteps,
  stepRoster,
  type WidenedLinearConfig,
  type WidenedLinearWorkflow,
} from "./family-application-registry.resolve-roster-parts.js";
import {
  type FamilyCategory,
  type FamilyDefinition,
  type FaultProofWitnessRole,
} from "./family-definition.js";
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
export const LINEAR_FAMILY_APPLICATION_RECORDS = Object.freeze(
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
export type DecisionDigestCursorFamilyConfig = Readonly<{
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
 * The common infrastructure every family's manifest-bound config carries
 * verbatim: the parts that are not a reference script or the decision digest.
 */
export const commonBoundConfigFields = (
  infrastructure: FamilyCommonInfrastructure,
) =>
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
export const requiredDecisionDigest = (
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
