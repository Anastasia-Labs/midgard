/** The `receivePurposeLanguage` family definition and its manifest-bound runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { DIRECT_PURPOSE_FIELDS } from "../direct-purpose-family/custody.js";
import { RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import {
  CHUNKED_VERIFY_WITNESS_ROLES,
  defineFamily,
  type FamilyAssemblyContext,
  type LinearFamilyPrerequisiteInput,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  planReceivePurposeLanguageArtifact,
  receivePurposeLanguageSourceInclusion,
} from "./workflow.plan.js";
import {
  createReceivePurposeLanguageTransactionPort,
  type ManifestBoundReceivePurposeLanguageWorkflowConfig,
  receivePurposeLanguageRoleAt,
  receivePurposeLanguageWorkflowContracts,
} from "./workflow.transaction-port.js";

export type ManifestBoundReceivePurposeLanguageWorkflow =
  ManifestBoundFamilyWorkflow<"receivePurposeLanguage", true, 21>;

const WITNESS_ROLES = CHUNKED_VERIFY_WITNESS_ROLES;

type Context = FamilyAssemblyContext<
  "receivePurposeLanguage",
  (typeof WITNESS_ROLES)[number],
  true,
  21
>;

/** The custody role an action spends; `null` for init, removal, step-01 and
 * the terminal, which open no committed field. */
const custodyRole = (input: LinearFamilyPrerequisiteInput) => {
  const stage = cursorFamilyActionInput({
    category: "receivePurposeLanguage",
    action: input.action,
  }).stage;
  const step = parseComputationThreadStage(stage);
  if (step === undefined) return null;
  const role = receivePurposeLanguageRoleAt(step);
  return role === "step01" || role === "terminal" ? null : role;
};

const planned = (context: Context, input: LinearFamilyPrerequisiteInput) =>
  planReceivePurposeLanguageArtifact({
    artifact: input.artifact,
    contracts: receivePurposeLanguageWorkflowContracts(
      context.binding,
      context.certificate,
    ),
    prover: context.signer.paymentKeyHash,
  });

/** The carriage of `field` ahead of any action whose role opens it. */
const fieldCarriage = (field: number) => ({
  requirementForAction: async (
    context: Context,
    input: LinearFamilyPrerequisiteInput,
  ) => {
    const role = custodyRole(input);
    if (role === null) return null;
    const plan = await planned(context, input);
    if (!plan.fieldsOf(role).includes(field)) return null;
    const { material } = plan.admitted;
    return {
      planned: plan.proof.plan(field).openingPlan,
      compactCbor: material.bound.compact_cbor,
      witnessSetCompactCbor:
        material.material.proofSource.witnessSetCompactCbor.toString("hex"),
      certificate: {
        policyId: context.certificate.policyId,
        mintingScript: context.certificate.mintingScript,
        referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
      },
    } satisfies FieldCarriageRequirement;
  },
});

const S = FraudProofComputationThreadStepDatum;

export const RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION = defineFamily({
  category: "receivePurposeLanguage",
  stepDatumSchemas: [
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
    S,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofReceivePurposeLanguage",
      "fraudProofReceivePurposeLanguageBindEvent",
      "fraudProofReceivePurposeLanguageBindPreState",
      "fraudProofReceivePurposeLanguageSourceStart",
      "fraudProofReceivePurposeLanguageSourceResolved",
      "fraudProofReceivePurposeLanguageSourceAuthenticate",
      "fraudProofReceivePurposeLanguageSourceMap",
      "fraudProofReceivePurposeLanguageSourceInputFinish",
      "fraudProofReceivePurposeLanguageSourceGrammar",
      "fraudProofReceivePurposeLanguageSourceWitnesses",
      "fraudProofReceivePurposeLanguageSourceFinish",
      "fraudProofReceivePurposeLanguageFamilyEntry",
      "fraudProofReceivePurposeLanguageReceiveStart",
      "fraudProofReceivePurposeLanguageReceiveGrammar",
      "fraudProofReceivePurposeLanguageReceiveOutputs",
      "fraudProofReceivePurposeLanguageReceiveFinish",
      "fraudProofReceivePurposeLanguageReceiveDeclaration",
      "fraudProofReceivePurposeLanguageSourceLookup",
      "fraudProofReceivePurposeLanguageClaimedRedeemerGrammar",
      "fraudProofReceivePurposeLanguageClaimedRedeemerScan",
      "fraudProofReceivePurposeLanguageTerminal",
    ],
    transactionPort: (context) =>
      createReceivePurposeLanguageTransactionPort({
        ...context,
        contracts: receivePurposeLanguageWorkflowContracts(
          context.binding,
          context.certificate,
        ),
      }),
  },
  fieldCarriage: DIRECT_PURPOSE_FIELDS.map(fieldCarriage),
  proofChunk: async (context, input) => {
    if (input.action.input.stage !== "step_01") return null;
    const { admitted } = await planned(context, input);
    return admitted.coordinate.sourceKind === 0
      ? (await receivePurposeLanguageSourceInclusion(admitted))
          .txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundReceivePurposeLanguageWorkflow = (
  config: ManifestBoundReceivePurposeLanguageWorkflowConfig,
): Promise<ManifestBoundReceivePurposeLanguageWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundReceivePurposeLanguageWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
