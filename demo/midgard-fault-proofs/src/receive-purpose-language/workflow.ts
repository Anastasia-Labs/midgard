/** The `receivePurposeLanguage` family definition and its manifest-bound runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { DIRECT_PURPOSE_FIELDS } from "../direct-purpose-family/custody.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type LinearFamilyPrerequisiteInput,
} from "../workflow/family-definition.js";
import {
  type FraudProofFamilyL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
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

export type ManifestBoundReceivePurposeLanguageWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"receivePurposeLanguage">;
  l1: FraudProofFamilyL1ObservationPort<"receivePurposeLanguage">;
  transactions: CursorFamilyTransactionPort<"receivePurposeLanguage">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

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

export const runOrResumeManifestBoundReceivePurposeLanguageWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundReceivePurposeLanguageWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
    replayContext: workflow.replayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["receivePurposeLanguage"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
