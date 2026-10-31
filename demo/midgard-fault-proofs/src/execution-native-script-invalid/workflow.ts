/** The `executionNativeScriptInvalid` family definition and its manifest-bound
 * runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
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
import { EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS } from "./custody.js";
import {
  executionNativeScriptInvalidSourceInclusion,
  planExecutionNativeScriptInvalidArtifact,
} from "./workflow.plan.js";
import {
  createExecutionNativeScriptInvalidTransactionPort,
  executionNativeScriptInvalidRoleAt,
  executionNativeScriptInvalidWorkflowContracts,
  type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
} from "./workflow.transaction-port.js";

export type ManifestBoundExecutionNativeScriptInvalidWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"executionNativeScriptInvalid">;
  l1: FraudProofFamilyL1ObservationPort<"executionNativeScriptInvalid">;
  transactions: CursorFamilyTransactionPort<"executionNativeScriptInvalid">;
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
  "executionNativeScriptInvalid",
  (typeof WITNESS_ROLES)[number],
  true,
  28
>;

/** The custody role an action spends; `null` for init, removal, step-01 and
 * the terminal, which open no committed field. */
const custodyRole = (input: LinearFamilyPrerequisiteInput) => {
  const stage = cursorFamilyActionInput({
    category: "executionNativeScriptInvalid",
    action: input.action,
  }).stage;
  const step = parseComputationThreadStage(stage);
  if (step === undefined) return null;
  const role = executionNativeScriptInvalidRoleAt(step);
  return role === "step01" || role === "terminal" ? null : role;
};

const planned = (context: Context, input: LinearFamilyPrerequisiteInput) =>
  planExecutionNativeScriptInvalidArtifact({
    artifact: input.artifact,
    contracts: executionNativeScriptInvalidWorkflowContracts(
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

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION = defineFamily({
  category: "executionNativeScriptInvalid",
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
  replayer: () => EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofExecutionNativeScriptInvalid",
      "fraudProofExecutionNativeScriptInvalidBindEvent",
      "fraudProofExecutionNativeScriptInvalidBindPreState",
      "fraudProofExecutionNativeScriptInvalidSourceStart",
      "fraudProofExecutionNativeScriptInvalidSourceResolved",
      "fraudProofExecutionNativeScriptInvalidSourceAuthenticate",
      "fraudProofExecutionNativeScriptInvalidSourceMap",
      "fraudProofExecutionNativeScriptInvalidSourceInputFinish",
      "fraudProofExecutionNativeScriptInvalidSourceGrammar",
      "fraudProofExecutionNativeScriptInvalidSourceWitnesses",
      "fraudProofExecutionNativeScriptInvalidSourceFinish",
      "fraudProofExecutionNativeScriptInvalidFamilyEntry",
      "fraudProofExecutionNativeScriptInvalidPurposeSelect",
      "fraudProofExecutionNativeScriptInvalidSpendSelect",
      "fraudProofExecutionNativeScriptInvalidSpendScan",
      "fraudProofExecutionNativeScriptInvalidMintGrammar",
      "fraudProofExecutionNativeScriptInvalidMintScan",
      "fraudProofExecutionNativeScriptInvalidReceiveStart",
      "fraudProofExecutionNativeScriptInvalidReceiveGrammar",
      "fraudProofExecutionNativeScriptInvalidReceiveOutputs",
      "fraudProofExecutionNativeScriptInvalidReceiveFinish",
      "fraudProofExecutionNativeScriptInvalidReceiveDeclaration",
      "fraudProofExecutionNativeScriptInvalidSourceLookup",
      "fraudProofExecutionNativeScriptInvalidSigners",
      "fraudProofExecutionNativeScriptInvalidEvaluate",
      "fraudProofExecutionNativeScriptInvalidSpendKey",
      "fraudProofExecutionNativeScriptInvalidSpendRank",
      "fraudProofExecutionNativeScriptInvalidTerminal",
    ],
    transactionPort: (context) =>
      createExecutionNativeScriptInvalidTransactionPort({
        ...context,
        contracts: executionNativeScriptInvalidWorkflowContracts(
          context.binding,
          context.certificate,
        ),
      }),
  },
  fieldCarriage: EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS.map(fieldCarriage),
  proofChunk: async (context, input) => {
    if (input.action.input.stage !== "step_01") return null;
    const { admitted } = await planned(context, input);
    return admitted.coordinate.sourceKind === 0
      ? (await executionNativeScriptInvalidSourceInclusion(admitted))
          .txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundExecutionNativeScriptInvalidWorkflow = (
  config: ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
): Promise<ManifestBoundExecutionNativeScriptInvalidWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }): Promise<FraudProofWorkflowRunResult> =>
    await runFraudProofWorkflowFromRetainedDa({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      observation: await observeFraudProofWorkflowHeader(workflow.l1, {
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
      replayer: EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
      replayContext: workflow.replayContext,
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflow.adapter],
        launchScope: ["executionNativeScriptInvalid"],
      }),
      journal,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
