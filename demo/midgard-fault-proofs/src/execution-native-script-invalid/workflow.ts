/** The `executionNativeScriptInvalid` family definition and its manifest-bound
 * runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
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

export type ManifestBoundExecutionNativeScriptInvalidWorkflow =
  ManifestBoundFamilyWorkflow<"executionNativeScriptInvalid", true, 28>;

const WITNESS_ROLES = CHUNKED_VERIFY_WITNESS_ROLES;

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
  runOrResumeManifestBoundFamilyWorkflow;
