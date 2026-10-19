import {
  FraudProofComputationThreadStepDatum,
  WitnessScriptDecodingStep02DatumSchema,
  WitnessScriptDecodingStep03DatumSchema,
  WitnessScriptDecodingStep04DatumSchema,
} from "@al-ft/midgard-sdk";

import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { defineFamily } from "../workflow/family-definition.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import {
  SCRIPT_DECODE_REFERENCE_SCRIPTS,
  scriptDecodeDeploymentFromReferences,
} from "../workflow/script-decode-prerequisite.js";
import {
  type BoundContext,
  createWitnessScriptDecodingRecoveryPorts,
  type ManifestBoundWitnessScriptDecodingWorkflow,
  type ManifestBoundWitnessScriptDecodingWorkflowConfig,
  type RunContext,
  runs,
  WITNESS_ROLES,
} from "./workflow.create-witness-script-decoding-recovery-ports.js";
import {
  WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS,
  WITNESS_SCRIPT_DECODING_WORKFLOW,
  witnessScriptDecodingConfigFromBinding,
} from "./workflow.witness-script-decoding-config-from-binding.js";
import { WITNESS_SCRIPT_DECODING_CURSOR_SPEC } from "./workflow-spec.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundWitnessScriptDecodingWorkflow = async (
  input: ManifestBoundWitnessScriptDecodingWorkflowConfig,
): Promise<ManifestBoundWitnessScriptDecodingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = witnessScriptDecodingConfigFromBinding({
    binding: deployment.binding,
    lucid: deployment.lucid,
    signer: deployment.signer,
    referenceScripts: {
      step01: deployment.references.steps[0],
      step02: deployment.references.steps[1],
      step03: deployment.references.steps[2],
      step04: deployment.references.steps[3],
      witnesses: deployment.references.witnesses,
      fieldPreimageCertificateMint:
        deployment.references.fieldPreimageCertificateMint,
    },
    scriptDecode: scriptDecodeDeploymentFromReferences({
      blueprint: deployment.binding.blueprint,
      network: deployment.binding.network,
      references: deployment.auxiliaryReferences,
    }),
  });
  return Object.freeze({
    deployment,
    workflowVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    config,
    binding: deployment.binding,
    l1: deployment.l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

const runFor = (context: BoundContext) => {
  const existing = runs.get(context);
  if (existing !== undefined) return existing;
  const created = createWitnessScriptDecodingRecoveryPorts(
    context.runtime.workflow,
  );
  runs.set(context, created);
  return created;
};

export const WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily<
  "witnessScriptDecoding",
  (typeof WITNESS_ROLES)[number],
  true,
  4,
  RunContext
>({
  category: "witnessScriptDecoding",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    WitnessScriptDecodingStep02DatumSchema,
    WitnessScriptDecodingStep03DatumSchema,
    WitnessScriptDecodingStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: WITNESS_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step01,
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step02,
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step03,
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step04,
    ],
    transactionPort: (context) => runFor(context).transactions,
  },
  auxiliaryReferenceScripts: SCRIPT_DECODE_REFERENCE_SCRIPTS,
  scriptDecodeRecord: (context, input) =>
    runFor(context).scriptDecodeItemForAction(input),
  fieldCarriage: [
    {
      requirementForAction: (context, input) =>
        runFor(context).requirementForAction(input),
    },
  ],
});

export const createWitnessScriptDecodingRecoveryAdapter = (
  workflow: ManifestBoundWitnessScriptDecodingWorkflow,
) => {
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
    workflow.deployment,
    { workflow },
  );
  return {
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"witnessScriptDecoding">,
  };
};

export const executeManifestBoundWitnessScriptDecodingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundWitnessScriptDecodingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createWitnessScriptDecodingRecoveryAdapter(workflow),
  });
