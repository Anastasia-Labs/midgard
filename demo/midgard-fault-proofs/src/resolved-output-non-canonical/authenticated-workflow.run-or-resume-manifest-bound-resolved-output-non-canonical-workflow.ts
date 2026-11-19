import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import {
  cursorStepContractNames,
  defineFamily,
  type FamilyAssemblyContext,
} from "../workflow/family-definition.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  executeManifestBoundFamilyRecovery,
  type ManifestBoundFamilyWorkflowWithRecovery,
  withManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { type WalletOnlySend } from "../workflow/wallet-only-send-gate.js";
import {
  createResolvedOutputNonCanonicalRecoveryPorts,
  type ManifestBoundResolvedOutputNonCanonicalInstallation,
  type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
  type RunContext,
  WITNESS_ROLES,
} from "./authenticated-workflow.create-resolved-output-non-canonical-recovery-ports.js";
import {
  RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
  resolvedOutputNonCanonicalConfigFromBinding,
} from "./authenticated-workflow.resolved-output-non-canonical-config-from-binding.js";
import {
  ResolvedOutputStep02DatumSchema,
  ResolvedOutputStep03DatumSchema,
  ResolvedOutputStep04DatumDatumSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep05DatumSchema,
} from "./schemas.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "./workflow-spec.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundResolvedOutputNonCanonicalWorkflow = async (
  input: ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
): Promise<ManifestBoundResolvedOutputNonCanonicalWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
          input.referenceScripts.step04Datum,
          input.referenceScripts.step05,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = resolvedOutputNonCanonicalConfigFromBinding({
    binding: deployment.binding,
    lucid: deployment.lucid,
    signer: deployment.signer,
    referenceScripts: {
      step01: deployment.references.steps[0],
      step02: deployment.references.steps[1],
      step03: deployment.references.steps[2],
      step04: deployment.references.steps[3],
      step04Datum: deployment.references.steps[4],
      step05: deployment.references.steps[5],
      witnesses: deployment.references.witnesses,
      fieldPreimageCertificateMint:
        deployment.references.fieldPreimageCertificateMint,
    },
  });
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
      config,
      binding: deployment.binding,
      l1: deployment.l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
      ...(input.replayContext === undefined
        ? {}
        : { replayContext: input.replayContext }),
    }),
    createResolvedOutputNonCanonicalRecoveryAdapter,
  );
};

type BoundContext = FamilyAssemblyContext<
  "resolvedOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  6,
  RunContext
>;

const runs = new WeakMap<
  BoundContext,
  ReturnType<typeof createResolvedOutputNonCanonicalRecoveryPorts>
>();

const runFor = (context: BoundContext) => {
  const existing = runs.get(context);
  if (existing !== undefined) return existing;
  const created = createResolvedOutputNonCanonicalRecoveryPorts(
    context.runtime.workflow,
  );
  runs.set(context, created);
  return created;
};

export const RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION = defineFamily<
  "resolvedOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  6,
  RunContext
>({
  category: "resolvedOutputNonCanonical",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    ResolvedOutputStep02DatumSchema,
    ResolvedOutputStep03DatumSchema,
    ResolvedOutputStep04DatumSchema,
    ResolvedOutputStep04DatumDatumSchema,
    ResolvedOutputStep05DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    ),
    transactionPort: (context) => runFor(context).transactions,
  },
  fieldCarriage: [
    {
      requirementForAction: (context, input) =>
        runFor(context).requirementForAction(input),
    },
  ],
});

export const createResolvedOutputNonCanonicalRecoveryAdapter = (
  workflow: ManifestBoundResolvedOutputNonCanonicalInstallation,
) => {
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    workflow.deployment,
    { workflow },
  );
  return {
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"resolvedOutputNonCanonical">,
  };
};

export type ManifestBoundResolvedOutputNonCanonicalWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundResolvedOutputNonCanonicalInstallation,
    ReturnType<typeof createResolvedOutputNonCanonicalRecoveryAdapter>
  >;

export const executeManifestBoundResolvedOutputNonCanonicalWorkflow = async ({
  workflow,
  sources,
  journal,
  walletOnlySend,
}: {
  readonly workflow: ManifestBoundResolvedOutputNonCanonicalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
  readonly walletOnlySend: WalletOnlySend;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    walletOnlySend,
  });
