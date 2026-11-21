import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";

import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import {
  cursorStepContractNames,
  defineFamily,
} from "../workflow/family-definition.family-definition.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import {
  executeManifestBoundFamilyRecovery,
  type ManifestBoundFamilyWorkflowWithRecovery,
  withManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { type WalletOnlySend } from "../workflow/wallet-only-send-gate.js";
import {
  TransactionOutputStep02DatumSchema,
  TransactionOutputStep03DatumDatumSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep04DatumSchema,
} from "./schemas.js";
import {
  type ManifestBoundTransactionOutputNonCanonicalInstallation,
  type ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
  type RunContext,
  runFor,
  WITNESS_ROLES,
} from "./workflow.create-transaction-output-non-canonical-recovery-ports.js";
import {
  TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
  transactionOutputNonCanonicalConfigFromBinding,
} from "./workflow.transaction-output-non-canonical-config-from-binding.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "./workflow-spec.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundTransactionOutputNonCanonicalWorkflow = async (
  input: ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
): Promise<ManifestBoundTransactionOutputNonCanonicalWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step03Datum,
          input.referenceScripts.step04,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
        fieldGrammarCertificateSpend:
          input.referenceScripts.fieldGrammarCertificateSpend,
      },
    },
  );
  const config = transactionOutputNonCanonicalConfigFromBinding({
    binding: deployment.binding,
    lucid: deployment.lucid,
    signer: deployment.signer,
    referenceScripts: {
      step01: deployment.references.steps[0],
      step02: deployment.references.steps[1],
      step03: deployment.references.steps[2],
      step03Datum: deployment.references.steps[3],
      step04: deployment.references.steps[4],
      witnesses: deployment.references.witnesses,
      fieldPreimageCertificateMint:
        deployment.references.fieldPreimageCertificateMint,
      fieldGrammarCertificateSpend:
        input.referenceScripts.fieldGrammarCertificateSpend,
    },
  });
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
      config,
      binding: deployment.binding,
      l1: deployment.l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
    }),
    createTransactionOutputNonCanonicalRecoveryAdapter,
  );
};

export const TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION = defineFamily<
  "transactionOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  5,
  RunContext
>({
  category: "transactionOutputNonCanonical",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    TransactionOutputStep02DatumSchema,
    TransactionOutputStep03DatumSchema,
    TransactionOutputStep03DatumDatumSchema,
    TransactionOutputStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  fieldGrammarCertificate: true,
  replayer: () => TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
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

export const createTransactionOutputNonCanonicalRecoveryAdapter = (
  workflow: ManifestBoundTransactionOutputNonCanonicalInstallation,
) => {
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    workflow.deployment,
    { workflow },
  );
  return {
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"transactionOutputNonCanonical">,
  };
};

export type ManifestBoundTransactionOutputNonCanonicalWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundTransactionOutputNonCanonicalInstallation,
    ReturnType<typeof createTransactionOutputNonCanonicalRecoveryAdapter>
  >;

export const executeManifestBoundTransactionOutputNonCanonicalWorkflow =
  async ({
    workflow,
    sources,
    journal,
    walletOnlySend,
  }: {
    readonly workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow;
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
