import {
  FraudProofComputationThreadStepDatum,
  MintAuthorizationEvaluateDatum,
  MintAuthorizationStep02ThreadDatum,
  MintAuthorizationStep03Datum,
  MintAuthorizationStep04Datum,
  MintAuthorizationStep05Datum,
  MintAuthorizationWitnessScanDatum,
} from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { MINT_AUTHORIZATION_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { defineFamily } from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { admitMintAuthorizationWorkflowArtifact } from "./artifact.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import {
  createMintAuthorizationTransactionPort,
  type ManifestBoundMintAuthorizationWorkflow,
  type ManifestBoundMintAuthorizationWorkflowConfig,
} from "./workflow.create-mint-authorization-transaction-port.js";
import {
  mintAuthorizationWorkflowFieldRequirement,
  mintAuthorizationWorkflowRawRequirement,
} from "./workflow.mint-authorization-workflow-raw-requirement.js";

export const MINT_AUTHORIZATION_FAMILY_DEFINITION = defineFamily({
  category: "mintAuthorization",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MintAuthorizationStep02ThreadDatum,
    MintAuthorizationStep03Datum,
    MintAuthorizationStep04Datum,
    MintAuthorizationStep05Datum,
    MintAuthorizationEvaluateDatum,
    MintAuthorizationWitnessScanDatum,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: MINT_AUTHORIZATION_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofMintAuthorization",
      "fraudProofMintAuthorizationStep02",
      "fraudProofMintAuthorizationStep03",
      "fraudProofMintAuthorizationStep04",
      "fraudProofMintAuthorizationStep05",
      "fraudProofMintAuthorizationStep06",
      "fraudProofMintAuthorizationStep07",
    ],
    transactionPort: (context) => {
      const { binding, certificate } = context;
      const chain = binding.resolvedContracts.contracts.mintAuthorization;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (
        chain === undefined ||
        stateQueuePolicyId === undefined ||
        certificate === null
      ) {
        throw new Error(
          "mint-authorization manifest omitted required contracts",
        );
      }

      const contracts: MintAuthorizationContracts = Object.freeze({
        steps: chain.steps,
        computationThread:
          binding.resolvedContracts.contracts.computationThread,
        fraudProof: {
          policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
          mintingScript:
            binding.resolvedContracts.contracts.fraudProof.mintingScript,
          spendingScriptAddress:
            binding.resolvedContracts.contracts.fraudProof
              .spendingScriptAddress,
        },
        hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
        stateQueuePolicyId,
        fieldPreimageCertificatePolicyId: certificate.policyId,
      });

      return createMintAuthorizationTransactionPort(
        { ...context, contracts },
        context.fieldCarriagePrerequisites[1]!,
      );
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        if (
          typeof action.input.stage !== "string" ||
          !["step_02", "step_03", "step_04"].includes(action.input.stage)
        )
          return null;
        const input = cursorFamilyActionInput({
          category: "mintAuthorization",
          action,
        });
        const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
        return mintAuthorizationWorkflowFieldRequirement(
          admitted,
          context.signer.paymentKeyHash,
          input.stage,
          {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        );
      },
    },
    {
      rawDatum: true,
      requirementForAction: async (_context, { action, artifact }) => {
        if (
          typeof action.input.stage !== "string" ||
          !["step_02", "step_03", "step_06", "step_07"].includes(
            action.input.stage,
          )
        )
          return null;
        return mintAuthorizationWorkflowRawRequirement(
          await admitMintAuthorizationWorkflowArtifact(artifact),
          action.input.stage,
        );
      },
    },
  ],
  proofChunk: async (_context, { action, artifact }) => {
    if (action.input.stage !== "step_01") return null;
    const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
    return admitted.txInclusion.txMembershipProofCbor ?? null;
  },
});

export const createManifestBoundMintAuthorizationWorkflow = (
  config: ManifestBoundMintAuthorizationWorkflowConfig,
): Promise<ManifestBoundMintAuthorizationWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    MINT_AUTHORIZATION_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundMintAuthorizationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintAuthorizationWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY,
    replayContext: workflow.replayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["mintAuthorization"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
