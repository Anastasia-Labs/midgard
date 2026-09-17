import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  workflowActuationAuthorizingDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import { SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import {
  createScriptIntegrityHashMissingTransactionPort,
  scriptIntegrityHashMissingFieldRequirement,
  type ScriptIntegrityHashMissingWorkflowReferenceScripts,
} from "./actuator.js";
import { admitScriptIntegrityHashMissingArtifact } from "./artifact.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts.js";
import { ScriptIntegrityStepDatums } from "./schemas.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_WORKFLOW_IDENTITY =
  "script-integrity-hash-missing-production-v1" as const;

export const SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS = Object.freeze({
  steps: [
    "fraudProofScriptIntegrityHashMissing",
    "fraudProofScriptIntegrityHashMissingStep02",
    "fraudProofScriptIntegrityHashMissingStep03",
    "fraudProofScriptIntegrityHashMissingScriptGrammar",
    "fraudProofScriptIntegrityHashMissingScriptScan",
    "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
    "fraudProofScriptIntegrityHashMissingStep04",
  ],
  witnesses: {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  },
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  removal: {
    correctionLockSpend: "correctionLockSpend",
    stateQueueSpend: "stateQueueSpend",
    stateQueueMint: "stateQueueMint",
    stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
    activeOperatorsSpend: "activeOperatorsSpend",
    activeOperatorsMint: "activeOperatorsMint",
    retiredOperatorsSpend: "retiredOperatorsSpend",
    retiredOperatorsMint: "retiredOperatorsMint",
    schedulerSpend: "schedulerSpend",
  },
} as const);

export const SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC: CursorFamilySpec<"scriptIntegrityHashMissing"> &
  Readonly<{ stepCount: 7 }> = Object.freeze({
  category: "scriptIntegrityHashMissing",
  stepCount: 7,
  successors: Object.freeze({
    1: [2] as const,
    2: [3] as const,
    3: [4, 7] as const,
    4: [4, 5] as const,
    5: [5, 6] as const,
    6: [6, 7] as const,
    7: ["proof_token"] as const,
  }),
});

export type { ScriptIntegrityHashMissingWorkflowReferenceScripts } from "./actuator.js";

export type ScriptIntegrityHashMissingRemovalReferenceScripts = Readonly<{
  correctionLockSpend: UTxO;
  stateQueueSpend: UTxO;
  stateQueueMint: UTxO;
  stateQueueFraudRemovalWithdraw: UTxO;
  activeOperatorsSpend: UTxO;
  activeOperatorsMint: UTxO;
  retiredOperatorsSpend: UTxO;
  retiredOperatorsMint: UTxO;
  schedulerSpend: UTxO;
}>;

export type ManifestBoundScriptIntegrityHashMissingWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ScriptIntegrityHashMissingWorkflowReferenceScripts &
    Readonly<{ removal: ScriptIntegrityHashMissingRemovalReferenceScripts }>;
}>;

export type ManifestBoundScriptIntegrityHashMissingWorkflow = Omit<
  ManifestBoundFamilyWorkflow<"scriptIntegrityHashMissing", true, 7>,
  "definition" | "replayer"
> &
  Readonly<{
    transactions: CursorFamilyTransactionPort<"scriptIntegrityHashMissing">;
    decisionDigest: string;
  }>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
type BoundContext = FamilyAssemblyContext<
  "scriptIntegrityHashMissing",
  (typeof WITNESS_ROLES)[number],
  true,
  7
>;
const createTransactionPort = (
  context: BoundContext,
): CursorFamilyTransactionPort<"scriptIntegrityHashMissing"> => {
  const { binding, references } = context;
  const chain = binding.resolvedContracts.contracts.scriptIntegrityHashMissing;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 7 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "scriptIntegrityHashMissing manifest omitted required contracts",
    );
  const contracts: ScriptIntegrityHashMissingContracts = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
  });
  return createScriptIntegrityHashMissingTransactionPort({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references,
    lease: context.stateQueueMutationLeaseCoordinator,
  });
};

export const SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION = defineFamily<
  "scriptIntegrityHashMissing",
  (typeof WITNESS_ROLES)[number],
  true,
  7
>({
  category: "scriptIntegrityHashMissing",
  stepDatumSchemas: ScriptIntegrityStepDatums,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  auxiliaryReferenceScripts:
    SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS.removal,
  replayer: () => SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC,
    stepContractNames: SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS.steps,
    transactionPort: createTransactionPort,
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const { certificate, references } = context;
        const planned = scriptIntegrityHashMissingFieldRequirement({
          actionStage: action.input.stage,
          artifact,
          owner: context.signer.paymentKeyHash,
        });
        if (planned === null) return null;
        const admitted = admitScriptIntegrityHashMissingArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        return {
          planned,
          compactCbor: admitted.evidence.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.evidence.witnessSetCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
});

export const createManifestBoundScriptIntegrityHashMissingWorkflow = async (
  config: ManifestBoundScriptIntegrityHashMissingWorkflowConfig,
): Promise<ManifestBoundScriptIntegrityHashMissingWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("scriptIntegrityHashMissing decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...workflow,
    decisionDigest,
  }) as ManifestBoundScriptIntegrityHashMissingWorkflow;
};

export const executeManifestBoundScriptIntegrityHashMissingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundScriptIntegrityHashMissingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error(
      "scriptIntegrityHashMissing journal actuation permit changed decision digest",
    );
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: "scriptIntegrityHashMissing",
      headerHash: workflow.binding.definition.headerHash,
      journal,
      adapter: workflow.adapter,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["scriptIntegrityHashMissing"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export type LoadedScriptIntegrityHashMissingWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundScriptIntegrityHashMissingWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadScriptIntegrityHashMissingWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedScriptIntegrityHashMissingWorkflow>;

/** Standard central-loader-compatible family runner; no evidence or actuator callbacks. */
export const createScriptIntegrityHashMissingWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadScriptIntegrityHashMissingWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== "scriptIntegrityHashMissing")
        throw new Error(
          `scriptIntegrityHashMissing production runner category mismatch: ${invocation.category}`,
        );
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "scriptIntegrityHashMissing",
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "scriptIntegrityHashMissing",
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function")
        throw new Error(
          "scriptIntegrityHashMissing runtime omitted its transport disposer",
        );
      try {
        if (
          loaded.schemaVersion !==
          "midgard-production-fraud-proof-runtime-config-v1"
        )
          throw new Error(
            "scriptIntegrityHashMissing runtime config has an unsupported schema",
          );
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "scriptIntegrityHashMissing runner requires concrete public retained-DA sources",
          );
        const workflow =
          await createManifestBoundScriptIntegrityHashMissingWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.category !==
            "scriptIntegrityHashMissing" ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "scriptIntegrityHashMissing manifest-bound workflow identity differs from invocation",
          );
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundScriptIntegrityHashMissingWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            }),
        });
      } finally {
        await loaded.close();
      }
    },
  });
