import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import { SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator-v1.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  workflowActuationDecisionDigest,
} from "../workflow/production-actuation-permit-v1.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/production-adapters-v1.js";
import {
  createCursorFamilyWorkflowAdapter,
  type CursorFamilyTransactionPort,
} from "../workflow/production-cursor-family-adapter-v1.js";
import type { CursorFamilySpec } from "../workflow/production-cursor-family-state-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/production-funding-reservation-permit-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy-v1.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts-v1.js";
import {
  createScriptIntegrityHashMissingTransactionPort,
  scriptIntegrityHashMissingFieldRequirement,
  type ScriptIntegrityHashMissingWorkflowReferenceScripts,
} from "./production-actuator-v1.js";
import { admitScriptIntegrityHashMissingArtifact } from "./production-artifact-v1.js";
import { ScriptIntegrityStepDatums } from "./schemas-v1.js";

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

export const SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC: CursorFamilySpec<"scriptIntegrityHashMissing"> =
  Object.freeze({
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

export type { ScriptIntegrityHashMissingWorkflowReferenceScripts } from "./production-actuator-v1.js";

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

export type ManifestBoundScriptIntegrityHashMissingWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">;
  l1: FraudProofFamilyL1ObservationPort<"scriptIntegrityHashMissing">;
  transactions: CursorFamilyTransactionPort<"scriptIntegrityHashMissing">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  decisionDigest: string;
}>;

export const createManifestBoundScriptIntegrityHashMissingWorkflow = async (
  config: ManifestBoundScriptIntegrityHashMissingWorkflowConfig,
): Promise<ManifestBoundScriptIntegrityHashMissingWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("scriptIntegrityHashMissing decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "scriptIntegrityHashMissing",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: ScriptIntegrityStepDatums,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
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
  const ref = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS.steps.map(
    (name, index) => ref(name, config.referenceScripts.steps[index]!),
  ) as unknown as ScriptIntegrityHashMissingWorkflowReferenceScripts["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(
      SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS.witnesses,
    ).map(([role, name]) => [
      role,
      ref(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  for (const [role, name] of Object.entries(
    SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS.removal,
  ))
    ref(
      name,
      config.referenceScripts.removal[
        role as keyof ScriptIntegrityHashMissingRemovalReferenceScripts
      ],
    );
  const references = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze(witnesses),
    fieldPreimageCertificateMint: ref(
      SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS.fieldPreimageCertificateMint,
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
  });
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined)
    throw new Error(
      "scriptIntegrityHashMissing raw L1 publication authority is unavailable",
    );
  const transactions = createScriptIntegrityHashMissingTransactionPort({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    lease: config.stateQueueMutationLeaseCoordinator,
  });
  let adapter = createCursorFamilyWorkflowAdapter({
    spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withFieldCarriagePrerequisite({
    category: "scriptIntegrityHashMissing",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
      category: "scriptIntegrityHashMissing",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: ({ action, artifact }) => {
        const planned = scriptIntegrityHashMissingFieldRequirement({
          actionStage: action.input.stage,
          artifact,
          owner: config.signer.paymentKeyHash,
        });
        if (planned === null) return null;
        const admitted = admitScriptIntegrityHashMissingArtifact(
          artifact,
          config.signer.paymentKeyHash,
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
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    decisionDigest: config.decisionDigest,
  });
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
  if (workflowActuationDecisionDigest(journal) !== workflow.decisionDigest)
    throw new Error(
      "scriptIntegrityHashMissing journal actuation permit changed decision digest",
    );
  const observation = await workflow.l1.observeHeader({
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
        return await executeManifestBoundScriptIntegrityHashMissingWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
