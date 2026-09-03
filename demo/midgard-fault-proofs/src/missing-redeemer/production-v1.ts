import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPort } from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import { bindWorkflowActuationJournal } from "../workflow/production-actuation-permit-v1.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/production-adapters-v1.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/production-funding-reservation-permit-v1.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary-v1.js";
import {
  MISSING_REDEEMER_BLUEPRINT_TITLES,
  type MissingRedeemerContracts,
} from "./contracts-v1.js";
import { createMissingRedeemerDirectoryJournal } from "./directory-journal-v1.js";
import {
  MISSING_REDEEMER_CATEGORY,
  missingRedeemerEvidenceIdentity,
} from "./family-v1.js";
import {
  createMissingRedeemerActuator,
  type MissingRedeemerActuatorAction,
  type MissingRedeemerWorkflowReferences,
} from "./production-actuator-v1.js";
import {
  type MissingRedeemerArtifact,
  replayMissingRedeemer,
} from "./production-replay-v1.js";
import {
  MissingRedeemerStep02aDatumSchema,
  MissingRedeemerStep02bDatumSchema,
  MissingRedeemerStep02DatumSchema,
  MissingRedeemerStep03DatumSchema,
  MissingRedeemerStep04DatumSchema,
  MissingRedeemerStep05DatumSchema,
} from "./schemas-v1.js";
import { planMissingRedeemerStagedWalk } from "./staged-plan-v1.js";
import type { MissingRedeemerDurableState } from "./workflow-v1.js";
import { runMissingRedeemerWorkflow } from "./workflow-v1.js";

export const MISSING_REDEEMER_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "stateQueueMutationLeaseCoordinator",
  "referenceScripts",
] as const);

export const MISSING_REDEEMER_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  MissingRedeemerStep02DatumSchema,
  MissingRedeemerStep02aDatumSchema,
  MissingRedeemerStep02bDatumSchema,
  MissingRedeemerStep03DatumSchema,
  MissingRedeemerStep04DatumSchema,
  MissingRedeemerStep05DatumSchema,
] as const);

export type MissingRedeemerRemovalReferenceScripts = Readonly<{
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

export type MissingRedeemerWorkflowReferenceScripts = Readonly<{
  steps: MissingRedeemerWorkflowReferences["steps"];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: MissingRedeemerRemovalReferenceScripts;
}>;

export type ManifestBoundMissingRedeemerWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MissingRedeemerWorkflowReferenceScripts;
}>;

export type ManifestBoundMissingRedeemerWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingRedeemer">;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  actuator: ReturnType<typeof createMissingRedeemerActuator>;
}>;

const manifestContracts = Object.freeze({
  steps: [
    "fraudProofMissingRedeemer",
    "fraudProofMissingRedeemerStep02",
    "fraudProofMissingRedeemerStep02a",
    "fraudProofMissingRedeemerStep02b",
    "fraudProofMissingRedeemerStep03",
    "fraudProofMissingRedeemerStep04",
    "fraudProofMissingRedeemerStep05",
  ],
  witnesses: {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  },
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

/** Strict manifest-bound construction; proof evidence is never configurable. */
export const createManifestBoundMissingRedeemerWorkflow = async (
  config: ManifestBoundMissingRedeemerWorkflowConfig,
): Promise<ManifestBoundMissingRedeemerWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...MISSING_REDEEMER_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "missingRedeemer production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("missingRedeemer decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: MISSING_REDEEMER_CATEGORY,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: MISSING_REDEEMER_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.missingRedeemer;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 7 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("missingRedeemer manifest omitted required contracts");
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = manifestContracts.steps.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as MissingRedeemerWorkflowReferences["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(manifestContracts.witnesses).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  for (const [role, name] of Object.entries(manifestContracts.removal))
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof MissingRedeemerRemovalReferenceScripts
      ],
    );
  bind(
    "fieldPreimageCertificateMint",
    config.referenceScripts.fieldPreimageCertificateMint,
  );
  const contracts: MissingRedeemerContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: MISSING_REDEEMER_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MissingRedeemerContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    decisionDigest: config.decisionDigest,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    actuator: createMissingRedeemerActuator({
      binding,
      lucid: config.lucid,
      signer: config.signer,
      contracts,
      references: { steps, witnesses },
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
  });
};

export type LoadedMissingRedeemerWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMissingRedeemerWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMissingRedeemerWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedMissingRedeemerWorkflow>;

const appendEvent = async (
  journal: FraudProofWorkflowJournalStore,
  workflowId: string,
  identity: FraudProofWorkflowIdentity,
  event: FraudProofWorkflowJournalEvent,
) => {
  const sequence = (await journal.load(workflowId)).length;
  await journal.append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence,
      recordedAt: new Date().toISOString(),
      event,
    },
    sequence,
  );
};

const countConfirmedActions = (
  entries: readonly Readonly<{ event: FraudProofWorkflowJournalEvent }>[],
  prefix: string,
) =>
  entries.filter(
    ({ event }) =>
      event.kind === "confirmed" && event.actionId.startsWith(prefix),
  ).length;

const actionFor = async ({
  workflow,
  artifact,
  entries,
}: {
  workflow: ManifestBoundMissingRedeemerWorkflow;
  artifact: MissingRedeemerArtifact;
  entries: readonly Readonly<{ event: FraudProofWorkflowJournalEvent }>[];
}): Promise<
  | Readonly<{ action: MissingRedeemerActuatorAction; actionId: string }>
  | "removed"
> => {
  const stage = (
    await workflow.l1.observe({
      headerHash: workflow.binding.definition.headerHash,
    })
  ).stage;
  if (stage.kind === "not_started")
    return {
      action: {
        stage: "init",
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      },
      actionId: "missingRedeemer:init",
    };
  if (stage.kind === "proof_token")
    return {
      action: {
        stage: "remove",
        nextRemovalOutRef: stage.nextRemovalOutRef,
        fraudProofOutRef: stage.fraudProofOutRef,
      },
      actionId: "missingRedeemer:remove",
    };
  if (stage.kind === "removed") return "removed";
  const threadOutRef = stage.threadOutRef;
  const fixed = (["step_01", "step_02", "step_02a", "step_02b"] as const)[
    stage.step - 1
  ];
  if (fixed !== undefined)
    return {
      action:
        fixed === "step_01"
          ? {
              stage: fixed,
              threadOutRef,
              stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
            }
          : { stage: fixed, threadOutRef },
      actionId: `missingRedeemer:${fixed}`,
    };
  if (stage.step === 5) {
    const count = countConfirmedActions(entries, "missingRedeemer:field:");
    if (artifact.evidence.carriage !== "Certified")
      return {
        action: { stage: "field", threadOutRef, action: { kind: "direct" } },
        actionId: "missingRedeemer:field:direct",
      };
    const staged = planMissingRedeemerStagedWalk({
      transactionId: artifact.evidence.subject.transaction_id,
      fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
    });
    if (count > staged.grammar.length)
      throw new Error(
        "missingRedeemer grammar journal passed terminal checkpoint",
      );
    const fieldAction =
      count === 0
        ? ({ kind: "grammar_start" } as const)
        : count === staged.grammar.length
          ? ({ kind: "grammar_finish" } as const)
          : ({ kind: "grammar_resume", ordinal: count } as const);
    return {
      action: { stage: "field", threadOutRef, action: fieldAction },
      actionId: `missingRedeemer:field:${count.toString()}`,
    };
  }
  if (stage.step === 6) {
    const count = countConfirmedActions(entries, "missingRedeemer:scan:");
    return {
      action: { stage: "scan", threadOutRef },
      actionId: `missingRedeemer:scan:${count.toString()}`,
    };
  }
  if (stage.step === 7)
    return {
      action: { stage: "finalize", threadOutRef },
      actionId: "missingRedeemer:finalize",
    };
  throw new Error("missingRedeemer observed impossible step");
};

/** One retained-DA-derived, locally evaluated, intent-journaled action. */
export const executeManifestBoundMissingRedeemerWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundMissingRedeemerWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const candidate = (await replayMissingRedeemer(block))[0];
  if (candidate === undefined)
    throw new Error(
      "missingRedeemer complete replay found no canonical violation",
    );
  const artifact = candidate.artifact;
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: MISSING_REDEEMER_CATEGORY,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendEvent(journal, workflowId, identity, {
      kind: "started",
    });
    entries = await journal.load(workflowId);
  }
  const pending = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent");
  const intent =
    pending?.event.kind === "submission_intent" ? pending.event : undefined;
  if (
    intent !== undefined &&
    !entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.actionId === intent.actionId,
    )
  ) {
    if (
      !(await workflow.l1.transactionConfirmed({
        headerHash,
        txHash: intent.txHash,
      }))
    )
      return { kind: "pending" as const, workflowId, txHash: intent.txHash };
    await appendEvent(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
    entries = await journal.load(workflowId);
  }
  const selected = await actionFor({ workflow, artifact, entries });
  if (selected === "removed") return { kind: "completed" as const, workflowId };
  const captured = await workflow.actuator.capture({
    action: selected.action,
    artifact,
  });
  await appendEvent(journal, workflowId, identity, {
    kind: "preflight_passed",
    actionId: selected.actionId,
    txHash: captured.transaction.txHash,
    localEvaluator: "lucid-evolution-local-uplc-v1",
    referenceScripts: captured.transaction.referenceScripts,
  });
  await appendEvent(journal, workflowId, identity, {
    kind: "submission_intent",
    actionId: selected.actionId,
    actionInput: {
      schemaVersion: "midgard-production-cursor-family-action-v1",
      category: MISSING_REDEEMER_CATEGORY,
      stage: selected.action.stage,
    },
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("missingRedeemer provider substituted transaction");
  await appendEvent(journal, workflowId, identity, {
    kind: "submitted",
    actionId: selected.actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

export const runOrResumeManifestBoundMissingRedeemerWorkflow = async (input: {
  workflow: ManifestBoundMissingRedeemerWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
    throw new Error("missingRedeemer runner rejects caller-authored evidence");
  return await executeManifestBoundMissingRedeemerWorkflow(input);
};

/** Loader-compatible surface; central admission remains fixed-category only. */
export const createMissingRedeemerWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadMissingRedeemerWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== MISSING_REDEEMER_CATEGORY)
        throw new Error("missingRedeemer runner category changed");
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "missingRedeemer requires concrete public retained DA",
          );
        const workflow = await createManifestBoundMissingRedeemerWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error("missingRedeemer runtime binding changed invocation");
        const journal = bindWorkflowFundingReservationJournal({
          permit: invocation.fundingReservationPermit,
          journal: bindWorkflowActuationJournal({
            journal: new DirectoryFraudProofWorkflowJournalStore(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: MISSING_REDEEMER_CATEGORY,
            headerHash: invocation.headerHash,
          }),
        });
        return await executeManifestBoundMissingRedeemerWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });

export interface MissingRedeemerObservationPort {
  observe(identity: string): Promise<MissingRedeemerDurableState>;
  stateQueueBlockOutRef(headerHash: string): Promise<string>;
  removalOutRefs(
    headerHash: string,
  ): Promise<Readonly<{ nextRemovalOutRef: string; fraudProofOutRef: string }>>;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
export type MissingRedeemerRunnerConfig = Readonly<{
  journalDirectory: string;
  actuator: ReturnType<typeof createMissingRedeemerActuator>;
  observation: MissingRedeemerObservationPort;
}>;

/**
 * Package-owned production runner. Evidence is accepted only as the strict
 * retained-DA artifact emitted by production replay; runtime configuration is
 * limited to durable storage and chain infrastructure.
 */
export const createMissingRedeemerRunner = async (
  config: MissingRedeemerRunnerConfig,
) => {
  const journal = await createMissingRedeemerDirectoryJournal(
    config.journalDirectory,
  );
  return Object.freeze({
    run: async (
      artifact: MissingRedeemerArtifact,
    ): Promise<"removed" | "cancelled"> => {
      const staged = planMissingRedeemerStagedWalk({
        transactionId: artifact.evidence.subject.transaction_id,
        fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
      });
      return await runMissingRedeemerWorkflow({
        evidence: artifact.evidence,
        journal,
        actuator: {
          observe: async (identity) =>
            await config.observation.observe(identity),
          submit: async ({ identity, action, scanCursor }) => {
            const observed = await config.observation.observe(identity);
            let concrete: MissingRedeemerActuatorAction;
            if (action === "init")
              concrete = {
                stage: "init",
                stateQueueBlockOutRef:
                  await config.observation.stateQueueBlockOutRef(
                    artifact.headerHash,
                  ),
              };
            else if (action === "bind")
              concrete = {
                stage: "step_01",
                threadOutRef: observed.outputReference!,
                stateQueueBlockOutRef:
                  await config.observation.stateQueueBlockOutRef(
                    artifact.headerHash,
                  ),
              };
            else if (action === "authenticatePurpose")
              concrete = {
                stage: "step_02",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "authenticateTrace")
              concrete = {
                stage: "step_02a",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "authenticateSelection")
              concrete = {
                stage: "step_02b",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "openRedeemers") {
              const fieldAction =
                artifact.evidence.carriage !== "Certified"
                  ? { kind: "direct" as const }
                  : scanCursor === 0
                    ? { kind: "grammar_start" as const }
                    : scanCursor < staged.grammar.length
                      ? { kind: "grammar_resume" as const, ordinal: scanCursor }
                      : { kind: "grammar_finish" as const };
              concrete = {
                stage: "field",
                threadOutRef: observed.outputReference!,
                action: fieldAction,
              };
            } else if (action === "scan")
              concrete = {
                stage: "scan",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "finalize")
              concrete = {
                stage: "finalize",
                threadOutRef: observed.outputReference!,
              };
            else {
              const refs = await config.observation.removalOutRefs(
                artifact.headerHash,
              );
              concrete = { stage: "remove", ...refs };
            }
            if (
              ("threadOutRef" in concrete && concrete.threadOutRef == null) ||
              observed.stage === "cancelled"
            )
              throw new Error(
                "missingRedeemer observed cursor cannot execute action",
              );
            const captured = await config.actuator.capture({
              action: concrete,
              artifact,
            });
            const submitted = await submitCapturedTransaction(
              captured.transaction,
            );
            if (submitted !== captured.transaction.txHash)
              throw new Error(
                "missingRedeemer provider substituted transaction identity",
              );
            if (!(await config.observation.transactionConfirmed(submitted)))
              throw new Error(
                "missingRedeemer submitted transaction is unresolved",
              );
            const next = await config.observation.observe(identity);
            if (next.txHash !== submitted)
              throw new Error(
                "missingRedeemer confirmed cursor changed transaction identity",
              );
            return next;
          },
        },
      });
    },
    cancel: async (artifact: MissingRedeemerArtifact): Promise<"cancelled"> => {
      const identity = missingRedeemerEvidenceIdentity(artifact.evidence);
      const observed = await config.observation.observe(identity);
      const stepIndex = [
        "step01",
        "step02",
        "step02a",
        "step02b",
        "step03",
        "step04",
        "step05",
      ].indexOf(observed.stage);
      if (stepIndex < 0 || observed.outputReference === null)
        throw new Error("missingRedeemer observed stage cannot cancel");
      const captured = await config.actuator.capture({
        action: {
          stage: "cancel",
          threadOutRef: observed.outputReference,
          stepIndex,
        },
        artifact,
      });
      const submitted = await submitCapturedTransaction(captured.transaction);
      if (
        submitted !== captured.transaction.txHash ||
        !(await config.observation.transactionConfirmed(submitted))
      )
        throw new Error("missingRedeemer cancellation is unresolved");
      const next = await config.observation.observe(identity);
      if (next.stage !== "cancelled" || next.txHash !== submitted)
        throw new Error("missingRedeemer cancellation cursor changed");
      const entries = await journal.load(identity);
      await journal.append(identity, entries.length, next);
      return "cancelled";
    },
  });
};
