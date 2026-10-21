/** The validation-trace dispute bound to one finalized deployment: the
 * adapter, its raw-L1 observation, recovery and removal built from the
 * deployment the host loaded, and the run over retained DA that replays the
 * challenged block's scripts. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  parseContractDeploymentDisputeBondLovelace,
  parseContractDeploymentL1SlotConfig,
} from "../inspect-contracts.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { createValidationTraceDisputeCompleteCanonicalReplay } from "../workflow/complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofDisputeDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type {
  FraudProofL1Source,
  FraudProofSignedTransactionRecovery,
} from "../workflow/l1-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { scriptDecodeDeploymentFromReferences } from "../workflow/script-decode-prerequisite.deployment.js";
import { createValidationTraceDisputeWorkflowAdapter } from "./adapter.js";
import { createValidationTraceCaseHistory } from "./case-history.js";
import {
  validationTraceOneStepReferences,
  validationTraceScriptDecodeReferences,
} from "./one-step-references.js";
import { validationTraceComputationSteps } from "./removal.js";
import { createValidationTraceReplayHolds } from "./replay-hold.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";
import {
  type ValidationTraceAwaitingCounterparty,
  validationTraceRunState,
} from "./waiting.js";

const CATEGORY = "validationTraceDispute" as const;

/** The dispute's published reference scripts, keyed by their roles. */
export const VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS = Object.freeze({
  opener: "validationTraceDispute",
  source: "validationTraceDisputeSource",
  case: "validationTraceDisputeCase",
  bond: "validationTraceDisputeBond",
  game: "validationTraceDisputeGame",
  timeout: "validationTraceDisputeTimeout",
  award: "validationTraceDisputeAward",
} as const);

type DisputeRole = keyof typeof VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS;

export type ManifestBoundValidationTraceDisputeWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  /** The admitted predecessor the challenged block's scripts replay on. */
  replayContext?: CompleteCanonicalReplayContext;
  referenceScripts: Readonly<
    Record<DisputeRole, UTxO> & {
      witnesses: Readonly<{
        computationThreadMint: UTxO;
        fraudProofMint: UTxO;
        phasMembershipWithdraw: UTxO;
      }>;
      /** The one-step resolution's and the script-decode deployment's
       * references, by manifest member
       * (`VALIDATION_TRACE_ONE_STEP_REFERENCE_SCRIPTS`). */
      oneStep: Readonly<Record<string, UTxO>>;
    }
  >;
  now?: () => number;
}>;

export type ManifestBoundValidationTraceDisputeWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<typeof CATEGORY>;
  l1: FraudProofFamilyL1ObservationPort<typeof CATEGORY> &
    FraudProofSignedTransactionRecovery;
  deployment: ValidationTraceDisputeDeployment;
  l1SlotConfig: L1SlotConfig;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export const createManifestBoundValidationTraceDisputeWorkflow = async (
  config: ManifestBoundValidationTraceDisputeWorkflowConfig,
): Promise<ManifestBoundValidationTraceDisputeWorkflow> => {
  const binding = await bindFraudProofDisputeDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: CATEGORY,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const contracts = binding.resolvedContracts;
  const chain = contracts.contracts.validationTraceDispute;
  if (chain === undefined || contracts.stateQueuePolicyId === undefined)
    throw new Error(
      "validation-trace manifest binding omitted required contracts",
    );
  const bound = (contractName: string, utxo: UTxO): UTxO =>
    requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });
  const references = Object.freeze(
    Object.fromEntries(
      Object.entries(VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS).map(
        ([role, contractName]) => [
          role,
          bound(contractName, config.referenceScripts[role as DisputeRole]),
        ],
      ),
    ) as Record<DisputeRole, UTxO>,
  );
  const witnesses = config.referenceScripts.witnesses;
  const member = (contractName: string): UTxO => {
    const utxo = config.referenceScripts.oneStep[contractName];
    if (utxo === undefined)
      throw new Error(
        `validation-trace workflow config omitted reference script ${contractName}`,
      );
    return bound(contractName, utxo);
  };
  const deployment: ValidationTraceDisputeDeployment = Object.freeze({
    deploymentFingerprint: binding.deploymentFingerprint,
    network: binding.network,
    blueprint: binding.blueprint,
    init: {
      contracts: {
        steps: [chain.opener],
        computationThread: contracts.contracts.computationThread,
        hubOraclePolicyId: contracts.hubOraclePolicyId,
        stateQueuePolicyId: contracts.stateQueuePolicyId,
      },
      category: contracts.category,
      catalogue: binding.catalogue,
      witnessReferenceScripts: {
        computationThreadMint: bound(
          "computationThreadMint",
          witnesses.computationThreadMint,
        ),
        fraudProofMint: bound("fraudProofMint", witnesses.fraudProofMint),
        phasMembershipWithdraw: bound(
          "phasMembershipWithdraw",
          witnesses.phasMembershipWithdraw,
        ),
      },
    },
    stateQueue: {
      policyId: binding.definition.stateQueue.policyId,
      spendingScriptAddress: binding.definition.stateQueue.address,
    },
    chain,
    fraudProof: {
      policyId: contracts.contracts.fraudProof.policyId,
      mintingScript: contracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        contracts.contracts.fraudProof.spendingScriptAddress,
    },
    references: {
      ...references,
      oneStep: validationTraceOneStepReferences(member),
    },
    bondLovelace: parseContractDeploymentDisputeBondLovelace(
      binding.deploymentInfo,
    ),
    scriptDecode: scriptDecodeDeploymentFromReferences({
      blueprint: binding.blueprint,
      network: binding.network,
      references: validationTraceScriptDecodeReferences(member),
    }),
  });
  const l1 = createFraudProofFamilyL1ObservationPort({
    l1: config.l1Source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: {
      ...binding.definition,
      computationThread: {
        ...binding.definition.computationThread,
        steps: validationTraceComputationSteps(chain),
      },
    },
  });
  if (l1.publications === undefined)
    throw new Error("validation-trace raw L1 omitted publication observer");
  const l1SlotConfig = parseContractDeploymentL1SlotConfig(
    binding.deploymentInfo,
  );
  const adapter = createValidationTraceDisputeWorkflowAdapter({
    lucid: config.lucid,
    signer: config.signer,
    deployment,
    l1SlotConfig,
    removal: {
      l1For: (headerHash) => {
        if (headerHash !== binding.definition.headerHash)
          throw new Error("validation-trace removal names another block");
        return l1;
      },
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      deploymentInfo: binding.deploymentInfo,
      fraudProverRewardLovelace: BigInt(
        binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    },
    signedRecovery: l1,
    caseHistory: createValidationTraceCaseHistory({
      authority: config.l1Source.snapshotAuthority({
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      }),
      releaseFinality: binding.releaseFinality,
    }),
    publications: l1.publications,
    ...(config.now === undefined ? {} : { now: config.now }),
  });
  return Object.freeze({
    binding,
    l1,
    deployment,
    l1SlotConfig,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
  });
};

/** The held events of every run in this process. A workflow is built per
 * run, so the holds live here: a run re-executes a held event only once its
 * backoff, carried from the runs before, has passed. */
const RUN_REPLAY_HOLDS = createValidationTraceReplayHolds();

/**
 * One run of the dispute, as its host sees it: a wait on the counterparty as
 * `awaiting_counterparty` with its deadline, and a state named for readiness
 * (an alarm among them) as a pending run that resumes on the next
 * observation under that state.
 */
export const runOrResumeManifestBoundValidationTraceDisputeWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundValidationTraceDisputeWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<
  FraudProofWorkflowRunResult | ValidationTraceAwaitingCounterparty
> =>
  validationTraceRunState(
    await runFraudProofWorkflowFromRetainedDa({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      observation: await observeFraudProofWorkflowHeader(workflow.l1, {
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
      replayer: createValidationTraceDisputeCompleteCanonicalReplay(
        workflow.l1SlotConfig,
        { holds: RUN_REPLAY_HOLDS },
      ),
      ...(workflow.replayContext === undefined
        ? {}
        : { replayContext: workflow.replayContext }),
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflow.adapter],
        launchScope: [CATEGORY],
      }),
      journal,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    }),
  );
