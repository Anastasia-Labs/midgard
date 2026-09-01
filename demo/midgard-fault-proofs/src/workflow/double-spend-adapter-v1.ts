import {
  assertSecurityGradeEvidenceV1,
  type AuthenticatedStateQueueHeaderObservationV1,
  type EvidenceProvenanceV1,
} from "@al-ft/midgard-sdk";
import {
  deriveFieldPreimageCertificationV1,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
} from "@al-ft/midgard-sdk";
import type {
  LucidEvolution,
  MintingPolicy,
  Network,
  UTxO,
} from "@lucid-evolution/lucid";

import { prepareDoubleSpendFromCanonicalEvidenceV1 } from "../evidence/prepare-from-evidence-v1.js";
import {
  certifyFaultProofFieldCarriageV1,
  fieldPreimageCertificateAddressV1,
  findMissingFaultProofFieldPublicationV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  publishProofChunksV1,
  resolvePublishedProofChunksV1,
} from "../publish-proof-chunks.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import {
  parseSubmitStep01TxInclusion,
  submitStep01,
} from "../submit-step-01.js";
import { submitStep02 } from "../submit-step-02.js";
import { submitStep03 } from "../submit-step-03.js";
import { submitStep04 } from "../submit-step-04.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import type {
  FraudProofWorkflowJournalEntryV1,
  FraudProofWorkflowJournalStoreV1,
  FraudProofWorkflowTerminalV1,
  JournalJsonObjectV1,
  JournalJsonValueV1,
} from "./journal-v1.js";
import {
  createLocalKupmiosHttpOgmiosRawSourceV1,
  type LocalKupmiosHttpOgmiosSourceConfigV1,
} from "./local-kupmios-http-ogmios-source-v1.js";
import { createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1 } from "./local-kupmios-raw-l1-authority-v1.js";
import type {
  FraudProofFamilyWorkflowAdapterV1,
  FraudProofWorkflowActionV1,
  FraudProofWorkflowPreflightV1,
  FraudProofWorkflowTerminalVerifierV1,
} from "./orchestrator-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  type FraudProofWorkflowRunResultV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1V1,
  deriveFraudProofRawL1FamilyStageV1,
  type FraudProofRawL1FamilyDefinitionV1,
  fraudProofRawL1SnapshotRequestForFamilyV1,
} from "./raw-l1-family-derivation-v1.js";
import {
  createFraudProofAuthenticatedPublicationObserverV1,
  type FraudProofAuthenticatedPublicationObserverV1,
} from "./raw-l1-publication-observation-v1.js";
import {
  admitFraudProofRawL1SnapshotV1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1,
  type FraudProofRawL1SnapshotAuthorityV1,
} from "./raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseEconomicsPolicyV1 } from "./release-economics-policy-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicyV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  LOCAL_UPLC_EVALUATOR_V1,
  type LocallyEvaluatedTransactionV1,
  requireReferenceOnlyScriptWitnessesV1,
  submitCapturedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const DOUBLE_SPEND_WORKFLOW_ADAPTER_V1 =
  "midgard-double-spend-production-workflow-adapter-v1" as const;

export type DoubleSpendWorkflowStageV1 =
  | { readonly kind: "not_started"; readonly stateQueueBlockOutRef: string }
  | {
      readonly kind: "step_01" | "step_02";
      readonly threadOutRef: string;
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "step_03" | "step_04";
      readonly threadOutRef: string;
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "proof_token";
      readonly fraudProofOutRef: string;
      readonly stateQueueBlockOutRef: string;
      /** Changes after every descendant removal, giving each tx a stable id. */
      readonly nextRemovalOutRef: string;
    }
  | {
      readonly kind: "removed";
      readonly terminal: FraudProofWorkflowTerminalV1;
    };

/**
 * Integration port for L1 stage observations. This type alone is not an
 * authentication boundary: production registration remains blocked until a
 * concrete raw local-node/provider implementation derives these facts.
 */
export interface DoubleSpendL1ObservationPortV1 {
  readonly publications?: FraudProofAuthenticatedPublicationObserverV1;
  observeHeader?(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservationV1>;
  transactionConfirmed?(input: {
    readonly headerHash: string;
    readonly txHash: string;
  }): Promise<boolean>;
  observe(input: { readonly headerHash: string }): Promise<{
    readonly provenance: EvidenceProvenanceV1;
    readonly stage: DoubleSpendWorkflowStageV1;
  }>;
}

/**
 * Production observation port: the provider returns only untrusted exact bytes;
 * family stage and terminal facts are derived locally after strict admission.
 */
export const createDoubleSpendRawL1ObservationPortV1 = ({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthorityV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
  readonly definition: FraudProofRawL1FamilyDefinitionV1 & {
    readonly category: "doubleSpend";
  };
}): DoubleSpendL1ObservationPortV1 => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1 ||
    definition.computationThread.steps.length !== 4
  ) {
    throw new Error("double-spend raw L1 observation authority is incomplete");
  }
  const request = fraudProofRawL1SnapshotRequestForFamilyV1({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error("double-spend raw L1 observation changed the header");
    }
    return admitFraudProofRawL1SnapshotV1({
      value: await authority.capture(request),
      request,
      releaseFinality,
    });
  };
  return {
    publications: createFraudProofAuthenticatedPublicationObserverV1({
      authority,
      releaseFinality,
    }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      (await capture(headerHash)).transactions.some(
        (transaction) => transaction.txHash === txHash,
      ),
    observeHeader: async ({ headerHash }) =>
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1V1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observe: async ({ headerHash }) => {
      const snapshot = await capture(headerHash);
      const derived = await deriveFraudProofRawL1FamilyStageV1({
        snapshot,
        definition,
        releaseEconomics,
      });
      const stage: DoubleSpendWorkflowStageV1 =
        derived.kind === "step"
          ? {
              kind: `step_0${derived.step}` as
                | "step_01"
                | "step_02"
                | "step_03"
                | "step_04",
              threadOutRef: derived.threadOutRef,
              stateQueueBlockOutRef: derived.stateQueueBlockOutRef,
            }
          : derived;
      return { provenance: snapshot.provenance, stage };
    },
  };
};

/** Concrete loopback Kupo HTTP + Ogmios WS production construction. */
export const createDoubleSpendLocalKupmiosL1ObservationPortV1 = ({
  source,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly source: Omit<
    LocalKupmiosHttpOgmiosSourceConfigV1,
    "releaseFinality"
  >;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
  readonly definition: FraudProofRawL1FamilyDefinitionV1 & {
    readonly category: "doubleSpend";
  };
}): DoubleSpendL1ObservationPortV1 => {
  const rawSource = createLocalKupmiosHttpOgmiosRawSourceV1({
    ...source,
    releaseFinality,
  });
  return createDoubleSpendRawL1ObservationPortV1({
    authority: createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1({
      source: rawSource,
      releaseFinality,
    }),
    releaseFinality,
    releaseEconomics,
    definition,
  });
};

const sameTerminal = (
  left: FraudProofWorkflowTerminalV1,
  right: FraudProofWorkflowTerminalV1,
): boolean => JSON.stringify(left) === JSON.stringify(right);

/** Second observation through the constrained integration port. */
export const createDoubleSpendAuthenticatedL1TerminalVerifierV1 = (
  l1: DoubleSpendL1ObservationPortV1,
): FraudProofWorkflowTerminalVerifierV1 => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  verify: async ({ identity, candidate, releaseFinality }) => {
    if (identity.target.kind !== "state_queue_header") {
      throw new Error(
        "double-spend terminal requires a state-queue header target",
      );
    }
    const observed = await l1.observe({
      headerHash: identity.target.headerHash,
    });
    const stage = admitSnapshot({
      headerHash: identity.target.headerHash,
      ...observed,
    });
    if (stage.kind !== "removed") {
      throw new Error(
        "authenticated L1 still reports an unfinished correction",
      );
    }
    if (!sameTerminal(stage.terminal, candidate)) {
      throw new Error(
        "adapter terminal candidate differs from independent L1 observation",
      );
    }
    if (
      stage.terminal.observedAt.confirmationDepth <
      releaseFinality.policy.confirmationDepth
    ) {
      throw new Error(
        `authenticated terminal depth is below the release threshold: required=${releaseFinality.policy.confirmationDepth.toString()} actual=${stage.terminal.observedAt.confirmationDepth.toString()} policy=${releaseFinality.policyDigest}`,
      );
    }
    return stage.terminal;
  },
});

export type DoubleSpendWorkflowReferenceScriptsV1 = {
  readonly steps: readonly [UTxO, UTxO, UTxO, UTxO];
  readonly witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
};

export type DoubleSpendConstrainedWorkflowAdapterConfigV1 = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly referenceScripts: DoubleSpendWorkflowReferenceScriptsV1;
  readonly fieldPreimageCertificate: {
    readonly policyId: string;
    readonly mintingScript: MintingPolicy;
    readonly referenceScriptUtxo: UTxO;
  };
  readonly l1: DoubleSpendL1ObservationPortV1;
  /** Coordination only; never used as proof evidence. */
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  readonly fraudProverRewardLovelace?: bigint;
};

export type ManifestBoundDoubleSpendWorkflowConfigV1 = Omit<
  DoubleSpendConstrainedWorkflowAdapterConfigV1,
  | "blueprint"
  | "deploymentInfo"
  | "network"
  | "fieldPreimageCertificate"
  | "l1"
  | "fraudProverRewardLovelace"
> & {
  readonly manifest: unknown;
  readonly blueprintJson: string;
  readonly deploymentInfo: unknown;
  readonly headerHash: string;
  readonly source: Omit<
    LocalKupmiosHttpOgmiosSourceConfigV1,
    "releaseFinality"
  >;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
};

export type ManifestBoundDoubleSpendWorkflowV1 = {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"doubleSpend">;
  readonly adapterConfig: DoubleSpendConstrainedWorkflowAdapterConfigV1;
  readonly adapter: FraudProofFamilyWorkflowAdapterV1;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
};

/**
 * Strict production construction. Every contract, network, category,
 * economics, and finality value comes from the same finalized manifest; the
 * caller supplies only live runtime capabilities and published UTxOs.
 */
export const createManifestBoundDoubleSpendWorkflowV1 = async (
  config: ManifestBoundDoubleSpendWorkflowConfigV1,
): Promise<ManifestBoundDoubleSpendWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "doubleSpend",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      DoubleSpendStep02Datum,
      DoubleSpendStep03Datum,
      DoubleSpendStep04Datum,
    ],
  });
  const certificate = binding.fieldPreimageCertificate;
  if (certificate === null) {
    throw new Error(
      "double-spend deployment omitted the field-preimage certificate policy",
    );
  }
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const stepNames = [
    "fraudProofDoubleSpend",
    "fraudProofDoubleSpendStep02",
    "fraudProofDoubleSpendStep03",
    "fraudProofDoubleSpendStep04",
  ] as const;
  const stepReference = (index: 0 | 1 | 2 | 3): UTxO =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: stepNames[index],
      utxo: config.referenceScripts.steps[index],
    });
  const referenceScripts: DoubleSpendWorkflowReferenceScriptsV1 = {
    steps: [
      stepReference(0),
      stepReference(1),
      stepReference(2),
      stepReference(3),
    ],
    witnesses: {
      computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    },
  };
  const certificateReferenceScript = requireManifestBoundReferenceScriptUtxoV1({
    binding,
    contractName: "fieldPreimageCertificateMint",
    utxo: config.fieldPreimageCertificateReferenceScript,
  });
  const l1 = createDoubleSpendLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const adapterConfig: DoubleSpendConstrainedWorkflowAdapterConfigV1 = {
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    referenceScripts,
    fieldPreimageCertificate: {
      policyId: certificate.policyId,
      mintingScript: certificate.mintingScript,
      referenceScriptUtxo: certificateReferenceScript,
    },
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  };
  return {
    binding,
    adapterConfig,
    adapter: createDoubleSpendConstrainedWorkflowAdapterV1(adapterConfig),
    terminalVerifier: createDoubleSpendAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  };
};

type DoubleSpendArtifactV1 = JournalJsonObjectV1 & {
  readonly headerHash: string;
  readonly tx1: JournalJsonObjectV1 & {
    readonly inclusion: JournalJsonObjectV1;
    readonly nativeTxId: string;
    readonly nativeTxCompactCbor: string;
    readonly spendInputCbors: readonly string[];
    readonly doubleSpentInputIndex: number;
  };
  readonly tx2: JournalJsonObjectV1 & {
    readonly inclusion: JournalJsonObjectV1;
    readonly nativeTxId: string;
    readonly nativeTxCompactCbor: string;
    readonly spendInputCbors: readonly string[];
    readonly doubleSpentInputIndex: number;
  };
};

const journalValue = (value: unknown): JournalJsonValueV1 => {
  if (typeof value === "bigint") return value.toString();
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean" ||
    typeof value === "number"
  ) {
    return value;
  }
  if (Array.isArray(value)) return value.map(journalValue);
  if (typeof value !== "object") {
    throw new Error("double-spend artifact contains a non-JSON value");
  }
  return Object.fromEntries(
    Object.entries(value as Readonly<Record<string, unknown>>).map(
      ([key, child]) => [key, journalValue(child)],
    ),
  );
};

const artifactFrom = (value: JournalJsonObjectV1): DoubleSpendArtifactV1 =>
  value as DoubleSpendArtifactV1;

const requireJournalString = (
  value: JournalJsonValueV1 | undefined,
  label: string,
): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a string`);
  }
  return value;
};

const canonicalOutRef = (value: string, label: string): string => {
  if (!/^[0-9a-f]{64}#[0-9]+$/u.test(value)) {
    throw new Error(`${label} must be a canonical Cardano output reference`);
  }
  return value;
};

const admitSnapshot = ({
  headerHash,
  provenance,
  stage,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: DoubleSpendWorkflowStageV1;
}): DoubleSpendWorkflowStageV1 => {
  const admitted = assertSecurityGradeEvidenceV1(provenance);
  if (admitted.trustClass !== "authenticated_cardano_l1") {
    throw new Error(
      "double-spend workflow observation is not authenticated L1",
    );
  }
  if (stage.kind === "removed") {
    if (stage.terminal.headerHash !== headerHash) {
      throw new Error("double-spend terminal targets a different header");
    }
    return stage;
  }
  canonicalOutRef(stage.stateQueueBlockOutRef, "state-queue block outRef");
  if (
    stage.kind === "step_01" ||
    stage.kind === "step_02" ||
    stage.kind === "step_03" ||
    stage.kind === "step_04"
  ) {
    canonicalOutRef(stage.threadOutRef, "computation-thread outRef");
  }
  if (stage.kind === "proof_token") {
    canonicalOutRef(stage.fraudProofOutRef, "fraud-proof outRef");
    canonicalOutRef(stage.nextRemovalOutRef, "next removal outRef");
  }
  return stage;
};

const confirmed = (
  entries: readonly FraudProofWorkflowJournalEntryV1[],
  actionId: string,
): boolean =>
  entries.some(
    (entry) =>
      entry.event.kind === "confirmed" && entry.event.actionId === actionId,
  );

const contentActionId = ({
  base,
  entries,
}: {
  readonly base: string;
  readonly entries: readonly FraudProofWorkflowJournalEntryV1[];
}): string => {
  const priorConfirmations = entries.filter(
    (entry) =>
      entry.event.kind === "confirmed" &&
      (entry.event.actionId === base ||
        entry.event.actionId.startsWith(`${base}:heal:`)),
  ).length;
  return priorConfirmations === 0
    ? base
    : `${base}:heal:${priorConfirmations.toString()}`;
};

const action = (
  actionId: string,
  input: JournalJsonObjectV1,
): FraudProofWorkflowActionV1 => ({ actionId, input });

const preflightOf = (
  actionId: string,
  transaction: LocallyEvaluatedTransactionV1,
  durableRecovery?: JournalJsonObjectV1,
): FraudProofWorkflowPreflightV1 => {
  requireReferenceOnlyScriptWitnessesV1({
    transaction,
    label: "double-spend production transaction",
  });
  return {
    actionId,
    txHash: transaction.txHash,
    scriptExecution:
      transaction.referenceScripts.length === 0 ? "none" : "reference_scripts",
    localUplcEvaluation: {
      status: "passed",
      evaluator: LOCAL_UPLC_EVALUATOR_V1,
    },
    referenceScripts: transaction.referenceScripts,
    ...(durableRecovery === undefined ? {} : { durableRecovery }),
  };
};

const mutationLeaseRecovery = (
  lease: StateQueueMutationLease,
): JournalJsonObjectV1 => ({
  stateQueueMutationLease: {
    token: lease.token,
    source: lease.source,
  },
});

const parseMutationLeaseRecovery = (
  recovery: JournalJsonObjectV1 | undefined,
): { readonly token: string; readonly source: string } | undefined => {
  if (recovery === undefined) return undefined;
  const keys = Object.keys(recovery);
  const value = recovery.stateQueueMutationLease;
  if (
    keys.length !== 1 ||
    keys[0] !== "stateQueueMutationLease" ||
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value)
  ) {
    throw new Error("durable recovery has an invalid mutation-lease shape");
  }
  const record = value as Readonly<Record<string, JournalJsonValueV1>>;
  if (
    Object.keys(record).sort().join(",") !== "source,token" ||
    typeof record.token !== "string" ||
    record.token.trim().length === 0 ||
    record.token.trim() !== record.token ||
    typeof record.source !== "string" ||
    record.source.trim().length === 0 ||
    record.source.trim() !== record.source
  ) {
    throw new Error("durable recovery mutation-lease identity is malformed");
  }
  return { token: record.token, source: record.source };
};

/**
 * Constrained adapter over the existing double-spend init/step/removal
 * builders. Its transaction boundary is real, but it is not production
 * registered until tier-3 carriage, raw L1 authentication, and durable lease
 * recovery are closed.
 */
export const createDoubleSpendConstrainedWorkflowAdapterV1 = (
  config: DoubleSpendConstrainedWorkflowAdapterConfigV1,
): FraudProofFamilyWorkflowAdapterV1 => {
  const preparedByAction = new Map<
    string,
    {
      readonly transaction: LocallyEvaluatedTransactionV1;
      readonly mutationLease?: StateQueueMutationLease;
    }
  >();
  const mutationLeaseByTxHash = new Map<string, StateQueueMutationLease>();
  const snapshot = async (headerHash: string) => {
    const observed = await config.l1.observe({ headerHash });
    return admitSnapshot({ headerHash, ...observed });
  };

  const proofChunks = async (proofCbor: string) =>
    await resolvePublishedProofChunksV1({
      lucid: config.lucid,
      address: config.signer.address,
      proofCbor,
    });

  const fieldPlan = (
    proofStage: "step_03" | "step_04",
    artifact: DoubleSpendArtifactV1,
  ) => {
    const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
    return planFaultProofFieldOpeningV1({
      fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
      anchorTxId: tx.nativeTxId,
      nativeTxCompactCbor: tx.nativeTxCompactCbor,
      itemCbors: tx.spendInputCbors.map((cbor) => Buffer.from(cbor, "hex")),
      owner: config.signer.paymentKeyHash,
      label: `${proofStage} production preflight`,
    });
  };

  const authenticatePublications = async ({
    headerHash,
    publications,
    certificate,
  }: {
    readonly headerHash: string;
    readonly publications: readonly UTxO[];
    readonly certificate?: UTxO;
  }): Promise<void> => {
    const observer = config.l1.publications;
    if (observer === undefined) {
      throw new Error(
        "production double-spend field inputs require authenticated publication observation",
      );
    }
    for (const publication of publications) {
      if (publication.datum == null) {
        throw new Error("double-spend publication omitted its inline datum");
      }
      const observed = await observer.observeExact({
        headerHash,
        kind: "field_publication",
        address: config.signer.address,
        expectedOutRef: `${publication.txHash}#${publication.outputIndex.toString()}`,
        expectedDatumCbor: publication.datum,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `double-spend publication ${publication.txHash}#${publication.outputIndex.toString()} is not release-final`,
        );
      }
    }
    if (certificate !== undefined) {
      if (certificate.datum == null) {
        throw new Error("double-spend certificate omitted its inline datum");
      }
      const observed = await observer.observeExact({
        headerHash,
        kind: "field_certificate",
        address: fieldPreimageCertificateAddressV1({
          network: config.network,
          certificatePolicyId: config.fieldPreimageCertificate.policyId,
        }),
        expectedOutRef: `${certificate.txHash}#${certificate.outputIndex.toString()}`,
        expectedDatumCbor: certificate.datum,
        expectedUnit: `${config.fieldPreimageCertificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1}`,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `double-spend certificate ${certificate.txHash}#${certificate.outputIndex.toString()} is not release-final`,
        );
      }
    }
  };

  const nextAction = async ({
    artifact,
    entries,
  }: {
    readonly artifact: DoubleSpendArtifactV1;
    readonly entries: readonly FraudProofWorkflowJournalEntryV1[];
  }): Promise<
    | {
        readonly kind: "completed";
        readonly terminal: FraudProofWorkflowTerminalV1;
      }
    | { readonly kind: "conflict"; readonly reason: string }
    | { readonly kind: "action"; readonly action: FraudProofWorkflowActionV1 }
  > => {
    const stage = await snapshot(artifact.headerHash);
    if (stage.kind === "removed") {
      return { kind: "completed", terminal: stage.terminal };
    }
    if (stage.kind === "not_started") {
      return {
        kind: "action",
        action: action("init", {
          stage: "init",
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        }),
      };
    }
    if (stage.kind === "step_01" || stage.kind === "step_02") {
      const tx = stage.kind === "step_01" ? artifact.tx1 : artifact.tx2;
      const publicationId = `${stage.kind}:publish-proof`;
      const chunks = await proofChunks(
        requireJournalString(
          tx.inclusion.txMembershipProofCbor,
          "tx.inclusion.txMembershipProofCbor",
        ),
      );
      if (chunks === undefined && !confirmed(entries, publicationId)) {
        return {
          kind: "action",
          action: action(publicationId, {
            stage: "publish-proof",
            proofFor: stage.kind,
            proofCbor: requireJournalString(
              tx.inclusion.txMembershipProofCbor,
              "tx.inclusion.txMembershipProofCbor",
            ),
          }),
        };
      }
      return {
        kind: "action",
        action: action(stage.kind, {
          stage: stage.kind,
          threadOutRef: stage.threadOutRef,
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        }),
      };
    }
    if (stage.kind === "step_03" || stage.kind === "step_04") {
      const plan = fieldPlan(stage.kind, artifact);
      const missing = await findMissingFaultProofFieldPublicationV1({
        lucid: config.lucid,
        publisherAddress: config.signer.address,
        planned: plan,
      });
      if (missing !== undefined) {
        const base = `${stage.kind}:publish-field:${missing.digest}`;
        return {
          kind: "action",
          action: action(contentActionId({ base, entries }), {
            stage: "publish-field",
            proofFor: stage.kind,
            threadOutRef: stage.threadOutRef,
            publicationDatumCbor: missing.datumCbor,
          }),
        };
      }
      if (plan.plan.tier === "Certified") {
        const certificate = await resolveFaultProofFieldPreimageCertificateV1({
          lucid: config.lucid,
          network: config.network,
          planned: plan,
          certificatePolicyId: config.fieldPreimageCertificate.policyId,
        });
        if (certificate === undefined) {
          const publications =
            await resolveFaultProofFieldCarriagePublicationsV1({
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: plan,
            });
          if (publications === undefined) {
            throw new Error(
              "tier-3 field publications disappeared before certification",
            );
          }
          const base = `${stage.kind}:certify-field:${plan.commitment}`;
          return {
            kind: "action",
            action: action(contentActionId({ base, entries }), {
              stage: "certify-field",
              proofFor: stage.kind,
              fieldCommitment: plan.commitment,
              chunkOutRefs: publications.map(
                (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`,
              ),
            }),
          };
        }
        return {
          kind: "action",
          action: action(stage.kind, {
            stage: stage.kind,
            threadOutRef: stage.threadOutRef,
            certificateOutRef: `${certificate.txHash}#${certificate.outputIndex.toString()}`,
          }),
        };
      }
      return {
        kind: "action",
        action: action(stage.kind, {
          stage: stage.kind,
          threadOutRef: stage.threadOutRef,
        }),
      };
    }
    if (stage.kind !== "proof_token") {
      throw new Error(
        `unsupported authenticated double-spend stage: ${String(stage.kind)}`,
      );
    }
    return {
      kind: "action",
      action: action(`remove:${stage.nextRemovalOutRef}`, {
        stage: "remove",
        fraudProofOutRef: stage.fraudProofOutRef,
        nextRemovalOutRef: stage.nextRemovalOutRef,
        requiresMutationLease:
          stage.nextRemovalOutRef !== stage.stateQueueBlockOutRef,
      }),
    };
  };

  const capture = async ({
    action: requested,
    artifact,
  }: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: DoubleSpendArtifactV1;
  }): Promise<LocallyEvaluatedTransactionV1> => {
    const stage = requireJournalString(requested.input.stage, "action.stage");
    if (stage === "publish-proof") {
      return await captureLocallyEvaluatedTransactionV1(async (boundary) => {
        await publishProofChunksV1({
          lucid: config.lucid,
          network: config.network,
          signer: config.signer,
          proofCbor: requireJournalString(
            requested.input.proofCbor,
            "action.proofCbor",
          ),
          preSubmitBoundary: boundary,
        });
      });
    }
    if (stage === "init") {
      return await captureLocallyEvaluatedTransactionV1(async (boundary) => {
        await submitInit({
          lucid: config.lucid,
          blueprint: config.blueprint,
          deploymentInfo: config.deploymentInfo,
          network: config.network,
          signer: config.signer,
          fraudCategory: "doubleSpend",
          fraudulentBlockOutRef: requireJournalString(
            requested.input.stateQueueBlockOutRef,
            "action.stateQueueBlockOutRef",
          ),
          fraudulentHeaderHash: artifact.headerHash,
          witnessReferenceScripts: config.referenceScripts.witnesses,
          preSubmitBoundary: boundary,
        });
      });
    }
    if (stage === "step_01" || stage === "step_02") {
      const tx = stage === "step_01" ? artifact.tx1 : artifact.tx2;
      const chunks = await proofChunks(
        requireJournalString(
          tx.inclusion.txMembershipProofCbor,
          "tx.inclusion.txMembershipProofCbor",
        ),
      );
      if (chunks === undefined) {
        throw new Error(`${stage} proof chunks are not observable on L1`);
      }
      await authenticatePublications({
        headerHash: artifact.headerHash,
        publications: chunks.map((chunk) => chunk.utxo),
      });
      const common = {
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        threadOutRef: requireJournalString(
          requested.input.threadOutRef,
          "action.threadOutRef",
        ),
        stateQueueBlockOutRef: requireJournalString(
          requested.input.stateQueueBlockOutRef,
          "action.stateQueueBlockOutRef",
        ),
        txInclusion: parseSubmitStep01TxInclusion(tx.inclusion),
        publishedProofChunks: chunks,
        witnessReferenceScripts: config.referenceScripts.witnesses,
      } as const;
      return await captureLocallyEvaluatedTransactionV1(async (boundary) => {
        if (stage === "step_01") {
          await submitStep01({
            ...common,
            referenceScriptUtxo: config.referenceScripts.steps[0],
            preSubmitBoundary: boundary,
          });
        } else {
          await submitStep02({
            ...common,
            referenceScriptUtxo: config.referenceScripts.steps[1],
            preSubmitBoundary: boundary,
          });
        }
      });
    }
    if (stage === "certify-field") {
      const proofStage = requireJournalString(
        requested.input.proofFor,
        "action.proofFor",
      );
      if (proofStage !== "step_03" && proofStage !== "step_04") {
        throw new Error("field certification names an unknown proof stage");
      }
      const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
      const plan = fieldPlan(proofStage, artifact);
      if (
        plan.plan.tier !== "Certified" ||
        requested.input.fieldCommitment !== plan.commitment
      ) {
        throw new Error(
          "field certification action does not match the tier-3 plan",
        );
      }
      const publications = await resolveFaultProofFieldCarriagePublicationsV1({
        lucid: config.lucid,
        publisherAddress: config.signer.address,
        planned: plan,
      });
      if (publications === undefined) {
        throw new Error("tier-3 field publications are not observable on L1");
      }
      await authenticatePublications({
        headerHash: artifact.headerHash,
        publications,
      });
      const observedOutRefs = publications.map(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`,
      );
      if (
        JSON.stringify(requested.input.chunkOutRefs) !==
        JSON.stringify(observedOutRefs)
      ) {
        throw new Error(
          "field certification action does not match the observed chunk UTxOs",
        );
      }
      return await captureLocallyEvaluatedTransactionV1(async (boundary) => {
        await certifyFaultProofFieldCarriageV1({
          lucid: config.lucid,
          network: config.network,
          signer: config.signer,
          planned: plan,
          certificatePolicyId: config.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            config.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo:
            config.fieldPreimageCertificate.referenceScriptUtxo,
          chunkUtxos: publications,
          compactCbor: tx.nativeTxCompactCbor,
          preSubmitBoundary: boundary,
          awaitConfirmation: false,
        });
      });
    }
    if (
      stage === "step_03" ||
      stage === "step_04" ||
      stage === "publish-field"
    ) {
      const proofStage =
        stage === "publish-field"
          ? requireJournalString(requested.input.proofFor, "action.proofFor")
          : stage;
      if (proofStage !== "step_03" && proofStage !== "step_04") {
        throw new Error("field action names an unknown proof stage");
      }
      const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
      const plan = fieldPlan(proofStage, artifact);
      if (stage === "publish-field") {
        const expectedMissing = await findMissingFaultProofFieldPublicationV1({
          lucid: config.lucid,
          publisherAddress: config.signer.address,
          planned: plan,
        });
        if (
          expectedMissing === undefined ||
          requested.input.publicationDatumCbor !== expectedMissing.datumCbor
        ) {
          throw new Error(
            "field publication action does not match the next missing plan chunk",
          );
        }
      }
      const publications =
        stage === "publish-field"
          ? undefined
          : await resolveFaultProofFieldCarriagePublicationsV1({
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: plan,
            });
      if (stage !== "publish-field" && publications === undefined) {
        throw new Error("field carriage publications are not observable on L1");
      }
      const certificate =
        stage !== "publish-field" && plan.plan.tier === "Certified"
          ? await resolveFaultProofFieldPreimageCertificateV1({
              lucid: config.lucid,
              network: config.network,
              planned: plan,
              certificatePolicyId: config.fieldPreimageCertificate.policyId,
            })
          : undefined;
      if (
        stage !== "publish-field" &&
        plan.plan.tier === "Certified" &&
        (certificate === undefined ||
          requested.input.certificateOutRef !==
            `${certificate.txHash}#${certificate.outputIndex.toString()}`)
      ) {
        throw new Error(
          "tier-3 proof step does not bind the observed field certificate",
        );
      }
      if (stage !== "publish-field") {
        await authenticatePublications({
          headerHash: artifact.headerHash,
          publications: publications ?? [],
          ...(certificate === undefined ? {} : { certificate }),
        });
      }
      return await captureLocallyEvaluatedTransactionV1(async (boundary) => {
        if (proofStage === "step_03") {
          await submitStep03({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            threadOutRef: requireJournalString(
              requested.input.threadOutRef,
              "action.threadOutRef",
            ),
            tx1SpendInputCbors: tx.spendInputCbors,
            nativeTxCompactCbor: tx.nativeTxCompactCbor,
            doubleSpentInputIndex: BigInt(tx.doubleSpentInputIndex),
            ...(publications === undefined
              ? {}
              : { publishedCarriageUtxos: publications }),
            ...(certificate === undefined
              ? {}
              : { certificateUtxo: certificate }),
            ...(plan.plan.tier === "Certified"
              ? {
                  certificatePolicyId: config.fieldPreimageCertificate.policyId,
                }
              : {}),
            referenceScriptUtxo: config.referenceScripts.steps[2],
            preSubmitBoundary: boundary,
          });
        } else {
          await submitStep04({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            threadOutRef: requireJournalString(
              requested.input.threadOutRef,
              "action.threadOutRef",
            ),
            tx2SpendInputCbors: tx.spendInputCbors,
            nativeTxCompactCbor: tx.nativeTxCompactCbor,
            doubleSpentInputIndex: BigInt(tx.doubleSpentInputIndex),
            ...(publications === undefined
              ? {}
              : { publishedCarriageUtxos: publications }),
            ...(certificate === undefined
              ? {}
              : { certificateUtxo: certificate }),
            ...(plan.plan.tier === "Certified"
              ? {
                  certificatePolicyId: config.fieldPreimageCertificate.policyId,
                }
              : {}),
            referenceScriptUtxo: config.referenceScripts.steps[3],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary: boundary,
          });
        }
      });
    }
    if (stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (boundary) => {
          await submitRemoveFraudulentBlock({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "doubleSpend",
            fraudulentHeaderHash: artifact.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            ...(config.fraudProverRewardLovelace === undefined
              ? {}
              : {
                  fraudProverRewardLovelace: config.fraudProverRewardLovelace,
                }),
            preSubmitBoundary: async (transaction) => {
              if (
                !workflowTransactionInputOutRefsV1(transaction.signed).includes(
                  requireJournalString(
                    requested.input.nextRemovalOutRef,
                    "action.nextRemovalOutRef",
                  ),
                )
              ) {
                throw new Error(
                  "removal transaction does not consume the authenticated next state-queue outRef",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefsV1(
                  transaction.signed,
                ).includes(
                  requireJournalString(
                    requested.input.fraudProofOutRef,
                    "action.fraudProofOutRef",
                  ),
                )
              ) {
                throw new Error(
                  "removal transaction does not reference the authenticated permanent proof token",
                );
              }
              await boundary(transaction);
            },
          });
        },
      );
      preparedByAction.set(requested.actionId, {
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
      if (
        (requested.input.requiresMutationLease === true) !==
        (mutationLease !== undefined)
      ) {
        await mutationLease?.fail(
          "authenticated removal topology disagreed with lease requirement",
        );
        preparedByAction.delete(requested.actionId);
        throw new Error(
          "authenticated removal topology disagreed with mutation-lease acquisition",
        );
      }
      return transaction;
    }
    throw new Error(`unknown double-spend workflow action stage: ${stage}`);
  };

  return {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
    category: "doubleSpend",
    safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
    prepare: async ({ evidence }) => {
      const prepared = await prepareDoubleSpendFromCanonicalEvidenceV1({
        evidence,
      });
      return journalValue({
        headerHash: prepared.headerHash,
        tx1: {
          inclusion: prepared.tx1.txInclusion,
          nativeTxId: prepared.tx1.nodeTxId,
          nativeTxCompactCbor: prepared.tx1.nativeTxCompactCbor,
          spendInputCbors: prepared.tx1.spendInputCbors,
          doubleSpentInputIndex: prepared.tx1.doubleSpentInputIndex,
        },
        tx2: {
          inclusion: prepared.tx2.txInclusion,
          nativeTxId: prepared.tx2.nodeTxId,
          nativeTxCompactCbor: prepared.tx2.nativeTxCompactCbor,
          spendInputCbors: prepared.tx2.spendInputCbors,
          doubleSpentInputIndex: prepared.tx2.doubleSpentInputIndex,
        },
      }) as JournalJsonObjectV1;
    },
    observe: async ({ artifact, entries }) => {
      const next = await nextAction({
        artifact: artifactFrom(artifact),
        entries,
      });
      return next.kind === "completed"
        ? { kind: "completed", terminal: next.terminal }
        : next.kind === "conflict"
          ? { kind: "conflict", reason: next.reason }
          : { kind: "action_required", action: next.action };
    },
    preflight: async ({ action: requested, artifact }) => {
      const transaction = await capture({
        action: requested,
        artifact: artifactFrom(artifact),
      });
      if (!preparedByAction.has(requested.actionId)) {
        preparedByAction.set(requested.actionId, { transaction });
      }
      const prepared = preparedByAction.get(requested.actionId)!;
      return preflightOf(
        requested.actionId,
        transaction,
        prepared.mutationLease === undefined
          ? undefined
          : mutationLeaseRecovery(prepared.mutationLease),
      );
    },
    submit: async ({ action: requested, preflight }) => {
      const prepared = preparedByAction.get(requested.actionId);
      if (prepared === undefined) {
        throw new Error(
          `locally evaluated transaction for ${requested.actionId} is not available in this process`,
        );
      }
      if (prepared.transaction.txHash !== preflight.txHash) {
        throw new Error(
          "cached transaction does not match durable intent hash",
        );
      }
      const recoveryIdentity = parseMutationLeaseRecovery(
        preflight.durableRecovery,
      );
      if (
        (prepared.mutationLease === undefined) !==
          (recoveryIdentity === undefined) ||
        (prepared.mutationLease !== undefined &&
          (prepared.mutationLease.token !== recoveryIdentity?.token ||
            prepared.mutationLease.source !== recoveryIdentity.source))
      ) {
        throw new Error(
          "cached mutation lease does not match durable recovery identity",
        );
      }
      preparedByAction.delete(requested.actionId);
      if (prepared.mutationLease !== undefined) {
        mutationLeaseByTxHash.set(preflight.txHash, prepared.mutationLease);
      }
      return {
        kind: "submitted",
        txHash: await submitCapturedTransactionV1(prepared.transaction),
      };
    },
    reconcile: async ({
      action: requested,
      artifact,
      txHash,
      durableRecovery,
    }) => {
      if (txHash === undefined) {
        return { kind: "conflict", reason: "durable intent omitted tx hash" };
      }
      const requiresMutationLease =
        requested.input.stage === "remove" &&
        requested.input.requiresMutationLease === true;
      let mutationLease = mutationLeaseByTxHash.get(txHash);
      const recoveryIdentity = parseMutationLeaseRecovery(durableRecovery);
      if (requiresMutationLease && recoveryIdentity === undefined) {
        return {
          kind: "conflict",
          reason:
            "descendant removal intent omitted its durable mutation-lease identity",
        };
      }
      if (!requiresMutationLease && recoveryIdentity !== undefined) {
        return {
          kind: "conflict",
          reason:
            "non-descendant action carried an unexpected mutation-lease identity",
        };
      }
      if (mutationLease === undefined && recoveryIdentity !== undefined) {
        if (config.stateQueueMutationLeaseCoordinator.resume === undefined) {
          return {
            kind: "conflict",
            reason:
              "mutation-lease coordinator cannot resume the journaled fencing token",
          };
        }
        try {
          mutationLease =
            await config.stateQueueMutationLeaseCoordinator.resume(
              recoveryIdentity,
            );
        } catch (cause) {
          return {
            kind: "conflict",
            reason: `journaled mutation lease cannot be resumed: ${String(cause)}`,
          };
        }
        mutationLeaseByTxHash.set(txHash, mutationLease);
      }
      const preparedArtifact = artifactFrom(artifact);
      const actionStage = requireJournalString(
        requested.input.stage,
        "action.stage",
      );
      if (
        actionStage === "publish-proof" ||
        actionStage === "publish-field" ||
        actionStage === "certify-field"
      ) {
        const publicationObserver = config.l1?.publications;
        if (publicationObserver === undefined) {
          return {
            kind: "conflict",
            reason:
              "double-spend publication reconciliation has no authenticated raw-L1 observer",
          };
        }
        if (actionStage === "publish-proof") {
          const chunks = await proofChunks(
            requireJournalString(requested.input.proofCbor, "action.proofCbor"),
          );
          if (
            chunks === undefined ||
            chunks.some((chunk) => chunk.utxo.txHash !== txHash)
          ) {
            return { kind: "not_found" };
          }
          for (const chunk of chunks) {
            const observed = await publicationObserver.observeExact({
              headerHash: preparedArtifact.headerHash,
              kind: "field_publication",
              address: config.signer.address,
              expectedOutRef: chunk.outRef,
              expectedDatumCbor: chunk.datumCbor,
            });
            if (observed.kind !== "confirmed") return { kind: "not_found" };
          }
        } else {
          const proofFor = requireJournalString(
            requested.input.proofFor,
            "action.proofFor",
          );
          if (proofFor !== "step_03" && proofFor !== "step_04") {
            return {
              kind: "conflict",
              reason:
                "double-spend publication action names an invalid proof step",
            };
          }
          const plan = fieldPlan(proofFor, preparedArtifact);
          if (actionStage === "publish-field") {
            const candidates = await config.lucid.utxosAt(
              config.signer.address,
            );
            const candidate = candidates.find(
              (utxo) =>
                utxo.txHash === txHash &&
                utxo.datum === requested.input.publicationDatumCbor,
            );
            if (candidate?.datum == null) return { kind: "not_found" };
            const observed = await publicationObserver.observeExact({
              headerHash: preparedArtifact.headerHash,
              kind: "field_publication",
              address: config.signer.address,
              expectedOutRef: `${candidate.txHash}#${candidate.outputIndex.toString()}`,
              expectedDatumCbor: candidate.datum,
            });
            if (observed.kind !== "confirmed") return { kind: "not_found" };
          } else {
            const certificate =
              await resolveFaultProofFieldPreimageCertificateV1({
                lucid: config.lucid,
                network: config.network,
                planned: plan,
                certificatePolicyId: config.fieldPreimageCertificate.policyId,
              });
            if (certificate === undefined || certificate.txHash !== txHash) {
              return { kind: "not_found" };
            }
            const certification = deriveFieldPreimageCertificationV1(plan.plan);
            const observed = await publicationObserver.observeExact({
              headerHash: preparedArtifact.headerHash,
              kind: "field_certificate",
              address: fieldPreimageCertificateAddressV1({
                network: config.network,
                certificatePolicyId: config.fieldPreimageCertificate.policyId,
              }),
              expectedOutRef: `${certificate.txHash}#${certificate.outputIndex.toString()}`,
              expectedDatumCbor: certification.datumCbor,
              expectedUnit: `${config.fieldPreimageCertificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1}`,
            });
            if (observed.kind !== "confirmed") return { kind: "not_found" };
          }
        }
        return { kind: "confirmed", txHash };
      }
      if (config.l1 === undefined) {
        const legacyStatus = await config.lucid.transactionStatus(txHash);
        await mutationLease?.renew();
        return legacyStatus.status === "pending"
          ? { kind: "pending", txHash }
          : { kind: "not_found" };
      }
      if (config.l1.transactionConfirmed === undefined) {
        return {
          kind: "conflict",
          reason:
            "double-spend reconciliation has no authenticated unit-history transaction observer",
        };
      }
      const [observedStage, intendedTransactionConfirmed] = await Promise.all([
        snapshot(preparedArtifact.headerHash),
        config.l1.transactionConfirmed({
          headerHash: preparedArtifact.headerHash,
          txHash,
        }),
      ]);
      const stageAdvanced =
        actionStage === "init"
          ? observedStage.kind !== "not_started"
          : actionStage === "step_01"
            ? observedStage.kind !== "step_01"
            : actionStage === "step_02"
              ? observedStage.kind !== "step_02"
              : actionStage === "step_03"
                ? observedStage.kind !== "step_03"
                : actionStage === "step_04"
                  ? observedStage.kind === "proof_token" ||
                    observedStage.kind === "removed"
                  : actionStage === "remove"
                    ? observedStage.kind === "removed" ||
                      (observedStage.kind === "proof_token" &&
                        observedStage.nextRemovalOutRef !==
                          requested.input.nextRemovalOutRef)
                    : false;
      if (stageAdvanced && !intendedTransactionConfirmed) {
        await mutationLease?.fail(
          "chain advanced without the journaled transaction in authenticated unit history",
        );
        mutationLeaseByTxHash.delete(txHash);
        return {
          kind: "conflict",
          reason:
            "double-spend chain advanced without the journaled transaction in authenticated unit history",
        };
      }
      if (stageAdvanced && intendedTransactionConfirmed) {
        await mutationLease?.release();
        mutationLeaseByTxHash.delete(txHash);
        return { kind: "confirmed", txHash };
      }
      await mutationLease?.renew();
      return intendedTransactionConfirmed
        ? { kind: "pending", txHash }
        : { kind: "not_found" };
    },
  };
};

/** Run/resume surface for supported constrained shapes; not production-ready. */
export const runOrResumeConstrainedDoubleSpendWorkflowV1 = async ({
  deploymentFingerprint,
  observation,
  sources,
  journal,
  adapterConfig,
  releaseFinalityAuthority,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly deploymentFingerprint: string;
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly adapterConfig: DoubleSpendConstrainedWorkflowAdapterConfigV1;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const adapter = createDoubleSpendConstrainedWorkflowAdapterV1(adapterConfig);
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint,
    observation,
    sources,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [adapter],
      launchScope: ["doubleSpend"],
    }),
    journal,
    releaseFinalityAuthority,
    terminalVerifier: createDoubleSpendAuthenticatedL1TerminalVerifierV1(
      adapterConfig.l1,
    ),
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};

/** Production run/resume with the L1 header derived from admitted raw bytes. */
export const runOrResumeManifestBoundDoubleSpendWorkflowV1 = async ({
  workflow,
  sources,
  journal,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly workflow: ManifestBoundDoubleSpendWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observeHeader = workflow.adapterConfig.l1.observeHeader;
  if (observeHeader === undefined) {
    throw new Error(
      "manifest-bound double-spend workflow omitted raw L1 header derivation",
    );
  }
  const observation = await observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runOrResumeConstrainedDoubleSpendWorkflowV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    journal,
    adapterConfig: workflow.adapterConfig,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};
