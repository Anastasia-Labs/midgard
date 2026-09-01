/**
 * Resumable Q35 workflow adapter for a current-head proof whose field-2
 * opening fits inline. Larger tiered openings remain a fail-closed Q38
 * dependency because every publication/certificate needs its own journaled
 * action before the final proof transaction.
 */
import {
  decodeMidgardNativeTxCompactV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  type AuthenticatedStateQueueHeaderObservationV1,
  deriveFieldPreimageCertificationV1,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1,
  FraudProofComputationThreadStepDatum,
  type NetworkIdFaultV1,
  NetworkIdStep02Datum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  certifyFaultProofFieldCarriageV1,
  fieldPreimageCertificateAddressV1,
  findMissingFaultProofFieldPublicationV1,
  publishFaultProofFieldCarriageV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import type {
  RemoveFraudulentBlockExplicitCategory,
  RemoveFraudulentBlockFraudCategory,
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import { NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1 } from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import type {
  FraudProofWorkflowJournalStoreV1,
  FraudProofWorkflowTerminalV1,
  JournalJsonObjectV1,
} from "../workflow/journal-v1.js";
import {
  createLocalKupmiosHttpOgmiosRawSourceV1,
  type LocalKupmiosHttpOgmiosSourceConfigV1,
} from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import { createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1 } from "../workflow/local-kupmios-raw-l1-authority-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowObservationV1,
  type FraudProofWorkflowPreflightV1,
  type FraudProofWorkflowReconcileResultV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "../workflow/orchestrator-v1.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1V1,
  deriveFraudProofRawL1FamilyStageV1,
  type FraudProofRawL1FamilyDefinitionV1,
  type FraudProofRawL1FamilyStageV1,
  fraudProofRawL1SnapshotRequestForFamilyV1,
} from "../workflow/raw-l1-family-derivation-v1.js";
import {
  createFraudProofAuthenticatedPublicationObserverV1,
  type FraudProofAuthenticatedPublicationObserverV1,
} from "../workflow/raw-l1-publication-observation-v1.js";
import {
  admitFraudProofRawL1SnapshotV1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1,
  type FraudProofRawL1SnapshotAuthorityV1,
} from "../workflow/raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseEconomicsPolicyV1 } from "../workflow/release-economics-policy-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicyV1 } from "../workflow/release-finality-policy-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "../workflow/release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  type FraudProofPreSubmitBoundaryV1,
  LOCAL_UPLC_EVALUATOR_V1,
  type LocallyEvaluatedTransactionV1,
  requireReferenceOnlyScriptWitnessesV1,
  submitCapturedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "../workflow/transaction-boundary-v1.js";
import type { NetworkIdContractsV1 } from "./contracts-v1.js";
import {
  planNetworkIdOutputsOpeningV1,
  type PreparedNetworkIdProofV1,
  prepareNetworkIdFromCanonicalEvidenceV1,
} from "./prepare-v1.js";
import type { NetworkIdCatalogueCategoryV1 } from "./submit-common-v1.js";
import { submitNetworkIdInit } from "./submit-network-id-init.js";
import { submitNetworkIdStep01 } from "./submit-network-id-step-01.js";
import { submitNetworkIdStep02 } from "./submit-network-id-step-02.js";

const ARTIFACT_VERSION = "midgard-network-id-workflow-artifact-v1" as const;
const CATEGORY = "networkId";

type NetworkIdWorkflowArtifactV1 = {
  readonly schemaVersion: typeof ARTIFACT_VERSION;
  readonly headerHash: string;
  readonly expectedNetworkId: "0" | "1";
  readonly badTxId: string;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly outputsItemCbors: readonly string[];
  readonly faultKind: "transaction-network" | "output-network";
  readonly outputIndex: string | null;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

type WorkflowContext = Parameters<
  FraudProofFamilyWorkflowAdapterV1["observe"]
>[0];

type ActionKind =
  | "init"
  | "step01"
  | "step02"
  | "publish_field"
  | "certify_field"
  | "remove";

const requireString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`network-id workflow ${label} must be a non-empty string`);
  }
  return value;
};

const actionKind = (action: FraudProofWorkflowActionV1): ActionKind => {
  const kind = action.input.kind;
  if (
    kind !== "init" &&
    kind !== "step01" &&
    kind !== "step02" &&
    kind !== "publish_field" &&
    kind !== "certify_field" &&
    kind !== "remove"
  ) {
    throw new Error(`network-id workflow action ${action.actionId} is unknown`);
  }
  return kind;
};

const action = (
  kind: ActionKind,
  input: Readonly<Record<string, string>>,
  actionId = `network-id:${kind}:${Object.values(input).join(":")}`,
): FraudProofWorkflowActionV1 => ({
  actionId,
  input: { kind, ...input },
});

const contentActionId = ({
  base,
  entries,
}: {
  readonly base: string;
  readonly entries: WorkflowContext["entries"];
}): string => {
  const confirmations = entries.filter(
    (entry) =>
      entry.event.kind === "confirmed" &&
      (entry.event.actionId === base ||
        entry.event.actionId.startsWith(`${base}:heal:`)),
  ).length;
  return confirmations === 0
    ? base
    : `${base}:heal:${confirmations.toString()}`;
};

const artifactFromPrepared = (
  prepared: PreparedNetworkIdProofV1,
): NetworkIdWorkflowArtifactV1 => ({
  schemaVersion: ARTIFACT_VERSION,
  headerHash: prepared.headerHash,
  expectedNetworkId: prepared.expectedNetworkId.toString() as "0" | "1",
  badTxId: prepared.badTxId,
  nativeTxCanonicalCbor: prepared.nativeTxCanonicalCbor,
  nativeTxCompactCbor: prepared.nativeTxCompactCbor,
  l2TransactionSourceCbor: prepared.txInclusion.l2TransactionSourceCbor,
  outputsItemCbors: prepared.outputsItemCbors,
  faultKind: prepared.faultClaim.kind,
  outputIndex:
    prepared.faultClaim.kind === "output-network"
      ? prepared.faultClaim.outputIndex.toString()
      : null,
  transactionsPhasRoot: prepared.txInclusion.transactionsPhasRoot,
  txMembershipProofCbor: prepared.txInclusion.txMembershipProofCbor,
});

const preparedFromArtifact = (
  value: WorkflowContext["artifact"],
): PreparedNetworkIdProofV1 => {
  const artifact = value as unknown as NetworkIdWorkflowArtifactV1;
  if (artifact.schemaVersion !== ARTIFACT_VERSION) {
    throw new Error("network-id workflow artifact has an unsupported version");
  }
  const expectedNetworkId =
    artifact.expectedNetworkId === "0"
      ? 0n
      : artifact.expectedNetworkId === "1"
        ? 1n
        : undefined;
  if (expectedNetworkId === undefined) {
    throw new Error("network-id workflow artifact has an invalid network id");
  }
  const outputIndex =
    artifact.outputIndex === null ? undefined : BigInt(artifact.outputIndex);
  if (
    (artifact.faultKind === "transaction-network" &&
      outputIndex !== undefined) ||
    (artifact.faultKind === "output-network" && outputIndex === undefined)
  ) {
    throw new Error("network-id workflow artifact has an inconsistent fault");
  }
  const fault: NetworkIdFaultV1 =
    artifact.faultKind === "transaction-network"
      ? "TransactionNetwork"
      : { OutputNetwork: { output_index: outputIndex! } };
  return {
    headerHash: requireString(artifact.headerHash, "header hash"),
    expectedNetworkId,
    badTxId: requireString(artifact.badTxId, "transaction id"),
    nativeTxCanonicalCbor: requireString(
      artifact.nativeTxCanonicalCbor,
      "canonical transaction",
    ),
    nativeTxCompactCbor: requireString(
      artifact.nativeTxCompactCbor,
      "compact transaction",
    ),
    outputsItemCbors: [...artifact.outputsItemCbors],
    faultClaim:
      artifact.faultKind === "transaction-network"
        ? { kind: "transaction-network" }
        : { kind: "output-network", outputIndex: outputIndex! },
    fault,
    txInclusion: {
      nativeTxId: artifact.badTxId,
      nativeTx: nativeTxFromCoreCompact(
        decodeMidgardNativeTxCompactV1(
          Buffer.from(artifact.nativeTxCompactCbor, "hex"),
        ),
      ),
      nativeTxCompactCbor: artifact.nativeTxCompactCbor,
      l2TransactionSourceCbor: requireString(
        artifact.l2TransactionSourceCbor,
        "transaction source",
      ),
      transactionsPhasRoot: artifact.transactionsPhasRoot,
      txMembershipProofCbor: artifact.txMembershipProofCbor,
    },
  };
};

const confirmedTxHash = (
  context: WorkflowContext,
  actionId: string,
): string | undefined => {
  for (const entry of [...context.entries].reverse()) {
    const event = entry.event;
    if (event.kind === "confirmed" && event.actionId === actionId) {
      return event.txHash;
    }
  }
  return undefined;
};

const latestRemovalIntent = (context: WorkflowContext) =>
  [...context.entries]
    .reverse()
    .map((entry) => entry.event)
    .find(
      (event) =>
        event.kind === "submission_intent" &&
        event.actionInput.kind === "remove",
    );

const parseMutationLeaseRecoveryV1 = (
  recovery: JournalJsonObjectV1 | undefined,
): { readonly token: string; readonly source: string } | undefined => {
  if (recovery === undefined) return undefined;
  const value = recovery.stateQueueMutationLease;
  if (
    Object.keys(recovery).length !== 1 ||
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value)
  ) {
    throw new Error(
      "network-id durable recovery has an invalid mutation-lease shape",
    );
  }
  const parsed = value as Readonly<Record<string, unknown>>;
  if (
    Object.keys(parsed).sort().join(",") !== "source,token" ||
    typeof parsed.token !== "string" ||
    parsed.token.trim() === "" ||
    parsed.token.trim() !== parsed.token ||
    typeof parsed.source !== "string" ||
    parsed.source.trim() === "" ||
    parsed.source.trim() !== parsed.source
  ) {
    throw new Error("network-id durable mutation-lease identity is malformed");
  }
  return { token: parsed.token, source: parsed.source };
};

const recoverMutationLeaseV1 = async ({
  config,
  txHash,
  durableRecovery,
  mutationLeaseByTxHash,
}: {
  readonly config: NetworkIdWorkflowAdapterConfigV1;
  readonly txHash: string;
  readonly durableRecovery: JournalJsonObjectV1 | undefined;
  readonly mutationLeaseByTxHash: Map<string, StateQueueMutationLease>;
}): Promise<
  | { readonly kind: "ok"; readonly lease: StateQueueMutationLease | undefined }
  | { readonly kind: "conflict"; readonly reason: string }
> => {
  let identity: { readonly token: string; readonly source: string } | undefined;
  try {
    identity = parseMutationLeaseRecoveryV1(durableRecovery);
  } catch (cause) {
    return {
      kind: "conflict",
      reason: cause instanceof Error ? cause.message : String(cause),
    };
  }
  if (identity === undefined) return { kind: "ok", lease: undefined };
  const cached = mutationLeaseByTxHash.get(txHash);
  if (cached !== undefined) {
    if (cached.token !== identity.token || cached.source !== identity.source) {
      return {
        kind: "conflict",
        reason: "network-id cached mutation lease changed its fencing identity",
      };
    }
    return { kind: "ok", lease: cached };
  }
  const resume = config.removal.stateQueueMutationLeaseCoordinator?.resume;
  if (resume === undefined) {
    return {
      kind: "conflict",
      reason:
        "network-id mutation-lease coordinator cannot resume the journaled fencing token",
    };
  }
  try {
    const lease = await resume(identity);
    mutationLeaseByTxHash.set(txHash, lease);
    return { kind: "ok", lease };
  } catch (cause) {
    return {
      kind: "conflict",
      reason: `journaled network-id mutation lease cannot be resumed: ${String(cause)}`,
    };
  }
};

export type NetworkIdWorkflowTerminalFactsV1 = {
  readonly economics: FraudProofWorkflowTerminalV1["economics"];
  readonly observedAt: FraudProofWorkflowTerminalV1["observedAt"];
};

export interface NetworkIdRawL1ObservationPortV1 {
  readonly publications?: FraudProofAuthenticatedPublicationObserverV1;
  observeHeader?(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservationV1>;
  transactionConfirmed?(input: {
    readonly headerHash: string;
    readonly txHash: string;
  }): Promise<boolean>;
  observe(input: {
    readonly headerHash: string;
  }): Promise<FraudProofRawL1FamilyStageV1>;
}

export const createNetworkIdRawL1ObservationPortV1 = ({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthorityV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1;
  readonly definition: FraudProofRawL1FamilyDefinitionV1 & {
    readonly category: "networkId";
  };
}): NetworkIdRawL1ObservationPortV1 => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1 ||
    definition.computationThread.steps.length !== 2
  ) {
    throw new Error("network-id raw L1 observation authority is incomplete");
  }
  const request = fraudProofRawL1SnapshotRequestForFamilyV1({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error("network-id raw L1 observation changed the header");
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
      return await deriveFraudProofRawL1FamilyStageV1({
        snapshot,
        definition,
        releaseEconomics,
      });
    },
  };
};

/** Concrete loopback Kupo HTTP + Ogmios WS production construction. */
export const createNetworkIdLocalKupmiosL1ObservationPortV1 = ({
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
    readonly category: "networkId";
  };
}): NetworkIdRawL1ObservationPortV1 => {
  const rawSource = createLocalKupmiosHttpOgmiosRawSourceV1({
    ...source,
    releaseFinality,
  });
  return createNetworkIdRawL1ObservationPortV1({
    authority: createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1({
      source: rawSource,
      releaseFinality,
    }),
    releaseFinality,
    releaseEconomics,
    definition,
  });
};

/** Independent second raw-L1 observation for terminal admission. */
export const createNetworkIdAuthenticatedL1TerminalVerifierV1 = (
  l1: NetworkIdRawL1ObservationPortV1,
): FraudProofWorkflowTerminalVerifierV1 => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  verify: async ({ identity, candidate, releaseFinality }) => {
    if (identity.target.kind !== "state_queue_header") {
      throw new Error(
        "network-id terminal requires a state-queue header target",
      );
    }
    const stage = await l1.observe({
      headerHash: identity.target.headerHash,
    });
    if (stage.kind !== "removed") {
      throw new Error(
        "authenticated L1 still reports unfinished network-id correction",
      );
    }
    if (JSON.stringify(stage.terminal) !== JSON.stringify(candidate)) {
      throw new Error(
        "network-id terminal candidate differs from independent L1 observation",
      );
    }
    if (
      stage.terminal.observedAt.confirmationDepth <
      releaseFinality.policy.confirmationDepth
    ) {
      throw new Error(
        `authenticated network-id terminal depth is below release finality: required=${releaseFinality.policy.confirmationDepth.toString()} actual=${stage.terminal.observedAt.confirmationDepth.toString()}`,
      );
    }
    return stage.terminal;
  },
});

export type NetworkIdWorkflowAdapterConfigV1 = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: NetworkIdContractsV1;
  readonly stateQueueAddress: string;
  readonly category: NetworkIdCatalogueCategoryV1;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly removal: {
    readonly deploymentInfo: unknown;
    readonly category:
      | RemoveFraudulentBlockFraudCategory
      | RemoveFraudulentBlockExplicitCategory;
    readonly requireReferenceScripts?: boolean;
    readonly validFrom?: bigint;
    readonly validTo?: bigint;
    /** Legacy normalized-provider route only. Production derives topology raw. */
    readonly isCurrentHead?: (headerHash: string) => Promise<boolean>;
    /** Required by the production raw-L1 route for descendant fencing. */
    readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  };
  /** Strict production route; omit only in emulator/diagnostic construction. */
  readonly rawL1?: NetworkIdRawL1ObservationPortV1;
  /** Candidate chain facts; the shared independent verifier reauthenticates them. */
  readonly terminalFacts?: (input: {
    readonly headerHash: string;
    readonly removalTxHash: string;
    readonly proofTokenOutRef: string;
  }) => Promise<NetworkIdWorkflowTerminalFactsV1>;
};

type NetworkIdRemovalConfigV1 = NetworkIdWorkflowAdapterConfigV1["removal"];

export type ManifestBoundNetworkIdWorkflowConfigV1 = Omit<
  NetworkIdWorkflowAdapterConfigV1,
  | "blueprint"
  | "network"
  | "contracts"
  | "stateQueueAddress"
  | "category"
  | "catalogue"
  | "removal"
  | "rawL1"
  | "terminalFacts"
  | "witnessReferenceScripts"
> & {
  readonly manifest: unknown;
  readonly blueprintJson: string;
  readonly deploymentInfo: unknown;
  readonly headerHash: string;
  readonly source: Omit<
    LocalKupmiosHttpOgmiosSourceConfigV1,
    "releaseFinality"
  >;
  readonly removal: Omit<
    NetworkIdRemovalConfigV1,
    | "deploymentInfo"
    | "category"
    | "isCurrentHead"
    | "requireReferenceScripts"
    | "stateQueueMutationLeaseCoordinator"
  > & {
    readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  };
  readonly witnessReferenceScripts: Required<
    Pick<
      FaultProofWitnessReferenceScriptsV1,
      | "computationThreadMint"
      | "fraudProofMint"
      | "phasMembershipWithdraw"
      | "chunkedVerifyWithdraw"
      | "pexcludesWithdraw"
    >
  >;
};

export type ManifestBoundNetworkIdWorkflowV1 = {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"networkId">;
  readonly adapterConfig: NetworkIdWorkflowAdapterConfigV1;
  readonly adapter: FraudProofFamilyWorkflowAdapterV1;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
};

export type ManifestBoundNetworkIdRuntimeSealV1 = {
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly removal: ManifestBoundNetworkIdWorkflowConfigV1["removal"] & {
    readonly requireReferenceScripts: true;
  };
};

/**
 * Pure manifest/runtime seal used before the production adapter is built.
 * Runtime objects may contain extra JavaScript properties despite their
 * TypeScript shape, so the reference-script-only removal flag is overwritten
 * after the caller object is spread. Every supplied reference UTxO is also
 * matched to its exact finalized manifest role, out-ref, and script hash.
 */
export const sealManifestBoundNetworkIdRuntimeV1 = ({
  binding,
  signer,
  stepReferenceScripts,
  fieldPreimageCertificateReferenceScript,
  witnessReferenceScripts,
  removal,
}: {
  readonly binding: Pick<
    FraudProofWorkflowDeploymentBindingV1<"networkId">,
    "network" | "referenceScriptsByContract"
  >;
  readonly signer: ResolvedProverSigner;
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: ManifestBoundNetworkIdWorkflowConfigV1["witnessReferenceScripts"];
  readonly removal: ManifestBoundNetworkIdWorkflowConfigV1["removal"];
}): ManifestBoundNetworkIdRuntimeSealV1 => {
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: signer.address,
    paymentKeyHash: signer.paymentKeyHash,
  });
  const requireReference = (contractName: string, utxo: UTxO): UTxO =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo,
    });
  return {
    stepReferenceScripts: [
      requireReference("fraudProofNetworkId", stepReferenceScripts[0]),
      requireReference("fraudProofNetworkIdStep02", stepReferenceScripts[1]),
    ],
    fieldPreimageCertificateReferenceScript: requireReference(
      "fieldPreimageCertificateMint",
      fieldPreimageCertificateReferenceScript,
    ),
    witnessReferenceScripts: {
      computationThreadMint: requireReference(
        "computationThreadMint",
        witnessReferenceScripts.computationThreadMint,
      ),
      fraudProofMint: requireReference(
        "fraudProofMint",
        witnessReferenceScripts.fraudProofMint,
      ),
      phasMembershipWithdraw: requireReference(
        "phasMembershipWithdraw",
        witnessReferenceScripts.phasMembershipWithdraw,
      ),
      chunkedVerifyWithdraw: requireReference(
        "chunkedVerifyWithdraw",
        witnessReferenceScripts.chunkedVerifyWithdraw,
      ),
      pexcludesWithdraw: requireReference(
        "pexcludesWithdraw",
        witnessReferenceScripts.pexcludesWithdraw,
      ),
    },
    removal: {
      ...removal,
      requireReferenceScripts: true,
    },
  };
};

/** Manifest-closed production construction for Q35. */
export const createManifestBoundNetworkIdWorkflowV1 = async (
  config: ManifestBoundNetworkIdWorkflowConfigV1,
): Promise<ManifestBoundNetworkIdWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "networkId",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      NetworkIdStep02Datum,
    ],
  });
  const resolved = binding.resolvedContracts;
  const networkIdContracts = resolved.contracts.networkId;
  if (networkIdContracts === undefined) {
    throw new Error("network-id deployment resolved a different family chain");
  }
  const certificate = binding.fieldPreimageCertificate;
  if (certificate === null) {
    throw new Error(
      "network-id deployment omitted the field-preimage certificate policy",
    );
  }
  const sealedRuntime = sealManifestBoundNetworkIdRuntimeV1({
    binding,
    signer: config.signer,
    stepReferenceScripts: config.stepReferenceScripts,
    fieldPreimageCertificateReferenceScript:
      config.fieldPreimageCertificateReferenceScript,
    witnessReferenceScripts: config.witnessReferenceScripts,
    removal: config.removal,
  });
  const rawL1 = createNetworkIdLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const adapterConfig: NetworkIdWorkflowAdapterConfigV1 = {
    lucid: config.lucid,
    blueprint: binding.blueprint,
    network: binding.network,
    contracts: {
      steps: networkIdContracts.steps,
      expectedNetworkId: binding.network === "Mainnet" ? 1n : 0n,
      computationThread: {
        policyId: resolved.contracts.computationThread.policyId,
        mintingScript: resolved.contracts.computationThread.mintingScript,
      },
      fraudProof: {
        policyId: resolved.contracts.fraudProof.policyId,
        mintingScript: resolved.contracts.fraudProof.mintingScript,
        spendingScriptAddress:
          resolved.contracts.fraudProof.spendingScriptAddress,
      },
      hubOraclePolicyId: resolved.hubOraclePolicyId,
      stateQueuePolicyId: binding.definition.stateQueue.policyId,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      fieldPreimageCertificateMintingScript: certificate.mintingScript,
    },
    stateQueueAddress: binding.definition.stateQueue.address,
    category: resolved.category,
    catalogue: binding.catalogue,
    signer: config.signer,
    stepReferenceScripts: sealedRuntime.stepReferenceScripts,
    fieldPreimageCertificateReferenceScript:
      sealedRuntime.fieldPreimageCertificateReferenceScript,
    witnessReferenceScripts: sealedRuntime.witnessReferenceScripts,
    removal: {
      ...sealedRuntime.removal,
      deploymentInfo: binding.deploymentInfo,
      category: "networkId",
      requireReferenceScripts: true,
    },
    rawL1,
  };
  return {
    binding,
    adapterConfig,
    adapter: createNetworkIdWorkflowAdapterV1(adapterConfig),
    terminalVerifier: createNetworkIdAuthenticatedL1TerminalVerifierV1(rawL1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  };
};

/**
 * Concrete last-header adapter. It captures the actual signed transaction at
 * the post-local-evaluation/pre-network boundary, journals that hash through
 * the shared orchestrator, then submits exactly the captured body.
 */
export const createNetworkIdWorkflowAdapterV1 = (
  config: NetworkIdWorkflowAdapterConfigV1,
): FraudProofFamilyWorkflowAdapterV1 => {
  const captured = new Map<
    string,
    {
      readonly transaction: LocallyEvaluatedTransactionV1;
      readonly mutationLease?: StateQueueMutationLease;
    }
  >();
  const mutationLeaseByTxHash = new Map<string, StateQueueMutationLease>();
  const category = CATEGORY;

  const live = async (prepared: PreparedNetworkIdProofV1) => {
    const threadUnit = toUnit(
      config.contracts.computationThread.policyId,
      `${config.category.categoryId}${prepared.headerHash}`,
    );
    const proofUnit = toUnit(
      config.contracts.fraudProof.policyId,
      `${config.category.categoryId}${prepared.headerHash}`,
    );
    const stateQueueUnit = toUnit(
      config.contracts.stateQueuePolicyId,
      `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${prepared.headerHash}`,
    );
    const [step01, step02, proofs, stateQueue] = await Promise.all([
      config.lucid.utxosAtWithUnit(
        config.contracts.steps[0].spendingScriptAddress,
        threadUnit,
      ),
      config.lucid.utxosAtWithUnit(
        config.contracts.steps[1].spendingScriptAddress,
        threadUnit,
      ),
      config.lucid.utxosAtWithUnit(
        config.contracts.fraudProof.spendingScriptAddress,
        proofUnit,
      ),
      config.lucid.utxosAtWithUnit(config.stateQueueAddress, stateQueueUnit),
    ]);
    for (const [label, utxos] of [
      ["step-01", step01],
      ["step-02", step02],
      ["proof", proofs],
      ["state-queue", stateQueue],
    ] as const) {
      if (utxos.length > 1) {
        throw new Error(`network-id workflow found duplicate ${label} UTxOs`);
      }
    }
    return {
      threadUnit,
      proofUnit,
      step01: step01[0],
      step02: step02[0],
      proof: proofs[0],
      stateQueue: stateQueue[0],
    };
  };

  const authenticateFieldInputs = async ({
    prepared,
    publications,
    certificate,
  }: {
    readonly prepared: PreparedNetworkIdProofV1;
    readonly publications: readonly UTxO[];
    readonly certificate?: UTxO;
  }): Promise<void> => {
    if (config.rawL1 === undefined) return;
    const observer = config.rawL1.publications;
    if (observer === undefined) {
      throw new Error(
        "production network-id field inputs require authenticated publication observation",
      );
    }
    for (const publication of publications) {
      if (publication.datum == null) {
        throw new Error(
          "network-id field publication omitted its inline datum",
        );
      }
      const observed = await observer.observeExact({
        headerHash: prepared.headerHash,
        kind: "field_publication",
        address: config.signer.address,
        expectedOutRef: outRefLabel(publication),
        expectedDatumCbor: publication.datum,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `network-id field publication ${outRefLabel(publication)} is not release-final`,
        );
      }
    }
    if (certificate !== undefined) {
      if (certificate.datum == null) {
        throw new Error(
          "network-id field certificate omitted its inline datum",
        );
      }
      const observed = await observer.observeExact({
        headerHash: prepared.headerHash,
        kind: "field_certificate",
        address: fieldPreimageCertificateAddressV1({
          network: config.network,
          certificatePolicyId:
            config.contracts.fieldPreimageCertificatePolicyId,
        }),
        expectedOutRef: outRefLabel(certificate),
        expectedDatumCbor: certificate.datum,
        expectedUnit: `${config.contracts.fieldPreimageCertificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1}`,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `network-id field certificate ${outRefLabel(certificate)} is not release-final`,
        );
      }
    }
  };

  const observe = async (
    context: WorkflowContext,
  ): Promise<FraudProofWorkflowObservationV1> => {
    const prepared = preparedFromArtifact(context.artifact);
    const rawStage = await config.rawL1?.observe({
      headerHash: prepared.headerHash,
    });
    if (rawStage?.kind === "removed") {
      return { kind: "completed", terminal: rawStage.terminal };
    }
    const state = rawStage === undefined ? await live(prepared) : undefined;
    const proofUnit =
      state?.proofUnit ??
      toUnit(
        config.contracts.fraudProof.policyId,
        `${config.category.categoryId}${prepared.headerHash}`,
      );
    const stateQueueOutRef =
      rawStage === undefined
        ? state?.stateQueue === undefined
          ? undefined
          : outRefLabel(state.stateQueue)
        : rawStage.stateQueueBlockOutRef;
    const step01OutRef =
      rawStage?.kind === "step" && rawStage.step === 1
        ? rawStage.threadOutRef
        : state?.step01 === undefined
          ? undefined
          : outRefLabel(state.step01);
    const step02OutRef =
      rawStage?.kind === "step" && rawStage.step === 2
        ? rawStage.threadOutRef
        : state?.step02 === undefined
          ? undefined
          : outRefLabel(state.step02);
    const proofOutRef =
      rawStage?.kind === "proof_token"
        ? rawStage.fraudProofOutRef
        : state?.proof === undefined
          ? undefined
          : outRefLabel(state.proof);
    if (stateQueueOutRef === undefined) {
      if (proofOutRef === undefined) {
        return {
          kind: "conflict",
          reason:
            "fraudulent header disappeared without the permanent Q35 proof token",
        };
      }
      const intent = latestRemovalIntent(context);
      if (intent === undefined || intent.kind !== "submission_intent") {
        return {
          kind: "conflict",
          reason:
            "fraudulent header disappeared without a journaled removal intent",
        };
      }
      const removalTxHash = confirmedTxHash(context, intent.actionId);
      if (removalTxHash === undefined) {
        return {
          kind: "conflict",
          reason:
            "confirmed removal facts are unavailable for terminal authentication",
        };
      }
      if (state?.proof === undefined) {
        return {
          kind: "conflict",
          reason: "raw L1 removal did not provide a derived terminal",
        };
      }
      if (config.terminalFacts === undefined) {
        return {
          kind: "conflict",
          reason:
            "legacy network-id observation has no authenticated terminal-facts authority",
        };
      }
      const facts = await config.terminalFacts({
        headerHash: prepared.headerHash,
        removalTxHash,
        proofTokenOutRef: proofOutRef,
      });
      return {
        kind: "completed",
        terminal: {
          schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
          category,
          headerHash: prepared.headerHash,
          proofToken: {
            unit: proofUnit,
            outRef: proofOutRef,
            createdByTxHash: state.proof.txHash,
            retainedAtFinalState: true,
          },
          correction: {
            removalTxHash,
            removedStateQueueOutRef: requireString(
              intent.actionInput.stateQueueBlockOutRef,
              "removed state-queue out-ref",
            ),
            fraudulentHeaderAbsent: true,
            referencedProofTokenOutRef: proofOutRef,
          },
          economics: facts.economics,
          observedAt: facts.observedAt,
        },
      };
    }
    if (proofOutRef !== undefined) {
      if (
        rawStage === undefined &&
        (config.removal.isCurrentHead === undefined ||
          !(await config.removal.isCurrentHead(prepared.headerHash)))
      ) {
        return {
          kind: "conflict",
          reason:
            "network-id adapter requires one journal action per descendant removal; target is not the current head",
        };
      }
      return {
        kind: "action_required",
        action: action("remove", {
          stateQueueBlockOutRef:
            rawStage?.kind === "proof_token"
              ? rawStage.nextRemovalOutRef
              : stateQueueOutRef,
          targetStateQueueBlockOutRef: stateQueueOutRef,
          proofOutRef,
          requiresMutationLease:
            rawStage?.kind === "proof_token" &&
            rawStage.nextRemovalOutRef !== rawStage.stateQueueBlockOutRef
              ? "true"
              : "false",
        }),
      };
    }
    if (step02OutRef !== undefined) {
      if (prepared.faultClaim.kind === "output-network") {
        const opening = planNetworkIdOutputsOpeningV1({
          prepared,
          owner: config.signer.paymentKeyHash,
        });
        const missing = await findMissingFaultProofFieldPublicationV1({
          lucid: config.lucid,
          publisherAddress: config.signer.address,
          planned: opening,
        });
        if (missing !== undefined) {
          const base = `network-id:publish-field:${opening.commitment}:${missing.digest}`;
          return {
            kind: "action_required",
            action: action(
              "publish_field",
              {
                threadOutRef: step02OutRef,
                fieldCommitment: opening.commitment,
                publicationDatumCbor: missing.datumCbor,
              },
              contentActionId({ base, entries: context.entries }),
            ),
          };
        }
        if (opening.plan.tier === "Certified") {
          const certificate = await resolveFaultProofFieldPreimageCertificateV1(
            {
              lucid: config.lucid,
              network: config.network,
              planned: opening,
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
            },
          );
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublicationsV1({
                lucid: config.lucid,
                publisherAddress: config.signer.address,
                planned: opening,
              });
            if (publications === undefined) {
              throw new Error(
                "network-id tier-3 publications disappeared before certification",
              );
            }
            const base = `network-id:certify-field:${opening.commitment}`;
            return {
              kind: "action_required",
              action: action(
                "certify_field",
                {
                  threadOutRef: step02OutRef,
                  fieldCommitment: opening.commitment,
                  chunkOutRefs: publications
                    .map((utxo) => outRefLabel(utxo))
                    .join(","),
                },
                contentActionId({ base, entries: context.entries }),
              ),
            };
          }
          return {
            kind: "action_required",
            action: action("step02", {
              threadOutRef: step02OutRef,
              certificateOutRef: outRefLabel(certificate),
            }),
          };
        }
      }
      return {
        kind: "action_required",
        action: action("step02", { threadOutRef: step02OutRef }),
      };
    }
    if (step01OutRef !== undefined) {
      return {
        kind: "action_required",
        action: action("step01", {
          threadOutRef: step01OutRef,
          stateQueueBlockOutRef: stateQueueOutRef,
        }),
      };
    }
    return {
      kind: "action_required",
      action: action("init", {
        fraudulentBlockOutRef: stateQueueOutRef,
      }),
    };
  };

  return {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
    prepare: async ({
      evidence,
    }: {
      readonly evidence: CanonicalBlockEvidenceV1;
    }): Promise<JournalJsonObjectV1> =>
      artifactFromPrepared(
        await prepareNetworkIdFromCanonicalEvidenceV1({
          evidence,
          expectedNetworkId: config.contracts.expectedNetworkId,
        }),
      ),
    observe,
    preflight: async (context): Promise<FraudProofWorkflowPreflightV1> => {
      const prepared = preparedFromArtifact(context.artifact);
      const kind = actionKind(context.action);
      let mutationLease: StateQueueMutationLease | undefined;
      const boundaryInvocation = async (
        boundary: FraudProofPreSubmitBoundaryV1,
      ) => {
        if (kind === "init") {
          await submitNetworkIdInit({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            category: config.category,
            catalogue: config.catalogue,
            signer: config.signer,
            fraudulentBlockOutRef: requireString(
              context.action.input.fraudulentBlockOutRef,
              "init block out-ref",
            ),
            witnessReferenceScripts: config.witnessReferenceScripts,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "step01") {
          await submitNetworkIdStep01({
            lucid: config.lucid,
            blueprint: config.blueprint,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "step-01 thread out-ref",
            ),
            stateQueueBlockOutRef: requireString(
              context.action.input.stateQueueBlockOutRef,
              "step-01 state-queue out-ref",
            ),
            prepared,
            referenceScriptUtxo: config.stepReferenceScripts[0],
            witnessReferenceScripts: config.witnessReferenceScripts,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "publish_field" || kind === "certify_field") {
          if (prepared.faultClaim.kind !== "output-network") {
            throw new Error(
              "transaction-network faults have no field-carriage action",
            );
          }
          const opening = planNetworkIdOutputsOpeningV1({
            prepared,
            owner: config.signer.paymentKeyHash,
          });
          if (context.action.input.fieldCommitment !== opening.commitment) {
            throw new Error(
              "network-id field action does not match the prepared opening",
            );
          }
          if (kind === "publish_field") {
            const missing = await findMissingFaultProofFieldPublicationV1({
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            });
            if (
              missing === undefined ||
              context.action.input.publicationDatumCbor !== missing.datumCbor
            ) {
              throw new Error(
                "network-id publication action is not the next missing plan chunk",
              );
            }
            await publishFaultProofFieldCarriageV1({
              lucid: config.lucid,
              signer: config.signer,
              planned: opening,
              publisherAddress: config.signer.address,
              label: "network-id step-02 outputs",
              preSubmitBoundary: boundary,
            });
            return;
          }
          if (opening.plan.tier !== "Certified") {
            throw new Error(
              "network-id certification action requires tier-3 carriage",
            );
          }
          const publications =
            await resolveFaultProofFieldCarriagePublicationsV1({
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            });
          if (publications === undefined) {
            throw new Error(
              "network-id tier-3 publications are not observable on L1",
            );
          }
          await authenticateFieldInputs({ prepared, publications });
          if (
            context.action.input.chunkOutRefs !==
            publications.map((utxo) => outRefLabel(utxo)).join(",")
          ) {
            throw new Error(
              "network-id certification action changed the observed chunks",
            );
          }
          await certifyFaultProofFieldCarriageV1({
            lucid: config.lucid,
            network: config.network,
            signer: config.signer,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              config.contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              config.fieldPreimageCertificateReferenceScript,
            chunkUtxos: publications,
            compactCbor: prepared.nativeTxCompactCbor,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "step02") {
          const opening =
            prepared.faultClaim.kind === "output-network"
              ? planNetworkIdOutputsOpeningV1({
                  prepared,
                  owner: config.signer.paymentKeyHash,
                })
              : undefined;
          const publications =
            opening === undefined
              ? []
              : await resolveFaultProofFieldCarriagePublicationsV1({
                  lucid: config.lucid,
                  publisherAddress: config.signer.address,
                  planned: opening,
                });
          if (publications === undefined) {
            throw new Error(
              "network-id field publications are not observable on L1",
            );
          }
          const certificate =
            opening?.plan.tier === "Certified"
              ? await resolveFaultProofFieldPreimageCertificateV1({
                  lucid: config.lucid,
                  network: config.network,
                  planned: opening,
                  certificatePolicyId:
                    config.contracts.fieldPreimageCertificatePolicyId,
                })
              : undefined;
          if (
            opening?.plan.tier === "Certified" &&
            (certificate === undefined ||
              context.action.input.certificateOutRef !==
                outRefLabel(certificate))
          ) {
            throw new Error(
              "network-id final step does not bind the observed field certificate",
            );
          }
          await authenticateFieldInputs({
            prepared,
            publications,
            ...(certificate === undefined ? {} : { certificate }),
          });
          await submitNetworkIdStep02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "step-02 thread out-ref",
            ),
            prepared,
            ...(opening === undefined ? {} : { outputsOpeningPlan: opening }),
            ...(certificate === undefined
              ? {}
              : { certificateUtxos: [certificate] }),
            referenceScriptUtxo: config.stepReferenceScripts[1],
            witnessReferenceScripts: config.witnessReferenceScripts,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (
          config.rawL1 !== undefined &&
          config.removal.stateQueueMutationLeaseCoordinator === undefined
        ) {
          throw new Error(
            "production network-id removal requires a state-queue mutation-lease coordinator",
          );
        }
        const retainingCoordinator:
          | StateQueueMutationLeaseCoordinator
          | undefined =
          config.removal.stateQueueMutationLeaseCoordinator === undefined
            ? undefined
            : {
                acquire: async () => {
                  mutationLease =
                    await config.removal.stateQueueMutationLeaseCoordinator!.acquire();
                  return mutationLease;
                },
              };
        await submitRemoveFraudulentBlock({
          lucid: config.lucid,
          blueprint: config.blueprint,
          deploymentInfo: config.removal.deploymentInfo,
          network: config.network,
          signer: config.signer,
          fraudCategory: config.removal.category,
          fraudulentHeaderHash: prepared.headerHash,
          requireReferenceScripts:
            config.removal.requireReferenceScripts ?? true,
          ...(retainingCoordinator === undefined
            ? {}
            : { stateQueueMutationLeaseCoordinator: retainingCoordinator }),
          ...(config.removal.validFrom === undefined
            ? {}
            : { validFrom: config.removal.validFrom }),
          ...(config.removal.validTo === undefined
            ? {}
            : { validTo: config.removal.validTo }),
          preSubmitBoundary: async (transaction) => {
            if (
              !workflowTransactionInputOutRefsV1(transaction.signed).includes(
                requireString(
                  context.action.input.stateQueueBlockOutRef,
                  "next removal out-ref",
                ),
              )
            ) {
              throw new Error(
                "network-id removal does not consume the authenticated next state-queue outRef",
              );
            }
            if (
              !workflowTransactionReferenceInputOutRefsV1(
                transaction.signed,
              ).includes(
                requireString(
                  context.action.input.proofOutRef,
                  "permanent proof-token out-ref",
                ),
              )
            ) {
              throw new Error(
                "network-id removal does not reference the authenticated permanent proof token",
              );
            }
            await boundary(transaction);
          },
          awaitConfirmation: false,
        });
      };
      const transaction =
        await captureLocallyEvaluatedTransactionV1(boundaryInvocation);
      requireReferenceOnlyScriptWitnessesV1({
        transaction,
        label: "network-id production transaction",
      });
      const requiresMutationLease =
        context.action.input.requiresMutationLease === "true";
      if (
        kind === "remove" &&
        requiresMutationLease !== (mutationLease !== undefined)
      ) {
        await mutationLease?.fail(
          "authenticated network-id removal topology disagreed with lease requirement",
        );
        throw new Error(
          "authenticated network-id removal topology disagreed with mutation-lease acquisition",
        );
      }
      captured.set(`${context.workflowId}:${context.action.actionId}`, {
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
      return {
        actionId: context.action.actionId,
        txHash: transaction.txHash,
        scriptExecution: "reference_scripts",
        localUplcEvaluation: {
          status: "passed",
          evaluator: LOCAL_UPLC_EVALUATOR_V1,
        },
        referenceScripts: transaction.referenceScripts,
        ...(mutationLease === undefined
          ? {}
          : {
              durableRecovery: {
                stateQueueMutationLease: {
                  token: mutationLease.token,
                  source: mutationLease.source,
                },
              },
            }),
      };
    },
    submit: async (context) => {
      const key = `${context.workflowId}:${context.action.actionId}`;
      const prepared = captured.get(key);
      if (
        prepared === undefined ||
        prepared.transaction.txHash !== context.preflight.txHash
      ) {
        return {
          kind: "ambiguous",
          detail:
            "captured locally evaluated transaction is unavailable or differs from the journaled preflight hash",
        };
      }
      const recovery = parseMutationLeaseRecoveryV1(
        context.preflight.durableRecovery,
      );
      if (
        (prepared.mutationLease === undefined) !== (recovery === undefined) ||
        (prepared.mutationLease !== undefined &&
          (prepared.mutationLease.token !== recovery?.token ||
            prepared.mutationLease.source !== recovery.source))
      ) {
        throw new Error(
          "network-id cached mutation lease differs from durable intent",
        );
      }
      try {
        const txHash = await submitCapturedTransactionV1(prepared.transaction);
        captured.delete(key);
        if (prepared.mutationLease !== undefined) {
          mutationLeaseByTxHash.set(txHash, prepared.mutationLease);
        }
        return { kind: "submitted", txHash };
      } catch (cause) {
        return {
          kind: "ambiguous",
          txHash: prepared.transaction.txHash,
          detail: cause instanceof Error ? cause.message : String(cause),
        };
      }
    },
    reconcile: async (
      context,
    ): Promise<FraudProofWorkflowReconcileResultV1> => {
      const prepared = preparedFromArtifact(context.artifact);
      const kind = actionKind(context.action);
      let advanced: boolean;
      if (kind === "publish_field" || kind === "certify_field") {
        if (prepared.faultClaim.kind !== "output-network") {
          return {
            kind: "conflict",
            reason: "field action exists for a transaction-network fault",
          };
        }
        const opening = planNetworkIdOutputsOpeningV1({
          prepared,
          owner: config.signer.paymentKeyHash,
        });
        if (context.action.input.fieldCommitment !== opening.commitment) {
          return {
            kind: "conflict",
            reason: "field action commitment changed during reconciliation",
          };
        }
        if (context.txHash === undefined) {
          return {
            kind: "conflict",
            reason: "publication reconciliation omitted the intended tx hash",
          };
        }
        const observer = config.rawL1?.publications;
        if (observer === undefined) {
          return {
            kind: "conflict",
            reason:
              "production publication reconciliation has no authenticated raw-L1 observer",
          };
        }
        if (kind === "publish_field") {
          const candidates = await config.lucid.utxosAt(config.signer.address);
          const candidate = candidates.find(
            (utxo) =>
              utxo.txHash === context.txHash &&
              utxo.datum === context.action.input.publicationDatumCbor,
          );
          if (candidate === undefined) return { kind: "not_found" };
          const observation = await observer.observeExact({
            headerHash: prepared.headerHash,
            kind: "field_publication",
            address: config.signer.address,
            expectedOutRef: outRefLabel(candidate),
            expectedDatumCbor: requireString(
              context.action.input.publicationDatumCbor,
              "publication datum",
            ),
          });
          advanced = observation.kind === "confirmed";
        } else {
          const certificate = await resolveFaultProofFieldPreimageCertificateV1(
            {
              lucid: config.lucid,
              network: config.network,
              planned: opening,
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
            },
          );
          if (
            certificate === undefined ||
            certificate.txHash !== context.txHash
          ) {
            return { kind: "not_found" };
          }
          const certification = deriveFieldPreimageCertificationV1(
            opening.plan,
          );
          const observation = await observer.observeExact({
            headerHash: prepared.headerHash,
            kind: "field_certificate",
            address: fieldPreimageCertificateAddressV1({
              network: config.network,
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
            }),
            expectedOutRef: outRefLabel(certificate),
            expectedDatumCbor: certification.datumCbor,
            expectedUnit: `${config.contracts.fieldPreimageCertificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1}`,
          });
          advanced = observation.kind === "confirmed";
        }
      } else if (config.rawL1 !== undefined) {
        if (
          context.txHash === undefined ||
          config.rawL1.transactionConfirmed === undefined
        ) {
          return {
            kind: "conflict",
            reason:
              "production network-id reconciliation requires an intended tx hash and authenticated transaction history",
          };
        }
        const rawStage = await config.rawL1.observe({
          headerHash: prepared.headerHash,
        });
        const stageAdvanced =
          kind === "init"
            ? rawStage.kind !== "not_started"
            : kind === "step01"
              ? rawStage.kind === "step"
                ? rawStage.step >= 2
                : rawStage.kind === "proof_token" || rawStage.kind === "removed"
              : kind === "step02"
                ? rawStage.kind === "proof_token" || rawStage.kind === "removed"
                : rawStage.kind === "removed" ||
                  (rawStage.kind === "proof_token" &&
                    rawStage.nextRemovalOutRef !==
                      context.action.input.stateQueueBlockOutRef);
        const intendedTransactionConfirmed =
          await config.rawL1.transactionConfirmed({
            headerHash: prepared.headerHash,
            txHash: context.txHash,
          });
        if (stageAdvanced && !intendedTransactionConfirmed) {
          return {
            kind: "conflict",
            reason:
              "network-id chain advanced without the journaled transaction in authenticated unit history",
          };
        }
        advanced = stageAdvanced && intendedTransactionConfirmed;
      } else {
        const state = await live(prepared);
        advanced =
          kind === "init"
            ? state.step01 !== undefined ||
              state.step02 !== undefined ||
              state.proof !== undefined ||
              state.stateQueue === undefined
            : kind === "step01"
              ? state.step01 === undefined &&
                (state.step02 !== undefined ||
                  state.proof !== undefined ||
                  state.stateQueue === undefined)
              : kind === "step02"
                ? state.step02 === undefined &&
                  (state.proof !== undefined || state.stateQueue === undefined)
                : state.stateQueue === undefined;
      }
      if (advanced) {
        if (context.txHash === undefined) {
          return {
            kind: "conflict",
            reason:
              "chain advanced but the journal carries no transaction hash",
          };
        }
        const recovery =
          context.txHash === undefined
            ? { kind: "ok" as const, lease: undefined }
            : await recoverMutationLeaseV1({
                config,
                txHash: context.txHash,
                durableRecovery: context.durableRecovery,
                mutationLeaseByTxHash,
              });
        if (recovery.kind === "conflict") return recovery;
        await recovery.lease?.release();
        if (context.txHash !== undefined) {
          mutationLeaseByTxHash.delete(context.txHash);
        }
        return { kind: "confirmed", txHash: context.txHash };
      }
      const recovery =
        context.txHash === undefined
          ? { kind: "ok" as const, lease: undefined }
          : await recoverMutationLeaseV1({
              config,
              txHash: context.txHash,
              durableRecovery: context.durableRecovery,
              mutationLeaseByTxHash,
            });
      if (recovery.kind === "conflict") return recovery;
      await recovery.lease?.renew();
      return { kind: "not_found" };
    },
  };
};

/** Production run/resume with its header derived from admitted raw L1 bytes. */
export const runOrResumeManifestBoundNetworkIdWorkflowV1 = async ({
  workflow,
  sources,
  journal,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly workflow: ManifestBoundNetworkIdWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const rawL1 = workflow.adapterConfig.rawL1;
  const observeHeader = rawL1?.observeHeader;
  if (observeHeader === undefined) {
    throw new Error(
      "manifest-bound network-id workflow omitted raw L1 header derivation",
    );
  }
  const observation = await observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["networkId"],
    }),
    journal,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    terminalVerifier: workflow.terminalVerifier,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};
