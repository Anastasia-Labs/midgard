/**
 * Resumable Q35 workflow adapter for a current-head proof whose field-2
 * opening fits inline. Larger tiered openings remain a fail-closed Q38
 * dependency because every publication/certificate needs its own journaled
 * action before the final proof transaction.
 */
import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardOutputFieldPreimage,
  outRefLabel,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import {
  type AuthenticatedStateQueueHeaderObservation,
  bindExactVerdictSubjectReason,
  deriveFieldPreimageCertification,
  encodeHeaderCbor,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  FraudProofComputationThreadStepDatum,
  HeaderSchema,
  type NetworkIdFault,
  NetworkIdForcedScanDatum,
  NetworkIdStep02Datum,
  OutputReferenceSchema,
  rootMembershipProofSchema,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  fieldPreimageCertificateAddress,
  findMissingFaultProofFieldPublication,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type {
  RemoveFraudulentBlockExplicitCategory,
  RemoveFraudulentBlockFraudCategory,
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import {
  NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
  type ResolvedProverSigner,
} from "../runtime.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import { NETWORK_ID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import type {
  FraudProofWorkflowJournalStore,
  FraudProofWorkflowTerminal,
  JournalJsonObject,
} from "../workflow/journal.js";
import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
  type LocalKupmiosHttpOgmiosSourceConfig,
} from "../workflow/local-kupmios-http-ogmios-source.js";
import { createLocalKupmiosFraudProofRawL1SnapshotAuthority } from "../workflow/local-kupmios-raw-l1-authority.js";
import {
  reconcileSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "../workflow/signed-transaction-reconciliation.js";
import {
  createFraudProofWorkflowRegistry,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowReconcileResult,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import {
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1,
  deriveFraudProofRawL1FamilyStage,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1FamilyStage,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../workflow/raw-l1-family-derivation.js";
import {
  createFraudProofAuthenticatedPublicationObserver,
  type FraudProofAuthenticatedPublicationObserver,
} from "../workflow/raw-l1-publication-observation.js";
import {
  admitFraudProofRawL1Snapshot,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "../workflow/raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseEconomicsPolicy } from "../workflow/release-economics-policy.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  bindWorkflowPreflightTransaction,
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import type { NetworkIdContracts } from "./contracts.js";
import {
  type NetworkIdForcedScanStep,
  networkIdForcedScanStepForState,
  networkIdForcedScanStepOrdinal,
  planNetworkIdForcedScan,
} from "./forced-scan-plan.js";
import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
  prepareNetworkIdFromCanonicalEvidence,
} from "./prepare.js";
import type { NetworkIdCatalogueCategory } from "./submit-common.js";
import { submitNetworkIdForcedBind } from "./submit-network-id-forced-bind.js";
import { submitNetworkIdForcedScanAction } from "./submit-network-id-forced-scan.js";
import { submitNetworkIdForcedStep01 } from "./submit-network-id-forced-step-01.js";
import { submitNetworkIdInit } from "./submit-network-id-init.js";
import { submitNetworkIdStep01 } from "./submit-network-id-step-01.js";
import { submitNetworkIdStep02 } from "./submit-network-id-step-02.js";
import {
  createNetworkIdWrongfulRejectionPlanner,
  detectNetworkIdWrongfulRejections,
  NETWORK_ID_MISMATCH_REASON,
  NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID,
  networkIdWrongfulRejectionCloses,
  type NetworkIdWrongfulRejectionEvidence,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection.js";

const ARTIFACT_VERSION = "midgard-network-id-workflow-artifact-v1" as const;
const CATEGORY = "networkId";

/**
 * Direction discriminator for the durable artifact.
 *
 * The accepted direction predates it, so an artifact without the field is
 * exactly the accepted shape it always was; the forced direction is additive
 * and the schema version is unchanged.
 */
export type NetworkIdWorkflowDirection = "accepted" | "forced";

type NetworkIdWorkflowArtifact = {
  readonly schemaVersion: typeof ARTIFACT_VERSION;
  readonly direction?: "accepted";
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

/**
 * §5.2 forced (wrongful-rejection) artifact.
 *
 * Everything the forced door and step 02 re-derive on chain is carried as the
 * authenticated bytes it is derived from: the counted-root membership proof
 * (header + leaf) as canonical `Data` CBOR, and the field-2 preimage. The
 * verdict subject, the transaction's own network id, the output network ids
 * and the item list are all re-derived on admission, never journaled as a
 * prover assertion.
 */
/**
 * Durable §5.2 journal artifact. It is the accepted artifact's sibling, not its
 * successor: the shared `schemaVersion` plus the additive `direction`
 * discriminator keeps every already-journaled accepted artifact admissible.
 */
export type NetworkIdForcedWorkflowArtifact = {
  readonly schemaVersion: typeof ARTIFACT_VERSION;
  readonly direction: "forced";
  readonly headerHash: string;
  readonly expectedNetworkId: "0" | "1";
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly outputsPreimageCbor: string;
  readonly outputsItemCbors: readonly string[];
  readonly forcedSourceCbor: string;
};

export const NetworkIdForcedSourceSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
});

type WorkflowContext = Parameters<
  FraudProofFamilyWorkflowAdapter["observe"]
>[0];

type ActionKind =
  | "init"
  | "step01"
  | "forced_step01"
  | "forced_bind"
  | "forced_scan_open"
  | "forced_scan_grammar"
  | "forced_scan_advance"
  | "step02"
  | "publish_field"
  | "certify_field"
  | "remove";

/** The three scan stages, named by the §10 phase each one drives. */
const FORCED_SCAN_KINDS = [
  "forced_scan_open",
  "forced_scan_grammar",
  "forced_scan_advance",
] as const;

type ForcedScanKind = (typeof FORCED_SCAN_KINDS)[number];

const isForcedScanKind = (kind: ActionKind): kind is ForcedScanKind =>
  (FORCED_SCAN_KINDS as readonly string[]).includes(kind);

const forcedScanKindFor = (step: NetworkIdForcedScanStep): ForcedScanKind =>
  step.kind === "open"
    ? "forced_scan_open"
    : step.kind === "advance"
      ? "forced_scan_advance"
      : "forced_scan_grammar";

const requireString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`network-id workflow ${label} must be a non-empty string`);
  }
  return value;
};

const actionKind = (action: FraudProofWorkflowAction): ActionKind => {
  const kind = action.input.kind;
  if (
    kind !== "init" &&
    kind !== "step01" &&
    kind !== "forced_step01" &&
    kind !== "forced_bind" &&
    kind !== "forced_scan_open" &&
    kind !== "forced_scan_grammar" &&
    kind !== "forced_scan_advance" &&
    kind !== "step02" &&
    kind !== "publish_field" &&
    kind !== "certify_field" &&
    kind !== "remove"
  ) {
    throw new Error(`network-id workflow action ${action.actionId} is unknown`);
  }
  return kind;
};

// Every action carries its kind twice: `kind` drives this adapter's own
// dispatch and `actionKind` is the stable action label the production funding
// reservation permit reads (it accepts `actionKind` or `stage`, never `kind`).
const action = (
  kind: ActionKind,
  input: Readonly<Record<string, string>>,
  actionId = `network-id:${kind}:${Object.values(input).join(":")}`,
): FraudProofWorkflowAction => ({
  actionId,
  input: { kind, actionKind: kind, ...input },
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
  prepared: PreparedNetworkIdProof,
): NetworkIdWorkflowArtifact => ({
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
): PreparedNetworkIdProof => {
  const artifact = value as unknown as NetworkIdWorkflowArtifact;
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
  const fault: NetworkIdFault =
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
        decodeMidgardNativeTxCompact(
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

type ForcedSourcePayload = Data.Static<typeof NetworkIdForcedSourceSchema>;

const proofSteps = (proof: ForcedSourcePayload["membership"]["proof"]) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

/**
 * Replays the counted-root membership the forced door re-derives on chain, so a
 * journal artifact whose proof no longer opens its own leaf is refused here
 * rather than at submission.
 */
const bindForcedLeafMembership = (source: ForcedSourcePayload): void => {
  const membership = source.membership;
  let replayed: Buffer | null;
  try {
    replayed = MpfProof.fromJSON(
      Buffer.from(
        Data.to(membership.key as never, OutputReferenceSchema as never),
        "hex",
      ),
      Buffer.from(
        Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
        "hex",
      ),
      proofSteps(membership.proof),
    ).verify(true);
  } catch {
    throw new Error(
      "network-id forced artifact membership proof cannot be replayed",
    );
  }
  if (replayed?.toString("hex") !== membership.phas_root) {
    throw new Error(
      "network-id forced artifact membership proof opens another root",
    );
  }
};

/** Prepared forced contradiction to its durable journal artifact. */
export const networkIdForcedArtifactFromPrepared = (
  prepared: PreparedNetworkIdWrongfulRejection,
): NetworkIdForcedWorkflowArtifact => ({
  schemaVersion: ARTIFACT_VERSION,
  direction: "forced",
  headerHash: prepared.headerHash,
  expectedNetworkId: prepared.expectedNetworkId.toString() as "0" | "1",
  badTxId: prepared.badTxId,
  nativeTxCompactCbor: prepared.nativeTxCompactCbor,
  outputsPreimageCbor: prepared.evidence.outputsPreimageCbor,
  outputsItemCbors: [...prepared.outputsItemCbors],
  forcedSourceCbor: Data.to(
    {
      header: prepared.forcedSource.header,
      membership: prepared.forcedSource.membership,
    } as never,
    NetworkIdForcedSourceSchema as never,
  ),
});

/**
 * Durable journal artifact back to the prepared forced contradiction, deriving
 * every claim from the artifact's own authenticated leaf.
 */
export const admitNetworkIdForcedArtifact = (
  artifact: NetworkIdForcedWorkflowArtifact,
): PreparedNetworkIdWrongfulRejection => {
  const expectedNetworkId =
    artifact.expectedNetworkId === "0"
      ? 0n
      : artifact.expectedNetworkId === "1"
        ? 1n
        : undefined;
  if (expectedNetworkId === undefined) {
    throw new Error("network-id workflow artifact has an invalid network id");
  }
  const headerHash = requireString(artifact.headerHash, "header hash");
  const badTxId = requireString(artifact.badTxId, "transaction id");
  const nativeTxCompactCbor = requireString(
    artifact.nativeTxCompactCbor,
    "compact transaction",
  );
  const forcedSource = Data.from(
    requireString(artifact.forcedSourceCbor, "forced source"),
    NetworkIdForcedSourceSchema as never,
  ) as ForcedSourcePayload;
  if (
    computeHash28(encodeHeaderCbor(forcedSource.header)).toString("hex") !==
      headerHash ||
    forcedSource.membership.root !==
      forcedSource.header.forcedTransactionsRoot ||
    forcedSource.membership.count !== forcedSource.header.forcedTransactionCount
  ) {
    throw new Error(
      "network-id forced artifact does not bind its authenticated header",
    );
  }
  bindForcedLeafMembership(forcedSource);
  const leaf = forcedSource.membership.value;
  if (
    leaf.tx_id !== badTxId ||
    leaf.submitted_source.compact_cbor !== nativeTxCompactCbor ||
    leaf.verdict === "ForcedTxValid"
  ) {
    throw new Error(
      "network-id forced artifact does not bind its authenticated forced leaf",
    );
  }
  const subject = forcedVerdictSubject({
    transactionId: leaf.tx_id,
    sourceKey: forcedSource.membership.key,
    rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
  });
  // Refuses another family's typed rejection reason before anything downstream
  // can treat this thread as a network-id contradiction.
  bindExactVerdictSubjectReason(subject, NETWORK_ID_MISMATCH_REASON);
  const outputsPreimage = Buffer.from(
    requireString(artifact.outputsPreimageCbor, "outputs preimage"),
    "hex",
  );
  const outputsItemCbors = decodeMidgardFieldPreimage(outputsPreimage).map(
    (item) => Buffer.from(item).toString("hex"),
  );
  if (
    outputsItemCbors.length !== artifact.outputsItemCbors.length ||
    outputsItemCbors.some(
      (item, index) => item !== artifact.outputsItemCbors[index],
    )
  ) {
    throw new Error(
      "network-id forced artifact outputs differ from their authenticated preimage",
    );
  }
  const outputNetworkIds = decodeMidgardOutputFieldPreimage(
    outputsPreimage,
  ).map((output) =>
    BigInt(decodeMidgardAddressBytes(output.address).networkId),
  );
  const evidence: NetworkIdWrongfulRejectionEvidence = Object.freeze({
    subject,
    expectedNetworkId,
    committedNetworkId: decodeMidgardForcedTxCompact(
      Buffer.from(nativeTxCompactCbor, "hex"),
    ).transactionBody.networkId,
    outputNetworkIds: Object.freeze(outputNetworkIds),
    outputsItemCbors: Object.freeze(outputsItemCbors),
    outputsPreimageCbor: outputsPreimage.toString("hex"),
  });
  if (!networkIdWrongfulRejectionCloses(evidence)) {
    throw new Error(
      "network-id forced artifact does not contradict NetworkIdMismatch; the rejection was honest",
    );
  }
  return Object.freeze({
    headerHash,
    expectedNetworkId,
    badTxId,
    nativeTxCompactCbor,
    outputsItemCbors: evidence.outputsItemCbors,
    faultClaim: Object.freeze({ kind: "forced-network-mismatch" as const }),
    fault: "ForcedNetworkIdMismatch" as NetworkIdFault,
    subject,
    forcedSource: Object.freeze({
      header: forcedSource.header,
      membership: forcedSource.membership,
      direction: 1n as const,
    }),
    evidence,
  });
};

/** Either direction of the family, discriminated by the artifact itself. */
export type AdmittedNetworkIdArtifact =
  | {
      readonly direction: "accepted";
      readonly prepared: PreparedNetworkIdProof;
    }
  | {
      readonly direction: "forced";
      readonly prepared: PreparedNetworkIdWrongfulRejection;
    };

export const admitNetworkIdWorkflowArtifact = (
  value: WorkflowContext["artifact"],
): AdmittedNetworkIdArtifact => {
  const candidate = value as unknown as {
    readonly schemaVersion?: unknown;
    readonly direction?: unknown;
  };
  if (candidate.schemaVersion !== ARTIFACT_VERSION) {
    throw new Error("network-id workflow artifact has an unsupported version");
  }
  if (candidate.direction === "forced") {
    return {
      direction: "forced",
      prepared: admitNetworkIdForcedArtifact(
        value as unknown as NetworkIdForcedWorkflowArtifact,
      ),
    };
  }
  if (candidate.direction !== undefined && candidate.direction !== "accepted") {
    throw new Error("network-id workflow artifact has an unknown direction");
  }
  return { direction: "accepted", prepared: preparedFromArtifact(value) };
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

const parseMutationLeaseRecovery = (
  recovery: JournalJsonObject | undefined,
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

const recoverMutationLease = async ({
  config,
  txHash,
  durableRecovery,
  mutationLeaseByTxHash,
}: {
  readonly config: NetworkIdWorkflowAdapterConfig;
  readonly txHash: string;
  readonly durableRecovery: JournalJsonObject | undefined;
  readonly mutationLeaseByTxHash: Map<string, StateQueueMutationLease>;
}): Promise<
  | { readonly kind: "ok"; readonly lease: StateQueueMutationLease | undefined }
  | { readonly kind: "conflict"; readonly reason: string }
> => {
  let identity: { readonly token: string; readonly source: string } | undefined;
  try {
    identity = parseMutationLeaseRecovery(durableRecovery);
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

export type NetworkIdWorkflowTerminalFacts = {
  readonly economics: FraudProofWorkflowTerminal["economics"];
  readonly observedAt: FraudProofWorkflowTerminal["observedAt"];
};

export interface NetworkIdRawL1ObservationPort {
  readonly publications?: FraudProofAuthenticatedPublicationObserver;
  /**
   * Canonical recovery of a journaled signed transaction (mempool, inclusion,
   * expiry). Reconciliation reports a submitted transaction as absent only
   * through this port; without it an unconfirmed submission stays unresolved
   * rather than being abandoned and rebuilt.
   */
  observeSignedTransaction?(
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation>;
  rebroadcastSignedTransaction?(
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string>;
  observeHeader?(input: {
    readonly headerHash: string;
  }): Promise<AuthenticatedStateQueueHeaderObservation>;
  transactionConfirmed?(input: {
    readonly headerHash: string;
    readonly txHash: string;
  }): Promise<boolean>;
  observe(input: {
    readonly headerHash: string;
  }): Promise<FraudProofRawL1FamilyStage>;
}

export const createNetworkIdRawL1ObservationPort = ({
  authority,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "networkId";
  };
}): NetworkIdRawL1ObservationPort => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY ||
    definition.computationThread.steps.length !== 2
  ) {
    throw new Error("network-id raw L1 observation authority is incomplete");
  }
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality,
  });
  const capture = async (headerHash: string) => {
    if (headerHash !== definition.headerHash) {
      throw new Error("network-id raw L1 observation changed the header");
    }
    return admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
    });
  };
  return {
    publications: createFraudProofAuthenticatedPublicationObserver({
      authority,
      releaseFinality,
    }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      (await capture(headerHash)).transactions.some(
        (transaction) => transaction.txHash === txHash,
      ),
    observeHeader: async ({ headerHash }) =>
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: await capture(headerHash),
        definition,
      }),
    observe: async ({ headerHash }) => {
      const snapshot = await capture(headerHash);
      return await deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition,
        releaseEconomics,
      });
    },
  };
};

/** Concrete loopback Kupo HTTP + Ogmios WS production construction. */
export const createNetworkIdLocalKupmiosL1ObservationPort = ({
  source,
  releaseFinality,
  releaseEconomics,
  definition,
}: {
  readonly source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  readonly definition: FraudProofRawL1FamilyDefinition & {
    readonly category: "networkId";
  };
}): NetworkIdRawL1ObservationPort => {
  const rawSource = createLocalKupmiosHttpOgmiosRawSource({
    ...source,
    releaseFinality,
  });
  const port = createNetworkIdRawL1ObservationPort({
    authority: createLocalKupmiosFraudProofRawL1SnapshotAuthority({
      source: rawSource,
      releaseFinality,
    }),
    releaseFinality,
    releaseEconomics,
    definition,
  });
  return Object.freeze({
    ...port,
    observeSignedTransaction: (input: SignedWorkflowTransaction) =>
      readAdmittedLocalKupmiosSignedTransactionRecovery({
        ...input,
        source: rawSource,
      }),
    rebroadcastSignedTransaction: (
      input: SignedWorkflowTransaction & {
        readonly authorizeResubmission: (
          input: SignedWorkflowTransaction,
        ) => Promise<void>;
      },
    ) =>
      rebroadcastAdmittedLocalKupmiosSignedTransaction({
        ...input,
        source: rawSource,
      }),
  });
};

/** Independent second raw-L1 observation for terminal admission. */
export const createNetworkIdAuthenticatedL1TerminalVerifier = (
  l1: NetworkIdRawL1ObservationPort,
): FraudProofWorkflowTerminalVerifier => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
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

export type NetworkIdWorkflowAdapterConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: NetworkIdContracts;
  readonly stateQueueAddress: string;
  readonly category: NetworkIdCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  /**
   * Published `fraudProofNetworkIdForcedStep` reference script. Optional in the
   * deployment shape and mandatory the moment a forced (§5.2) artifact is
   * admitted; the forced door is reference-script-only like every other step.
   */
  readonly forcedStepReferenceScript?: UTxO;
  /**
   * Published `fraudProofNetworkIdForcedScan` reference script: the resumable
   * outputs scan the forced door hands the thread to. Optional and mandatory
   * on the same terms as the forced step.
   */
  readonly forcedScanReferenceScript?: UTxO;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
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
  readonly rawL1?: NetworkIdRawL1ObservationPort;
  /** Candidate chain facts; the shared independent verifier reauthenticates them. */
  readonly terminalFacts?: (input: {
    readonly headerHash: string;
    readonly removalTxHash: string;
    readonly proofTokenOutRef: string;
  }) => Promise<NetworkIdWorkflowTerminalFacts>;
};

type NetworkIdRemovalConfig = NetworkIdWorkflowAdapterConfig["removal"];

export type ManifestBoundNetworkIdWorkflowConfig = Omit<
  NetworkIdWorkflowAdapterConfig,
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
  readonly source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  readonly removal: Omit<
    NetworkIdRemovalConfig,
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
      FaultProofWitnessReferenceScripts,
      | "computationThreadMint"
      | "fraudProofMint"
      | "phasMembershipWithdraw"
      | "chunkedVerifyWithdraw"
      | "pexcludesWithdraw"
    >
  >;
};

export type ManifestBoundNetworkIdWorkflow = {
  readonly binding: FraudProofWorkflowDeploymentBinding<"networkId">;
  readonly adapterConfig: NetworkIdWorkflowAdapterConfig;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
};

export type ManifestBoundNetworkIdRuntimeSeal = {
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly forcedStepReferenceScript?: UTxO;
  readonly forcedScanReferenceScript?: UTxO;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly removal: ManifestBoundNetworkIdWorkflowConfig["removal"] & {
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
export const sealManifestBoundNetworkIdRuntime = ({
  binding,
  signer,
  stepReferenceScripts,
  forcedStepReferenceScript,
  forcedScanReferenceScript,
  fieldPreimageCertificateReferenceScript,
  witnessReferenceScripts,
  removal,
}: {
  readonly binding: Pick<
    FraudProofWorkflowDeploymentBinding<"networkId">,
    "network" | "referenceScriptsByContract"
  >;
  readonly signer: ResolvedProverSigner;
  readonly stepReferenceScripts: readonly [UTxO, UTxO];
  readonly forcedStepReferenceScript?: UTxO;
  readonly forcedScanReferenceScript?: UTxO;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
  readonly witnessReferenceScripts: ManifestBoundNetworkIdWorkflowConfig["witnessReferenceScripts"];
  readonly removal: ManifestBoundNetworkIdWorkflowConfig["removal"];
}): ManifestBoundNetworkIdRuntimeSeal => {
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: signer.address,
    paymentKeyHash: signer.paymentKeyHash,
  });
  const requireReference = (contractName: string, utxo: UTxO): UTxO =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo,
    });
  return {
    stepReferenceScripts: [
      requireReference("fraudProofNetworkId", stepReferenceScripts[0]),
      requireReference("fraudProofNetworkIdStep02", stepReferenceScripts[1]),
    ],
    ...(forcedStepReferenceScript === undefined
      ? {}
      : {
          forcedStepReferenceScript: requireReference(
            "fraudProofNetworkIdForcedStep",
            forcedStepReferenceScript,
          ),
        }),
    ...(forcedScanReferenceScript === undefined
      ? {}
      : {
          forcedScanReferenceScript: requireReference(
            NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY,
            forcedScanReferenceScript,
          ),
        }),
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
export const createManifestBoundNetworkIdWorkflow = async (
  config: ManifestBoundNetworkIdWorkflowConfig,
): Promise<ManifestBoundNetworkIdWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  const sealedRuntime = sealManifestBoundNetworkIdRuntime({
    binding,
    signer: config.signer,
    stepReferenceScripts: config.stepReferenceScripts,
    ...(config.forcedStepReferenceScript === undefined
      ? {}
      : { forcedStepReferenceScript: config.forcedStepReferenceScript }),
    ...(config.forcedScanReferenceScript === undefined
      ? {}
      : { forcedScanReferenceScript: config.forcedScanReferenceScript }),
    fieldPreimageCertificateReferenceScript:
      config.fieldPreimageCertificateReferenceScript,
    witnessReferenceScripts: config.witnessReferenceScripts,
    removal: config.removal,
  });
  const rawL1 = createNetworkIdLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const adapterConfig: NetworkIdWorkflowAdapterConfig = {
    lucid: config.lucid,
    blueprint: binding.blueprint,
    network: binding.network,
    contracts: {
      steps: networkIdContracts.steps,
      forcedStep: networkIdContracts.forcedStep,
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
    ...(sealedRuntime.forcedStepReferenceScript === undefined
      ? {}
      : {
          forcedStepReferenceScript: sealedRuntime.forcedStepReferenceScript,
        }),
    ...(sealedRuntime.forcedScanReferenceScript === undefined
      ? {}
      : {
          forcedScanReferenceScript: sealedRuntime.forcedScanReferenceScript,
        }),
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
    adapter: createNetworkIdWorkflowAdapter(adapterConfig),
    terminalVerifier: createNetworkIdAuthenticatedL1TerminalVerifier(rawL1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  };
};

/**
 * Concrete last-header adapter. It captures the actual signed transaction at
 * the post-local-evaluation/pre-network boundary, journals that hash through
 * the shared orchestrator, then submits exactly the captured body.
 */
export const createNetworkIdWorkflowAdapter = (
  config: NetworkIdWorkflowAdapterConfig,
): FraudProofFamilyWorkflowAdapter => {
  const captured = new Map<
    string,
    {
      readonly transaction: LocallyEvaluatedTransaction;
      readonly mutationLease?: StateQueueMutationLease;
    }
  >();
  const mutationLeaseByTxHash = new Map<string, StateQueueMutationLease>();
  const category = CATEGORY;

  const requireForcedStep = () => {
    const forcedStep = config.contracts.forcedStep;
    if (forcedStep === undefined) {
      throw new Error(
        "network-id forced direction requires the deployed forced step",
      );
    }
    return forcedStep;
  };

  const requireForcedStepReferenceScript = (): UTxO => {
    const referenceScript = config.forcedStepReferenceScript;
    if (referenceScript === undefined) {
      throw new Error(
        "network-id forced direction requires the published fraudProofNetworkIdForcedStep reference script",
      );
    }
    return referenceScript;
  };

  const requireForcedScan = () => {
    const forcedScan = config.contracts.forcedScan;
    if (forcedScan === undefined) {
      throw new Error(
        "network-id forced direction requires the deployed forced outputs scan",
      );
    }
    return forcedScan;
  };

  const requireForcedScanReferenceScript = (): UTxO => {
    const referenceScript = config.forcedScanReferenceScript;
    if (referenceScript === undefined) {
      throw new Error(
        `network-id forced direction requires the published ${NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY} reference script`,
      );
    }
    return referenceScript;
  };

  /**
   * The single scan thread, read directly for the same reason the forced door
   * is: the §10 loop self-loops at one address, so it is not a link the linear
   * raw-L1 family definition can walk.
   */
  const forcedScanThread = async (
    headerHash: string,
  ): Promise<UTxO | undefined> => {
    const forcedScan = requireForcedScan();
    const utxos = await config.lucid.utxosAtWithUnit(
      forcedScan.spendingScriptAddress,
      toUnit(
        config.contracts.computationThread.policyId,
        `${config.category.categoryId}${headerHash}`,
      ),
    );
    if (utxos.length > 1) {
      throw new Error("network-id workflow found duplicate forced-scan UTxOs");
    }
    return utxos[0];
  };

  /**
   * Scan occupancy, read only for the stages whose progress it decides. An
   * accepted artifact, or a deployment without the scan, never pays the query.
   */
  const forcedScanOutRef = async ({
    admitted,
    kind,
  }: {
    readonly admitted: AdmittedNetworkIdArtifact;
    readonly kind: ActionKind;
  }): Promise<string | undefined> => {
    if (admitted.direction !== "forced") return undefined;
    if (
      kind !== "init" &&
      kind !== "forced_step01" &&
      kind !== "forced_bind" &&
      !isForcedScanKind(kind)
    ) {
      return undefined;
    }
    if (config.contracts.forcedScan === undefined) return undefined;
    const utxo = await forcedScanThread(admitted.prepared.headerHash);
    return utxo === undefined ? undefined : outRefLabel(utxo);
  };

  /**
   * The forced door is deliberately not a link in the linear chain the raw-L1
   * family definition walks, so its occupancy is read directly. Nothing here is
   * trusted: `submitNetworkIdForcedBind` re-authenticates the thread token, the
   * handoff datum, the prover and the header before it spends this UTxO.
   */
  const forcedThreadOutRef = async (
    headerHash: string,
  ): Promise<string | undefined> => {
    const forcedStep = requireForcedStep();
    const utxos = await config.lucid.utxosAtWithUnit(
      forcedStep.spendingScriptAddress,
      toUnit(
        config.contracts.computationThread.policyId,
        `${config.category.categoryId}${headerHash}`,
      ),
    );
    if (utxos.length > 1) {
      throw new Error("network-id workflow found duplicate forced-step UTxOs");
    }
    return utxos[0] === undefined ? undefined : outRefLabel(utxos[0]);
  };

  /**
   * Forced-door occupancy, read only for the stages whose progress it decides.
   * An accepted artifact never touches the door, so it never pays the query.
   */
  const forcedDoorOccupied = async ({
    admitted,
    kind,
  }: {
    readonly admitted: AdmittedNetworkIdArtifact;
    readonly kind: ActionKind;
  }): Promise<boolean> => {
    if (admitted.direction !== "forced") return false;
    if (kind !== "init" && kind !== "forced_step01" && kind !== "forced_bind") {
      return false;
    }
    return (
      (await forcedThreadOutRef(admitted.prepared.headerHash)) !== undefined
    );
  };

  const live = async (headerHash: string) => {
    const threadUnit = toUnit(
      config.contracts.computationThread.policyId,
      `${config.category.categoryId}${headerHash}`,
    );
    const proofUnit = toUnit(
      config.contracts.fraudProof.policyId,
      `${config.category.categoryId}${headerHash}`,
    );
    const stateQueueUnit = toUnit(
      config.contracts.stateQueuePolicyId,
      `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
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

  /**
   * The §2.5 field-2 opening this direction's final step needs, or `undefined`
   * when the claim opens no field at all. The forced contradiction always opens
   * the complete output field, and publishes it: the door reads every output's
   * network id, so the bytes never ride the step redeemer.
   */
  const outputsOpeningPlanFor = (
    admitted: AdmittedNetworkIdArtifact,
  ): FaultProofFieldOpeningPlan | undefined =>
    admitted.direction === "forced"
      ? planNetworkIdOutputsOpening({
          prepared: admitted.prepared,
          owner: config.signer.paymentKeyHash,
          publish: true,
        })
      : admitted.prepared.faultClaim.kind === "output-network"
        ? planNetworkIdOutputsOpening({
            prepared: admitted.prepared,
            owner: config.signer.paymentKeyHash,
          })
        : undefined;

  /**
   * What the *final* step opens. The forced direction opens nothing there any
   * more: `forced_scan` certifies field 2 in batches and step 02 takes its
   * terminal state as established, so re-opening the field at finalization
   * would put back exactly the single-transaction budget the scan escapes.
   */
  const finalStepOutputsOpeningPlanFor = (
    admitted: AdmittedNetworkIdArtifact,
  ): FaultProofFieldOpeningPlan | undefined =>
    admitted.direction === "forced"
      ? undefined
      : outputsOpeningPlanFor(admitted);

  /** The planned §10 batch schedule for an admitted forced artifact. */
  const forcedScanPlanFor = (admitted: AdmittedNetworkIdArtifact) => {
    const opening = outputsOpeningPlanFor(admitted);
    if (opening === undefined) {
      throw new Error(
        "network-id forced scan requires the authenticated field-2 opening",
      );
    }
    return {
      opening,
      plan: planNetworkIdForcedScan({
        outputsCarriagePlan: opening,
        outputCount: opening.itemCount,
      }),
    };
  };

  /** The batch the live scan thread is waiting for, read from its datum. */
  const forcedScanNextStep = ({
    utxo,
    plan,
  }: {
    readonly utxo: UTxO;
    readonly plan: ReturnType<typeof forcedScanPlanFor>["plan"];
  }): NetworkIdForcedScanStep => {
    if (utxo.datum == null) {
      throw new Error(
        `network-id forced scan thread ${outRefLabel(utxo)} has no inline datum`,
      );
    }
    const datum = Data.from(utxo.datum, NetworkIdForcedScanDatum);
    if (datum.data === null) {
      throw new Error(
        "network-id forced scan thread carries an empty scan state",
      );
    }
    return networkIdForcedScanStepForState(plan, datum.data);
  };

  const authenticateFieldInputs = async ({
    prepared,
    publications,
    certificate,
  }: {
    readonly prepared:
      | PreparedNetworkIdProof
      | PreparedNetworkIdWrongfulRejection;
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
        address: fieldPreimageCertificateAddress({
          network: config.network,
          certificatePolicyId:
            config.contracts.fieldPreimageCertificatePolicyId,
        }),
        expectedOutRef: outRefLabel(certificate),
        expectedDatumCbor: certificate.datum,
        expectedUnit: `${config.contracts.fieldPreimageCertificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
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
  ): Promise<FraudProofWorkflowObservation> => {
    const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
    const prepared = admitted.prepared;
    const rawStage = await config.rawL1?.observe({
      headerHash: prepared.headerHash,
    });
    if (rawStage?.kind === "removed") {
      return { kind: "completed", terminal: rawStage.terminal };
    }
    const state =
      rawStage === undefined ? await live(prepared.headerHash) : undefined;
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
      const opening = finalStepOutputsOpeningPlanFor(admitted);
      if (opening !== undefined) {
        const missing = await findMissingFaultProofFieldPublication({
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
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublications({
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
    if (
      admitted.direction === "forced" &&
      config.contracts.forcedScan !== undefined
    ) {
      const scanUtxo = await forcedScanThread(prepared.headerHash);
      if (scanUtxo !== undefined) {
        const scanOutRef = outRefLabel(scanUtxo);
        const { opening, plan } = forcedScanPlanFor(admitted);
        const missing = await findMissingFaultProofFieldPublication({
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
                threadOutRef: scanOutRef,
                fieldCommitment: opening.commitment,
                publicationDatumCbor: missing.datumCbor,
              },
              contentActionId({ base, entries: context.entries }),
            ),
          };
        }
        let certificateOutRef: string | undefined;
        if (opening.plan.tier === "Certified") {
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublications({
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
                  threadOutRef: scanOutRef,
                  fieldCommitment: opening.commitment,
                  chunkOutRefs: publications
                    .map((utxo) => outRefLabel(utxo))
                    .join(","),
                },
                contentActionId({ base, entries: context.entries }),
              ),
            };
          }
          certificateOutRef = outRefLabel(certificate);
        }
        const step = forcedScanNextStep({ utxo: scanUtxo, plan });
        return {
          kind: "action_required",
          action: action(forcedScanKindFor(step), {
            threadOutRef: scanOutRef,
            scanAction: step.kind,
            scanOrdinal: networkIdForcedScanStepOrdinal(step).toString(),
            ...(certificateOutRef === undefined ? {} : { certificateOutRef }),
          }),
        };
      }
    }
    if (step01OutRef !== undefined) {
      return {
        kind: "action_required",
        action:
          admitted.direction === "forced"
            ? action("forced_step01", { threadOutRef: step01OutRef })
            : action("step01", {
                threadOutRef: step01OutRef,
                stateQueueBlockOutRef: stateQueueOutRef,
              }),
      };
    }
    if (admitted.direction === "forced") {
      const forcedOutRef = await forcedThreadOutRef(prepared.headerHash);
      if (forcedOutRef !== undefined) {
        return {
          kind: "action_required",
          action: action("forced_bind", { threadOutRef: forcedOutRef }),
        };
      }
    }
    return {
      kind: "action_required",
      action: action("init", {
        fraudulentBlockOutRef: stateQueueOutRef,
      }),
    };
  };

  return {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({
      evidence,
      classification,
    }: {
      readonly evidence: CanonicalBlockEvidence;
      readonly classification?: Extract<
        CanonicalBlockClassification,
        { readonly decision: "fault_detected" }
      >;
    }): Promise<JournalJsonObject> => {
      if (
        classification?.category === CATEGORY &&
        classification.selected.violationId ===
          NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID
      ) {
        const expectedNetworkId = config.contracts.expectedNetworkId;
        const detections = detectNetworkIdWrongfulRejections({
          block: evidence,
          expectedNetworkId,
        });
        if (
          detections[0]?.detectionId !== classification.selected.detectionId
        ) {
          throw new Error(
            "network-id forced classification does not select the earliest authenticated wrongful rejection",
          );
        }
        return networkIdForcedArtifactFromPrepared(
          await createNetworkIdWrongfulRejectionPlanner(expectedNetworkId)({
            block: evidence,
          }),
        ) as unknown as JournalJsonObject;
      }
      return artifactFromPrepared(
        await prepareNetworkIdFromCanonicalEvidence({
          evidence,
          expectedNetworkId: config.contracts.expectedNetworkId,
        }),
      );
    },
    observe,
    preflight: async (context): Promise<FraudProofWorkflowPreflight> => {
      const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
      const prepared = admitted.prepared;
      const kind = actionKind(context.action);
      let mutationLease: StateQueueMutationLease | undefined;
      const boundaryInvocation = async (
        boundary: FraudProofPreSubmitBoundary,
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
          if (admitted.direction !== "accepted") {
            throw new Error(
              "network-id forced artifact cannot enter the accepted step-01",
            );
          }
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
            prepared: admitted.prepared,
            referenceScriptUtxo: config.stepReferenceScripts[0],
            witnessReferenceScripts: config.witnessReferenceScripts,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "forced_step01") {
          if (admitted.direction !== "forced") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced step-01 handover",
            );
          }
          requireForcedStep();
          await submitNetworkIdForcedStep01({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "forced step-01 thread out-ref",
            ),
            prepared: admitted.prepared,
            referenceScriptUtxo: config.stepReferenceScripts[0],
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "forced_bind") {
          if (admitted.direction !== "forced") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced door",
            );
          }
          await submitNetworkIdForcedBind({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "forced-step thread out-ref",
            ),
            prepared: admitted.prepared,
            referenceScriptUtxo: requireForcedStepReferenceScript(),
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (isForcedScanKind(kind)) {
          if (admitted.direction !== "forced") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced outputs scan",
            );
          }
          requireForcedScan();
          const threadOutRef = requireString(
            context.action.input.threadOutRef,
            "forced-scan thread out-ref",
          );
          const { opening, plan } = forcedScanPlanFor(admitted);
          const scanUtxo = await forcedScanThread(prepared.headerHash);
          if (
            scanUtxo === undefined ||
            outRefLabel(scanUtxo) !== threadOutRef
          ) {
            throw new Error(
              "network-id forced scan thread moved away from the journaled batch out-ref",
            );
          }
          const step = forcedScanNextStep({ utxo: scanUtxo, plan });
          if (
            forcedScanKindFor(step) !== kind ||
            context.action.input.scanAction !== step.kind ||
            context.action.input.scanOrdinal !==
              networkIdForcedScanStepOrdinal(step).toString()
          ) {
            throw new Error(
              "network-id forced scan action is no longer the batch the live thread state is waiting for",
            );
          }
          const publications = await resolveFaultProofFieldCarriagePublications(
            {
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            },
          );
          if (publications === undefined) {
            throw new Error(
              "network-id forced scan carriage is not observable on L1",
            );
          }
          const certificate =
            opening.plan.tier === "Certified"
              ? await resolveFaultProofFieldPreimageCertificate({
                  lucid: config.lucid,
                  network: config.network,
                  planned: opening,
                  certificatePolicyId:
                    config.contracts.fieldPreimageCertificatePolicyId,
                })
              : undefined;
          if (
            opening.plan.tier === "Certified" &&
            (certificate === undefined ||
              context.action.input.certificateOutRef !==
                outRefLabel(certificate))
          ) {
            throw new Error(
              "network-id forced scan does not bind the observed field certificate",
            );
          }
          await authenticateFieldInputs({
            prepared,
            publications,
            ...(certificate === undefined ? {} : { certificate }),
          });
          await submitNetworkIdForcedScanAction({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef,
            prepared: admitted.prepared,
            outputsOpeningPlan: opening,
            scan: plan,
            step,
            referenceScriptUtxo: requireForcedScanReferenceScript(),
            carriageUtxos: publications,
            certificateUtxos: certificate === undefined ? [] : [certificate],
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "publish_field" || kind === "certify_field") {
          const opening = outputsOpeningPlanFor(admitted);
          if (opening === undefined) {
            throw new Error(
              "transaction-network faults have no field-carriage action",
            );
          }
          if (context.action.input.fieldCommitment !== opening.commitment) {
            throw new Error(
              "network-id field action does not match the prepared opening",
            );
          }
          if (kind === "publish_field") {
            const missing = await findMissingFaultProofFieldPublication({
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
            await publishFaultProofFieldCarriage({
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
          const publications = await resolveFaultProofFieldCarriagePublications(
            {
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            },
          );
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
          await certifyFaultProofFieldCarriage({
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
          const opening = finalStepOutputsOpeningPlanFor(admitted);
          const publications =
            opening === undefined
              ? []
              : await resolveFaultProofFieldCarriagePublications({
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
              ? await resolveFaultProofFieldPreimageCertificate({
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
              !workflowTransactionInputOutRefs(transaction.signed).includes(
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
              !workflowTransactionReferenceInputOutRefs(
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
        await captureLocallyEvaluatedTransaction(boundaryInvocation);
      requireReferenceOnlyScriptWitnesses({
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
      // The production funding reservation permit reads the signed body back
      // from the in-memory preflight to reconcile the reserved inputs it
      // actually spends, so the capture is bound beside the journal-safe view.
      return bindWorkflowPreflightTransaction(
        {
          actionId: context.action.actionId,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts",
          localUplcEvaluation: {
            status: "passed",
            evaluator: LOCAL_UPLC_EVALUATOR,
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
        },
        transaction.signed,
      );
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
      const recovery = parseMutationLeaseRecovery(
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
        const txHash = await submitCapturedTransaction(prepared.transaction);
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
    reconcile: async (context): Promise<FraudProofWorkflowReconcileResult> => {
      const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
      const prepared = admitted.prepared;
      const kind = actionKind(context.action);
      // A journaled submission the chain has not reflected yet is resolved
      // through canonical signed-transaction recovery: still in the mempool is
      // `pending`, expired is `not_found`, and anything the port cannot settle
      // stays `unknown`. Reporting `not_found` directly would make the
      // orchestrator abandon the funding transition and rebuild the identical
      // transaction while the original is still landing.
      const unconfirmed = async (
        lease?: StateQueueMutationLease,
      ): Promise<FraudProofWorkflowReconcileResult> => {
        const observe = config.rawL1?.observeSignedTransaction;
        if (context.txHash === undefined || observe === undefined) {
          return { kind: "not_found" };
        }
        const { authorizeResubmission } = context;
        return await reconcileSignedWorkflowTransaction({
          transactionHash: context.txHash,
          signedTransactionCborHex: context.signedTransactionCborHex,
          observe,
          rebroadcast: config.rawL1?.rebroadcastSignedTransaction,
          authorizeResubmission:
            authorizeResubmission === undefined
              ? undefined
              : async (signed) => {
                  await lease?.renew();
                  await authorizeResubmission(signed);
                },
        });
      };
      let advanced: boolean;
      if (kind === "publish_field" || kind === "certify_field") {
        const opening = outputsOpeningPlanFor(admitted);
        if (opening === undefined) {
          return {
            kind: "conflict",
            reason: "field action exists for a transaction-network fault",
          };
        }
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
          if (candidate === undefined) return await unconfirmed();
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
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (
            certificate === undefined ||
            certificate.txHash !== context.txHash
          ) {
            return await unconfirmed();
          }
          const certification = deriveFieldPreimageCertification(opening.plan);
          const observation = await observer.observeExact({
            headerHash: prepared.headerHash,
            kind: "field_certificate",
            address: fieldPreimageCertificateAddress({
              network: config.network,
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
            }),
            expectedOutRef: outRefLabel(certificate),
            expectedDatumCbor: certification.datumCbor,
            expectedUnit: `${config.contracts.fieldPreimageCertificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
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
        const forcedOccupied = await forcedDoorOccupied({ admitted, kind });
        const scanOutRef = await forcedScanOutRef({ admitted, kind });
        const scanOccupied = scanOutRef !== undefined;
        const beyondStep01 =
          rawStage.kind === "step"
            ? rawStage.step >= 2
            : rawStage.kind === "proof_token" || rawStage.kind === "removed";
        const tokenMinted =
          rawStage.kind === "proof_token" || rawStage.kind === "removed";
        const stageAdvanced = isForcedScanKind(kind)
          ? scanOutRef !== context.action.input.threadOutRef
          : kind === "init"
            ? rawStage.kind !== "not_started" || forcedOccupied || scanOccupied
            : kind === "step01"
              ? beyondStep01
              : kind === "forced_step01"
                ? forcedOccupied || scanOccupied || beyondStep01
                : kind === "forced_bind"
                  ? !forcedOccupied && (scanOccupied || beyondStep01)
                  : kind === "step02"
                    ? tokenMinted
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
        const state = await live(prepared.headerHash);
        const forcedOccupied = await forcedDoorOccupied({ admitted, kind });
        const scanOutRef = await forcedScanOutRef({ admitted, kind });
        const scanOccupied = scanOutRef !== undefined;
        const beyondStep01 =
          state.step01 === undefined &&
          (state.step02 !== undefined ||
            state.proof !== undefined ||
            state.stateQueue === undefined);
        advanced = isForcedScanKind(kind)
          ? scanOutRef !== context.action.input.threadOutRef
          : kind === "init"
            ? state.step01 !== undefined ||
              state.step02 !== undefined ||
              state.proof !== undefined ||
              state.stateQueue === undefined ||
              forcedOccupied ||
              scanOccupied
            : kind === "step01"
              ? beyondStep01
              : kind === "forced_step01"
                ? forcedOccupied || scanOccupied || beyondStep01
                : kind === "forced_bind"
                  ? !forcedOccupied && (scanOccupied || beyondStep01)
                  : kind === "step02"
                    ? state.step02 === undefined &&
                      (state.proof !== undefined ||
                        state.stateQueue === undefined)
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
            : await recoverMutationLease({
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
          : await recoverMutationLease({
              config,
              txHash: context.txHash,
              durableRecovery: context.durableRecovery,
              mutationLeaseByTxHash,
            });
      if (recovery.kind === "conflict") return recovery;
      const result = await unconfirmed(recovery.lease);
      if (result.kind === "not_found") {
        await recovery.lease?.release();
        if (context.txHash !== undefined) {
          mutationLeaseByTxHash.delete(context.txHash);
        }
      } else if (result.kind === "conflict") {
        await recovery.lease?.fail(result.reason);
        if (context.txHash !== undefined) {
          mutationLeaseByTxHash.delete(context.txHash);
        }
      } else {
        await recovery.lease?.renew();
      }
      return result;
    },
  };
};

/** Production run/resume with its header derived from admitted raw L1 bytes. */
export const runOrResumeManifestBoundNetworkIdWorkflow = async ({
  workflow,
  sources,
  journal,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly workflow: ManifestBoundNetworkIdWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
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
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
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
