import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardOutputFieldPreimage,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import {
  type AuthenticatedStateQueueHeaderObservation,
  bindExactVerdictSubjectReason,
  encodeHeaderCbor,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  type NetworkIdFault,
  OutputReferenceSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  FraudProofWorkflowTerminal,
  JournalJsonObject,
} from "../workflow/journal.js";
import { type FraudProofRawL1FamilyStage } from "../workflow/raw-l1-family-derivation.js";
import { type FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import {
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "../workflow/signed-transaction-reconciliation.js";
import { type PreparedNetworkIdProof } from "./prepare.js";
import {
  ARTIFACT_VERSION,
  type ForcedSourcePayload,
  NetworkIdForcedSourceSchema,
  type NetworkIdForcedWorkflowArtifact,
  preparedFromArtifact,
  proofSteps,
  requireString,
  type WorkflowContext,
} from "./workflow-adapter.prepared-from-artifact.js";
import {
  NETWORK_ID_MISMATCH_REASON,
  networkIdWrongfulRejectionCloses,
  type NetworkIdWrongfulRejectionEvidence,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection.js";

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

export const confirmedTxHash = (
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

export const latestRemovalIntent = (context: WorkflowContext) =>
  [...context.entries]
    .reverse()
    .map((entry) => entry.event)
    .find(
      (event) =>
        event.kind === "submission_intent" &&
        event.actionInput.kind === "remove",
    );

export const parseMutationLeaseRecovery = (
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
  observeRetainedHeader?(input: {
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
