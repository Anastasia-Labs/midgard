import { computeHash28, decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { encodeHeaderCbor } from "@al-ft/midgard-sdk/core";
import {
  type AuthenticatedStateQueueHeaderObservation,
  bindExactVerdictSubjectReason,
  forcedVerdictSubject,
  type NetworkIdFault,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import type { FraudProofWorkflowTerminal } from "../workflow/journal.js";
import { type FraudProofRawL1FamilyStage } from "../workflow/raw-l1-family-derivation.js";
import type { FraudProofOwnThreads } from "../workflow/raw-l1-family-derivation.own-thread.js";
import { type FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import {
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "../workflow/signed-transaction-reconciliation.js";
import type { PreparedNetworkIdAcceptedForced } from "./accepted-forced.js";
import { type PreparedNetworkIdProof } from "./prepare.js";
import {
  admitNetworkIdAcceptedForcedArtifact,
  type NetworkIdAcceptedForcedWorkflowArtifact,
} from "./workflow-adapter.accepted-forced-artifact.js";
import { bindForcedLeafMembership } from "./workflow-adapter.bind-forced-leaf.js";
import {
  ARTIFACT_VERSION,
  type ForcedSourcePayload,
  NetworkIdForcedSourceSchema,
  type NetworkIdForcedWorkflowArtifact,
  preparedFromArtifact,
  requireString,
  type WorkflowContext,
} from "./workflow-adapter.prepared-from-artifact.js";
import {
  NETWORK_ID_MISMATCH_REASON,
  networkIdOfOutput,
  networkIdWrongfulRejectionCloses,
  type NetworkIdWrongfulRejectionEvidence,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection.js";

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
  const outputNetworkIds =
    decodeMidgardFieldPreimage(outputsPreimage).map(networkIdOfOutput);
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
    }),
    evidence,
  });
};

/** Either direction of the family, discriminated by the artifact itself. */
export type AdmittedNetworkIdArtifact =
  | {
      readonly direction: "accepted_forced";
      readonly prepared: PreparedNetworkIdAcceptedForced;
    }
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
  if (candidate.direction === "accepted_forced") {
    return {
      direction: "accepted_forced",
      prepared: admitNetworkIdAcceptedForcedArtifact(
        value as unknown as NetworkIdAcceptedForcedWorkflowArtifact,
      ),
    };
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

export type NetworkIdWorkflowTerminalFacts = {
  readonly economics: FraudProofWorkflowTerminal["economics"];
  readonly observedAt: FraudProofWorkflowTerminal["observedAt"];
};

export interface NetworkIdRawL1ObservationPort {
  readonly publications?: FraudProofAuthenticatedPublicationObserver;
  /**
   * Where an exact signed transaction stands on the canonical chain
   * (mempool, inclusion, expiry).
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
  /** Our threads on the header at every physical position of the family,
   * forced door and forced outputs scan included. */
  observeOwnThreads?(input: {
    readonly headerHash: string;
  }): Promise<FraudProofOwnThreads>;
}
