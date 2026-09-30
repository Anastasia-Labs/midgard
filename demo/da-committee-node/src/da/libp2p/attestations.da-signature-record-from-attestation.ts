import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  type DaAttestationGossip,
  DaGossipTopic,
  DaRequestResponseProtocol,
  decodeDaAttestationsByHeaderResponseCbor,
  encodeDaAttestationsByHeaderRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { Libp2pDaTransportLimits } from "../../config.js";
import type {
  DaSignatureRecord,
  DaStoredPayloadCountSet,
  DaStoredPayloadRootSet,
  DaStoredValidationSummary,
  PayloadRootSet,
  StateQueueHeaderRecord,
} from "../../domain.js";
import { validateDaSignatureRecord } from "../../peer/signatures.js";
import type { DaCommitteeValidation } from "../../signer.js";
import type { CommitteeStore } from "../../store.js";
import {
  type DaAttestationExchange,
  type DaAttestationPeer,
  type DaAttestationPublishResult,
  encodeDaAttestationGossip,
  isVerifiedPayload,
  StoreBackedDaAttestationProtocol,
} from "./attestations.store-backed-da-attestation-protocol.js";
import type { DaLibp2pNode, DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import type { DaPeerRegistry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";

export type DaLibp2pAttestationExchangeOptions = {
  readonly deploymentFingerprint: string;
  readonly localPeerId: string;
  readonly node: Pick<DaLibp2pNode, "request" | "publishGossip">;
  readonly registry: DaPeerRegistry;
  readonly protocol: StoreBackedDaAttestationProtocol;
  readonly committeeValidation: DaCommitteeValidation;
  readonly store: Pick<CommitteeStore, "getDaPayload" | "getStateQueueHeader">;
  readonly requestTimeoutMs: number;
};

export class DaLibp2pAttestationExchange implements DaAttestationExchange {
  private readonly options: DaLibp2pAttestationExchangeOptions;
  private readonly protocolIds: ReturnType<typeof createDaProtocolAllowlist>;

  constructor(options: DaLibp2pAttestationExchangeOptions) {
    this.options = options;
    this.protocolIds = createDaProtocolAllowlist(options.deploymentFingerprint);
    options.protocol.setConflictEvidencePublisher((gossipCbor) =>
      options.node.publishGossip(DaGossipTopic.conflicts, gossipCbor),
    );
  }

  async publishAttestation({
    record,
  }: {
    readonly peer: DaAttestationPeer;
    readonly record: DaSignatureRecord;
  }): Promise<DaAttestationPublishResult> {
    try {
      await this.options.node.publishGossip(
        DaGossipTopic.attestations,
        encodeDaAttestationGossip(
          this.options.protocol.gossipMessageFor(record),
        ),
      );
      return { status: "accepted" };
    } catch (error) {
      return {
        status: "unavailable",
        reason: error instanceof Error ? error.message : String(error),
      };
    }
  }

  async attestationsByHeader({
    peer,
    deploymentFingerprint,
    headerHash,
  }: {
    readonly peer: DaAttestationPeer;
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
  }): Promise<readonly DaSignatureRecord[]> {
    const registryEntry = this.options.registry.getByPeerId(peer.peerId);
    if (registryEntry === undefined) {
      throw new Error(`unknown DA libp2p attestation peer ${peer.peerId}`);
    }
    const response = decodeDaAttestationsByHeaderResponseCbor(
      await this.options.node.request({
        peer: registryEntry,
        protocolId: this.protocolId(
          DaRequestResponseProtocol.attestationsByHeader,
        ),
        timeoutMs: this.options.requestTimeoutMs,
        payload: encodeDaAttestationsByHeaderRequestCbor({
          deploymentFingerprint: Buffer.from(deploymentFingerprint, "hex"),
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedSignerIndexes: null,
          maxAttestations: null,
        }),
      }),
    );
    if (response.status !== "found") {
      return [];
    }
    const records: DaSignatureRecord[] = [];
    for (const attestation of response.attestations) {
      const result = await daSignatureRecordFromAttestation({
        deploymentFingerprint: this.options.deploymentFingerprint,
        committeeValidation: this.options.committeeValidation,
        store: this.options.store,
        announcerPeerId: registryEntry.peerId,
        attestation,
      });
      if (result.status === "converted") {
        records.push(result.record);
      }
    }
    return records;
  }

  async publishConflictEvidence(gossipCbor: Buffer): Promise<void> {
    await this.options.node.publishGossip(DaGossipTopic.conflicts, gossipCbor);
  }

  private protocolId(protocol: DaRequestResponseProtocol): string {
    return this.protocolIds.protocolIdByName.get(protocol)!;
  }
}

export const createDaLibp2pAttestationRequestHandlers = ({
  deploymentFingerprint,
  protocol,
  limits,
}: {
  readonly deploymentFingerprint: string;
  readonly protocol: StoreBackedDaAttestationProtocol;
  readonly limits: Libp2pDaTransportLimits;
}): ReadonlyMap<string, DaLibp2pStreamHandler> => {
  const protocolIds = createDaProtocolAllowlist(deploymentFingerprint);
  const protocolId = protocolIds.protocolIdByName.get(
    DaRequestResponseProtocol.attestationsByHeader,
  )!;
  return new Map([
    [
      protocolId,
      async ({ stream }) => {
        const requestCbor = await readSingleDaStreamFrame(stream, {
          maxFrameBytes: limits.maxPayloadBytes,
        });
        const responseCbor =
          await protocol.handleAttestationsByHeaderRequest(requestCbor);
        await writeDaStreamFrame(stream, responseCbor, {
          maxFrameBytes: limits.maxPayloadBytes,
          close: true,
        });
      },
    ],
  ]);
};

export type DaSignatureRecordFromAttestationResult =
  | { readonly status: "converted"; readonly record: DaSignatureRecord }
  | { readonly status: "rejected"; readonly reason: string };

/**
 * Converts an attestation announced by `announcerPeerId` (over pull or
 * gossip) into a peer signature record bound to the locally verified payload
 * and observed header. Only cryptographic validity is checked here: every
 * valid commitment variant is preserved so callers can compare variants and
 * emit equivocation evidence before deciding whether a record belongs to the
 * locally authorised commitment group.
 */
export const daSignatureRecordFromAttestation = async ({
  deploymentFingerprint,
  committeeValidation,
  store,
  announcerPeerId,
  attestation,
}: {
  readonly deploymentFingerprint: string;
  readonly committeeValidation: DaCommitteeValidation;
  readonly store: Pick<CommitteeStore, "getDaPayload" | "getStateQueueHeader">;
  readonly announcerPeerId: string;
  readonly attestation: DaAttestationGossip;
}): Promise<DaSignatureRecordFromAttestationResult> => {
  const headerHash = attestation.headerHash.toString("hex");
  if (
    attestation.deploymentFingerprint.toString("hex") !== deploymentFingerprint
  ) {
    return { status: "rejected", reason: "deployment fingerprint mismatch" };
  }
  if (attestation.announcedByPeerId !== announcerPeerId) {
    return {
      status: "rejected",
      reason: "announcing peer does not match the authenticated peer",
    };
  }
  const expectedDaVkey =
    committeeValidation.committeeKeys[attestation.signerIndex];
  if (
    expectedDaVkey === undefined ||
    expectedDaVkey !== attestation.daVkey.toString("hex")
  ) {
    return {
      status: "rejected",
      reason: "DA vkey does not match the committee key at the signer index",
    };
  }
  const payload = await store.getDaPayload(headerHash);
  const header = await store.getStateQueueHeader(headerHash);
  if (!isVerifiedPayload(payload) || header === undefined) {
    return {
      status: "rejected",
      reason: "verified payload or observed header is not available",
    };
  }
  if (payload.payloadSha256 !== attestation.payloadHash.toString("hex")) {
    return {
      status: "rejected",
      reason: "payload hash does not match the verified payload",
    };
  }
  const now = new Date().toISOString();
  const record: DaSignatureRecord = {
    deploymentFingerprint,
    headerHash,
    signerIndex: attestation.signerIndex,
    signatureWitness: attestation.onChainWitness.toString("hex"),
    payloadHash: payload.payloadSha256,
    availabilityCommitmentCbor:
      attestation.availabilityCommitmentCbor.toString("hex"),
    availabilityCommitmentDigest:
      attestation.availabilityCommitmentDigest.toString("hex"),
    committeeSignersHash: committeeValidation.committeeSignersHash,
    signedAt: now,
    broadcastStatus: "posted",
    source: "peer",
    sourcePeer: announcerPeerId,
    receivedAt: now,
    verifiedAt: now,
    l1ChainPoint: header.observedChainPoint,
    validation: validationSummaryFromHeader(
      header,
      rootSummaryFromHeader(header, payload.rootSummary),
    ),
  };
  const validationError = validateDaSignatureRecord({
    body: record,
    headerHash,
    deploymentFingerprint,
    signerValidation: committeeValidation,
  });
  return validationError === undefined
    ? { status: "converted", record }
    : { status: "rejected", reason: validationError };
};

const validationSummaryFromHeader = (
  header: StateQueueHeaderRecord,
  rootSummary: DaStoredPayloadRootSet,
): DaStoredValidationSummary => ({
  payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
  rootsMatch: true,
  stateQueueOutRef: header.stateQueueOutRef,
  headerHash: header.headerHash,
  rootSummary,
  countSummary: countSummaryFromHeader(header),
  l1Header: {
    startTime: header.header.startTime.toString(),
    endTime: header.header.endTime.toString(),
    operatorVkey: header.header.operatorVkey,
    prevHeaderHash: header.header.prevHeaderHash,
    protocolVersion: header.header.protocolVersion.toString(),
  },
});

const rootSummaryFromHeader = (
  header: StateQueueHeaderRecord,
  rootSummary?: PayloadRootSet,
): DaStoredPayloadRootSet => ({
  ...(rootSummary ?? {
    utxosRoot: header.header.utxosRoot,
    transactionsRoot: header.header.transactionsRoot,
    depositsRoot: header.header.depositsRoot,
    withdrawalsRoot: header.header.withdrawalsRoot,
    forcedTransactionsRoot: header.header.forcedTransactionsRoot,
    transitionTraceRoot: header.header.transitionTraceRoot,
    eventToStepRoot: header.header.eventToStepRoot,
  }),
  validationTracesRoot: header.header.validationTracesRoot,
});

const countSummaryFromHeader = (
  header: StateQueueHeaderRecord,
): DaStoredPayloadCountSet => ({
  withdrawalCount: header.header.withdrawalCount,
  forcedTransactionCount: header.header.forcedTransactionCount,
  l2TransactionCount: header.header.l2TransactionCount,
  depositCount: header.header.depositCount,
  totalEventCount: header.header.totalEventCount,
  transitionStepCount: header.header.transitionStepCount,
  validationTraceCount: header.header.validationTraceCount,
});
