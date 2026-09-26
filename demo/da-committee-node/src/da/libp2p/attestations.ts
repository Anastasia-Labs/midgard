import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  type DaAttestationGossip,
  DaGossipTopic,
  DaRequestResponseProtocol,
  decodeDaAttestationGossipCbor,
  decodeDaAttestationsByHeaderRequestCbor,
  decodeDaAttestationsByHeaderResponseCbor,
  encodeDaAttestationGossipCbor,
  encodeDaAttestationsByHeaderRequestCbor,
  encodeDaAttestationsByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { Libp2pDaTransportLimits } from "../../config.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaStoredPayloadCountSet,
  DaStoredPayloadRootSet,
  DaStoredValidationSummary,
  PayloadRootSet,
  StateQueueHeaderRecord,
} from "../../domain.js";
import {
  buildDaSignatureConflictEvidence,
  type DaAvailabilityCommitmentAuthority,
  deriveExpectedDaAvailabilityCommitment,
  validateDaSignatureRecord,
} from "../../peer/signatures.js";
import type { DaCommitteeValidation } from "../../signer.js";
import type { CommitteeStore } from "../../store.js";
import type { DaGossipMessageHandler } from "./DaGossip.js";
import type { DaLibp2pNode, DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import type { DaPeerRegistry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";

export type DaAttestationPeer = {
  readonly peerId: string;
  readonly signerIndex?: number;
};

export type DaAttestationPublishResult =
  | { readonly status: "accepted" }
  | { readonly status: "rejected" | "unavailable"; readonly reason: string };

export interface DaAttestationExchange {
  publishAttestation(args: {
    readonly peer: DaAttestationPeer;
    readonly record: DaSignatureRecord;
  }): Promise<DaAttestationPublishResult>;
  attestationsByHeader(args: {
    readonly peer: DaAttestationPeer;
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
  }): Promise<readonly DaSignatureRecord[]>;
  publishConflictEvidence(gossipCbor: Buffer): Promise<void>;
}

export type StoreBackedDaAttestationProtocolDeps = {
  readonly deploymentFingerprint: string;
  readonly localPeerId: string;
  readonly committeeValidation: DaCommitteeValidation;
  readonly availabilityCommitmentAuthority: DaAvailabilityCommitmentAuthority;
  readonly store: Pick<
    CommitteeStore,
    | "getDaPayload"
    | "getL1SourceState"
    | "saveDaSignature"
    | "listDaSignatures"
    | "saveDaConflictEvidence"
  >;
};

export class StoreBackedDaAttestationProtocol {
  private readonly deps: StoreBackedDaAttestationProtocolDeps;
  private publishConflictEvidence?: (gossipCbor: Buffer) => Promise<void>;

  constructor(deps: StoreBackedDaAttestationProtocolDeps) {
    this.deps = deps;
  }

  setConflictEvidencePublisher(
    publisher: (gossipCbor: Buffer) => Promise<void>,
  ): void {
    this.publishConflictEvidence = publisher;
  }

  async acceptAttestation(args: {
    readonly record: DaSignatureRecord;
    readonly sourcePeerId: string;
  }): Promise<DaAttestationPublishResult> {
    if ((await this.deps.store.getL1SourceState())?.status === "quarantined") {
      return { status: "rejected", reason: "L1 source is quarantined" };
    }
    const payload = await this.deps.store.getDaPayload(args.record.headerHash);
    if (!isVerifiedPayload(payload)) {
      return {
        status: "rejected",
        reason: "verified payload is not available",
      };
    }
    const cryptographicValidationError = validateDaSignatureRecord({
      body: args.record,
      headerHash: args.record.headerHash,
      deploymentFingerprint: this.deps.deploymentFingerprint,
      signerValidation: this.deps.committeeValidation,
    });
    if (cryptographicValidationError !== undefined) {
      return { status: "rejected", reason: cryptographicValidationError };
    }
    const now = new Date().toISOString();
    const canonicalCandidate: DaSignatureRecord = {
      ...args.record,
      broadcastStatus: "posted",
      source: "peer",
      sourcePeer: args.sourcePeerId,
      receivedAt: now,
      verifiedAt: now,
    };
    const priorSameHeaderSigner = (
      await this.deps.store.listDaSignatures(args.record.headerHash)
    ).find(
      (entry) =>
        entry.signerIndex === canonicalCandidate.signerIndex &&
        entry.availabilityCommitmentDigest !==
          canonicalCandidate.availabilityCommitmentDigest,
    );
    await this.deps.store.saveDaSignature(canonicalCandidate);
    if (priorSameHeaderSigner !== undefined) {
      const conflict = buildDaSignatureConflictEvidence({
        first: priorSameHeaderSigner,
        second: canonicalCandidate,
        daVkey:
          this.deps.committeeValidation.committeeKeys[
            canonicalCandidate.signerIndex
          ]!,
        reporterPeerId: this.deps.localPeerId,
        receivedAt: now,
      });
      if (
        conflict !== undefined &&
        (await this.deps.store.saveDaConflictEvidence(conflict.record))
      ) {
        await this.publishConflictEvidence?.(conflict.gossipCbor);
      }
    }
    const authorityValidationError = validateDaSignatureRecord({
      body: canonicalCandidate,
      headerHash: args.record.headerHash,
      deploymentFingerprint: this.deps.deploymentFingerprint,
      signerValidation: this.deps.committeeValidation,
      verifiedPayload: payload,
      ...expectedCommitmentValidation(
        this.deps.availabilityCommitmentAuthority,
        args.record.headerHash,
        payload,
      ),
    });
    if (authorityValidationError !== undefined) {
      return { status: "rejected", reason: authorityValidationError };
    }
    return { status: "accepted" };
  }

  async attestationsByHeader(args: {
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
  }): Promise<readonly DaSignatureRecord[]> {
    if (args.deploymentFingerprint !== this.deps.deploymentFingerprint) {
      return [];
    }
    return this.serveableAttestations(args.headerHash);
  }

  async handleAttestationsByHeaderRequest(
    requestCbor: Uint8Array,
  ): Promise<Buffer> {
    const request = decodeDaAttestationsByHeaderRequestCbor(requestCbor);
    const headerHash = request.headerHash.toString("hex");
    if (
      request.deploymentFingerprint.toString("hex") !==
      this.deps.deploymentFingerprint
    ) {
      return encodeDaAttestationsByHeaderResponseCbor({
        status: "rejected",
        headerHash: request.headerHash,
        attestations: [],
        reasonCode: "deployment_fingerprint_mismatch",
      });
    }
    const records = await this.serveableAttestations(headerHash);
    const acceptedSignerIndexes =
      request.acceptedSignerIndexes === null
        ? undefined
        : new Set(request.acceptedSignerIndexes);
    const attestations: DaAttestationGossip[] = [];
    for (const record of records) {
      if (
        request.maxAttestations !== null &&
        attestations.length >= request.maxAttestations
      ) {
        break;
      }
      if (
        acceptedSignerIndexes !== undefined &&
        !acceptedSignerIndexes.has(record.signerIndex)
      ) {
        continue;
      }
      try {
        attestations.push(this.gossipMessageFor(record));
      } catch {
        continue;
      }
    }
    return encodeDaAttestationsByHeaderResponseCbor({
      status: attestations.length > 0 ? "found" : "not_found",
      headerHash: request.headerHash,
      attestations,
      reasonCode: null,
    });
  }

  gossipMessageFor(record: DaSignatureRecord): DaAttestationGossip {
    const daVkey =
      this.deps.committeeValidation.committeeKeys[record.signerIndex];
    if (daVkey === undefined) {
      throw new Error("DA attestation signer index is outside the committee");
    }
    return daAttestationGossipFromRecord({
      record,
      daVkey,
      announcedByPeerId: this.deps.localPeerId,
    });
  }

  availabilityCommitmentAuthority(): DaAvailabilityCommitmentAuthority {
    return this.deps.availabilityCommitmentAuthority;
  }

  private async serveableAttestations(
    headerHash: string,
  ): Promise<readonly DaSignatureRecord[]> {
    if ((await this.deps.store.getL1SourceState())?.status === "quarantined") {
      return [];
    }
    const payload = await this.deps.store.getDaPayload(headerHash);
    if (!isVerifiedPayload(payload)) {
      return [];
    }
    const expected = deriveExpectedDaAvailabilityCommitment({
      authority: this.deps.availabilityCommitmentAuthority,
      headerHash,
      payloadCborHex: payload.payloadCborHex,
    });
    return (await this.deps.store.listDaSignatures(headerHash)).filter(
      (record) =>
        record.broadcastStatus !== "post_failed" &&
        record.availabilityCommitmentCbor === expected.commitmentCbor &&
        record.availabilityCommitmentDigest === expected.commitmentDigest,
    );
  }
}

export const daAttestationGossipFromRecord = ({
  record,
  daVkey,
  announcedByPeerId,
  retentionUntilSlot = 0,
}: {
  readonly record: DaSignatureRecord;
  readonly daVkey: string;
  readonly announcedByPeerId: string;
  readonly retentionUntilSlot?: number;
}): DaAttestationGossip => ({
  deploymentFingerprint: Buffer.from(record.deploymentFingerprint, "hex"),
  headerHash: Buffer.from(record.headerHash, "hex"),
  payloadHash: Buffer.from(record.payloadHash, "hex"),
  availabilityCommitmentCbor: Buffer.from(
    record.availabilityCommitmentCbor,
    "hex",
  ),
  availabilityCommitmentDigest: Buffer.from(
    record.availabilityCommitmentDigest,
    "hex",
  ),
  signerIndex: record.signerIndex,
  daVkey: Buffer.from(daVkey, "hex"),
  onChainWitness: Buffer.from(record.signatureWitness, "hex"),
  retentionUntilSlot,
  announcedByPeerId,
});

export const encodeDaAttestationGossip = (
  message: DaAttestationGossip,
): Buffer => encodeDaAttestationGossipCbor(message);

export const decodeDaAttestationGossip = (
  bytes: Uint8Array,
): DaAttestationGossip => decodeDaAttestationGossipCbor(bytes);

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

/**
 * Ingests committee signatures gossiped on the attestations topic. The
 * gossip author is authenticated by StrictSign; it must be a manifest
 * committee peer publishing its own signer index, because members only
 * gossip their own signatures. Accepted records are stored as peer
 * signatures for the local coordinator. A rejection throws, which the gossip
 * pipeline reports through its message error hook. Gossip is best effort:
 * `attestationsByHeader` pulls remain the recovery path for signatures that
 * arrive before the local payload is verified or while a peer is offline.
 */
export const createDaLibp2pAttestationGossipHandlers = ({
  deploymentFingerprint,
  registry,
  protocol,
  committeeValidation,
  store,
}: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly protocol: Pick<
    StoreBackedDaAttestationProtocol,
    "acceptAttestation"
  >;
  readonly committeeValidation: DaCommitteeValidation;
  readonly store: Pick<CommitteeStore, "getDaPayload" | "getStateQueueHeader">;
}): ReadonlyMap<DaGossipTopic, DaGossipMessageHandler> =>
  new Map([
    [
      DaGossipTopic.attestations,
      async (context) => {
        if (context.topicName !== DaGossipTopic.attestations) {
          throw new Error("DA attestation gossip arrived on the wrong topic");
        }
        const sender = registry.requireKnownPeer(context.remotePeerId);
        const attestation = decodeDaAttestationGossip(context.data);
        if (!encodeDaAttestationGossip(attestation).equals(context.data)) {
          throw new Error("DA attestation gossip must use canonical CBOR");
        }
        if (
          sender.signerIndex === undefined ||
          sender.signerIndex !== attestation.signerIndex
        ) {
          throw new Error(
            `DA attestation gossip signer index ${attestation.signerIndex.toString()} does not belong to authenticated peer ${sender.peerId}`,
          );
        }
        const converted = await daSignatureRecordFromAttestation({
          deploymentFingerprint,
          committeeValidation,
          store,
          announcerPeerId: sender.peerId,
          attestation,
        });
        if (converted.status === "rejected") {
          throw new Error(
            `rejected DA attestation gossip from ${sender.peerId}: ${converted.reason}`,
          );
        }
        const accepted = await protocol.acceptAttestation({
          record: converted.record,
          sourcePeerId: sender.peerId,
        });
        if (accepted.status !== "accepted") {
          throw new Error(
            `rejected DA attestation gossip from ${sender.peerId}: ${accepted.reason}`,
          );
        }
      },
    ],
  ]);

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

const isVerifiedPayload = (
  payload: DaPayloadRecord | undefined,
): payload is DaPayloadRecord =>
  payload !== undefined &&
  payload.validationStatus === "verified" &&
  payload.payloadSha256.length > 0;

const expectedCommitmentValidation = (
  authority: DaAvailabilityCommitmentAuthority,
  headerHash: string,
  payload: DaPayloadRecord,
): Readonly<{
  expectedAvailabilityCommitmentCbor: string;
  expectedAvailabilityCommitmentDigest: string;
}> => {
  const expected = deriveExpectedDaAvailabilityCommitment({
    authority,
    headerHash,
    payloadCborHex: payload.payloadCborHex,
  });
  return {
    expectedAvailabilityCommitmentCbor: expected.commitmentCbor,
    expectedAvailabilityCommitmentDigest: expected.commitmentDigest,
  };
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
