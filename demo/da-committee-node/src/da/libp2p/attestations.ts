import {
  type DaAttestationGossipV1,
  DaGossipTopic,
  DaRequestResponseProtocol,
  decodeDaAttestationGossipV1Cbor,
  decodeDaAttestationsByHeaderRequestV1Cbor,
  decodeDaAttestationsByHeaderResponseV1Cbor,
  encodeDaAttestationGossipV1Cbor,
  encodeDaAttestationsByHeaderRequestV1Cbor,
  encodeDaAttestationsByHeaderResponseV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { Libp2pDaTransportLimits } from "../../config.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaStoredPayloadCountSetV1,
  DaStoredPayloadRootSetV1,
  DaStoredValidationSummaryV1,
  PayloadRootSet,
  StateQueueHeaderRecord,
} from "../../domain.js";
import { validateDaSignatureRecord } from "../../peer/signatures.js";
import type { DaCommitteeValidation } from "../../signer.js";
import type { WatcherStore } from "../../store.js";
import type { DaLibp2pNode, DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import type { DaPeerRegistry, DaPeerRegistryEntry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "./DaStreamCodec.js";

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
}

export type StoreBackedDaAttestationProtocolDeps = {
  readonly deploymentFingerprint: string;
  readonly localPeerId: string;
  readonly committeeValidation: DaCommitteeValidation;
  readonly store: Pick<
    WatcherStore,
    "getDaPayload" | "saveDaSignature" | "listDaSignatures"
  >;
};

export class StoreBackedDaAttestationProtocol {
  private readonly deps: StoreBackedDaAttestationProtocolDeps;

  constructor(deps: StoreBackedDaAttestationProtocolDeps) {
    this.deps = deps;
  }

  async acceptAttestation(args: {
    readonly record: DaSignatureRecord;
    readonly sourcePeerId: string;
  }): Promise<DaAttestationPublishResult> {
    const payload = await this.deps.store.getDaPayload(args.record.headerHash);
    if (!isVerifiedPayload(payload)) {
      return {
        status: "rejected",
        reason: "verified payload is not available",
      };
    }
    const validationError = validateDaSignatureRecord({
      body: args.record,
      headerHash: args.record.headerHash,
      deploymentFingerprint: this.deps.deploymentFingerprint,
      signerValidation: this.deps.committeeValidation,
      verifiedPayload: payload,
    });
    if (validationError !== undefined) {
      return { status: "rejected", reason: validationError };
    }
    const now = new Date().toISOString();
    await this.deps.store.saveDaSignature({
      ...args.record,
      broadcastStatus: "posted",
      source: "peer",
      sourcePeer: args.sourcePeerId,
      receivedAt: now,
      verifiedAt: now,
    });
    return { status: "accepted" };
  }

  async attestationsByHeader(args: {
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
  }): Promise<readonly DaSignatureRecord[]> {
    if (args.deploymentFingerprint !== this.deps.deploymentFingerprint) {
      return [];
    }
    return this.deps.store.listDaSignatures(args.headerHash);
  }

  async handleAttestationsByHeaderRequest(
    requestCbor: Uint8Array,
  ): Promise<Buffer> {
    const request = decodeDaAttestationsByHeaderRequestV1Cbor(requestCbor);
    const headerHash = request.headerHash.toString("hex");
    if (
      request.deploymentFingerprint.toString("hex") !==
      this.deps.deploymentFingerprint
    ) {
      return encodeDaAttestationsByHeaderResponseV1Cbor({
        status: "rejected",
        headerHash: request.headerHash,
        attestations: [],
        reasonCode: "deployment_fingerprint_mismatch",
      });
    }
    const records = await this.deps.store.listDaSignatures(headerHash);
    const acceptedSignerIndexes =
      request.acceptedSignerIndexes === null
        ? undefined
        : new Set(request.acceptedSignerIndexes);
    const attestations: DaAttestationGossipV1[] = [];
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
    return encodeDaAttestationsByHeaderResponseV1Cbor({
      status: attestations.length > 0 ? "found" : "not_found",
      headerHash: request.headerHash,
      attestations,
      reasonCode: null,
    });
  }

  gossipMessageFor(record: DaSignatureRecord): DaAttestationGossipV1 {
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
}): DaAttestationGossipV1 => ({
  deploymentFingerprint: Buffer.from(record.deploymentFingerprint, "hex"),
  headerHash: Buffer.from(record.headerHash, "hex"),
  payloadHash: Buffer.from(record.payloadHash, "hex"),
  signerIndex: record.signerIndex,
  daVkey: Buffer.from(daVkey, "hex"),
  onChainWitness: Buffer.from(record.signatureWitness, "hex"),
  retentionUntilSlot,
  announcedByPeerId,
});

export const encodeDaAttestationGossip = (
  message: DaAttestationGossipV1,
): Buffer => encodeDaAttestationGossipV1Cbor(message);

export const decodeDaAttestationGossip = (
  bytes: Uint8Array,
): DaAttestationGossipV1 => decodeDaAttestationGossipV1Cbor(bytes);

export type DaLibp2pAttestationExchangeOptions = {
  readonly deploymentFingerprint: string;
  readonly localPeerId: string;
  readonly node: Pick<DaLibp2pNode, "request" | "publishGossip">;
  readonly registry: DaPeerRegistry;
  readonly protocol: StoreBackedDaAttestationProtocol;
  readonly committeeValidation: DaCommitteeValidation;
  readonly store: Pick<WatcherStore, "getDaPayload" | "getStateQueueHeader">;
  readonly requestTimeoutMs: number;
};

export class DaLibp2pAttestationExchange implements DaAttestationExchange {
  private readonly options: DaLibp2pAttestationExchangeOptions;
  private readonly protocolIds: ReturnType<typeof createDaProtocolAllowlist>;

  constructor(options: DaLibp2pAttestationExchangeOptions) {
    this.options = options;
    this.protocolIds = createDaProtocolAllowlist(options.deploymentFingerprint);
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
    const response = decodeDaAttestationsByHeaderResponseV1Cbor(
      await this.options.node.request({
        peer: registryEntry,
        protocolId: this.protocolId(
          DaRequestResponseProtocol.attestationsByHeader,
        ),
        timeoutMs: this.options.requestTimeoutMs,
        payload: encodeDaAttestationsByHeaderRequestV1Cbor({
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
      const record = await this.recordFromAttestation({
        peer: registryEntry,
        attestation,
      });
      if (record !== undefined) {
        records.push(record);
      }
    }
    return records;
  }

  private async recordFromAttestation({
    peer,
    attestation,
  }: {
    readonly peer: DaPeerRegistryEntry;
    readonly attestation: DaAttestationGossipV1;
  }): Promise<DaSignatureRecord | undefined> {
    const headerHash = attestation.headerHash.toString("hex");
    if (
      attestation.deploymentFingerprint.toString("hex") !==
        this.options.deploymentFingerprint ||
      attestation.announcedByPeerId !== peer.peerId
    ) {
      return undefined;
    }
    const expectedDaVkey =
      this.options.committeeValidation.committeeKeys[attestation.signerIndex];
    if (
      expectedDaVkey === undefined ||
      expectedDaVkey !== attestation.daVkey.toString("hex")
    ) {
      return undefined;
    }
    const payload = await this.options.store.getDaPayload(headerHash);
    const header = await this.options.store.getStateQueueHeader(headerHash);
    if (!isVerifiedPayload(payload) || header === undefined) {
      return undefined;
    }
    if (payload.payloadSha256 !== attestation.payloadHash.toString("hex")) {
      return undefined;
    }
    const now = new Date().toISOString();
    const record: DaSignatureRecord = {
      deploymentFingerprint: this.options.deploymentFingerprint,
      headerHash,
      signerIndex: attestation.signerIndex,
      signatureWitness: attestation.onChainWitness.toString("hex"),
      payloadHash: payload.payloadSha256,
      committeeSignersHash:
        this.options.committeeValidation.committeeSignersHash,
      signedAt: now,
      broadcastStatus: "posted",
      source: "peer",
      sourcePeer: peer.peerId,
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
      deploymentFingerprint: this.options.deploymentFingerprint,
      signerValidation: this.options.committeeValidation,
      verifiedPayload: payload,
    });
    return validationError === undefined ? record : undefined;
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
        });
      },
    ],
  ]);
};

const isVerifiedPayload = (
  payload: DaPayloadRecord | undefined,
): payload is DaPayloadRecord =>
  payload !== undefined &&
  payload.validationStatus === "verified" &&
  payload.payloadSha256.length > 0;

const validationSummaryFromHeader = (
  header: StateQueueHeaderRecord,
  rootSummary: DaStoredPayloadRootSetV1,
): DaStoredValidationSummaryV1 => ({
  payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
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
): DaStoredPayloadRootSetV1 => ({
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
): DaStoredPayloadCountSetV1 => ({
  withdrawalCount: header.header.withdrawalCount,
  forcedTransactionCount: header.header.forcedTransactionCount,
  l2TransactionCount: header.header.l2TransactionCount,
  depositCount: header.header.depositCount,
  totalEventCount: header.header.totalEventCount,
  transitionStepCount: header.header.transitionStepCount,
  validationTraceCount: header.header.validationTraceCount,
});
