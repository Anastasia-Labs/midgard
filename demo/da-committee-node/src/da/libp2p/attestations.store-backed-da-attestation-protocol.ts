import {
  type DaAttestationGossip,
  decodeDaAttestationGossipCbor,
  decodeDaAttestationsByHeaderRequestCbor,
  encodeDaAttestationGossipCbor,
  encodeDaAttestationsByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";

import type { DaPayloadRecord, DaSignatureRecord } from "../../domain.js";
import {
  buildDaSignatureConflictEvidence,
  type DaAvailabilityCommitmentAuthority,
  deriveExpectedDaAvailabilityCommitment,
  validateDaSignatureRecord,
} from "../../peer/signatures.js";
import type { DaCommitteeValidation } from "../../signer.js";
import type { CommitteeStore } from "../../store.js";

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

export const isVerifiedPayload = (
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
