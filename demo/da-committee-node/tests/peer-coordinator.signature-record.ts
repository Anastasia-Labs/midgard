import * as SDK from "@al-ft/midgard-sdk";

import {
  type DaAttestationExchange,
  StoreBackedDaAttestationProtocol,
} from "../src/da/libp2p/attestations.js";
import type {
  DaPayloadRecord,
  DaSignatureRecordV1,
  DaStoredPayloadRootSet,
  Header,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import {
  type DaAvailabilityCommitmentAuthority,
  deriveExpectedDaAvailabilityCommitment,
} from "../src/peer/signatures.js";
import { JsonFileCommitteeStore } from "../src/store.js";

export const availabilityCommitmentAuthority: DaAvailabilityCommitmentAuthority =
  {
    deploymentIdentity: "99".repeat(28),
    responseGeometry: {
      chunkByteLength: 4_096,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    },
  };

export const commitmentFor = (headerHash: string, payloadCborHex = "aabb") =>
  deriveExpectedDaAvailabilityCommitment({
    authority: availabilityCommitmentAuthority,
    headerHash,
    payloadCborHex,
  });

export const inMemoryExchange = ({
  senderPeerId,
  protocols,
}: {
  readonly senderPeerId: string;
  readonly protocols: ReadonlyMap<string, StoreBackedDaAttestationProtocol>;
}): DaAttestationExchange => ({
  publishAttestation: async ({ peer, record }) => {
    const protocol = protocols.get(peer.peerId);
    if (protocol === undefined) {
      return { status: "unavailable", reason: "peer is unavailable" };
    }
    return protocol.acceptAttestation({ record, sourcePeerId: senderPeerId });
  },
  attestationsByHeader: async ({ peer, deploymentFingerprint, headerHash }) => {
    const protocol = protocols.get(peer.peerId);
    if (protocol === undefined) {
      return [];
    }
    return protocol.attestationsByHeader({
      deploymentFingerprint,
      headerHash,
    });
  },
  publishConflictEvidence: async () => undefined,
});

export const seedByte = (value: number): string => {
  if (value < 0 || value > 255) {
    throw new Error("seed byte must be uint8");
  }
  return value.toString(16).padStart(2, "0");
};

export const signatureRecord = ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  committeeSignersHash,
  payloadHash = "22".repeat(32),
  signatureWitness,
  commitment,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly committeeSignersHash: string;
  readonly payloadHash?: string;
  readonly signatureWitness: string;
  readonly commitment: ReturnType<typeof commitmentFor>;
}): DaSignatureRecordV1 => ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  signatureWitness,
  availabilityCommitmentCbor: commitment.commitmentCbor,
  availabilityCommitmentDigest: commitment.commitmentDigest,
  payloadHash,
  committeeSignersHash,
  signedAt: new Date().toISOString(),
  broadcastStatus: "local",
  source: "local",
  l1ChainPoint: {},
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
    rootsMatch: true,
    stateQueueOutRef: "tx#0",
    headerHash,
    rootSummary: {
      utxosRoot: "44".repeat(32),
      transactionsRoot: "55".repeat(32),
      depositsRoot: "66".repeat(32),
      withdrawalsRoot: "77".repeat(32),
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    },
    countSummary: {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    },
    l1Header: {
      startTime: "1",
      endTime: "2",
      operatorVkey: "88".repeat(28),
      prevHeaderHash: "99".repeat(28),
      protocolVersion: "1",
    },
  },
});

export const saveVerifiedPayload = async (
  store: JsonFileCommitteeStore,
  {
    deploymentFingerprint,
    headerHash,
    payloadHash,
    payloadCbor,
    header,
  }: {
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
    readonly payloadHash: string;
    readonly payloadCbor: Buffer;
    readonly header: Header;
  },
): Promise<void> => {
  await store.upsertStateQueueHeader(
    stateQueueRecord({ deploymentFingerprint, headerHash, header }),
  );
  await store.saveDaPayload({
    deploymentFingerprint,
    headerHash,
    payloadSchemaVersion: 1,
    payloadCborHex: payloadCbor.toString("hex"),
    payloadSha256: payloadHash,
    sourcePeerId: "fixture",
    fetchedAt: new Date().toISOString(),
    verifiedAt: new Date().toISOString(),
    rootSummary: rootSummaryFromHeader(header),
    validationStatus: "verified",
    conflictStatus: "none",
  } satisfies DaPayloadRecord);
};

const stateQueueRecord = ({
  deploymentFingerprint,
  headerHash,
  header,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly header: Header;
}): StateQueueHeaderRecord => ({
  deploymentFingerprint,
  headerHash,
  stateQueueOutRef: "state-queue#0",
  blockAssetName: `block-${headerHash}`,
  header,
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: {
    slot: 1,
    blockHash: "aa".repeat(32),
    depth: 10,
    providerSource: "fixture",
  },
  finalized: true,
  status: "unattested",
  validationErrors: [],
  updatedAt: new Date().toISOString(),
});

const rootSummaryFromHeader = (header: Header): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});
