import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import { l1SourceAuthorityDigest } from "../../src/config.js";
import type {
  DaPayloadRecord,
  DaSignatureRecordV1,
  StateQueueHeaderRecord,
} from "../../src/domain.js";
import type { CommitteeStore, L1SourceState } from "../../src/store.js";
import { fixtureHeaderBase } from "../helpers.js";
import {
  challengeFixture,
  commitment,
  deploymentFingerprint,
} from "./availability-challenge.js";

export const l1Source = {
  sourceMode: "local_node" as const,
  authorityNodeId: "node-a",
  chainSyncProviderUrl: "chain-sync:ogmios:ws://ogmios.local",
  queryProviderUrls: ["kupmios:http://kupo.local|ws://ogmios.local"],
};
export const network = "Preprod";
export const responderConfig = { network, l1Source };

/** The signed header, and a second decided header whose bytes diverged. */
export const signedHeader = commitment.header_hash;
export const divergentHeader = "44".repeat(28);

const headerRecord = (headerHash: string): StateQueueHeaderRecord => ({
  deploymentFingerprint,
  headerHash,
  stateQueueOutRef: `${"11".repeat(32)}#0`,
  blockAssetName: headerHash,
  header: {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    endTime: 1_000n,
  },
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: { finalized: true },
  finalized: true,
  status: "unattested",
  validationErrors: [],
  updatedAt: "2026-10-01T00:00:00.000Z",
});

export const sourceState = (
  status: "healthy" | "quarantined",
  headerHashes: readonly string[] = [signedHeader, divergentHeader],
): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network,
  authoritySha256: l1SourceAuthorityDigest(network, l1Source),
  status,
  observations: headerHashes.map((headerHash) => ({
    headerHash,
    stateQueueOutRef: `${"11".repeat(32)}#0`,
    stateQueueStatus: "unattested" as const,
    finalized: true,
    hasPersistedDecision: true,
  })),
  observedAt: "2026-10-01T00:00:00.000Z",
  ...(status === "quarantined"
    ? {
        quarantineReason: "decision_disappeared: fixture",
        quarantinedAt: "2026-10-01T00:00:01.000Z",
      }
    : {}),
});

export const verified = challengeFixture().stored;
export const divergentBytes = Uint8Array.from([9, 9, 9, 9]);
const divergent: DaPayloadRecord = {
  ...verified,
  headerHash: divergentHeader,
  payloadCborHex: Buffer.from(divergentBytes).toString("hex"),
  payloadSha256: computeDaSha256Hash(divergentBytes).toString("hex"),
  validationStatus: "conflicted",
  conflictStatus: "conflicting_bytes",
  validationError: "payload bytes conflict with an earlier sha256",
};

const signedCommitmentCbor = SDK.encodeDaAvailabilityCommitment(commitment);

/** This member's signature over `commitment`, as made before quarantine. */
export const localSignature = (signerIndex = 0): DaSignatureRecordV1 => ({
  deploymentFingerprint,
  headerHash: signedHeader,
  signerIndex,
  signatureWitness: "00" + "44".repeat(64),
  availabilityCommitmentCbor: signedCommitmentCbor,
  availabilityCommitmentDigest: computeDaSha256Hash(
    Buffer.from(signedCommitmentCbor, "hex"),
  ).toString("hex"),
  payloadHash: verified.payloadSha256,
  committeeSignersHash: "55".repeat(32),
  signedAt: "2026-10-01T00:00:00.500Z",
  broadcastStatus: "local",
  source: "local",
  l1ChainPoint: {
    slot: 1,
    blockHash: "66".repeat(32),
    blockHeight: 1,
    depth: 10,
    finalized: true,
    providerSource: "fixture",
  },
  validation: {
    payloadVersion: 1,
    rootsMatch: true,
    stateQueueOutRef: `${"11".repeat(32)}#0`,
    headerHash: signedHeader,
    rootSummary: {
      utxosRoot: "01".repeat(32),
      withdrawalsRoot: "02".repeat(32),
      forcedTransactionsRoot: "03".repeat(32),
      transactionsRoot: "04".repeat(32),
      depositsRoot: "05".repeat(32),
      transitionTraceRoot: "06".repeat(32),
      eventToStepRoot: "07".repeat(32),
      validationTracesRoot: "08".repeat(32),
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

/**
 * A store whose source quarantined over both decided headers, after this
 * member signed the first one.
 */
export const quarantinedStore = async (
  open: () => Promise<CommitteeStore>,
  signed: DaPayloadRecord = verified,
): Promise<CommitteeStore> => {
  const store = await open();
  await store.upsertStateQueueHeader(headerRecord(signedHeader));
  await store.upsertStateQueueHeader(headerRecord(divergentHeader));
  await store.saveDaPayload(signed);
  await store.saveDaPayload(divergent);
  await store.saveL1SourceState(sourceState("healthy"));
  await store.saveDaSignature(localSignature());
  await store.quarantineL1Decisions(sourceState("quarantined"));
  return store;
};
