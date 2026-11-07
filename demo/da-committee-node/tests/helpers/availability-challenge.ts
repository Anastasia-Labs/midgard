import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { AvailabilityResponderChallenge } from "../../src/availability/responder.js";
import type { DaPayloadRecord } from "../../src/domain.js";

export const deploymentIdentity = "11".repeat(28);
export const deploymentFingerprint = "22".repeat(32);
export const payload = Uint8Array.from([1, 2, 3, 4]);
export const commitment = SDK.buildDaAvailabilityCommitment({
  deploymentIdentity,
  headerHash: "33".repeat(28),
  payload,
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  }),
});

export const utxo = (outputIndex: number): UTxO => ({
  txHash: "66".repeat(32),
  outputIndex,
  address: "retained-challenge-address",
  assets: { lovelace: 100_000_000n },
});

export const challengeFixture = (
  bytes: Uint8Array = payload,
): { challenge: AvailabilityResponderChallenge; stored: DaPayloadRecord } => {
  const frozen = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity,
    headerHash: commitment.header_hash,
    payload: bytes,
    responseGeometry: commitment.response_geometry,
  });
  const parameters = SDK.daAvailabilityParameters({
    responseGeometry: commitment.response_geometry,
    ...SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
    challengerBondLovelace: 12_000_000_000n,
    maxOpenFeeLovelace: 500_000n,
    maxPublicationFeeLovelace: 500_000n,
    maxSettlementFeeLovelace: 500_000n,
    maxCloseFeeLovelace: 1_000_000n,
    maxTimeoutFeeLovelace: 1_200_000n,
  });
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    commitment: frozen,
    challengerFundingOutRef: {
      transactionId: "99".repeat(32),
      outputIndex: 0n,
    },
    challenger: "aa".repeat(28),
    openedAt: 1_000n,
    parameters,
  });
  return {
    challenge: {
      record: { utxo: utxo(0), datum: plan.record },
      terminal: { utxo: utxo(1), datum: plan.terminalAccumulator },
      queue: utxo(2),
      tranches: plan.trancheThreads.map((datum, index) => ({
        utxo: utxo(index + 3),
        datum,
      })),
    },
    stored: {
      ...record,
      payloadCborHex: Buffer.from(bytes).toString("hex"),
      payloadSha256: computeDaSha256Hash(bytes).toString("hex"),
    },
  };
};

export const record: DaPayloadRecord = {
  deploymentFingerprint,
  headerHash: commitment.header_hash,
  payloadSchemaVersion: 1,
  payloadCborHex: Buffer.from(payload).toString("hex"),
  payloadSha256: computeDaSha256Hash(payload).toString("hex"),
  sourcePeerId: "retained-committee-peer",
  fetchedAt: "2026-09-08T00:00:00.000Z",
  validationStatus: "verified",
};
