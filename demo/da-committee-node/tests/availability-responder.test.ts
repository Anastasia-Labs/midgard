import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  AvailabilityResponder,
  type AvailabilityResponderChallenge,
  type AvailabilityResponderDeps,
} from "../src/availability/responder.js";
import { retainedAvailabilityPayload } from "../src/availability/retained-payload.js";
import type { DaPayloadRecord } from "../src/domain.js";

const deploymentIdentity = "11".repeat(28);
const deploymentFingerprint = "22".repeat(32);
const payload = Uint8Array.from([1, 2, 3, 4]);
const commitment = SDK.buildDaAvailabilityCommitment({
  deploymentIdentity,
  headerHash: "33".repeat(28),
  bondOwner: "44".repeat(28),
  payload,
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  }),
});

const utxo = (outputIndex: number): UTxO => ({
  txHash: "66".repeat(32),
  outputIndex,
  address: "retained-challenge-address",
  assets: { lovelace: 100_000_000n },
});

const challengeFixture = (
  bytes: Uint8Array = payload,
): { challenge: AvailabilityResponderChallenge; stored: DaPayloadRecord } => {
  const frozen = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity,
    headerHash: commitment.header_hash,
    bondOwner: commitment.bond_owner,
    payload: bytes,
    responseGeometry: commitment.response_geometry,
  });
  const parameters = SDK.daAvailabilityParameters({
    responseGeometry: commitment.response_geometry,
    daBondLovelace: 12_000_000_000n,
    challengerBondLovelace: 12_000_000_000n,
    maxOpenFeeLovelace: 500_000n,
    maxPublicationFeeLovelace: 500_000n,
    maxSettlementFeeLovelace: 500_000n,
    maxCloseFeeLovelace: 1_000_000n,
    maxTimeoutFeeLovelace: 1_200_000n,
  });
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    availableBond: {
      Available: {
        commitment: frozen,
        da_bond_asset_name: SDK.daAvailabilityBondAssetName({
          transactionId: "77".repeat(32),
          outputIndex: 0n,
        }),
        committee_signers_hash: "88".repeat(32),
        attested_signers: "80" + "00".repeat(31),
      },
    },
    bondInputOutRef: { transactionId: "99".repeat(32), outputIndex: 0n },
    challenger: "aa".repeat(28),
    openedAt: 1_000n,
    parameters,
  });
  if (!("ChallengedBond" in plan.challengedBond))
    throw new Error("Expected challenged fixture bond");
  return {
    challenge: {
      bond: { utxo: utxo(0), datum: plan.challengedBond },
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

describe("availability responder lifecycle", () => {
  it("answers retained data when the public source withholds after attestation, then settles and closes", async () => {
    const fixture = challengeFixture();
    let challenge = fixture.challenge;
    const executed: string[] = [];
    const deps: AvailabilityResponderDeps = {
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover: async () => [challenge],
      reconcile: async () => "ready",
      now: () => 2_000,
      execute: async (action) => {
        executed.push(action.kind);
        if (action.kind === "publish") {
          challenge = {
            ...challenge,
            tranches: [
              {
                utxo: utxo(4),
                datum: SDK.advanceDaAvailabilityTranche({
                  active: action.tranche.datum,
                  publication: action.publication,
                  responseGeometry: commitment.response_geometry,
                  inclusiveValidityUpper: 3_000n,
                  carrierOutputIndex: 1n,
                }),
              },
            ],
          };
        } else if (action.kind === "settle") {
          challenge = {
            ...challenge,
            tranches: [],
            terminal: {
              ...challenge.terminal,
              datum: { ...challenge.terminal.datum, next_tranche_index: 1n },
            },
          };
        }
        return "confirmed";
      },
    };
    const responder = new AvailabilityResponder(deps);
    expect(await responder.tick()).toMatchObject({
      action: "publish",
      status: "confirmed",
    });
    expect(await responder.tick()).toMatchObject({
      action: "settle",
      status: "confirmed",
    });
    expect(await responder.tick()).toMatchObject({
      action: "close",
      status: "confirmed",
    });
    expect(executed).toEqual(["publish", "settle", "close"]);
  });

  it("resumes a partial answer from its chain offset after process restart", async () => {
    const bytes = new Uint8Array(5_000).fill(9);
    const fixture = challengeFixture(bytes);
    let challenge = fixture.challenge;
    const offsets: bigint[] = [];
    const deps: AvailabilityResponderDeps = {
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover: async () => [challenge],
      reconcile: async () => "ready",
      now: () => 2_000,
      execute: async (action) => {
        if (action.kind !== "publish") throw new Error("Expected publication");
        offsets.push(action.publication.chunk_offset);
        challenge = {
          ...challenge,
          tranches: [
            {
              utxo: utxo(4),
              datum: SDK.advanceDaAvailabilityTranche({
                active: action.tranche.datum,
                publication: action.publication,
                responseGeometry: commitment.response_geometry,
                inclusiveValidityUpper: 3_000n,
                carrierOutputIndex: 1n,
              }),
            },
          ],
        };
        return "confirmed";
      },
    };
    await new AvailabilityResponder(deps).tick();
    await new AvailabilityResponder(deps).tick();
    expect(offsets).toEqual([0n, 4_096n]);
  });

  it("reconciles an ambiguous transaction before reading payloads or constructing another action", async () => {
    const fixture = challengeFixture();
    const discover = vi.fn(async () => [fixture.challenge]);
    const getDaPayload = vi.fn(async () => fixture.stored);
    const execute = vi.fn(async () => "confirmed" as const);
    const responder = new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload },
      discover,
      execute,
      reconcile: async () => "pending",
    });
    expect(await responder.tick()).toMatchObject({ status: "pending" });
    expect(discover).not.toHaveBeenCalled();
    expect(getDaPayload).not.toHaveBeenCalled();
    expect(execute).not.toHaveBeenCalled();
  });

  it("leaves missing retained data visible and never fabricates a publication", async () => {
    const fixture = challengeFixture();
    const execute = vi.fn(async () => "confirmed" as const);
    const responder = new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => undefined },
      discover: async () => [fixture.challenge],
      execute,
      reconcile: async () => "ready",
      now: () => 2_000,
    });
    expect(await responder.tick()).toMatchObject({ status: "unavailable" });
    expect(execute).not.toHaveBeenCalled();
  });
});

const record: DaPayloadRecord = {
  deploymentFingerprint,
  headerHash: commitment.header_hash,
  payloadSchemaVersion: 1,
  payloadCborHex: Buffer.from(payload).toString("hex"),
  payloadSha256: computeDaSha256Hash(payload).toString("hex"),
  sourcePeerId: "retained-committee-peer",
  fetchedAt: "2026-09-08T00:00:00.000Z",
  validationStatus: "verified",
};

const retained = (stored: DaPayloadRecord | undefined) =>
  retainedAvailabilityPayload({
    store: { getDaPayload: async () => stored },
    deploymentFingerprint,
    deploymentIdentity,
    commitment,
  });

describe("retained availability responses", () => {
  it("publishes exact committed stored bytes without a current committee or bond-owner key", async () => {
    expect(await retained(record)).toEqual(Buffer.from(payload));
  });

  it("leaves absent payloads unavailable", async () => {
    expect(await retained(undefined)).toBeUndefined();
  });

  it("refuses corrupt bytes even when a record says verified", async () => {
    await expect(
      retained({ ...record, payloadCborHex: "01020305" }),
    ).rejects.toThrow(/digest/);
    const altered = Uint8Array.from([1, 2, 3, 5]);
    await expect(
      retained({
        ...record,
        payloadCborHex: Buffer.from(altered).toString("hex"),
        payloadSha256: computeDaSha256Hash(altered).toString("hex"),
      }),
    ).rejects.toThrow(/frozen signed commitment/);
  });

  it("refuses conflicted and foreign deployment records", async () => {
    await expect(
      retained({ ...record, conflictStatus: "conflicting_bytes" }),
    ).rejects.toThrow(/verified retained payload/);
    await expect(
      retained({ ...record, deploymentFingerprint: "55".repeat(32) }),
    ).rejects.toThrow(/this deployment/);
  });
});
