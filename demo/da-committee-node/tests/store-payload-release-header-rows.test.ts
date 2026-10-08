import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  type CommitteeConfig,
  LIBP2P_DA_MIN_RETENTION_DAYS,
} from "../src/config.js";
import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import type { PostgresCommitteeStore } from "../src/store/postgres.js";
import {
  retentionCycleOptions,
  runRetentionCycle,
} from "../src/store/retention.js";
import {
  openTestCommitteeStore,
  saveHealthyL1SourceState,
} from "./helpers/committee-store.js";
import {
  commitmentFor,
  signatureRecord,
} from "./peer-coordinator.signature-record.js";
import {
  FINGERPRINT,
  hashOf,
  HEAD,
  NOW,
  retentionOptions,
  seed,
} from "./store-retention.header-record.js";

/**
 * A released payload takes its header row and the rows still keyed by the
 * header (`releasePostgresHeaderRows`) only while no signed decision of the
 * member's own is stored, so "own signature implies header row" holds for
 * the promise-admission readers; and never under promise adoption, where
 * retirement is the only deleter of those rows.
 */

/** Long past any challengeability horizon: every status releases. */
const LONG_AGO = NOW - 60 * RETENTION_MS_PER_DAY;

const ADOPTION: NonNullable<CommitteeConfig["availabilityPromiseAdoption"]> = {
  policyArtifactPath: "/unused/policy.json",
  trustedPolicyDigest: "11".repeat(32),
  resourceProfilePath: "/unused/profile.json",
  trustedResourceProfileDigest: "22".repeat(32),
  calibrationEvidencePath: "/unused/calibration.json",
  trustedCalibrationEvidenceDigest: "33".repeat(32),
  faultModelPath: "/unused/fault-model.json",
  trustedFaultModelDigest: "44".repeat(32),
};

/** Signer 0 is the member (its own signed decision); signer 1 a peer. */
const signature = async (headerHash: string, signerIndex: 0 | 1) => {
  const signer = await loadDaSigner(
    `hex:${"00".repeat(31)}${(signerIndex + 1).toString(16).padStart(2, "0")}`,
  );
  const commitment = commitmentFor(headerHash);
  const record = signatureRecord({
    deploymentFingerprint: FINGERPRINT,
    headerHash,
    signerIndex,
    committeeSignersHash: "77".repeat(32),
    commitment,
    signatureWitness: signDaAttestation({
      signer,
      signerIndex,
      availabilityCommitment: commitment.commitment,
    }),
  });
  const local = signerIndex === 0;
  return {
    ...record,
    source: local ? "local" : "peer",
    broadcastStatus: local ? "local" : "posted",
    validation: {
      ...record.validation,
      l1Header: { ...record.validation.l1Header, endTime: String(LONG_AGO) },
    },
  } as const;
};

/** A merged header with a retained payload and the given signatures. */
const seeded = async (
  signers: readonly (0 | 1)[],
): Promise<{ store: PostgresCommitteeStore; headerHash: string }> => {
  const store = await saveHealthyL1SourceState(await openTestCommitteeStore());
  const headerHash = hashOf(40);
  await seed(store, [{ headerHash, endTimeMs: LONG_AGO, status: "merged" }]);
  for (const signerIndex of signers)
    await store.saveDaSignature(await signature(headerHash, signerIndex));
  return { store, headerHash };
};

const rows = async (store: PostgresCommitteeStore, headerHash: string) => ({
  payload: (await store.getDaPayload(headerHash)) !== undefined,
  header: (await store.getStateQueueHeader(headerHash)) !== undefined,
  signatures: (await store.listDaSignatures(headerHash)).map(
    ({ signerIndex }) => signerIndex,
  ),
  signed: (await store.listSignedDecisions()).some(
    (signed) => signed.headerHash === headerHash,
  ),
});

describe("deleteDaPayloadIfPrunable with releaseHeader", () => {
  it("keeps the header row and the decision while the member's own signature is stored", async () => {
    const { store, headerHash } = await seeded([0, 1]);
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...retentionOptions(),
        headerHash,
        releaseHeader: true,
      }),
    ).toBe(true);
    expect(await rows(store, headerHash)).toEqual({
      payload: false,
      header: true,
      signatures: [0, 1],
      signed: true,
    });
  });

  it("takes the header row and the peer rows when no own signature is stored", async () => {
    const { store, headerHash } = await seeded([1]);
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...retentionOptions(),
        headerHash,
        releaseHeader: true,
      }),
    ).toBe(true);
    expect(await rows(store, headerHash)).toEqual({
      payload: false,
      header: false,
      signatures: [],
      signed: false,
    });
  });
});

describe("the runtime retention cycle releasing a payload with only peer signatures", () => {
  const cycle = async (
    store: PostgresCommitteeStore,
    availabilityPromiseAdoption: CommitteeConfig["availabilityPromiseAdoption"],
  ) =>
    runRetentionCycle(
      store,
      retentionCycleOptions(
        {
          deploymentFingerprint: FINGERPRINT,
          automaticRecoveryMaxDepth: 2160,
          retentionAlertThresholdMs: undefined,
          availabilityPromiseAdoption,
          daTransport: { retentionDays: LIBP2P_DA_MIN_RETENTION_DAYS },
        },
        {
          confirmedHeadHash: HEAD,
          liveQueueHeaderHashes: new Set(),
          finalBlockTimeMs: NOW,
        },
        NOW,
      ),
    );

  it("keeps the header row and the peer rows under promise adoption", async () => {
    const { store, headerHash } = await seeded([1]);
    const { prune } = await cycle(store, ADOPTION);
    expect(prune.prunedHeaderHashes).toEqual([headerHash]);
    expect(await rows(store, headerHash)).toEqual({
      payload: false,
      header: true,
      signatures: [1],
      signed: false,
    });
  });

  it("takes them with the payload without promise adoption", async () => {
    const { store, headerHash } = await seeded([1]);
    const { prune } = await cycle(store, undefined);
    expect(prune.prunedHeaderHashes).toEqual([headerHash]);
    expect(await rows(store, headerHash)).toEqual({
      payload: false,
      header: false,
      signatures: [],
      signed: false,
    });
  });
});
