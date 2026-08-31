import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import type { DaAttestationCandidateRecord } from "../src/domain.js";
import { deriveExpectedDaAvailabilityCommitmentV1 } from "../src/peer/signatures.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { WatcherService } from "../src/watcher.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from "./helpers.js";

describe("multi-node DA committee integration", () => {
  it("uses a peer signature to init, add, and apply threshold DA attestation without HTTP DA transport", async () => {
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const dir = await tempDir();
    const coordinatorSeed = "00".repeat(31) + "01";
    const peerSeed = "00".repeat(31) + "02";
    const coordinatorSigner = await loadDaSigner(`hex:${coordinatorSeed}`);
    const peerSigner = await loadDaSigner(`hex:${peerSeed}`);
    const committeeHex =
      coordinatorSigner.publicKeyHex + peerSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const baseConfig = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: coordinatorSeed,
      signerPublicKey: coordinatorSigner.publicKeyHex,
    });
    const daParams = {
      committeeHex,
      committeeSignersHash,
      threshold: 2,
    };
    const coordinatorConfig = {
      ...baseConfig,
      daParams,
    };
    const peerConfig = {
      ...baseConfig,
      signerIndex: 1,
      signerKeySource: `hex:${peerSeed}`,
      daParams,
    };
    const coordinatorValidation = validateDaSignerMembership({
      daParams,
      signer: coordinatorSigner,
      signerIndex: 0,
    });
    const peerValidation = validateDaSignerMembership({
      daParams,
      signer: peerSigner,
      signerIndex: 1,
    });
    const coordinatorStore = await JsonFileWatcherStore.open(
      `${dir}/coordinator-store`,
    );
    const peerStore = await JsonFileWatcherStore.open(`${dir}/peer-store`);
    const observedHeaderProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, depth: 10 }),
      ],
    };
    const payloadSource = payloadSourceFromBytes(payloadCbor, "producer-peer");
    const peerService = new WatcherService({
      config: peerConfig,
      store: peerStore,
      stateQueueProvider: observedHeaderProvider,
      payloadSource,
      signer: peerSigner,
      signerValidation: peerValidation,
      coordinator: {
        publishSignature: async (record) => {
          await coordinatorStore.saveDaSignature({
            ...record,
            broadcastStatus: "posted",
            source: "peer",
            sourcePeer: "peer-node",
            receivedAt: new Date().toISOString(),
            verifiedAt: new Date().toISOString(),
          });
          return "posted";
        },
      },
    });
    await peerService.initialize();
    await expect(peerService.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    const storedPayload = await peerStore.getDaPayload(headerHash);
    const expectedCommitment = deriveExpectedDaAvailabilityCommitmentV1({
      authority: {
        deploymentIdentity: peerConfig.hubOraclePolicyId,
        bondOwnerCredential:
          peerConfig.availabilityChallenge.bondOwnerCredential,
        responseGeometry: peerConfig.availabilityChallenge.responseGeometry,
      },
      headerHash,
      payloadCborHex: storedPayload!.payloadCborHex,
    });
    await expect(
      coordinatorStore.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment.commitmentDigest,
        signerIndex: 1,
      }),
    ).resolves.toMatchObject({
      headerHash,
      signerIndex: 1,
      broadcastStatus: "posted",
    });

    const initialized = candidateRecord({
      headerHash,
      committeeSignersHash,
      attestationCount: 0,
      threshold: 2,
    });
    const thresholdCandidate = candidateRecord({
      headerHash,
      committeeSignersHash,
      attestationCount: 2,
      threshold: 2,
      status: "threshold",
      bitmap: "c0" + "00".repeat(31),
    });
    const candidateResponses = [[], [initialized], [thresholdCandidate]];
    const calls: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      peerSignaturesFor: (candidateHeaderHash) =>
        coordinatorStore.listDaSignatures(candidateHeaderHash),
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [thresholdCandidate],
      },
      recordCandidate: (record) =>
        coordinatorStore.saveDaAttestationCandidate(record),
      recordSubmission: (record) => coordinatorStore.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          calls.push("init");
          return submitted("initTx");
        },
        addSignatures: async ({ packedWitnessesHex, signerIndexes }) => {
          calls.push(
            `add:${signerIndexes.join(",")}:${packedWitnessesHex.slice(0, 2)}`,
          );
          return submitted("addTx");
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return submitted("applyTx");
        },
      },
    });
    const coordinatorService = new WatcherService({
      config: coordinatorConfig,
      store: coordinatorStore,
      stateQueueProvider: observedHeaderProvider,
      payloadSource,
      signer: coordinatorSigner,
      signerValidation: coordinatorValidation,
      coordinator,
    });
    await coordinatorService.initialize();
    await expect(coordinatorService.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });

    expect(calls).toEqual([
      "init",
      "add:0,1:00",
      `apply:${thresholdCandidate.outRef}`,
    ]);
    await expect(coordinatorStore.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
      { headerHash, txKind: "init", txHash: "initTx" },
    ]);
  });
});

const submitted = (txHash: string) => ({
  status: "submitted" as const,
  txHash,
});

const candidateRecord = ({
  headerHash,
  committeeSignersHash,
  attestationCount,
  threshold,
  status = "initialized",
  bitmap = "00".repeat(32),
}: {
  readonly headerHash: string;
  readonly committeeSignersHash: string;
  readonly attestationCount: number;
  readonly threshold: number;
  readonly status?: DaAttestationCandidateRecord["status"];
  readonly bitmap?: string;
}): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash,
  outRef: `${"ab".repeat(32)}#1`,
  datumCbor: "d87980",
  attestationCount,
  threshold,
  committeeSignersHash,
  bitmap,
  observedChainPoint: {},
  status,
});
