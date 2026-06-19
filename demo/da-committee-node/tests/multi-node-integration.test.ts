import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { createWatcherApiServer } from "../src/api/server.js";
import {
  HttpSignatureCoordinator,
} from "../src/coordinator/coordinator.js";
import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import { DaPayloadClient } from "../src/da/client.js";
import type { DaAttestationCandidateRecord } from "../src/domain.js";
import {
  loadDaSigner,
  validateDaSignerMembership,
} from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { WatcherService } from "../src/watcher.js";
import { bytesToHex } from "../src/utils/hex.js";
import {
  IDENTITY_TX_PROJECTOR,
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  tempDir,
} from "./helpers.js";

describe("multi-node DA committee integration", () => {
  it("accepts a peer signature over HTTP and uses it to init, add, and apply threshold DA attestation", async () => {
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const payloadServer = await startPayloadServer({ headerHash, payloadCbor });
    const payloadEndpoint = endpointFor(payloadServer);
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
      daPayloadEndpoints: [payloadEndpoint],
      daParams,
    };
    const peerConfig = {
      ...baseConfig,
      daPayloadEndpoints: [payloadEndpoint],
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
    const coordinatorApi = createWatcherApiServer({
      deploymentFingerprint: coordinatorConfig.deploymentFingerprint,
      signerIndex: 0,
      signerValidation: coordinatorValidation,
      store: coordinatorStore,
      ready: () => true,
    });
    await coordinatorApi.listen(0, "127.0.0.1");

    try {
      const signaturePostEndpoint = `${endpointFor(
        coordinatorApi,
      )}/v1/deployments/${
        coordinatorConfig.deploymentFingerprint
      }/headers/${headerHash}/signature`;
      const observedHeaderProvider = {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      };
      const peerService = new WatcherService({
        config: peerConfig,
        store: peerStore,
        stateQueueProvider: observedHeaderProvider,
        payloadClient: new DaPayloadClient({
          endpoints: peerConfig.daPayloadEndpoints,
        }),
        signer: peerSigner,
        signerValidation: peerValidation,
        coordinator: new HttpSignatureCoordinator(signaturePostEndpoint),
        transactionProjector: IDENTITY_TX_PROJECTOR,
      });
      await peerService.initialize();
      await expect(peerService.tick()).resolves.toMatchObject({
        scannedHeaders: 1,
        signedHeaders: 1,
        skippedHeaders: 0,
        errors: [],
      });
      await expect(
        coordinatorStore.getDaSignature({ headerHash, signerIndex: 1 }),
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
        recordSubmission: (record) =>
          coordinatorStore.saveL1Submission(record),
        submitter: {
          initAttestation: async () => {
            calls.push("init");
            return "initTx";
          },
          addSignatures: async ({ packedWitnessesHex, signerIndexes }) => {
            calls.push(
              `add:${signerIndexes.join(",")}:${packedWitnessesHex.slice(
                0,
                2,
              )}`,
            );
            return "addTx";
          },
          applyAttestation: async ({ candidate }) => {
            calls.push(`apply:${candidate.outRef}`);
            return "applyTx";
          },
        },
      });
      const coordinatorService = new WatcherService({
        config: coordinatorConfig,
        store: coordinatorStore,
        stateQueueProvider: observedHeaderProvider,
        payloadClient: new DaPayloadClient({
          endpoints: coordinatorConfig.daPayloadEndpoints,
        }),
        signer: coordinatorSigner,
        signerValidation: coordinatorValidation,
        coordinator,
        transactionProjector: IDENTITY_TX_PROJECTOR,
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
    } finally {
      await coordinatorApi.close();
      await closeServer(payloadServer);
    }
  });
});

const startPayloadServer = async ({
  headerHash,
  payloadCbor,
}: {
  readonly headerHash: string;
  readonly payloadCbor: Buffer;
}): Promise<Server> => {
  const server = createServer((request, response) => {
    const url = new URL(request.url ?? "/", "http://midgard-node.local");
    if (
      request.method === "GET" &&
      url.pathname === "/da/payload" &&
      url.searchParams.get("header_hash") === headerHash
    ) {
      response.writeHead(200, { "content-type": "application/cbor" });
      response.end(payloadCbor);
      return;
    }
    if (
      request.method === "GET" &&
      url.pathname === "/da/payload/metadata" &&
      url.searchParams.get("header_hash") === headerHash
    ) {
      response.writeHead(200, { "content-type": "application/json" });
      response.end(
        `${JSON.stringify({
          headerHash,
          payloadBytes: payloadCbor.length,
          status: "available",
        })}\n`,
      );
      return;
    }
    response.writeHead(404, { "content-type": "application/json" });
    response.end('{"error":"not found"}\n');
  });
  await new Promise<void>((resolve) => {
    server.listen(0, "127.0.0.1", resolve);
  });
  return server;
};

const endpointFor = (
  server: Pick<Server, "address"> | { readonly address: () => AddressInfo | string | null },
): string => {
  const address = server.address();
  const port =
    typeof address === "object" && address !== null
      ? address.port
      : 0;
  return `http://127.0.0.1:${port.toString()}`;
};

const closeServer = (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    server.close((error) => (error === undefined ? resolve() : reject(error)));
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
