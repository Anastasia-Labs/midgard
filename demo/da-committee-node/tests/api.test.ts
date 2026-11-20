import { AddressInfo } from "node:net";

import { describe, expect, it } from "vitest";

import { createWatcherApiServer } from "../src/api/server.js";
import { JsonFileWatcherStore } from "../src/store.js";
import type { WatcherReadinessSnapshot } from "../src/watcher.js";
import { tempDir } from "./helpers.js";

const readinessSnapshot = (
  ready: boolean,
  reasons: readonly string[] = [],
): WatcherReadinessSnapshot => ({
  ready,
  deployment: {
    configuredFingerprint: "dep",
    storeFingerprint: "dep",
    storeMatchesConfigured: true,
    manifestSha256: "aa".repeat(32),
    storeManifestSha256: "aa".repeat(32),
    contractDeploymentInfoSha256: "bb".repeat(32),
    storeContractDeploymentInfoSha256: "bb".repeat(32),
  },
  contracts: {
    stateQueuePolicyId: "policy-state",
    stateQueueAddress: "addr-state",
    daAttestationPolicyId: "policy-da",
    daAttestationAddress: "addr-da",
    daParamsGovernorPolicyId: "policy-gov",
    daParamsGovernorAddress: "addr-gov",
    committeeSignersHash: "cc".repeat(32),
    threshold: 1,
  },
  peer: {
    localPeerId: "watcher-peer",
    signerIndex: 0,
    producerPeerIds: ["producer-peer"],
    configuredPeerCount: 2,
    producerTargetCount: 1,
    localPeerIsProducer: false,
    l1SubmissionEnabled: false,
    l1SubmitterIds: [],
    l1SubmitterSignerIndexes: [],
    l1SubmitterPreflight: { status: "not_required" },
  },
  scanner: {
    status: ready ? "ok" : "failed",
    lastStartedAt: "2026-01-01T00:00:00.000Z",
    lastFinishedAt: "2026-01-01T00:00:01.000Z",
    scannedHeaders: 1,
    signedHeaders: 1,
    reconciledHeaders: 0,
    skippedHeaders: 0,
    errors: reasons,
  },
  counts: {
    discoveredHeaders: 1,
    missingPayloads: 0,
    verifiedPayloads: 1,
    verifiedPayloadsMissingL1Attestation: 0,
    signatures: 1,
    l1AttestationSubmissions: 0,
    submittedOrConfirmedL1Attestations: 0,
  },
  reasons,
});

describe("watcher API", () => {
  it("serves process health, readiness, and manifest only", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const api = createWatcherApiServer({
      deploymentFingerprint: "dep",
      signerIndex: 0,
      store,
      readiness: () => readinessSnapshot(true),
      manifest: { deployment: { fingerprint: "dep" } },
    });
    await api.listen(0, "127.0.0.1");
    try {
      const port = apiPort(api.address());
      const health = await fetch(`http://127.0.0.1:${port.toString()}/healthz`);
      expect(health.status).toBe(200);
      await expect(health.json()).resolves.toEqual({ ok: true });

      const ready = await fetch(`http://127.0.0.1:${port.toString()}/readyz`);
      expect(ready.status).toBe(200);
      await expect(ready.json()).resolves.toMatchObject({
        ready: true,
        deployment: {
          configuredFingerprint: "dep",
          storeFingerprint: "dep",
        },
        scanner: { status: "ok", scannedHeaders: 1 },
        counts: { discoveredHeaders: 1, verifiedPayloads: 1 },
        reasons: [],
      });

      const manifest = await fetch(
        `http://127.0.0.1:${port.toString()}/v1/manifest`,
      );
      expect(manifest.status).toBe(200);
      await expect(manifest.json()).resolves.toEqual({
        deployment: { fingerprint: "dep" },
      });
    } finally {
      await api.close();
    }
  });

  it("returns 503 readiness with reasons when the snapshot is not ready", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const api = createWatcherApiServer({
      deploymentFingerprint: "dep",
      signerIndex: 0,
      store,
      readiness: () =>
        readinessSnapshot(false, ["last state queue scanner tick failed"]),
    });
    await api.listen(0, "127.0.0.1");
    try {
      const port = apiPort(api.address());
      const ready = await fetch(`http://127.0.0.1:${port.toString()}/readyz`);
      expect(ready.status).toBe(503);
      await expect(ready.json()).resolves.toMatchObject({
        ready: false,
        scanner: { status: "failed" },
        reasons: ["last state queue scanner tick failed"],
      });
    } finally {
      await api.close();
    }
  });

  it("does not expose DA payload, status, or signature routes over HTTP", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const api = createWatcherApiServer({
      deploymentFingerprint: "dep",
      signerIndex: 0,
      store,
      readiness: () => readinessSnapshot(true),
    });
    await api.listen(0, "127.0.0.1");
    try {
      const port = apiPort(api.address());
      for (const resource of [
        "payload",
        "payload/metadata",
        "signature",
        "signatures",
        "status",
      ]) {
        const response = await fetch(
          `http://127.0.0.1:${port.toString()}/v1/deployments/dep/headers/${"01".repeat(
            28,
          )}/${resource}`,
        );
        expect(response.status, resource).toBe(404);
      }
    } finally {
      await api.close();
    }
  });
});

const apiPort = (address: AddressInfo | string | null): number =>
  typeof address === "object" && address !== null
    ? (address as AddressInfo).port
    : 0;
