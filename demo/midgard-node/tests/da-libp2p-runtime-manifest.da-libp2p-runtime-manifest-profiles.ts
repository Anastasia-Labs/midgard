import { mkdtemp, readdir, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { readFinalizedDeploymentIdentity } from "../src/commands/contract-deployment-info.js";
import { parseDaProducerPublicationManifest } from "../src/da/libp2p-producer.js";
import {
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "../src/da/libp2p-runtime-manifest.js";
import { prepareDaHardeningStartup } from "../src/da/startup.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
  type NodeConfigDep,
} from "../src/services/index.js";
import {
  COMMITTEE_KEY,
  DA_VKEY,
  PRODUCER_DA_VKEY,
  PRODUCER_KEY,
  PUBLIC_RETAINED_DA_KEY,
  tempDirs,
  writeFinalizedDeploymentInfo,
} from "./da-libp2p-runtime-manifest.write-finalized-deployment-info.js";

describe("DA libp2p runtime manifest profiles", () => {
  it("emits host.docker.internal committee addresses for producer-container-to-host runs", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "producer-container-committee-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: COMMITTEE_KEY,
          roles: ["committee", "retrieval"],
        },
      ],
    });

    expect(JSON.stringify(manifest)).toContain("host.docker.internal");
    expect(manifest.schemaVersion).toBe(
      "midgard-da-libp2p-runtime-manifest-v1",
    );
    expect(manifest.deployment).toEqual({
      fingerprint: deploymentInfo.manifestId,
      contract_deployment_manifest_id: deploymentInfo.manifestId,
      contract_deployment_info_sha256: deploymentInfo.sha256,
      identity_source: "contract_deployment_manifest_id",
    });
    const parsed = parseDaProducerPublicationManifest(manifest, {
      DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_KEY,
    });
    expect(parsed).toMatchObject({
      deploymentFingerprint: deploymentInfo.manifestId,
      contractDeploymentManifestId: deploymentInfo.manifestId,
      threshold: 1,
      committeePeers: [
        expect.objectContaining({
          signerIndex: 0,
          multiaddrs: [
            expect.stringContaining("/dns4/host.docker.internal/tcp/39001/"),
          ],
        }),
      ],
    });
  });

  it("emits compose service DNS addresses for compose profile", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "compose",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      committeeServiceName: "committee-a",
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: COMMITTEE_KEY,
          roles: ["committee"],
        },
      ],
    });

    expect(JSON.stringify(manifest)).toContain("/dns4/committee-a/tcp/39001/");
  });

  it.each(["producer", "committee"] as const)(
    "emits the verified 120-day retention for the %s runtime",
    async (target) => {
      const deploymentInfo = await writeFinalizedDeploymentInfo((manifest) => {
        const da = manifest.da as Record<string, unknown>;
        (da.transportProfile as Record<string, unknown>).retentionDays = 120;
      });
      const manifest = await generateDaLibp2pRuntimeManifest({
        target,
        profile: "host",
        contractDeploymentInfoPath: deploymentInfo.path,
        network: "Preprod",
        producerPrivateKeySource: PRODUCER_KEY,
        publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
        threshold: 1,
        ...(target === "committee" ? { localSignerIndex: 0 } : {}),
        committeeMembers: [
          {
            signerIndex: 0,
            daVkey: DA_VKEY,
            libp2pPrivateKeySource: COMMITTEE_KEY,
            roles: ["committee", "retrieval"],
          },
        ],
      });
      expect(manifest.da_transport.retention_days).toBe(120);
    },
  );

  it("refuses a shorter advertised DA retention before node startup", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo((manifest) => {
      const da = manifest.da as Record<string, unknown>;
      (da.transportProfile as Record<string, unknown>).retentionDays = 120;
    });
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: COMMITTEE_KEY,
          roles: ["committee", "retrieval"],
        },
      ],
    });
    const path = `${deploymentInfo.path}.runtime.json`;
    await writeFile(
      path,
      JSON.stringify({
        ...manifest,
        da_transport: { ...manifest.da_transport, retention_days: 119 },
      }),
    );
    const identity = readFinalizedDeploymentIdentity(deploymentInfo.path);
    vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", path);
    vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", PRODUCER_KEY);
    try {
      const startup = Effect.runPromise(
        prepareDaHardeningStartup.pipe(
          Effect.provideService(NodeConfig, {
            MIDGARD_DA_PAYLOAD_ENVELOPE: "identity",
          } as NodeConfigDep),
          Effect.provideService(
            ContractDeploymentIdentity,
            new ContractDeploymentIdentity({
              kind: "manifest",
              manifestId: identity.manifestId,
              deploymentMarker: makeDeploymentMarker(identity.manifestId),
              manifest: identity.manifest,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            }),
          ),
        ),
      );
      await expect(startup).rejects.toThrow();
    } finally {
      vi.unstubAllEnvs();
    }
  });

  it("persists the exact runtime manifest atomically", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: COMMITTEE_KEY,
          roles: ["committee"],
        },
      ],
    });
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-da-runtime-write-"),
    );
    tempDirs.push(directory);
    const path = join(directory, "runtime-manifest.json");

    await writeDaLibp2pRuntimeManifest(path, manifest);

    expect(await readFile(path, "utf8")).toBe(
      `${JSON.stringify(manifest, null, 2)}\n`,
    );
    expect(
      (await readdir(directory)).filter((entry) => entry.includes(".tmp-")),
    ).toEqual([]);
  });

  it("keeps producer retrieval peers on the producer port and out of producer bootstrap", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "producer-container-committee-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: COMMITTEE_KEY,
          roles: ["committee", "retrieval", "watcher"],
        },
        {
          signerIndex: 1,
          daVkey: PRODUCER_DA_VKEY,
          libp2pPrivateKeySource: PRODUCER_KEY,
          roles: ["producer", "retrieval"],
        },
      ],
    });
    const producerPeerId = manifest.runtime_topology.producer_peer_id;
    const daCommittee = manifest.da_committee;
    const producerMember = daCommittee.members.find(
      (member) => member.peer_id === producerPeerId,
    );

    expect(producerMember?.multiaddrs).toEqual([
      expect.stringContaining("/ip4/127.0.0.1/tcp/39002/"),
    ]);
    expect(
      manifest.da_transport.bootstrap_multiaddrs.some((addr) =>
        addr.endsWith(`/p2p/${producerPeerId}`),
      ),
    ).toBe(false);
  });

  it("rejects local-only hosts in public profile", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    await expect(
      generateDaLibp2pRuntimeManifest({
        target: "producer",
        profile: "public",
        contractDeploymentInfoPath: deploymentInfo.path,
        network: "Preprod",
        producerPrivateKeySource: PRODUCER_KEY,
        publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
        producerPublicHost: "127.0.0.1",
        committeePublicHost: "da.example",
        threshold: 1,
        committeeMembers: [
          {
            signerIndex: 0,
            daVkey: DA_VKEY,
            libp2pPrivateKeySource: COMMITTEE_KEY,
            roles: ["committee"],
          },
        ],
      }),
    ).rejects.toThrow(/public host/);
  });

  it("rejects incomplete contract deployment manifests", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo((manifest) => {
      const steps = manifest.steps as Record<string, Record<string, unknown>>;
      steps.initProtocol = { status: "pending" };
    });

    await expect(
      generateDaLibp2pRuntimeManifest({
        target: "producer",
        profile: "host",
        contractDeploymentInfoPath: deploymentInfo.path,
        network: "Preprod",
        producerPrivateKeySource: PRODUCER_KEY,
        publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
        threshold: 1,
        committeeMembers: [
          {
            signerIndex: 0,
            daVkey: DA_VKEY,
            libp2pPrivateKeySource: COMMITTEE_KEY,
            roles: ["committee"],
          },
        ],
      }),
    ).rejects.toThrow(/steps\.initProtocol\.status must be complete/);
  });

  it("rejects stale or inconsistent producer runtime manifest identities", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: COMMITTEE_KEY,
          roles: ["committee"],
        },
      ],
    });

    expect(() =>
      parseDaProducerPublicationManifest(
        {
          ...manifest,
          schemaVersion: "unsupported-da-runtime-manifest",
        },
        { DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_KEY },
      ),
    ).toThrow(/schemaVersion/);

    expect(() =>
      parseDaProducerPublicationManifest(
        {
          ...manifest,
          deployment: {
            ...(manifest.deployment as Record<string, unknown>),
            contract_deployment_manifest_id: "cd".repeat(32),
          },
        },
        { DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_KEY },
      ),
    ).toThrow(/fingerprint must equal/);

    const deploymentWithoutAuditSha = {
      ...(manifest.deployment as Record<string, unknown>),
    };
    delete deploymentWithoutAuditSha.contract_deployment_info_sha256;
    expect(() =>
      parseDaProducerPublicationManifest(
        {
          ...manifest,
          deployment: deploymentWithoutAuditSha,
        },
        { DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_KEY },
      ),
    ).toThrow(/contract_deployment_info_sha256/);
  });
});
