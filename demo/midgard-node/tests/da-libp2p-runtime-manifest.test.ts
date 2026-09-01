import { createHash } from "node:crypto";
import { mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifestV1,
  type DeploymentManifestV1IdentityContext,
} from "@/commands/contract-deployment-info.js";
import { parseDaProducerPublicationManifest } from "@/da/libp2p-producer.js";
import {
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "@/da/libp2p-runtime-manifest.js";
import {
  computeDeploymentManifestId,
  computeDeploymentManifestV1DaCommitteeSignersHash,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifestV1Value,
  normalizeDeploymentManifestV1JsonValue,
} from "@/deployment-manifest-v1.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";

const PRODUCER_KEY = `seed:${"00".repeat(31)}01`;
const WATCHER_KEY = `seed:${"00".repeat(31)}02`;
const PUBLIC_RETAINED_DA_KEY = `seed:${"00".repeat(31)}03`;
const SECOND_WATCHER_KEY = `seed:${"00".repeat(31)}04`;
const DA_VKEY = "11".repeat(32);
const PRODUCER_DA_VKEY = "22".repeat(32);
const CARDANO_PARAMETERS = normalizeDeploymentManifestV1JsonValue({
  maxTxSize: 16_384,
});
const MANIFEST_IDENTITY_CONTEXT: DeploymentManifestV1IdentityContext = {
  cardanoProtocolParameters: {
    snapshot: CARDANO_PARAMETERS,
    digest: computeDeploymentManifestV1JsonDigest(CARDANO_PARAMETERS),
  },
  genesis: {
    headerHash: "00".repeat(28),
    utxoSetDigest: computeDeploymentManifestV1JsonDigest(
      normalizeDeploymentManifestV1JsonValue([]),
    ),
  },
  da: {
    committeeVkeys: [DA_VKEY],
    committeeSignersHash: computeDeploymentManifestV1DaCommitteeSignersHash([
      DA_VKEY,
    ]),
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
      envelopeEncoding: "identity" as const,
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS_V1,
      retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    },
  },
  proofEvidence: {
    digest: null,
    blueprintHash: "33".repeat(32),
  },
};

const tempDirs: string[] = [];

afterEach(async () => {
  await Promise.all(tempDirs.map((dir) => rm(dir, { recursive: true })));
  tempDirs.length = 0;
});

describe("DA libp2p runtime manifest profiles", () => {
  it("emits host.docker.internal committee addresses for producer-container-to-host runs", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "producer-container-watcher-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: WATCHER_KEY,
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
      watcherServiceName: "watcher-a",
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: WATCHER_KEY,
          roles: ["committee"],
        },
      ],
    });

    expect(JSON.stringify(manifest)).toContain("/dns4/watcher-a/tcp/39001/");
  });

  it("assigns distinct ports to multiple host watchers", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const committeeMembers = [
      {
        signerIndex: 0,
        daVkey: DA_VKEY,
        libp2pPrivateKeySource: WATCHER_KEY,
        roles: ["committee", "retrieval"],
        watcherPort: 39001,
      },
      {
        signerIndex: 1,
        daVkey: PRODUCER_DA_VKEY,
        libp2pPrivateKeySource: SECOND_WATCHER_KEY,
        roles: ["committee", "retrieval"],
        watcherPort: 39004,
      },
    ];
    const producer = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "producer-container-watcher-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 2,
      committeeMembers,
    });
    const watcher = await generateDaLibp2pRuntimeManifest({
      target: "watcher",
      profile: "producer-container-watcher-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 2,
      localSignerIndex: 1,
      committeeMembers,
    });

    expect(producer.da_committee.members[0]?.multiaddrs[0]).toContain(
      "/tcp/39001/",
    );
    expect(producer.da_committee.members[1]?.multiaddrs[0]).toContain(
      "/tcp/39004/",
    );
    expect(watcher.da_transport.listen_multiaddrs).toEqual([
      "/ip4/0.0.0.0/tcp/39004",
    ]);
    expect(watcher.da_transport.announce_multiaddrs[0]).toContain(
      "/tcp/39004/",
    );
    expect(watcher.da_committee.members[0]?.multiaddrs).toEqual([
      expect.stringContaining("/ip4/127.0.0.1/tcp/39001/"),
      expect.stringContaining("/dns4/host.docker.internal/tcp/39001/"),
    ]);
    expect(watcher.da_committee.members[1]?.multiaddrs).toEqual([
      expect.stringContaining("/ip4/127.0.0.1/tcp/39004/"),
      expect.stringContaining("/dns4/host.docker.internal/tcp/39004/"),
    ]);
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
          libp2pPrivateKeySource: WATCHER_KEY,
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
      profile: "producer-container-watcher-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 1,
      committeeMembers: [
        {
          signerIndex: 0,
          daVkey: DA_VKEY,
          libp2pPrivateKeySource: WATCHER_KEY,
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
        watcherPublicHost: "da.example",
        threshold: 1,
        committeeMembers: [
          {
            signerIndex: 0,
            daVkey: DA_VKEY,
            libp2pPrivateKeySource: WATCHER_KEY,
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
            libp2pPrivateKeySource: WATCHER_KEY,
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
          libp2pPrivateKeySource: WATCHER_KEY,
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

const writeFinalizedDeploymentInfo = async (
  mutate?: (manifest: Record<string, unknown>) => void,
): Promise<{
  readonly path: string;
  readonly manifestId: string;
  readonly sha256: string;
}> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-da-runtime-manifest-"));
  tempDirs.push(dir);
  const contracts = await Effect.runPromise(
    AlwaysSucceedsContract.pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
  const nativeScriptCbor = `8200581c${"00".repeat(28)}`;
  const referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo = {
    policyId: validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    }),
    nativeScript: {
      type: "Native",
      cborHex: nativeScriptCbor,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 1,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: { required: true, rule: "test fixture" },
  };
  const referenceScriptOutRefs = new Map(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, index) => [
        contractName,
        {
          txHash: (index + 1).toString(16).padStart(2, "0").repeat(32),
          outputIndex: 0,
        },
      ],
    ),
  );
  const fraudProofCatalogue = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    ),
  );
  const manifest = buildDeploymentManifestV1(
    buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    ),
    {
      network: "Preprod",
      ...MANIFEST_IDENTITY_CONTEXT,
      referenceScriptDeployAddress: "addr_test1reference",
      hubOracleOneShotTxHash: "ab".repeat(32),
      hubOracleOneShotOutputIndex: 0,
      hubOracleOneShotStatus: "consumed_by_init",
      steps: {
        initProtocol: { status: "complete" },
      },
    },
  ) as unknown as Record<string, unknown>;
  mutate?.(manifest);
  delete manifest.manifestId;
  manifest.manifestId = computeDeploymentManifestId(
    manifest as unknown as Omit<DeploymentManifestV1Value, "manifestId">,
  );
  const raw = `${JSON.stringify(manifest, null, 2)}\n`;
  const path = join(dir, "contract-deployment-info.json");
  await writeFile(path, raw, "utf8");
  return {
    path,
    manifestId: String(manifest.manifestId),
    sha256: createHash("sha256").update(raw).digest("hex"),
  };
};
