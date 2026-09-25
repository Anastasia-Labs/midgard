import { createHash } from "node:crypto";
import { mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { h32ForOrdinal } from "@al-ft/midgard-test-support/hex";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
  type DeploymentManifestIdentityContext,
} from "../src/commands/contract-deployment-info.js";
import { parseDaProducerPublicationManifest } from "../src/da/libp2p-producer.js";
import {
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "../src/da/libp2p-runtime-manifest.js";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestJsonValue,
} from "../src/deployment-manifest.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./helpers/availability-challenge.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./helpers/cardano-protocol-parameters.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";

const PRODUCER_KEY = `seed:${"00".repeat(31)}01`;
const COMMITTEE_KEY = `seed:${"00".repeat(31)}02`;
const PUBLIC_RETAINED_DA_KEY = `seed:${"00".repeat(31)}03`;
const DA_VKEY = "11".repeat(32);
const PRODUCER_DA_VKEY = "22".repeat(32);
const CARDANO_PARAMETERS = TEST_CARDANO_PROTOCOL_PARAMETERS;
const MANIFEST_IDENTITY_CONTEXT: DeploymentManifestIdentityContext = {
  availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  cardanoProtocolParameters: {
    snapshot: CARDANO_PARAMETERS,
    digest: computeDeploymentManifestJsonDigest(CARDANO_PARAMETERS),
  },
  genesis: {
    headerHash: "00".repeat(28),
    utxoSetDigest: computeDeploymentManifestJsonDigest(
      normalizeDeploymentManifestJsonValue([]),
    ),
  },
  da: {
    committeeVkeys: [DA_VKEY],
    committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash([
      DA_VKEY,
    ]),
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
      envelopeEncoding: "identity" as const,
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS,
      retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
    },
  },
  artifacts: {
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

describe("DA libp2p runtime manifest committee member endpoints", () => {
  const SECOND_COMMITTEE_KEY = `seed:${"00".repeat(31)}04`;
  const SECOND_DA_VKEY = "33".repeat(32);
  const twoMembers = (
    first: { readonly port: number; readonly host?: string } | undefined,
    second: { readonly port: number; readonly host?: string } | undefined,
  ) => [
    {
      signerIndex: 0,
      daVkey: DA_VKEY,
      libp2pPrivateKeySource: COMMITTEE_KEY,
      roles: ["committee", "coordinator", "retrieval"],
      ...(first === undefined ? {} : { endpoint: first }),
    },
    {
      signerIndex: 1,
      daVkey: SECOND_DA_VKEY,
      libp2pPrivateKeySource: SECOND_COMMITTEE_KEY,
      roles: ["committee", "retrieval"],
      ...(second === undefined ? {} : { endpoint: second }),
    },
  ];
  const generate = async (
    options: Partial<Parameters<typeof generateDaLibp2pRuntimeManifest>[0]>,
  ) => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    return generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 2,
      committeeMembers: twoMembers({ port: 39001 }, { port: 39011 }),
      ...options,
    });
  };
  const tcpPort = (address: string): string =>
    /\/tcp\/(\d+)\//u.exec(`${address}/`)![1]!;

  it("addresses co-hosted members at their own ports", async () => {
    const producer = await generate({});
    const members = producer.da_committee.members;
    expect(members.map(({ multiaddrs }) => tcpPort(multiaddrs[0]!))).toEqual([
      "39001",
      "39011",
    ]);
    expect(
      parseDaProducerPublicationManifest(producer, {
        DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_KEY,
      }).threshold,
    ).toBe(2);

    const second = await generate({ target: "committee", localSignerIndex: 1 });
    const transport = second.da_transport;
    expect(transport.listen_multiaddrs).toEqual(["/ip4/127.0.0.1/tcp/39011"]);
    expect(tcpPort(transport.announce_multiaddrs[0]!)).toBe("39011");
  });

  it("gives compose and public members their own hosts", async () => {
    const manifest = await generate({
      profile: "public",
      producerPublicHost: "producer.example",
      committeePublicHost: "da-0.example",
      committeeMembers: twoMembers(undefined, {
        host: "da-1.example",
        port: 39001,
      }),
    });
    const members = manifest.da_committee.members;
    expect(members[0]!.multiaddrs[0]).toContain(
      "/dns4/da-0.example/tcp/39001/",
    );
    expect(members[1]!.multiaddrs[0]).toContain(
      "/dns4/da-1.example/tcp/39001/",
    );
  });

  it("rejects members that would share one endpoint", async () => {
    await expect(
      generate({ committeeMembers: twoMembers(undefined, undefined) }),
    ).rejects.toThrow(
      "committee member 1 and committee member 0 must not share the libp2p endpoint 127.0.0.1:39001",
    );
    await expect(
      generate({
        committeeMembers: twoMembers({ port: 39001 }, { port: 39002 }),
      }),
    ).rejects.toThrow(
      "committee member 1 and producer must not share the libp2p endpoint 127.0.0.1:39002",
    );
    await expect(
      generate({
        committeeMembers: twoMembers({ port: 39001 }, { port: 39003 }),
      }),
    ).rejects.toThrow(/public retained DA must not share/);
  });

  it("rejects member hosts the profile fixes or cannot publish", async () => {
    await expect(
      generate({
        committeeMembers: twoMembers(
          { port: 39001 },
          { host: "10.0.0.2", port: 39011 },
        ),
      }),
    ).rejects.toThrow("committee member 1 host is fixed by the host profile");
    await expect(
      generate({
        profile: "public",
        producerPublicHost: "producer.example",
        committeePublicHost: "da-0.example",
        committeeMembers: twoMembers(undefined, {
          host: "127.0.0.1",
          port: 39011,
        }),
      }),
    ).rejects.toThrow(/committee member 1 public host must not be local-only/);
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
  const contracts = withRealEventHistoryForTest(
    await Effect.runPromise(
      AlwaysSucceedsContract.pipe(
        Effect.provide(AlwaysSucceedsContract.Default),
      ),
    ),
    { txHash: "ab".repeat(32), outputIndex: 0 },
  );
  const nativeScriptCbor = "820500";
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
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, index) => [
        contractName,
        {
          txHash: h32ForOrdinal(index + 1),
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
  const manifest = buildDeploymentManifest(
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
        availabilityRegistration: { status: "complete" },
      },
    },
  ) as unknown as Record<string, unknown>;
  mutate?.(manifest);
  delete manifest.manifestId;
  manifest.manifestId = computeDeploymentManifestId(
    manifest as unknown as Omit<DeploymentManifest, "manifestId">,
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
