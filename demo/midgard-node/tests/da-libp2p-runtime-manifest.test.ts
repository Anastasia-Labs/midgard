import { createHash } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifestV2,
} from "@/commands/contract-deployment-info.js";
import { parseDaProducerPublicationManifest } from "@/da/libp2p-producer.js";
import { generateDaLibp2pRuntimeManifest } from "@/da/libp2p-runtime-manifest.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

const PRODUCER_KEY = `seed:${"00".repeat(31)}01`;
const WATCHER_KEY = `seed:${"00".repeat(31)}02`;
const DA_VKEY = "11".repeat(32);
const PRODUCER_DA_VKEY = "22".repeat(32);

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
      producerPrivateKeySource: PRODUCER_KEY,
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
      "midgard-da-libp2p-runtime-manifest-v2",
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
      producerPrivateKeySource: PRODUCER_KEY,
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

  it("keeps producer retrieval peers on the producer port and out of producer bootstrap", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "producer-container-watcher-host",
      contractDeploymentInfoPath: deploymentInfo.path,
      producerPrivateKeySource: PRODUCER_KEY,
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
    const producerPeerId = (manifest.runtime_topology as Record<string, string>)
      .producer_peer_id;
    const daCommittee = manifest.da_committee as {
      members: Array<{ peer_id: string; multiaddrs: string[] }>;
    };
    const producerMember = daCommittee.members.find(
      (member) => member.peer_id === producerPeerId,
    );

    expect(producerMember?.multiaddrs).toEqual([
      expect.stringContaining("/ip4/127.0.0.1/tcp/39002/"),
    ]);
    expect(
      (
        (manifest.da_transport as Record<string, string[]>)
          .bootstrap_multiaddrs ?? []
      ).some((addr) => addr.endsWith(`/p2p/${producerPeerId}`)),
    ).toBe(false);
  });

  it("rejects local-only hosts in public profile", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    await expect(
      generateDaLibp2pRuntimeManifest({
        target: "producer",
        profile: "public",
        contractDeploymentInfoPath: deploymentInfo.path,
        producerPrivateKeySource: PRODUCER_KEY,
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
        producerPrivateKeySource: PRODUCER_KEY,
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
    ).rejects.toThrow(/steps\.initProtocol\.status=pending/);
  });

  it("rejects stale or inconsistent producer runtime manifest identities", async () => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    const manifest = await generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "host",
      contractDeploymentInfoPath: deploymentInfo.path,
      producerPrivateKeySource: PRODUCER_KEY,
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
          schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
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
  const referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo = {
    policyId: contracts.referenceScriptAuth.policyId,
    nativeScript: {
      type: "Native",
      cborHex: contracts.referenceScriptAuth.mintingScriptCBOR,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 0,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: { required: true, rule: "test fixture" },
  };
  const manifest = buildDeploymentManifestV2(
    buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
    ),
    {
      network: "Preprod",
      referenceScriptDeployAddress: "addr_test1reference",
      hubOracleOneShotTxHash: "ab".repeat(32),
      hubOracleOneShotOutputIndex: 0,
    },
  ) as unknown as Record<string, unknown>;
  const hubOracleOneShot = manifest.hubOracleOneShot as Record<string, unknown>;
  hubOracleOneShot.status = "consumed_by_init";
  const steps = manifest.steps as Record<string, Record<string, unknown>>;
  for (const step of [
    "prepareHubOracleNonce",
    "deployNodeRuntimeReferenceScripts",
    "initProtocol",
  ]) {
    steps[step] = { ...(steps[step] ?? {}), status: "complete" };
  }
  const referenceScripts = manifest.referenceScripts as Record<
    string,
    Record<string, unknown>
  >;
  let index = 0;
  for (const record of Object.values(referenceScripts)) {
    record.status = "confirmed";
    record.outRef ??= `${(index + 1).toString(16).padStart(2, "0").repeat(32)}#0`;
    index += 1;
  }
  mutate?.(manifest);
  const raw = `${JSON.stringify(manifest, null, 2)}\n`;
  const path = join(dir, "contract-deployment-info.json");
  await writeFile(path, raw, "utf8");
  return {
    path,
    manifestId: String(manifest.manifestId),
    sha256: createHash("sha256").update(raw).digest("hex"),
  };
};
