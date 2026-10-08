import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { inspect } from "node:util";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as deploymentProfile from "@al-ft/midgard-core/deployment-profile";
import { loadRuntimeConfig } from "@al-ft/midgard-core/runtime-config";
import { describe, expect, it, vi } from "vitest";

import {
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
  l1SourceAuthorityDigest,
  LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  LIBP2P_DA_TRANSPORT_LIMITS,
  loadCommitteeConfig,
} from "../src/config.js";
import { parseMidgardNodeDeploymentInfo } from "../src/l1/deployment.js";
import { loadPublicRetainedDaRuntimeConfig } from "../src/public-retained-da-config.js";
import { retentionCycleOptions } from "../src/store/retention.js";
import {
  deploymentManifestIdFromFile,
  withRecomputedDeploymentManifestId,
  writeDaContractDeploymentFixture,
} from "./config.deployment-manifest-id-from-file.js";
import { tempDir } from "./helpers.js";
import {
  DEPLOYMENT_MANIFEST_ID,
  LIBP2P_PEER_ID_A,
  LIBP2P_PEER_ID_B,
  LIBP2P_PEER_ID_PUBLIC,
  LIBP2P_PRIVATE_KEY_SOURCE,
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
  writeMinimalDeploymentInfo,
} from "./helpers/committee-config-files.js";
import { readDaDeploymentFixture } from "./helpers/deployment-fixture.js";

describe("loadCommitteeConfig indexed signer sources", () => {
  const fakeSources = {
    DA_SIGNER_KEY_SOURCE_0: `hex:${"10".repeat(32)}`,
    DA_SIGNER_KEY_SOURCE_1: `hex:${"20".repeat(32)}`,
  };
  const fixture = async () => {
    const dir = await tempDir();
    const manifest = libp2pManifest("01".repeat(32));
    manifest.da_committee = {
      threshold: 1,
      members: [
        {
          signer_index: 0,
          da_vkey: "01".repeat(32),
          peer_id: LIBP2P_PEER_ID_A,
          multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`],
          roles: ["committee", "retrieval"],
        },
        {
          signer_index: 1,
          da_vkey: "02".repeat(32),
          peer_id: LIBP2P_PEER_ID_B,
          multiaddrs: [`/dns4/da-b.example/tcp/4001/p2p/${LIBP2P_PEER_ID_B}`],
          roles: ["committee", "retrieval"],
        },
      ],
    };
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );
    return { dir, env: libp2pConfigEnv(manifestPath, deploymentInfoPath) };
  };

  it.each([0, 1] as const)(
    "selects only signer %i from one indexed source map",
    async (index) => {
      const { env } = await fixture();
      const config = await loadCommitteeConfig({
        ...env,
        ...fakeSources,
        DA_SIGNER_INDEX: String(index),
      });
      expect(config.signerIndex).toBe(index);
      expect(config.signerKeySource).toBe(
        fakeSources[`DA_SIGNER_KEY_SOURCE_${index}`],
      );
      expect(config.signerKeySource).not.toBe(
        fakeSources[`DA_SIGNER_KEY_SOURCE_${index === 0 ? 1 : 0}`],
      );
      expect(config.daCommitteeMembers.map((member) => member.index)).toEqual([
        0, 1,
      ]);
    },
  );

  it("preserves explicit single-source selection", async () => {
    const { env } = await fixture();
    await expect(
      loadCommitteeConfig({
        ...env,
        DA_SIGNER_INDEX: "1",
        DA_SIGNER_KEY_SOURCE: fakeSources.DA_SIGNER_KEY_SOURCE_1,
      }),
    ).resolves.toMatchObject({
      signerIndex: 1,
      signerKeySource: fakeSources.DA_SIGNER_KEY_SOURCE_1,
    });
  });

  it("uses the process-selected index over the shared YAML default", async () => {
    const { dir, env: base } = await fixture();
    await writeFile(
      join(dir, "config.yaml"),
      [
        'DA_SIGNER_INDEX: "0"',
        `DA_SIGNER_KEY_SOURCE_0: "${fakeSources.DA_SIGNER_KEY_SOURCE_0}"`,
        `DA_SIGNER_KEY_SOURCE_1: "${fakeSources.DA_SIGNER_KEY_SOURCE_1}"`,
      ].join("\n"),
    );
    const env: NodeJS.ProcessEnv = { ...base, DA_SIGNER_INDEX: "1" };
    loadRuntimeConfig({ env, cwd: dir });
    expect(env.DA_SIGNER_INDEX).toBe("1");
    expect(env.DA_SIGNER_KEY_SOURCE_0).toBe(fakeSources.DA_SIGNER_KEY_SOURCE_0);
    expect(env.DA_SIGNER_KEY_SOURCE_1).toBe(fakeSources.DA_SIGNER_KEY_SOURCE_1);
    await expect(loadCommitteeConfig(env)).resolves.toMatchObject({
      signerIndex: 1,
      signerKeySource: fakeSources.DA_SIGNER_KEY_SOURCE_1,
    });
  });

  const malformedSources: readonly {
    readonly label: string;
    readonly overrides: Record<string, string | undefined>;
    readonly message: RegExp;
  }[] = [
    {
      label: "mixed single and indexed source",
      overrides: { DA_SIGNER_KEY_SOURCE: "fake-secret-not-for-errors" },
      message: /not both/u,
    },
    {
      label: "missing selected index",
      overrides: { DA_SIGNER_INDEX: undefined },
      message: /required to select an indexed signer/u,
    },
    {
      label: "absent selected source",
      overrides: { DA_SIGNER_INDEX: "2" },
      message: /No indexed DA signer source matches/u,
    },
    {
      label: "empty suffix",
      overrides: { DA_SIGNER_KEY_SOURCE_: "fake-secret-not-for-errors" },
      message: /canonical indices/u,
    },
    {
      label: "nonnumeric suffix",
      overrides: { DA_SIGNER_KEY_SOURCE_fake: "fake-secret-not-for-errors" },
      message: /canonical indices/u,
    },
    {
      label: "noncanonical padded suffix",
      overrides: { DA_SIGNER_KEY_SOURCE_01: "fake-secret-not-for-errors" },
      message: /canonical indices/u,
    },
    {
      label: "negative suffix",
      overrides: { "DA_SIGNER_KEY_SOURCE_-1": "fake-secret-not-for-errors" },
      message: /canonical indices/u,
    },
    {
      label: "fractional suffix",
      overrides: { "DA_SIGNER_KEY_SOURCE_1.0": "fake-secret-not-for-errors" },
      message: /canonical indices/u,
    },
    {
      label: "out-of-range suffix",
      overrides: { DA_SIGNER_KEY_SOURCE_256: "fake-secret-not-for-errors" },
      message: /canonical indices/u,
    },
    {
      label: "empty selected source",
      overrides: { DA_SIGNER_KEY_SOURCE_0: "" },
      message: /nonempty values/u,
    },
    {
      label: "blank unselected source",
      overrides: { DA_SIGNER_KEY_SOURCE_1: "  " },
      message: /nonempty values/u,
    },
    {
      label: "undefined unselected source",
      overrides: { DA_SIGNER_KEY_SOURCE_1: undefined },
      message: /nonempty values/u,
    },
    {
      label: "out-of-range selected index",
      overrides: { DA_SIGNER_INDEX: "256" },
      message: /must fit in one byte/u,
    },
    {
      label: "invalid selected index",
      overrides: { DA_SIGNER_INDEX: "fake-secret-not-for-errors" },
      message: /must be a non-negative integer/u,
    },
  ];
  it.each(malformedSources)(
    "rejects $label without disclosing any key source",
    async ({ overrides, message }) => {
      const { env } = await fixture();
      let failure: unknown;
      try {
        await loadCommitteeConfig({
          ...env,
          ...fakeSources,
          DA_SIGNER_INDEX: "0",
          ...overrides,
        });
      } catch (error) {
        failure = error;
      }
      expect(failure).toBeInstanceOf(Error);
      expect(failure).toMatchObject({
        message: expect.stringMatching(message),
      });
      const diagnostic = inspect(failure, { depth: 10 });
      expect(diagnostic).not.toContain("fake-secret-not-for-errors");
      expect(diagnostic).not.toContain(fakeSources.DA_SIGNER_KEY_SOURCE_0);
      expect(diagnostic).not.toContain(fakeSources.DA_SIGNER_KEY_SOURCE_1);
    },
  );
});

describe("loadCommitteeConfig", () => {
  it("parses only the exact V1 manifest and consensus-profile pairing", async () => {
    const dir = await tempDir();
    const manifest = libp2pManifest("01".repeat(32));
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );
    const canonicalManifest: Record<string, unknown> = {
      ...(await readDaDeploymentFixture()),
      manifestId: DEPLOYMENT_MANIFEST_ID,
    };
    const canonicalContracts = canonicalManifest.contracts as Record<
      string,
      Record<string, unknown>
    >;
    const canonicalCategories = (
      canonicalContracts.fraudProofCatalogueMint!.fraudProofCatalogue as Record<
        string,
        Record<string, unknown>
      >
    ).categories!;
    expect(canonicalCategories).toMatchObject({
      zeroInput: {
        categoryId: "00000005",
        scriptHash: canonicalContracts.fraudProofZeroInput!.scriptHash,
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: canonicalContracts.validationTraceDispute!.scriptHash,
      },
    });
    await writeFile(deploymentInfoPath, JSON.stringify(canonicalManifest));
    await expect(
      loadCommitteeConfig(libp2pConfigEnv(manifestPath, deploymentInfoPath)),
    ).resolves.toMatchObject({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });

    await writeFile(
      deploymentInfoPath,
      JSON.stringify({
        ...canonicalManifest,
        schemaVersion: "unsupported-deployment-manifest",
      }),
    );
    await expect(
      loadCommitteeConfig(libp2pConfigEnv(manifestPath, deploymentInfoPath)),
    ).rejects.toThrow(/schemaVersion must be/u);
  });

  it("loads deployment files and DA params from the manifest", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifest = libp2pManifest(member);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    await writeFile(manifestPath, JSON.stringify(manifest));
    await writeMinimalDeploymentInfo(deploymentInfoPath);
    const config = await loadCommitteeConfig({
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    });
    expect(config.network).toBe("Preprod");
    expect(config.daTransport.kind).toBe("libp2p");
    expect(config.daParams.committeeHex).toBe(member);
    expect(config.daParams.threshold).toBe(1);
  });

  it("binds committee finality to the verified deployment release depth", async () => {
    const dir = await tempDir();
    const manifest = libp2pManifest("01".repeat(32));
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );
    const base = libp2pConfigEnv(manifestPath, deploymentInfoPath);

    await expect(loadCommitteeConfig(base)).resolves.toMatchObject({
      finalityDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
    });
    await expect(
      loadCommitteeConfig({ ...base, CARDANO_FINALITY_DEPTH: "29" }),
    ).rejects.toThrow(
      /must exactly equal the verified deployment manifest l1Finality\.confirmationDepth/u,
    );
  });

  it("leaves the retention deadline alert off unless the operator sets a threshold", async () => {
    const dir = await tempDir();
    const manifest = libp2pManifest("01".repeat(32));
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );
    const base = libp2pConfigEnv(manifestPath, deploymentInfoPath);

    const view = {
      confirmedHeadHash: "aa".repeat(28),
      liveQueueHeaderHashes: new Set<string>(),
    };
    const unset = await loadCommitteeConfig(base);
    expect(unset.retentionAlertThresholdMs).toBeUndefined();
    // The runtime cycle is built from the loaded config: no threshold, no alert.
    expect(retentionCycleOptions(unset, view, 1).alertThresholdMs).toBe(
      undefined,
    );
    const blank = await loadCommitteeConfig({
      ...base,
      DA_RETENTION_ALERT_THRESHOLD_MS: " ",
    });
    expect(blank.retentionAlertThresholdMs).toBeUndefined();
    // A header merges no earlier than block maturity after its end time, so a
    // merged payload has at most horizon - maturity left: the largest useful
    // threshold is one below that. At it, or anywhere up to the horizon, every
    // merged payload would alert from the moment it merges.
    const mergedWindowMs =
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs -
      MIDGARD_RETENTION_WINDOW.maturityMs;
    const set = await loadCommitteeConfig({
      ...base,
      DA_RETENTION_ALERT_THRESHOLD_MS: (mergedWindowMs - 1).toString(),
    });
    expect(set.retentionAlertThresholdMs).toBe(mergedWindowMs - 1);
    expect(retentionCycleOptions(set, view, 1)).toMatchObject({
      nowMs: 1,
      alertThresholdMs: mergedWindowMs - 1,
      retentionDays: set.daTransport.retentionDays,
      deploymentFingerprint: set.deploymentFingerprint,
      minimumFinalityDepth: set.finalityDepth,
      confirmedHeadHash: view.confirmedHeadHash,
    });
    for (const refused of [
      mergedWindowMs,
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs - 1,
    ]) {
      await expect(
        loadCommitteeConfig({
          ...base,
          DA_RETENTION_ALERT_THRESHOLD_MS: refused.toString(),
        }),
      ).rejects.toThrow(
        /DA_RETENTION_ALERT_THRESHOLD_MS=\d+ must be below the merged-payload window/u,
      );
    }
    for (const bad of ["-1", "1.5", "soon"]) {
      await expect(
        loadCommitteeConfig({
          ...base,
          DA_RETENTION_ALERT_THRESHOLD_MS: bad,
        }),
      ).rejects.toThrow(/DA_RETENTION_ALERT_THRESHOLD_MS/u);
    }
  });

  it("parses libp2p DA transport manifests without HTTP endpoint config", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifest = libp2pManifest(member);
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );

    const config = await loadCommitteeConfig(
      libp2pConfigEnv(manifestPath, deploymentInfoPath),
    );

    expect(config.network).toBe("Preprod");
    expect(config.deploymentFingerprint).toBe(DEPLOYMENT_MANIFEST_ID);
    expect(config.libp2pPrivateKeySource).toBe(LIBP2P_PRIVATE_KEY_SOURCE);
    expect(config.l1SubmitterSignerIndexes).toEqual([]);
    expect(config.daCommitteeMembers).toEqual([
      { index: 0, vkey: member, canSubmitL1: false },
    ]);
    expect(config.daTransport.kind).toBe("libp2p");
    if (config.daTransport.kind !== "libp2p") {
      throw new Error("expected libp2p DA transport config");
    }
    expect(config.daTransport).toMatchObject({
      deploymentFingerprint: DEPLOYMENT_MANIFEST_ID,
      noHttpDaTransport: true,
      threshold: 1,
      listenMultiaddrs: ["/ip4/0.0.0.0/tcp/0"],
      announceMultiaddrs: [
        `/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`,
      ],
      bootstrapMultiaddrs: [
        `/dns4/bootstrap.example/tcp/4001/p2p/${LIBP2P_PEER_ID_B}`,
      ],
      gossip: {
        strictSign: true,
        emitSelf: false,
        allowedTopicsOnly: true,
        maxGossipMessageBytes: LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
      },
      limits: LIBP2P_DA_TRANSPORT_LIMITS,
      retentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
      peers: [
        {
          signerIndex: 0,
          daVkey: member,
          peerId: LIBP2P_PEER_ID_A,
          multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`],
          roles: ["committee", "retrieval"],
        },
      ],
    });
  });

  it("loads the manifest-bound public retained-DA process with only its read-only authority", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const publicProcessEnv = {
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      DA_PUBLIC_RETAINED_DA_ENABLED: "true",
      DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE: `seed:${"03".repeat(32)}`,
      DA_PUBLIC_RETAINED_DA_DATABASE_URL:
        "postgresql://public_reader@localhost/midgard",
      DA_PUBLIC_RETAINED_DA_DATABASE_ROLE: "public_reader",
    };
    const enabled = await loadPublicRetainedDaRuntimeConfig(publicProcessEnv);
    expect(enabled.publicRetainedDa).toMatchObject({
      peerId: LIBP2P_PEER_ID_PUBLIC,
      listenMultiaddrs: ["/ip4/0.0.0.0/tcp/0"],
      announceMultiaddrs: [
        `/dns4/public-da.example/tcp/4002/p2p/${LIBP2P_PEER_ID_PUBLIC}`,
      ],
      protocols: [
        "capabilities",
        "payload-by-header",
        "payload-chunk",
        "metadata-by-header",
        "proof-bundle-by-header",
        "trace-step-by-index",
        "event-to-step-by-event",
      ],
    });
    expect(enabled.databaseRole).toBe("public_reader");

    const {
      DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE: _publicPrivateKey,
      ...missingPublicKeyEnv
    } = publicProcessEnv;
    await expect(
      loadPublicRetainedDaRuntimeConfig(missingPublicKeyEnv),
    ).rejects.toThrow(/DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE/);
    await expect(
      loadCommitteeConfig({
        ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
        DA_PUBLIC_RETAINED_DA_ENABLED: "true",
        DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE: `seed:${"03".repeat(32)}`,
      }),
    ).rejects.toThrow(/dedicated midgard-public-retained-da process/u);
  });

  it("allows contract deployment info raw SHA drift without changing identity", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifest = libp2pManifest(member);
    (
      manifest.deployment as Record<string, unknown>
    ).contract_deployment_info_sha256 = "ef".repeat(32);
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );

    const config = await loadCommitteeConfig(
      libp2pConfigEnv(manifestPath, deploymentInfoPath),
    );

    expect(config.deploymentFingerprint).toBe(DEPLOYMENT_MANIFEST_ID);
  });

  it("rejects DA URL environment overrides in libp2p mode", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const baseEnv = libp2pConfigEnv(manifestPath, deploymentInfoPath);
    for (const [name, value] of Object.entries({
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      DA_PEER_ENDPOINTS: "0@http://da-1.example",
      DA_COORDINATOR_ENDPOINT: "http://coordinator.example",
      DA_PUBLIC_BASE_URL: "http://da-self.example",
    })) {
      await expect(
        loadCommitteeConfig({ ...baseEnv, [name]: value }),
      ).rejects.toThrow(new RegExp(name));
    }
  });

  it("requires a persistent libp2p private key source in libp2p mode", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const baseEnv = libp2pConfigEnv(manifestPath, deploymentInfoPath);
    const missingKeyEnv: Record<string, string> = { ...baseEnv };
    delete missingKeyEnv.DA_LIBP2P_PRIVATE_KEY_SOURCE;

    await expect(loadCommitteeConfig(missingKeyEnv)).rejects.toThrow(
      /DA_LIBP2P_PRIVATE_KEY_SOURCE/,
    );
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_LIBP2P_PRIVATE_KEY_SOURCE: "seed:abcd",
      }),
    ).rejects.toThrow(/DA_LIBP2P_PRIVATE_KEY_SOURCE seed/);
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_LIBP2P_PRIVATE_KEY_SOURCE: "private-key:ed25519_sk_test",
      }),
    ).rejects.toThrow(/seed:, hex:, or file:/);
  });

  it("rejects URL-shaped DA manifest fields and values in libp2p mode", async () => {
    await expectLibp2pManifestRejects((manifest) => {
      (manifest.da_transport as Record<string, unknown>).baseUrl =
        "http://da-0.example";
    }, /baseUrl/);
    await expectLibp2pManifestRejects((manifest) => {
      (
        (manifest.da_committee as Record<string, unknown>).members as Record<
          string,
          unknown
        >[]
      )[0]!.peer_id = "https://da-0.example/peer";
    }, /HTTP\(S\) URL/);
  });

  it("rejects missing or unknown runtime-manifest root and nested keys", async () => {
    const cases: readonly {
      readonly mutate: (manifest: Record<string, unknown>) => void;
      readonly error: RegExp;
    }[] = [
      {
        mutate: (manifest) => {
          delete manifest.network;
        },
        error: /network is required/u,
      },
      {
        mutate: (manifest) => {
          manifest.unknown_root = true;
        },
        error: /unknown_root is unexpected/u,
      },
      {
        mutate: (manifest) => {
          const gossip = (
            manifest.da_transport as Record<string, Record<string, unknown>>
          ).gossip;
          gossip.unknown_nested = true;
        },
        error: /gossip\.unknown_nested is unexpected/u,
      },
      {
        mutate: (manifest) => {
          const committee = manifest.da_committee as {
            members: Record<string, unknown>[];
          };
          delete committee.members[0]!.roles;
        },
        error: /members\[0\]\.roles is required/u,
      },
    ];

    for (const testCase of cases) {
      await expectLibp2pManifestRejects(testCase.mutate, testCase.error);
    }
  });

  it("binds runtime-manifest network to deployment identity and operator config", async () => {
    await expectLibp2pManifestRejects((manifest) => {
      manifest.network = "Preview";
    }, /must exactly match contract deployment manifest network/u);

    await expectLibp2pManifestRejects(
      () => undefined,
      /MIDGARD_NETWORK must exactly match runtime manifest network Preprod/u,
      { MIDGARD_NETWORK: "Preview" },
    );
  });

  it("requires and binds an explicit network magic for Custom local-node authority", async () => {
    const localProfile =
      deploymentProfile.DEPLOYMENT_PROFILES["local-devnet-testing"];
    const localDigest =
      deploymentProfile.DEPLOYMENT_PROFILE_DIGESTS["local-devnet-testing"];
    const binding = vi
      .spyOn(deploymentProfile, "verifyDeploymentProfileBinding")
      .mockImplementation((profile, digest, network) => {
        expect(profile).toEqual(localProfile);
        expect(digest).toBe(localDigest);
        expect(network).toBe("Custom");
      });
    try {
      const dir = await tempDir();
      const deployment = await readDaDeploymentFixture();
      const customDeployment = withRecomputedDeploymentManifestId({
        ...deployment,
        network: "Custom",
        deploymentProfile: localProfile,
        deploymentProfileDigest: localDigest,
      });
      const manifest = libp2pManifest(
        "01".repeat(32),
        ["committee", "retrieval"],
        String(customDeployment.manifestId),
      );
      manifest.network = "Custom";
      const manifestPath = join(dir, "manifest.json");
      const deploymentInfoPath = join(dir, "deployment.json");
      await writeFile(manifestPath, JSON.stringify(manifest));
      await writeFile(deploymentInfoPath, JSON.stringify(customDeployment));
      const baseEnv = libp2pConfigEnv(manifestPath, deploymentInfoPath);

      await expect(loadCommitteeConfig(baseEnv)).rejects.toThrow(
        /CARDANO_NETWORK_MAGIC is required for Custom/,
      );
      for (const invalid of ["-1", "01", "1.5", "4294967296"]) {
        await expect(
          loadCommitteeConfig({ ...baseEnv, CARDANO_NETWORK_MAGIC: invalid }),
        ).rejects.toThrow(/CARDANO_NETWORK_MAGIC/);
      }

      const config = await loadCommitteeConfig({
        ...baseEnv,
        CARDANO_NETWORK_MAGIC: "424242",
      });
      expect(config.cardanoL1Source).toEqual({ networkMagic: 424242 });
    } finally {
      binding.mockRestore();
    }
  });

  it("rejects explicit network magic for named networks", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    await expect(
      loadCommitteeConfig({
        ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
        CARDANO_NETWORK_MAGIC: "2",
      }),
    ).rejects.toThrow(/must be omitted for named Cardano networks/);
  });

  it("loads with no chain index configured: no Kupo, Ogmios or provider-URL key", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const env = libp2pConfigEnv(manifestPath, deploymentInfoPath);
    expect(
      Object.entries(env).filter(([name, value]) =>
        /kupo|ogmios|kupmios|PROVIDER_URL|L1_SOURCE_MODE|CHAIN_SYNC_URL/iu.test(
          `${name}=${value}`,
        ),
      ),
    ).toEqual([]);
    const config = await loadCommitteeConfig(env);
    expect(config.cardanoL1Source).toEqual({ networkMagic: 1 });
    expect(
      Object.keys(config).filter((key) =>
        /provider|^l1Source$|kupo|ogmios/iu.test(key),
      ),
    ).toEqual([]);
  });

  it("binds the L1 source authority digest to the network, node authority and L1 origin", () => {
    const base = {
      network: "Preprod",
      nativeLedger: {
        authorityNodeId: "preview-node-a",
        socketPath: "/run/cardano/node.socket",
        nodeConfigPath: "/etc/cardano/config.json",
        binaryPath: "/usr/local/bin/midgard-l1-node-transport",
      },
      l1Origin: { slot: 7, blockHash: "ab".repeat(32) },
    };
    const baseline = l1SourceAuthorityDigest(base);
    expect(baseline).toMatch(/^[0-9a-f]{64}$/u);
    expect(l1SourceAuthorityDigest({ ...base, network: "Preview" })).not.toBe(
      baseline,
    );
    expect(
      l1SourceAuthorityDigest({
        ...base,
        nativeLedger: { ...base.nativeLedger, authorityNodeId: "node-b" },
      }),
    ).not.toBe(baseline);
    expect(
      l1SourceAuthorityDigest({
        ...base,
        l1Origin: { slot: 7, blockHash: "cd".repeat(32) },
      }),
    ).not.toBe(baseline);
    // Paths are not identity: moving the socket keeps the binding.
    expect(
      l1SourceAuthorityDigest({
        ...base,
        nativeLedger: { ...base.nativeLedger, socketPath: "/tmp/node.socket" },
      }),
    ).toBe(baseline);
  });

  // Eleven separate on-disk manifest/load cycles exceeded five seconds in
  // both full-suite runs under contention; each exact rejection stays checked.
  it("fails closed for invalid libp2p DA manifest security fields", async () => {
    const cases: readonly {
      readonly mutate: (manifest: Record<string, unknown>) => void;
      readonly env?: Record<string, string>;
      readonly error: RegExp;
    }[] = [
      {
        mutate: (manifest) => {
          manifest.schemaVersion = "unsupported-da-runtime-manifest";
        },
        error: /schemaVersion/,
      },
      {
        mutate: (manifest) => {
          (manifest.deployment as Record<string, unknown>).identity_source =
            "contract_deployment_info_sha256";
        },
        error: /identity_source/,
      },
      {
        mutate: (manifest) => {
          (
            manifest.deployment as Record<string, unknown>
          ).contract_deployment_manifest_id = "cd".repeat(32);
        },
        error:
          /fingerprint must equal deployment\.contract_deployment_manifest_id/,
      },
      {
        mutate: (manifest) => {
          delete (manifest.deployment as Record<string, unknown>)
            .contract_deployment_info_sha256;
        },
        error: /contract_deployment_info_sha256/,
      },
      {
        mutate: (manifest) => {
          (manifest.deployment as Record<string, unknown>).fingerprint = "ab";
        },
        error: /deployment[._ ]fingerprint/,
      },
      {
        mutate: (manifest) => {
          (
            manifest.da_transport as Record<string, unknown>
          ).no_http_da_transport = false;
        },
        error: /no_http_da_transport/,
      },
      {
        mutate: (manifest) => {
          (manifest.da_transport as Record<string, unknown>).retention_days =
            14;
        },
        error: /retention_days.*15/u,
      },
      {
        mutate: (manifest) => {
          (
            (manifest.da_transport as Record<string, unknown>).limits as Record<
              string,
              unknown
            >
          ).max_payload_bytes = LIBP2P_DA_TRANSPORT_LIMITS.maxPayloadBytes + 1;
        },
        error: /max_payload_bytes/,
      },
      {
        mutate: (manifest) => {
          (
            (manifest.da_committee as Record<string, unknown>)
              .members as Record<string, unknown>[]
          )[0]!.roles = ["committee", "admin"];
        },
        error: /unrecognized libp2p DA role/,
      },
      {
        mutate: (manifest) => {
          (
            (manifest.da_committee as Record<string, unknown>)
              .members as Record<string, unknown>[]
          )[0]!.multiaddrs = [
            `/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_B}`,
          ];
        },
        error: /peer id must match/,
      },
      {
        mutate: (manifest) => {
          (
            (manifest.da_committee as Record<string, unknown>)
              .members as Record<string, unknown>[]
          )[0]!.da_vkey = "02".repeat(32);
        },
        env: { DA_COMMITTEE_HEX: "01".repeat(32), DA_THRESHOLD: "1" },
        error: /DA_COMMITTEE_HEX must exactly match/,
      },
    ];
    for (const testCase of cases) {
      await expectLibp2pManifestRejects(
        testCase.mutate,
        testCase.error,
        testCase.env,
      );
    }
  }, 30_000);

  it("derives contracts from the Midgard node deployment-info format", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeDaContractDeploymentFixture(dir);
    const manifest = libp2pManifest(
      member,
      ["committee", "retrieval"],
      await deploymentManifestIdFromFile(deploymentInfoPath),
    );
    delete manifest.contracts;
    const expectedDeployment = parseMidgardNodeDeploymentInfo(
      JSON.parse(await readFile(deploymentInfoPath, "utf8")) as Record<
        string,
        unknown
      >,
      "Preprod",
    );
    if (expectedDeployment === undefined) {
      throw new Error("real Midgard deployment fixture did not parse");
    }
    await writeFile(manifestPath, JSON.stringify(manifest));
    const config = await loadCommitteeConfig({
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    });

    expect(config.daAttestationPolicyId).toBe(
      expectedDeployment.daAttestation.policyId,
    );
    expect(config.daAttestationAddress).toBe(
      expectedDeployment.daAttestation.spendingScriptAddress,
    );
    expect(config.daParamsGovernorPolicyId).toBe(
      expectedDeployment.daParamsGovernor.policyId,
    );
    expect(config.daParamsGovernorAddress).toBe(
      expectedDeployment.daParamsGovernor.spendingScriptAddress,
    );
    expect(config.stateQueuePolicyId).toBe(
      expectedDeployment.stateQueue.policyId,
    );
    expect(config.stateQueueAddress).toBe(
      expectedDeployment.stateQueue.spendingScriptAddress,
    );
    expect(
      config.midgardNodeDeployment?.daAttestation.mint.refScriptOutRef,
    ).toEqual({
      // The reference-script out-ref the DA fixture records for
      // `daAttestationMint`; a literal rather than a re-read of
      // `expectedDeployment` so the config path is proved to carry the
      // document's own value rather than agreeing with itself.
      txHash: "8".padStart(64, "0"),
      outputIndex: 0,
    });
    expect(config.midgardNodeDeployment?.stateQueue.spend.scriptHash).toBe(
      expectedDeployment.stateQueue.spend.scriptHash,
    );
  });

  it("requires an L1 submitter key source when L1 submission is enabled", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeDaContractDeploymentFixture(dir);
    await writeFile(
      manifestPath,
      JSON.stringify(
        libp2pManifest(
          member,
          ["committee", "retrieval"],
          await deploymentManifestIdFromFile(deploymentInfoPath),
        ),
      ),
    );
    await expect(
      loadCommitteeConfig({
        ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        DA_L1_SUBMISSION_ENABLED: "true",
      }),
    ).rejects.toThrow(/L1_SUBMITTER_KEY_SOURCE/);
  });

  it("requires script CBOR and reference-script UTxOs in self-submitting coordinator mode", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    const incompleteDeployment = await readDaDeploymentFixture();
    delete (incompleteDeployment.contracts as Record<string, unknown>)
      .daAttestationMint;
    const deploymentWithId =
      withRecomputedDeploymentManifestId(incompleteDeployment);
    const manifest = libp2pManifest(
      member,
      ["committee", "coordinator"],
      String(deploymentWithId.manifestId),
    );
    await writeFile(manifestPath, JSON.stringify(manifest));
    await writeFile(deploymentInfoPath, JSON.stringify(deploymentWithId));
    await expect(
      loadCommitteeConfig({
        ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
        DA_L1_SUBMISSION_ENABLED: "true",
      }),
    ).rejects.toThrow(/contracts\.daAttestationMint is required/);
  });

  it("accepts real Midgard deployment info for self-submitting coordinator mode", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeDaContractDeploymentFixture(dir);
    await writeFile(
      manifestPath,
      JSON.stringify(
        libp2pManifest(
          member,
          ["committee", "coordinator"],
          await deploymentManifestIdFromFile(deploymentInfoPath),
        ),
      ),
    );
    await expect(
      loadCommitteeConfig({
        ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
        DA_L1_SUBMISSION_ENABLED: "true",
      }),
    ).resolves.toMatchObject({
      l1SubmissionEnabled: true,
      l1SubmitterKeySource: "private-key:ed25519_sk_test",
      l1SubmitterPreflight: {
        enabled: true,
        // 50 ADA of fee headroom plus 30 ADA for the attestation output; the
        // pooled DA bond is funded by the committee, not per block.
        minPlainAdaLovelace: 80_000_000n,
        minCollateralLovelace:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.minCollateralLovelace,
        minSpendableUtxoCount:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.minSpendableUtxoCount,
        autoFundBufferLovelace:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.autoFundBufferLovelace,
        retryCount: DEFAULT_L1_SUBMITTER_PREFLIGHT.retryCount,
        retryDelayMs: DEFAULT_L1_SUBMITTER_PREFLIGHT.retryDelayMs,
      },
    });
  });

  it("accepts explicit L1 wallet preflight and auto-fund settings", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeDaContractDeploymentFixture(dir);
    await writeFile(
      manifestPath,
      JSON.stringify(
        libp2pManifest(
          member,
          ["committee", "coordinator"],
          await deploymentManifestIdFromFile(deploymentInfoPath),
        ),
      ),
    );

    const config = await loadCommitteeConfig({
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
      DA_L1_MIN_PLAIN_ADA_LOVELACE: "30000000000",
      DA_L1_MIN_COLLATERAL_LOVELACE: "6000000",
      DA_L1_MIN_SPENDABLE_UTXO_COUNT: "3",
      DA_L1_AUTO_FUND_KEY_SOURCE: "file:/tmp/funder.seed",
      DA_L1_AUTO_FUND_BUFFER_LOVELACE: "12000000",
      DA_L1_PREFLIGHT_RETRY_COUNT: "5",
      DA_L1_PREFLIGHT_RETRY_DELAY_MS: "250",
    });

    expect(config.l1SubmitterPreflight).toEqual({
      enabled: true,
      minPlainAdaLovelace: 30_000_000_000n,
      minCollateralLovelace: 6_000_000n,
      minSpendableUtxoCount: 3,
      autoFundKeySource: "file:/tmp/funder.seed",
      autoFundBufferLovelace: 12_000_000n,
      retryCount: 5,
      retryDelayMs: 250,
    });
  });

  it("rejects malformed L1 wallet preflight config before network work", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeDaContractDeploymentFixture(dir);
    await writeFile(
      manifestPath,
      JSON.stringify(
        libp2pManifest(
          member,
          ["committee", "coordinator"],
          await deploymentManifestIdFromFile(deploymentInfoPath),
        ),
      ),
    );
    const baseEnv = {
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
    };

    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_MIN_PLAIN_ADA_LOVELACE: "not-a-number",
      }),
    ).rejects.toThrow(/DA_L1_MIN_PLAIN_ADA_LOVELACE/);
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_MIN_PLAIN_ADA_LOVELACE: "79999999",
      }),
    ).rejects.toThrow(
      /DA_L1_MIN_PLAIN_ADA_LOVELACE must be at least 80000000 \(50000000 fee headroom \+ 30000000 attestation min-ADA\)/u,
    );
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_MIN_PLAIN_ADA_LOVELACE: "80000000",
      }),
    ).resolves.toMatchObject({
      l1SubmitterPreflight: { minPlainAdaLovelace: 80_000_000n },
    });
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_MIN_SPENDABLE_UTXO_COUNT: "0",
      }),
    ).rejects.toThrow(/DA_L1_MIN_SPENDABLE_UTXO_COUNT/);
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_PREFLIGHT_RETRY_DELAY_MS: "-1",
      }),
    ).rejects.toThrow(/DA_L1_PREFLIGHT_RETRY_DELAY_MS/);
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_AUTO_FUND_KEY_SOURCE: "file:",
      }),
    ).rejects.toThrow(/DA_L1_AUTO_FUND_KEY_SOURCE/);
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_AUTO_FUND_KEY_SOURCE: "private-key:ed25519_sk_test",
      }),
    ).rejects.toThrow(/must not equal/);
  });

  it("accepts submitter-only L1 mode with optional relayer ids", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeDaContractDeploymentFixture(dir);
    await writeFile(
      manifestPath,
      JSON.stringify(
        libp2pManifest(
          member,
          ["committee", "retrieval"],
          await deploymentManifestIdFromFile(deploymentInfoPath),
        ),
      ),
    );
    const baseEnv = {
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
    };
    const config = await loadCommitteeConfig({
      ...baseEnv,
      DA_L1_SUBMITTER_ID: "relayer-a",
      DA_L1_SUBMITTER_IDS: "relayer-a,relayer-b",
    });
    expect(config).toMatchObject({
      l1SubmissionEnabled: true,
      l1SubmitterId: "relayer-a",
      l1SubmitterIds: ["relayer-a", "relayer-b"],
    });
    expect("signerIndex" in config).toBe(false);
    expect("signerKeySource" in config).toBe(false);
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        DA_L1_SUBMITTER_ID: "relayer-c",
        DA_L1_SUBMITTER_IDS: "relayer-a,relayer-b",
      }),
    ).rejects.toThrow(/DA_L1_SUBMITTER_ID/);
  });

  it("fails closed when required deployment contract fields are absent", async () => {
    const dir = await tempDir();
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    const deploymentInfo = await readDaDeploymentFixture();
    delete (deploymentInfo.contracts as Record<string, unknown>)
      .daAttestationMint;
    const deploymentWithId = withRecomputedDeploymentManifestId(deploymentInfo);
    const manifest = libp2pManifest(
      "01".repeat(32),
      ["committee"],
      String(deploymentWithId.manifestId),
    );
    await writeFile(manifestPath, JSON.stringify(manifest));
    await writeFile(deploymentInfoPath, JSON.stringify(deploymentWithId));
    await expect(
      loadCommitteeConfig({
        ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
      }),
    ).rejects.toThrow(/contracts\.daAttestationMint is required/);
  });

  it("rejects a recomputed manifest that omits a non-DA V1 contract", async () => {
    const dir = await tempDir();
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    const deploymentInfo = await readDaDeploymentFixture();
    delete (deploymentInfo.contracts as Record<string, unknown>).payoutMint;
    const deploymentWithId = withRecomputedDeploymentManifestId(deploymentInfo);
    const manifest = libp2pManifest(
      "01".repeat(32),
      ["committee"],
      String(deploymentWithId.manifestId),
    );
    await writeFile(manifestPath, JSON.stringify(manifest));
    await writeFile(deploymentInfoPath, JSON.stringify(deploymentWithId));
    await expect(
      loadCommitteeConfig(libp2pConfigEnv(manifestPath, deploymentInfoPath)),
    ).rejects.toThrow(/contracts\.payoutMint is required/);
  });
});

const expectLibp2pManifestRejects = async (
  mutate: (manifest: Record<string, unknown>) => void,
  error: RegExp,
  envOverrides: Record<string, string> = {},
): Promise<void> => {
  const dir = await tempDir();
  const manifest = libp2pManifest("01".repeat(32));
  mutate(manifest);
  const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
    dir,
    manifest,
  );
  await expect(
    loadCommitteeConfig({
      ...libp2pConfigEnv(manifestPath, deploymentInfoPath),
      ...envOverrides,
    }),
  ).rejects.toThrow(error);
};
