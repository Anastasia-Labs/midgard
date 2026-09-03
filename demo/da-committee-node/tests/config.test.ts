import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile-v1";
import { computeDeploymentManifestId } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { describe, expect, it, vi } from "vitest";

vi.mock("@al-ft/midgard-core/consensus-profile-v1", async (importOriginal) => {
  const original =
    await importOriginal<
      typeof import("@al-ft/midgard-core/consensus-profile-v1")
    >();
  return {
    ...original,
    assertMidgardConsensusReleaseReady: (): void => undefined,
  };
});

import {
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
  l1SourceAuthorityDigest,
  LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
  LIBP2P_DA_MIN_RETENTION_DAYS,
  LIBP2P_DA_TRANSPORT_LIMITS,
  loadWatcherConfig,
  parseL1SourceConfig,
} from "../src/config.js";
import { parseMidgardNodeDeploymentInfo } from "../src/l1/deployment.js";
import { loadPublicRetainedDaRuntimeConfig } from "../src/public-retained-da-config.js";
import { tempDir } from "./helpers.js";
import { readDaDeploymentFixture } from "./helpers/deployment-fixture.js";

describe("loadWatcherConfig", () => {
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
      loadWatcherConfig(libp2pConfigEnv(dir, manifestPath, deploymentInfoPath)),
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
      loadWatcherConfig(libp2pConfigEnv(dir, manifestPath, deploymentInfoPath)),
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
    const config = await loadWatcherConfig({
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    });
    expect(config.network).toBe("Preview");
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
    const base = libp2pConfigEnv(dir, manifestPath, deploymentInfoPath);

    await expect(loadWatcherConfig(base)).resolves.toMatchObject({
      finalityDepth: 30,
    });
    await expect(
      loadWatcherConfig({ ...base, CARDANO_FINALITY_DEPTH: "29" }),
    ).rejects.toThrow(
      /must exactly equal the verified deployment manifest l1Finality\.confirmationDepth/u,
    );
  });

  it("parses libp2p DA transport manifests without HTTP endpoint config", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifest = libp2pManifest(member);
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      manifest,
    );

    const config = await loadWatcherConfig(
      libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
    );

    expect(config.network).toBe("Preview");
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
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
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

    const config = await loadWatcherConfig(
      libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
    );

    expect(config.deploymentFingerprint).toBe(DEPLOYMENT_MANIFEST_ID);
  });

  it("rejects DA URL environment overrides in libp2p mode", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const baseEnv = libp2pConfigEnv(dir, manifestPath, deploymentInfoPath);
    for (const [name, value] of Object.entries({
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      DA_PEER_ENDPOINTS: "0@http://da-1.example",
      DA_COORDINATOR_ENDPOINT: "http://coordinator.example",
      DA_PUBLIC_BASE_URL: "http://da-self.example",
    })) {
      await expect(
        loadWatcherConfig({ ...baseEnv, [name]: value }),
      ).rejects.toThrow(new RegExp(name));
    }
  });

  it("requires a persistent libp2p private key source in libp2p mode", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const baseEnv = libp2pConfigEnv(dir, manifestPath, deploymentInfoPath);
    const missingKeyEnv: Record<string, string> = { ...baseEnv };
    delete missingKeyEnv.DA_LIBP2P_PRIVATE_KEY_SOURCE;

    await expect(loadWatcherConfig(missingKeyEnv)).rejects.toThrow(
      /DA_LIBP2P_PRIVATE_KEY_SOURCE/,
    );
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_LIBP2P_PRIVATE_KEY_SOURCE: "seed:abcd",
      }),
    ).rejects.toThrow(/DA_LIBP2P_PRIVATE_KEY_SOURCE seed/);
    await expect(
      loadWatcherConfig({
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
      manifest.network = "Preprod";
    }, /must exactly match contract deployment manifest network/u);

    await expectLibp2pManifestRejects(
      () => undefined,
      /MIDGARD_NETWORK must exactly match runtime manifest network Preview/u,
      { MIDGARD_NETWORK: "Preprod" },
    );
  });

  it("requires and binds an explicit network magic for Custom local-node authority", async () => {
    const dir = await tempDir();
    const deployment = await readDaDeploymentFixture();
    const customDeployment = withRecomputedDeploymentManifestId({
      ...deployment,
      network: "Custom",
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
    const baseEnv = libp2pConfigEnv(dir, manifestPath, deploymentInfoPath);

    await expect(loadWatcherConfig(baseEnv)).rejects.toThrow(
      /CARDANO_NETWORK_MAGIC is required for Custom/,
    );
    for (const invalid of ["-1", "01", "1.5", "4294967296"]) {
      await expect(
        loadWatcherConfig({ ...baseEnv, CARDANO_NETWORK_MAGIC: invalid }),
      ).rejects.toThrow(/CARDANO_NETWORK_MAGIC/);
    }

    const config = await loadWatcherConfig({
      ...baseEnv,
      CARDANO_NETWORK_MAGIC: "424242",
    });
    expect(config.cardanoL1Source).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "test-cardano-node",
      networkMagic: 424242,
    });
    expect(config.cardanoL1Source.authorityDigest).toMatch(/^[0-9a-f]{64}$/u);

    const otherAuthority = await loadWatcherConfig({
      ...baseEnv,
      CARDANO_NETWORK_MAGIC: "424242",
      CARDANO_LOCAL_NODE_AUTHORITY_ID: "other-cardano-node",
    });
    expect(otherAuthority.cardanoL1Source.authorityDigest).not.toBe(
      config.cardanoL1Source.authorityDigest,
    );
    const otherMagic = await loadWatcherConfig({
      ...baseEnv,
      CARDANO_NETWORK_MAGIC: "424243",
    });
    expect(otherMagic.cardanoL1Source.authorityDigest).not.toBe(
      config.cardanoL1Source.authorityDigest,
    );
  });

  it("rejects explicit network magic for named networks", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    await expect(
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
        CARDANO_NETWORK_MAGIC: "2",
      }),
    ).rejects.toThrow(/must be omitted for named Cardano networks/);
  });

  it("enforces disjoint local-node and external-provider authority identities", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const baseEnv = libp2pConfigEnv(dir, manifestPath, deploymentInfoPath);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        CARDANO_PROVIDER_URLS:
          "blockfrost:https://preview-a.example/api#project",
      }),
    ).rejects.toThrow(/local_node mode permits only same-node kupmios/);

    const external = await loadWatcherConfig({
      ...baseEnv,
      ...externalProviderConfigEnv(),
    });
    expect(external.cardanoL1Source).toMatchObject({
      sourceMode: "external_providers",
      providerAuthorityIds: ["11".repeat(32), "22".repeat(32)],
      networkMagic: 2,
    });

    await expect(
      loadWatcherConfig({
        ...baseEnv,
        ...externalProviderConfigEnv(),
        CARDANO_PROVIDER_AUTHORITY_IDS: `${"11".repeat(32)},${"11".repeat(32)}`,
      }),
    ).rejects.toThrow(/operationally independent/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        ...externalProviderConfigEnv(),
        CARDANO_PROVIDER_URLS:
          "blockfrost:https://preview-a.example/api#project",
        CARDANO_PROVIDER_AUTHORITY_IDS: "11".repeat(32),
      }),
    ).rejects.toThrow(/at least two/);
  });

  it("requires an explicit L1 source mode and keeps local query surfaces under one authority", async () => {
    const baseEnv = {
      CARDANO_L1_SOURCE_MODE: "local_node",
      CARDANO_L1_TEST_MODE: "true",
      CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-a",
      CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: "chain-sync:fixture:/tmp/state.json",
      CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH:
        "/tmp/state.chain-sync-cursor.json",
    };
    const missingMode: Record<string, string> = { ...baseEnv };
    delete missingMode.CARDANO_L1_SOURCE_MODE;
    expect(() =>
      parseL1SourceConfig(missingMode, ["fixture:/tmp/state.json"]),
    ).toThrow(/CARDANO_L1_SOURCE_MODE is required/u);

    expect(
      parseL1SourceConfig(baseEnv, ["fixture:/tmp/state.json"]),
    ).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "preview-node-a",
      chainSyncProviderUrl: "chain-sync:fixture:/tmp/state.json",
      chainSyncCursorPath: "/tmp/state.chain-sync-cursor.json",
      queryProviderUrls: ["fixture:/tmp/state.json"],
    });
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: "fixture:/tmp/state.json",
        },
        ["fixture:/tmp/state.json"],
      ),
    ).toThrow(/chain-sync:<provider>/u);
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_L1_TEST_MODE: "false",
        },
        ["fixture:/tmp/state.json"],
      ),
    ).toThrow(/CARDANO_L1_TEST_MODE=true/u);
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_L1_TEST_MODE: "false",
        },
        ["kupmios:http://kupo.local|ws://ogmios.local"],
      ),
    ).toThrow(/local chain-sync sources.*CARDANO_L1_TEST_MODE=true/u);
    expect(
      parseL1SourceConfig(
        {
          CARDANO_L1_SOURCE_MODE: "local_node",
          CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-a",
          CARDANO_LOCAL_NODE_CHAIN_SYNC_URL:
            "chain-sync:kupmios:http://kupo.local|ws://ogmios.local",
          CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH:
            "/var/lib/midgard/chain-sync-cursor.json",
        },
        ["kupmios:http://kupo.local|ws://ogmios.local"],
      ),
    ).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "preview-node-a",
    });
    expect(() =>
      parseL1SourceConfig(
        {
          CARDANO_L1_SOURCE_MODE: "local_node",
          CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-a",
          CARDANO_LOCAL_NODE_CHAIN_SYNC_URL:
            "chain-sync:ogmios:ws://ogmios.local",
          CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH:
            "/var/lib/midgard/chain-sync-cursor.json",
        },
        ["blockfrost:https://cardano.example#project"],
      ),
    ).toThrow(/must be kupmios: backed by the local authority/u);
    expect(() =>
      parseL1SourceConfig(
        {
          CARDANO_L1_SOURCE_MODE: "local_node",
          CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-a",
          CARDANO_LOCAL_NODE_CHAIN_SYNC_URL:
            "chain-sync:ogmios:ws://ogmios-a.local",
          CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH:
            "/var/lib/midgard/chain-sync-cursor.json",
        },
        ["kupmios:http://kupo.local|ws://ogmios-b.local"],
      ),
    ).toThrow(/not backed by the configured chain-sync authority/u);
  });

  it("binds the authority digest to the source mode, network, and authority endpoints", () => {
    const local = parseL1SourceConfig(
      {
        CARDANO_L1_SOURCE_MODE: "local_node",
        CARDANO_L1_TEST_MODE: "true",
        CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-a",
        CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: "chain-sync:fixture:/tmp/state.json",
        CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH:
          "/tmp/state.chain-sync-cursor.json",
      },
      ["fixture:/tmp/state.json"],
    );
    if (local.sourceMode !== "local_node") {
      throw new Error("expected local-node source fixture");
    }
    const baseline = l1SourceAuthorityDigest("Preview", local);
    expect(baseline).toMatch(/^[0-9a-f]{64}$/u);
    expect(l1SourceAuthorityDigest("Preprod", local)).not.toBe(baseline);
    expect(
      l1SourceAuthorityDigest("Preview", {
        ...local,
        authorityNodeId: "preview-node-b",
      }),
    ).not.toBe(baseline);
  });

  it("requires two distinct operational identities in external-provider mode", () => {
    const baseEnv: Record<string, string> = {
      CARDANO_L1_SOURCE_MODE: "external_providers",
      CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a,operator-b",
    };
    expect(
      parseL1SourceConfig(baseEnv, [
        "blockfrost:https://a.example#project-a",
        "blockfrost:https://b.example#project-b",
      ]),
    ).toMatchObject({
      sourceMode: "external_providers",
      providers: [{ identity: "operator-a" }, { identity: "operator-b" }],
    });
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a",
        },
        ["blockfrost:https://a.example#project-a"],
      ),
    ).toThrow(/at least two operationally independent/u);
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a,operator-a",
        },
        [
          "blockfrost:https://a.example#project-a",
          "blockfrost:https://b.example#project-b",
        ],
      ),
    ).toThrow(/distinct operational provider identities/u);
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a,operator-b",
        },
        [
          "blockfrost:https://shared.example/api/#project-a",
          "blockfrost:https://SHARED.example:443/other#project-b",
        ],
      ),
    ).toThrow(/share normalized endpoint https:\/\/shared\.example/u);
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a,operator-b",
        },
        [
          "kupmios:https://kupo-a.example|wss://shared-ogmios.example",
          "kupmios:https://kupo-b.example|wss://shared-ogmios.example/",
        ],
      ),
    ).toThrow(/share normalized endpoint https:\/\/shared-ogmios\.example/u);
    const plaintextKupmios = [
      "kupmios:http://kupo-a.example|ws://ogmios-a.example",
      "kupmios:http://kupo-b.example|ws://ogmios-b.example",
    ];
    expect(() => parseL1SourceConfig(baseEnv, plaintextKupmios)).toThrow(
      /require HTTPS Kupo and TLS-protected WSS\/HTTPS Ogmios/u,
    );
    expect(
      parseL1SourceConfig(
        { ...baseEnv, CARDANO_L1_TEST_MODE: "true" },
        plaintextKupmios,
      ),
    ).toMatchObject({ sourceMode: "external_providers" });
    expect(() =>
      parseL1SourceConfig(
        {
          ...baseEnv,
          CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a,operator-b",
        },
        [
          "blockfrost:https://shared.example.#project-a",
          "blockfrost:https://shared.example#project-b",
        ],
      ),
    ).toThrow(/share normalized endpoint https:\/\/shared\.example/u);
    expect(
      parseL1SourceConfig(baseEnv, [
        "blockfrost:https://a.example#project-a",
        "blockfrost:https://b.example#project-b",
      ]),
    ).toMatchObject({
      providers: [
        {
          operationalIdentity: {
            operatorId: "operator-a",
            transport: "blockfrost_https",
            normalizedEndpoints: ["https://a.example"],
          },
        },
        {
          operationalIdentity: {
            operatorId: "operator-b",
            transport: "blockfrost_https",
            normalizedEndpoints: ["https://b.example"],
          },
        },
      ],
    });
  });

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
  });

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
      "Preview",
    );
    if (expectedDeployment === undefined) {
      throw new Error("real Midgard deployment fixture did not parse");
    }
    await writeFile(manifestPath, JSON.stringify(manifest));
    const config = await loadWatcherConfig({
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
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
      txHash: "01".repeat(32),
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
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        DA_L1_SUBMISSION_ENABLED: "true",
      }),
    ).rejects.toThrow(/L1_SUBMITTER_KEY_SOURCE/);
  });

  it("requires a live Cardano provider in self-submitting coordinator mode", async () => {
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
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
        DA_L1_SUBMISSION_ENABLED: "true",
      }),
    ).rejects.toThrow(/blockfrost: or kupmios:/);
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
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
        ...externalProviderConfigEnv(),
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
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
        ...externalProviderConfigEnv(),
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
        minPlainAdaLovelace: DEFAULT_L1_SUBMITTER_PREFLIGHT.minPlainAdaLovelace,
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

    const config = await loadWatcherConfig({
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      ...externalProviderConfigEnv(),
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
      DA_L1_MIN_PLAIN_ADA_LOVELACE: "75000000",
      DA_L1_MIN_COLLATERAL_LOVELACE: "6000000",
      DA_L1_MIN_SPENDABLE_UTXO_COUNT: "3",
      DA_L1_AUTO_FUND_KEY_SOURCE: "file:/tmp/funder.seed",
      DA_L1_AUTO_FUND_BUFFER_LOVELACE: "12000000",
      DA_L1_PREFLIGHT_RETRY_COUNT: "5",
      DA_L1_PREFLIGHT_RETRY_DELAY_MS: "250",
    });

    expect(config.l1SubmitterPreflight).toEqual({
      enabled: true,
      minPlainAdaLovelace: 75_000_000n,
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
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      ...externalProviderConfigEnv(),
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
    };

    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_MIN_PLAIN_ADA_LOVELACE: "not-a-number",
      }),
    ).rejects.toThrow(/DA_L1_MIN_PLAIN_ADA_LOVELACE/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_MIN_SPENDABLE_UTXO_COUNT: "0",
      }),
    ).rejects.toThrow(/DA_L1_MIN_SPENDABLE_UTXO_COUNT/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_PREFLIGHT_RETRY_DELAY_MS: "-1",
      }),
    ).rejects.toThrow(/DA_L1_PREFLIGHT_RETRY_DELAY_MS/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_AUTO_FUND_KEY_SOURCE: "file:",
      }),
    ).rejects.toThrow(/DA_L1_AUTO_FUND_KEY_SOURCE/);
    await expect(
      loadWatcherConfig({
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
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      ...externalProviderConfigEnv(),
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
    };
    const config = await loadWatcherConfig({
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
      loadWatcherConfig({
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
      loadWatcherConfig({
        ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
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
      loadWatcherConfig(libp2pConfigEnv(dir, manifestPath, deploymentInfoPath)),
    ).rejects.toThrow(/contracts\.payoutMint is required/);
  });
});

const LIBP2P_PEER_ID_A = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
const LIBP2P_PEER_ID_B = "12D3KooWR3iZBFz6W2fyFdRt2t45x2Ytz9p6c9JwHyDqaN49XU47";
const LIBP2P_PEER_ID_PUBLIC =
  "12D3KooWCQ8WRN84GxEkR7k8dV6gb4ca3bNqM5LmT3evQVfBPGwv";
const LIBP2P_PRIVATE_KEY_SOURCE = `seed:${"00".repeat(31)}01`;
const canonicalDeploymentManifest = await readDaDeploymentFixture();
const DEPLOYMENT_MANIFEST_ID = canonicalDeploymentManifest.manifestId;
if (typeof DEPLOYMENT_MANIFEST_ID !== "string") {
  throw new Error("Canonical deployment fixture is missing manifestId");
}

const libp2pManifest = (
  member: string,
  roles: readonly string[] = ["committee", "retrieval"],
  deploymentManifestId = DEPLOYMENT_MANIFEST_ID,
): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
  network: "Preview",
  deployment: {
    fingerprint: deploymentManifestId.toUpperCase(),
    contract_deployment_manifest_id: deploymentManifestId,
    contract_deployment_info_sha256: "cd".repeat(32),
    identity_source: "contract_deployment_manifest_id",
  },
  runtime_topology: {
    target: "watcher",
    profile: "public",
    producer_peer_id: LIBP2P_PEER_ID_B,
    local_signer_index: 0,
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
    announce_multiaddrs: [
      `/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`,
    ],
    bootstrap_multiaddrs: [
      `/dns4/bootstrap.example/tcp/4001/p2p/${LIBP2P_PEER_ID_B}`,
    ],
    retention_days: LIBP2P_DA_MIN_RETENTION_DAYS,
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: LIBP2P_DA_GOSSIP_MAX_MESSAGE_BYTES,
    },
    limits: {
      max_payload_bytes: LIBP2P_DA_TRANSPORT_LIMITS.maxPayloadBytes,
      max_inline_response_bytes:
        LIBP2P_DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      max_chunk_bytes: LIBP2P_DA_TRANSPORT_LIMITS.maxChunkBytes,
      max_streams_per_peer: LIBP2P_DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      request_timeout_ms: LIBP2P_DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  },
  public_retained_da: {
    profile: "public-retained-da-v1",
    access_policy: "any_noise_authenticated_peer",
    peer_id: LIBP2P_PEER_ID_PUBLIC,
    listen_multiaddrs: ["/ip4/0.0.0.0/tcp/0"],
    announce_multiaddrs: [
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
    limits: {
      max_streams_per_peer: 4,
      max_inflight_requests: 32,
      max_inflight_requests_per_peer: 2,
      max_inflight_proof_requests: 1,
      request_timeout_ms: LIBP2P_DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  },
  da_committee: {
    threshold: 1,
    members: [
      {
        signer_index: 0,
        da_vkey: member,
        peer_id: LIBP2P_PEER_ID_A,
        multiaddrs: [`/dns4/da-a.example/tcp/4001/p2p/${LIBP2P_PEER_ID_A}`],
        roles,
      },
    ],
  },
});

const withRecomputedDeploymentManifestId = (
  manifest: Record<string, unknown>,
): Record<string, unknown> => {
  const { manifestId: _manifestId, ...identityInput } = manifest;
  return {
    ...identityInput,
    manifestId: computeDeploymentManifestId(identityInput),
  };
};

const writeConfigFiles = async (
  dir: string,
  manifest: Record<string, unknown>,
): Promise<{
  readonly manifestPath: string;
  readonly deploymentInfoPath: string;
}> => {
  const manifestPath = join(dir, "manifest.json");
  const deploymentInfoPath = join(dir, "deployment.json");
  await writeFile(manifestPath, JSON.stringify(manifest));
  await writeMinimalDeploymentInfo(deploymentInfoPath);
  return { manifestPath, deploymentInfoPath };
};

const writeMinimalDeploymentInfo = async (
  path: string,
  manifestId = DEPLOYMENT_MANIFEST_ID,
): Promise<void> => {
  const fixture = await readDaDeploymentFixture();
  await writeFile(
    path,
    JSON.stringify({
      ...fixture,
      manifestId,
    }),
  );
};

const deploymentManifestIdFromFile = async (path: string): Promise<string> => {
  const parsed = JSON.parse(await readFile(path, "utf8")) as Record<
    string,
    unknown
  >;
  if (typeof parsed.manifestId !== "string") {
    throw new Error(`${path} is missing manifestId`);
  }
  return parsed.manifestId;
};

const libp2pConfigEnv = (
  dir: string,
  manifestPath: string,
  deploymentInfoPath: string,
): Record<string, string> => ({
  MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
  CARDANO_L1_SOURCE_MODE: "local_node",
  CARDANO_LOCAL_NODE_AUTHORITY_ID: "test-cardano-node",
  CARDANO_L1_TEST_MODE: "true",
  CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: "chain-sync:fixture:/tmp/state.json",
  CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH: join(
    dir,
    "chain-sync-cursor.json",
  ),
  CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
  CARDANO_FINALITY_DEPTH: "30",
  DA_LIBP2P_PRIVATE_KEY_SOURCE: LIBP2P_PRIVATE_KEY_SOURCE,
  WATCHER_DB_PATH: join(dir, "db"),
});

const externalProviderConfigEnv = (): Record<string, string | undefined> => ({
  CARDANO_L1_SOURCE_MODE: "external_providers",
  CARDANO_LOCAL_NODE_AUTHORITY_ID: undefined,
  CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: undefined,
  CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH: undefined,
  CARDANO_PROVIDER_URLS:
    "blockfrost:https://preview-a.example/api#project-a,blockfrost:https://preview-b.example/api#project-b",
  CARDANO_PROVIDER_AUTHORITY_IDS: `${"11".repeat(32)},${"22".repeat(32)}`,
  CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "operator-a,operator-b",
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
    loadWatcherConfig({
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      ...envOverrides,
    }),
  ).rejects.toThrow(error);
};

const writeDaContractDeploymentFixture = async (
  dir: string,
): Promise<string> => {
  const fixturePath = join(dir, "contract-deployment-info.with-refs.json");
  await writeFile(fixturePath, JSON.stringify(await readDaDeploymentFixture()));
  return fixturePath;
};
