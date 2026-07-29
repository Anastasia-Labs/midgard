import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistencyV1,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION,
} from "../src/multi-provider-consistency.js";

const provider = (
  providerId: string,
  identityByte: string,
  operatorIdentityByte = identityByte,
) => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId,
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: operatorIdentityByte.repeat(32),
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256: identityByte.repeat(32),
  },
});

const localProvider = (
  surface: "chain_sync" | "ogmios" | "kupo" | "kupmios" | "db_sync",
  identityByte: string,
  authorityNodeId = "watcher-node-a",
) => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: surface.replace("_", "-"),
  source: {
    sourceMode: "local_node",
    authorityNodeId,
    surface,
  },
  authentication: {
    kind:
      surface === "chain_sync"
        ? ("cardano_node_genesis_v1" as const)
        : ("https_tls_identity_v1" as const),
    publicIdentitySha256: identityByte.repeat(32),
  },
});

const externalConfig = (network = "Preprod") => ({
  sourceMode: "external_providers",
  network,
  providers: [
    {
      providerId: "provider-a",
      operatorIdentitySha256: "aa".repeat(32),
    },
    {
      providerId: "provider-b",
      operatorIdentitySha256: "bb".repeat(32),
    },
  ],
});

const localConfig = () => ({
  sourceMode: "local_node",
  network: "Preprod",
  authorityNodeId: "watcher-node-a",
  genesisIdentitySha256: "cc".repeat(32),
});

const transaction = (bodyHex: string) => ({
  txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
  body: {
    bytesHex: bodyHex,
    sha256: createHash("sha256")
      .update(Buffer.from(bodyHex, "hex"))
      .digest("hex"),
  },
  utxos: [],
  scripts: [],
  datums: [],
  redeemers: [],
});

const observation = (
  providerId: string,
  identityByte: string,
  options: {
    blockHash?: string;
    slot?: string;
    blockNo?: string;
    depth?: string;
    bodyHex?: string;
    operatorIdentityByte?: string;
  } = {},
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(
    provider(
      providerId,
      identityByte,
      options.operatorIdentityByte ?? identityByte,
    ),
    {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: "Preprod",
      providerId,
      chainPoint: {
        blockHash: options.blockHash ?? "11".repeat(32),
        slot: options.slot ?? "1000",
        blockNo: options.blockNo ?? "100",
        depth: options.depth ?? "15",
      },
      transactions:
        options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
    },
  );

const localObservation = (
  surface: "chain_sync" | "ogmios" | "kupo" | "kupmios" | "db_sync",
  identityByte: string,
  options: {
    authorityNodeId?: string;
    blockHash?: string;
    slot?: string;
    blockNo?: string;
    depth?: string;
    bodyHex?: string;
  } = {},
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(
    localProvider(surface, identityByte, options.authorityNodeId),
    {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: "Preprod",
      providerId: surface.replace("_", "-"),
      chainPoint: {
        blockHash: options.blockHash ?? "11".repeat(32),
        slot: options.slot ?? "1000",
        blockNo: options.blockNo ?? "100",
        depth: options.depth ?? "15",
      },
      transactions:
        options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
    },
  );

describe("fail-closed multi-provider consistency", () => {
  it("allows exact independently authenticated agreement with explicit minimum depth", () => {
    const first = observation("provider-a", "aa", { depth: "15" });
    const second = observation("provider-b", "bb", { depth: "12" });
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      first,
      second,
    ]);

    expect(result).toMatchObject({
      schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION,
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "external_providers",
      configuredNetwork: "Preprod",
      authorityNodeId: null,
      authorityGenesisIdentitySha256: null,
      chainAuthorityObservationDigest: null,
      queryObservationCount: 0,
      observationCount: 2,
      independentProviderCount: 2,
      reasonCodes: ["providers_consistent"],
      alertCodes: [],
      rejectedObservationCount: 0,
      agreement: {
        pointDigest: first.chainPoint.pointDigest,
        blockHash: first.chainPoint.blockHash,
        slot: "1000",
        blockNo: "100",
        minimumDepth: "12",
        blockContentDigest: first.blockContentDigest,
      },
    });
    expect(result.consistencyDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.reasonCodes)).toBe(true);
    expect(Object.isFrozen(result.agreement)).toBe(true);
  });

  it("is byte-stable under provider arrival reordering", () => {
    const first = observation("provider-a", "aa");
    const second = observation("provider-b", "bb");

    const forward = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [first, second],
    );
    const reverse = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [second, first],
    );

    expect(reverse).toEqual(forward);
    expect(reverse.consistencyDigest).toBe(forward.consistencyDigest);
  });

  it("quarantines a single provider as insufficient", () => {
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      observation("provider-a", "aa"),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 1,
      reasonCodes: ["insufficient_independent_providers"],
      alertCodes: ["watcher_provider_quorum_unavailable"],
      agreement: null,
    });
  });

  it("quarantines duplicate provider and trust identities", () => {
    const duplicated = observation("provider-a", "aa");
    const exactDuplicate = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [duplicated, duplicated],
    );
    expect(exactDuplicate).toMatchObject({
      status: "quarantined",
      independentProviderCount: 1,
      reasonCodes: [
        "insufficient_independent_providers",
        "duplicate_provider_id",
        "duplicate_trust_identity",
        "duplicate_operator_identity",
      ],
      alertCodes: [
        "watcher_provider_quorum_unavailable",
        "watcher_provider_identity_collision",
      ],
    });

    const sharedTrust = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [
        observation("provider-a", "aa"),
        observation("provider-b", "aa", { operatorIdentityByte: "bb" }),
      ],
    );
    expect(sharedTrust.reasonCodes).toEqual([
      "insufficient_independent_providers",
      "duplicate_trust_identity",
    ]);

    const sharedOperator = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [
        observation("provider-a", "aa", { operatorIdentityByte: "ee" }),
        observation("provider-b", "bb", { operatorIdentityByte: "ee" }),
      ],
    );
    expect(sharedOperator.reasonCodes).toEqual([
      "insufficient_independent_providers",
      "unconfigured_provider",
    ]);
    expect(sharedOperator.alertCodes).toEqual([
      "watcher_provider_quorum_unavailable",
      "watcher_provider_not_configured",
    ]);
  });

  it("quarantines independently authenticated providers outside the configured allowlist", () => {
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      observation("provider-c", "cc"),
      observation("provider-d", "dd"),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 0,
      reasonCodes: [
        "insufficient_independent_providers",
        "unconfigured_provider",
      ],
      alertCodes: [
        "watcher_provider_quorum_unavailable",
        "watcher_provider_not_configured",
      ],
      agreement: null,
    });
  });

  it("quarantines observations from the wrong configured network", () => {
    const result = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig("Preview"),
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

    expect(result).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      independentProviderCount: 0,
      reasonCodes: ["insufficient_independent_providers", "network_mismatch"],
      alertCodes: [
        "watcher_provider_quorum_unavailable",
        "watcher_provider_network_mismatch",
      ],
      agreement: null,
    });
  });

  it("keeps bounded provider lag pending and never allows a protocol decision", () => {
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      observation("provider-a", "aa"),
      observation("provider-b", "bb", {
        blockHash: "22".repeat(32),
        slot: "1001",
        blockNo: "101",
        depth: "0",
      }),
    ]);

    expect(result).toMatchObject({
      status: "pending",
      protocolDecision: "quarantined",
      reasonCodes: ["bounded_provider_lag"],
      alertCodes: ["watcher_provider_lag"],
      agreement: null,
    });
  });

  it("quarantines stale and forked provider points", () => {
    const stale = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      observation("provider-a", "aa"),
      observation("provider-b", "bb", {
        blockHash: "22".repeat(32),
        slot: "1100",
        blockNo: "165",
      }),
    ]);
    expect(stale).toMatchObject({
      status: "quarantined",
      reasonCodes: ["stale_provider_observation"],
      alertCodes: ["watcher_provider_stale"],
    });

    const fork = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      observation("provider-a", "aa"),
      observation("provider-b", "bb", {
        blockHash: "22".repeat(32),
      }),
    ]);
    expect(fork).toMatchObject({
      status: "quarantined",
      reasonCodes: ["fork_disagreement"],
      alertCodes: ["watcher_provider_fork"],
    });
  });

  it("quarantines different provider-neutral content at the same point", () => {
    const result = evaluateWatcherMultiProviderConsistencyV1(externalConfig(), [
      observation("provider-a", "aa", { bodyHex: "a100" }),
      observation("provider-b", "bb", { bodyHex: "a101" }),
    ]);

    expect(result).toMatchObject({
      status: "quarantined",
      reasonCodes: ["block_content_mismatch"],
      alertCodes: ["watcher_provider_content_disagreement"],
      agreement: null,
    });
  });

  it("allows one watcher-operated chain-sync authority without a provider quorum", () => {
    const chainSync = localObservation("chain_sync", "cc", { depth: "19" });
    const result = evaluateWatcherMultiProviderConsistencyV1(localConfig(), [
      chainSync,
    ]);

    expect(result).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "local_node",
      configuredNetwork: "Preprod",
      authorityNodeId: "watcher-node-a",
      authorityGenesisIdentitySha256: "cc".repeat(32),
      chainAuthorityObservationDigest: chainSync.observationDigest,
      queryObservationCount: 0,
      observationCount: 1,
      independentProviderCount: 1,
      reasonCodes: ["local_node_consistent"],
      alertCodes: [],
      agreement: {
        pointDigest: chainSync.chainPoint.pointDigest,
        minimumDepth: "19",
        blockContentDigest: chainSync.blockContentDigest,
      },
    });
    expect(result.reasonCodes).not.toContain(
      "insufficient_independent_providers",
    );
  });

  it("accepts aligned query surfaces sharing the local node and ignores transport identity duplication for independence", () => {
    const chainSync = localObservation("chain_sync", "cc");
    const ogmios = localObservation("ogmios", "dd");
    const kupo = localObservation("kupo", "dd");
    const forward = evaluateWatcherMultiProviderConsistencyV1(localConfig(), [
      chainSync,
      ogmios,
      kupo,
    ]);
    const reverse = evaluateWatcherMultiProviderConsistencyV1(localConfig(), [
      kupo,
      ogmios,
      chainSync,
    ]);

    expect(forward).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      independentProviderCount: 1,
      queryObservationCount: 2,
      reasonCodes: ["local_node_consistent"],
      alertCodes: [],
    });
    expect(reverse).toEqual(forward);
  });

  it("fails closed when local query data is stale, forked, content-mismatched, or has not propagated a rollback", () => {
    const chainSync = localObservation("chain_sync", "cc");
    const stale = evaluateWatcherMultiProviderConsistencyV1(localConfig(), [
      chainSync,
      localObservation("ogmios", "dd", {
        blockHash: "22".repeat(32),
        slot: "999",
        blockNo: "99",
      }),
    ]);
    const fork = evaluateWatcherMultiProviderConsistencyV1(localConfig(), [
      chainSync,
      localObservation("kupo", "ee", {
        blockHash: "22".repeat(32),
      }),
    ]);
    const mismatchedBytes = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [
        localObservation("chain_sync", "cc", { bodyHex: "a100" }),
        localObservation("db_sync", "ff", { bodyHex: "a101" }),
      ],
    );
    const rollbackNotPropagated = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [
        chainSync,
        localObservation("kupmios", "11", {
          blockHash: "22".repeat(32),
          slot: "1001",
          blockNo: "101",
        }),
      ],
    );

    expect(stale).toMatchObject({
      status: "quarantined",
      reasonCodes: ["stale_provider_observation"],
      alertCodes: ["watcher_provider_stale"],
    });
    expect(fork).toMatchObject({
      status: "quarantined",
      reasonCodes: ["fork_disagreement"],
      alertCodes: ["watcher_provider_fork"],
    });
    expect(mismatchedBytes).toMatchObject({
      status: "quarantined",
      reasonCodes: ["block_content_mismatch"],
      alertCodes: ["watcher_provider_content_disagreement"],
    });
    expect(rollbackNotPropagated).toMatchObject({
      status: "quarantined",
      reasonCodes: ["rollback_not_propagated"],
      alertCodes: ["watcher_local_node_rollback_not_propagated"],
    });
  });

  it("rejects local authority, genesis, source-mode, and missing chain-sync substitutions", () => {
    const wrongAuthority = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [
        localObservation("chain_sync", "cc", {
          authorityNodeId: "watcher-node-b",
        }),
      ],
    );
    const wrongGenesis = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [localObservation("chain_sync", "aa")],
    );
    const missingChainSync = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [localObservation("ogmios", "dd")],
    );
    const externalSubstitution = evaluateWatcherMultiProviderConsistencyV1(
      localConfig(),
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

    expect(wrongAuthority.reasonCodes).toEqual([
      "local_node_authority_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(wrongGenesis.reasonCodes).toEqual([
      "local_node_genesis_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(missingChainSync.reasonCodes).toEqual([
      "missing_chain_sync_authority",
    ]);
    expect(externalSubstitution.reasonCodes).toEqual([
      "source_mode_mismatch",
      "missing_chain_sync_authority",
    ]);
    expect(
      [
        wrongAuthority,
        wrongGenesis,
        missingChainSync,
        externalSubstitution,
      ].every(({ protocolDecision }) => protocolDecision === "quarantined"),
    ).toBe(true);
  });

  it("quarantines malformed, unknown, and foreign input at a secret-safe boundary", () => {
    const secret = "https://operator:secret@example.invalid";
    const malformed = {
      ...observation("provider-a", "aa"),
      endpoint: secret,
    };
    const foreign = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(foreign, "provider", {
      enumerable: true,
      get: () => {
        throw new Error(secret);
      },
    });

    const malformedResult = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      [malformed, foreign],
    );
    const foreignBoundary = evaluateWatcherMultiProviderConsistencyV1(
      externalConfig(),
      new Error(secret),
    );
    const unknownNetwork = evaluateWatcherMultiProviderConsistencyV1(secret, [
      observation("provider-a", "aa"),
      observation("provider-b", "bb"),
    ]);
    const missingDiscriminator = evaluateWatcherMultiProviderConsistencyV1(
      "Preprod",
      [observation("provider-a", "aa"), observation("provider-b", "bb")],
    );

    expect(malformedResult).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      rejectedObservationCount: 2,
      reasonCodes: [
        "insufficient_independent_providers",
        "malformed_observation",
      ],
      agreement: null,
    });
    expect(foreignBoundary.status).toBe("quarantined");
    expect(unknownNetwork.reasonCodes).toContain("invalid_configured_network");
    expect(missingDiscriminator).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
      sourceMode: null,
      reasonCodes: [
        "insufficient_independent_providers",
        "invalid_configured_network",
      ],
    });
    expect(
      JSON.stringify([
        malformedResult,
        foreignBoundary,
        unknownNetwork,
        missingDiscriminator,
      ]),
    ).not.toContain(secret);
  });
});
