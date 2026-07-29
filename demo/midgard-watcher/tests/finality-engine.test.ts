import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  makeDeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../src/config.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
  parseWatcherFinalityPolicyV1,
  parseWatcherFinalityStateV1,
  WATCHER_FINALITY_POLICY_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_STATE_V1_SCHEMA_VERSION,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityStateV1,
} from "../src/finality-engine.js";
import {
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "../src/multi-provider-consistency.js";

const hex32 = (byte: string): string => byte.repeat(32);
const externalSource = {
  sourceMode: "external_providers",
  network: "Preprod",
} as const;

const config = (depth = 3, rollbackDepth = depth) => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: hex32("a1"),
          endpoint: "https://cardano-a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: hex32("b2"),
          endpoint: "https://cardano-b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
    finality: {
      depth,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: rollbackDepth,
      },
    },
  },
  da: {
    peers: [
      {
        identity: "da-peer-a",
        multiaddr:
          "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
  },
  proverWallet: {
    keySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_PROVER_KEY",
    },
  },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

const localConfig = (depth = 3, rollbackDepth = depth) => {
  const base = config(depth, rollbackDepth);
  return {
    ...base,
    l1: {
      ...base.l1,
      source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "cardano-node-a",
        chainSync: {
          kind: "cardano_node_socket" as const,
          socketPath: "/var/lib/cardano/node.socket",
          genesisIdentitySha256: hex32("a1"),
        },
        queryServices: [],
      },
    },
  };
};

const deploymentIdentity = (
  manifestByte = "11",
  releaseByte = "22",
  network: "Mainnet" | "Preprod" | "Preview" = "Preprod",
) => ({
  manifestId: hex32(manifestByte),
  network,
  trustRootId: hex32("33"),
  releaseEvidenceDigest: hex32(releaseByte),
  ruleBundleCommitment: hex32("44"),
  programCommitments: { validation: hex32("55") },
  durableMarker: makeDeploymentMarkerV1(hex32(manifestByte)),
});

const policy = (
  depth = 3,
  manifestByte = "11",
  releaseByte = "22",
  rollbackDepth = depth,
): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
    config(depth, rollbackDepth),
    deploymentIdentity(manifestByte, releaseByte),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const localPolicy = (depth = 3): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
    localConfig(depth),
    deploymentIdentity(),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

const provider = (providerId: string, identityByte: string) => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod" as const,
  providerId,
  source: {
    sourceMode: "external_providers" as const,
    operatorIdentitySha256: hex32(identityByte),
  },
  authentication: {
    kind: "https_tls_identity_v1" as const,
    publicIdentitySha256: hex32(identityByte),
  },
});

const localNodeProvider = (
  surface: "chain_sync" | "ogmios" = "chain_sync",
) => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod" as const,
  providerId:
    surface === "chain_sync" ? "cardano-node-a" : `cardano-node-a-${surface}`,
  source: {
    sourceMode: "local_node" as const,
    authorityNodeId: "cardano-node-a",
    surface,
  },
  authentication: {
    kind:
      surface === "chain_sync"
        ? ("cardano_node_genesis_v1" as const)
        : ("https_tls_identity_v1" as const),
    publicIdentitySha256: surface === "chain_sync" ? hex32("a1") : hex32("b2"),
  },
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

type ObservationOptions = Readonly<{
  blockHash?: string;
  slot?: string;
  blockNo?: string;
  depth?: string;
  bodyHex?: string;
}>;

const observation = (
  providerId: string,
  identityByte: string,
  options: ObservationOptions = {},
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(provider(providerId, identityByte), {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId,
    chainPoint: {
      blockHash: options.blockHash ?? hex32("aa"),
      slot: options.slot ?? "1000",
      blockNo: options.blockNo ?? "100",
      depth: options.depth ?? "0",
    },
    transactions:
      options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
  });

const agreement = (
  depth: string,
  options: ObservationOptions = {},
  reverse = false,
) => {
  const observations = [
    observation("provider-a", "a1", { ...options, depth }),
    observation("provider-b", "b2", { ...options, depth }),
  ];
  return evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    reverse ? observations.reverse() : observations,
  );
};

const localAgreement = (
  depth: string,
  options: ObservationOptions = {},
  includeAlignedQuery = false,
) => {
  const normalized = (
    surface: "chain_sync" | "ogmios",
  ): WatcherNormalizedL1BlockV1 =>
    normalizeWatcherL1BlockV1(localNodeProvider(surface), {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: "Preprod",
      providerId:
        surface === "chain_sync"
          ? "cardano-node-a"
          : `cardano-node-a-${surface}`,
      chainPoint: {
        blockHash: options.blockHash ?? hex32("aa"),
        slot: options.slot ?? "1000",
        blockNo: options.blockNo ?? "100",
        depth,
      },
      transactions:
        options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
    });
  return evaluateWatcherMultiProviderConsistencyV1(
    {
      sourceMode: "local_node",
      network: "Preprod",
      authorityNodeId: "cardano-node-a",
      genesisIdentitySha256: hex32("a1"),
    },
    [
      normalized("chain_sync"),
      ...(includeAlignedQuery ? [normalized("ogmios")] : []),
    ],
  );
};

const pendingAt = (
  finalityPolicy: WatcherFinalityPolicyV1,
  depth: string,
  options: ObservationOptions = {},
): WatcherFinalityStateV1 => {
  const result = evaluateWatcherFinalityV1(
    finalityPolicy,
    null,
    agreement(depth, options),
  );
  expect(result.action).toBe("observe_pending");
  return result.state as WatcherFinalityStateV1;
};

const finalizeAtThreshold = (
  finalityPolicy: WatcherFinalityPolicyV1,
  options: ObservationOptions = {},
): WatcherFinalityStateV1 => {
  const pending = pendingAt(finalityPolicy, "2", options);
  const result = evaluateWatcherFinalityV1(
    finalityPolicy,
    pending,
    agreement("3", options),
  );
  expect(result.action).toBe("finalize");
  return result.state as WatcherFinalityStateV1;
};

describe("canonical release-bound watcher finality", () => {
  it("binds W01 finality to the verified release and deployment marker", () => {
    const value = policy();

    expect(value).toMatchObject({
      schemaVersion: WATCHER_FINALITY_POLICY_V1_SCHEMA_VERSION,
      network: "Preprod",
      sourceMode: "external_providers",
      authorityNodeId: null,
      authorityGenesisIdentitySha256: null,
      confirmationDepth: "3",
      maximumPreFinalityRollbackDepth: "3",
      beforeFinalityRollback: "rewind",
      afterFinalityRollback: "quarantine",
      releaseEvidenceDigest: hex32("22"),
      deploymentMarker: {
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
        manifestId: hex32("11"),
      },
    });
    expect(value.policyDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(parseWatcherFinalityPolicyV1(value)).toEqual(value);
    expect(Object.isFrozen(value)).toBe(true);

    expect(localPolicy()).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "cardano-node-a",
      authorityGenesisIdentitySha256: hex32("a1"),
    });
  });

  it("rejects configuration/deployment mismatches without emitting values", () => {
    const wrongNetwork = makeWatcherFinalityPolicyV1(
      config(),
      deploymentIdentity("11", "22", "Preview"),
    );
    const malformedMarker = {
      ...deploymentIdentity(),
      durableMarker: makeDeploymentMarkerV1(hex32("99")),
    };

    expect(wrongNetwork).toBeNull();
    expect(makeWatcherFinalityPolicyV1(config(), malformedMarker)).toBeNull();
  });

  it("accepts one authoritative local-node observation and rejects source substitution", () => {
    const finalityPolicy = localPolicy();
    const firstConsistency = localAgreement("2");
    const first = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      firstConsistency,
    );
    const finalized = evaluateWatcherFinalityV1(
      finalityPolicy,
      first.state,
      localAgreement("3", {}, true),
    );
    const externalSubstitution = evaluateWatcherFinalityV1(
      finalityPolicy,
      first.state,
      agreement("3"),
    );
    const foreignConfig = localConfig();
    const foreignPolicy = makeWatcherFinalityPolicyV1(
      {
        ...foreignConfig,
        l1: {
          ...foreignConfig.l1,
          source: {
            ...foreignConfig.l1.source,
            authorityNodeId: "cardano-node-b",
            chainSync: {
              ...foreignConfig.l1.source.chainSync,
              genesisIdentitySha256: hex32("c3"),
            },
          },
        },
      },
      deploymentIdentity(),
    );

    expect(firstConsistency).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "local_node",
      authorityNodeId: "cardano-node-a",
      authorityGenesisIdentitySha256: hex32("a1"),
      observationCount: 1,
      independentProviderCount: 1,
      queryObservationCount: 0,
      reasonCodes: ["local_node_consistent"],
    });
    expect(first).toMatchObject({
      action: "observe_pending",
      protocolDecision: "hold",
    });
    expect(finalized).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      state: { phase: "finalized" },
    });
    expect(externalSubstitution).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_mode_mismatch"],
      state: first.state,
    });
    expect(
      evaluateWatcherFinalityV1(foreignPolicy, null, firstConsistency),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["source_authority_mismatch"],
    });
  });

  it("keeps first visibility pending even when already above the threshold", () => {
    const first = evaluateWatcherFinalityV1(policy(), null, agreement("8"));

    expect(first).toMatchObject({
      schemaVersion: WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
      action: "observe_pending",
      protocolDecision: "hold",
      reasonCodes: ["first_visibility_pending"],
      alertCodes: ["watcher_finality_pending"],
      state: {
        schemaVersion: WATCHER_FINALITY_STATE_V1_SCHEMA_VERSION,
        phase: "pending",
        pending: {
          firstSeenDepth: "8",
          currentDepth: "8",
          visibilityCount: "1",
        },
      },
    });
    expect(first.resultDigest).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("finalizes threshold-1 to threshold exactly once", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "2");
    const finalized = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("3"),
    );
    const later = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized.state,
      agreement("4"),
    );

    expect(finalized).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      reasonCodes: ["confirmation_depth_reached"],
      alertCodes: [],
      state: {
        phase: "finalized",
        pending: null,
        finalized: {
          firstSeenDepth: "2",
          currentDepth: "3",
          visibilityCount: "2",
        },
      },
    });
    expect(later).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["already_finalized"],
      state: finalized.state,
    });
  });

  it("makes exact duplicates idempotent before and after restart", () => {
    const finalityPolicy = policy();
    const evidence = agreement("1");
    const first = evaluateWatcherFinalityV1(finalityPolicy, null, evidence);
    const restarted = JSON.parse(
      JSON.stringify(first.state),
    ) as WatcherFinalityStateV1;
    const duplicate = evaluateWatcherFinalityV1(
      finalityPolicy,
      restarted,
      evidence,
    );

    expect(parseWatcherFinalityStateV1(restarted)).toEqual(first.state);
    expect(duplicate).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_observation"],
      state: first.state,
    });
    expect(duplicate.state?.stateDigest).toBe(first.state?.stateDigest);
  });

  it("advances below-threshold depth without irreversible state", () => {
    const finalityPolicy = policy(4);
    const pending = pendingAt(finalityPolicy, "1");
    const advanced = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2"),
    );

    expect(advanced).toMatchObject({
      action: "advance_pending",
      protocolDecision: "hold",
      reasonCodes: ["pending_depth_progress", "confirmation_depth_pending"],
      state: { phase: "pending", pending: { currentDepth: "2" } },
    });
  });

  it("emits a deterministic rewind for a pre-finality depth regression", () => {
    const finalityPolicy = policy(5);
    const pending = pendingAt(finalityPolicy, "3");
    const rewound = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2"),
    );
    const replay = evaluateWatcherFinalityV1(
      finalityPolicy,
      JSON.parse(JSON.stringify(pending)),
      agreement("2", {}, true),
    );

    expect(rewound).toMatchObject({
      action: "rewind_pending",
      protocolDecision: "rewind_required",
      reasonCodes: ["pending_depth_regression"],
      alertCodes: [
        "watcher_finality_pending",
        "watcher_finality_rewind_required",
      ].reverse(),
      state: { phase: "pending", pending: { currentDepth: "2" } },
      rewindInstruction: {
        kind: "pending_depth_regression",
        discardedStateDigest: pending.stateDigest,
        replacementDepth: "2",
      },
    });
    expect(replay).toEqual(rewound);
  });

  it("emits explicit rewinds for pre-finality fork and content mutation", () => {
    const finalityPolicy = policy(5);
    const pending = pendingAt(finalityPolicy, "1");
    const fork = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      agreement("2", { bodyHex: "a100" }),
    );

    expect(fork).toMatchObject({
      action: "rewind_pending",
      reasonCodes: ["pending_point_changed"],
      rewindInstruction: { kind: "pending_point_changed" },
    });
    expect(content).toMatchObject({
      action: "rewind_pending",
      reasonCodes: ["pending_content_changed"],
      rewindInstruction: { kind: "pending_content_changed" },
    });
  });

  it("enforces the exact pre-finality rollback bound and adjacent excess", () => {
    const finalityPolicy = policy(5, "11", "22", 2);
    const depthTwo = pendingAt(finalityPolicy, "2");
    const exactDepth = evaluateWatcherFinalityV1(
      finalityPolicy,
      depthTwo,
      agreement("0"),
    );
    const depthThree = pendingAt(finalityPolicy, "3");
    const adjacentExcess = evaluateWatcherFinalityV1(
      finalityPolicy,
      depthThree,
      agreement("0"),
    );
    const exactFork = evaluateWatcherFinalityV1(
      finalityPolicy,
      pendingAt(finalityPolicy, "1"),
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const excessiveFork = evaluateWatcherFinalityV1(
      finalityPolicy,
      depthTwo,
      agreement("3", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );

    expect(exactDepth).toMatchObject({
      action: "rewind_pending",
      protocolDecision: "rewind_required",
      reasonCodes: ["pending_depth_regression"],
    });
    expect(adjacentExcess).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["pre_finality_rollback_depth_exceeded"],
      alertCodes: [
        "watcher_finality_input_rejected",
        "watcher_finality_rollback_limit_exceeded",
      ],
      state: depthThree,
    });
    expect(exactFork.action).toBe("rewind_pending");
    expect(excessiveFork.reasonCodes).toEqual([
      "pre_finality_rollback_depth_exceeded",
    ]);
  });

  it("fails closed on same-depth stale evidence", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "1");
    const third = observation("provider-c", "c3", { depth: "1" });
    const threeProviderAgreement = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [
        observation("provider-a", "a1", { depth: "1" }),
        observation("provider-b", "b2", { depth: "1" }),
        third,
      ],
    );
    const stale = evaluateWatcherFinalityV1(
      finalityPolicy,
      pending,
      threeProviderAgreement,
    );

    expect(stale).toMatchObject({
      action: "reject",
      protocolDecision: "hold",
      reasonCodes: ["stale_observation"],
      state: pending,
    });
  });

  it("rejects W11 pending and quarantine without advancing state", () => {
    const finalityPolicy = policy();
    const initialPending = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [
        observation("provider-a", "a1", { depth: "0" }),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
          depth: "0",
        }),
      ],
    );
    const quarantined = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [observation("provider-a", "a1")],
    );

    expect(
      evaluateWatcherFinalityV1(finalityPolicy, null, initialPending),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_pending"],
      state: { phase: "unobserved" },
    });
    expect(
      evaluateWatcherFinalityV1(finalityPolicy, null, quarantined),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      state: { phase: "unobserved" },
    });
  });

  it("turns post-finality depth rollback into a durable quarantine incident", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const rolledBack = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      agreement("2"),
    );

    expect(rolledBack).toMatchObject({
      action: "quarantine_incident",
      protocolDecision: "quarantined",
      reasonCodes: [
        "post_finality_depth_regression",
        "post_finality_contradiction",
      ],
      alertCodes: ["watcher_finality_post_finality_incident"],
      state: {
        phase: "quarantined",
        pending: null,
        finalized: finalized.finalized,
        incident: {
          reasonCode: "post_finality_depth_regression",
        },
      },
    });
    expect(rolledBack.state?.finalized).toEqual(finalized.finalized);
  });

  it("quarantines post-finality point/content changes and W11 quasi-rollbacks", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const point = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      agreement("4", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      agreement("4", { bodyHex: "a100" }),
    );
    const pendingW11 = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [
        observation("provider-a", "a1"),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
        }),
      ],
    );
    const quasiRollback = evaluateWatcherFinalityV1(
      finalityPolicy,
      finalized,
      pendingW11,
    );

    expect(point.reasonCodes).toContain("post_finality_point_changed");
    expect(content.reasonCodes).toContain("post_finality_content_changed");
    expect(quasiRollback.reasonCodes).toEqual([
      "provider_result_pending",
      "post_finality_contradiction",
    ]);
    expect(point.state?.finalized).toEqual(finalized.finalized);
    expect(content.state?.finalized).toEqual(finalized.finalized);
    expect(quasiRollback.state?.finalized).toEqual(finalized.finalized);
  });

  it("rejects stale policy state, deployment, and release bindings", () => {
    const originalPolicy = policy(3);
    const state = pendingAt(originalPolicy, "1");
    const stalePolicy = policy(4);
    const otherDeployment = policy(3, "99");
    const otherRelease = policy(3, "11", "99");

    expect(
      evaluateWatcherFinalityV1(stalePolicy, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["stale_state"],
      state: null,
    });
    expect(
      evaluateWatcherFinalityV1(otherDeployment, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["deployment_mismatch"],
      state: null,
    });
    expect(
      evaluateWatcherFinalityV1(otherRelease, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["release_evidence_mismatch"],
      state: null,
    });
  });

  it("rejects self-hashed restart states that bypass finality semantics", () => {
    const finalityPolicy = policy(5);
    const finalized = finalizeAtThreshold(policy());
    const impossible = structuredClone(finalized) as Record<string, unknown>;
    const bound = impossible.finalized as Record<string, unknown>;
    bound.firstSeenDepth = "0";
    bound.currentDepth = "0";
    bound.visibilityCount = "1";
    const canonical = { ...impossible };
    delete canonical.stateDigest;
    impossible.stateDigest = createHash("sha256")
      .update(JSON.stringify(canonical), "utf8")
      .digest("hex");

    expect(parseWatcherFinalityStateV1(impossible)).not.toBeNull();
    expect(parseWatcherFinalityStateV1(impossible, finalityPolicy)).toBeNull();
    expect(
      evaluateWatcherFinalityV1(finalityPolicy, impossible, agreement("6")),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["stale_state"],
      state: null,
    });

    const samePolicyImpossible = {
      ...impossible,
      policyDigest: finalityPolicy.policyDigest,
      releaseEvidenceDigest: finalityPolicy.releaseEvidenceDigest,
      deploymentMarker: finalityPolicy.deploymentMarker,
    } as Record<string, unknown>;
    const samePolicyCanonical = {
      ...samePolicyImpossible,
    } as Record<string, unknown>;
    delete samePolicyCanonical.stateDigest;
    samePolicyImpossible.stateDigest = createHash("sha256")
      .update(JSON.stringify(samePolicyCanonical), "utf8")
      .digest("hex");
    expect(
      evaluateWatcherFinalityV1(
        finalityPolicy,
        samePolicyImpossible,
        agreement("6"),
      ),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["invalid_state_semantics"],
      state: null,
    });
  });

  it("is deterministic across provider order, restart, and input key order", () => {
    const finalityPolicy = policy();
    const forwardEvidence = agreement("1");
    const reverseEvidence = agreement("1", {}, true);
    const forward = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      forwardEvidence,
    );
    const reorderedPolicy = Object.fromEntries(
      Object.entries(finalityPolicy).reverse(),
    );
    const reverse = evaluateWatcherFinalityV1(
      reorderedPolicy,
      null,
      reverseEvidence,
    );

    expect(reverseEvidence).toEqual(forwardEvidence);
    expect(reverse).toEqual(forward);
    expect(reverse.resultDigest).toBe(forward.resultDigest);
  });

  it("rejects malformed, unsafe, unknown, and uint64-overflow inputs", () => {
    const finalityPolicy = policy();
    const unsafe = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(unsafe, "status", {
      enumerable: true,
      get: () => {
        throw new Error("not public");
      },
    });
    const unknown = { ...agreement("1"), unknown: true };
    const overflow = structuredClone(agreement("1")) as Record<string, unknown>;
    (overflow.agreement as Record<string, unknown>).minimumDepth =
      "18446744073709551616";
    const withoutDigest = { ...overflow };
    delete withoutDigest.consistencyDigest;
    overflow.consistencyDigest = createHash("sha256")
      .update(JSON.stringify(withoutDigest), "utf8")
      .digest("hex");

    for (const malformed of [unsafe, unknown, overflow, new Error("no")]) {
      expect(
        evaluateWatcherFinalityV1(finalityPolicy, null, malformed),
      ).toMatchObject({
        action: "reject",
        protocolDecision: "quarantined",
        reasonCodes: ["malformed_provider_result"],
        state: { phase: "unobserved" },
      });
    }
  });

  it("uses value-free diagnostics for secret-bearing malformed inputs", () => {
    const secret = "postgres://operator:super-secret@example.invalid/watcher";
    const unsafePolicy = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(unsafePolicy, "releaseEvidenceDigest", {
      enumerable: true,
      get: () => {
        throw new Error(secret);
      },
    });
    const unsafeState = Object.create(null) as Record<string, unknown>;
    Object.defineProperty(unsafeState, "stateDigest", {
      enumerable: true,
      get: () => {
        throw new Error(secret);
      },
    });
    const policyFailure = evaluateWatcherFinalityV1(
      unsafePolicy,
      null,
      new Error(secret),
    );
    const stateFailure = evaluateWatcherFinalityV1(
      policy(),
      unsafeState,
      new Error(secret),
    );

    expect(policyFailure.reasonCodes).toEqual(["malformed_policy"]);
    expect(stateFailure.reasonCodes).toEqual(["malformed_state"]);
    expect(JSON.stringify([policyFailure, stateFailure])).not.toContain(secret);
  });
});
