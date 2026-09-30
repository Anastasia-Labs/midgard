import {
  makeDeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
  WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
  WATCHER_FINALITY_STATE_SCHEMA_VERSION,
  type WatcherFinalityState,
} from "../../src/l1/finality-engine.js";
import { sha256Canonical as sha256CanonicalForTest } from "../support/canonical-json.js";
import {
  config,
  deploymentIdentity,
  externalEndpoints,
  externalSource,
  hex32,
  policy,
  reorderObjectKeysForTest,
} from "./finality-engine.config.js";
import {
  agreement,
  evaluateWatcherMultiProviderConsistency,
  finalizeAtThreshold,
  observation,
  pendingAt,
  policyOverProviders,
  rebindConsistencyToPolicy,
} from "./finality-engine.policy-over-providers.js";

describe("canonical release-bound watcher finality", () => {
  it("binds W01 finality to the verified release and deployment marker", () => {
    const value = policy();

    expect(value).toMatchObject({
      schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
      network: "Preprod",
      sourceMode: "external_providers",
      authorityNodeId: null,
      authorityGenesisIdentitySha256: null,
      authorityChainSyncSocketPath: null,
      externalProviders: [
        {
          providerId: "provider-a",
          operatorIdentitySha256: hex32("a1"),
          endpoint: externalEndpoints.get("provider-a:a1:a1"),
          authenticationKind: "https_tls_identity_v1",
        },
        {
          providerId: "provider-b",
          operatorIdentitySha256: hex32("b2"),
          endpoint: externalEndpoints.get("provider-b:b2:b2"),
          authenticationKind: "https_tls_identity_v1",
        },
      ],
      confirmationDepth: "3",
      maximumPreFinalityRollbackDepth: "3",
      maximumPostFinalityRecoveryDepth: "2160",
      beforeFinalityRollback: "rewind",
      afterFinalityRollback: "quarantine",
      blueprintHash: hex32("22"),
      deploymentMarker: {
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
        manifestId: hex32("11"),
      },
    });
    expect(value.policyDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(parseWatcherFinalityPolicy(value)).toEqual(value);
    expect(Object.isFrozen(value)).toBe(true);
    expect(
      parseWatcherFinalityPolicy({
        ...value,
        maximumPostFinalityRecoveryDepth: "2161",
      }),
    ).toBeNull();
    const { policyDigest: _policyDigest, ...narrowedRecoveryPolicyCanonical } =
      {
        ...value,
        maximumPostFinalityRecoveryDepth: "2159",
      };
    expect(
      parseWatcherFinalityPolicy({
        ...narrowedRecoveryPolicyCanonical,
        policyDigest: sha256CanonicalForTest(narrowedRecoveryPolicyCanonical),
      }),
    ).toBeNull();

    const changedEndpointConfig = config();
    changedEndpointConfig.l1.source.providers[0]!.endpoint =
      "https://cardano-a-new.example";
    const changedEndpointPolicy = makeWatcherFinalityPolicy(
      changedEndpointConfig,
      deploymentIdentity(),
    );
    expect(changedEndpointPolicy).not.toBeNull();
    expect(changedEndpointPolicy?.policyDigest).not.toBe(value.policyDigest);

    const reorderedConfig = config();
    reorderedConfig.l1.source.providers.reverse();
    expect(
      makeWatcherFinalityPolicy(reorderedConfig, deploymentIdentity()),
    ).toEqual(value);

    const unauthenticatedTransportConfig = config();
    unauthenticatedTransportConfig.l1.source.providers[0]!.endpoint =
      "http://127.0.0.1:1442";
    expect(
      makeWatcherFinalityPolicy(
        unauthenticatedTransportConfig,
        deploymentIdentity(),
      ),
    ).toBeNull();
  });

  it("derives the local-node finality authority and Kupmios bindings from W01 config", () => {
    const base = config();
    const value = makeWatcherFinalityPolicy(
      {
        ...base,
        l1: {
          ...base.l1,
          source: {
            sourceMode: "local_node",
            authorityNodeId: "watcher-node",
            chainSync: {
              kind: "cardano_node_socket",
              socketPath: "/run/cardano/node.socket",
              nodeConfigPath: "/etc/cardano/node-config.json",
              genesisConfigPath: "/etc/cardano/shelley-genesis.json",
              genesisIdentitySha256: hex32("66"),
            },
            queryServices: [
              {
                kind: "ogmios",
                identity: "local-ogmios",
                endpoint: "ws://127.0.0.1:1337",
              },
              {
                kind: "kupo",
                identity: "local-kupo",
                endpoint: "http://127.0.0.1:1442",
              },
            ],
          },
        },
      },
      deploymentIdentity(),
    );

    expect(value).toMatchObject({
      sourceMode: "local_node",
      authorityNodeId: "watcher-node",
      authorityGenesisIdentitySha256: hex32("66"),
      authorityChainSyncSocketPath: "/run/cardano/node.socket",
      externalProviders: null,
      localQueryServices: [
        {
          kind: "kupo",
          providerId: "local-kupo",
          endpoint: "http://127.0.0.1:1442",
        },
        {
          kind: "ogmios",
          providerId: "local-ogmios",
          endpoint: "ws://127.0.0.1:1337",
        },
      ],
    });
    expect(parseWatcherFinalityPolicy(value)).toEqual(value);
  });

  it("rejects configuration/deployment mismatches without emitting values", () => {
    const wrongNetwork = makeWatcherFinalityPolicy(
      config(),
      deploymentIdentity("11", "22", "Preview"),
    );
    const malformedMarker = {
      ...deploymentIdentity(),
      durableMarker: makeDeploymentMarker(hex32("99")),
    };

    expect(wrongNetwork).toBeNull();
    expect(makeWatcherFinalityPolicy(config(), malformedMarker)).toBeNull();
  });

  it("rejects agreement from two distinct providers outside the W01 allowlist", () => {
    const finalityPolicy = policy();
    const hostileAgreement = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [
        observation("provider-x", "c3", {
          operatorIdentityByte: "d4",
        }),
        observation("provider-y", "e5", {
          operatorIdentityByte: "f6",
        }),
      ],
    );

    expect(hostileAgreement.status).toBe("quarantined");
    expect(
      evaluateWatcherFinality(finalityPolicy, null, hostileAgreement),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: { phase: "unobserved" },
    });
  });

  it("rejects configured provider labels with substituted operator identities", () => {
    const hostileAgreement = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [
        observation("provider-a", "c3", {
          operatorIdentityByte: "d4",
        }),
        observation("provider-b", "e5", {
          operatorIdentityByte: "f6",
        }),
      ],
    );

    expect(
      evaluateWatcherFinality(policy(), null, hostileAgreement),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
    });
  });

  it("rejects an otherwise valid W11 agreement bound to another configured provider set", () => {
    const foreign = evaluateWatcherMultiProviderConsistency(
      {
        sourceMode: "external_providers",
        network: "Preprod",
        providers: [
          {
            providerId: "provider-c",
            operatorIdentitySha256: hex32("c3"),
            endpoint: externalEndpoints.get("provider-c:c3:c3"),
          },
          {
            providerId: "provider-d",
            operatorIdentitySha256: hex32("d4"),
            endpoint: externalEndpoints.get("provider-d:d4:d4"),
          },
        ],
      },
      [
        observation("provider-c", "c3", { depth: "3" }),
        observation("provider-d", "d4", { depth: "3" }),
      ],
    );
    expect(foreign.status).toBe("agreed");
    expect(evaluateWatcherFinality(policy(), null, foreign)).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_authority_mismatch"],
    });
  });

  it("refuses finality while a configured external provider is unbound", () => {
    // provider-c is in the W01 allowlist and never observed. The W11 record is
    // a genuine two-provider agreement over provider-a/provider-b; every
    // binding it carries matches the policy, so `.every` over the binding list
    // is true and used to say nothing at all about provider-c.
    const finalityPolicy = policyOverProviders([
      ["provider-a", "a1"],
      ["provider-b", "b2"],
      ["provider-c", "c3"],
    ]);
    const belowThreshold = rebindConsistencyToPolicy(
      agreement("2"),
      finalityPolicy,
    );
    const atThreshold = rebindConsistencyToPolicy(
      agreement("3"),
      finalityPolicy,
    );
    expect(belowThreshold.status).toBe("agreed");
    expect((belowThreshold.externalProviderBindings as unknown[]).length).toBe(
      2,
    );
    expect(finalityPolicy.externalProviders).toHaveLength(3);

    const first = evaluateWatcherFinality(finalityPolicy, null, belowThreshold);
    const second = evaluateWatcherFinality(
      finalityPolicy,
      first.state,
      atThreshold,
    );

    expect(second.protocolDecision).toBe("quarantined");
    expect(second.action).toBe("reject");
    expect(second.reasonCodes).toEqual(["source_provider_binding_unrun"]);
    expect(first).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_provider_binding_unrun"],
      alertCodes: [
        "watcher_finality_input_rejected",
        "watcher_finality_configuration_mismatch",
      ],
    });
    expect(first.state?.phase).toBe("unobserved");
    expect(second.state?.phase).toBe("unobserved");
  });

  it("refuses finality when only part of the configured provider set ran", () => {
    // Half the allowlist - provider-c and provider-d - never ran, so the
    // operator, TLS-identity, and endpoint binding for both is unevaluated.
    const finalityPolicy = policyOverProviders([
      ["provider-a", "a1"],
      ["provider-b", "b2"],
      ["provider-c", "c3"],
      ["provider-d", "d4"],
    ]);
    const partial = rebindConsistencyToPolicy(agreement("8"), finalityPolicy);

    expect(finalityPolicy.externalProviders).toHaveLength(4);
    expect(
      evaluateWatcherFinality(finalityPolicy, null, partial),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["source_provider_binding_unrun"],
    });
  });

  it("grants finality when every configured external provider is bound", () => {
    const finalityPolicy = policyOverProviders([
      ["provider-a", "a1"],
      ["provider-b", "b2"],
    ]);
    const belowThreshold = agreement("2");
    const atThreshold = agreement("3");

    // The re-stamp is the identity function on a record the policy already
    // matches: the rejections above differ only in the configured provider set.
    expect(rebindConsistencyToPolicy(atThreshold, finalityPolicy)).toEqual(
      atThreshold,
    );
    expect(finalityPolicy.policyDigest).toBe(policy().policyDigest);

    const first = evaluateWatcherFinality(finalityPolicy, null, belowThreshold);
    const second = evaluateWatcherFinality(
      finalityPolicy,
      first.state,
      atThreshold,
    );

    expect(first.action).toBe("observe_pending");
    expect(second).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      reasonCodes: ["confirmation_depth_reached"],
      alertCodes: [],
      state: { phase: "finalized" },
    });
  });

  it.each(["3", "8", "2161"])(
    "keeps first visibility pending at depth %s and requires real progress",
    (depth) => {
      const finalityPolicy = policy();
      const firstAgreement = agreement(depth);
      const first = evaluateWatcherFinality(
        finalityPolicy,
        null,
        firstAgreement,
      );

      expect(first).toMatchObject({
        schemaVersion: WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
        action: "observe_pending",
        protocolDecision: "hold",
        reasonCodes: ["first_visibility_pending"],
        alertCodes: ["watcher_finality_pending"],
        state: {
          schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
          phase: "pending",
          pending: {
            firstSeenDepth: depth,
            currentDepth: depth,
            firstSeenConsistencyDigest: firstAgreement.consistencyDigest,
            lastSeenConsistencyDigest: firstAgreement.consistencyDigest,
            visibilityCount: "1",
          },
        },
      });
      expect(first.resultDigest).toMatch(/^[0-9a-f]{64}$/u);
      const restored = parseWatcherFinalityState(
        JSON.parse(JSON.stringify(first.state)),
        finalityPolicy,
      );
      expect(restored).toEqual(first.state);
      expect(restored).not.toBeNull();
      expect(
        evaluateWatcherFinality(finalityPolicy, restored, firstAgreement),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "hold",
        reasonCodes: ["duplicate_observation"],
        state: restored,
      });
      const greaterDepth = (BigInt(depth) + 1n).toString();
      const sameMinimum = evaluateWatcherMultiProviderConsistency(
        externalSource(),
        [
          observation("provider-a", "a1", { depth }),
          observation("provider-b", "b2", { depth: greaterDepth }),
        ],
      );
      expect(sameMinimum.status).toBe("agreed");
      expect(sameMinimum.agreement?.minimumDepth).toBe(depth);
      expect(
        evaluateWatcherFinality(finalityPolicy, restored, sameMinimum),
      ).toMatchObject({
        action: "reject",
        protocolDecision: "hold",
        reasonCodes: ["stale_observation"],
        state: restored,
      });
      const confirmed = evaluateWatcherFinality(
        finalityPolicy,
        restored,
        agreement(greaterDepth),
      );
      expect(confirmed).toMatchObject({
        action: "finalize",
        protocolDecision: "finality_granted",
        state: {
          phase: "finalized",
          pending: null,
          finalized: {
            firstSeenDepth: depth,
            currentDepth: greaterDepth,
            visibilityCount: "2",
          },
        },
      });
      expect(
        parseWatcherFinalityState(confirmed.state, finalityPolicy),
      ).toEqual(confirmed.state);
    },
  );

  it.each([
    { firstSeenDepth: "7" },
    { lastSeenConsistencyDigest: hex32("ff") },
    { visibilityCount: "2" },
    { firstSeenDepth: "7", visibilityCount: "2" },
  ])("rejects impossible pending visibility fields %j", (changes) => {
    const finalityPolicy = policy();
    const state = pendingAt(finalityPolicy, "8");
    const { stateDigest: _, ...content } = state;
    const canonical = {
      ...content,
      pending: { ...state.pending!, ...changes },
    };
    const malformed = {
      ...canonical,
      stateDigest: sha256CanonicalForTest(canonical),
    };
    expect(parseWatcherFinalityState(malformed)).not.toBeNull();
    expect(parseWatcherFinalityState(malformed, finalityPolicy)).toBeNull();
    expect(
      evaluateWatcherFinality(finalityPolicy, malformed, agreement("9")),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["invalid_state_semantics"],
      state: null,
    });
  });

  it("finalizes threshold-1 to threshold exactly once", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "2");
    const finalized = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      agreement("3"),
    );
    const later = evaluateWatcherFinality(
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
    const first = evaluateWatcherFinality(finalityPolicy, null, evidence);
    const restarted = JSON.parse(
      JSON.stringify(first.state),
    ) as WatcherFinalityState;
    const duplicate = evaluateWatcherFinality(
      finalityPolicy,
      restarted,
      evidence,
    );

    expect(parseWatcherFinalityState(restarted)).toEqual(first.state);
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
    const advanced = evaluateWatcherFinality(
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
    const rewound = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      agreement("2"),
    );
    const replay = evaluateWatcherFinality(
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
    const fork = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinality(
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
    const exactDepth = evaluateWatcherFinality(
      finalityPolicy,
      depthTwo,
      agreement("0"),
    );
    const depthThree = pendingAt(finalityPolicy, "3");
    const adjacentExcess = evaluateWatcherFinality(
      finalityPolicy,
      depthThree,
      agreement("0"),
    );
    const exactFork = evaluateWatcherFinality(
      finalityPolicy,
      pendingAt(finalityPolicy, "1"),
      agreement("2", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const excessiveFork = evaluateWatcherFinality(
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

  it("fails closed when same-depth evidence arrives over a substituted endpoint", () => {
    const finalityPolicy = policy();
    const pending = pendingAt(finalityPolicy, "1");
    const freshTransportAgreement = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [
        observation("provider-a", "c3", {
          depth: "1",
          operatorIdentityByte: "a1",
        }),
        observation("provider-b", "b2", { depth: "1" }),
      ],
    );
    const stale = evaluateWatcherFinality(
      finalityPolicy,
      pending,
      freshTransportAgreement,
    );

    expect(stale).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      state: pending,
    });
  });

  it("rejects W11 pending and quarantine without advancing state", () => {
    const finalityPolicy = policy();
    const initialPending = evaluateWatcherMultiProviderConsistency(
      externalSource(),
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
    const quarantined = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [observation("provider-a", "a1")],
    );

    expect(
      evaluateWatcherFinality(finalityPolicy, null, initialPending),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_pending"],
      state: { phase: "unobserved" },
    });
    expect(
      evaluateWatcherFinality(finalityPolicy, null, quarantined),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_quarantined"],
      state: { phase: "unobserved" },
    });
  });

  it("holds same-point post-finality depth regression without an incident and resumes on recovered depth", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const rolledBack = evaluateWatcherFinality(
      finalityPolicy,
      finalized,
      agreement("2"),
    );

    expect(rolledBack).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_depth_regression"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: finalized,
    });
    expect(rolledBack.state?.phase).toBe("finalized");
    expect(rolledBack.state?.incident).toBeNull();
    expect(
      evaluateWatcherFinality(finalityPolicy, rolledBack.state, agreement("4")),
    ).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["already_finalized"],
      state: finalized,
    });
  });

  it("opens incidents only for agreed point replacement and keeps same-point content or transient disagreement nonterminal", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const point = evaluateWatcherFinality(
      finalityPolicy,
      finalized,
      agreement("4", {
        blockHash: hex32("bb"),
        slot: "1001",
        blockNo: "101",
      }),
    );
    const content = evaluateWatcherFinality(
      finalityPolicy,
      finalized,
      agreement("4", { bodyHex: "a100" }),
    );
    const pendingW11 = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [
        observation("provider-a", "a1"),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
        }),
      ],
    );
    const quasiRollback = evaluateWatcherFinality(
      finalityPolicy,
      finalized,
      pendingW11,
    );

    expect(point.reasonCodes).toContain("post_finality_point_changed");
    expect(content).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_content_changed"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: finalized,
    });
    expect(quasiRollback).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["provider_result_pending"],
      alertCodes: ["watcher_finality_input_rejected"],
      state: finalized,
    });
    expect(point.state?.finalized).toEqual(finalized.finalized);
    expect(content.state).toEqual(finalized);
    expect(quasiRollback.state).toEqual(finalized);
    expect(
      evaluateWatcherFinality(
        finalityPolicy,
        quasiRollback.state,
        agreement("4"),
      ),
    ).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["already_finalized"],
      state: finalized,
    });
  });

  it("keeps malformed input and bounded external lag transient after finality", () => {
    const finalityPolicy = policy();
    const finalized = finalizeAtThreshold(finalityPolicy);
    const boundedLag = evaluateWatcherMultiProviderConsistency(
      externalSource(),
      [
        observation("provider-a", "a1", { depth: "4" }),
        observation("provider-b", "b2", {
          blockHash: hex32("bb"),
          slot: "1001",
          blockNo: "101",
          depth: "0",
        }),
      ],
    );
    expect(boundedLag).toMatchObject({
      status: "pending",
      reasonCodes: ["bounded_provider_lag"],
    });
    for (const transient of [new Error("malformed"), boundedLag]) {
      const held = evaluateWatcherFinality(
        finalityPolicy,
        finalized,
        transient,
      );
      expect(held).toMatchObject({
        action: "reject",
        protocolDecision: "quarantined",
        state: finalized,
      });
      expect(held.state?.phase).toBe("finalized");
      expect(held.state?.incident).toBeNull();
      expect(
        evaluateWatcherFinality(finalityPolicy, held.state, agreement("4")),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "hold",
        state: finalized,
      });
    }
  });

  it("rejects stale policy state, deployment, and release bindings", () => {
    const originalPolicy = policy(3);
    const state = pendingAt(originalPolicy, "1");
    const stalePolicy = policy(4);
    const otherDeployment = policy(3, "99");
    const otherRelease = policy(3, "11", "99");

    expect(
      evaluateWatcherFinality(stalePolicy, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["stale_state"],
      state: null,
    });
    expect(
      evaluateWatcherFinality(otherDeployment, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["deployment_mismatch"],
      state: null,
    });
    expect(
      evaluateWatcherFinality(otherRelease, state, agreement("2")),
    ).toMatchObject({
      reasonCodes: ["blueprint_mismatch"],
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
    impossible.stateDigest = sha256CanonicalForTest(canonical);

    expect(parseWatcherFinalityState(impossible)).not.toBeNull();
    expect(parseWatcherFinalityState(impossible, finalityPolicy)).toBeNull();
    expect(
      evaluateWatcherFinality(finalityPolicy, impossible, agreement("6")),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["stale_state"],
      state: null,
    });

    const samePolicyImpossible = {
      ...impossible,
      policyDigest: finalityPolicy.policyDigest,
      blueprintHash: finalityPolicy.blueprintHash,
      deploymentMarker: finalityPolicy.deploymentMarker,
    } as Record<string, unknown>;
    const samePolicyCanonical = {
      ...samePolicyImpossible,
    } as Record<string, unknown>;
    delete samePolicyCanonical.stateDigest;
    samePolicyImpossible.stateDigest =
      sha256CanonicalForTest(samePolicyCanonical);
    expect(
      evaluateWatcherFinality(
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
    const forward = evaluateWatcherFinality(
      finalityPolicy,
      null,
      forwardEvidence,
    );
    const reverse = evaluateWatcherFinality(
      reorderObjectKeysForTest(finalityPolicy),
      null,
      reorderObjectKeysForTest(reverseEvidence),
    );

    expect(reverseEvidence).toEqual(forwardEvidence);
    expect(reverse).toEqual(forward);
    expect(reverse.resultDigest).toBe(forward.resultDigest);
    const { policyDigest, ...policyCanonical } = finalityPolicy;
    expect(policyDigest).toBe(sha256CanonicalForTest(policyCanonical));
    const reversedProviders = {
      ...policyCanonical,
      externalProviders: [...policyCanonical.externalProviders!].reverse(),
    };
    expect(sha256CanonicalForTest(reversedProviders)).not.toBe(policyDigest);
    const { stateDigest, ...stateCanonical } = forward.state!;
    expect(stateDigest).toBe(sha256CanonicalForTest(stateCanonical));
    const { resultDigest, ...resultCanonical } = forward;
    expect(resultDigest).toBe(sha256CanonicalForTest(resultCanonical));
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
    overflow.consistencyDigest = sha256CanonicalForTest(withoutDigest);
    const arrayOrder = structuredClone(agreement("1")) as Record<
      string,
      unknown
    >;
    arrayOrder.observationEvidenceDigests = [
      ...(arrayOrder.observationEvidenceDigests as string[]),
    ].reverse();
    const unsupportedBigInt = structuredClone(agreement("1")) as Record<
      string,
      unknown
    >;
    (unsupportedBigInt.agreement as Record<string, unknown>).minimumDepth = 1n;
    const unsupportedDate = structuredClone(agreement("1")) as Record<
      string,
      unknown
    >;
    (unsupportedDate.agreement as Record<string, unknown>).minimumDepth =
      new Date(0);
    const cycle = structuredClone(agreement("1")) as Record<string, unknown>;
    cycle.agreement = cycle;

    for (const malformed of [
      unsafe,
      unknown,
      overflow,
      arrayOrder,
      unsupportedBigInt,
      unsupportedDate,
      cycle,
      new Error("no"),
    ]) {
      expect(
        evaluateWatcherFinality(finalityPolicy, null, malformed),
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
    Object.defineProperty(unsafePolicy, "blueprintHash", {
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
    const policyFailure = evaluateWatcherFinality(
      unsafePolicy,
      null,
      new Error(secret),
    );
    const stateFailure = evaluateWatcherFinality(
      policy(),
      unsafeState,
      new Error(secret),
    );

    expect(policyFailure.reasonCodes).toEqual(["malformed_policy"]);
    expect(stateFailure.reasonCodes).toEqual(["malformed_state"]);
    expect(JSON.stringify([policyFailure, stateFailure])).not.toContain(secret);
  });
});
