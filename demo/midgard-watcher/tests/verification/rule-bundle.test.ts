import "node:crypto";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-core/deployment-profile";
import "@al-ft/midgard-core/validation-trace";
import "@lucid-evolution/lucid";
import "vitest";
import "../../src/runtime/deployment-identity.js";
import "../../src/verification/rule-bundle.js";
import "../canonical-fraud-proof-catalogue.js";
import "../support/deployment-authority-fixture.js";
import "./rule-bundle.canonical-manifest-identity.js";
import "./rule-bundle.fixture.js";

import {
  MIDGARD_CONSENSUS_FEATURES,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  computeDeploymentManifestJsonDigest,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { MidgardValidationPhase } from "@al-ft/midgard-core/validation-trace";
import { describe, expect, it } from "vitest";

import {
  computeWatcherRuleBundleCommitment,
  encodeWatcherRuleBundle,
  loadWatcherRuleBundle,
  parseWatcherRuleBundle,
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
} from "../../src/verification/rule-bundle.js";
import {
  h32,
  makeTrustRoot,
  TARGET_PARAMETERS,
} from "./rule-bundle.canonical-manifest-identity.js";
import {
  authorityRejected,
  clone,
  fixture,
  rejected,
} from "./rule-bundle.fixture.js";

describe("watcher canonical V1 rule bundle", () => {
  it("rejects a network different from the compiled deployment profile", () => {
    expect(() => fixture("Custom")).toThrow();
  });

  it.each(["Preprod"] as const)(
    "loads the exact W02-bound profile on %s",
    (network) => {
      const { authority, bundle, verifiedIdentity } = fixture(network);
      const loaded = loadWatcherRuleBundle({
        ...authority,
        ruleBundle: bundle,
      });

      expect(loaded.ruleBundleCommitment).toBe(
        verifiedIdentity.ruleBundleCommitment,
      );
      expect(loaded.ruleBundle.consensusProfileDigest).toBe(
        MIDGARD_CONSENSUS_PROFILE_DIGEST,
      );
      expect(loaded.ruleBundle.features).toEqual(
        MIDGARD_CONSENSUS_FEATURES.map((featureId) => ({
          featureId,
          enabled: true,
        })),
      );
      expect(loaded.ruleBundle.limits).toBe(MIDGARD_CONSENSUS_LIMITS);
      expect(loaded.ruleBundle.targetParameters).toEqual({
        snapshot: TARGET_PARAMETERS,
        digest: computeDeploymentManifestJsonDigest(TARGET_PARAMETERS),
      });
      expect(loaded.ruleBundle.transitionPriority).toBe(
        WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
      );
      expect(loaded.ruleBundle.validation.phasePriority).toBe(
        WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
      );
      expect(loaded.ruleBundle.validation.rejectionSelection).toBe(
        WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
      );
      expect(loaded.ruleBundle.programCommitments).toEqual(
        verifiedIdentity.programCommitments,
      );
      expect(Object.isFrozen(loaded)).toBe(true);
      expect(Object.isFrozen(loaded.ruleBundle)).toBe(true);
      expect(Object.isFrozen(loaded.ruleBundle.features)).toBe(true);
      expect(Object.isFrozen(loaded.ruleBundle.targetParameters.snapshot)).toBe(
        true,
      );
    },
  );

  it("has deterministic bytes and survives exact JSON restart serialization", () => {
    const { authority, bundle, verifiedIdentity } = fixture();
    const firstBytes = encodeWatcherRuleBundle(bundle);
    const restarted = JSON.parse(firstBytes.toString("utf8")) as unknown;
    const loaded = loadWatcherRuleBundle({
      ...clone(authority),
      ruleBundle: restarted,
    });

    expect(encodeWatcherRuleBundle(loaded.ruleBundle)).toEqual(firstBytes);
    expect(computeWatcherRuleBundleCommitment(restarted)).toBe(
      verifiedIdentity.ruleBundleCommitment,
    );
    expect(Object.keys(loaded.ruleBundle.targetParameters.snapshot)).toEqual(
      Object.keys(TARGET_PARAMETERS).sort(),
    );
  });

  it("rejects unknown, adjacent, missing, and extra V1 bundle shapes", () => {
    const { bundle } = fixture();

    const unknownVersion = clone(bundle) as Record<string, unknown>;
    unknownVersion.ruleBundleVersion = 2;
    rejected(
      () => parseWatcherRuleBundle(unknownVersion),
      "unsupported_version",
      "$.ruleBundleVersion",
    );

    const adjacentSchema = clone(bundle) as Record<string, unknown>;
    adjacentSchema.schemaVersion = "midgard-watcher-rule-bundle-v2";
    rejected(
      () => parseWatcherRuleBundle(adjacentSchema),
      "unsupported_version",
      "$.schemaVersion",
    );

    const missing = clone(bundle) as Record<string, unknown>;
    delete missing.validation;
    rejected(
      () => parseWatcherRuleBundle(missing),
      "missing_field",
      "$.validation",
    );

    const extra = clone(bundle) as Record<string, unknown>;
    extra.compatibility = true;
    rejected(
      () => parseWatcherRuleBundle(extra),
      "unknown_field",
      "$.compatibility",
    );

    rejected(() => parseWatcherRuleBundle([bundle]), "invalid_field", "$");
  });

  it("rejects every feature-set weakening, extension, duplication, or reordering", () => {
    const { bundle } = fixture();

    const disabled = clone(bundle);
    disabled.features[0]!.enabled = false as true;
    rejected(
      () => parseWatcherRuleBundle(disabled),
      "disabled_feature",
      "$.features[0].enabled",
    );

    const unknown = clone(bundle);
    unknown.features[0]!.featureId =
      "watcher_only_feature" as (typeof unknown.features)[number]["featureId"];
    rejected(
      () => parseWatcherRuleBundle(unknown),
      "unknown_feature",
      "$.features[0].featureId",
    );

    const missing = clone(bundle);
    missing.features.pop();
    rejected(
      () => parseWatcherRuleBundle(missing),
      "feature_set_mismatch",
      "$.features",
    );

    const duplicate = clone(bundle);
    duplicate.features[1] = clone(duplicate.features[0]!);
    rejected(
      () => parseWatcherRuleBundle(duplicate),
      "feature_set_mismatch",
      "$.features",
    );

    const extra = clone(bundle);
    extra.features.push(clone(extra.features[0]!));
    rejected(
      () => parseWatcherRuleBundle(extra),
      "feature_set_mismatch",
      "$.features",
    );

    const reordered = clone(bundle);
    [reordered.features[0], reordered.features[1]] = [
      reordered.features[1]!,
      reordered.features[0]!,
    ];
    rejected(
      () => parseWatcherRuleBundle(reordered),
      "feature_set_mismatch",
      "$.features",
    );
  });

  it("rejects profile, limit, target-parameter, transition, and validation drift", () => {
    const { bundle } = fixture();

    const profile = clone(bundle);
    profile.consensusProfileDigest = h32("a");
    rejected(
      () => parseWatcherRuleBundle(profile),
      "consensus_profile_mismatch",
      "$.consensusProfileDigest",
    );

    const limit = clone(bundle);
    (limit.limits as Record<string, number>).maxL2TransactionCount += 1;
    rejected(
      () => parseWatcherRuleBundle(limit),
      "consensus_profile_mismatch",
      "$.limits.maxL2TransactionCount",
    );

    const target = clone(bundle);
    (target.targetParameters.snapshot as Record<string, unknown>).minFeeA = 45;
    rejected(
      () => parseWatcherRuleBundle(target),
      "target_parameters_mismatch",
      "$.targetParameters.digest",
    );

    const transition = clone(bundle);
    [transition.transitionPriority[0], transition.transitionPriority[1]] = [
      transition.transitionPriority[1]!,
      transition.transitionPriority[0]!,
    ];
    rejected(
      () => parseWatcherRuleBundle(transition),
      "transition_priority_mismatch",
      "$.transitionPriority",
    );

    const validation = clone(bundle);
    [
      validation.validation.phasePriority[0],
      validation.validation.phasePriority[1],
    ] = [
      validation.validation.phasePriority[1]!,
      validation.validation.phasePriority[0]!,
    ];
    rejected(
      () => parseWatcherRuleBundle(validation),
      "validation_priority_mismatch",
      "$.validation.phasePriority",
    );

    const selection = clone(bundle);
    selection.validation.rejectionSelection =
      "watcher_first_observed_rejection_v1" as typeof selection.validation.rejectionSelection;
    rejected(
      () => parseWatcherRuleBundle(selection),
      "validation_priority_mismatch",
      "$.validation.rejectionSelection",
    );
  });

  it("rejects program and rule-bundle commitment drift independently", () => {
    const { authority, bundle } = fixture();

    const program = clone(bundle);
    program.programCommitments["validation-machine-v1"] = h32("a");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: program,
        }),
      "program_commitment_mismatch",
      "$.ruleBundle.programCommitments",
    );

    const missingProgram = clone(bundle);
    delete missingProgram.programCommitments["validation-machine-v1"];
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: missingProgram,
        }),
      "program_commitment_mismatch",
      "$.ruleBundle.programCommitments",
    );

    const extraProgram = clone(bundle);
    extraProgram.programCommitments["watcher-folklore-v1"] = h32("b");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: extraProgram,
        }),
      "program_commitment_mismatch",
      "$.ruleBundle.programCommitments",
    );

    const content = clone(bundle);
    (content.targetParameters.snapshot as Record<string, unknown>).minFeeA = 45;
    content.targetParameters.digest = computeDeploymentManifestJsonDigest(
      content.targetParameters.snapshot,
    );
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: content,
        }),
      "rule_bundle_commitment_mismatch",
      "$.ruleBundle",
    );
  });

  it("rejects deployment, release, network, and durable-marker cross-binding drift", () => {
    const { authority, bundle } = fixture();

    const deployment = clone(bundle);
    deployment.deploymentManifestId = h32("a");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: deployment,
        }),
      "deployment_identity_mismatch",
      "$.ruleBundle",
    );

    const network = clone(bundle);
    network.network = "Preview";
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: network,
        }),
      "deployment_identity_mismatch",
      "$.ruleBundle",
    );

    const release = clone(bundle);
    release.blueprintHash = h32("b");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: release,
        }),
      "deployment_identity_mismatch",
      "$.ruleBundle",
    );

    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          durableMarker: makeDeploymentMarker(h32("c")),
          ruleBundle: bundle,
        }),
      "durable_marker_mismatch",
      "$.durableMarker",
    );
  });

  it("requires raw signed W02 authority and rejects forged summaries, signatures, policies, and trust roots", () => {
    const { authority, bundle, verifiedIdentity } = fixture();

    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          signedIdentity: verifiedIdentity,
          ruleBundle: bundle,
        }),
      "unknown_field",
      "$.manifestId",
    );

    const invalidSignature = clone(authority);
    const signature = invalidSignature.signedIdentity.attestation
      .signature as string;
    invalidSignature.signedIdentity.attestation.signature = `${signature.startsWith("0") ? "1" : "0"}${signature.slice(1)}`;
    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...invalidSignature,
          ruleBundle: bundle,
        }),
      "invalid_signature",
      "$.attestation.signature",
    );

    const mismatchedPolicy = clone(authority.policy);
    mismatchedPolicy.ruleBundleCommitment = h32("a");
    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          policy: mismatchedPolicy,
          ruleBundle: bundle,
        }),
      "mismatched_identity",
      "$.releaseBindings",
    );

    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          trustRoots: [makeTrustRoot().trustRoot],
          ruleBundle: bundle,
        }),
      "untrusted_signer",
      "$.attestation.trustRootId",
    );
  });

  it("derives the validation priority from the production canonical phase codes", () => {
    const order = Object.entries(MidgardValidationPhase)
      .sort((left, right) => left[1] - right[1])
      .map(([phase]) => phase);
    expect(WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY).toEqual(order);
    expect(order).toEqual([
      "canonicalDecode",
      "compactBinding",
      "staticLedgerRules",
      "inputSets",
      "signatures",
      "phaseANativeScripts",
      "phaseAScriptPreconditions",
      "resolveInputs",
      "scriptSources",
      "nativeScripts",
      "scriptIntegrity",
      "cek",
      "valueAndMint",
      "ledgerDelta",
      "terminal",
    ]);
  });
});
