import "node:crypto";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-core/deployment-profile";
import "@lucid-evolution/lucid";
import "vitest";
import "../../src/runtime/deployment-identity.js";
import "../canonical-fraud-proof-catalogue.js";
import "../support/deployment-authority-fixture.js";
import "./deployment-identity.canonical-identity.js";
import "./deployment-identity.make-fixture.js";

import {
  computeDeploymentManifestId,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  daBondManifestAmounts,
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { RELEASE_L1_FINALITY_POLICY } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  assertWatcherDeploymentAvailabilityChallengeAuthority,
  assertWatcherDeploymentProtocolParameterAuthority,
  assertWatcherDeploymentProtocolScriptAuthority,
  verifyWatcherDeploymentIdentity,
  watcherDeploymentAvailabilityChallengeAuthority,
  watcherDeploymentIdentityDiagnostic,
  WatcherDeploymentIdentityError,
  watcherDeploymentProtocolParameterAuthority,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseEconomicsAuthority,
  watcherDeploymentReleaseFinalityAuthority,
  watcherDeploymentReleaseFinalityPolicy,
} from "../../src/runtime/deployment-identity.js";
import {
  BLUEPRINT_HASH,
  makeTrustRoot,
  type MutableRecord,
  NATIVE_SCRIPT_HASH,
  RULE_BUNDLE_COMMITMENT,
} from "./deployment-identity.canonical-identity.js";
import { makeFixture, rejection } from "./deployment-identity.make-fixture.js";

describe("watcher deployment identity", () => {
  it("authenticates the funding bundle digest with the deployment signature", () => {
    const fixture = makeFixture();
    fixture.signedIdentity.releaseBindings.fundingProfileBundleDigest =
      "cd".repeat(32);
    rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "invalid_signature",
      "$.attestation.signature",
    );
    fixture.resign();
    rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "mismatched_identity",
      "$.releaseBindings.fundingProfileBundleDigest",
    );
  });

  it.each(["policy", "releaseBindings"] as const)(
    "requires the funding bundle digest in %s",
    (location) => {
      const fixture = makeFixture();
      const input = {
        signedIdentity: fixture.signedIdentity,
        policy: { ...fixture.policy },
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      };
      const record: MutableRecord =
        location === "policy"
          ? input.policy
          : input.signedIdentity.releaseBindings;
      delete record.fundingProfileBundleDigest;
      expect(() => verifyWatcherDeploymentIdentity(input)).toThrow(
        WatcherDeploymentIdentityError,
      );
    },
  );

  it("verifies the exact signed deployment identity and durable marker", () => {
    const fixture = makeFixture();

    expect(
      verifyWatcherDeploymentIdentity({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      }),
    ).toEqual({
      manifestId: fixture.signedIdentity.manifest.manifestId,
      network: "Preprod",
      trustRootId: fixture.trustRoot.trustRootId,
      blueprintHash: BLUEPRINT_HASH,
      fundingProfileBundleDigest: "ab".repeat(32),
      ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
      programCommitments:
        fixture.signedIdentity.releaseBindings.programCommitments,
      durableMarker: fixture.durableMarker,
    });
  });

  it("mints an opaque deployment-bound state-queue and CorrectionLock script authority", () => {
    const fixture = makeFixture();
    const identity = verifyWatcherDeploymentIdentity({
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: [fixture.trustRoot],
      durableMarker: fixture.durableMarker,
    });
    const authority = watcherDeploymentProtocolScriptAuthority(identity);

    expect(authority).toMatchObject({
      deploymentFingerprint: identity.manifestId,
      network: "Preprod",
      hubOracleOneShotOutRef: fixture.policy.hubOracleOneShotOutRef,
      protocolScriptHashes: {
        hubOracleMint: fixture.policy.appliedScriptHashes.hubOracleMint,
        referenceScriptAuthMint:
          fixture.policy.appliedScriptHashes.referenceScriptAuthMint,
        availabilityChallengeSpend:
          fixture.policy.appliedScriptHashes.availabilityChallengeSpend,
        availabilityChallengeMint:
          fixture.policy.appliedScriptHashes.availabilityChallengeMint,
        daBondPoolSpend: fixture.policy.appliedScriptHashes.daBondPoolSpend,
        daBondPoolMint: fixture.policy.appliedScriptHashes.daBondPoolMint,
        daAttestationMint: fixture.policy.appliedScriptHashes.daAttestationMint,
        availabilityChallengeOpenWithdraw:
          fixture.policy.appliedScriptHashes.availabilityChallengeOpenWithdraw,
        availabilityChallengeSettleWithdraw:
          fixture.policy.appliedScriptHashes
            .availabilityChallengeSettleWithdraw,
        availabilityChallengeCloseWithdraw:
          fixture.policy.appliedScriptHashes.availabilityChallengeCloseWithdraw,
        availabilityChallengeTimeoutWithdraw:
          fixture.policy.appliedScriptHashes
            .availabilityChallengeTimeoutWithdraw,
        stateQueueSpend: fixture.policy.appliedScriptHashes.stateQueueSpend,
        stateQueueMint: fixture.policy.appliedScriptHashes.stateQueueMint,
        correctionLockSpend:
          fixture.policy.appliedScriptHashes.correctionLockSpend,
      },
      referenceScripts: fixture.policy.referenceScripts,
      authorityDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(Object.isFrozen(authority)).toBe(true);
    expect(Object.isFrozen(authority.protocolScriptHashes)).toBe(true);
    expect(Object.isFrozen(authority.referenceScripts)).toBe(true);
    expect(() =>
      assertWatcherDeploymentProtocolScriptAuthority(authority),
    ).not.toThrow();
    expect(() =>
      assertWatcherDeploymentProtocolScriptAuthority({
        ...authority,
      }),
    ).toThrow("invalid_field");
    expect(() =>
      watcherDeploymentProtocolScriptAuthority({ ...identity }),
    ).toThrow("invalid_field");
  });

  it("mints exact protocol parameters only from the signed deployment identity", () => {
    const fixture = makeFixture();
    const identity = verifyWatcherDeploymentIdentity({
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: [fixture.trustRoot],
      durableMarker: fixture.durableMarker,
    });
    const authority = watcherDeploymentProtocolParameterAuthority(identity);

    expect(authority).toEqual({
      schemaVersion:
        "midgard-watcher-deployment-protocol-parameter-authority-v1",
      deploymentFingerprint: identity.manifestId,
      snapshot:
        fixture.signedIdentity.manifest.cardanoProtocolParameters.snapshot,
      snapshotDigest:
        fixture.signedIdentity.manifest.cardanoProtocolParameters.digest,
      authorityDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(Object.isFrozen(authority)).toBe(true);
    expect(Object.isFrozen(authority.snapshot)).toBe(true);
    expect(authority.snapshot).not.toBe(
      fixture.signedIdentity.manifest.cardanoProtocolParameters.snapshot,
    );
    const maxTxSize = authority.snapshot.maxTxSize;
    fixture.signedIdentity.manifest.cardanoProtocolParameters.snapshot.maxTxSize =
      "1";
    expect(authority.snapshot.maxTxSize).toBe(maxTxSize);
    expect(() =>
      assertWatcherDeploymentProtocolParameterAuthority(authority),
    ).not.toThrow();
    expect(() =>
      assertWatcherDeploymentProtocolParameterAuthority({ ...authority }),
    ).toThrow("invalid_field");
    expect(() =>
      watcherDeploymentProtocolParameterAuthority({ ...identity }),
    ).toThrow("invalid_field");
  });

  it("mints exact Q58 availability parameters only from the signed deployment identity", () => {
    const fixture = makeFixture();
    const identity = verifyWatcherDeploymentIdentity({
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: [fixture.trustRoot],
      durableMarker: fixture.durableMarker,
    });
    const authority = watcherDeploymentAvailabilityChallengeAuthority(identity);

    expect(authority).toEqual({
      schemaVersion:
        "midgard-watcher-deployment-availability-challenge-authority-v1",
      deploymentFingerprint: identity.manifestId,
      parameters: fixture.signedIdentity.manifest.availabilityChallenge,
      parametersDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      authorityDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(authority.parameters).toMatchObject({
      responseGeometry: {
        chunkByteLength: 14_020,
        trancheByteLength: 4_194_304,
        maxTrancheCount: 16,
      },
      ...daBondManifestAmounts(),
      challengerBondLovelace: 10_000_000_000,
      maxOpenFeeLovelace: 500_000,
      maxPublicationFeeLovelace: 500_000,
      maxSettlementFeeLovelace: 500_000,
      maxCloseFeeLovelace: 1_000_000,
      maxTimeoutFeeLovelace: 1_200_000,
    });
    expect(Object.isFrozen(authority)).toBe(true);
    expect(Object.isFrozen(authority.parameters)).toBe(true);
    expect(authority.parameters).not.toBe(
      fixture.signedIdentity.manifest.availabilityChallenge,
    );
    fixture.signedIdentity.manifest.availabilityChallenge.responseGeometry.chunkByteLength = 1;
    expect(authority.parameters.responseGeometry.chunkByteLength).toBe(14_020);
    expect(() =>
      assertWatcherDeploymentAvailabilityChallengeAuthority(authority),
    ).not.toThrow();
    expect(() =>
      assertWatcherDeploymentAvailabilityChallengeAuthority({
        ...authority,
      }),
    ).toThrow("invalid_field");
    expect(() =>
      watcherDeploymentAvailabilityChallengeAuthority({ ...identity }),
    ).toThrow("invalid_field");
  });

  it("mints release finality only from the exact signed deployment identity", async () => {
    const fixture = makeFixture();
    const identity = verifyWatcherDeploymentIdentity({
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: [fixture.trustRoot],
      durableMarker: fixture.durableMarker,
    });
    const authority = watcherDeploymentReleaseFinalityAuthority(identity);

    await expect(
      authority.verifyForWorkflow({
        deploymentFingerprint: identity.manifestId,
      }),
    ).resolves.toMatchObject({
      deploymentIdentityDigest: identity.manifestId,
      blueprintHash: BLUEPRINT_HASH,
      policy: RELEASE_L1_FINALITY_POLICY,
      policyDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    await expect(
      authority.verifyForWorkflow({
        deploymentFingerprint: "aa".repeat(32),
      }),
    ).rejects.toThrow("mismatched_identity");

    const clone = { ...authority };
    await expect(
      clone.verifyForWorkflow({ deploymentFingerprint: identity.manifestId }),
    ).rejects.toThrow("invalid_field");
    expect(() =>
      watcherDeploymentReleaseFinalityAuthority({ ...identity }),
    ).toThrow("invalid_field");
  });

  it("binds release confirmation depth to the compiled deployment profile end to end", async () => {
    // The profile family fixes the depth: live testing at 10, public at 30.
    // Verification admits only the compiled selection, so the accepted depth
    // is the selected profile's (10 in a testing build) and the other family's
    // depth is refused whichever profile is compiled in.
    expect(
      [
        "preprod-testing",
        "local-devnet-testing",
        "mainnet",
        "preprod-public",
      ].map(
        (name) =>
          DEPLOYMENT_PROFILES[name as keyof typeof DEPLOYMENT_PROFILES]
            .l1_finality.confirmation_depth,
      ),
    ).toEqual([10, 10, 30, 30]);
    const selectedDepth =
      SELECTED_DEPLOYMENT_PROFILE.l1_finality.confirmation_depth;
    expect(RELEASE_L1_FINALITY_POLICY.confirmationDepth).toBe(selectedDepth);

    const fixture = makeFixture();
    expect(fixture.signedIdentity.manifest.l1Finality.confirmationDepth).toBe(
      selectedDepth,
    );
    const identity = verifyWatcherDeploymentIdentity({
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: [fixture.trustRoot],
      durableMarker: fixture.durableMarker,
    });
    const releaseFinality = watcherDeploymentReleaseFinalityPolicy(identity);
    expect(releaseFinality.policy.confirmationDepth).toBe(selectedDepth);
    expect(releaseFinality.policy.automaticRecoveryMaxDepth).toBe(2160);
    await expect(
      watcherDeploymentReleaseFinalityAuthority(identity).verifyForWorkflow({
        deploymentFingerprint: identity.manifestId,
      }),
    ).resolves.toEqual(releaseFinality);
    expect(() =>
      watcherDeploymentReleaseFinalityPolicy({ ...identity }),
    ).toThrow("invalid_field");

    const otherDepth = selectedDepth === 10 ? 30 : 10;
    const mismatched = structuredClone(fixture.signedIdentity);
    mismatched.manifest.l1Finality = {
      ...mismatched.manifest.l1Finality,
      confirmationDepth: otherDepth,
    };
    const { manifestId: _manifestId, ...unsigned } = mismatched.manifest;
    mismatched.manifest.manifestId = computeDeploymentManifestId(unsigned);
    fixture.resign(mismatched);
    rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity: mismatched,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: makeDeploymentMarker(mismatched.manifest.manifestId),
        }),
      "canonical_manifest_invalid",
      "$.manifest",
    );
  });

  it("mints exact release economics only from the signed deployment identity", async () => {
    const fixture = makeFixture();
    const identity = verifyWatcherDeploymentIdentity({
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: [fixture.trustRoot],
      durableMarker: fixture.durableMarker,
    });
    const authority = watcherDeploymentReleaseEconomicsAuthority(identity);

    await expect(
      authority.verifyForWorkflow({
        deploymentFingerprint: identity.manifestId,
      }),
    ).resolves.toMatchObject({
      deploymentIdentityDigest: identity.manifestId,
      blueprintHash: BLUEPRINT_HASH,
      policy: {
        profile: "bounded-acceptance-v1",
        proverCollateralFloorLovelace: "5000000",
      },
      policyDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    await expect(
      authority.verifyForWorkflow({
        deploymentFingerprint: "aa".repeat(32),
      }),
    ).rejects.toThrow("mismatched_identity");

    const clone = { ...authority };
    await expect(
      clone.verifyForWorkflow({ deploymentFingerprint: identity.manifestId }),
    ).rejects.toThrow("invalid_field");
    expect(() =>
      watcherDeploymentReleaseEconomicsAuthority({ ...identity }),
    ).toThrow("invalid_field");
  });

  it("binds the complete registered sparse-ID catalogue to deployed contracts", () => {
    const fixture = makeFixture();
    const categories = fixture.policy.fraudProofCatalogue.categories;

    expect(categories.zeroInput).toEqual({
      categoryId: "00000005",
      scriptHash: fixture.policy.appliedScriptHashes.fraudProofZeroInput,
    });
    expect(categories.validationTraceDispute).toEqual({
      categoryId: "00000006",
      scriptHash: fixture.policy.appliedScriptHashes.validationTraceDispute,
    });
    expect(categories.missingSignature).toEqual({
      categoryId: "0000000e",
      scriptHash: fixture.policy.appliedScriptHashes.fraudProofMissingSignature,
    });
    expect(categories.withdrawnInput).toEqual({
      categoryId: "00000018",
      scriptHash: fixture.policy.appliedScriptHashes.fraudProofWithdrawnInput,
    });
    expect(categories.valueNotPreserved).toEqual({
      categoryId: "00000019",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofValueNotPreserved,
    });
    expect(categories.inputSetUniqueness).toEqual({
      categoryId: "0000001a",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofInputSetUniqueness,
    });
    expect(categories.mintAuthorization).toEqual({
      categoryId: "0000001b",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofMintAuthorization,
    });
    expect(categories.networkId).toEqual({
      categoryId: "0000001c",
      scriptHash: fixture.policy.appliedScriptHashes.fraudProofNetworkId,
    });
    expect(categories.nativeScriptInvalid).toEqual({
      categoryId: "0000001e",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofNativeScriptInvalid,
    });
    expect(categories.minAda).toEqual({
      categoryId: "0000001f",
      scriptHash: fixture.policy.appliedScriptHashes.fraudProofMinAda,
    });
    expect(categories.fieldPreimageLengthMismatch).toEqual({
      categoryId: "00000020",
      scriptHash:
        fixture.policy.appliedScriptHashes
          .fraudProofFieldPreimageLengthMismatch,
    });
    expect(categories.fieldItemWidthIllegal).toEqual({
      categoryId: "00000021",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofFieldItemWidthIllegal,
    });
    expect(categories.witnessScriptDecoding).toEqual({
      categoryId: "00000022",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofWitnessScriptDecoding,
    });
    expect(categories.scriptIntegrityHashMissing).toEqual({
      categoryId: "00000023",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofScriptIntegrityHashMissing,
    });
    expect(categories.transactionOutputNonCanonical).toEqual({
      categoryId: "00000029",
      scriptHash:
        fixture.policy.appliedScriptHashes
          .fraudProofTransactionOutputNonCanonical,
    });
    expect(categories.resolvedOutputNonCanonical).toEqual({
      categoryId: "00000026",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofResolvedOutputNonCanonical,
    });
    expect(categories.mintDeclaredAssetLimit).toEqual({
      categoryId: "0000002c",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofMintDeclaredAssetLimit,
    });
    expect(categories.mintItemNonCanonical).toEqual({
      categoryId: "00000036",
      scriptHash:
        fixture.policy.appliedScriptHashes.fraudProofMintItemNonCanonical,
    });
    expect(Object.keys(categories)).toHaveLength(53);

    fixture.policy = {
      ...fixture.policy,
      fraudProofCatalogue: {
        ...fixture.policy.fraudProofCatalogue,
        categories: {
          ...categories,
          zeroInput: {
            ...categories.zeroInput,
            scriptHash: NATIVE_SCRIPT_HASH,
          },
        },
      },
    };
    rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "mismatched_identity",
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.zeroInput",
    );
  });

  it("requires the exact durable deployment marker", () => {
    const fixture = makeFixture();
    const verify = (durableMarker: unknown) =>
      verifyWatcherDeploymentIdentity({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker,
      });

    rejection(() => verify(null), "missing_durable_marker", "$.durableMarker");
    rejection(
      () => verify(makeDeploymentMarker("aa".repeat(32))),
      "durable_marker_mismatch",
      "$.durableMarker",
    );
  });

  it("rejects an untrusted signer and any signature mutation", () => {
    const fixture = makeFixture();
    const other = makeTrustRoot();
    const verify = () =>
      verifyWatcherDeploymentIdentity({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      });

    fixture.signedIdentity.attestation.trustRootId =
      other.trustRoot.trustRootId;
    rejection(verify, "untrusted_signer", "$.attestation.trustRootId");

    fixture.signedIdentity.attestation.trustRootId =
      fixture.trustRoot.trustRootId;
    fixture.signedIdentity.attestation.signature = "00".repeat(64);
    rejection(verify, "invalid_signature", "$.attestation.signature");
  });

  it("rejects unknown, missing, and malformed signed-identity fields", () => {
    const fixture = makeFixture();
    const verify = () =>
      verifyWatcherDeploymentIdentity({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      });

    fixture.signedIdentity.historicalVersion = 9;
    rejection(verify, "unknown_field", "$.historicalVersion");
    delete fixture.signedIdentity.historicalVersion;

    delete fixture.signedIdentity.releaseBindings.da;
    rejection(verify, "missing_field", "$.releaseBindings.da");
    fixture.signedIdentity.releaseBindings.da = {
      mode: "operator_private",
      identityDigest: "aa".repeat(32),
    };
    rejection(verify, "invalid_field", "$.releaseBindings.da.mode");
  });

  it.each([
    [
      "canonical feature removal",
      (manifest: MutableRecord) => {
        manifest.consensusProfile.features =
          manifest.consensusProfile.features.slice(1);
      },
    ],
    [
      "applied script-byte drift",
      (manifest: MutableRecord) => {
        manifest.contracts.payoutSpend.contract.cborHex = "02";
      },
    ],
    [
      "nested legacy manifest field",
      (manifest: MutableRecord) => {
        manifest.hubOracleOneShot.legacyNonceVersion = 2;
      },
    ],
  ])("rejects %s at the canonical manifest boundary", (_label, mutate) => {
    const fixture = makeFixture();
    const signedIdentity = structuredClone(fixture.signedIdentity);
    mutate(signedIdentity.manifest);
    const { manifestId: _manifestId, ...identity } = signedIdentity.manifest;
    signedIdentity.manifest.manifestId = computeDeploymentManifestId(identity);
    fixture.resign(signedIdentity);

    rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "canonical_manifest_invalid",
      "$.manifest",
    );
  });

  it.each([
    [
      "network",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.signedIdentity.manifest.network = "Preview";
        const { manifestId: _manifestId, ...identity } =
          fixture.signedIdentity.manifest;
        fixture.signedIdentity.manifest.manifestId =
          computeDeploymentManifestId(identity);
        fixture.resign();
      },
      "$.manifest.network",
    ],
    [
      "one-shot",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          hubOracleOneShotOutRef: `${"aa".repeat(32)}#0`,
        };
      },
      "$.manifest.hubOracleOneShot",
    ],
    [
      "applied script hash",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          appliedScriptHashes: {
            ...fixture.policy.appliedScriptHashes,
            payoutSpend: "aa".repeat(28),
          },
        };
      },
      "$.manifest.contracts",
    ],
    [
      "reference script",
      (fixture: ReturnType<typeof makeFixture>) => {
        const role = Object.keys(fixture.policy.referenceScripts)[0];
        fixture.policy = {
          ...fixture.policy,
          referenceScripts: {
            ...fixture.policy.referenceScripts,
            [role]: {
              ...fixture.policy.referenceScripts[role],
              outRef: `${"aa".repeat(32)}#0`,
            },
          },
        };
      },
      /^[$]\.manifest\.referenceScripts\./u,
    ],
    [
      "catalogue",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          fraudProofCatalogue: {
            ...fixture.policy.fraudProofCatalogue,
            root: "aa".repeat(32),
          },
        };
      },
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    ],
    [
      "rule bundle",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          ruleBundleCommitment: "aa".repeat(32),
        };
      },
      "$.releaseBindings",
    ],
    [
      "program commitments",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          programCommitments: {
            ...fixture.policy.programCommitments,
            "transition-order-v1": "aa".repeat(32),
          },
        };
      },
      "$.releaseBindings",
    ],
    [
      "DA identity",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          daIdentityDigest: "aa".repeat(32),
        };
      },
      "$.releaseBindings.da",
    ],
    [
      "contract blueprint",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          blueprintHash: "aa".repeat(32),
        };
      },
      "$.releaseBindings.artifacts",
    ],
  ])("fails closed on a %s mismatch", (_label, mutate, expectedPath) => {
    const fixture = makeFixture();
    mutate(fixture);

    const error = rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      _label === "network"
        ? "canonical_manifest_invalid"
        : "mismatched_identity",
      _label === "network" ? "$.manifest" : expectedPath,
    );
    expect(error.code).toBe(
      _label === "network"
        ? "canonical_manifest_invalid"
        : "mismatched_identity",
    );
  });

  it("keeps signature and trust-root bytes out of diagnostics", () => {
    const fixture = makeFixture();
    fixture.signedIdentity.attestation.signature = "00".repeat(64);
    const error = rejection(
      () =>
        verifyWatcherDeploymentIdentity({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "invalid_signature",
      "$.attestation.signature",
    );
    const diagnostic = watcherDeploymentIdentityDiagnostic(error);

    expect(JSON.stringify(diagnostic)).not.toContain(
      fixture.signedIdentity.attestation.signature,
    );
    expect(JSON.stringify(diagnostic)).not.toContain(
      fixture.trustRoot.publicKeySpkiDerHex,
    );
  });
});
