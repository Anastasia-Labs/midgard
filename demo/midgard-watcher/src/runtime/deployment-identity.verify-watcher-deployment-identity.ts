import { createPublicKey, verify as verifySignature } from "node:crypto";

import {
  assertDeploymentMarkerMatches,
  computeDeploymentManifestJsonDigest,
  makeDeploymentMarker,
  parseDeploymentManifestAvailabilityChallenge,
  parseDeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestEconomics,
  parseDeploymentManifestEventHistoryRecipe,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_AUTHORITY,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseEconomicsAuthority,
  type FraudProofReleaseFinalityAuthority,
  type ReleaseFraudProofEconomicsPolicy,
  releaseL1FinalityPolicyOf,
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";

import {
  exactRecord,
  exactString,
  fail,
  HEX_28,
  HEX_32,
  HEX_64,
  WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION,
  WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION,
  WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "./deployment-identity.catalogue-category-to-contract.js";
import {
  parsePolicy,
  parseReleaseBindings,
  signaturePayload,
} from "./deployment-identity.parse-policy.js";
import {
  appliedScriptHashesByDeploymentIdentity,
  assertPolicyBindings,
  authenticatedWatcherAvailabilityChallengeAuthorities,
  authenticatedWatcherDeploymentIdentities,
  authenticatedWatcherProtocolParameterAuthorities,
  authenticatedWatcherProtocolScriptAuthorities,
  authenticatedWatcherReleaseEconomicsAuthorities,
  authenticatedWatcherReleaseFinalityAuthorities,
  availabilityChallengeAuthorityByDeploymentIdentity,
  parseTrustRoots,
  protocolParameterAuthorityByDeploymentIdentity,
  protocolScriptAuthorityByDeploymentIdentity,
  releaseEconomicsAuthorityByDeploymentIdentity,
  releaseFinalityAuthorityByDeploymentIdentity,
  releaseFinalityByDeploymentIdentity,
  type SignedUserEventScript,
  USER_EVENT_SIGNED_CONTRACT_NAMES,
  userEventScriptsByDeploymentIdentity,
  type UserEventSignedContractName,
  type VerifiedWatcherDeploymentIdentity,
  verifyCanonicalManifest,
} from "./deployment-identity.parse-trust-roots.js";

export const verifyWatcherDeploymentIdentity = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicy;
  readonly trustRoots: readonly WatcherDeploymentTrustRoot[];
  readonly durableMarker: unknown;
}): VerifiedWatcherDeploymentIdentity => {
  const policy = parsePolicy(input.policy);
  const trustRoots = parseTrustRoots(input.trustRoots);
  const envelope = exactRecord(input.signedIdentity, "$", [
    "schemaVersion",
    "manifest",
    "releaseBindings",
    "attestation",
  ]);
  if (
    envelope.schemaVersion !== WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION
  ) {
    fail("invalid_field", "$.schemaVersion");
  }
  const manifest = verifyCanonicalManifest(envelope.manifest);
  const manifestId = manifest.manifestId;
  const bindings = parseReleaseBindings(
    envelope.releaseBindings,
    "$.releaseBindings",
  );
  const attestation = exactRecord(envelope.attestation, "$.attestation", [
    "algorithm",
    "trustRootId",
    "signature",
  ]);
  if (attestation.algorithm !== "ed25519") {
    fail("invalid_field", "$.attestation.algorithm");
  }
  const trustRootId = exactString(
    attestation.trustRootId,
    "$.attestation.trustRootId",
    HEX_32,
  );
  const trustRoot =
    trustRoots.get(trustRootId) ??
    fail("untrusted_signer", "$.attestation.trustRootId");
  const signature = Buffer.from(
    exactString(attestation.signature, "$.attestation.signature", HEX_64),
    "hex",
  );
  let signatureValid = false;
  try {
    signatureValid = verifySignature(
      null,
      signaturePayload(manifestId, bindings),
      createPublicKey({
        key: trustRoot.publicKeySpkiDer,
        format: "der",
        type: "spki",
      }),
      signature,
    );
  } catch {
    fail("invalid_signature", "$.attestation.signature");
  }
  if (!signatureValid) {
    fail("invalid_signature", "$.attestation.signature");
  }
  assertPolicyBindings(manifest, bindings, policy);

  const expectedMarker = makeDeploymentMarker(manifestId);
  if (input.durableMarker === null || input.durableMarker === undefined) {
    fail("missing_durable_marker", "$.durableMarker");
  }
  try {
    assertDeploymentMarkerMatches(
      expectedMarker,
      input.durableMarker,
      "watcher durable store",
    );
  } catch {
    fail("durable_marker_mismatch", "$.durableMarker");
  }

  const verified = Object.freeze({
    manifestId,
    network: policy.network,
    trustRootId,
    blueprintHash: policy.blueprintHash,
    fundingProfileBundleDigest: policy.fundingProfileBundleDigest,
    ruleBundleCommitment: policy.ruleBundleCommitment,
    programCommitments: policy.programCommitments,
    durableMarker: expectedMarker,
  });
  const protocolScriptHashes = Object.freeze({
    hubOracleMint: policy.appliedScriptHashes.hubOracleMint!,
    stateQueueSpend: policy.appliedScriptHashes.stateQueueSpend!,
    stateQueueMint: policy.appliedScriptHashes.stateQueueMint!,
    correctionLockSpend: policy.appliedScriptHashes.correctionLockSpend!,
    fraudProofSpend: policy.appliedScriptHashes.fraudProofSpend!,
    fraudProofMint: policy.appliedScriptHashes.fraudProofMint!,
    referenceScriptAuthMint:
      policy.appliedScriptHashes.referenceScriptAuthMint!,
    availabilityChallengeSpend:
      policy.appliedScriptHashes.availabilityChallengeSpend!,
    availabilityChallengeMint:
      policy.appliedScriptHashes.availabilityChallengeMint!,
    daBondPoolSpend: policy.appliedScriptHashes.daBondPoolSpend!,
    daBondPoolMint: policy.appliedScriptHashes.daBondPoolMint!,
    daAttestationMint: policy.appliedScriptHashes.daAttestationMint!,
    availabilityChallengeOpenWithdraw:
      policy.appliedScriptHashes.availabilityChallengeOpenWithdraw!,
    availabilityChallengeSettleWithdraw:
      policy.appliedScriptHashes.availabilityChallengeSettleWithdraw!,
    availabilityChallengeCloseWithdraw:
      policy.appliedScriptHashes.availabilityChallengeCloseWithdraw!,
    availabilityChallengeTimeoutWithdraw:
      policy.appliedScriptHashes.availabilityChallengeTimeoutWithdraw!,
  });
  appliedScriptHashesByDeploymentIdentity.set(
    verified,
    Object.freeze({ ...policy.appliedScriptHashes }),
  );
  if (Object.values(protocolScriptHashes).some((hash) => !HEX_28.test(hash))) {
    fail("mismatched_identity", "$.policy.appliedScriptHashes");
  }
  const referenceScripts = Object.freeze(
    Object.fromEntries(
      Object.entries(policy.referenceScripts)
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([role, reference]) => [role, Object.freeze({ ...reference })]),
    ),
  );
  const authorityInput = Object.freeze({
    schemaVersion: WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    network: policy.network,
    hubOracleOneShotOutRef: policy.hubOracleOneShotOutRef,
    protocolScriptHashes,
    referenceScripts,
  });
  const protocolScriptAuthority = Object.freeze({
    ...authorityInput,
    authorityDigest: computeDeploymentManifestJsonDigest(authorityInput),
  });
  const manifestProtocolParameters = manifest.cardanoProtocolParameters;
  // Keep the independently owned, frozen snapshot supplied by the parser.
  const protocolParameterSnapshot =
    parseDeploymentManifestCardanoProtocolParameters(
      manifestProtocolParameters.snapshot,
    );
  const protocolParameterSnapshotDigest = manifestProtocolParameters.digest;
  const protocolParameterAuthorityInput = Object.freeze({
    schemaVersion:
      WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    snapshot: protocolParameterSnapshot,
    snapshotDigest: protocolParameterSnapshotDigest,
  });
  const protocolParameterAuthority = Object.freeze({
    ...protocolParameterAuthorityInput,
    authorityDigest: computeDeploymentManifestJsonDigest(
      protocolParameterAuthorityInput,
    ),
  });
  const availabilityChallengeParameters =
    parseDeploymentManifestAvailabilityChallenge(
      manifest.availabilityChallenge,
    );
  const availabilityChallengeAuthorityInput = Object.freeze({
    schemaVersion:
      WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    parameters: availabilityChallengeParameters,
    parametersDigest: computeDeploymentManifestJsonDigest(
      availabilityChallengeParameters,
    ),
  });
  const availabilityChallengeAuthority = Object.freeze({
    ...availabilityChallengeAuthorityInput,
    authorityDigest: computeDeploymentManifestJsonDigest(
      availabilityChallengeAuthorityInput,
    ),
  });
  const releaseFinalityPolicy = releaseL1FinalityPolicyOf(manifest.l1Finality);
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifestId,
    blueprintHash: manifest.artifacts.blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      releaseFinalityPolicy,
    ),
    policy: releaseFinalityPolicy,
  });
  const releaseFinalityAuthority: FraudProofReleaseFinalityAuthority =
    Object.freeze({
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      async verifyForWorkflow(
        this: FraudProofReleaseFinalityAuthority,
        input: { readonly deploymentFingerprint: string },
      ): Promise<VerifiedFraudProofReleaseFinalityPolicy> {
        if (!authenticatedWatcherReleaseFinalityAuthorities.has(this)) {
          fail("invalid_field", "$.releaseFinalityAuthority");
        }
        if (input.deploymentFingerprint !== manifestId) {
          fail(
            "mismatched_identity",
            "$.releaseFinalityAuthority.deploymentFingerprint",
          );
        }
        return releaseFinality;
      },
    });
  const manifestEconomics = parseDeploymentManifestEconomics(
    manifest.economics,
  );
  const releaseEconomicsPolicy = Object.freeze({
    profile: manifestEconomics.profile,
    requiredBondLovelace: manifestEconomics.requiredBondLovelace.toString(),
    slashingPenaltyLovelace:
      manifestEconomics.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace:
      manifestEconomics.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      manifestEconomics.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      manifestEconomics.proverCollateralFloorLovelace.toString(),
  }) satisfies ReleaseFraudProofEconomicsPolicy;
  const releaseEconomics = validateVerifiedFraudProofReleaseEconomicsPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifestId,
    blueprintHash: manifest.artifacts.blueprintHash,
    policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(
      releaseEconomicsPolicy,
    ),
    policy: releaseEconomicsPolicy,
  });
  const releaseEconomicsAuthority: FraudProofReleaseEconomicsAuthority =
    Object.freeze({
      authorityVersion: FRAUD_PROOF_RELEASE_ECONOMICS_AUTHORITY,
      async verifyForWorkflow(
        this: FraudProofReleaseEconomicsAuthority,
        input: { readonly deploymentFingerprint: string },
      ): Promise<VerifiedFraudProofReleaseEconomicsPolicy> {
        if (!authenticatedWatcherReleaseEconomicsAuthorities.has(this)) {
          fail("invalid_field", "$.releaseEconomicsAuthority");
        }
        if (input.deploymentFingerprint !== manifestId) {
          fail(
            "mismatched_identity",
            "$.releaseEconomicsAuthority.deploymentFingerprint",
          );
        }
        return releaseEconomics;
      },
    });
  const signedUserEventContracts = Object.freeze(
    Object.fromEntries(
      USER_EVENT_SIGNED_CONTRACT_NAMES.map((name) => {
        const entry = manifest.contracts[name];
        return [
          name,
          Object.freeze({
            type: entry.contract.type,
            cborHex: entry.contract.cborHex,
            scriptHash: entry.scriptHash,
          }),
        ];
      }),
    ),
  ) as Readonly<Record<UserEventSignedContractName, SignedUserEventScript>>;
  userEventScriptsByDeploymentIdentity.set(
    verified,
    Object.freeze({
      blueprintSha256: policy.blueprintHash,
      historyRecipes: Object.freeze({
        deposit: parseDeploymentManifestEventHistoryRecipe(
          manifest.contracts.depositMint.eventHistoryRecipe,
        ),
        withdrawal: parseDeploymentManifestEventHistoryRecipe(
          manifest.contracts.withdrawalMint.eventHistoryRecipe,
        ),
      }),
      contracts: signedUserEventContracts,
    }),
  );
  authenticatedWatcherDeploymentIdentities.add(verified);
  authenticatedWatcherProtocolScriptAuthorities.add(protocolScriptAuthority);
  authenticatedWatcherProtocolParameterAuthorities.add(
    protocolParameterAuthority,
  );
  authenticatedWatcherReleaseFinalityAuthorities.add(releaseFinalityAuthority);
  authenticatedWatcherReleaseEconomicsAuthorities.add(
    releaseEconomicsAuthority,
  );
  authenticatedWatcherAvailabilityChallengeAuthorities.add(
    availabilityChallengeAuthority,
  );
  protocolScriptAuthorityByDeploymentIdentity.set(
    verified,
    protocolScriptAuthority,
  );
  protocolParameterAuthorityByDeploymentIdentity.set(
    verified,
    protocolParameterAuthority,
  );
  releaseFinalityAuthorityByDeploymentIdentity.set(
    verified,
    releaseFinalityAuthority,
  );
  releaseFinalityByDeploymentIdentity.set(verified, releaseFinality);
  releaseEconomicsAuthorityByDeploymentIdentity.set(
    verified,
    releaseEconomicsAuthority,
  );
  availabilityChallengeAuthorityByDeploymentIdentity.set(
    verified,
    availabilityChallengeAuthority,
  );
  return verified;
};
