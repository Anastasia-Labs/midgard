import { createHash } from "node:crypto";

import {
  type HistoricalNativeScriptProviderRoster,
  requireHistoricalNativeScriptProviderRoster,
} from "../workflow/historical-native-script-corpus.js";
import {
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../workflow/release-finality-policy.js";
import {
  admittedAuthenticatedHistoricalNativeScriptRosters,
  admittedHistoricalNativeScriptRosters,
  createHistoricalNativeScriptSourceRoster,
  HISTORICAL_NATIVE_SCRIPT_SOURCE,
  type HistoricalNativeScriptEvidence,
  type HistoricalNativeScriptSource,
  type HistoricalNativeScriptSourceRoster,
  postHistoricalNativeScriptJson,
} from "./historical-script.admit-source-identities.js";

/**
 * Concrete production quorum derived only from the admitted immutable history
 * overlay. No callback or source identity is accepted alongside a workflow.
 */
export const createExternalHistoricalNativeScriptSourceRoster = ({
  providerRoster: untrustedProviderRoster,
  releaseFinality,
}: {
  readonly providerRoster: HistoricalNativeScriptProviderRoster;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): HistoricalNativeScriptSourceRoster => {
  const providerRoster = requireHistoricalNativeScriptProviderRoster(
    untrustedProviderRoster,
  );
  const sources = providerRoster.providers.map(
    (provider): HistoricalNativeScriptSource =>
      Object.freeze({
        sourceVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE,
        sourceMode: "external_providers",
        sourceId: provider.sourceId,
        operatorIdentitySha256: provider.operatorIdentitySha256,
        resolveReferenceScriptPublication: async (
          request: Parameters<
            HistoricalNativeScriptSource["resolveReferenceScriptPublication"]
          >[0],
        ) =>
          await postHistoricalNativeScriptJson({
            authorityEndpoint: provider.authorityEndpoint,
            path: "/midgard/v1/native-script-publication",
            body: request,
            sourceId: provider.sourceId,
          }),
        confirmCanonicalHistory: async (
          request: Parameters<
            HistoricalNativeScriptSource["confirmCanonicalHistory"]
          >[0],
        ) =>
          await postHistoricalNativeScriptJson({
            authorityEndpoint: provider.authorityEndpoint,
            path: "/midgard/v1/native-script-publication/canonicality",
            body: request,
            sourceId: provider.sourceId,
          }),
      }),
  );
  const roster = createHistoricalNativeScriptSourceRoster({
    sourceMode: "external_providers",
    sources,
    applicationOverlayDigest: providerRoster.rosterDigest,
    releaseFinality,
  });
  admittedAuthenticatedHistoricalNativeScriptRosters.add(roster);
  return roster;
};

export const requireHistoricalNativeScriptSourceRoster = (
  roster: HistoricalNativeScriptSourceRoster,
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicy,
): HistoricalNativeScriptSourceRoster => {
  const verifiedFinality =
    validateVerifiedFraudProofReleaseFinalityPolicy(releaseFinality);
  if (!admittedAuthenticatedHistoricalNativeScriptRosters.has(roster)) {
    throw new Error(
      "historical native-script source roster is not a concrete production authority",
    );
  }
  requireSourceRoster({ roster, releaseFinality: verifiedFinality });
  return roster;
};

export const requireSourceRoster = ({
  roster,
  releaseFinality,
}: {
  readonly roster: HistoricalNativeScriptSourceRoster;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): readonly HistoricalNativeScriptSource[] => {
  const sources = admittedHistoricalNativeScriptRosters.get(roster);
  if (
    sources === undefined ||
    roster.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    roster.blueprintHash !== releaseFinality.blueprintHash ||
    roster.finalityPolicyDigest !== releaseFinality.policyDigest ||
    roster.rosterDigest !==
      createHash("sha256")
        .update(
          JSON.stringify({
            schemaVersion: roster.schemaVersion,
            sourceMode: roster.sourceMode,
            applicationOverlayDigest: roster.applicationOverlayDigest,
            deploymentIdentityDigest: roster.deploymentIdentityDigest,
            blueprintHash: roster.blueprintHash,
            finalityPolicyDigest: roster.finalityPolicyDigest,
            sources: roster.sources,
          }),
        )
        .digest("hex")
  ) {
    throw new Error(
      "historical native script source roster is not the installed release authority",
    );
  }
  return sources;
};

export type AdmittedCandidate = Omit<
  HistoricalNativeScriptEvidence,
  | "schemaVersion"
  | "sourceMode"
  | "applicationOverlayDigest"
  | "rosterDigest"
  | "sources"
  | "evidenceDigest"
>;
