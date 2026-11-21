import type { DatabaseSync } from "node:sqlite";

import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { deriveDeploymentManifestCardanoProtocolParametersFromLedger } from "@al-ft/midgard-l1-follower/protocol-parameters";

import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.parse-trust-roots.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.verify-watcher-user-event-script-binding.js";
import {
  assertWatcherDeploymentProtocolParameterAuthority,
  watcherDeploymentProtocolParameterAuthority,
} from "../runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import { createWatcherProtocolParameterHistoryStorage } from "./prover-funding-parameter-history.js";
import {
  type WatcherProverFundingReservationRecord,
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.watcher-prover-funding-reservation-store.js";

export const WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY =
  "midgard-watcher-production-protocol-parameter-runtime-authority-v1" as const;

export type WatcherProtocolParameterRuntimeAuthority = Readonly<{
  schemaVersion: typeof WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY;
  deploymentFingerprint: string;
  source: "local_node" | "signed_deployment" | "authenticated_history";
  sourceEndpoint: string;
  snapshot: DeploymentManifestCardanoProtocolParameters;
  snapshotDigest: string;
  authorityDigest: string;
}>;

const admittedRuntimeAuthorities = new WeakSet<object>();
const refreshRuntimeAuthorities = new WeakMap<
  object,
  () => Promise<WatcherProtocolParameterRuntimeAuthority>
>();

export const assertWatcherProtocolParameterRuntimeAuthority = (
  authority: WatcherProtocolParameterRuntimeAuthority,
): void => {
  if (!admittedRuntimeAuthorities.has(authority)) {
    throw new Error(
      "prover funding protocol-parameter runtime authority is not admitted",
    );
  }
};

/**
 * Reads the node's current parameters through `query` (the raw
 * `protocol_params` local-state-query answer). A node that cannot answer now
 * is the funding outage, kept as an L1 transient by its cause; an answer that
 * does not decode stays a hard failure.
 */
const queryLiveProtocolParameters = async (
  query: () => Promise<Uint8Array>,
): Promise<DeploymentManifestCardanoProtocolParameters> => {
  let bytes: Uint8Array;
  try {
    bytes = await query();
  } catch (cause) {
    if (!isWatcherL1TransientFailure(cause)) throw cause;
    throw new WatcherProverFundingUnavailableError(
      "Current local funding parameters are temporarily unavailable",
      { cause },
    );
  }
  return deriveDeploymentManifestCardanoProtocolParametersFromLedger(bytes);
};

const createRuntimeAuthority = async ({
  deploymentIdentity,
  query,
}: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly query: () => Promise<Uint8Array>;
}): Promise<WatcherProtocolParameterRuntimeAuthority> => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const signed =
    watcherDeploymentProtocolParameterAuthority(deploymentIdentity);
  assertWatcherDeploymentProtocolParameterAuthority(signed);
  const live = await queryLiveProtocolParameters(query);
  const liveDigest = computeDeploymentManifestJsonDigest(live);
  // The signed snapshot authenticates deployment-time measurements, not future
  // L1 fees. Local native startup continues to authenticate the chain identity.
  const identity = Object.freeze({
    schemaVersion: WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY,
    deploymentFingerprint: deploymentIdentity.manifestId,
    source: "local_node" as const,
    sourceEndpoint: "",
    snapshot: live,
    snapshotDigest: liveDigest,
  });
  const authority = Object.freeze({
    ...identity,
    authorityDigest: computeDeploymentManifestJsonDigest(identity),
  });
  admittedRuntimeAuthorities.add(authority);
  refreshRuntimeAuthorities.set(authority, () =>
    createRuntimeAuthority({ deploymentIdentity, query }),
  );
  return authority;
};

/**
 * The live funding parameters from the watcher's node: `query` returns the
 * raw `protocol_params` local-state-query answer (the node transport's
 * `query({ query: "protocol_params" })`).
 */
export const createWatcherProtocolParameterRuntimeAuthority = async (
  input: Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    query: () => Promise<Uint8Array>;
  }>,
): Promise<WatcherProtocolParameterRuntimeAuthority> =>
  await createRuntimeAuthority(input);

/** One bounded local query; transport and admission failures remain failures. */
export const refreshWatcherProtocolParameterRuntimeAuthority = async (
  authority: WatcherProtocolParameterRuntimeAuthority,
): Promise<WatcherProtocolParameterRuntimeAuthority> => {
  assertWatcherProtocolParameterRuntimeAuthority(authority);
  const refresh = refreshRuntimeAuthorities.get(authority);
  if (refresh === undefined)
    throw new Error(
      "Historical funding parameters cannot authorize a fresh live query",
    );
  const current = await refresh();
  return current.snapshotDigest === authority.snapshotDigest
    ? authority
    : current;
};

/** Authenticated historical evidence for exact lease recovery, never fresh funding. */
export const watcherSignedDeploymentProtocolParameterRecoveryAuthority = (
  deploymentIdentity: VerifiedWatcherDeploymentIdentity,
): WatcherProtocolParameterRuntimeAuthority => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const signed =
    watcherDeploymentProtocolParameterAuthority(deploymentIdentity);
  assertWatcherDeploymentProtocolParameterAuthority(signed);
  const identity = Object.freeze({
    schemaVersion: WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY,
    deploymentFingerprint: deploymentIdentity.manifestId,
    source: "signed_deployment" as const,
    sourceEndpoint: "",
    snapshot: signed.snapshot,
    snapshotDigest: signed.snapshotDigest,
  });
  const authority = Object.freeze({
    ...identity,
    authorityDigest: computeDeploymentManifestJsonDigest(identity),
  });
  admittedRuntimeAuthorities.add(authority);
  return authority;
};

export type WatcherProtocolParameterHistory = Readonly<{
  read(
    record: WatcherProverFundingReservationRecord,
  ): WatcherProtocolParameterRuntimeAuthority | null;
  remember(
    record: WatcherProverFundingReservationRecord,
    authority: WatcherProtocolParameterRuntimeAuthority,
  ): void;
}>;
const admittedParameterHistories = new WeakSet<object>();
export const assertWatcherProtocolParameterHistory = (
  history: WatcherProtocolParameterHistory,
): void => {
  if (!admittedParameterHistories.has(history))
    throw new Error("Funding parameter history is not admitted");
};

/** Only authenticated durable evidence can re-admit a historical local snapshot. */
export const createWatcherProtocolParameterHistory = (input: {
  readonly database: DatabaseSync;
  readonly authenticationKey: Uint8Array;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
}): WatcherProtocolParameterHistory => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  const storage = createWatcherProtocolParameterHistoryStorage({
    ...input,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
  });
  const read = (record: WatcherProverFundingReservationRecord) => {
    const snapshot = storage.read(record);
    if (snapshot === null) return null;
    const identity = Object.freeze({
      schemaVersion: WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY,
      deploymentFingerprint: input.deploymentIdentity.manifestId,
      source: "authenticated_history" as const,
      sourceEndpoint: "",
      snapshot,
      snapshotDigest: computeDeploymentManifestJsonDigest(snapshot),
    });
    const authority = Object.freeze({
      ...identity,
      authorityDigest: computeDeploymentManifestJsonDigest(identity),
    });
    admittedRuntimeAuthorities.add(authority);
    return authority;
  };
  const history: WatcherProtocolParameterHistory = Object.freeze({
    read,
    remember: (record, authority) => {
      assertWatcherProtocolParameterRuntimeAuthority(authority);
      if (
        authority.deploymentFingerprint !== input.deploymentIdentity.manifestId
      )
        throw new Error("Funding parameter history changed deployment");
      storage.remember(record, authority.snapshot);
    },
  });
  admittedParameterHistories.add(history);
  return history;
};
