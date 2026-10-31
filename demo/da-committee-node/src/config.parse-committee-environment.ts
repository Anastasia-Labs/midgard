import {
  requireRetentionAlertThresholdMs,
  resolveL1ViewFatalMs,
} from "@al-ft/midgard-core";
import { parseL1Origin } from "@al-ft/midgard-core/l1-origin";
import { assertRoleL1Env } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeConfig, Env } from "./config.committee-config.js";
import {
  availabilityJournalPath,
  l1SubmitterPreflightConfig,
  localState,
  nonNegativeInt,
  optionalSignerConfig,
  parseNativeLedgerConfig,
  positiveInt,
} from "./config.l1-submitter-preflight-config.js";
import {
  libp2pPrivateKeySourceConfig,
  rejectPublicRetainedDaCoHosting,
} from "./config.libp2p-da-transport-config.js";
import {
  booleanEnv,
  optionalKeySource,
  optionalNonEmpty,
  optionalSplitList,
  requireEnv,
} from "./config.operational-provider-identity.js";
import { committeePromiseAdoptionConfig } from "./config.promise-admission.js";

/**
 * The committee settings its environment alone fixes. `finalityDepth` is the
 * configured value; `loadCommitteeConfig` requires it to equal the
 * deployment manifest's.
 */
export type CommitteeEnvironmentConfig = Pick<
  CommitteeConfig,
  | "deploymentManifestPath"
  | "contractDeploymentInfoPath"
  | "nativeLedger"
  | "l1Origin"
  | "finalityDepth"
  | "libp2pPrivateKeySource"
  | "signerIndex"
  | "signerKeySource"
  | "l1SubmitterKeySource"
  | "l1SubmissionEnabled"
  | "availabilityPromiseAdoption"
  | "availabilityJournalPath"
  | "availabilitySubmitterKeySource"
  | "l1SubmitterPreflight"
  | "l1SubmitterId"
  | "l1SubmitterIds"
  | "l1LeaderFailoverMs"
  | "localState"
  | "peerRequestTimeoutMs"
  | "peerReplayWindowMs"
  | "peerMaxBodyBytes"
  | "peerRetryInitialDelayMs"
  | "peerRetryMaxDelayMs"
  | "peerRetryMaxAttempts"
  | "peerRateLimitWindowMs"
  | "peerRateLimitMaxRequests"
  | "apiHost"
  | "apiPort"
  | "pollIntervalMs"
  | "l1ViewFatalMs"
  | "retentionAlertThresholdMs"
>;

/**
 * Parses every committee setting that needs neither deployment file: the
 * part of `loadCommitteeConfig` that runs before it reads them, so a
 * deployment tool can check a committee environment before the deployment
 * exists.
 */
export const parseCommitteeEnvironment = (
  env: Env,
): CommitteeEnvironmentConfig => {
  // A role: L1 only through its follower; a Kupmios, Blockfrost or tool
  // `L1_ACCESS` setting is refused at start, by name.
  assertRoleL1Env(env, "da-committee-node");
  const deploymentManifestPath = requireEnv(
    env,
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  );
  const contractDeploymentInfoPath = requireEnv(
    env,
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
  );
  const libp2pPrivateKeySource = libp2pPrivateKeySourceConfig(env);
  rejectPublicRetainedDaCoHosting(env);
  const l1SubmitterKeySource = optionalKeySource(
    env.L1_SUBMITTER_KEY_SOURCE,
    "L1_SUBMITTER_KEY_SOURCE",
  );
  // The deployment's L1 origin point until the redeploy carries it in the
  // manifest: `<slot>.<block hash>`, as `midgard-l1-follower find-origin`
  // prints it. Absent when unset.
  const l1OriginText = optionalNonEmpty(env.L1_ORIGIN);
  const l1Origin =
    l1OriginText === undefined
      ? undefined
      : parseL1Origin(l1OriginText, "L1_ORIGIN");
  const l1SubmitterId = optionalNonEmpty(env.DA_L1_SUBMITTER_ID);
  const l1SubmitterIds = optionalSplitList(env.DA_L1_SUBMITTER_IDS);
  if (
    l1SubmitterIds.length > 0 &&
    (l1SubmitterId === undefined || !l1SubmitterIds.includes(l1SubmitterId))
  ) {
    throw new Error(
      "DA_L1_SUBMITTER_ID must be present in DA_L1_SUBMITTER_IDS",
    );
  }
  const nativeLedger = parseNativeLedgerConfig(env);
  const l1SubmissionEnabled = booleanEnv(
    env.DA_L1_SUBMISSION_ENABLED,
    l1SubmitterKeySource !== undefined,
  );
  if (l1SubmissionEnabled && l1SubmitterKeySource === undefined) {
    throw new Error(
      "L1_SUBMITTER_KEY_SOURCE is required when DA_L1_SUBMISSION_ENABLED=true",
    );
  }
  const l1SubmitterPreflight = l1SubmitterPreflightConfig({
    env,
    l1SubmissionEnabled,
    l1SubmitterKeySource,
  });
  const maybeSigner = optionalSignerConfig(env);
  const finalityDepth = nonNegativeInt(
    requireEnv(env, "CARDANO_FINALITY_DEPTH"),
    "CARDANO_FINALITY_DEPTH",
  );
  const pollIntervalMs = positiveInt(
    env.DA_COMMITTEE_POLL_INTERVAL_MS ?? "15000",
    "DA_COMMITTEE_POLL_INTERVAL_MS",
  );
  // A committee that cannot read L1 for the attestation timeout has already
  // failed the duty that timeout exists for, so that is the default deadline.
  const l1ViewFatalMs = resolveL1ViewFatalMs({
    value: env.L1_VIEW_FATAL_MS,
    defaultMs: Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    pollIntervalMs,
    fieldName: "L1_VIEW_FATAL_MS",
  });
  const retentionAlertThresholdRaw = optionalNonEmpty(
    env.DA_RETENTION_ALERT_THRESHOLD_MS,
  );

  return {
    deploymentManifestPath,
    contractDeploymentInfoPath,
    ...(nativeLedger === undefined ? {} : { nativeLedger }),
    ...(l1Origin === undefined ? {} : { l1Origin }),
    finalityDepth,
    libp2pPrivateKeySource,
    ...maybeSigner,
    l1SubmitterKeySource,
    l1SubmissionEnabled,
    availabilityPromiseAdoption: committeePromiseAdoptionConfig(env),
    availabilityJournalPath: availabilityJournalPath(env),
    availabilitySubmitterKeySource: optionalNonEmpty(
      env.DA_AVAILABILITY_SUBMITTER_KEY_SOURCE,
    ),
    l1SubmitterPreflight,
    ...(l1SubmitterId === undefined ? {} : { l1SubmitterId }),
    l1SubmitterIds:
      l1SubmitterIds.length > 0
        ? l1SubmitterIds
        : l1SubmitterId === undefined
          ? []
          : [l1SubmitterId],
    l1LeaderFailoverMs: nonNegativeInt(
      env.DA_L1_LEADER_FAILOVER_MS ?? "15000",
      "DA_L1_LEADER_FAILOVER_MS",
    ),
    localState: localState(env),
    peerRequestTimeoutMs: positiveInt(
      env.DA_PEER_REQUEST_TIMEOUT_MS ?? "5000",
      "DA_PEER_REQUEST_TIMEOUT_MS",
    ),
    peerReplayWindowMs: positiveInt(
      env.DA_PEER_REPLAY_WINDOW_MS ?? "300000",
      "DA_PEER_REPLAY_WINDOW_MS",
    ),
    peerMaxBodyBytes: positiveInt(
      env.DA_PEER_MAX_BODY_BYTES ?? "1048576",
      "DA_PEER_MAX_BODY_BYTES",
    ),
    peerRetryInitialDelayMs: positiveInt(
      env.DA_PEER_RETRY_INITIAL_DELAY_MS ?? "1000",
      "DA_PEER_RETRY_INITIAL_DELAY_MS",
    ),
    peerRetryMaxDelayMs: positiveInt(
      env.DA_PEER_RETRY_MAX_DELAY_MS ?? "60000",
      "DA_PEER_RETRY_MAX_DELAY_MS",
    ),
    peerRetryMaxAttempts: positiveInt(
      env.DA_PEER_RETRY_MAX_ATTEMPTS ?? "12",
      "DA_PEER_RETRY_MAX_ATTEMPTS",
    ),
    peerRateLimitWindowMs: positiveInt(
      env.DA_PEER_RATE_LIMIT_WINDOW_MS ?? "60000",
      "DA_PEER_RATE_LIMIT_WINDOW_MS",
    ),
    peerRateLimitMaxRequests: positiveInt(
      env.DA_PEER_RATE_LIMIT_MAX_REQUESTS ?? "120",
      "DA_PEER_RATE_LIMIT_MAX_REQUESTS",
    ),
    apiHost: env.DA_COMMITTEE_API_HOST ?? "127.0.0.1",
    apiPort: positiveInt(
      env.DA_COMMITTEE_API_PORT ?? "8787",
      "DA_COMMITTEE_API_PORT",
    ),
    pollIntervalMs,
    l1ViewFatalMs,
    ...(retentionAlertThresholdRaw === undefined
      ? {}
      : {
          retentionAlertThresholdMs: requireRetentionAlertThresholdMs(
            nonNegativeInt(
              retentionAlertThresholdRaw,
              "DA_RETENTION_ALERT_THRESHOLD_MS",
            ),
            "DA_RETENTION_ALERT_THRESHOLD_MS",
          ),
        }),
  };
};
