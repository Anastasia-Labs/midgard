import { createHash } from "node:crypto";
import { arch, cpus, platform, totalmem } from "node:os";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { parseOgmiosShelleyGenesisSlotConfig } from "@al-ft/midgard-core/ogmios-slot";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import {
  type CommitteeL1ClientConfig,
  l1SourceAuthorityDigest,
} from "../config.js";
import { committeeScopedProtocolDigest } from "../l1/availability-scoped-protocol.js";
import { assertNetworkMagic } from "../l1/provider.ogmios-rpc-session.js";
import {
  getRecord,
  safeSlot,
} from "../l1/provider.parse-persisted-chain-sync-state.js";
import { loadTrustedPromiseEvidence } from "./promise-adoption-evidence.js";
import {
  type CommitteePromiseCausalArtifact,
  verifyCommitteePromiseCausalPolicy,
} from "./promise-causal-policy.js";
import { committeePromiseNetworkClock } from "./promise-network-clock.js";
import { committeePromiseJoinedReads } from "./promise-owned-read.js";
import type { CommitteePromiseStageEnforcement } from "./promise-runtime-policy.js";
import {
  committeeScopedOgmiosRpc,
  type CommitteeSourceReadLimits,
} from "./scoped-transports.js";

const digest = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJson(value, "configured promise runtime evidence"))
    .digest("hex");
const record = (value: unknown, label: string) => getRecord(value, label);
const loopback = (url: string) =>
  ["127.0.0.1", "localhost", "[::1]"].includes(new URL(url).hostname);

/** Captures actual host identity as a conditional resource assumption. It does
 * not claim OS scheduling or CPU reclamation is deterministically bounded. */
export const committeePromiseResourceProfile = (input: {
  config: CommitteeL1ClientConfig;
  actorId: string;
  storeBackend: string;
  kupoUrl: string;
  ogmiosUrl: string;
}) => ({
  schemaVersion: 2,
  mode: "private_controlled_devnet",
  host: {
    platform: platform(),
    architecture: arch(),
    processors: cpus().map((cpu) => cpu.model),
    totalMemoryBytes: totalmem(),
  },
  actorId: input.actorId,
  actorSharing: "exclusive_committee_responder_journal",
  journalBackend: "sqlite_wal",
  storeBackend: input.storeBackend,
  source: {
    kupoUrl: input.kupoUrl,
    ogmiosUrl: input.ogmiosUrl,
    authorityDigest: l1SourceAuthorityDigest(
      input.config.network,
      input.config.l1Source,
    ),
    networkMagic: input.config.cardanoL1Source.networkMagic,
  },
  runtime: {
    pollIntervalMs: input.config.pollIntervalMs,
    cursorEvents: 32,
    sourceRead: {
      requestRefusalMs: 10000,
      httpResponseBytes: 4194304,
      webSocketMessageBytes: 4194304,
      rawUtxos: 1024,
    },
  },
  clock: {
    initialProducerMemberSkewMs: 2000,
    maximumWallMonotonicDriftMs: 2000,
    condition: "owner_adopted_same_host_clock_alignment",
  },
  ownership: {
    storeDurability: "existing_exclusive_store_lease",
    storeReads: "direct_await_local_completion_with_late_result_fence",
    journalDurability: "existing_atomic_sqlite_journal",
    providerRequests: "owned_instance_http_with_local_join",
    webSocketByteLimit: "receiver_max_payload_and_pre_parse_fence",
    localConnectionCeilings: {
      httpOrigins: 2,
      httpPerOrigin: 4,
      webSockets: 8,
      total: 16,
    },
  },
});

/** Fresh genesis identity is checked again at every admission source fence. */
export const readCommitteePromiseGenesis = async (input: {
  config: CommitteeL1ClientConfig;
  ogmiosUrl: string;
  limits: CommitteeSourceReadLimits;
  scope: DaAvailabilityReadScope;
}) => {
  const rpc = await committeeScopedOgmiosRpc(
    input.ogmiosUrl,
    input.scope,
    input.limits,
  );
  try {
    const raw = getRecord(
      await rpc.request("queryNetwork/genesisConfiguration", {
        era: "shelley",
      }),
      "Promise admission Shelley genesis",
    );
    assertNetworkMagic(
      input.config.network,
      safeSlot(
        raw.networkMagic ?? raw.network_magic,
        "Promise genesis network magic",
      ),
      "Ogmios",
      input.config.cardanoL1Source.networkMagic,
    );
    return parseOgmiosShelleyGenesisSlotConfig({ result: raw });
  } finally {
    rpc.close();
  }
};

/** Evidence pins grant an explicitly configured private capability only after
 * the factory supplies installed stage owners and current source authority. */
export const loadCommitteePromiseCausalAdoption = async (input: {
  config: CommitteeL1ClientConfig;
  actorId: string;
  storeBackend: string;
  kupoUrl: string;
  ogmiosUrl: string;
  runtimeBuildDigest: string;
  installedEnforcement: readonly CommitteePromiseStageEnforcement[];
  currentRollbackGeneration: () => number;
  scope: DaAvailabilityReadScope;
}) => {
  const adoption = input.config.availabilityPromiseAdoption;
  if (!adoption)
    throw new Error("Explicit committee promise adoption is absent");
  if (
    input.config.network !== "Custom" ||
    input.config.finalityDepth !== 10 ||
    input.config.automaticRecoveryMaxDepth !== 2160 ||
    input.config.pollIntervalMs <= 0 ||
    input.config.pollIntervalMs > 15000 ||
    !loopback(input.kupoUrl) ||
    !loopback(input.ogmiosUrl)
  )
    throw new Error(
      "Committee causal profile requires its measured private runtime domain",
    );
  const profile = committeePromiseResourceProfile(input);
  const limits = profile.runtime.sourceRead;
  const [
    policyRaw,
    profileRaw,
    calibrationRaw,
    faultRaw,
    genesis,
    protocolDigest,
  ] = await committeePromiseJoinedReads([
    loadTrustedPromiseEvidence(
      adoption.policyArtifactPath,
      adoption.trustedPolicyDigest,
    ),
    loadTrustedPromiseEvidence(
      adoption.resourceProfilePath,
      adoption.trustedResourceProfileDigest,
    ),
    loadTrustedPromiseEvidence(
      adoption.calibrationEvidencePath,
      adoption.trustedCalibrationEvidenceDigest,
    ),
    loadTrustedPromiseEvidence(
      adoption.faultModelPath,
      adoption.trustedFaultModelDigest,
    ),
    readCommitteePromiseGenesis({ ...input, limits }),
    committeeScopedProtocolDigest({ ...input, limits })(input.scope),
  ]);
  input.scope.assertCurrent();
  // One encoding is required for policy/profile: raw pin equals canonical bytes.
  if (
    digest(profileRaw) !== adoption.trustedResourceProfileDigest ||
    canonicalJson(profileRaw, "loaded resource profile") !==
      canonicalJson(profile, "running resource profile")
  )
    throw new Error("Measured resource profile does not match this runtime");
  if (
    genesis.slotLengthMs !== 1000 ||
    genesis.activeSlotsCoefficient !== 1 / 20
  )
    throw new Error("Causal model does not match authenticated genesis");
  const calibration = record(calibrationRaw, "Causal calibration evidence");
  if (
    calibration.schemaVersion !== 2 ||
    calibration.runtimeBuildDigest !== input.runtimeBuildDigest ||
    calibration.resourceProfileDigest !==
      adoption.trustedResourceProfileDigest ||
    calibration.genesisDigest !== genesis.configurationSha256 ||
    calibration.protocolDigest !== protocolDigest ||
    typeof calibration.maximumCompleteSourceMs !== "number" ||
    calibration.maximumCompleteSourceMs <= 0 ||
    calibration.maximumCompleteSourceMs > 10000 ||
    typeof calibration.maximumBuildSignPersistMs !== "number" ||
    calibration.maximumBuildSignPersistMs <= 0 ||
    calibration.maximumBuildSignPersistMs > 2000 ||
    calibration.completeMaximumResourceDomain !== true
  )
    throw new Error(
      "Complete source and software calibration evidence is unavailable",
    );
  const fault = record(faultRaw, "Adopted private chain assumptions");
  if (
    fault.schemaVersion !== 2 ||
    fault.mode !== "private_controlled_devnet" ||
    fault.readyValidTransactionInNextEligibleCanonicalActiveSlot !== true ||
    fault.noRollbackDuringResponseInterval !== true ||
    fault.initialProducerMemberSkewMs !==
      profile.clock.initialProducerMemberSkewMs ||
    fault.genesisDigest !== genesis.configurationSha256 ||
    fault.sourceDigest !== profile.source.authorityDigest
  )
    throw new Error(
      "Explicit controlled chain and clock assumptions are unavailable",
    );
  const artifact = record(
    policyRaw,
    "Causal policy artifact",
  ) as CommitteePromiseCausalArtifact;
  const clock = committeePromiseNetworkClock({
    slotConfig: {
      zeroSlot: 0,
      zeroTime: genesis.startTimeMs,
      slotLength: genesis.slotLengthMs,
    },
    initialProducerMemberSkewMs: profile.clock.initialProducerMemberSkewMs,
    maxWallMonotonicDriftMs: profile.clock.maximumWallMonotonicDriftMs,
    nowMs: Date.now,
    monotonicMs: () => performance.now(),
  });
  const generation = input.currentRollbackGeneration();
  const authority = verifyCommitteePromiseCausalPolicy({
    artifact,
    trustedPolicyDigest: adoption.trustedPolicyDigest,
    liveBinding: {
      deploymentFingerprint: input.config.deploymentFingerprint,
      contractManifestId: String(
        input.config.contractDeploymentInfo.manifestId,
      ),
      actorId: input.actorId,
      protocolDigest,
      runtimeBuildDigest: input.runtimeBuildDigest,
      resourceProfileDigest: adoption.trustedResourceProfileDigest,
    },
    liveSourceBinding: {
      sourceDigest: profile.source.authorityDigest,
      genesisDigest: genesis.configurationSha256,
      rollbackGeneration: generation,
    },
    installedEnforcement: input.installedEnforcement,
    verifiedCalibrationEvidenceDigest:
      adoption.trustedCalibrationEvidenceDigest,
    adoptedFaultModelDigest: adoption.trustedFaultModelDigest,
    upperNetworkTimeMs: () => clock.upperTimeMs(3000),
    assertEpochCurrent: () => {
      if (input.currentRollbackGeneration() !== generation)
        throw new Error("Adopted rollback generation changed");
    },
    maximumWallMonotonicDriftMs: profile.clock.maximumWallMonotonicDriftMs,
  });
  if (authority.status().status !== "conditional")
    throw new Error("Configured causal adoption did not verify");
  const readProtocolDigest = async (scope: DaAvailabilityReadScope) => {
    const fresh = await readCommitteePromiseGenesis({
      ...input,
      scope,
      limits,
    });
    if (fresh.configurationSha256 !== genesis.configurationSha256) {
      authority.breach("fixed_genesis_changed");
      throw new Error("Fixed genesis changed before signing");
    }
    clock.upperTimeMs(3000);
    return committeeScopedProtocolDigest({ ...input, limits })(scope);
  };
  const slotTimeMs = (slot: number): number => {
    const time = genesis.startTimeMs + slot * genesis.slotLengthMs;
    if (!Number.isSafeInteger(slot) || slot < 0 || !Number.isSafeInteger(time))
      throw new Error("Native slot time exceeds the verified genesis domain");
    return time;
  };
  return { authority, clock, limits, readProtocolDigest, slotTimeMs };
};
