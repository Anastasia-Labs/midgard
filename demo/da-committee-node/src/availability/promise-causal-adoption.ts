import { createHash } from "node:crypto";
import { arch, cpus, platform, totalmem } from "node:os";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { isPlainRecord } from "@al-ft/midgard-core/narrowing";
import type { View } from "@al-ft/midgard-l1-follower";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import type { ProtocolParameters, SlotConfig } from "@lucid-evolution/lucid";

import {
  type CommitteeL1ClientConfig,
  l1SourceAuthorityDigest,
} from "../config.js";
import { protocolParametersDigest } from "../l1/follower/availability-reads.js";
import { loadTrustedPromiseEvidence } from "./promise-adoption-evidence.js";
import {
  type CommitteePromiseCausalArtifact,
  type CommitteePromiseSourceView,
  verifyCommitteePromiseCausalPolicy,
} from "./promise-causal-policy.js";
import { committeePromiseNetworkClock } from "./promise-network-clock.js";
import { committeePromiseJoinedReads } from "./promise-owned-read.js";
import type { CommitteePromiseStageEnforcement } from "./promise-runtime-policy.js";

const digest = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJson(value, "configured promise runtime evidence"))
    .digest("hex");
const record = (value: unknown, label: string): Record<string, unknown> => {
  if (!isPlainRecord(value)) throw new Error(`${label} must be an object`);
  return value;
};

/**
 * What the adopted profile reads from the committee's own node: the protocol
 * parameters and slot configuration by local state query, and whether a view
 * of the committee's L1 follower is still current (plan §8.1).
 */
export type CommitteePromiseLedger = Readonly<{
  protocolParameters: () => Promise<ProtocolParameters>;
  slotConfig: () => Promise<SlotConfig>;
  viewValid: (view: View) => Promise<boolean>;
}>;

/** Captures actual host identity as a conditional resource assumption. It does
 * not claim OS scheduling or CPU reclamation is deterministically bounded. */
export const committeePromiseResourceProfile = (input: {
  config: CommitteeL1ClientConfig;
  actorId: string;
  storeBackend: string;
}) => ({
  schemaVersion: 3,
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
    authorityDigest: l1SourceAuthorityDigest(input.config),
    networkMagic: input.config.cardanoL1Source.networkMagic,
  },
  runtime: {
    pollIntervalMs: input.config.pollIntervalMs,
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
    l1Reads: "committee_follower_facts_and_local_state_query",
  },
});

/**
 * The chain's fixed timing identity: its network magic and the node's slot
 * configuration (system start and era history, by local state query). The
 * calibration and fault evidence pin it, and every source fence re-reads it.
 */
const genesisDigestOf = (networkMagic: number, slotConfig: SlotConfig) =>
  digest({
    networkMagic,
    zeroTime: slotConfig.zeroTime,
    zeroSlot: slotConfig.zeroSlot,
    slotLength: slotConfig.slotLength,
  });

const sourceView = (view: CommitteePromiseSourceView): View => ({
  generation: view.generation,
  point: { slot: view.slot, hash: Buffer.from(view.blockHash, "hex") },
  height: 0,
});

/** Evidence pins grant an explicitly configured private capability only after
 * the factory supplies installed stage owners and current source authority. */
export const loadCommitteePromiseCausalAdoption = async (input: {
  config: CommitteeL1ClientConfig;
  actorId: string;
  storeBackend: string;
  runtimeBuildDigest: string;
  installedEnforcement: readonly CommitteePromiseStageEnforcement[];
  ledger: CommitteePromiseLedger;
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
    input.config.pollIntervalMs > 15000
  )
    throw new Error(
      "Committee causal profile requires its measured private runtime domain",
    );
  const { ledger, scope } = input;
  const profile = committeePromiseResourceProfile(input);
  const [
    policyRaw,
    profileRaw,
    calibrationRaw,
    faultRaw,
    slotConfig,
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
    scope.read(() => ledger.slotConfig()),
    scope.read(async () =>
      protocolParametersDigest(await ledger.protocolParameters()),
    ),
  ]);
  scope.assertCurrent();
  const networkMagic = input.config.cardanoL1Source.networkMagic;
  const genesisDigest = genesisDigestOf(networkMagic, slotConfig);
  // One encoding is required for policy/profile: raw pin equals canonical bytes.
  if (
    digest(profileRaw) !== adoption.trustedResourceProfileDigest ||
    canonicalJson(profileRaw, "loaded resource profile") !==
      canonicalJson(profile, "running resource profile")
  )
    throw new Error("Measured resource profile does not match this runtime");
  // The calibration and fault evidence below pin this chain's timing by its
  // digest; the node's ledger state does not expose the active-slot
  // coefficient, so the adopted fault model carries it.
  if (slotConfig.slotLength !== 1000)
    throw new Error("Causal model does not match the node's slot length");
  const calibration = record(calibrationRaw, "Causal calibration evidence");
  if (
    calibration.schemaVersion !== 3 ||
    calibration.runtimeBuildDigest !== input.runtimeBuildDigest ||
    calibration.resourceProfileDigest !==
      adoption.trustedResourceProfileDigest ||
    calibration.genesisDigest !== genesisDigest ||
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
    fault.schemaVersion !== 3 ||
    fault.mode !== "private_controlled_devnet" ||
    fault.readyValidTransactionInNextEligibleCanonicalActiveSlot !== true ||
    fault.noRollbackDuringResponseInterval !== true ||
    fault.initialProducerMemberSkewMs !==
      profile.clock.initialProducerMemberSkewMs ||
    fault.genesisDigest !== genesisDigest ||
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
    slotConfig,
    initialProducerMemberSkewMs: profile.clock.initialProducerMemberSkewMs,
    maxWallMonotonicDriftMs: profile.clock.maximumWallMonotonicDriftMs,
    nowMs: Date.now,
    monotonicMs: () => performance.now(),
  });
  // The artifact's follower view stays current while no rollback undid its
  // point (§8.1). Every source fence re-reads it; the policy's synchronous
  // epoch check reads the last answer.
  const pinnedView = record(
    record(artifact.sourceBinding, "Causal policy source binding").view,
    "Causal policy source view",
  ) as CommitteePromiseSourceView;
  let viewCurrent = false;
  const refreshView = async (fence: DaAvailabilityReadScope) => {
    viewCurrent = await fence.read(() =>
      ledger.viewValid(sourceView(pinnedView)),
    );
    if (!viewCurrent) throw new Error("Adopted follower view was rolled back");
  };
  await refreshView(scope);
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
      genesisDigest,
      view: pinnedView,
    },
    installedEnforcement: input.installedEnforcement,
    verifiedCalibrationEvidenceDigest:
      adoption.trustedCalibrationEvidenceDigest,
    adoptedFaultModelDigest: adoption.trustedFaultModelDigest,
    upperNetworkTimeMs: () => clock.upperTimeMs(3000),
    assertEpochCurrent: () => {
      if (!viewCurrent)
        throw new Error("Adopted follower view was rolled back");
    },
    maximumWallMonotonicDriftMs: profile.clock.maximumWallMonotonicDriftMs,
  });
  if (authority.status().status !== "conditional")
    throw new Error("Configured causal adoption did not verify");
  const readProtocolDigest = async (fence: DaAvailabilityReadScope) => {
    const fresh = genesisDigestOf(
      networkMagic,
      await fence.read(() => ledger.slotConfig()),
    );
    if (fresh !== genesisDigest) {
      authority.breach("fixed_genesis_changed");
      throw new Error("Fixed genesis changed before signing");
    }
    await refreshView(fence);
    clock.upperTimeMs(3000);
    return protocolParametersDigest(
      await fence.read(() => ledger.protocolParameters()),
    );
  };
  const slotTimeMs = (slot: number): number => {
    const time =
      slotConfig.zeroTime +
      (slot - slotConfig.zeroSlot) * slotConfig.slotLength;
    if (
      !Number.isSafeInteger(slot) ||
      slot < slotConfig.zeroSlot ||
      !Number.isSafeInteger(time)
    )
      throw new Error("Native slot time exceeds the node's slot configuration");
    return time;
  };
  return { authority, clock, readProtocolDigest, slotTimeMs };
};
