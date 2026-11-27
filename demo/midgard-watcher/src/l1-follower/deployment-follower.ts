import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import { parseFaultProofBlueprint } from "@al-ft/midgard-sdk/core";
import { buildComputationThreadValidator } from "@al-ft/midgard-sdk/deployment";

import type { WatcherFaultProofL1 } from "../fault-proofs/fault-proof-application.production-dependencies.js";
import type { WatcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.parse-trust-roots.js";
import {
  createWatcherFaultProofL1Source,
  WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX,
} from "./fault-proof-l1-source.js";
import {
  openWatcherFollowerRuntime,
  type WatcherFollowerRuntime,
  type WatcherFollowerRuntimeInput,
} from "./follower-runtime.js";
import { parseHubOracleOneShot } from "./hub-oracle-one-shot.js";
import { createWatcherPendingRead } from "./pending-read.js";

export type WatcherDeploymentFollower = WatcherFollowerRuntime &
  Readonly<{
    /**
     * The fault-proof families' L1: follower sources and a provider over
     * the same store, both reading the node's mempool applied to the
     * follower's facts (`pending-read.ts`).
     */
    faultProofL1: WatcherFaultProofL1;
  }>;

const HEX_28 = /^[0-9a-f]{56}$/u;

const point = (origin: L1Origin) => ({
  slot: origin.slot,
  hash: Buffer.from(origin.blockHash, "hex"),
});

/**
 * The computation-thread policy, which no manifest contract names: the
 * blueprint's minting script applied to the catalogue and hub-oracle
 * policies, as the fault-proof families derive it.
 */
export const watcherComputationThreadPolicyId = (
  input: Readonly<{
    /** The verified manifest contracts' script hashes, by contract name. */
    contractScriptHashes: Readonly<Record<string, string>>;
    /** The deployment blueprint (parsed JSON). */
    blueprint: unknown;
  }>,
): string => {
  const hashes = input.contractScriptHashes;
  const catalogue = hashes.fraudProofCatalogueMint;
  const hubOracle = hashes.hubOracleMint;
  if (catalogue === undefined || hubOracle === undefined)
    throw new Error(
      "the deployment names no fraudProofCatalogueMint or hubOracleMint script",
    );
  return buildComputationThreadValidator(
    parseFaultProofBlueprint(input.blueprint),
    {
      fraudProofCatalogue: { policyId: catalogue },
      hubOracle: { policyId: hubOracle },
    },
  ).policyId;
};

/**
 * Every script the watcher follows beyond the projection's named ones: each
 * manifest contract's script hash, and the computation-thread policy
 * (`watcherComputationThreadPolicyId`). The fault-proof families' snapshot
 * scopes and unit histories are all among them.
 */
export const watcherFollowedScripts = (
  input: Parameters<typeof watcherComputationThreadPolicyId>[0],
): readonly string[] => {
  const followed = [
    ...new Set([
      ...Object.values(input.contractScriptHashes),
      watcherComputationThreadPolicyId(input),
    ]),
  ];
  const malformed = followed.find((hash) => !HEX_28.test(hash));
  if (malformed !== undefined)
    throw new Error(`followed script ${malformed} is not a script hash`);
  return followed.sort();
};

/**
 * The follower of one verified deployment: its protocol scripts and every
 * followed script (`watcherFollowedScripts`) are the projection, its
 * hub-oracle one-shot bounds the origin, and every fault-proof sourceId
 * keeps the watcher's provenance prefix. An absent origin opens the
 * follower unready (`l1_origin_not_configured`). The origin is `l1.origin`,
 * else the manifest's recorded `l1Origin`; a recorded origin is also the
 * point the store's origin must be at or before (R3).
 */
export const openWatcherDeploymentFollower = (
  input: Readonly<{
    authority: Pick<
      WatcherDeploymentProtocolScriptAuthority,
      | "network"
      | "protocolScriptHashes"
      | "hubOracleOneShotOutRef"
      | "recordedL1Origin"
    >;
    origin: L1Origin | undefined;
    followedScripts: readonly string[];
  }> &
    Omit<WatcherFollowerRuntimeInput, "deployment" | "origin">,
): WatcherDeploymentFollower => {
  const hashes = input.authority.protocolScriptHashes;
  const recorded = input.authority.recordedL1Origin;
  const origin = input.origin ?? recorded;
  const follower = openWatcherFollowerRuntime({
    ...input,
    deployment: {
      network: input.authority.network,
      stateQueueSpend: hashes.stateQueueSpend,
      stateQueueMint: hashes.stateQueueMint,
      correctionLockSpend: hashes.correctionLockSpend,
      hubOracleMint: hashes.hubOracleMint,
      fraudProofSpend: hashes.fraudProofSpend,
      fraudProofMint: hashes.fraudProofMint,
      availabilityChallengeSpend: hashes.availabilityChallengeSpend,
      availabilityChallengeMint: hashes.availabilityChallengeMint,
      daBondPoolSpend: hashes.daBondPoolSpend,
      daAttestationMint: hashes.daAttestationMint,
      followedScripts: input.followedScripts,
    },
    origin:
      origin === undefined
        ? null
        : {
            origin: point(origin),
            hubOracleOneShot: parseHubOracleOneShot(
              input.authority.hubOracleOneShotOutRef,
            ),
            ...(recorded === undefined
              ? {}
              : { recordedOrigin: point(recorded) }),
          },
  });
  // One read for captures and builds: a build reads the mempool listing
  // the pass derived its action from.
  const pending = createWatcherPendingRead({
    store: follower.store,
    node: follower.transport,
    nodeTip: () => follower.provider.readTip(),
  });
  return Object.freeze({
    ...follower,
    faultProofL1: Object.freeze({
      source: (sourceId: string) =>
        createWatcherFaultProofL1Source({
          store: follower.store,
          rawReads: follower.rawReads,
          pending,
          sourceId: `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}${sourceId}`,
          proofRetention: follower.proofRetention,
        }),
      provider: new L1FollowerProvider({
        store: follower.store,
        transport: follower.transport,
        pending: pending.latest,
      }),
    }),
  });
};
