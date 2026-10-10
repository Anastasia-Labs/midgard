import { join } from "node:path";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import {
  openWatcherDeploymentFollower,
  watcherFollowedScripts,
} from "midgard-watcher";

import type { JourneyContext } from "./fixture.js";

const CATCH_UP_TIMEOUT_MS = 10 * 60_000;

type FollowerInput = Parameters<typeof openWatcherDeploymentFollower>[0];

/** The follower authority of a journey's deployment manifest. */
export const journeyFollowerAuthority = (
  manifest: JourneyContext["deployment"]["manifest"],
): FollowerInput["authority"] => {
  const contracts = manifest.contracts;
  return {
    network: manifest.network,
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    protocolScriptHashes: {
      hubOracleMint: contracts.hubOracleMint.scriptHash,
      stateQueueSpend: contracts.stateQueueSpend.scriptHash,
      stateQueueMint: contracts.stateQueueMint.scriptHash,
      correctionLockSpend: contracts.correctionLockSpend.scriptHash,
      fraudProofSpend: contracts.fraudProofSpend.scriptHash,
      fraudProofMint: contracts.fraudProofMint.scriptHash,
      referenceScriptAuthMint: contracts.referenceScriptAuthMint.scriptHash,
      availabilityChallengeSpend:
        contracts.availabilityChallengeSpend.scriptHash,
      availabilityChallengeMint: contracts.availabilityChallengeMint.scriptHash,
      daBondPoolSpend: contracts.daBondPoolSpend.scriptHash,
      daBondPoolMint: contracts.daBondPoolMint.scriptHash,
      daAttestationMint: contracts.daAttestationMint.scriptHash,
      availabilityChallengeOpenWithdraw:
        contracts.availabilityChallengeOpenWithdraw.scriptHash,
      availabilityChallengeSettleWithdraw:
        contracts.availabilityChallengeSettleWithdraw.scriptHash,
      availabilityChallengeCloseWithdraw:
        contracts.availabilityChallengeCloseWithdraw.scriptHash,
      availabilityChallengeTimeoutWithdraw:
        contracts.availabilityChallengeTimeoutWithdraw.scriptHash,
    },
  };
};

/** The scripts a journey deployment's follower follows, as the watcher's do. */
export const journeyFollowedScripts = (
  deployment: Pick<JourneyContext["deployment"], "manifest" | "blueprintJson">,
): readonly string[] =>
  watcherFollowedScripts({
    contractScriptHashes: Object.fromEntries(
      Object.entries(deployment.manifest.contracts).map(([name, contract]) => [
        name,
        contract.scriptHash,
      ]),
    ),
    blueprint: JSON.parse(deployment.blueprintJson),
  });

/** The run's devnet node, as the journey reaches it. */
export const journeyFollowerNode = (
  context: Pick<JourneyContext, "runDirectory" | "customNetwork">,
): FollowerInput["node"] => ({
  binaryPath: join(context.runDirectory, "work/midgard-l1-node-transport"),
  socketPath: join(context.runDirectory, "cardano/ipc/node.socket"),
  networkMagic: context.customNetwork.networkMagic,
  // The bound the journey session configures for the run watcher's l1.
  requestTimeoutMs: 30_000,
});

/**
 * A journey's own watcher follower over the run's devnet node: the
 * fault-proof families' L1 sources and provider, exactly as the installed
 * watcher builds them. It follows from the run watcher's configured
 * `l1.origin` (the devnet stack must set it; without one the watcher itself
 * is unready) and resolves once its cursor reaches the node tip. Its store
 * is its own, so it never contends for the installed watcher's lease.
 */
export const openJourneyFollowerL1 = async (input: {
  authority: FollowerInput["authority"];
  followedScripts: FollowerInput["followedScripts"];
  node: FollowerInput["node"];
  origin: L1Origin | undefined;
  automaticRecoveryMaxDepth: number;
  storeDirectory: string;
}) => {
  if (input.origin === undefined)
    throw new Error(
      "The run watcher config has no l1.origin: the journey follower needs the point before the prepareHubOracleNonce block",
    );
  const follower = openWatcherDeploymentFollower({
    authority: input.authority,
    origin: input.origin,
    followedScripts: input.followedScripts,
    storePath: join(input.storeDirectory, "journey-l1-follower.sqlite"),
    automaticRecoveryMaxDepth: input.automaticRecoveryMaxDepth,
    node: input.node,
    walletAddresses: [],
  });
  /** Resolves once the cursor reaches the node tip after `aboveHeight`. */
  const atTip = (aboveHeight = -1) =>
    new Promise<void>((resolve, reject) => {
      const reached = () => {
        const status = follower.status();
        return (
          status !== null &&
          status.atTip &&
          (status.cursor?.height ?? -1) > aboveHeight
        );
      };
      if (reached()) return resolve();
      const timer = setTimeout(() => {
        unsubscribe();
        reject(
          new Error(
            `The journey follower did not reach the node tip: ${JSON.stringify(follower.status())}`,
          ),
        );
      }, CATCH_UP_TIMEOUT_MS);
      const unsubscribe = follower.onChange(() => {
        if (!reached()) return;
        clearTimeout(timer);
        unsubscribe();
        resolve();
      });
    });
  try {
    await atTip();
  } catch (error) {
    await follower.close();
    throw error;
  }
  return {
    l1: follower.faultProofL1,
    /** The cursor height the follower has applied. */
    height: () => follower.status()?.cursor?.height ?? -1,
    atTip,
    close: () => follower.close(),
  };
};
