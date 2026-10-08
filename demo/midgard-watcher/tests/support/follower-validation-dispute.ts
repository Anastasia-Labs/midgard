/**
 * Adapters from the installed validation-trace dispute journey
 * (`@al-ft/midgard-fault-proofs/test-support/installed-validation-trace-dispute-journey`)
 * to the watcher's follower-sourced capture: a synthetic origin deployment
 * that follows the journey's state queue, a follower store fed the emulator's
 * own confirmed transactions in their confirmed blocks, and the watcher's
 * classification and transcript capture of the journey's committed header.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import type { FollowerValidationDisputeFixture } from "@al-ft/midgard-fault-proofs/test-support/installed-validation-follower-fixture";
import type { InstalledValidationJourneyStaged } from "@al-ft/midgard-fault-proofs/test-support/installed-validation-trace-dispute-journey";

import { captureWatcherValidationReplayTranscript } from "../../src/fault-proofs/replay-transcript-capture.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "./deployment-authority-fixture.js";
import {
  followerUserEventsDeploymentOf,
  openFollowerUserEvents,
} from "./follower-user-events-fixture.js";
import { buildBlock } from "./user-event-origin-fixture.build-block.js";
import {
  makeOriginDeployment,
  type Point,
  pointAt,
  type SyntheticUserEventBlock,
} from "./user-event-origin-fixture.make-config.js";
import { classifyRetainedValidationHeader } from "./validation-capture-fixture.js";

type Staged =
  InstalledValidationJourneyStaged<FollowerValidationDisputeFixture>;

/**
 * The synthetic origin deployment, re-pointed at the journey's state queue
 * and correction lock, and its verified authority as the watcher loads it.
 */
export const loadJourneyWatcherDeployment = async (
  contracts: Staged["contracts"],
) => {
  const scripts = {
    stateQueueMint: contracts.stateQueue.mintingScript,
    stateQueueSpend: contracts.stateQueue.spendingScript,
    correctionLockSpend: contracts.correctionLock.spendingScript,
  };
  const identity = makeOriginDeployment(undefined, scripts).result;
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: identity.manifestId,
      network: identity.network,
      blueprintHash: identity.blueprintHash,
      programCommitments: identity.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  const followed = followerUserEventsDeploymentOf(
    makeOriginDeployment(
      computeWatcherRuleBundleCommitment(ruleBundle),
      scripts,
    ),
  );
  const directory = await mkdtemp("/var/tmp/follower-validation-dispute-");
  try {
    const authorityPath = join(directory, "authority.json");
    const ruleBundlePath = join(directory, "rules.json");
    const { deployment } = followed;
    await writeFile(
      authorityPath,
      JSON.stringify({
        signedIdentity: deployment.signedIdentity,
        policy: deployment.policy,
        trustRoots: deployment.trustRoots,
        durableMarker: deployment.marker,
      }),
    );
    await writeFile(ruleBundlePath, JSON.stringify(ruleBundle));
    return {
      followed,
      deploymentAuthority: await loadWatcherVerifiedDeploymentAuthority({
        path: authorityPath,
        ruleBundlePath,
      }),
    };
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
};

/**
 * The emulator's confirmed transactions, in submission order, framed as the
 * blocks the emulator confirmed them in (one frame per confirmed slot and
 * height, at that slot), then `RELEASE_FINALITY_DEPTH` empty blocks so the
 * last of them is release-final. The origin sits one slot before the first.
 */
export const emulatorFollowerChain = async (
  staged: Pick<Staged, "emulator" | "recorder">,
) => {
  const groups: { slot: number; height: number; transactions: string[] }[] = [];
  for (const [hash, cbor] of staged.recorder.signedCbors) {
    const status = await staged.emulator.getTransactionStatus(hash);
    if (
      status.status !== "confirmed" ||
      status.confirmation.slot === undefined ||
      status.confirmation.blockHeight === undefined
    )
      throw new Error(`Emulator transaction ${hash} is not confirmed`);
    const { slot, blockHeight: height } = status.confirmation;
    const previous = groups.at(-1);
    if (previous?.slot === slot && previous.height === height)
      previous.transactions.push(cbor);
    else groups.push({ slot, height, transactions: [cbor] });
  }
  const first = groups[0];
  if (first === undefined)
    throw new Error("The emulator confirmed no transaction");
  const origin: Point = pointAt("ab".repeat(32), 0n, BigInt(first.slot - 1));
  const blocks: SyntheticUserEventBlock[] = [];
  const tip = () => blocks.at(-1)?.point ?? origin;
  for (const group of groups)
    blocks.push(
      buildBlock(
        group.transactions,
        tip(),
        BigInt(group.slot) - BigInt(tip().slot),
      ),
    );
  for (let index = 0; index < RELEASE_FINALITY_DEPTH; index += 1)
    blocks.push(buildBlock([], tip()));
  return { origin, blocks };
};

/**
 * Follows the staged emulator chain, reads the release-final state-queue
 * observation, classifies the journey's committed header from it and
 * captures its replay transcript. A capture is the watcher's challenge; an
 * honest header's healthy decision has none.
 */
export const followJourneyValidationDecision = async (staged: Staged) => {
  const { followed, deploymentAuthority } = await loadJourneyWatcherDeployment(
    staged.contracts,
  );
  const chain = await emulatorFollowerChain(staged);
  const follower = await openFollowerUserEvents({
    deployment: followed,
    origin: chain.origin,
  });
  try {
    await follower.apply(chain.blocks);
    const observation = await follower.observe();
    const header = observation.finalizedHeaders.find(
      (item) => item.headerHash === staged.setup.headerHash,
    );
    if (header === undefined)
      throw new Error("The follower did not observe the journey's header");
    const context = {
      retained: staged.fixture.retained,
      deploymentAuthority,
    };
    const classify = () =>
      classifyRetainedValidationHeader(context, { observation, header });
    const capture = async (
      decision: Parameters<
        typeof captureWatcherValidationReplayTranscript
      >[0]["decision"],
    ) =>
      await captureWatcherValidationReplayTranscript({
        deploymentAuthority,
        stateQueueObservation: observation,
        header,
        decision,
        userEvents: follower.userEvents,
      });
    return {
      deploymentAuthority,
      follower,
      observation,
      header,
      classify,
      capture,
    };
  } catch (error) {
    await follower.close();
    throw error;
  }
};
