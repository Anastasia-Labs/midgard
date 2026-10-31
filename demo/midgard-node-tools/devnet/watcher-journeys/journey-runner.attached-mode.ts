import { nodeLedger } from "../../src/full-stack/journey.js";
import { stackSuccessorPort } from "./honest-successor.js";
import { journeyNetwork } from "./journey-network.js";
import type { JourneyRunMode } from "./journey-runner.journey-mode.js";
import type { AttachedJourneySession } from "./journey-session.js";
import { readStackJourneyTiming } from "./journey-timing.js";
import {
  fetchJourneyPublicDaPayload,
  readStackJourneyWatcherConfig,
  STACK_PUBLIC_DA_TRAIL_MS,
  verifyStackJourneyPublicDa,
} from "./public-da-preflight.js";
import {
  stackJourneyTransactions,
  stackSignedCommitRecovery,
} from "./stack-transactions.js";

/**
 * An attached stack journey: the stack's own watcher, read through its
 * operations endpoint and its journals; confirmed transactions read off the
 * stack's Kupo and Ogmios; the watcher's configured prover, which the
 * journey never funds; and our own node building the honest successor.
 * Operator-2's node is held stopped from before staging until the run
 * resumes it.
 */
export const attachedJourneyMode = (
  session: AttachedJourneySession,
  directory: string,
): JourneyRunMode => {
  const { context, watcher, evidence, operator2 } = session;
  const transactions = stackJourneyTransactions({
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
    directory,
  });
  const deploymentIdentity = context.release.deploymentIdentity;
  let watcherConfig:
    | ReturnType<typeof readStackJourneyWatcherConfig>
    | undefined;
  // The stack watcher's own DA peers and request bounds, read once.
  const readWatcherConfig = () =>
    (watcherConfig ??= readStackJourneyWatcherConfig(
      context.watcher.processConfigPath,
    ));
  const fetchPayloadEnvelope = async (headerHash: string) =>
    (
      await fetchJourneyPublicDaPayload({
        watcherConfig: await readWatcherConfig(),
        deploymentIdentity,
        headerHash,
        availability: { waitMs: STACK_PUBLIC_DA_TRAIL_MS },
      })
    ).payloadEnvelopeCbor;
  return {
    captureWorkflowBaseline: session.captureWorkflowBaseline,
    readTiming: (category, options) =>
      readStackJourneyTiming(
        {
          nodeConfigPath: context.layout.node.nodeConfigPath,
          manifestPath: context.layout.manifestPath,
        },
        category,
        options,
      ),
    stageFixture: async (fixture, { directory, onStage }) => {
      // The drill signs as operator-2 from here on; its node stays stopped.
      await operator2.hold();
      return await fixture.stage({
        context,
        directory,
        ledgerAt: session.ledgerAt,
        // The stack's committee retains payloads; the journey only proves
        // each commit is on the stack's chain.
        retain: async (_block, commitTxHash) => {
          await transactions.transaction(commitTxHash);
        },
        readConfirmedTransaction: transactions.transaction,
        readSignedCommitRecovery: async (attempt) =>
          stackSignedCommitRecovery({
            kupoUrl: context.kupoUrl,
            ogmiosUrl: context.ogmiosUrl,
            timeoutMs: (await readWatcherConfig()).l1.requestTimeoutMs,
          })(attempt),
        onStage,
      });
    },
    verifyPublicDa: async ({ directory, predecessor, current }) =>
      verifyStackJourneyPublicDa({
        directory,
        watcherConfig: await readWatcherConfig(),
        deploymentIdentity,
        predecessor,
        current,
      }),
    startWatcher: ({ onWatcher }) => {
      onWatcher(watcher);
      return Promise.resolve(watcher);
    },
    readDecisions: async () =>
      (await evidence.readDecisions()).map((decision) => ({ decision })),
    watcherStore: evidence,
    transactions,
    proverAddress: context.layout.proverAddress,
    successor: ({ directory, staged, polls }) =>
      stackSuccessorPort({
        directory,
        nodeEndpoint: context.stackConfig.endpoint,
        ledger: nodeLedger(context.stackConfig.endpoint),
        kupoUrl: context.kupoUrl,
        stateQueuePolicyId: context.deployment.contracts.stateQueue.policyId,
        honestOperatorKeyHash: context.layout.honestOperatorKeyHash,
        ledgerOwner: context.accounts.ledgerOwner,
        network: journeyNetwork().network,
        transferLovelace: BigInt(context.stackConfig.journey.transferLovelace),
        predecessor: staged.predecessor,
        fault: staged.current,
        fetchPayloadEnvelope,
        poll: polls.poll,
        // The stack journey's own wait for our node to commit a transfer.
        commitTimeoutMs: context.stackConfig.timeoutMs,
      }),
  };
};
