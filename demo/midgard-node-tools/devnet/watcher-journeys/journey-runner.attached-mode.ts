import { nodeLedger } from "../../src/full-stack/journey.js";
import type { JourneyBlock, JourneyFixture, StagedJourney } from "./fixture.js";
import { stackSuccessorPort } from "./honest-successor.js";
import { journeyNetwork } from "./journey-network.js";
import type { JourneyRunMode } from "./journey-runner.journey-mode.js";
import type { AttachedJourneySession } from "./journey-session.js";
import { readStackJourneyTiming } from "./journey-timing.js";
import {
  type JourneyChainTransactions,
  stackJourneyTransactions,
} from "./stack-transactions.js";

/**
 * How an attached stack journey stages a family and reads its public DA:
 * operator-2's blocks published through the stack's own committee.
 */
export type AttachedJourneyStaging = Readonly<{
  stage(
    fixture: JourneyFixture,
    input: Readonly<{
      directory: string;
      readConfirmedTransaction: JourneyChainTransactions["transaction"];
      onStage(name: string): void;
    }>,
  ): Promise<StagedJourney>;
  /** Proves the staged blocks' payloads are publicly retrievable. */
  verifyPublicDa(
    input: Readonly<{
      directory: string;
      predecessor: JourneyBlock;
      current: JourneyBlock;
    }>,
  ): Promise<unknown>;
  /** The public DA copy of a header's payload envelope, verified against it. */
  fetchPayloadEnvelope(headerHash: string): Promise<Uint8Array>;
}>;

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
  staging: AttachedJourneyStaging,
  directory: string,
): JourneyRunMode => {
  const { context, watcher, evidence, operator2 } = session;
  const transactions = stackJourneyTransactions({
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
    directory,
  });
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
      return await staging.stage(fixture, {
        directory,
        readConfirmedTransaction: transactions.transaction,
        onStage,
      });
    },
    verifyPublicDa: staging.verifyPublicDa,
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
        fetchPayloadEnvelope: staging.fetchPayloadEnvelope,
        poll: polls.poll,
        // The stack journey's own wait for our node to commit a transfer.
        commitTimeoutMs: context.stackConfig.timeoutMs,
      }),
  };
};
