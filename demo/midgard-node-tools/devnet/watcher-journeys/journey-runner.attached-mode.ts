import * as SDK from "@al-ft/midgard-sdk";

import { nodeLedger } from "../../src/full-stack/journey.js";
import { stackSuccessorPort } from "./honest-successor.js";
import { LANDED_BLOCK_AWAITING_DA } from "./honest-successor.observables.js";
import { stackTransferBeforeFault } from "./honest-successor.transfer.js";
import { journeyNetwork } from "./journey-network.js";
import type { JourneyRunMode } from "./journey-runner.journey-mode.js";
import type { AttachedJourneySession } from "./journey-session.js";
import { readStackJourneyTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { createNodeHoldWatch } from "./node-hold-watch.js";
import {
  fetchJourneyPublicDaPayload,
  readStackJourneyWatcherConfig,
  STACK_PUBLIC_DA_TRAIL_MS,
  verifyStackJourneyPublicDa,
} from "./public-da-preflight.js";
import { stackDaExpectation } from "./stack-da-publication.js";
import { stackJourneyDa } from "./stack-journey-da.js";
import { stackLateShiftWindowOpensMs } from "./stack-late-shift.js";
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
  // Our node's hold on the fault, watched from the fault's commit.
  const holdWatch = createNodeHoldWatch({
    directory,
    nodeEndpoint: context.stackConfig.endpoint,
  });
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
      try {
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
          onFaultCommitted: (fault) => {
            holdWatch.start(fault.headerHash);
          },
          // Our node holds its honest work from its first shift after the
          // fault: handed over in operator-2's shift, where our node cannot
          // commit, just before the fault's commit. A refused fault has no
          // successor.
          ...(stackDaExpectation(fixture.category) === "refusal"
            ? {}
            : {
                beforeFaultCommit: (predecessor) =>
                  stackTransferBeforeFault({
                    directory,
                    nodeEndpoint: context.stackConfig.endpoint,
                    ledger: nodeLedger(context.stackConfig.endpoint),
                    ledgerOwner: context.accounts.ledgerOwner,
                    network: journeyNetwork().network,
                    transferLovelace: BigInt(
                      context.stackConfig.journey.transferLovelace,
                    ),
                    timeoutMs: context.stackConfig.timeoutMs,
                    now: () => context.deployment.chain.now(),
                    // Never past the opening of the fault's late-shift window.
                    readyByMs: async () =>
                      stackLateShiftWindowOpensMs(
                        context,
                        (
                          await readStackJourneyTiming(
                            {
                              nodeConfigPath:
                                context.layout.node.nodeConfigPath,
                              manifestPath: context.layout.manifestPath,
                            },
                            fixture.category,
                            { actionDepth: JOURNEY_ACTION_DEPTH },
                          )
                        ).transactionAllowanceMs,
                      ),
                    onStage,
                  })(predecessor),
              }),
          onStage,
        });
      } catch (cause) {
        await holdWatch.stop();
        throw cause;
      }
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
        holdWatch,
        poll: polls.poll,
        // The stack journey's own wait for our node to commit a transfer.
        commitTimeoutMs: context.stackConfig.timeoutMs,
        schedulerPolicyId: context.deployment.contracts.scheduler.policyId,
        shiftDurationMs: Number(SDK.SHIFT_DURATION_MS),
        slotToUnixTime: (slot) =>
          context.deployment.publisherLucid.slotToUnixTime(slot),
      }),
    refusal: async ({ fault, refused, polls }) => {
      const removal =
        refused.removal ??
        (await polls.stage("refused fault aged out", () =>
          stackJourneyDa(context, {
            readConfirmedTransaction: transactions.transaction,
            onStage: (name) => console.info(`Live fixture: ${name}`),
          }).awaitAgedOut(fault.headerHash, refused.attestationDeadlineMs),
        ));
      const failure = await holdWatch.stop();
      if (failure !== undefined) throw failure;
      const observations = await holdWatch.observations(fault.headerHash);
      const awaiting = observations.find(
        ({ hold }) => hold?.state === "awaiting_da",
      );
      if (awaiting === undefined)
        throw new Error(
          `Our node never reported ${LANDED_BLOCK_AWAITING_DA} for the refused fault ${fault.headerHash} (${observations.length.toString()} hold observation(s))`,
        );
      return {
        delivery: refused.delivery,
        attestationDeadlineMs: refused.attestationDeadlineMs,
        removal,
        awaitingDaObservedAt: awaiting.observedAt,
        holdBinding: awaiting.hold?.binding,
        holdObservations: observations.length,
      };
    },
    // A run that fails between the fault's commit and its successor or
    // refusal would otherwise leave the watch polling our node.
    close: async () => {
      await holdWatch.stop();
    },
  };
};
