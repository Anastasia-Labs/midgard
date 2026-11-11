import { existsSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import type {
  createPublishedWatcherBlockActor,
  PublishedDaAttestationOutcome,
} from "midgard-watcher/tests/support/published-block-actor";
import { sharedShiftWork } from "midgard-watcher/tests/support/published-block-actor.operator-shift";
import type { PublishedDaTransactionRecord } from "midgard-watcher/tests/support/published-da-target-consumption";
import {
  type PublishedDepositTraceCheckpoint,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type {
  JourneyBlock,
  JourneyCategory,
  JourneyFixtureStage,
  JourneySuccessor,
} from "./fixture.js";
import { journeyDaSignerConfig } from "./journey-network.js";
import {
  readStackJourneyTiming,
  stackStagingShiftCover,
} from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import {
  isStackJourneyContext,
  stackDaExpectation,
  type StackDaOutcome,
} from "./stack-da-publication.js";
import { stackJourneyDa } from "./stack-journey-da.js";
import { stackLateShiftGate } from "./stack-late-shift.js";
import { fundStackLedgerOwner } from "./stack-ledger-owner.js";
import type {
  FaultPreparation,
  JourneyRetainedBlock,
  StagingCheckpoint,
} from "./staging.decode-journey-retained-block.js";

export type JourneyBlockActor = Awaited<
  ReturnType<typeof createPublishedWatcherBlockActor>
>;

/**
 * How a staged journey's blocks reach DA, and where its healthy predecessor
 * comes from. An owned devnet signs as its own committee and extends its
 * retained head; an attached stack publishes as operator-2's node would, to
 * the stack's own committee, and extends the stack's attested tail.
 */
export type JourneyDaPath = Readonly<{
  kind: "devnet" | "stack";
  /** Whose L2 outputs the drill's blocks spend. */
  ledgerOwnerSeedPhrase: string;
  /** The block actor's committee keys, or its share of the scheduler. */
  actorOptions: Pick<
    Parameters<typeof createPublishedWatcherBlockActor>[0],
    "daSignerConfig" | "sharedScheduler"
  >;
  /** The healthy predecessor a fresh staging extends. */
  resolvePredecessor(
    input: Readonly<{
      /** The stack's operator-2 actor; a devnet chooses its signer later. */
      stackActor: JourneyBlockActor | null;
      decodeRetained(block: JourneyBlock): Promise<JourneyRetainedBlock>;
    }>,
  ): Promise<JourneySuccessor>;
  /** Before each attempt to commit the fault: on a stack, its late-shift wait. */
  beforeFaultCommit(actor: JourneyBlockActor): Promise<void>;
  /** A history block's DA, which the committee must attest. */
  attestHistory(
    actor: JourneyBlockActor,
    block: JourneyBlock,
    checkpointPaths: readonly string[],
  ): Promise<PublishedDaAttestationOutcome>;
  /**
   * The fault's DA outcome. Every DA transaction a devnet signs is saved in
   * the checkpoint before submission, so a resumed run reconciles the same
   * set; a stack saves which committee peers took the payload.
   */
  settleFault(
    input: Readonly<{
      actor: JourneyBlockActor;
      block: JourneyBlock;
      category: JourneyCategory;
      checkpoint: StagingCheckpoint;
      checkpointPath: string;
      reconcileSubmitted(
        record: PublishedDaTransactionRecord,
      ): Promise<{ kind: "included" | "retired" }>;
    }>,
  ): Promise<StackDaOutcome>;
}>;

const attestedBy = (actor: JourneyBlockActor, block: JourneyBlock) =>
  actor.attest({
    ...block,
    payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
  });

export const journeyDaPath = async (
  stage: Pick<
    JourneyFixtureStage,
    "context" | "directory" | "retain" | "readConfirmedTransaction" | "onStage"
  >,
  task: Pick<FaultPreparation, "category" | "historyBlocks" | "userEvents">,
): Promise<JourneyDaPath> => {
  const { context, directory, retain, onStage } = stage;
  const fingerprint = context.deployment.manifest.manifestId;
  if (isStackJourneyContext(context)) {
    const da = stackJourneyDa(context, {
      readConfirmedTransaction: stage.readConfirmedTransaction,
      onStage,
    });
    const timing = await readStackJourneyTiming(
      {
        nodeConfigPath: context.layout.node.nodeConfigPath,
        manifestPath: context.layout.manifestPath,
      },
      task.category,
      { actionDepth: JOURNEY_ACTION_DEPTH },
    );
    const history = SDK.requireEventHistoryContracts(
      context.deployment.contracts,
    );
    const eventHistoryProtectionMs = Number(
      [history.deposit, history.withdrawal].reduce(
        (longest, { recipe }) =>
          recipe.protectionDurationMs > longest
            ? recipe.protectionDurationMs
            : longest,
        0n,
      ),
    );
    // Refuses here, before any L1 work, when the lead is required and unset.
    const lateShift = stackLateShiftGate(context, {
      category: task.category,
      commitAllowanceMs: timing.transactionAllowanceMs,
      directory,
      onStage,
    });
    return {
      kind: "stack",
      ledgerOwnerSeedPhrase: context.accounts.ledgerOwner.seedPhrase,
      // Operator-2 shares the scheduler with our node. From the tail it reads
      // to the fault's commit, one shift of operator-2's holds the staging:
      // our node, which cannot commit in it, leaves the tail as read.
      actorOptions: {
        sharedScheduler: sharedShiftWork(
          stackStagingShiftCover({
            category: task.category,
            timing,
            historyBlocks: task.historyBlocks ?? 0,
            userEvents: task.userEvents ?? 0,
            eventHistoryProtectionMs,
            profile: {
              event_wait_ms: SDK.EVENT_WAIT_DURATION_MS,
              operator_shift_ms: Number(SDK.SHIFT_DURATION_MS),
            },
          }),
        ),
      },
      resolvePredecessor: async ({ stackActor, decodeRetained }) => {
        await fundStackLedgerOwner(context, {
          readAttestedTail: async () =>
            decodeRetained(await da.readAttestedTail()),
          onStage,
        });
        await stackActor!.onboardOperator();
        return da.readAttestedTail();
      },
      beforeFaultCommit: (actor) => lateShift(actor.operatorVkey),
      attestHistory: async (_actor, block, checkpointPaths) => {
        const outcome = await da.publish(block, "attestation", checkpointPaths);
        if (outcome.kind === "refused")
          throw new Error(
            `The DA committee refused history ${block.headerHash}`,
          );
        return outcome;
      },
      settleFault: ({ block, category, checkpoint, checkpointPath }) =>
        da.publish(block, stackDaExpectation(category), [checkpointPath], {
          saved: checkpoint.daDelivery,
          save: async (delivery) => {
            checkpoint.daDelivery = delivery;
            await writeJourneyArtifact(checkpointPath, checkpoint);
          },
        }),
    };
  }
  const { deployment, accounts } = context;
  const daSignerConfig = journeyDaSignerConfig(accounts);
  const headPath = join(context.runDirectory, "work/journeys/head.json");
  return {
    kind: "devnet",
    ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
    actorOptions: { daSignerConfig },
    beforeFaultCommit: () => Promise.resolve(),
    resolvePredecessor: async () => {
      if (existsSync(headPath)) {
        const head = await readJourneyArtifact<{
          deploymentFingerprint: string;
          block: JourneySuccessor;
        }>(headPath);
        if (head.deploymentFingerprint !== fingerprint)
          throw new Error("Journey head belongs to a different deployment");
        // Preparation can already have appended journaled history before a
        // stop. Its checkpoint restores that tail; every new commit verifies
        // the live tail immediately before constructing its inputs.
        return head.block;
      }
      const initialPath = join(directory, "honest-deposit.json");
      const initial = await stagePublishedDepositTrace(deployment, {
        daSignerConfig,
        honest: true,
        onStage,
        resume: existsSync(initialPath)
          ? await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
              initialPath,
            )
          : undefined,
        onCheckpoint: (checkpoint) =>
          writeJourneyArtifact(initialPath, checkpoint),
        timeoutCorrectionJournalPath: join(
          directory,
          "timeout-correction.json",
        ),
        // The watcher observes the queue at its action depth, so the
        // abandoned header removal only has to be that deep before the
        // watcher starts.
        finalityDepth: JOURNEY_ACTION_DEPTH,
      });
      await retain(initial.predecessor, initial.commits[0]!);
      await retain(initial.current, initial.commits[1]!);
      return { ...initial.current, commitTxHash: initial.commits[1]! };
    },
    attestHistory: (actor, block) => attestedBy(actor, block),
    settleFault: ({
      actor,
      block,
      checkpoint,
      checkpointPath,
      reconcileSubmitted,
    }) =>
      actor.attest(
        {
          ...block,
          payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
        },
        {
          submitted: checkpoint.daTransactions ?? [],
          reconcileSubmitted,
          onSubmitted: async (record) => {
            checkpoint.daTransactions = [
              ...(checkpoint.daTransactions ?? []),
              record,
            ];
            await writeJourneyArtifact(checkpointPath, checkpoint);
          },
        },
      ),
  };
};
