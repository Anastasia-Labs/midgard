import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { expect } from "vitest";

import { reconcileStateQueueCorrections } from "../src/fibers/attestation-timeout-correction.js";
import { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import type { StateQueueCorrectionObserverSource } from "../src/services/state-queue-correction-observer.js";
import { type Scenario } from "./expired-signed-intent-release-evidence-emulator.signed-intent-release-evidence.js";
import { read, readObserver } from "./helpers/correction-rewind-scenario.js";
import { type Handle } from "./helpers/signed-intent-replacement.js";

/** What the synthetic observer history is bound to: a correction-rewind
 * scenario, or a lifecycle through `observerContext`. */
export type ObserverContext = Pick<
  Scenario,
  "manifestId" | "requiredFinalityDepth"
> & {
  readonly h: Pick<Handle, "fixture" | "globals">;
};

export const observerContext = (h: Handle): ObserverContext => {
  const manifestId = h.fixture.runtimeOverrides!.deploymentIdentity.manifestId;
  if (manifestId === undefined)
    throw new Error("The fixture deployment must be manifest-bound");
  return {
    h,
    manifestId,
    requiredFinalityDepth: BigInt(
      h.deployment.manifest.l1Finality.confirmationDepth,
    ),
  };
};

/** A synthetic authenticated merge of `previous[1]` into the root, whose
 * continued root is output `rootOutputIndex` of `transactionHash`. */
export const mergeCheckpoint = (
  scenario: ObserverContext,
  previous: readonly SDK.StateQueueTransitionNode[],
  transactionHash: string,
  blockNo: number,
  rootOutputIndex = 0,
) => {
  const policyId = scenario.h.fixture.contracts.stateQueue.policyId;
  const [root, merged, ...rest] = previous;
  const lockRef = `${"ee".repeat(32)}#0`;
  const [rootTx, rootIndex] = root!.outRef.split("#") as [string, string];
  const zero = "00".repeat(32);
  const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: scenario.manifestId,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: transactionHash,
    chainPointId: transactionHash,
    slot: blockNo.toString(),
    blockNo: blockNo.toString(),
    transactionIndex: "0",
    finalityDepth: "1",
    mintPolicyIds: [policyId],
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            MergeToConfirmedStateV1: {
              yield_to_ref_input_index: 0n,
              header_node_key: merged!.headerHash!,
              confirmed_state_input_outref: {
                transactionId: rootTx,
                outputIndex: BigInt(rootIndex),
              },
              confirmed_state_output_index: BigInt(rootOutputIndex),
              m_settlement_redeemer_index: null,
              merged_block_withdrawals_root: zero,
              merged_block_forced_transactions_root: zero,
              merged_block_transactions_root: zero,
              merged_block_deposits_root: zero,
              merged_block_transition_trace_root: zero,
              merged_block_event_to_step_root: zero,
              merged_block_validation_traces_root: zero,
              merged_block_withdrawal_count: 0n,
              merged_block_forced_transaction_count: 0n,
              merged_block_l2_transaction_count: 0n,
              merged_block_deposit_count: 0n,
              merged_block_total_event_count: 0n,
              merged_block_transition_step_count: 0n,
              merged_block_validation_trace_count: 0n,
            },
          },
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [root!.outRef, merged!.outRef],
    referenceInputOutRefs: [lockRef],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: lockRef,
      datum: "Idle",
    },
    previousQueue: previous,
    nextQueue: [
      {
        headerHash: null,
        outRef: `${transactionHash}#${rootOutputIndex.toString()}`,
      },
      ...rest,
    ],
  });
  if (checkpoint === null) throw new Error("The synthetic merge is not exact");
  expect(checkpoint.checkpointKind).toBe("merge");
  return checkpoint;
};

/** A synthetic authenticated commit of `headerHash` onto the tail of
 * `previous` (the root when the queue is empty), which it spends. */
export const appendCheckpoint = (
  context: ObserverContext,
  previous: readonly SDK.StateQueueTransitionNode[],
  transactionHash: string,
  headerHash: string,
  blockNo: number,
) => {
  const policyId = context.h.fixture.contracts.stateQueue.policyId;
  const tail = previous.at(-1)!;
  const lockRef = `${"ee".repeat(32)}#0`;
  const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: context.manifestId,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: transactionHash,
    chainPointId: transactionHash,
    slot: blockNo.toString(),
    blockNo: blockNo.toString(),
    transactionIndex: "0",
    finalityDepth: "1",
    mintPolicyIds: [policyId],
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            CommitBlockHeader: {
              yield_to_ref_input_index: 0n,
              new_block_output_index: 1n,
              continued_latest_block_output_index: 0n,
              operator: "99".repeat(28),
              scheduler_ref_input_index: 0n,
              active_operators_input_index: 0n,
              active_operators_redeemer_index: 0n,
              m_confirmed_state_ref_input_index: null,
              m_head_state_queue_node_ref_input_index: null,
            },
          },
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [tail.outRef],
    referenceInputOutRefs: [lockRef],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: lockRef,
      datum: "Idle",
    },
    previousQueue: previous,
    nextQueue: [
      ...previous.slice(0, -1),
      { headerHash: tail.headerHash, outRef: `${transactionHash}#0` },
      { headerHash, outRef: `${transactionHash}#1` },
    ],
  });
  if (checkpoint === null) throw new Error("The synthetic commit is not exact");
  expect(checkpoint.checkpointKind).toBe("append");
  return checkpoint;
};

/** One tick of the production correction observer over a synthetic source. */
export const observerTick = async (
  scenario: ObserverContext,
  source: StateQueueCorrectionObserverSource,
) => {
  const { h } = scenario;
  const exit = await Effect.runPromiseExit(
    reconcileStateQueueCorrections({
      source,
      deploymentIdentityDigest: scenario.manifestId,
      stateQueuePolicyId: h.fixture.contracts.stateQueue.policyId,
      requiredFinalityDepth: scenario.requiredFinalityDepth,
      deploymentManifest:
        h.fixture.runtimeOverrides!.deploymentIdentity.manifest,
    }).pipe(
      Effect.provideService(Globals, h.globals),
      Effect.provide(Database.layer),
    ),
  );
  if (Exit.isSuccess(exit)) return exit.value;
  throw new Error(Cause.pretty(exit.cause));
};

/** The observer's pending terminal transitions, in record order. */
const readPending = async () =>
  (
    (await readObserver()) as unknown as {
      pending: readonly { transactionHash: string; transitionKind: string }[];
    }
  ).pending;

/** Record `checkpoints` with the production correction observer, from a
 * cursor re-seeded at `start`: every terminal among them is pending below the
 * release depth. Returns the observer's cursor queue after them and the
 * terminals' transaction hashes. */
export const recordTransitions = async (
  context: ObserverContext,
  start: readonly SDK.StateQueueTransitionNode[],
  checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[],
) => {
  await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM state_queue_terminal_observer_states`;
    }),
  );
  const unexpected = async () => {
    throw new Error("No transition is observed while seeding");
  };
  expect(
    (
      await observerTick(context, {
        readQueue: async () => start,
        observeTransitions: unexpected,
        canonicalDepth: unexpected,
      })
    ).status,
  ).toBe("bootstrapped");
  return extendTransitions(context, start, checkpoints);
};

/** Record `checkpoints` from the observer's current cursor `from`, pending
 * below the release depth, as `recordTransitions` does. */
export const extendTransitions = async (
  context: ObserverContext,
  from: readonly SDK.StateQueueTransitionNode[],
  checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[],
) => {
  const queue = checkpoints.at(-1)?.nextQueue ?? from;
  const terminals = checkpoints.filter(
    ({ checkpointKind }) => checkpointKind !== "append",
  );
  const before = (await readPending()).length;
  expect(
    (
      await observerTick(context, {
        readQueue: async () => queue,
        observeTransitions: async (previous) => {
          expect(previous).toEqual(from);
          return checkpoints;
        },
        canonicalDepth: async () => 1n,
      })
    ).status,
  ).toBe("reconciled");
  expect(
    (await readPending())
      .slice(before)
      .map(({ transactionHash }) => transactionHash),
  ).toEqual(terminals.map(({ transactionHash }) => transactionHash));
  return {
    queue,
    transactionHashes: terminals.map(({ transactionHash }) => transactionHash),
  };
};

/** Record merges of the queue's first nodes with the production correction
 * observer: the cursor is re-seeded at `start` (D followed by `successors`),
 * then each merge is observed, pending below the release depth. Returns the
 * observer's cursor queue after the merges and their transaction hashes. */
export const observeMerges = async (
  scenario: ObserverContext,
  start: readonly SDK.StateQueueTransitionNode[],
  merges: number,
) => {
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  let queue = start;
  for (let index = 0; index < merges; index += 1) {
    const checkpoint = mergeCheckpoint(
      scenario,
      queue,
      (0xa1 + index).toString(16).repeat(32),
      10 + index,
    );
    checkpoints.push(checkpoint);
    queue = checkpoint.nextQueue;
  }
  const recorded = await recordTransitions(scenario, start, checkpoints);
  expect(
    (await readPending()).map(({ transitionKind }) => transitionKind),
  ).toEqual(checkpoints.map(() => "merge"));
  return recorded;
};
