import { existsSync } from "node:fs";
import { join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type Network } from "@lucid-evolution/lucid";

import { type JourneyLedger } from "../../src/full-stack/journey.js";
import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type {
  JourneyBlock,
  JourneySuccessor,
  JourneySuccessorCheckpoint,
  StagedJourney,
} from "./fixture.js";
import {
  faultRemovedBeforeAttestation,
  readFaultAttestation,
} from "./honest-successor.fault-attestation.js";
import {
  assertNoHonestHeaderOnFault,
  COMMIT_BASE_PENDING,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_INVALID,
  readIndexedStateQueueHeader,
  readOperatorShiftsOnFault,
} from "./honest-successor.observables.js";
import type {
  JourneyFaultAttestation,
  JourneySuccessorStage,
} from "./honest-successor.stage.js";
import { successorTransfer } from "./honest-successor.transfer.js";
import { createNodeHoldWatch, type NodeHoldWatch } from "./node-hold-watch.js";

export type {
  JourneyFaultAttestation,
  JourneySuccessorStage,
} from "./honest-successor.stage.js";

/**
 * How a family's honest successor comes about once the fault is on L1.
 * - `afterFaultLanded`: the fault is the queue's tail; give our node work.
 * - `commit`: the watcher removed the fault in `removalTxHash`; return the
 *   successor block, whether the successor stage was reached, and, where L1
 *   was read for it, when the fault's DA attestation was applied.
 * - `close`: stop anything `afterFaultLanded` left running.
 */
export type JourneySuccessorPort = Readonly<{
  afterFaultLanded(): Promise<void>;
  commit(correction: Readonly<{ removalTxHash: string }>): Promise<
    Readonly<{
      successor: JourneySuccessor;
      stage: JourneySuccessorStage;
      faultAttestation?: JourneyFaultAttestation;
    }>
  >;
  close(): Promise<void>;
}>;

const SUCCESSOR = "successor.json";

/**
 * The devnet successor: the journey commits it itself as the honest
 * operator, through the fixture's own builder, after the correction.
 */
export const devnetSuccessorPort = (
  input: Readonly<{
    directory: string;
    staged: Pick<StagedJourney, "commitHonestSuccessor">;
    /** Retains the successor's payload before its commit is signed. */
    retainPayload(block: JourneyBlock): Promise<void>;
    /** Retains the committed successor in the journey's retained DA. */
    retain(block: JourneyBlock, commitTxHash: string): Promise<void>;
    stage<T>(name: string, action: () => Promise<T>): Promise<T>;
  }>,
): JourneySuccessorPort => ({
  afterFaultLanded: async () => {},
  commit: async () => {
    const successorPath = join(input.directory, SUCCESSOR);
    const progressPath = join(input.directory, "successor-progress.json");
    const successor = existsSync(successorPath)
      ? await readJourneyArtifact<JourneySuccessor>(successorPath)
      : await input.stage("honest successor commitment", async () =>
          input.staged.commitHonestSuccessor({
            beforeCommit: input.retainPayload,
            resume: existsSync(progressPath)
              ? await readJourneyArtifact<JourneySuccessorCheckpoint>(
                  progressPath,
                )
              : undefined,
            onCheckpoint: (checkpoint) =>
              writeJourneyArtifact(progressPath, checkpoint),
          }),
        );
    await writeJourneyArtifact(successorPath, successor);
    await input.retain(successor, successor.commitTxHash);
    return { successor, stage: { successorStage: "exercised" } };
  },
  close: async () => {},
});

/**
 * The successor stage was not reached only when L1 shows all of it: the
 * fault spent by the watcher's own removal, our operator's first shift after
 * the fault's commit starting strictly after that removal's slot, and the
 * fault operator's shift the commit landed in. A removal in the slot our
 * shift starts in is not before it. Otherwise names what L1 does not show.
 */
const faultRemovedBeforeOurShift = (
  faultTail: Awaited<ReturnType<typeof readOperatorShiftsOnFault>>,
  removalTxHash: string,
): JourneySuccessorStage | string => {
  const { removal, ourFirstShiftStartMs, faultOperatorShift } = faultTail;
  if (removal === null)
    return `Kupo shows no output of the fault spent by the watcher's removal ${removalTxHash}, so its removal is not proven before our shift`;
  if (ourFirstShiftStartMs === null)
    return "L1 shows no shift of ours after the fault's commit to place the removal before";
  if (removal.atMs >= ourFirstShiftStartMs)
    return `the removal at ${removal.atMs.toString()} ms is not before our first shift's start at ${ourFirstShiftStartMs.toString()} ms`;
  if (faultOperatorShift === null)
    return "L1 shows no shift of operator-2's that the fault's commit landed in";
  return {
    successorStage: "not-reached-fault-removed-before-our-shift",
    successorStageEvidence: {
      removal: {
        txHash: removal.txHash,
        slot: removal.slot,
        atMs: removal.atMs,
      },
      ourFirstShiftStartMs,
      operator2ShiftEndMs: faultOperatorShift.endTime,
    },
  };
};

/**
 * The stack successor: our own node builds it. After the fault lands the
 * journey submits one L2 transfer to our node, which gives it work while the
 * fault is the tail, and watches our node's `/readyz` for its hold on the
 * fault. After the watcher's removal it proves the three refusal
 * observables:
 * 1. our node held `landed_block_invalid` on the fault while it was the tail,
 *    or held it awaiting its DA only and L1 shows the watcher's removal
 *    strictly before the fault's DA apply (or no DA apply), which the
 *    family records as its own stage;
 * 2. no header of our operator on L1 names the fault as its parent;
 * 3. the hold cleared, and our node's next header links to the fault's
 *    predecessor and carries the transfer.
 * The refusal is tested only if our node could have built on the fault: it
 * had a shift while the fault was the tail, or reported `commit_base_pending`
 * then. Neither fails the family, unless L1 shows the watcher's removal of
 * the fault strictly before our first shift's start: then the stage was not
 * reached, and the family records why.
 */
export const stackSuccessorPort = (
  input: Readonly<{
    directory: string;
    /** Our node's HTTP endpoint. */
    nodeEndpoint: string;
    ledger: JourneyLedger;
    kupoUrl: string;
    stateQueuePolicyId: string;
    honestOperatorKeyHash: string;
    /** The drill's L2 owner, which sends the transfer to itself. */
    ledgerOwner: Readonly<{ seedPhrase: string; address: string }>;
    network: Network;
    transferLovelace: bigint;
    /** The queue's tail once the fault is removed. */
    predecessor: JourneyBlock;
    fault: JourneyBlock;
    /** The public DA copy of a header's payload envelope, verified against it. */
    fetchPayloadEnvelope(headerHash: string): Promise<Uint8Array>;
    poll<T>(
      name: string,
      action: () => Promise<T | undefined>,
      timeoutMs?: number,
    ): Promise<T>;
    /** How long our node may take to commit the transfer after the removal. */
    commitTimeoutMs: number;
    /**
     * Our node's hold watch, started by staging at the fault's commit; one
     * started when the fault landed otherwise.
     */
    holdWatch?: NodeHoldWatch;
    holdPollIntervalMs?: number;
    schedulerPolicyId: string;
    shiftDurationMs: number;
    slotToUnixTime(slot: number): number;
    fetchImpl?: typeof fetch;
  }>,
): JourneySuccessorPort => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const faultHeaderHash = input.fault.headerHash;
  const refusalPath = join(input.directory, "refusal.json");
  const holdWatch =
    input.holdWatch ??
    createNodeHoldWatch({
      directory: input.directory,
      nodeEndpoint: input.nodeEndpoint,
      pollIntervalMs: input.holdPollIntervalMs,
      fetchImpl,
    });
  const transfer = successorTransfer(input);
  const { txStatus } = transfer;
  const savedIntent = () => transfer.saved(input.predecessor, faultHeaderHash);
  return {
    afterFaultLanded: async () => {
      if (existsSync(join(input.directory, SUCCESSOR))) return;
      // Handed over before the fault's commit, or built now.
      await transfer.send(
        (await savedIntent()) ?? (await transfer.build(input.predecessor)),
      );
      holdWatch.start(faultHeaderHash);
    },
    commit: async ({ removalTxHash }) => {
      const failure = await holdWatch.stop();
      if (failure !== undefined) throw failure;
      const successorPath = join(input.directory, SUCCESSOR);
      // A run that stopped after its successor reads both back; a refusal
      // record without a stage predates the stage and passed the shift check.
      if (existsSync(successorPath)) {
        const refusal = await readJourneyArtifact<{
          stage?: JourneySuccessorStage;
          faultAttestation?: JourneyFaultAttestation;
        }>(refusalPath);
        return {
          successor: await readJourneyArtifact<JourneySuccessor>(successorPath),
          stage: refusal.stage ?? { successorStage: "exercised" },
          ...(refusal.faultAttestation === undefined
            ? {}
            : { faultAttestation: refusal.faultAttestation }),
        };
      }
      const intent = await savedIntent();
      if (intent === undefined)
        throw new Error("The drill's successor transfer was never submitted");
      // Observable 1: the hold was on the fault while it was the tail.
      const observations = await holdWatch.observations(faultHeaderHash);
      const firstHeld = observations.find(({ hold }) => hold?.state === "held");
      const firstAwaiting = observations.find(
        ({ hold }) => hold?.state === "awaiting_da",
      );
      const unobserved = `Our node never reported ${LANDED_BLOCK_INVALID} for the fault ${input.fault.headerHash} while it was the tail; the refusal was not observed`;
      const firstHold = firstHeld ?? firstAwaiting;
      if (firstHold === undefined) throw new Error(unobserved);
      // When the fault's DA apply landed, read once: it decides observable 1
      // without a held report, and the race is measured from it.
      const attestation = await readFaultAttestation(
        {
          kupoUrl: input.kupoUrl,
          policyId: input.stateQueuePolicyId,
          faultHeaderHash,
          slotToUnixTime: input.slotToUnixTime,
        },
        fetchImpl,
      );
      const faultAttestation: JourneyFaultAttestation = {
        blockTimeMs: attestation.blockTimeMs,
        apply: attestation.apply,
        lagAfterBlockTimeMs: attestation.lagAfterBlockTimeMs,
      };
      let stage: JourneySuccessorStage = { successorStage: "exercised" };
      if (firstHeld === undefined) {
        const removedBeforeAttestation = faultRemovedBeforeAttestation(
          attestation,
          removalTxHash,
          input.slotToUnixTime,
        );
        if (typeof removedBeforeAttestation === "string")
          throw new Error(
            `${unobserved}: it reported only ${LANDED_BLOCK_AWAITING_DA}, and ${removedBeforeAttestation}`,
          );
        stage = removedBeforeAttestation;
      }
      const committed = await input.poll(
        "our node's successor commitment",
        async () => {
          const status = await txStatus(intent.txId);
          return status.status === "committed" &&
            status.headerHash !== undefined
            ? status.headerHash
            : undefined;
        },
        input.commitTimeoutMs,
      );
      // Observable 3: our node's block links to the predecessor and carries
      // the transfer, read from the payload the committee published.
      const payloadEnvelopeCbor = Buffer.from(
        await input.fetchPayloadEnvelope(committed),
      );
      const payload = SDK.decodeDaPayload(
        Buffer.from(
          (
            await unwrapDaPayload(payloadEnvelopeCbor, {
              maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
            })
          ).innerBytes,
        ),
      );
      const { header, header_hash: headerHash } = payload.block_body;
      if (headerHash !== committed)
        throw new Error(
          `The published payload for ${committed} names header ${headerHash}`,
        );
      if (header.prevHeaderHash !== input.predecessor.headerHash)
        throw new Error(
          `Our node's header ${committed} links to ${header.prevHeaderHash}, not to the fault's predecessor ${input.predecessor.headerHash}`,
        );
      if (header.operatorVkey !== input.honestOperatorKeyHash)
        throw new Error(
          `Header ${committed} carrying the drill's transfer is not our operator's`,
        );
      if (!payload.block_body.transactions.some(([id]) => id === intent.txId))
        throw new Error(
          `Our node's header ${committed} does not carry the drill's transfer ${intent.txId}`,
        );
      const [onL1] = await input.poll(
        "our node's successor on L1",
        async () => {
          const indexed = await readIndexedStateQueueHeader(
            {
              kupoUrl: input.kupoUrl,
              policyId: input.stateQueuePolicyId,
              headerHash: committed,
            },
            fetchImpl,
          );
          return indexed.length > 0 ? indexed : undefined;
        },
        input.commitTimeoutMs,
      );
      if (Data.to(onL1!.header, SDK.Header) !== Data.to(header, SDK.Header))
        throw new Error(
          `Header ${committed} on L1 differs from its published payload's`,
        );
      // Observable 2, over the whole window from the fault's commit.
      const linkage = await assertNoHonestHeaderOnFault(
        {
          kupoUrl: input.kupoUrl,
          policyId: input.stateQueuePolicyId,
          faultHeaderHash: input.fault.headerHash,
          honestOperatorKeyHash: input.honestOperatorKeyHash,
        },
        fetchImpl,
      );
      // Observable 3: the hold cleared once our node built past the fault.
      const after = await holdWatch.read(faultHeaderHash);
      if (after.hold === undefined || after.hold.state !== "not_held")
        throw new Error(
          `Our node still holds the fault after its successor: ${JSON.stringify(after)}`,
        );
      // Our node could have built on the fault: its shift, or its pending
      // commit, overlapped the window in which it held the fault as the tail.
      const faultTail = {
        ...(await readOperatorShiftsOnFault(
          {
            kupoUrl: input.kupoUrl,
            stateQueuePolicyId: input.stateQueuePolicyId,
            schedulerPolicyId: input.schedulerPolicyId,
            faultHeaderHash,
            removalTxHash,
            operatorKeyHash: input.honestOperatorKeyHash,
            shiftDurationMs: input.shiftDurationMs,
            slotToUnixTime: input.slotToUnixTime,
            untilSlot: onL1!.createdAtSlot,
            heldFromMs: Date.parse(firstHold.observedAt),
          },
          fetchImpl,
        )),
        commitBasePendingAt: observations.find(({ hold }) =>
          hold?.reasons.includes(COMMIT_BASE_PENDING),
        )?.observedAt,
      };
      // Kept even when the check below fails: the first live runs measure
      // the late-shift lead from it.
      await writeJourneyArtifact(join(input.directory, "fault-tail.json"), {
        fault: faultHeaderHash,
        ...faultTail,
        faultAttestation,
      });
      if (
        faultTail.ourShifts.length === 0 &&
        faultTail.commitBasePendingAt === undefined
      ) {
        const removedBeforeOurShift = faultRemovedBeforeOurShift(
          faultTail,
          removalTxHash,
        );
        if (typeof removedBeforeOurShift === "string")
          throw new Error(
            `Our node had no shift while the fault ${faultHeaderHash} was the tail (${faultTail.fromMs.toString()}..${faultTail.toMs.toString()} ms) and never reported ${COMMIT_BASE_PENDING}: its refusal to build on the fault went untested, and ${removedBeforeOurShift}`,
          );
        stage = removedBeforeOurShift;
      }
      const successor: JourneySuccessor = {
        header,
        headerHash: committed,
        payloadEnvelopeCbor,
        commitTxHash: onL1!.txHash,
      };
      await writeJourneyArtifact(refusalPath, {
        fault: input.fault.headerHash,
        holdObservations: observations.length,
        holdBinding: observations.find(({ hold }) => hold?.state === "held")
          ?.hold?.binding,
        linkage,
        faultTail,
        transfer: intent.txId,
        successor: committed,
        commitTxHash: onL1!.txHash,
        stage,
        faultAttestation,
      });
      await writeJourneyArtifact(successorPath, successor);
      return { successor, stage, faultAttestation };
    },
    close: async () => {
      await holdWatch.stop();
    },
  };
};
