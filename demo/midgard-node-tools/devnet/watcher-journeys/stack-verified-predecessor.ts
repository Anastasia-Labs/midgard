/**
 * Before each family on an attached stack, the block the family will build
 * on must carry the stack watcher's `verified` record, bound to its payload.
 *
 * Why this check exists:
 * - Each family checks that record for its predecessor only at its healthy
 *   decision, after its whole fault, proof and correction
 *   (`createJourneyVerifiedHeaders`, verified-headers.ts). The watcher keeps
 *   these records only in memory, at most 10,000 of them
 *   (midgard-watcher operations-observability.sink.ts).
 * - On an idle stack every block merges, so the first family's predecessor
 *   is the block the queue's root holds (state-queue-root-anchor.ts), which
 *   may be hours old. A watcher restarted after that merge records it
 *   `unverified_merged` (fault-decision-bridge.merged-headers.ts), and the
 *   family fails late with "was merged on L1 before the watcher verified
 *   it". Or the watcher records nothing, and the healthy wait runs out.
 * - The ledger owner's funding does nothing once the owner is funded, and an
 *   idle stack commits no block by itself. So nothing makes a fresh block,
 *   whether the watcher restarted before the run or between families.
 *
 * What it does:
 * - It reads the stack's attested tail, as staging reads it. It then pages
 *   the watcher's verification records from the first one, with the same
 *   rules the healthy decision applies (`keepJourneyDecision`).
 * - A tail verified from its own payload passes. A live tail the watcher has
 *   not decided is waited for. A tail decided faulty, a gap or a failure is
 *   refused before any L1 work.
 * - A tail the watcher released unverified, or that left the queue by a
 *   merge before the watcher verified it, is healed. A watcher never
 *   verifies a block that has left the queue. The heal is one honest L2
 *   transfer from the drill's ledger owner back to itself through our node.
 *   It is never a deposit, so no user event is published.
 * - The transfer is saved, under the tail it was built on, before its first
 *   send, and sent again only while our node does not know it. A restarted
 *   drill resumes it rather than making a second one.
 * - The check then waits for our node to commit the transfer's block, for
 *   the block to reach L1 and be attested, and for the watcher to verify it.
 *   Any other end is refused by name. One check never sends a second
 *   transfer.
 *
 * The transfer is signed by the ledger owner alone. Nothing here signs as
 * operator-2 or holds or resumes its node. A closing session stops each wait.
 */
import { createHash } from "node:crypto";
import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import type { WatcherVerificationDiagnostic } from "midgard-watcher";

import { nodeLedger } from "../../src/full-stack/journey.js";
import { pauseUnlessAborted } from "./abortable-pause.js";
import type { JourneyBlock } from "./fixture.js";
import { readIndexedStateQueueHeader } from "./honest-successor.observables.js";
import { successorTransfer } from "./honest-successor.transfer.js";
import { journeyNetwork } from "./journey-network.js";
import type { AttachedJourneySession } from "./journey-session.js";
import { readStackJourneyTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { stackJourneyDa } from "./stack-journey-da.js";
import { STACK_ABSORB_WITHIN_MS } from "./stack-ledger-owner.js";
import { stackJourneyTransactions } from "./stack-transactions.js";
import { leftQueueByMerge } from "./state-queue-root-anchor.js";
import { answerOrNotYet } from "./transport-failure.js";
import {
  keepJourneyDecision,
  readJourneyVerificationRecords,
  RELEASED_UNVERIFIED,
} from "./verified-headers.js";

/** The poll cadence of a family's own waits on the watcher. */
const POLL_MS = 1000;

/** What the check reads and sends through. */
export type StackVerifiedPredecessorPorts = Readonly<{
  /** Where each tail's transfer is saved, in a directory named by the tail. */
  directory: string;
  /** The stack's attested queue tail, as staging reads its predecessor. */
  readAttestedTail(): Promise<JourneyBlock>;
  /** The newest attested block at or after the tail as first read. */
  readAttestedAtOrAfterTail(): Promise<JourneyBlock>;
  /** Whether a block is a live queue node, or left it by merge or removal. */
  standing(headerHash: string): Promise<"queued" | "merged" | "removed">;
  /** The block's header once L1 has indexed a state-queue node for it. */
  headerOnL1(headerHash: string): Promise<SDK.Header | undefined>;
  /** The stack watcher's operations endpoint. */
  operations(path: string): Promise<unknown>;
  /** The drill's transfer, saved in `directory`. */
  transfer(directory: string): ReturnType<typeof successorTransfer>;
  now(): number;
  pause(): Promise<void>;
  /** How long our node may take to commit the transfer and land its block. */
  absorbWithinMs: number;
  /** How long the watcher may take to decide a block. */
  decisionWithinMs: number;
  onStage(name: string): void;
}>;

export type StackVerifiedPredecessorRefusalReason =
  | "tail-not-healthy"
  | "payload-mismatch"
  | "left-queue-unmerged"
  | "watcher-undecided"
  | "transfer-not-committed"
  | "transfer-block-not-on-l1"
  | "healed-block-not-in-queue"
  | "healed-block-unverified"
  | "healed-block-not-healthy";

/** The check refused: the family must not build on the stack's tail. */
export class StackVerifiedPredecessorRefusal extends Error {
  override readonly name = "StackVerifiedPredecessorRefusal";
  readonly reason: StackVerifiedPredecessorRefusalReason;
  readonly headerHash: string;

  constructor(
    reason: StackVerifiedPredecessorRefusalReason,
    headerHash: string,
    detail: string,
  ) {
    super(
      `Verified predecessor refused (${reason}) at ${headerHash}: ${detail}`,
    );
    this.reason = reason;
    this.headerHash = headerHash;
  }
}

/** How the watcher's records leave a block. */
type Decision =
  | { kind: "verified" }
  | { kind: "payload-mismatch" }
  | { kind: "released"; outcome: string }
  | { kind: "undecided" }
  | { kind: "not-healthy"; outcome: string };

const envelopeSha256 = (block: JourneyBlock) =>
  createHash("sha256").update(block.payloadEnvelopeCbor).digest("hex");

/**
 * The watcher's records for `block`, read from its first one: its kept
 * decision, or else its latest record.
 */
const classify = async (
  ports: StackVerifiedPredecessorPorts,
  block: JourneyBlock,
): Promise<Decision> => {
  const decided = new Map<string, WatcherVerificationDiagnostic>();
  let latest: WatcherVerificationDiagnostic | undefined;
  await readJourneyVerificationRecords(ports.operations, "0", (record) => {
    if (record.headerHash !== block.headerHash) return;
    keepJourneyDecision(decided, record);
    latest = record;
  });
  const record = decided.get(block.headerHash) ?? latest;
  if (
    record === undefined ||
    record.outcome === "pending_da" ||
    record.outcome === "pending_l1"
  )
    return { kind: "undecided" };
  if (record.outcome === "verified")
    return record.payloadEnvelopeSha256 === envelopeSha256(block)
      ? { kind: "verified" }
      : { kind: "payload-mismatch" };
  return RELEASED_UNVERIFIED.has(record.outcome) ||
    record.outcome === "unverified_past_horizon"
    ? { kind: "released", outcome: record.outcome }
    : { kind: "not-healthy", outcome: record.outcome };
};

/**
 * The watcher's decision on `block`. While it has none, a live block is
 * waited for within the decision allowance, and one that merged in the
 * meantime counts as released unverified.
 */
const awaitDecision = async (
  ports: StackVerifiedPredecessorPorts,
  block: JourneyBlock,
): Promise<Exclude<Decision, { kind: "undecided" }>> => {
  let deadline: number | undefined;
  for (;;) {
    const found = await answerOrNotYet(() => classify(ports, block));
    if (found !== undefined && found.kind !== "undecided") return found;
    const standing = await answerOrNotYet(() =>
      ports.standing(block.headerHash),
    );
    // A record written just before the merge still counts; a read of it
    // that got no answer is tried again within the bound.
    const last =
      standing === "merged"
        ? await answerOrNotYet(() => classify(ports, block))
        : undefined;
    if (last !== undefined)
      return last.kind === "undecided"
        ? { kind: "released", outcome: "merged with no verified record" }
        : last;
    if (standing === "removed")
      throw new StackVerifiedPredecessorRefusal(
        "left-queue-unmerged",
        block.headerHash,
        "the block left the queue without a merge before the watcher decided it",
      );
    deadline ??= ports.now() + ports.decisionWithinMs;
    if (ports.now() > deadline)
      throw new StackVerifiedPredecessorRefusal(
        "watcher-undecided",
        block.headerHash,
        `the watcher did not decide the block within ${ports.decisionWithinMs.toString()} ms`,
      );
    await ports.pause();
  }
};

/**
 * Makes sure the stack's attested tail is a block the watcher verified from
 * its payload, healing it with one honest transfer when the watcher can no
 * longer verify it.
 */
export const ensureStackVerifiedPredecessor = async (
  ports: StackVerifiedPredecessorPorts,
): Promise<
  | Readonly<{ kind: "verified"; block: JourneyBlock }>
  | Readonly<{
      kind: "healed";
      from: JourneyBlock;
      block: JourneyBlock;
      transferTxId: string;
    }>
> => {
  const tail = await ports.readAttestedTail();
  const decided = await awaitDecision(ports, tail);
  if (decided.kind === "verified") return { kind: "verified", block: tail };
  if (decided.kind === "payload-mismatch")
    throw new StackVerifiedPredecessorRefusal(
      "payload-mismatch",
      tail.headerHash,
      "the watcher verified the tail from another payload envelope",
    );
  if (decided.kind === "not-healthy")
    throw new StackVerifiedPredecessorRefusal(
      "tail-not-healthy",
      tail.headerHash,
      `the watcher decided the tail ${decided.outcome}`,
    );
  ports.onStage(
    `the watcher can no longer verify tail ${tail.headerHash} (${decided.outcome}); healing it with one transfer`,
  );
  const directory = join(ports.directory, tail.headerHash);
  await mkdir(directory, { recursive: true, mode: 0o700 });
  const transfer = ports.transfer(directory);
  const intent = (await transfer.saved(tail)) ?? (await transfer.build(tail));
  const deadline = ports.now() + ports.absorbWithinMs;
  // A send that got no answer is "not yet": the next /tx-status read says
  // whether to send again, as for a dropped transfer.
  await answerOrNotYet(() => transfer.send(intent));
  let committed: string | undefined;
  for (;;) {
    const status = await answerOrNotYet(() => transfer.txStatus(intent.txId));
    if (status?.status === "committed" && status.headerHash !== undefined) {
      committed = status.headerHash;
      break;
    }
    if (status?.status === "not_found")
      await answerOrNotYet(() => transfer.send(intent));
    if (ports.now() > deadline)
      throw new StackVerifiedPredecessorRefusal(
        "transfer-not-committed",
        tail.headerHash,
        `our node did not commit transfer ${intent.txId} within ${ports.absorbWithinMs.toString()} ms`,
      );
    await ports.pause();
  }
  ports.onStage(`our node committed the transfer in ${committed}`);
  let carrying: SDK.Header | undefined;
  const onL1 = () => answerOrNotYet(() => ports.headerOnL1(committed!));
  while ((carrying = await onL1()) === undefined) {
    if (ports.now() > deadline)
      throw new StackVerifiedPredecessorRefusal(
        "transfer-block-not-on-l1",
        committed,
        `our node's block did not reach L1 within ${ports.absorbWithinMs.toString()} ms`,
      );
    await ports.pause();
  }
  // The block the next family builds on: ours, or a newer attested one,
  // which ends after ours. A queue read that lags L1 still returns the old
  // tail, or a block before ours, for a while; it is read again within the
  // same bound, and the transfer is never sent again.
  const { endTime } = carrying;
  const isNew = (read: JourneyBlock) =>
    read.headerHash !== tail.headerHash &&
    (read.headerHash === committed || read.header.endTime > endTime);
  let block = await answerOrNotYet(() => ports.readAttestedAtOrAfterTail());
  while (block === undefined || !isNew(block)) {
    if (ports.now() > deadline)
      throw new StackVerifiedPredecessorRefusal(
        "healed-block-not-in-queue",
        committed,
        `the stack's queue still read ${block?.headerHash ?? "nothing it could answer"}, not our node's block or a later one, after ${ports.absorbWithinMs.toString()} ms`,
      );
    await ports.pause();
    block = await answerOrNotYet(() => ports.readAttestedAtOrAfterTail());
  }
  const healed = await awaitDecision(ports, block);
  if (healed.kind === "verified")
    return { kind: "healed", from: tail, block, transferTxId: intent.txId };
  throw healed.kind === "payload-mismatch"
    ? new StackVerifiedPredecessorRefusal(
        "payload-mismatch",
        block.headerHash,
        "the watcher verified the healed block from another payload envelope",
      )
    : new StackVerifiedPredecessorRefusal(
        healed.kind === "released"
          ? "healed-block-unverified"
          : "healed-block-not-healthy",
        block.headerHash,
        `the watcher left the healed block ${healed.outcome}; no second transfer is sent`,
      );
};

/** The check on an attached stack, through its own services and our node. */
export const checkStackVerifiedPredecessor = async (
  session: AttachedJourneySession,
  input: Readonly<{ category: string; onStage(name: string): void }>,
) => {
  const { context } = session;
  const { contracts, chain } = context.deployment;
  const directory = join(
    context.runDirectory,
    "work/journeys/verified-predecessor",
  );
  await mkdir(directory, { recursive: true, mode: 0o700 });
  const transactions = stackJourneyTransactions({
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
    directory,
  });
  const da = stackJourneyDa(context, {
    readConfirmedTransaction: transactions.transaction,
    onStage: input.onStage,
  });
  const timing = await readStackJourneyTiming(
    {
      nodeConfigPath: context.layout.node.nodeConfigPath,
      manifestPath: context.layout.manifestPath,
    },
    input.category,
    {
      actionDepth: JOURNEY_ACTION_DEPTH,
      authenticatedConfirmationDepth:
        session.releaseFinality.policy.confirmationDepth,
    },
  );
  return ensureStackVerifiedPredecessor({
    directory,
    readAttestedTail: () => da.readAttestedTail(),
    readAttestedAtOrAfterTail: () => da.readAttestedAtOrAfterTail(),
    standing: async (headerHash) => {
      const live = await context.provider.getUtxosWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        toUnit(
          contracts.stateQueue.policyId,
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
        ),
      );
      if (live.length > 0) return "queued";
      return (await leftQueueByMerge({
        kupoUrl: context.kupoUrl,
        stateQueue: contracts.stateQueue,
        readConfirmedTransaction: transactions.transaction,
        headerHash,
      }))
        ? "merged"
        : "removed";
    },
    headerOnL1: async (headerHash) =>
      (
        await readIndexedStateQueueHeader({
          kupoUrl: context.kupoUrl,
          policyId: contracts.stateQueue.policyId,
          headerHash,
        })
      )[0]?.header,
    operations: (path) => session.watcher.operations(path),
    transfer: (transferDirectory) =>
      successorTransfer({
        directory: transferDirectory,
        nodeEndpoint: context.stackConfig.endpoint,
        ledger: nodeLedger(context.stackConfig.endpoint),
        // The owner's wallet, from its seed's environment name.
        ledgerOwner: context.accounts.ledgerOwner,
        network: journeyNetwork().network,
        transferLovelace: BigInt(context.stackConfig.journey.transferLovelace),
      }),
    now: () => chain.now(),
    pause: () => pauseUnlessAborted(POLL_MS, session.signal),
    absorbWithinMs: STACK_ABSORB_WITHIN_MS,
    decisionWithinMs: timing.allowances.automaticDecisionMs,
    onStage: input.onStage,
  });
};
