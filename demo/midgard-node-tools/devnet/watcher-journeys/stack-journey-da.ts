import { existsSync, mkdirSync, renameSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { CONSUMPTION_INDEX_GRACE_MS } from "midgard-watcher/tests/support/published-block-actor.is-unanswered-submission";

import type {
  JourneyBlock,
  JourneyFixtureStage,
  JourneySuccessor,
} from "./fixture.js";
import { readKupoUnitConsumptions } from "./kupo-consumption.js";
import {
  fetchJourneyPublicDaPayload,
  readStackJourneyWatcherConfig,
  STACK_PUBLIC_DA_TRAIL_MS,
} from "./public-da-preflight.js";
import type { StackJourneyContext } from "./stack-context.js";
import {
  awaitStackDaAgedOut,
  classifyStackDaRemoval,
  publishStackDaPayload,
  type StackDaDelivery,
  type StackDaExpectation,
  StackDaPayloadAgedOutError,
  StackDaRefusalNotObserved,
  type StackDaTarget,
} from "./stack-da-publication.js";

const decodeStateQueueNode = (output: UTxO, stateQueuePolicyId: string) =>
  Effect.runPromise(
    Effect.flatMap(
      SDK.utxoToStateQueueUTxO(output, stateQueuePolicyId),
      (utxo) => SDK.getStateQueueNodeFromStateQueueDatum(utxo.datum),
    ),
  );

/** Keeps an aged-out block's checkpoints aside; journey state is never discarded. */
export const archiveAgedOutCheckpoints = (
  paths: readonly string[],
  now: Date,
  onStage: (name: string) => void,
) => {
  const stamp = now.toISOString().replace(/[:.]/g, "-");
  for (const path of paths.filter((value) => existsSync(value))) {
    const archived = join(
      dirname(path),
      "archive",
      `${basename(path, ".json")}-aged-out-${stamp}.json`,
    );
    mkdirSync(dirname(archived), { recursive: true });
    renameSync(path, archived);
    onStage(`archived aged-out checkpoint ${path} to ${archived}`);
  }
};

/** Polls L1 and DA no faster than this while the committee works. */
const STACK_DA_POLL_MS = 10_000;

/** A block of the stack's state queue as one poll reads it. */
export type StackQueueNode = Readonly<{
  headerHash: string;
  /** The block's time on L1 (POSIX ms), its attestation timeout's start. */
  blockTime: bigint;
  attested: boolean;
}>;

/** A queue tail neither attested nor removed within its attestation bound. */
export class StackQueueTailUnattestedError extends Error {
  constructor(readonly headerHash: string) {
    super(
      `Stack queue tail ${headerHash} was neither attested nor removed after its DA attestation timeout`,
    );
    this.name = "StackQueueTailUnattestedError";
  }
}

/** The stack kept appending unattested tails past the first one's bound. */
export class StackQueueTailsOutpaceAttestationError extends Error {
  constructor(
    readonly firstHeaderHash: string,
    readonly latestHeaderHash: string,
  ) {
    super(
      `The stack kept appending unattested queue tails for longer than the DA attestation window: none was attested from ${firstHeaderHash}, the first this read saw, to ${latestHeaderHash}, the latest`,
    );
    this.name = "StackQueueTailsOutpaceAttestationError";
  }
}

/** A queue block awaited for its attestation left the queue instead. */
export class StackQueueNodeRemovedError extends Error {
  constructor(readonly headerHash: string) {
    super(
      `Stack queue block ${headerHash} left the state queue before its DA attestation`,
    );
    this.name = "StackQueueNodeRemovedError";
  }
}

/** What a wait for an attested queue block reads and does. */
export type StackQueuePorts<Node extends StackQueueNode> = Readonly<{
  /** The queue's last block. */
  readTail(): Promise<Node>;
  /** The queue's blocks from `headerHash` to its tail, absent once removed. */
  readFrom(headerHash: string): Promise<readonly Node[] | undefined>;
  now(): number;
  pause(): Promise<void>;
  attestationTimeoutMs: bigint;
  onStage(name: string): void;
}>;

/** Whether `node`, unattested, is past the bound its attestation holds to. */
const pastAttestationBound = (
  ports: Pick<StackQueuePorts<StackQueueNode>, "now" | "attestationTimeoutMs">,
  node: StackQueueNode,
) => ports.now() > Number(node.blockTime + 2n * ports.attestationTimeoutMs);

/**
 * The stack's current queue tail once it is attested. An unattested tail is
 * attested or removed within twice its DA attestation timeout; one that is
 * neither fails the wait. The contract lets blocks be appended to an
 * unattested tail, so a newer tail can replace it at every poll: the wait
 * keeps the bound of the first unattested tail it saw, whatever arrives.
 */
export const awaitAttestedStackQueueTail = async <Node extends StackQueueNode>(
  ports: StackQueuePorts<Node>,
): Promise<Node> => {
  let first: Node | undefined;
  for (;;) {
    const tail = await ports.readTail();
    if (tail.attested) return tail;
    first ??= tail;
    if (pastAttestationBound(ports, tail))
      throw new StackQueueTailUnattestedError(tail.headerHash);
    if (pastAttestationBound(ports, first))
      throw new StackQueueTailsOutpaceAttestationError(
        first.headerHash,
        tail.headerHash,
      );
    ports.onStage(
      `stack queue tail ${tail.headerHash} awaits its DA attestation`,
    );
    await ports.pause();
  }
};

/**
 * The newest attested block at or after the queue's tail as first read: the
 * state the stack has committed to by then, however many newer unattested
 * tails arrive meanwhile. The first tail is attested or removed within its
 * own bound; one that is removed, or neither by then with nothing after it
 * attested, fails the wait.
 */
export const awaitAttestedAtOrAfterStackQueueTail = async <
  Node extends StackQueueNode,
>(
  ports: StackQueuePorts<Node>,
): Promise<Node> => {
  const pinned = await ports.readTail();
  for (let from: readonly Node[] | undefined = [pinned]; ; ) {
    if (from === undefined)
      throw new StackQueueNodeRemovedError(pinned.headerHash);
    const attested = [...from].reverse().find((node) => node.attested);
    if (attested !== undefined) return attested;
    if (pastAttestationBound(ports, pinned))
      throw new StackQueueTailUnattestedError(pinned.headerHash);
    ports.onStage(
      `stack queue block ${pinned.headerHash} awaits its DA attestation`,
    );
    await ports.pause();
    from = await ports.readFrom(pinned.headerHash);
  }
};

/**
 * A stack journey's producer-path DA: its blocks' payloads published as
 * operator-2's node would, followed through the stack's own committee and
 * L1, and the stack's healthy queue tail read back from public DA.
 */
export const stackJourneyDa = (
  context: StackJourneyContext,
  input: Pick<JourneyFixtureStage, "readConfirmedTransaction" | "onStage"> & {
    pause?: () => Promise<void>;
  },
) => {
  const { deployment, provider, kupoUrl } = context;
  const { contracts, chain, operatorLucid: lucid } = deployment;
  const pause = input.pause ?? (() => delay(STACK_DA_POLL_MS));
  const { publish } = context.daProducer;
  const nodeUnit = (headerHash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    );
  const readTarget = async (headerHash: string): Promise<StackDaTarget> => {
    const outputs = await provider.getUtxosWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      nodeUnit(headerHash),
    );
    if (outputs.length > 1)
      throw new Error(`Ambiguous state queue header ${headerHash}`);
    const output = outputs[0];
    if (output === undefined) return { kind: "absent" };
    const node = await decodeStateQueueNode(
      output,
      contracts.stateQueue.policyId,
    );
    return node.da_attestation === SDK.NO_DA_ATTESTATION
      ? { kind: "unattested", blockTime: node.block_time }
      : { kind: "attested", txHash: output.txHash };
  };
  const readRemoval = async (headerHash: string) => {
    const { readConfirmedTransaction } = input;
    if (readConfirmedTransaction === undefined)
      throw new Error(
        `Header ${headerHash} left the state queue, and this session reads no confirmed transactions to authenticate its removal`,
      );
    input.onStage(
      `DA target ${headerHash} removed; authenticating its removal`,
    );
    const unit = nodeUnit(headerHash);
    const deadline = chain.now() + CONSUMPTION_INDEX_GRACE_MS;
    let consumptions = await readKupoUnitConsumptions(kupoUrl, unit);
    while (consumptions.length === 0 && chain.now() <= deadline) {
      await chain.delaySlots(1);
      consumptions = await readKupoUnitConsumptions(kupoUrl, unit);
    }
    return classifyStackDaRemoval({
      headerHash,
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      consumptions,
      readConfirmedTransaction,
      slotToUnixTime: (slot) => lucid.slotToUnixTime(slot),
      attestationTimeoutMs: SDK.DA_ATTESTATION_TIMEOUT_MS,
    });
  };
  /** The stack's state queue's blocks, root excluded, root first. */
  const readQueueBlocks = async () => {
    const sorted = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(lucid, {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      }),
    );
    if (sorted.length === 0) throw new Error("State queue has no root");
    const blocks = sorted.flatMap((entry) =>
      entry.datum.key === "Empty"
        ? []
        : [{ headerHash: entry.datum.key.Key.key, utxo: entry.utxo }],
    );
    if (blocks.length === 0)
      throw new Error(
        "The stack's state queue holds no committed block to extend",
      );
    return blocks;
  };
  const decodeQueueBlock = async (
    block: Readonly<{ headerHash: string; utxo: UTxO }>,
  ) => {
    const node = await decodeStateQueueNode(
      block.utxo,
      contracts.stateQueue.policyId,
    );
    return {
      headerHash: block.headerHash,
      blockTime: node.block_time,
      attested: node.da_attestation !== SDK.NO_DA_ATTESTATION,
      header: node.header,
      // The transaction whose output now holds the block: its commit or its
      // DA apply. Either carries the committed header on L1.
      commitTxHash: block.utxo.txHash,
    };
  };
  const queuePorts = () => ({
    readTail: async () => decodeQueueBlock((await readQueueBlocks()).at(-1)!),
    readFrom: async (headerHash: string) => {
      const blocks = await readQueueBlocks();
      const index = blocks.findIndex(
        (block) => block.headerHash === headerHash,
      );
      return index === -1
        ? undefined
        : Promise.all(blocks.slice(index).map(decodeQueueBlock));
    },
    now: () => chain.now(),
    pause,
    attestationTimeoutMs: SDK.DA_ATTESTATION_TIMEOUT_MS,
    onStage: input.onStage,
  });
  const withPublicPayload = async (
    headerHash: string,
    header: SDK.Header,
    commitTxHash: string,
  ): Promise<JourneySuccessor> => {
    const { payloadEnvelopeCbor } = await fetchJourneyPublicDaPayload({
      watcherConfig: await readStackJourneyWatcherConfig(
        context.watcher.processConfigPath,
      ),
      deploymentIdentity: context.release.deploymentIdentity,
      headerHash,
      availability: { waitMs: STACK_PUBLIC_DA_TRAIL_MS },
    });
    return { header, headerHash, payloadEnvelopeCbor, commitTxHash };
  };
  const followPorts = (headerHash: string) => ({
    readTarget: () => readTarget(headerHash),
    readRemoval: () => readRemoval(headerHash),
    now: () => chain.now(),
    pause,
    attestationTimeoutMs: SDK.DA_ATTESTATION_TIMEOUT_MS,
  });
  return {
    /**
     * Publish a committed block's payload and follow it to its outcome. A
     * block that ages out where it had to be attested, or whose refusal was
     * not observed by its attestation deadline, can never be staged again:
     * its checkpoints are archived so the next run stages afresh. `delivery`
     * keeps the committee peers that took the payload across a restart.
     */
    publish: async (
      block: JourneyBlock,
      expectation: StackDaExpectation,
      checkpointPaths: readonly string[],
      delivery?: {
        saved?: StackDaDelivery;
        save(delivery: StackDaDelivery): Promise<void>;
      },
    ) => {
      try {
        return await publishStackDaPayload(block, expectation, {
          ...followPorts(block.headerHash),
          publish,
          committeeThreshold: context.daProducer.manifest.threshold,
          ...(delivery === undefined ? {} : { delivery }),
          onStage: input.onStage,
        });
      } catch (error) {
        if (
          error instanceof StackDaPayloadAgedOutError ||
          error instanceof StackDaRefusalNotObserved
        )
          archiveAgedOutCheckpoints(checkpointPaths, new Date(), input.onStage);
        throw error;
      }
    },
    /** A refused block's authenticated timeout removal, within its bound. */
    awaitAgedOut: (headerHash: string, attestationDeadlineMs: number) =>
      awaitStackDaAgedOut(
        headerHash,
        attestationDeadlineMs,
        followPorts(headerHash),
      ),
    /**
     * The stack's current queue tail once the committee has attested it,
     * with the payload public DA serves for it. A tail that ages out instead
     * is replaced by the next tail.
     */
    readAttestedTail: async (): Promise<JourneySuccessor> => {
      const { headerHash, header, commitTxHash } =
        await awaitAttestedStackQueueTail(queuePorts());
      return withPublicPayload(headerHash, header, commitTxHash);
    },
    /**
     * The newest attested block at or after the stack's queue tail as this
     * call first reads it, with its payload from public DA: the state the
     * stack has committed to by then, however many newer unattested tails
     * arrive meanwhile. That first tail must be attested within its bound.
     */
    readAttestedAtOrAfterTail: async (): Promise<JourneySuccessor> => {
      const { headerHash, header, commitTxHash } =
        await awaitAttestedAtOrAfterStackQueueTail(queuePorts());
      return withPublicPayload(headerHash, header, commitTxHash);
    },
  };
};
