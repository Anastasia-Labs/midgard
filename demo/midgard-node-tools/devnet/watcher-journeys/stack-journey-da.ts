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
      const watcherConfig = await readStackJourneyWatcherConfig(
        context.watcher.processConfigPath,
      );
      const queue = SDK.fetchSortedStateQueueUTxOsProgram(lucid, {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      });
      for (;;) {
        const last = (await Effect.runPromise(queue)).at(-1);
        if (last === undefined) throw new Error("State queue has no root");
        if (last.datum.key === "Empty")
          throw new Error(
            "The stack's state queue holds no committed block to extend",
          );
        const headerHash = last.datum.key.Key.key;
        const node = await decodeStateQueueNode(
          last.utxo,
          contracts.stateQueue.policyId,
        );
        if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
          const { payloadEnvelopeCbor } = await fetchJourneyPublicDaPayload({
            watcherConfig,
            deploymentIdentity: context.release.deploymentIdentity,
            headerHash,
            availability: { waitMs: STACK_PUBLIC_DA_TRAIL_MS },
          });
          return {
            header: node.header,
            headerHash,
            payloadEnvelopeCbor,
            // The transaction whose output now holds the tail: its commit or
            // its DA apply. Either carries the committed header on L1.
            commitTxHash: last.utxo.txHash,
          };
        }
        // An unattested tail is attested or removed within its timeout.
        if (
          chain.now() >
          Number(node.block_time + 2n * SDK.DA_ATTESTATION_TIMEOUT_MS)
        )
          throw new Error(
            `Stack queue tail ${headerHash} was neither attested nor removed after its DA attestation timeout`,
          );
        input.onStage(
          `stack queue tail ${headerHash} awaits its DA attestation`,
        );
        await pause();
      }
    },
  };
};
