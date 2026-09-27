import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type Script,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import { type StateQueueCorrectionObserverSource } from "../../src/services/state-queue-correction-observer.js";
import type { EmulatorFixture } from "../deposit-flow-emulator-shared.js";
import { submitHistoryObservation } from "./history-projection-observations.js";

const outRef = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/** Real deployed validators, signed removal transaction and emulator-confirmed
 * inputs / outputs for the timed-out state-queue tail. Only block/chain-point
 * names and observer transport are synthetic; depth is counted from actual
 * emulator block advancement. The returned source reports nothing until
 * `submit` has been ledger-accepted, so an observer can bootstrap its cursor
 * on the pre-removal queue first. */
export const prepareTimedOutTailRemoval = async ({
  fixture,
  targetHeaderHash,
  deploymentIdentityDigest,
}: {
  fixture: EmulatorFixture;
  targetHeaderHash: string;
  deploymentIdentityDigest: string;
}) => {
  const { contracts, emulator, operatorLucid: lucid } = fixture;
  const config = {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  };
  const fetchQueue = () =>
    Effect.runPromise(SDK.fetchSortedStateQueueUTxOsProgram(lucid, config));
  const queueNodes = async (queue: readonly SDK.StateQueueUTxO[]) =>
    Promise.all(
      queue.map(async (node, index) => ({
        headerHash:
          index === 0
            ? null
            : await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(node)),
        outRef: outRef(node.utxo),
      })),
    );
  const beforeQueue = await fetchQueue();
  const target = beforeQueue.at(-1)!;
  const predecessor = beforeQueue.at(-2)!;
  expect(
    await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(target)),
  ).toBe(targetHeaderHash);
  expect(target.datum.next).toBe("Empty");
  expect(predecessor.datum.next).toEqual({ Key: { key: targetHeaderHash } });
  const previousQueue = await queueNodes(beforeQueue);
  const recorded: {
    checkpoint?: SDK.StateQueueAuthenticatedReplayCheckpoint;
    acceptedHeight?: number;
  } = {};
  const source: StateQueueCorrectionObserverSource = {
    readQueue: async () => queueNodes(await fetchQueue()),
    observeTransitions: async (previous, next) => {
      const { checkpoint } = recorded;
      if (checkpoint === undefined)
        throw new Error("Correction was not ledger-accepted");
      expect(previous).toEqual(checkpoint.previousQueue);
      expect(next).toEqual(checkpoint.nextQueue);
      return [checkpoint];
    },
    canonicalDepth: async (transition) => {
      const { checkpoint, acceptedHeight } = recorded;
      if (checkpoint === undefined || acceptedHeight === undefined)
        throw new Error("Missing accepted correction receipt");
      expect(transition.transactionHash).toBe(checkpoint.transactionHash);
      const status = await lucid.transactionStatus(transition.transactionHash);
      if (status.status !== "confirmed") return null;
      expect(await queueNodes(await fetchQueue())).toEqual(
        checkpoint.nextQueue,
      );
      return BigInt(emulator.blockHeight - acceptedHeight + 1);
    },
  };
  const submit = async () => {
    if (recorded.checkpoint !== undefined)
      throw new Error("The timed-out tail was already removed");
    const header = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(target.datum),
    );
    const readyAt =
      Number(header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS) + 60_000;
    if (emulator.now() < readyAt)
      emulator.awaitSlot(Math.ceil((readyAt - emulator.now()) / 1000));
    vi.setSystemTime(new Date(emulator.now()));
    const lock = await Effect.runPromise(
      SDK.fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
        hubOraclePolicyId: contracts.hubOracle.policyId,
      }),
    );
    expect(lock.datum).toBe("Idle");
    const hub = await lucid.utxoByUnit(
      toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    );
    const published = await lucid.utxosAt(
      fixture.referenceScripts.init.stateQueueMinting.address,
    );
    const reference = (script: Script): UTxO => {
      const hash = validatorToScriptHash(script);
      const matches = published.filter(
        (entry) =>
          entry.scriptRef != null &&
          validatorToScriptHash(entry.scriptRef) === hash,
      );
      expect(matches.length).toBeGreaterThan(0);
      return matches[0]!;
    };
    const withdrawal =
      contracts.stateQueue.yields.unattestedTimeout.withdrawalScript;
    const validFrom = BigInt(emulator.now() - 60_000);
    const builder = SDK.incompleteRemoveLastUnattestedBlockTxProgram(
      lucid,
      config,
      {
        timedOutBlockUTxO: target,
        predecessorUTxO: predecessor,
        hubOracleRefInput: hub,
        correctionLockInput: lock,
        correctionLockSpendingScript: contracts.correctionLock.spendingScript,
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        validFrom,
        validTo: validFrom + 300_000n,
        referenceScripts: {
          stateQueueSpend: reference(contracts.stateQueue.spendingScript),
          stateQueueMint: reference(contracts.stateQueue.mintingScript),
          correctionLockSpend: reference(
            contracts.correctionLock.spendingScript,
          ),
        },
        yieldWitness: {
          script: withdrawal,
          referenceInput: reference(withdrawal),
        },
      },
    );
    const accepted = await submitHistoryObservation(
      lucid,
      await builder.complete({ localUPLCEval: true }),
    );
    const acceptedHeight = emulator.blockHeight;
    const acceptedSlot = emulator.slot;
    const parameters = await lucid.config().provider!.getProtocolParameters();
    expect(accepted.measurement.completeSignedBytes).toBeLessThanOrEqual(
      parameters.maxTxSize,
    );
    expect(accepted.measurement.executionMemory).toBeLessThanOrEqual(
      BigInt(parameters.maxTxExMem),
    );
    expect(accepted.measurement.executionSteps).toBeLessThanOrEqual(
      BigInt(parameters.maxTxExSteps),
    );
    const afterQueue = await fetchQueue();
    expect(afterQueue).toHaveLength(beforeQueue.length - 1);
    const correctedPredecessor = afterQueue.at(-1)!;
    expect(correctedPredecessor.datum).toEqual({
      ...predecessor.datum,
      next: "Empty",
    });
    expect(correctedPredecessor.utxo.txHash).toBe(accepted.transaction.txHash);
    expect(correctedPredecessor.utxo.assets).toEqual(predecessor.utxo.assets);
    const spent = accepted.transaction.inputs.map(outRef);
    expect(spent).toEqual(
      expect.arrayContaining([
        outRef(target.utxo),
        outRef(predecessor.utxo),
        outRef(lock.utxo),
      ]),
    );
    const nextLock = await Effect.runPromise(
      SDK.fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
        hubOraclePolicyId: contracts.hubOracle.policyId,
      }),
    );
    expect(nextLock.utxo.txHash).toBe(accepted.transaction.txHash);
    expect(nextLock.datum).toBe("Idle");
    expect(
      accepted.transaction.outputs.find(
        (output) => outRef(output) === outRef(nextLock.utxo),
      )?.datum,
    ).toBe(Data.to(nextLock.datum, SDK.CorrectionLockDatum));
    const blockHash = createHash("sha256")
      .update(
        `emulator-correction:${acceptedHeight}:${acceptedSlot}:${accepted.transaction.txHash}`,
      )
      .digest("hex");
    const checkpoint =
      SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
        deploymentIdentityDigest,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        transactionHash: accepted.transaction.txHash,
        blockHash,
        slot: acceptedSlot.toString(),
        blockNo: acceptedHeight.toString(),
        transactionIndex: "0",
        chainPointId: blockHash,
        finalityDepth: "1",
        mintPolicyIds: [
          ...new Set(
            Object.keys(accepted.transaction.mint).map((unit) =>
              unit.slice(0, 56),
            ),
          ),
        ].sort(),
        redeemers: accepted.transaction.redeemers.map((redeemer) => ({
          purpose: redeemer.purpose,
          index: redeemer.index.toString(),
          cborHex: redeemer.cbor,
        })),
        spentInputOutRefs: spent,
        referenceInputOutRefs: accepted.transaction.references.map(outRef),
        correctionLockWitness: {
          kind: "correction_transition",
          consumedOutRef: outRef(lock.utxo),
          continuedOutRef: outRef(nextLock.utxo),
          targetHeaderHash,
          correctionIdentity: "AttestationTimeout",
          previousDatum: lock.datum,
          nextDatum: nextLock.datum,
        },
        previousQueue,
        nextQueue: await queueNodes(afterQueue),
      }) ?? undefined;
    if (checkpoint === undefined)
      throw new Error("Accepted removal has no authenticated checkpoint");
    recorded.checkpoint = checkpoint;
    recorded.acceptedHeight = acceptedHeight;
    expect(checkpoint.checkpointKind).toBe("timeout_correction");
    expect(checkpoint.terminalTransition?.removedHeaderHashes).toEqual([
      targetHeaderHash,
    ]);
    return {
      accepted,
      acceptedHeight,
      checkpoint,
      correctedPredecessor,
      nextQueue: await queueNodes(afterQueue),
    };
  };
  return { source, submit, previousQueue };
};
