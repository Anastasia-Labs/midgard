import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Data,
  type Script,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect, vi } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import {
  reconcileStateQueueCorrectionObserver,
  type StateQueueCorrectionObserverSource,
  type StateQueueCorrectionObserverState,
} from "../../src/services/state-queue-correction-observer.js";
import { reincludeFinalizedStateQueueCorrectionTransition } from "../../src/services/state-queue-correction-recovery.js";
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

/** Real deployed validators, signed transaction and emulator-confirmed inputs /
 * outputs. Only block/chain-point names and observer transport are synthetic;
 * depth is counted from actual emulator block advancement. This is not live-L1
 * acceptance or a finalized production deployment-manifest fixture. */
export const correctAcceptedT1BlockAfterTimeout = async ({
  fixture,
  targetHeaderHash,
  requiredFinalityDepth,
  runDatabase,
}: {
  fixture: EmulatorFixture;
  targetHeaderHash: string;
  requiredFinalityDepth: bigint;
  runDatabase: <A, E>(
    program: Effect.Effect<A, E, SqlClient.SqlClient>,
  ) => Promise<A>;
}) => {
  const { contracts, emulator } = fixture;
  const pending = Option.getOrThrow(
    await runDatabase(Pending.retrieveActive()),
  );
  expect(pending.header_hash.toString("hex")).toBe(targetHeaderHash);
  expect(pending.intended_tx_hash).not.toBeNull();
  expect(pending.submitted_tx_hash).toEqual(pending.intended_tx_hash);
  const deploymentIdentityDigest = pending.deployment_manifest_id;
  const removal = await prepareTimedOutTailRemoval({
    fixture,
    targetHeaderHash,
    deploymentIdentityDigest,
  });
  const { source, previousQueue } = removal;
  let observerState: StateQueueCorrectionObserverState | null = null;
  let reinclusions = 0;
  let admittedDigest: string | undefined;
  const reconcile = () =>
    reconcileStateQueueCorrectionObserver({
      deploymentIdentityDigest,
      stateQueuePolicyId: contracts.stateQueue.policyId,
      requiredFinalityDepth,
      source,
      store: {
        load: async () => structuredClone(observerState),
        save: async (state) => {
          observerState = structuredClone(state);
        },
      },
      reinclude: async (transition) => {
        const result = await runDatabase(
          reincludeFinalizedStateQueueCorrectionTransition(transition, {
            expectedDeploymentIdentityDigest: deploymentIdentityDigest,
            requiredFinalityDepth,
          }),
        );
        expect(result).toHaveLength(1);
        expect(result[0]).toMatchObject({
          headerHash: targetHeaderHash,
          journalFound: true,
          reopenedEvents: 1,
        });
        admittedDigest = transition.transitionDigest;
        reinclusions++;
      },
      restoreAfterRollback: async () => {
        throw new Error("No rollback was performed in this timeout fixture");
      },
    });
  expect((await reconcile()).status).toBe("bootstrapped");
  const {
    accepted,
    acceptedHeight,
    checkpoint,
    correctedPredecessor,
    nextQueue,
  } = await removal.submit();
  if (requiredFinalityDepth > 1n) {
    expect((await reconcile()).admittedTransactionHashes).toEqual([]);
    expect(reinclusions).toBe(0);
    expect(
      Option.getOrThrow(await runDatabase(Pending.retrieveActive())),
    ).toEqual(pending);
    emulator.awaitBlock(Number(requiredFinalityDepth - 1n));
    vi.setSystemTime(new Date(emulator.now()));
  }
  expect((await reconcile()).admittedTransactionHashes).toEqual([
    accepted.transaction.txHash,
  ]);
  expect(reinclusions).toBe(1);
  const corrected = Option.getOrThrow(
    await runDatabase(Pending.retrieveByHeaderHash(pending.header_hash)),
  );
  expect(corrected.status).toBe(Pending.Status.Abandoned);
  expect(corrected.intended_tx_hash).toEqual(pending.intended_tx_hash);
  expect(corrected.signed_tx_cbor).toEqual(pending.signed_tx_cbor);
  expect(corrected.correction_transition_digest).toBe(admittedDigest);
  console.info(
    "T1 accepted timeout correction evidence",
    JSON.stringify(
      {
        scope: "emulator ledger; synthetic ancestry and observer transport",
        signedCbor: accepted.signedCbor,
        measurement: accepted.measurement,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        checkpoint,
        admittedDigest,
        releaseDepth: requiredFinalityDepth,
        acceptedHeight,
        releaseHeight: emulator.blockHeight,
        previousQueue,
        nextQueue,
      },
      (_key, value) => (typeof value === "bigint" ? value.toString() : value),
    ),
  );
  return { correctedPredecessor, accepted, checkpoint };
};
