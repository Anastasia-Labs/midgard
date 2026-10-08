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

import type { EmulatorFixture } from "../deposit-flow-emulator-shared.js";
import { submitHistoryObservation } from "./history-projection-observations.js";

const outRef = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/** A removal of the timed-out state-queue tail `targetHeaderHash` by the
 * attestation-timeout correction: the deployed validators, a signed
 * transaction from the operator's wallet, accepted by the emulator.
 * `submit` waits until the tail is removable, lands the removal and checks
 * the queue it leaves; it returns the signed bytes, so a test can land the
 * same removal again after a rollback discarded it. */
export const prepareTimedOutTailRemoval = async ({
  fixture,
  targetHeaderHash,
}: {
  fixture: EmulatorFixture;
  targetHeaderHash: string;
}) => {
  const { contracts, emulator, operatorLucid: lucid } = fixture;
  const config = {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  };
  const fetchQueue = () =>
    Effect.runPromise(SDK.fetchSortedStateQueueUTxOsProgram(lucid, config));
  const beforeQueue = await fetchQueue();
  const target = beforeQueue.at(-1)!;
  const predecessor = beforeQueue.at(-2)!;
  expect(
    await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(target)),
  ).toBe(targetHeaderHash);
  expect(target.datum.next).toBe("Empty");
  expect(predecessor.datum.next).toEqual({ Key: { key: targetHeaderHash } });
  let submitted = false;
  const submit = async () => {
    if (submitted) throw new Error("The timed-out tail was already removed");
    submitted = true;
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
    return {
      accepted,
      acceptedHeight,
      correctedPredecessor,
      signedCbor: accepted.signedCbor,
    };
  };
  return { submit };
};
