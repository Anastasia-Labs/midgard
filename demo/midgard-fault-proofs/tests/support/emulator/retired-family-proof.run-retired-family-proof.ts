import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitRemoveFraudulentBlock } from "../../../src/remove-fraudulent-block.js";
import { outRefLabel } from "../../../src/runtime.js";
import { submitFabricatedDepositStep04 } from "../../../src/submit-fabricated-deposit-step-04.js";
import { submitFabricatedWithdrawalStep04 } from "../../../src/submit-fabricated-withdrawal-step-04.js";
import { prepare } from "./retired-family-proof.prepare.js";
import {
  type EligibleFamilyRefusalResult,
  type RetiredFamilyProofInput,
  type RetiredFamilyProofResult,
} from "./retired-family-proof.types.js";

/** Full absence proof, permanent evidence and removal on the same real chain. */
export const runRetiredFamilyProof = async (
  input: RetiredFamilyProofInput,
): Promise<RetiredFamilyProofResult> => {
  const p = await prepare(input, true);
  const thirdResult = await p.step03();
  const fault = p.deposit
    ? "NonexistentDepositIdentity"
    : "NonexistentWithdrawalIdentity";
  expect(thirdResult.fault).toBe(fault);
  await p.spent(p.secondResult.nextThreadOutRef);
  const fourth = await p.threadAt(
    thirdResult.nextThreadOutRef,
    p.contracts.steps[3].spendingScriptAddress,
    p.init.computationThreadUnit,
  );
  const fourthDatum = p.deposit
    ? Data.from(fourth.datum!, SDK.FabricatedDepositStep04Datum)
    : Data.from(fourth.datum!, SDK.FabricatedWithdrawalStep04Datum);
  expect(fourthDatum).toEqual({
    fraud_prover: input.signer.paymentKeyHash,
    data: {
      state_queue_policy: p.contracts.stateQueuePolicyId,
      challenged_header_hash: input.headerHash,
      header_start_time: p.header.startTime,
      header_end_time: p.header.endTime,
      [p.deposit ? "committed_deposit_id" : "committed_withdrawal_id"]:
        Data.from(input.inclusion.keyCbor, SDK.OutputReference),
      fault,
    },
  });
  p.selectSigner();
  const lastInput = {
    ...p.common,
    threadOutRef: thirdResult.nextThreadOutRef,
    referenceScriptUtxo: p.references[3],
    witnessReferenceScripts: p.witnesses,
    preSubmitBoundary: p.boundary("step04"),
  };
  const last = p.deposit
    ? await submitFabricatedDepositStep04(lastInput)
    : await submitFabricatedWithdrawalStep04(lastInput);
  expect(last.fault).toBe(fault);
  expect(last.fraudProofAssetName).toBe(p.init.computationThreadAssetName);
  const finalization = p.transactions.find(({ stage }) => stage === "step04");
  expect(finalization?.txHash).toBe(last.txHash);
  const mint = CML.Transaction.from_cbor_hex(finalization!.signedCbor)
    .body()
    .mint();
  expect(
    mint?.get(
      CML.ScriptHash.from_hex(last.computationThreadPolicyId),
      CML.AssetName.from_hex(last.computationThreadAssetName),
    ),
  ).toBe(-1n);
  expect(
    mint?.get(
      CML.ScriptHash.from_hex(last.fraudProofPolicyId),
      CML.AssetName.from_hex(last.fraudProofAssetName),
    ),
  ).toBe(1n);
  await p.spent(thirdResult.nextThreadOutRef);
  const proof = await p.byOutRef(last.fraudProofOutRef);
  expect(proof.assets[last.fraudProofUnit]).toBe(1n);
  expect(Data.from(proof.datum!, SDK.FraudProofTokenDatum)).toEqual({
    fraud_prover: input.signer.paymentKeyHash,
  });
  const marked = await input.lucid.utxoByUnit(p.queueUnit);
  expect(marked.assets).toEqual(p.queue.assets);
  expect(marked.txHash).toBe(last.txHash);
  expect(outRefLabel(marked)).not.toBe(input.stateQueueBlockOutRef);
  const markedView = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(marked),
  );
  expect(
    Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(markedView))
      .proven_fraud,
  ).toBe(p.init.computationThreadAssetName);
  p.selectSigner();
  const now = BigInt(input.now());
  const removed = await submitRemoveFraudulentBlock({
    lucid: input.lucid,
    blueprint: JSON.parse(input.deployment.blueprintJson),
    deploymentInfo: input.deployment.manifest,
    network: p.binding.network,
    signer: input.signer,
    fraudCategory: p.category,
    fraudulentHeaderHash: input.headerHash,
    requireReferenceScripts: true,
    awaitConfirmation: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
    preSubmitBoundary: p.boundary("remove"),
  });
  expect(removed.transactions).toHaveLength(1);
  expect(removed.transactions[0]!.removedHeaderHash).toBe(input.headerHash);
  expect(
    await input.lucid.utxosAtWithUnit(p.queue.address, p.queueUnit),
  ).toHaveLength(0);
  expect(await p.byOutRef(last.fraudProofOutRef)).toEqual(proof);
  expect(p.transactions.map(({ stage }) => stage)).toEqual([
    "init",
    "step01",
    "step02",
    "step03",
    "step04",
    "remove",
  ]);
  return {
    kind: input.kind,
    headerHash: input.headerHash,
    verdict: p.deposit ? "DepositIdentityAbsent" : "WithdrawalIdentityAbsent",
    computationThreadUnit: p.init.computationThreadUnit,
    fraudProofUnit: last.fraudProofUnit,
    fraudProofOutRef: last.fraudProofOutRef,
    removalTxHashes: removed.transactions.map(({ txHash }) => txHash),
    transactions: p.transactions,
  };
};

/** An eligible exact-content capture must refuse before stage03 signs anything. */
export const checkFreshEligibleFamilyRefusal = async (
  input: RetiredFamilyProofInput,
): Promise<EligibleFamilyRefusalResult> => {
  const p = await prepare(input, false);
  const refusalMessage =
    "Authentic eligible event content matches the header commitment";
  await expect(p.step03()).rejects.toThrow(refusalMessage);
  expect(await p.byOutRef(p.secondResult.nextThreadOutRef)).toEqual(p.third);
  expect(await p.byOutRef(input.stateQueueBlockOutRef)).toEqual(p.queue);
  const proofUnit =
    p.contracts.fraudProof.policyId + p.contracts.categoryId + input.headerHash;
  expect(
    await input.lucid.utxosAtWithUnit(
      p.contracts.fraudProof.spendingScriptAddress,
      proofUnit,
    ),
  ).toHaveLength(0);
  expect(p.transactions.map(({ stage }) => stage)).toEqual([
    "init",
    "step01",
    "step02",
  ]);
  return {
    kind: input.kind,
    headerHash: input.headerHash,
    computationThreadUnit: p.init.computationThreadUnit,
    preservedThreadOutRef: p.secondResult.nextThreadOutRef,
    preservedStateQueueBlockOutRef: input.stateQueueBlockOutRef,
    refusalMessage,
    transactions: p.transactions,
  };
};
