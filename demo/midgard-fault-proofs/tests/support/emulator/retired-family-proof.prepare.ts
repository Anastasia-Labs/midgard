import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { parseOutRef } from "../../../src/runtime.js";
import {
  deriveFabricatedDepositStep01Handoff,
  parseSubmitFabricatedDepositInclusion,
  submitFabricatedDepositStep01,
} from "../../../src/submit-fabricated-deposit-step-01.js";
import { submitFabricatedDepositStep02 } from "../../../src/submit-fabricated-deposit-step-02.js";
import { submitFabricatedDepositStep03 } from "../../../src/submit-fabricated-deposit-step-03.js";
import {
  deriveFabricatedWithdrawalStep01Handoff,
  parseSubmitFabricatedWithdrawalInclusion,
  submitFabricatedWithdrawalStep01,
} from "../../../src/submit-fabricated-withdrawal-step-01.js";
import { submitFabricatedWithdrawalStep02 } from "../../../src/submit-fabricated-withdrawal-step-02.js";
import { submitFabricatedWithdrawalStep03 } from "../../../src/submit-fabricated-withdrawal-step-03.js";
import { submitInit } from "../../../src/submit-init.js";
import {
  bindFraudProofWorkflowDeployment,
  requireManifestBoundReferenceScriptUtxo,
} from "../../../src/workflow/deployment-manifest-binding.js";
import type { FraudProofPreSubmitBoundary } from "../../../src/workflow/transaction-boundary.js";
import { measureCompleteSignedTransaction } from "./measurement.js";
import {
  type RetiredFamilyProofInput,
  type RetiredFamilyProofTransaction,
} from "./retired-family-proof.types.js";

export const prepare = async (
  input: RetiredFamilyProofInput,
  absent: boolean,
) => {
  const { lucid, signer, deployment, now } = input;
  const deposit = input.kind === "Deposit";
  const category: "fabricatedDeposit" | "fabricatedWithdrawal" = deposit
    ? "fabricatedDeposit"
    : "fabricatedWithdrawal";
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: deployment.manifest,
    blueprintJson: deployment.blueprintJson,
    deploymentInfo: deployment.deploymentInfo,
    category,
    headerHash: input.headerHash,
    proverCredential: signer.paymentKeyHash,
    stepDatumSchemas: deposit
      ? [
          SDK.FraudProofComputationThreadStepDatum,
          SDK.FabricatedDepositStep02Datum,
          SDK.FabricatedDepositStep03Datum,
          SDK.FabricatedDepositStep04Datum,
        ]
      : [
          SDK.FraudProofComputationThreadStepDatum,
          SDK.FabricatedWithdrawalStep02Datum,
          SDK.FabricatedWithdrawalStep03Datum,
          SDK.FabricatedWithdrawalStep04Datum,
        ],
  });
  const resolved = binding.resolvedContracts;
  const family = deposit
    ? resolved.contracts.fabricatedDeposit
    : resolved.contracts.fabricatedWithdrawal;
  if (family === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error(
      "Published deployment omitted the requested history family",
    );
  const contracts = {
    steps: family.steps,
    history: family.history,
    computationThread: resolved.contracts.computationThread,
    fraudProof: resolved.contracts.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    categoryId: resolved.category.categoryId,
  };
  const reference = (name: string) => {
    const utxo = deployment.references.get(name);
    if (utxo === undefined)
      throw new Error(`Missing published ${name} reference`);
    return requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  };
  const prefix = deposit
    ? "fraudProofFabricatedDeposit"
    : "fraudProofFabricatedWithdrawal";
  const references = [
    reference(prefix),
    reference(`${prefix}Step02`),
    reference(`${prefix}Step03`),
    reference(`${prefix}Step04`),
  ] as const;
  const witnesses = {
    stateQueueSpend: reference("stateQueueSpend"),
    computationThreadMint: reference("computationThreadMint"),
    fraudProofMint: reference("fraudProofMint"),
    phasMembershipWithdraw: reference("phasMembershipWithdraw"),
  };
  const transactions: RetiredFamilyProofTransaction[] = [];
  const boundary =
    (
      stage: RetiredFamilyProofTransaction["stage"],
    ): FraudProofPreSubmitBoundary =>
    async ({ signed }) => {
      const signedCbor = signed.toCBOR();
      const measured = measureCompleteSignedTransaction(signedCbor);
      const protocol = binding.cardanoProtocolParameters;
      expect(measured.completeSignedBytes).toBeLessThanOrEqual(
        Number(protocol.maxTxSize),
      );
      expect(measured.executionMemory).toBeLessThanOrEqual(
        BigInt(protocol.maxTxExUnits.memory),
      );
      expect(measured.executionSteps).toBeLessThanOrEqual(
        BigInt(protocol.maxTxExUnits.steps),
      );
      const record = {
        stage,
        txHash: signed.toHash(),
        signedCbor,
        fee: CML.Transaction.from_cbor_hex(signedCbor).body().fee(),
        ...measured,
      };
      transactions.push(record);
      input.onSigned(record);
    };
  const refreshFunding = async () => {
    signer.selectWallet(lucid);
    lucid.overrideUTxOs(await lucid.utxosAt(signer.address));
  };
  const byOutRef = async (outRef: string) => {
    const found = await lucid.utxosByOutRef([
      parseOutRef(outRef, "family proof output"),
    ]);
    expect(found).toHaveLength(1);
    return found[0]!;
  };
  const threadAt = async (outRef: string, address: string, unit: string) => {
    const utxo = await byOutRef(outRef);
    expect(utxo.address).toBe(address);
    expect(utxo.assets[unit]).toBe(1n);
    return utxo;
  };
  const spent = async (outRef: string) =>
    expect(
      await lucid.utxosByOutRef([parseOutRef(outRef, "consumed proof input")]),
    ).toHaveLength(0);
  const queue = await byOutRef(input.stateQueueBlockOutRef);
  const queueUnit =
    contracts.stateQueuePolicyId +
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    input.headerHash;
  expect(queue.assets[queueUnit]).toBe(1n);
  const header = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(
      await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(queue)),
    ),
  );
  expect(await Effect.runPromise(SDK.hashBlockHeader(header))).toBe(
    input.headerHash,
  );
  const leaf = input.inclusion;
  const depositInclusion = parseSubmitFabricatedDepositInclusion({
    committedDepositIdCbor: leaf.keyCbor,
    committedDepositInfoCbor: leaf.valueCbor,
    depositsPhasRoot: leaf.phasRoot,
    depositMembershipProofCbor: leaf.membershipProofCbor,
  });
  const withdrawalInclusion = parseSubmitFabricatedWithdrawalInclusion({
    committedWithdrawalIdCbor: leaf.keyCbor,
    committedWithdrawalInfoCbor: leaf.valueCbor,
    withdrawalsPhasRoot: leaf.phasRoot,
    withdrawalMembershipProofCbor: leaf.membershipProofCbor,
  });
  await refreshFunding();
  const init = await submitInit({
    lucid,
    blueprint: JSON.parse(deployment.blueprintJson),
    deploymentInfo: deployment.deploymentInfo,
    network: binding.network,
    signer,
    fraudCategory: category,
    fraudulentBlockOutRef: input.stateQueueBlockOutRef,
    fraudulentHeaderHash: input.headerHash,
    witnessReferenceScripts: witnesses,
    preSubmitBoundary: boundary("init"),
    awaitConfirmation: true,
  });
  expect(init.fraudulentHeaderHash).toBe(input.headerHash);
  expect(init.computationThreadAssetName).toBe(
    contracts.categoryId + input.headerHash,
  );
  const initialOutRef = `${init.txHash}#${init.firstStepOutputIndex}`;
  const first = await threadAt(
    initialOutRef,
    init.firstStepAddress,
    init.computationThreadUnit,
  );
  expect(
    Data.from(first.datum!, SDK.FraudProofComputationThreadStepDatum),
  ).toEqual({ fraud_prover: signer.paymentKeyHash, data: null });
  const common = { lucid, contracts, signer, now, awaitConfirmation: true };
  const firstInput = {
    ...common,
    network: binding.network,
    threadOutRef: initialOutRef,
    stateQueueBlockOutRef: input.stateQueueBlockOutRef,
    referenceScriptUtxo: references[0],
    preSubmitBoundary: boundary("step01"),
  };
  const expected02 = deposit
    ? (
        await deriveFabricatedDepositStep01Handoff({
          stateQueuePolicyId: contracts.stateQueuePolicyId,
          header,
          headerHash: input.headerHash,
          inclusion: depositInclusion,
        })
      ).step02State
    : (
        await deriveFabricatedWithdrawalStep01Handoff({
          stateQueuePolicyId: contracts.stateQueuePolicyId,
          header,
          headerHash: input.headerHash,
          inclusion: withdrawalInclusion,
        })
      ).step02State;
  await refreshFunding();
  const firstResult = deposit
    ? await submitFabricatedDepositStep01({ ...firstInput, depositInclusion })
    : await submitFabricatedWithdrawalStep01({
        ...firstInput,
        withdrawalInclusion,
      });
  await spent(initialOutRef);
  const second = await threadAt(
    firstResult.nextThreadOutRef,
    contracts.steps[1].spendingScriptAddress,
    init.computationThreadUnit,
  );
  expect(
    deposit
      ? Data.from(second.datum!, SDK.FabricatedDepositStep02Datum)
      : Data.from(second.datum!, SDK.FabricatedWithdrawalStep02Datum),
  ).toEqual({ fraud_prover: signer.paymentKeyHash, data: expected02 });
  await refreshFunding();
  const secondInput = {
    ...common,
    network: binding.network,
    threadOutRef: firstResult.nextThreadOutRef,
    evidence: absent
      ? { kind: "absent_identity" as const }
      : { kind: "present_event" as const },
    referenceScriptUtxo: references[1],
    preSubmitBoundary: boundary("step02"),
  };
  const secondResult = deposit
    ? await submitFabricatedDepositStep02(secondInput)
    : await submitFabricatedWithdrawalStep02(secondInput);
  await spent(firstResult.nextThreadOutRef);
  const third = await threadAt(
    secondResult.nextThreadOutRef,
    contracts.steps[2].spendingScriptAddress,
    init.computationThreadUnit,
  );
  expect(
    deposit
      ? Data.from(third.datum!, SDK.FabricatedDepositStep03Datum)
      : Data.from(third.datum!, SDK.FabricatedWithdrawalStep03Datum),
  ).toEqual({
    fraud_prover: signer.paymentKeyHash,
    data: { ...expected02, verdict: secondResult.verdict },
  });
  expect(secondResult.evidenceKind).toBe(
    absent ? "absent_identity" : "present_event",
  );
  if (absent) {
    expect(secondResult.verdict).toBe(
      deposit ? "DepositIdentityAbsent" : "WithdrawalIdentityAbsent",
    );
    expect(secondResult.openingCbor).toBeNull();
  } else {
    expect(secondResult.openingCbor).toEqual(expect.any(String));
    expect(typeof secondResult.verdict).toBe("object");
  }
  const thirdInput = {
    ...common,
    threadOutRef: secondResult.nextThreadOutRef,
    ...(secondResult.openingCbor === null
      ? {}
      : { openingCbor: secondResult.openingCbor }),
    referenceScriptUtxo: references[2],
    preSubmitBoundary: boundary("step03"),
  };
  const step03 = async () => {
    await refreshFunding();
    return deposit
      ? submitFabricatedDepositStep03(thirdInput)
      : submitFabricatedWithdrawalStep03(thirdInput);
  };
  return {
    binding,
    category,
    contracts,
    references,
    witnesses,
    transactions,
    boundary,
    refreshFunding,
    byOutRef,
    threadAt,
    spent,
    queue,
    queueUnit,
    init,
    secondResult,
    third,
    step03,
    common,
    deposit,
    header,
  };
};
