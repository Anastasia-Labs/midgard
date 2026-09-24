/** Compose production fabricated-family builders on an existing published chain. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitRemoveFraudulentBlock } from "../../../src/remove-fraudulent-block.js";
import {
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "../../../src/runtime.js";
import {
  deriveFabricatedDepositStep01Handoff,
  parseSubmitFabricatedDepositInclusion,
  submitFabricatedDepositStep01,
} from "../../../src/submit-fabricated-deposit-step-01.js";
import { submitFabricatedDepositStep02 } from "../../../src/submit-fabricated-deposit-step-02.js";
import { submitFabricatedDepositStep03 } from "../../../src/submit-fabricated-deposit-step-03.js";
import { submitFabricatedDepositStep04 } from "../../../src/submit-fabricated-deposit-step-04.js";
import {
  deriveFabricatedWithdrawalStep01Handoff,
  parseSubmitFabricatedWithdrawalInclusion,
  submitFabricatedWithdrawalStep01,
} from "../../../src/submit-fabricated-withdrawal-step-01.js";
import { submitFabricatedWithdrawalStep02 } from "../../../src/submit-fabricated-withdrawal-step-02.js";
import { submitFabricatedWithdrawalStep03 } from "../../../src/submit-fabricated-withdrawal-step-03.js";
import { submitFabricatedWithdrawalStep04 } from "../../../src/submit-fabricated-withdrawal-step-04.js";
import { submitInit } from "../../../src/submit-init.js";
import {
  bindFraudProofWorkflowDeployment,
  requireManifestBoundReferenceScriptUtxo,
} from "../../../src/workflow/deployment-manifest-binding.js";
import type { FraudProofPreSubmitBoundary } from "../../../src/workflow/transaction-boundary.js";
import { measureCompleteSignedTransaction } from "./measurement.js";

export type RetiredFamilyProofTransaction = Readonly<{
  stage: "init" | "step01" | "step02" | "step03" | "step04" | "remove";
  txHash: string;
  signedCbor: string;
  fee: bigint;
  completeSignedBytes: number;
  executionMemory: bigint;
  executionSteps: bigint;
}>;

export type RetiredFamilyProofInput = Readonly<{
  kind: SDK.EventHistoryKind;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  deployment: Readonly<{
    manifest: unknown;
    blueprintJson: string;
    deploymentInfo: unknown;
    references: ReadonlyMap<string, UTxO>;
  }>;
  headerHash: string;
  stateQueueBlockOutRef: string;
  inclusion: Readonly<{
    keyCbor: string;
    valueCbor: string;
    phasRoot: string;
    membershipProofCbor: string;
  }>;
  now: () => number;
  onSigned: (transaction: RetiredFamilyProofTransaction) => void;
}>;

export type RetiredFamilyProofResult = Readonly<{
  kind: SDK.EventHistoryKind;
  headerHash: string;
  verdict: "DepositIdentityAbsent" | "WithdrawalIdentityAbsent";
  computationThreadUnit: string;
  fraudProofUnit: string;
  fraudProofOutRef: string;
  removalTxHashes: readonly string[];
  transactions: readonly RetiredFamilyProofTransaction[];
}>;

export type EligibleFamilyRefusalResult = Readonly<{
  kind: SDK.EventHistoryKind;
  headerHash: string;
  computationThreadUnit: string;
  preservedThreadOutRef: string;
  preservedStateQueueBlockOutRef: string;
  refusalMessage: string;
  transactions: readonly RetiredFamilyProofTransaction[];
}>;

const prepare = async (input: RetiredFamilyProofInput, absent: boolean) => {
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
  await p.refreshFunding();
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
  await p.refreshFunding();
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
