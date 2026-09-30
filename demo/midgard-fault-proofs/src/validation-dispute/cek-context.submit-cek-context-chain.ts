import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { deriveCekRedeemerItemPlan } from "../redeemer-item-plan.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { type CekContextStageKey } from "./cek-context.derive-cek-context-item-return-plan.js";
import { deriveCekContextPlan } from "./cek-context.derive-cek-context-plan.js";

export const submitCekContextChain = async ({
  lucid,
  signer,
  contracts,
  binder,
  binderReference,
  stageReferences,
  sharedItem,
  threadUtxo: initialThread,
  threadUnit,
  prepared,
  transition,
  auxiliary,
  successorWorkWitnessCbor,
  evidenceReferences = [],
  awardDatum,
  getValidityRange,
  maxTransactions,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly contracts: {
    readonly award: SDK.SpendingValidator;
    readonly cekContextStages: SDK.CekContextStages;
  };
  readonly binder: SDK.SpendingValidator;
  readonly binderReference?: UTxO;
  readonly stageReferences: Readonly<Partial<Record<CekContextStageKey, UTxO>>>;
  readonly sharedItem?: {
    readonly stages: SDK.SharedRedeemerItemStages;
    readonly deploymentId: string;
    readonly references: ReadonlyMap<string, UTxO>;
  };
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly prepared: Data;
  readonly transition: Data;
  readonly auxiliary: Data;
  readonly successorWorkWitnessCbor: string;
  readonly evidenceReferences?: readonly UTxO[];
  readonly awardDatum: string;
  readonly getValidityRange: () => {
    readonly validFrom: number;
    readonly validTo: number;
  };
  readonly maxTransactions?: number;
}) => {
  if (
    maxTransactions !== undefined &&
    (!Number.isSafeInteger(maxTransactions) || maxTransactions < 1)
  )
    throw new Error("CEK context transaction limit must be positive");
  const plan = deriveCekContextPlan({
    prepared,
    transition,
    auxiliary,
    successorWorkWitnessCbor,
  });
  const keys: string[] = ["binder", ...plan.route];
  const outputStates = [...plan.states];
  const stageContracts: Record<string, SDK.SpendingValidator> = {
    binder,
    ...contracts.cekContextStages,
  };
  const references: Record<string, UTxO | undefined> = {
    binder: binderReference,
    ...stageReferences,
  };
  const sharedRedeemers = new Map<
    string,
    (input: bigint, output: bigint) => Data
  >();
  if (plan.item !== undefined) {
    if (sharedItem === undefined)
      throw new Error("CEK item route requires its deployed shared stages");
    const shared = deriveCekRedeemerItemPlan({
      pending: plan.item.pending,
      witness: plan.item.witness,
      stages: sharedItem.stages,
      deploymentId: sharedItem.deploymentId,
    });
    const insertion = keys.indexOf("itemBind") + 1;
    const sharedKeys = shared.map((step) => `shared:${step.key}`);
    keys.splice(insertion, 0, ...sharedKeys);
    outputStates.splice(
      insertion,
      0,
      ...shared.map((step) => step.outputState),
    );
    for (const [index, step] of shared.entries()) {
      const key = sharedKeys[index]!;
      stageContracts[key] = step.validator;
      references[key] = sharedItem.references.get(
        step.validator.spendingScriptHash,
      );
      sharedRedeemers.set(key, step.spendRedeemer);
    }
  }
  const stateDatum = (bound: Data) =>
    Data.to(
      { fraud_prover: signer.paymentKeyHash, data: bound },
      SDK.CekContextDatum,
    );
  const initialDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: Data.from(Data.to(prepared), SDK.PreparedValidationResolutionState),
    },
    SDK.PreparedValidationResolutionDatum,
  );
  let threadUtxo = initialThread;
  let foundCheckpoint = false;
  const transactions: {
    kind: "authenticate" | "proof" | "settle";
    txHash: string;
    nextThreadOutRef: string;
    completeSignedBytes: number;
    inputIndex: number;
    outputIndex: number;
  }[] = [];
  for (let index = 0; index < keys.length; index++) {
    const key = keys[index]!;
    const contract = stageContracts[key]!;
    const expectedDatum =
      index === 0 ? initialDatum : stateDatum(outputStates[index - 1]!);
    if (!foundCheckpoint) {
      if (
        threadUtxo.address !== contract.spendingScriptAddress ||
        threadUtxo.datum == null ||
        Data.to(Data.from(threadUtxo.datum)) !==
          Data.to(Data.from(expectedDatum))
      )
        continue;
      foundCheckpoint = true;
    }
    const reference = references[key];
    if (reference === undefined)
      throw new Error(`Missing CEK context ${key} reference script`);
    const nextKey = keys[index + 1];
    if (key !== "settle" && (nextKey === undefined || nextKey === "binder"))
      throw new Error("Missing CEK context successor");
    const nextContract =
      nextKey === undefined || nextKey === "binder"
        ? contracts.award
        : stageContracts[nextKey]!;
    const nextDatum =
      key === "settle" ? awardDatum : stateDatum(outputStates[index]!);
    const currentInput = threadUtxo;
    let inputIndex = -1;
    let outputIndex = -1;
    const redeemer = ((ctx) => {
      inputIndex = Number(
        SDK.requireInputIndex(ctx, currentInput, `CEK context ${key}`),
      );
      outputIndex = Number(
        SDK.requireUniqueOutputIndex(
          ctx.outputs,
          computationThreadOutputPredicate({
            address: nextContract.spendingScriptAddress,
            datum: nextDatum,
            unit: threadUnit,
          }),
          `CEK context ${key}`,
        ),
      );
      const sharedRedeemer = sharedRedeemers.get(key);
      if (sharedRedeemer !== undefined)
        return Data.to(sharedRedeemer(BigInt(inputIndex), BigInt(outputIndex)));
      return SDK.encodeCekContextRedeemer(
        BigInt(inputIndex),
        BigInt(outputIndex),
        transition,
        auxiliary,
        key === "itemBind" ? plan.item?.claimedNext : undefined,
      );
    }) satisfies BuildTxWithRedeemer;
    signer.selectWallet(lucid);
    const { validFrom, validTo } = getValidityRange();
    const unsigned = await lucid
      .newTx()
      .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
      .collectFrom([currentInput], redeemer)
      .readFrom([reference, ...evidenceReferences])
      .pay.ToContract(
        nextContract.spendingScriptAddress,
        { kind: "inline", value: nextDatum },
        { lovelace: currentInput.assets.lovelace ?? 0n, [threadUnit]: 1n },
      )
      .validFrom(validFrom)
      .validTo(validTo)
      .addSignerKey(signer.paymentKeyHash)
      .complete({ localUPLCEval: true })
      .catch((error: unknown) => {
        throw new Error(
          `CEK context ${key} transaction failed: ${String(error)}`,
        );
      });
    const signed = await unsigned.sign.withWallet().complete();
    const completeSignedBytes = signed.toCBOR().length / 2;
    if (completeSignedBytes > 16384)
      throw new Error(
        `CEK context ${key} exceeds maximum signed transaction bytes`,
      );
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    const nextThreadOutRef = `${txHash}#${outputIndex}`;
    threadUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: { txHash, outputIndex },
      label: `CEK context ${key} continuation`,
    });
    transactions.push({
      kind:
        key === "binder"
          ? "authenticate"
          : key === "settle"
            ? "settle"
            : "proof",
      txHash,
      nextThreadOutRef,
      completeSignedBytes,
      inputIndex,
      outputIndex,
    });
    if (
      maxTransactions !== undefined &&
      transactions.length === maxTransactions
    )
      return { transactions, threadUtxo, completed: key === "settle" };
  }
  if (!foundCheckpoint)
    throw new Error(
      "Live CEK context checkpoint does not match retained evidence",
    );
  return { transactions, threadUtxo, completed: true };
};
