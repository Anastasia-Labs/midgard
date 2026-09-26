import {
  advanceMidgardRedeemerItemProof,
  finalizeMidgardRedeemerItemProof,
  hashMidgardRedeemerItemProofControl,
  initialMidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";
import { hashMidgardRedeemerItemLeaf } from "@al-ft/midgard-core/script-proof";
import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  redeemerItemControlData,
  summarizeMidgardCekLucidData,
} from "@al-ft/midgard-validation";
import {
  cardanoScriptPurposeData,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "@al-ft/midgard-validation/midgard-redeemers";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
} from "../redeemer-item-data.js";
import { deriveCekRedeemerItemPlan } from "../redeemer-item-plan.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";

export type CekContextStageKey = keyof SDK.CekContextStages;

/** Retained from adjacent fresh canonical replay, never from operator authority. */
export type CekContextSuccessorEvidence = {
  readonly successorWorkWitnessCbor: string;
};

const fields = (value: Data, tag: number, count: number): Data[] => {
  if (
    !(value instanceof Constr) ||
    value.index !== tag ||
    value.fields.length !== count
  )
    throw new Error(`Expected exact CEK context constructor ${tag}/${count}`);
  return value.fields;
};
const integer = (value: Data): bigint => {
  if (typeof value !== "bigint")
    throw new Error("Expected CEK context integer");
  return value;
};
const cborArray = SDK.decodeCekContextCborArray;

const record = (values: Data[]) => new Constr(0, values);

/** Every proposed output remains checked by the corresponding on-chain leaf. */
export const deriveCekContextPlan = ({
  prepared,
  transition,
  auxiliary,
  successorWorkWitnessCbor,
}: CekContextSuccessorEvidence & {
  readonly prepared: Data;
  readonly transition: Data;
  readonly auxiliary: Data;
}) => {
  const witness = Data.from(Data.to(transition), SDK.ValidationOneStepWitness);
  const base = Data.from(
    Data.to(prepared),
    SDK.PreparedValidationResolutionState,
  );
  const successor = witness.claimed_successor;
  if (successor.phase !== "Cek")
    throw new Error("Context successor must remain in CEK");
  const successorHash = hashMidgardValidationWorkWitness({
    phase: "cek",
    programCounter: Number(successor.program_counter),
    witnessCbor: Buffer.from(successorWorkWitnessCbor, "hex"),
  }).toString("hex");
  if (successorHash !== successor.work_root)
    throw new Error(
      "CEK context successor witness does not match frozen successor work root",
    );
  const binding = SDK.deriveCekContextBinding({
    prepared,
    transactionId: base.resolution.pre_state.transaction_id,
    sourceKind: base.resolution.pre_state.source_kind,
    workWitnessCbor: witness.work_witness_cbor,
    auxiliary,
  });
  const context = fields(binding.context, 0, 25);
  const stage = Number(integer(context[0]!));
  const nextWork = cborArray(successorWorkWitnessCbor, 9);
  let successorData: Data;
  let nextContext: Data[] | undefined;
  if (stage === 13) {
    successorData = new Constr(1, [nextWork[5]!, nextWork[7]!, nextWork[8]!]);
  } else {
    const nextBinding = SDK.deriveCekContextBinding({
      prepared,
      transactionId: base.resolution.pre_state.transaction_id,
      sourceKind: base.resolution.pre_state.source_kind,
      workWitnessCbor: successorWorkWitnessCbor,
      auxiliary,
    });
    nextContext = fields(nextBinding.context, 0, 25);
    successorData = record([nextBinding.context]);
  }
  const verified = record([binding.staged, successorData]);
  if (
    (stage === 0 && context[9] !== "") ||
    (stage === 9 && auxiliary instanceof Constr && auxiliary.index === 18)
  ) {
    const item = deriveCekContextItemReturnPlan({
      staged: binding.staged,
      auxiliary,
      verifiedContext: verified,
    });
    return {
      route: [
        "control",
        "itemBind",
        ...item.route,
        "settle",
      ] as CekContextStageKey[],
      states: [binding.bound, binding.staged, item.pending, ...item.states],
      item,
    };
  }
  const route: CekContextStageKey[] = [];
  const states: Data[] = [binding.bound, binding.staged];
  const finish = (key: CekContextStageKey) => {
    route.push(key);
    states.push(verified);
  };
  switch (stage) {
    case 0:
      if (context[9] !== "")
        throw new Error("CEK item continuation requires its shared item plan");
      finish("redeemerBegin");
      break;
    case 1:
      finish("reference");
      break;
    case 2:
      finish("spend");
      break;
    case 3:
      finish("output");
      break;
    case 4:
      finish("signer");
      break;
    case 5: {
      if (nextContext === undefined)
        throw new Error("Missing observer successor context");
      const opening =
        integer(nextContext[0]!) === 6n
          ? new Constr(integer(context[16]!) === 0n ? 0 : 1, [])
          : new Constr(2, [nextContext[16]!, nextContext[18]!]);
      route.push("observerAuthenticate");
      states.push(record([binding.staged, opening]));
      finish("observerFold");
      break;
    }
    case 6:
      finish("mintInit");
      break;
    case 8:
      finish("mintItem");
      break;
    case 9: {
      const raw = fields(auxiliary, 17, 12);
      const kind = Number(integer(raw[7]!));
      if (kind < 0 || kind > 3)
        throw new Error("Invalid redeemer purpose kind");
      const midgard = integer(context[1]!) === 128n;
      const scriptHash = raw[9];
      const subject = raw[10];
      const commitment = raw[4];
      if (
        typeof scriptHash !== "string" ||
        typeof subject !== "string" ||
        typeof commitment !== "string"
      )
        throw new Error("Expected exact redeemer selection bytes");
      const purpose: MidgardScriptPurpose =
        kind === 0
          ? { kind: "spend", scriptHash, outRefHex: subject }
          : kind === 1
            ? { kind: "mint", scriptHash, policyId: scriptHash }
            : kind === 2
              ? { kind: "observe", scriptHash }
              : { kind: "receive", scriptHash };
      const omitted = !midgard && kind === 3;
      const purposeData = omitted
        ? undefined
        : midgard
          ? midgardScriptPurposeData(purpose)
          : cardanoScriptPurposeData(purpose);
      const summary =
        purposeData === undefined
          ? undefined
          : summarizeMidgardCekLucidData(purposeData as Data);
      const optionalPurpose =
        summary === undefined
          ? new Constr(1, [])
          : new Constr(0, [
              record([
                Buffer.from(summary.root).toString("hex"),
                summary.cborLength,
                summary.memory,
              ]),
            ]);
      const index = Number(integer(raw[1]!));
      const initial = initialMidgardRedeemerItemProofControl({
        mode: omitted ? 0 : 1,
        itemIndex: index,
        itemCount: Number(integer(raw[2]!)),
        totalLength: Number(integer(raw[3]!)),
        itemCommitment: Buffer.from(commitment, "hex"),
        expectedPurposeTag: [0, 1, 3, 6][kind],
        expectedPointerIndex: Number(integer(raw[8]!)),
      });
      const leaf = hashMidgardRedeemerItemLeaf({
        redeemerIndex: index,
        itemCommitment: Buffer.from(commitment, "hex"),
      }).toString("hex");
      const selection = record([
        binding.staged,
        raw[0]!,
        raw[1]!,
        raw[2]!,
        raw[3]!,
        commitment,
        leaf,
        raw[7]!,
        raw[8]!,
        scriptHash,
        subject,
      ]);
      route.push("redeemerSelectAuthenticate");
      states.push(selection);
      route.push("redeemerSelectInitialize");
      states.push(record([selection, optionalPurpose]));
      route.push("redeemerSelectHash");
      states.push(
        record([
          selection,
          optionalPurpose,
          hashMidgardRedeemerItemProofControl(initial).toString("hex"),
        ]),
      );
      finish("redeemerSelectFinish");
      break;
    }
    case 10: {
      const purpose = integer(context[4]!);
      const midgard = integer(context[1]!) === 128n;
      const raw = fields(
        auxiliary,
        !midgard && purpose === 0n ? 20 : 19,
        !midgard && purpose === 0n ? 5 : 1,
      );
      const current = fields(raw[0]!, 0, 6);
      route.push("finalizeAuthenticate");
      states.push(record([binding.staged, current[1]!, current[5]!]));
      const key = midgard
        ? "finalizeMidgard"
        : ([
            "finalizeSpend",
            "finalizeMint",
            "finalizeWithdraw",
            "finalizeObserve",
          ] as const);
      if (typeof key === "string") finish(key);
      else {
        const selected = key[Number(purpose)];
        if (selected === undefined)
          throw new Error("Invalid context purpose kind");
        finish(selected);
      }
      break;
    }
    case 11:
      finish("assemble");
      break;
    case 12:
      finish("txInfo");
      break;
    case 13:
      finish("seed");
      break;
    default:
      throw new Error(
        `Context stage ${stage} requires the shared redeemer plan`,
      );
  }
  return {
    route: ["control", ...route, "settle"] as CekContextStageKey[],
    states,
  };
};

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

/** Shared-machine handoff and return states for an exact canonical item advance. */
export const deriveCekContextItemReturnPlan = ({
  staged,
  auxiliary,
  verifiedContext,
}: {
  readonly staged: Data;
  readonly auxiliary: Data;
  readonly verifiedContext: Data;
}) => {
  const stagedFields = fields(staged, 0, 4);
  const context = fields(stagedFields[1]!, 0, 25);
  const raw = fields(auxiliary, 18, 3);
  const control = decodeRedeemerItemControlData(raw[1]!);
  const next = advanceMidgardRedeemerItemProof({
    control,
    witness: decodeRedeemerItemWitnessData(raw[2]!),
  });
  if (next === null)
    throw new Error("CEK context item witness does not advance");
  const claimedNext = redeemerItemControlData(next) as Data;
  const pending = record([
    staged,
    raw[1]!,
    SDK.hashCekCoreWitness(raw[2]!),
    claimedNext,
  ]);
  const verified = record([staged, claimedNext]);
  const route: CekContextStageKey[] = ["itemReturn"];
  const states: Data[] = [];
  const terminal = next.stage === MidgardRedeemerItemProofStages.Terminal;
  const selection = integer(context[0]!) === 0n;
  if (!selection && integer(context[0]!) !== 9n)
    throw new Error("CEK context item has invalid context stage");
  if (!terminal) {
    const key = selection ? "itemSelectionHash" : "itemDataHash";
    route.push(key);
    states.push(verified);
    states.push(
      record([
        verified,
        hashMidgardRedeemerItemProofControl(next).toString("hex"),
      ]),
    );
    route.push(selection ? "itemSelectionContinue" : "itemDataContinue");
  } else if (selection) {
    route.push("itemSelectionFinish");
    states.push(verified);
  } else if (next.mode === MidgardRedeemerItemProofModes.Descriptor) {
    route.push("itemDataFinishDescriptor");
    states.push(verified);
  } else {
    const summary = finalizeMidgardRedeemerItemProof(next);
    if (summary === null)
      throw new Error("CEK terminal data item has no canonical summary");
    route.push("itemFinalize");
    states.push(verified);
    route.push("itemDataFinishValue");
    states.push(
      record([
        verified,
        record([
          Buffer.from(summary.root).toString("hex"),
          summary.cborLength,
          summary.memory,
        ]),
      ]),
    );
  }
  states.push(verifiedContext);
  return { pending, claimedNext, witness: raw[2]!, verified, route, states };
};
