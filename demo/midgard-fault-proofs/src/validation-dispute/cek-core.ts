import { hashMidgardCekValueNode } from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  hashMidgardCekDirectArguments,
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
} from "@al-ft/midgard-validation/cek-builtin";
import {
  decodeMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
} from "@al-ft/midgard-validation/cek-constant";
import { commitMidgardCekDataTree } from "@al-ft/midgard-validation/cek-data-tree";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";

const fields = (value: Data, tag: number, count: number): Data[] => {
  if (
    !(value instanceof Constr) ||
    value.index !== tag ||
    value.fields.length !== count
  )
    throw new Error(`Expected exact CEK constructor ${tag}/${count}`);
  return value.fields;
};
const integer = (value: Data): bigint => {
  if (typeof value !== "bigint") throw new Error("Expected CEK integer");
  return value;
};
const bytes = (value: Data): Buffer => {
  if (typeof value !== "string" || !/^(?:[0-9a-f]{2})*$/u.test(value))
    throw new Error("Expected canonical CEK bytes");
  return Buffer.from(value, "hex");
};
const valueWitness = (raw: Data): MidgardCekDirectValueWitness => {
  if (!(raw instanceof Constr))
    throw new Error("Expected CEK value constructor");
  switch (raw.index) {
    case 0: {
      const [constant] = fields(raw, 0, 1);
      const [type, payload] = fields(constant!, 0, 2);
      return {
        kind: "constant",
        witness: { typeCbor: bytes(type!), payloadCbor: bytes(payload!) },
      };
    }
    case 1: {
      const [type, payload, memory] = fields(raw, 1, 3);
      const [root, length, payloadMemory] = fields(payload!, 0, 3);
      return {
        kind: "semanticConstant",
        witness: {
          typeCbor: bytes(type!),
          payload: {
            root: bytes(root!),
            cborLength: integer(length!),
            memory: integer(payloadMemory!),
          },
          memory: integer(memory!),
        },
      };
    }
    case 2:
      return { kind: "opaque", root: bytes(fields(raw, 2, 1)[0]!) };
    case 3:
      return {
        kind: "blsMillerLoop",
        expressionRoot: bytes(fields(raw, 3, 1)[0]!),
      };
    default:
      throw new Error("Unknown CEK direct value constructor");
  }
};
const compactSemanticValue = (raw: Data): Data => {
  const value = valueWitness(raw);
  if (value.kind !== "constant") return raw;
  const decoded = decodeMidgardCekConstantWitness(value.witness);
  const payload = commitMidgardCekDataTree(decoded.payload);
  return new Constr(1, [
    Buffer.from(value.witness.typeCbor).toString("hex"),
    new Constr(0, [
      Buffer.from(payload.root).toString("hex"),
      payload.cborLength,
      payload.memory,
    ]),
    midgardCekConstantMemorySize(decoded.type, decoded.payload),
  ]);
};
const success = (witness: Constr<Data>) => {
  const raw = fields(witness, witness.index, witness.index === 30 ? 3 : 4);
  if (!Array.isArray(raw[1]))
    throw new Error("Expected CEK builtin argument list");
  return {
    tag: integer(raw[0]!),
    arguments: raw[1].map(valueWitness),
    result: valueWitness(raw[2]!),
  };
};
export type CekCoreStageKey = keyof SDK.CekCoreStages;
export const deriveCekCorePlan = (prepared: Data, step: Data) => {
  const [preData, postData, rawWitness] = fields(step, 0, 3);
  const pre = Data.from(Data.to(preData!), SDK.CekCoreMachineState);
  const post = Data.from(Data.to(postData!), SDK.CekCoreMachineState);
  if (!(rawWitness instanceof Constr))
    throw new Error("Expected CEK core witness");
  const witness: Constr<Data> = rawWitness;
  const arm = witness.index;
  let group: number;
  let route: CekCoreStageKey[];
  if (pre.mode === 0n && ((arm >= 0 && arm <= 10) || arm === 40)) {
    group = 0;
    route = ["compute"];
  } else if (
    (pre.mode === 2n && (arm === 11 || arm === 12)) ||
    (pre.mode === 1n && arm >= 13 && arm <= 27) ||
    (pre.mode === 6n && arm === 28) ||
    (pre.mode === 7n && arm === 29)
  ) {
    group = 1;
    route = ["machine"];
  } else if (pre.mode === 8n && arm >= 33 && arm <= 35) {
    group = 2;
    route = ["mapConversion"];
  } else {
    if (pre.mode !== 3n)
      throw new Error("CEK arm does not match its machine mode");
    switch (arm) {
      case 30: {
        const { tag } = success(witness);
        const structured =
          tag >= 29n && tag <= 69n && tag !== 52n && tag !== 53n;
        group = structured ? 4 : 3;
        route = structured
          ? [
              "directStructuredRoots",
              "directStructuredBudget",
              "directStructured",
            ]
          : ["directScalarRoots", "directScalarBudget", "directScalar"];
        break;
      }
      case 31: {
        group = 5;
        const { tag } = success(witness);
        if (tag < 29n || tag > 51n || tag === 38n || tag === 43n)
          throw new Error("Unsupported semantic builtin tag");
        const leaf: CekCoreStageKey =
          tag === 29n || tag === 30n
            ? "semanticPair"
            : tag === 31n || tag === 32n || tag === 35n
              ? "semanticListConstruct"
              : tag === 33n || tag === 34n
                ? "semanticListSelect"
                : tag === 36n
                  ? "semanticChoose"
                  : tag === 37n || tag === 42n
                    ? "semanticDataConstruct"
                    : tag >= 47n && tag <= 51n
                      ? "semanticDataMisc"
                      : "semanticDataScalar";
        route = ["semanticRoots", "semanticResult", "semanticBudget", leaf];
        break;
      }
      case 32:
        group = 6;
        route = ["mapStartRoots", "mapStartBudget", "mapStartNodes"];
        break;
      case 36:
        group = 7;
        route = ["semanticFailureRoots", "semanticFailureMaterial"];
        break;
      case 37:
        group = 8;
        route = ["blsBudget", "blsRoots", "blsFinal"];
        break;
      case 38:
        group = 9;
        route = ["failureKnown", "failureBudget"];
        break;
      case 39:
        group = 10;
        route = ["typeFailureRoots", "typeFailureKinds"];
        break;
      default:
        throw new Error("Unknown CEK builtin arm");
    }
  }
  let bound: SDK.CekCoreBound = {
    prepared,
    pre,
    post,
    witness_hash: SDK.hashCekCoreWitness(witness),
    arm: BigInt(arm),
    group: BigInt(group),
    progress: 0n,
    facts: new Constr(0, []),
  };
  const bounds = [bound];
  for (const key of route) {
    let facts = bound.facts;
    if (
      key === "directScalarRoots" ||
      key === "directStructuredRoots" ||
      key === "semanticRoots"
    ) {
      const item = success(witness);
      const args = hashMidgardCekDirectArguments(item.arguments);
      facts = Data.from(
        Data.to(
          {
            arguments_root: Buffer.from(args.root).toString("hex"),
            arguments_count: args.count,
            result_root: Buffer.from(
              hashMidgardCekDirectValueWitness(item.result),
            ).toString("hex"),
            builtin_root: Buffer.from(
              hashMidgardCekValueNode({
                kind: "builtin",
                tag: item.tag,
                forcesRemaining: 0n,
                argumentsCount: args.count,
                argumentsRoot: args.root,
              }),
            ).toString("hex"),
          },
          SDK.CekCoreBuiltinRoots,
        ),
      );
      if (key === "semanticRoots") {
        const raw = fields(witness, 31, 4);
        if (!Array.isArray(raw[1]))
          throw new Error("Expected semantic argument list");
        const roots = fields(facts, 0, 4);
        facts = new Constr(0, [
          roots[0]!,
          roots[1]!,
          roots[3]!,
          raw[1].map(compactSemanticValue),
        ]);
      }
    } else if (key === "semanticResult") {
      const prior = fields(facts, 0, 4);
      const raw = fields(witness, 31, 4);
      const result = compactSemanticValue(raw[2]!);
      const resultRoot = Buffer.from(
        hashMidgardCekDirectValueWitness(valueWitness(result)),
      ).toString("hex");
      facts = new Constr(0, [
        new Constr(0, [prior[0]!, prior[1]!, resultRoot, prior[2]!]),
        prior[3]!,
        result,
      ]);
    } else if (key === "mapStartRoots") {
      const raw = fields(witness, 32, 4);
      if (!Array.isArray(raw[1])) throw new Error("Expected map arguments");
      facts = new Constr(0, [
        raw[1].map(compactSemanticValue),
        compactSemanticValue(raw[2]!),
        new Constr(0, []),
      ]);
    } else if (key === "mapStartBudget") {
      const prior = fields(facts, 0, 3);
      const item = success(witness);
      facts = new Constr(0, [
        prior[0]!,
        prior[1]!,
        Data.from(
          Data.to(
            midgardCekDirectBuiltinBudget(item.tag, item.arguments),
            SDK.CekCoreBudget,
          ),
        ),
      ]);
    }

    bound = { ...bound, progress: bound.progress + 1n, facts };
    bounds.push(bound);
  }
  return { route, bounds, witness };
};

export const submitCekCoreChain = async ({
  lucid,
  signer,
  contracts,
  binder,
  binderReference,
  stageReferences,
  threadUtxo: initialThread,
  threadUnit,
  prepared,
  transition,
  step,
  awardDatum,
  getValidityRange,
  maxTransactions,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly contracts: SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];
  readonly binder: SDK.SpendingValidator;
  readonly binderReference?: UTxO;
  readonly stageReferences: Readonly<Partial<Record<CekCoreStageKey, UTxO>>>;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly prepared: Data;
  readonly transition: Data;
  readonly step: Data;
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
    throw new Error("CEK core transaction limit must be positive");
  const plan = deriveCekCorePlan(prepared, step);
  const keys = ["binder", ...plan.route, "settle"] as const;
  const stateDatum = (bound: SDK.CekCoreBound) =>
    Data.to(
      { fraud_prover: signer.paymentKeyHash, data: bound },
      SDK.CekCoreDatum,
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
    const contract = key === "binder" ? binder : contracts.cekCoreStages[key];
    const expectedDatum =
      index === 0 ? initialDatum : stateDatum(plan.bounds[index - 1]!);
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
    const reference = key === "binder" ? binderReference : stageReferences[key];
    if (reference === undefined)
      throw new Error(`Missing CEK core ${key} reference script`);
    const nextKey = keys[index + 1];
    if (key !== "settle" && (nextKey === undefined || nextKey === "binder"))
      throw new Error("Missing CEK core successor");
    const nextContract =
      nextKey === undefined || nextKey === "binder"
        ? contracts.award
        : contracts.cekCoreStages[nextKey];
    const nextDatum =
      key === "settle" ? awardDatum : stateDatum(plan.bounds[index]!);
    const currentInput = threadUtxo;
    let inputIndex = -1;
    let outputIndex = -1;
    const redeemer = ((ctx) => {
      inputIndex = Number(
        SDK.requireInputIndex(ctx, currentInput, `CEK core ${key}`),
      );
      outputIndex = Number(
        SDK.requireUniqueOutputIndex(
          ctx.outputs,
          computationThreadOutputPredicate({
            address: nextContract.spendingScriptAddress,
            datum: nextDatum,
            unit: threadUnit,
          }),
          `CEK core ${key}`,
        ),
      );
      const evidence =
        key === "binder"
          ? { kind: "binder" as const, transition, step }
          : key === "settle"
            ? { kind: "settle" as const, transition, witness: plan.witness }
            : { kind: "arm" as const, witness: plan.witness };
      return SDK.encodeCekCoreRedeemer(
        BigInt(inputIndex),
        BigInt(outputIndex),
        evidence,
      );
    }) satisfies BuildTxWithRedeemer;
    signer.selectWallet(lucid);
    const { validFrom, validTo } = getValidityRange();
    const unsigned = await lucid
      .newTx()
      .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
      .collectFrom([currentInput], redeemer)
      .readFrom([reference])
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
        throw new Error(`CEK core ${key} transaction failed: ${String(error)}`);
      });
    const signed = await unsigned.sign.withWallet().complete();
    const completeSignedBytes = signed.toCBOR().length / 2;
    if (completeSignedBytes > 16384)
      throw new Error(
        `CEK core ${key} exceeds maximum signed transaction bytes`,
      );
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    const nextThreadOutRef = `${txHash}#${outputIndex}`;
    threadUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: { txHash, outputIndex },
      label: `CEK core ${key} continuation`,
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
      "Live CEK core checkpoint does not match retained evidence",
    );
  return { transactions, threadUtxo, completed: true };
};
