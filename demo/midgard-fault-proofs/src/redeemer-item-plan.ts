import {
  advanceMidgardRedeemerItemProof,
  computeHash32,
  encodeCbor,
  encodeMidgardCekDataTraverseControl,
  hashMidgardRedeemerItemProofControl,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofWitness,
  nextMidgardRedeemerItemProofSpan,
  readMidgardRedeemerItemProofSource,
} from "@al-ft/midgard-core";
import type {
  SharedRedeemerItemStages,
  SpendingValidator,
} from "@al-ft/midgard-sdk";
import { redeemerItemControlData } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
  deriveRedeemerItemStepPlan,
} from "./redeemer-item-data.js";

const record = (value: Data, length: number): Data[] => {
  if (
    !(value instanceof Constr) ||
    value.index !== 0 ||
    value.fields.length !== length
  )
    throw new Error("shared item record is malformed");
  return value.fields;
};
const rawBytes = (value: Data): Buffer => Buffer.from(Data.to(value), "hex");
const rawHash = (value: Data): string =>
  computeHash32(rawBytes(value)).toString("hex");
const domain = (name: string): Buffer =>
  Buffer.from(`MidgardScriptSources${name}V1`, "ascii");
const domainDataHash = (name: string, value: Data): string =>
  computeHash32(Buffer.concat([domain(name), rawBytes(value)])).toString("hex");
const domainCborHash = (
  name: string,
  value: Parameters<typeof encodeCbor>[0],
): string =>
  computeHash32(Buffer.concat([domain(name), encodeCbor(value)])).toString(
    "hex",
  );
const b = (value: string): Buffer => Buffer.from(value, "hex");
const some = (value: Data): Data => new Constr(0, [value]);
const none = new Constr<Data>(1, []);
const c = (fields: Data[]): Constr<Data> => new Constr(0, fields);

export const redeemerItemExecutor = (
  control: MidgardRedeemerItemProofControl,
  witness: MidgardRedeemerItemProofWitness,
): { readonly family: number; readonly index: number } => {
  const action = witness.action;
  if (action.kind === "openHeader") return { family: 2, index: 2 };
  if (action.kind === "openTail") return { family: 3, index: 3 };
  if (action.kind === "finishData") return { family: 8, index: 16 };
  const inner = action.action;
  if (inner === null) {
    const stage = control.traversal?.stage;
    if (stage === undefined || stage < 1 || stage > 5)
      throw new Error("item NoAction is outside its stage domain");
    return { family: 7, index: stage + 10 };
  }
  switch (inner.kind) {
    case "foldMap":
      return { family: 0, index: 0 };
    case "finalizeFrame":
      return { family: 1, index: 1 };
    case "headScalar":
      return { family: 4, index: 4 };
    case "headSequence":
      return { family: 4, index: 5 };
    case "headMap":
      return { family: 4, index: 6 };
    case "headLargeConstructor":
      return { family: 4, index: 7 };
    case "attachScalar": {
      const stage = control.traversal?.stage;
      if (stage !== 1 && stage !== 2)
        throw new Error("item attachment is outside its stage domain");
      return { family: 5, index: stage + 7 };
    }
    case "foldList":
      return { family: 6, index: 10 };
  }
};

export type RedeemerItemStageKey =
  | "entry"
  | "normalize_current_traversal"
  | "normalize_current_outer"
  | "normalize_next_traversal"
  | "normalize_next_outer"
  | "authenticate_source"
  | "execute"
  | "settle";

export type RedeemerItemPlannedStage = {
  readonly key: RedeemerItemStageKey;
  readonly validator: SpendingValidator;
  readonly inputState: Data;
  readonly outputState: Data;
  readonly spendRedeemer: (inputIndex: bigint, outputIndex: bigint) => Data;
};

/** Exact output construction for the shared Pending → Verified CEK handoff. */
export const deriveCekRedeemerItemPlan = ({
  pending,
  witness,
  stages,
  deploymentId,
}: {
  readonly pending: Data;
  readonly witness: Data;
  readonly stages: SharedRedeemerItemStages;
  readonly deploymentId: string;
}): readonly RedeemerItemPlannedStage[] => {
  const pendingFields = record(pending, 4);
  const current = pendingFields[1]!,
    claimedNext = pendingFields[3]!;
  const witnessHash = rawHash(witness);
  if (pendingFields[2] !== witnessHash)
    throw new Error(
      "shared item witness differs from authenticated CEK handoff",
    );
  const plan = deriveRedeemerItemStepPlan({ current, witness, claimedNext });
  return deriveItemPlan({
    carrier: 1n,
    base: pending,
    current,
    witness,
    claimedNext,
    plan,
    stages,
    deploymentId,
    transition: null,
  });
};

/** The canonical item result determines Advanced versus Invalid before constructing outputs. */
export const deriveScriptSourcesRedeemerItemPlan = ({
  preparedResolution,
  transition,
  auxiliary,
  stages,
  deploymentId,
}: {
  readonly preparedResolution: Data;
  readonly transition: Data;
  readonly auxiliary: Data;
  readonly stages: SharedRedeemerItemStages;
  readonly deploymentId: string;
}): readonly RedeemerItemPlannedStage[] => {
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 18 ||
    auxiliary.fields.length !== 3 ||
    Data.to(auxiliary.fields[0]!) !== Data.to(none)
  )
    throw new Error("ScriptSources item auxiliary has the wrong exact shape");
  const current = auxiliary.fields[1]!,
    witness = auxiliary.fields[2]!;
  const control = decodeRedeemerItemControlData(current),
    coreWitness = decodeRedeemerItemWitnessData(witness);
  if (control.mode !== 1)
    throw new Error("ScriptSources item carrier requires mode_data");
  const source = readMidgardRedeemerItemProofSource({
    control,
    witness: coreWitness,
  });
  if (source === null)
    throw new Error("ScriptSources item source evidence does not authenticate");
  const next = advanceMidgardRedeemerItemProof({
    control,
    witness: coreWitness,
  });
  if (
    next === null &&
    !(
      (control.stage === 0 && coreWitness.action.kind === "openHeader") ||
      (control.stage === 1 && coreWitness.action.kind === "openTail")
    )
  )
    throw new Error(
      "ScriptSources item action is not a canonical Advanced or Invalid result",
    );
  const claimedNext =
    next === null
      ? none
      : Data.from(Data.to<unknown>(redeemerItemControlData(next)));
  const plan = {
    control,
    next: next ?? control,
    currentControlHash: hashMidgardRedeemerItemProofControl(control),
    nextControlHash:
      next === null
        ? Buffer.alloc(32)
        : hashMidgardRedeemerItemProofControl(next),
  };
  return deriveItemPlan({
    carrier: 0n,
    base: preparedResolution,
    current,
    witness,
    claimedNext,
    plan,
    stages,
    deploymentId,
    transition,
    invalid: next === null,
  });
};

const deriveItemPlan = ({
  carrier,
  base,
  current,
  witness,
  claimedNext,
  plan,
  stages,
  deploymentId,
  transition,
  invalid = false,
}: {
  readonly carrier: bigint;
  readonly base: Data;
  readonly current: Data;
  readonly witness: Data;
  readonly claimedNext: Data;
  readonly plan: Pick<
    ReturnType<typeof deriveRedeemerItemStepPlan>,
    "control" | "next" | "currentControlHash" | "nextControlHash"
  >;
  readonly stages: SharedRedeemerItemStages;
  readonly deploymentId: string;
  readonly transition: Data | null;
  readonly invalid?: boolean;
}): readonly RedeemerItemPlannedStage[] => {
  const witnessHash = rawHash(witness);
  const coreWitness = decodeRedeemerItemWitnessData(witness);
  const { family, index } = invalid
    ? { family: 9, index: plan.control.stage === 0 ? 17 : 18 }
    : redeemerItemExecutor(plan.control, coreWitness);
  const executor = stages.executors[index];
  if (executor === undefined)
    throw new Error("shared item executor roster is incomplete");
  const auxiliary = new Constr(18, [none, current, witness]);
  const action = record(witness, 3)[0]!;
  const traversalAction =
    coreWitness.action.kind === "traverseData" ? recordAction(action) : null;
  const actionPreimage: Data =
    family === 0 || family === 1
      ? [current, traversalAction!]
      : [current, ...record(witness, 3)];
  const actionHash = domainDataHash("NarrowActionIdentity", actionPreimage);
  const auxiliaryHash = domainDataHash("AuxiliaryIdentity", auxiliary);
  const carrierIdentity = domainDataHash("ResolutionIdentity", base);
  const currentHash = plan.currentControlHash.toString("hex"),
    nextHash = plan.nextControlHash.toString("hex"),
    nextDataHash = carrier === 1n ? rawHash(claimedNext) : "";
  const entryHash = stages.entry.spendingScriptHash,
    traversalHash = stages.traversalNormalizer.spendingScriptHash,
    outerHash = stages.outerNormalizer.spendingScriptHash,
    executorHash = executor.spendingScriptHash,
    settlementHash = stages.settlement.spendingScriptHash;
  const count = BigInt(plan.control.itemIndex),
    total = BigInt(plan.control.itemCount);
  const commitment = domainCborHash("RedeemerEnvelopeCommitment", [
    1n,
    b(deploymentId),
    carrier,
    b(carrierIdentity),
    BigInt(family),
    b(auxiliaryHash),
    b(actionHash),
    b(currentHash),
    b(nextHash),
    count,
    total,
    b(entryHash),
    b(traversalHash),
    b(outerHash),
    b(executorHash),
    b(settlementHash),
    b(nextDataHash),
  ]);
  const envelope = c([
    1n,
    domain("RedeemerEnvelope").toString("hex"),
    deploymentId,
    carrier,
    base,
    carrierIdentity,
    nextDataHash,
    BigInt(family),
    auxiliaryHash,
    actionHash,
    currentHash,
    nextHash,
    count,
    total,
    entryHash,
    traversalHash,
    outerHash,
    executorHash,
    settlementHash,
    commitment,
  ]);
  const checkedTraversal = (
    control: MidgardRedeemerItemProofControl,
  ): string =>
    control.traversal === null
      ? "d87a80"
      : Buffer.concat([
          Buffer.from("d8799f", "hex"),
          encodeMidgardCekDataTraverseControl(control.traversal),
          Buffer.from("ff", "hex"),
        ]).toString("hex");
  const currentTraversal = c([
    new Constr(0, []),
    envelope,
    current,
    claimedNext,
    witnessHash,
    checkedTraversal(plan.control),
  ]);
  const currentChecked = c([envelope, current, claimedNext, witnessHash]);
  const nextTraversal = c([
    new Constr(1, []),
    envelope,
    current,
    claimedNext,
    witnessHash,
    checkedTraversal(plan.next),
  ]);
  const controlsChecked = c([
    envelope,
    current,
    invalid ? current : claimedNext,
    witnessHash,
  ]);
  const provenance = domainCborHash("BaseProvenanceIdentity", [
    carrier,
    b(carrierIdentity),
    b(commitment),
  ]);
  const attestation = c([
    1n,
    domain("RedeemerExecutionAttested").toString("hex"),
    deploymentId,
    provenance,
    entryHash,
    traversalHash,
    outerHash,
    executorHash,
    settlementHash,
    BigInt(family),
    actionHash,
    witnessHash,
    currentHash,
    nextHash,
    nextHash,
    count,
    total,
  ]);
  const output = c([
    deploymentId,
    BigInt(family),
    executorHash,
    settlementHash,
    attestation,
  ]);
  const span = nextMidgardRedeemerItemProofSpan(plan.control);
  let source: Data = none;
  if (span !== null) {
    const first = coreWitness.chunkProof;
    if (first === null)
      throw new Error("shared item source witness is missing");
    const joined = Buffer.concat([
      first.chunk,
      ...(coreWitness.nextChunkProof === null
        ? []
        : [coreWitness.nextChunkProof.chunk]),
    ]);
    const start =
      span.absoluteStart - first.chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
    source = some(joined.subarray(start, start + span.length).toString("hex"));
  }
  const execution =
    traversalAction === null
      ? c([output, current, invalid ? current : claimedNext, action, source])
      : c([
          output,
          optionValue(record(current, 16)[15]!),
          optionValue(record(claimedNext, 16)[15]!),
          traversalAction,
          source,
        ]);
  const verified =
    carrier === 1n ? c([record(base, 4)[0]!, claimedNext]) : c([1n]);
  const stage = (
    key: RedeemerItemStageKey,
    validator: SpendingValidator,
    inputState: Data,
    outputState: Data,
    tag: number,
    args: (input: bigint, output: bigint) => Data[],
  ): RedeemerItemPlannedStage => ({
    key,
    validator,
    inputState,
    outputState,
    spendRedeemer: (input, output) =>
      new Constr(1, [new Constr(tag, args(input, output))]),
  });
  const stagesPlan = [
    stage("entry", stages.entry, base, envelope, 0, (i, o) =>
      carrier === 1n
        ? [
            i,
            o,
            witness,
            plan.currentControlHash.toString("hex"),
            nextHash,
            BigInt(family),
          ]
        : [i, o, transition!, auxiliary, nextHash, BigInt(family)],
    ),
    stage(
      "normalize_current_traversal",
      stages.traversalNormalizer,
      envelope,
      currentTraversal,
      0,
      (i, o) => [i, o, auxiliary, claimedNext],
    ),
    stage(
      "normalize_current_outer",
      stages.outerNormalizer,
      currentTraversal,
      invalid ? controlsChecked : currentChecked,
      0,
      (i, o) => [i, o],
    ),
    stage(
      "normalize_next_traversal",
      stages.traversalNormalizer,
      currentChecked,
      nextTraversal,
      1,
      (i, o) => [i, o],
    ),
    stage(
      "normalize_next_outer",
      stages.outerNormalizer,
      nextTraversal,
      controlsChecked,
      0,
      (i, o) => [i, o],
    ),
    stage(
      "authenticate_source",
      stages.sourceAuthenticator,
      controlsChecked,
      execution,
      0,
      (i, o) => [i, o, witness],
    ),
    stage("execute", executor, execution, attestation, 0, (i, o) => [i, o]),
    stage("settle", stages.settlement, attestation, verified, 0, (i, o) => [
      i,
      o,
      envelope,
    ]),
  ];
  return invalid
    ? stagesPlan.filter(
        (stage) =>
          stage.key !== "normalize_next_traversal" &&
          stage.key !== "normalize_next_outer",
      )
    : stagesPlan;
};

const recordAction = (value: Data): Data => {
  if (
    !(value instanceof Constr) ||
    value.index !== 2 ||
    value.fields.length !== 1
  )
    throw new Error("shared item traversal action is malformed");
  return value.fields[0]!;
};
const optionValue = (value: Data): Data => record(value, 1)[0]!;
