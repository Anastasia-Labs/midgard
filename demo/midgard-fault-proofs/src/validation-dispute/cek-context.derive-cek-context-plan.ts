import {
  hashMidgardRedeemerItemProofControl,
  initialMidgardRedeemerItemProofControl,
} from "@al-ft/midgard-core";
import { hashMidgardRedeemerItemLeaf } from "@al-ft/midgard-core/script-proof";
import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { summarizeMidgardCekLucidData } from "@al-ft/midgard-validation";
import {
  cardanoScriptPurposeData,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "@al-ft/midgard-validation/midgard-redeemers";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  cborArray,
  type CekContextStageKey,
  type CekContextSuccessorEvidence,
  deriveCekContextItemReturnPlan,
  fields,
  integer,
  record,
} from "./cek-context.derive-cek-context-item-return-plan.js";

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
      // A native execution leaf at the frontier is skipped straight to settle.
      if (auxiliary instanceof Constr && auxiliary.index === 40) {
        fields(auxiliary, 40, 4);
        finish("redeemerSelectAuthenticate");
        break;
      }
      const raw = fields(auxiliary, 17, 12);
      const kind = Number(integer(raw[4]!));
      if (kind < 0 || kind > 3)
        throw new Error("Invalid redeemer purpose kind");
      const midgard = integer(context[1]!) === 128n;
      const scriptHash = raw[6];
      const subject = raw[7];
      const commitment = raw[3];
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
      // The witness no longer carries the redeemer count or the frontier
      // index: select-authenticate derives both, and so does the plan.
      const itemCount = integer(fields(binding.native, 0, 16)[8]!);
      const purposeFrontier = integer(fields(raw[0]!, 0, 7)[6]!) - 1n;
      const index = Number(integer(raw[1]!));
      const initial = initialMidgardRedeemerItemProofControl({
        mode: omitted ? 0 : 1,
        itemIndex: index,
        itemCount: Number(itemCount),
        totalLength: Number(integer(raw[2]!)),
        itemCommitment: Buffer.from(commitment, "hex"),
        expectedPurposeTag: [0, 1, 3, 6][kind],
        expectedPointerIndex: Number(integer(raw[5]!)),
      });
      const leaf = hashMidgardRedeemerItemLeaf({
        redeemerIndex: index,
        itemCommitment: Buffer.from(commitment, "hex"),
      }).toString("hex");
      const selection = record([
        binding.staged,
        raw[0]!,
        raw[1]!,
        itemCount,
        raw[2]!,
        commitment,
        leaf,
        purposeFrontier,
        raw[4]!,
        raw[5]!,
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
      const current = fields(raw[0]!, 0, 7);
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
