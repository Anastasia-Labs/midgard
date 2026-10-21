/** The hops of a thread of ours through the one-step resolution of a CEK core
 * step, past the game: from the boundary to the CEK resolver
 * (PrepareResolution), from it to the CEK core step binder (PrepareSelected),
 * from the binder to the arm its step selects (VerifyCoreStep), from the arm
 * to the settle (VerifyArm), and from the settle to the award (Settle). Each
 * hop spends the thread alone, which pays its fee; its successor keeps the
 * thread's single token and carries the state the next holder checks as an
 * inline datum. */
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type ValidationTraceArmHolder,
  type ValidationTraceOneStepHolder,
} from "./one-step-holders.js";
import {
  CEK_CORE_SEMANTIC_RESOLVER_INDEX,
  hashSerialisedData,
  SINGLE_HOP_CORE_ARMS,
  type ValidationTraceOneStepPlan,
} from "./one-step-plan.js";

/** The holders a hop spends, past the game. */
export type ValidationTraceHopHolder = Exclude<
  ValidationTraceOneStepHolder,
  "award"
>;

/** A hop: where its successor goes, what it carries and how it is redeemed. */
export type ValidationTraceHop = Readonly<{
  next: ValidationTraceOneStepHolder;
  /** The successor's inline datum. */
  datum: string;
  /** The redeemer, for the thread at `inputIndex` and its successor at
   * `outputIndex`. */
  redeemer: (inputIndex: bigint, outputIndex: bigint) => string;
  /** Whether the holder lets the prover alone move the thread. */
  proverOnly: boolean;
}>;

const integer = (value: bigint | number) => Data.to(BigInt(value));
const bytes = (hex: string) => Data.to(hex);
/** A constructor's CBOR from its fields' CBOR. */
const constr = (index: number, fields: readonly string[]) =>
  `d8${(0x79 + index).toString(16)}${
    fields.length === 0 ? "80" : `9f${fields.join("")}ff`
  }`;
/** `ct.StepDatum { fraud_prover, data: Some(state) }`. */
const stepDatum = (prover: string, stateCbor: string) =>
  constr(0, [bytes(prover), constr(0, [stateCbor])]);
/** `ct.Continue(action)`. */
const continued = (actionCbor: string) => constr(1, [actionCbor]);

/** The arm holder of the plan's core group. */
export const validationTraceArmOf = (
  plan: ValidationTraceOneStepPlan,
): ValidationTraceArmHolder => SINGLE_HOP_CORE_ARMS[plan.core.group];

const datumOf = (utxo: UTxO): string => {
  if (typeof utxo.datum !== "string")
    throw new Error("validation-trace thread has no inline datum");
  return utxo.datum;
};

/**
 * The hop of the thread `utxo` of prover `prover`, held by `holder`, in the
 * one-step resolution `plan`.
 */
export const validationTraceHop = ({
  plan,
  holder,
  utxo,
  prover,
}: {
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: ValidationTraceHopHolder;
  readonly utxo: UTxO;
  readonly prover: string;
}): ValidationTraceHop => {
  const datum = datumOf(utxo);
  const { core } = plan;
  switch (holder) {
    case "boundary": {
      const thread = Data.from(datum, SDK.CaseThreadDatum).data;
      if (thread === null)
        throw new Error("validation-trace boundary thread has no state");
      const preState = SDK.validationMachineStateDataFromCore(plan.preState);
      return {
        next: "prepare",
        datum: Data.to(
          {
            fraud_prover: prover,
            data: {
              version: 1n,
              pre_state: preState,
              operator_successor_hash: thread.defender_high_hash,
              challenger_successor_hash: thread.challenger_high_hash,
            },
          },
          SDK.ValidationResolutionDatum,
        ),
        redeemer: (inputIndex, outputIndex) =>
          Data.to(
            {
              Continue: [
                {
                  PrepareResolution: {
                    input_index: inputIndex,
                    output_index: outputIndex,
                    resolver_index: BigInt(plan.resolverIndex),
                    evidence: { pre_state: preState },
                  },
                },
              ],
            },
            SDK.ValidationBoundarySpendRedeemer,
          ),
        proverOnly: true,
      };
    }
    case "prepare":
      return {
        next: "core",
        datum: stepDatum(
          prover,
          constr(0, [
            integer(1),
            plutusConstrFieldCbor(datum, [1, 0]),
            bytes(plan.evidenceHash),
          ]),
        ),
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [
              integer(inputIndex),
              integer(outputIndex),
              integer(CEK_CORE_SEMANTIC_RESOLVER_INDEX),
              plan.transitionCbor,
              plan.auxiliaryCbor,
            ]),
          ),
        proverOnly: true,
      };
    case "core":
      return {
        next: validationTraceArmOf(plan),
        datum: stepDatum(
          prover,
          constr(0, [
            plutusConstrFieldCbor(datum, [1, 0]),
            core.preCbor,
            core.postCbor,
            bytes(hashSerialisedData(core.armWitnessCbor)),
            integer(core.arm),
            integer(core.group),
            integer(0),
            constr(0, []),
          ]),
        ),
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [
              integer(inputIndex),
              integer(outputIndex),
              plan.transitionCbor,
              core.stepCbor,
            ]),
          ),
        proverOnly: true,
      };
    case "settle":
      return {
        next: "award",
        datum: stepDatum(prover, constr(0, [integer(1)])),
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [
              integer(inputIndex),
              integer(outputIndex),
              plan.transitionCbor,
              core.armWitnessCbor,
            ]),
          ),
        proverOnly: false,
      };
    case "compute":
    case "machine":
    case "mapConversion":
    case "caseConstant":
      // An arm whose step settles after one hop carries the bound on with
      // its progress one higher.
      return {
        next: "settle",
        datum: replacePlutusConstrFieldCbor(datum, [1, 0, 6], integer(1)),
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [
              integer(inputIndex),
              integer(outputIndex),
              core.armWitnessCbor,
            ]),
          ),
        proverOnly: true,
      };
  }
};

/**
 * The hop `hop` of `thread`, carrying its token `unit` (reading the holder's
 * script from `reference`) to `address`, which keeps the thread's tokens and its lovelace less `fee`,
 * before completion; signed by `prover` when the holder needs it.
 */
export const buildValidationTraceHop = ({
  lucid,
  thread,
  unit,
  reference,
  hop,
  address,
  prover,
  fee,
  validFrom,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly thread: UTxO;
  readonly unit: string;
  readonly reference: UTxO;
  readonly hop: ValidationTraceHop;
  readonly address: string;
  readonly prover: string;
  readonly fee: bigint;
  readonly validFrom: number;
  readonly validTo: number;
}): TxBuilder => {
  const tx = lucid
    .newTx()
    .collectFrom([thread], (ctx: RedeemerContext) =>
      hop.redeemer(
        SDK.requireInputIndex(ctx, thread, "thread"),
        SDK.requireUniqueOutputIndex(
          ctx.outputs,
          outputWithDatumAndUnitPredicate({ address, datum: hop.datum, unit }),
          "thread successor",
        ),
      ),
    )
    .readFrom([reference])
    .pay.ToContract(
      address,
      { kind: "inline", value: hop.datum },
      { ...thread.assets, lovelace: (thread.assets.lovelace ?? 0n) - fee },
    )
    .validFrom(validFrom)
    .validTo(validTo);
  return hop.proverOnly ? tx.addSignerKey(prover) : tx;
};
