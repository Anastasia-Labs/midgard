/** The hops of a thread of ours through the one-step resolution of a CEK core
 * step, past the game: from the boundary to the CEK resolver
 * (PrepareResolution), from it to the CEK core step binder (PrepareSelected),
 * from the binder to the arm its step selects (VerifyCoreStep), from the arm
 * to the settle (VerifyArm), and from the settle to the award (Settle). A CEK
 * script selection goes from the CEK resolver to the selection resolver
 * (PrepareSelected), and from it to the award with the selection's two
 * yields (VerifyExecutionSelection). A CEK context step goes from the CEK
 * resolver to the context step resolver (PrepareSelected), and on through
 * the context's holders to the award (one-step-context-hops.ts). A
 * canonical-decode step goes from the boundary to the canonical-decode
 * resolver and on to the award (one-step-canonical-decode.ts). Each hop
 * spends the thread, which pays
 * its fee up to the cap, or our wallet pays it all (move-fee.ts decides); its
 * successor keeps the thread's single token and carries the state the next
 * holder checks as an inline datum. */
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import type { DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import {
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type Script,
  type TxBuilder,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";

import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import { buildValidationCekExecutionSelectionAction } from "../validation-cek-selection.js";
import {
  canonicalDecodeHop,
  isValidationTraceCanonicalDecodeHolder,
} from "./one-step-canonical-decode.js";
import {
  isValidationTraceContextHolder,
  validationTraceContextHop,
} from "./one-step-context-hops.js";
import type { ValidationTraceContextItemReads } from "./one-step-context-item-hops.js";
import {
  bytes,
  constr,
  continued,
  integer,
  stepDatum,
} from "./one-step-data.js";
import {
  type ValidationTraceArmHolder,
  type ValidationTraceOneStepHolder,
} from "./one-step-holders.js";
import {
  hashSerialisedData,
  SINGLE_HOP_CORE_ARMS,
  type ValidationTraceCoreStepPlan,
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
   * `outputIndex`, in the final transaction `context`. */
  redeemer: (
    inputIndex: bigint,
    outputIndex: bigint,
    context: RedeemerContext,
  ) => string;
  /** Whether the holder lets the prover alone move the thread. */
  proverOnly: boolean;
  /** What else the holder reads: further reference inputs, and the yields it
   * requires invoked by a zero withdrawal each. */
  reads?: Readonly<{
    references: readonly UTxO[];
    yields: readonly Script[];
  }>;
}>;

/**
 * What a CEK script selection's hop reads: our trace, the selection's two
 * yields (authenticate, successor) with their authenticated references, and
 * the selected script's decode record under the script-decode policy, which
 * a selection that reads no program goes without.
 */
export type ValidationTraceSelectionReads = Readonly<{
  trace: DeterministicValidationMachineTrace;
  yields: Readonly<{
    references: readonly [UTxO, UTxO];
    scripts: readonly [Script, Script];
  }>;
  record: UTxO | undefined;
  scriptDecodePolicyId: string;
}>;

/** The arm holder of the plan's core group. */
export const validationTraceArmOf = (
  plan: ValidationTraceCoreStepPlan,
): ValidationTraceArmHolder => SINGLE_HOP_CORE_ARMS[plan.core.group];

/** The core step plan a core holder's hop needs. */
const corePlan = (
  plan: ValidationTraceOneStepPlan,
  holder: ValidationTraceHopHolder,
): ValidationTraceCoreStepPlan => {
  if (plan.kind !== "core")
    throw new Error(
      `validation-trace ${holder} holds a thread whose step is not a CEK core step`,
    );
  return plan;
};

const datumOf = (utxo: UTxO): string => {
  if (typeof utxo.datum !== "string")
    throw new Error("validation-trace thread has no inline datum");
  return utxo.datum;
};

/**
 * The hop of the thread `utxo` of prover `prover`, held by `holder`, in the
 * one-step resolution `plan`; a selection's hop reads `selection`, and a hop
 * through a context step's redeemer item step reads `item`.
 */
export const validationTraceHop = ({
  plan,
  holder,
  utxo,
  prover,
  selection,
  item,
}: {
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: ValidationTraceHopHolder;
  readonly utxo: UTxO;
  readonly prover: string;
  readonly selection?: ValidationTraceSelectionReads;
  readonly item?: ValidationTraceContextItemReads;
}): ValidationTraceHop => {
  const datum = datumOf(utxo);
  if (isValidationTraceContextHolder(holder))
    return validationTraceContextHop({
      plan,
      holder,
      datum,
      prover,
      ...(item === undefined ? {} : { item }),
    });
  if (isValidationTraceCanonicalDecodeHolder(holder)) {
    if (plan.kind !== "canonicalDecode")
      throw new Error(
        `validation-trace ${holder} holds a thread whose step is not a canonical-decode step`,
      );
    return canonicalDecodeHop({
      holder,
      datum,
      prover,
      semanticResolverIndex: plan.semanticResolverIndex,
      transitionCbor: plan.transitionCbor,
      step: plan.canonicalDecode,
    });
  }
  switch (holder) {
    case "boundary": {
      const thread = Data.from(datum, SDK.CaseThreadDatum).data;
      if (thread === null)
        throw new Error("validation-trace boundary thread has no state");
      const preState = SDK.validationMachineStateDataFromCore(plan.preState);
      return {
        next:
          plan.kind === "canonicalDecode"
            ? "canonicalDecodePrepare"
            : "prepare",
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
        next:
          plan.kind === "selection"
            ? "selection"
            : plan.kind === "context"
              ? "context"
              : "core",
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
              integer(plan.semanticResolverIndex),
              plan.transitionCbor,
              plan.auxiliaryCbor,
            ]),
          ),
        proverOnly: true,
      };
    case "selection": {
      if (plan.kind !== "selection" || selection === undefined)
        throw new Error(
          "validation-trace selection holds a thread whose step is not a CEK script selection",
        );
      const { trace, yields, record, scriptDecodePolicyId } = selection;
      return {
        next: "award",
        datum: stepDatum(prover, constr(0, [integer(1)])),
        redeemer: (inputIndex, outputIndex, context) =>
          Data.to(
            {
              Continue: [
                buildValidationCekExecutionSelectionAction({
                  context,
                  trace,
                  stateIndex: plan.stateIndex,
                  inputIndex,
                  outputIndex,
                  yieldReferences: yields.references,
                  ...(record === undefined
                    ? {}
                    : { decodeRecordReference: record }),
                  scriptDecodePolicyId,
                }),
              ],
            },
            SDK.ValidationCekExecutionSelectionSpendRedeemer,
          ),
        proverOnly: false,
        reads: {
          references: [
            ...yields.references,
            ...(record === undefined ? [] : [record]),
          ],
          yields: yields.scripts,
        },
      };
    }
    case "core": {
      const step = corePlan(plan, holder);
      const { core } = step;
      return {
        next: validationTraceArmOf(step),
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
    }
    case "settle": {
      const { core } = corePlan(plan, holder);
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
    }
    case "compute":
    case "machine":
    case "mapConversion":
    case "caseConstant": {
      const { core } = corePlan(plan, holder);
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
  }
};

/**
 * The hop `hop` of `thread`, carrying its token `unit` (reading the holder's
 * script from `reference`) to `address`, which keeps the thread's tokens and
 * its lovelace less `fee`, before completion; with the references the hop
 * reads and a zero withdrawal from each yield it invokes; signed by `prover`
 * when the holder needs it.
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
  let tx = lucid
    .newTx()
    .collectFrom([thread], (ctx: RedeemerContext) =>
      hop.redeemer(
        SDK.requireInputIndex(ctx, thread, "thread"),
        SDK.requireUniqueOutputIndex(
          ctx.outputs,
          outputWithDatumAndUnitPredicate({ address, datum: hop.datum, unit }),
          "thread successor",
        ),
        ctx,
      ),
    )
    .readFrom([reference, ...(hop.reads?.references ?? [])]);
  const network = lucid.config().network;
  if (network === undefined)
    throw new Error("validation-trace hop has no network");
  for (const script of hop.reads?.yields ?? [])
    tx = tx.withdraw(
      validatorToRewardAddress(network, script),
      0n,
      Data.void(),
    );
  tx = tx.pay
    .ToContract(
      address,
      { kind: "inline", value: hop.datum },
      { ...thread.assets, lovelace: (thread.assets.lovelace ?? 0n) - fee },
    )
    .validFrom(validFrom)
    .validTo(validTo);
  return hop.proverOnly ? tx.addSignerKey(prover) : tx;
};
