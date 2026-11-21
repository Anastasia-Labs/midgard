/** Our plan of the CEK context step that finalizes a Plutus execution's
 * context, and the hops it drives: the finalize authenticate binds the
 * execution's redeemers control and routes the thread by the staged
 * context, a MidgardV1 execution to the finalize midgard and a PlutusV3 one
 * by its purpose, to the finalize spend, mint or withdraw; each of those
 * finalizes the context for the context settle. A failing transaction
 * reaches each such step when the failing script is the spend's, a minting
 * policy's or a required observer's, after a passing spend. */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { ValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import { validationTraceHop } from "../src/validation-trace-dispute/one-step-builders.js";
import {
  constr,
  stepDatum,
} from "../src/validation-trace-dispute/one-step-data.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceContextStepPlan,
} from "../src/validation-trace-dispute/one-step-plan.js";
import {
  FAILING_SCRIPT,
  PASSING_SCRIPT,
  plutusExecutionBlockFixture,
  type ScenarioPlutusPurpose,
} from "./helpers/plutus-execution-block-fixture.js";
import { claimMaterialOf } from "./support/validation-trace-claim-material.js";

const PROVER = "ab".repeat(28);

/** The failing script, run as a MidgardV1 script. */
const MIDGARD_FAILING_SCRIPT: MidgardVersionedScript = {
  language: "MidgardV1",
  scriptBytes: FAILING_SCRIPT.scriptBytes,
};

/** A thread whose datum nests the state each context holder's hop reads. */
const threadUtxo: UTxO = {
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: "addr_test1wz",
  assets: { lovelace: 5_000_000n },
  datum: Data.to(
    new Constr(0, [
      PROVER,
      new Constr(0, [new Constr(0, [new Constr(0, Array(8).fill(0n))])]),
    ]),
  ),
};

/** The last step of our trace that finalizes a CEK context. */
const lastFinalizeStep = async (
  material: ValidationTraceClaimMaterial,
): Promise<number> => {
  for (let index = Number(material.stepCount) - 1; index >= 0; index -= 1) {
    const kind = (await material.steps(index)).witnesses[index]?.auxiliary
      ?.kind;
    if (kind === "cekContextFinalize" || kind === "cekContextFinalizeSpend")
      return index;
  }
  throw new Error("our trace finalizes no CEK context");
};

/** Our plan of the last finalize step of the failing transaction. */
const finalizePlan = async (
  options: Readonly<{
    spend?: MidgardVersionedScript;
    plutusPurpose?: ScenarioPlutusPurpose;
  }>,
): Promise<ValidationTraceContextStepPlan> => {
  const material = await claimMaterialOf(
    await plutusExecutionBlockFixture("failing", {
      ...(options.spend === undefined
        ? {}
        : { spend: { script: options.spend } }),
      ...(options.plutusPurpose === undefined
        ? {}
        : { plutusPurpose: options.plutusPurpose }),
    }),
  );
  const stateIndex = await lastFinalizeStep(material);
  const planning = await planValidationTraceOneStep({
    material,
    stateIndex,
    challengerHighHash: hashMidgardValidationMachineState(
      (await material.steps(stateIndex)).states[stateIndex + 1]!,
    ).toString("hex"),
    contexts: true,
  });
  expect(planning).toMatchObject({ kind: "plan", plan: { kind: "context" } });
  if (planning.kind !== "plan" || planning.plan.kind !== "context")
    throw new Error("the finalize step is not planned as a context step");
  return planning.plan;
};

const CASES = [
  {
    name: "a PlutusV3 spend",
    options: {},
    follower: "contextFinalizeSpend",
  },
  {
    name: "a PlutusV3 minting policy",
    options: {
      spend: PASSING_SCRIPT,
      plutusPurpose: { purpose: "mint", script: FAILING_SCRIPT },
    },
    follower: "contextFinalizeMint",
  },
  {
    name: "a PlutusV3 required observer",
    options: {
      spend: PASSING_SCRIPT,
      plutusPurpose: { purpose: "withdraw", script: FAILING_SCRIPT },
    },
    follower: "contextFinalizeWithdraw",
  },
  {
    name: "a MidgardV1 spend",
    options: { spend: MIDGARD_FAILING_SCRIPT },
    follower: "contextFinalizeMidgard",
  },
] as const;

describe("our plan of a CEK context finalize step", () => {
  it.each(CASES)(
    "plans the finalize of $name through the finalize authenticate to $follower, and on to the context settle",
    async ({ options, follower }) => {
      const plan = await finalizePlan(options);
      expect(plan.context.stage).toBe("finalizeAuthenticate");
      const { finalization } = plan.context;
      expect(finalization?.holder).toBe(follower);
      // The redeemers control the finalize evidence carries: its map items
      // and current redeemer, whichever constructor carries it.
      const redeemers = (Data.from(plan.auxiliaryCbor) as Constr<Data>)
        .fields[0] as Constr<Data>;
      expect(finalization?.redeemerItemsCbor).toBe(
        Data.to(redeemers.fields[1]!),
      );
      expect(finalization?.redeemerCbor).toBe(Data.to(redeemers.fields[5]!));

      const authenticate = validationTraceHop({
        plan,
        holder: "contextFinalizeAuthenticate",
        utxo: threadUtxo,
        prover: PROVER,
      });
      expect(authenticate.next).toBe(follower);
      const staged = Data.to(new Constr(0, [new Constr(0, Array(8).fill(0n))]));
      expect(authenticate.datum).toBe(
        stepDatum(
          PROVER,
          constr(0, [
            staged,
            finalization!.redeemerItemsCbor,
            finalization!.redeemerCbor,
          ]),
        ),
      );
      const finalize = validationTraceHop({
        plan,
        holder: follower,
        utxo: { ...threadUtxo, datum: authenticate.datum },
        prover: PROVER,
      });
      expect(finalize.next).toBe("contextSettle");
      expect(finalize.datum).toBe(
        stepDatum(PROVER, constr(0, [staged, plan.context.successorCbor])),
      );
    },
    300_000,
  );
});
