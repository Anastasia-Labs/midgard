/** Our plan of a staged builtin step (arm 31, semantics groups 7 and 8)
 * and the hops it drives: the step's evidence names the staging outputs
 * that hold its staged operands, so the plan commits it only once they are
 * staged, and the hops from the PrepareSelected move through the arm chain
 * read them. */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type MidgardCekStagingResult,
  stageMidgardCekStagedBuiltinMaterial,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import {
  validationTraceHop,
  type ValidationTraceHopHolder,
} from "../src/validation-trace-dispute/one-step-builders.js";
import { decodeCekCoreSemanticMaterial } from "../src/validation-trace-dispute/one-step-core-chains.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceCoreStepPlan,
} from "../src/validation-trace-dispute/one-step-plan.js";
import { resolvingPlan } from "../src/validation-trace-dispute/one-step-resolving-plan.js";
import {
  stagedTraceOf,
  type ValidationTraceStagingLookup,
} from "../src/validation-trace-dispute/one-step-staged.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import { claimMaterialOf } from "./support/validation-trace-claim-material.js";
import {
  STAGED_SHA2_SCRIPT,
  stagedScript,
  type StagedWitness,
} from "./support/validation-trace-dispute-staged-scripts.js";

const PROVER = "ab".repeat(28);

/** The staged witness of the step from `index`, if it is one. */
const stagedWitnessAt = (
  trace: ValidationTraceSteps,
  index: number,
): StagedWitness | undefined => {
  const auxiliary = trace.witnesses[index]?.auxiliary;
  return auxiliary?.kind === "cekCoreStep" &&
    auxiliary.step.witness.kind === "executeBuiltinStaged"
    ? auxiliary.step.witness
    : undefined;
};

/** The last step of our trace that is a staged builtin step. */
const lastStagedStep = async (
  material: ValidationTraceClaimMaterial,
): Promise<number> => {
  for (let index = Number(material.stepCount) - 1; index >= 0; index -= 1)
    if (stagedWitnessAt(await material.steps(index), index) !== undefined)
      return index;
  throw new Error("our trace has no staged builtin step");
};

/** A staging of `operands` on outputs of the transactions `ids`, in turn,
 * one output a run. */
const stagingOn =
  (...ids: number[]) =>
  (operands: readonly Uint8Array[]): MidgardCekStagingResult => {
    let next = 0;
    return operands.map((operand) =>
      Array.from(
        { length: Math.max(1, Math.ceil(operand.length / 12_285)) },
        (_, run) => ({
          transactionId: Buffer.alloc(32, ids[run % ids.length]!),
          outputIndex: next++,
        }),
      ),
    );
  };

/** A lookup that answers `staging` and records what it was asked. */
const lookup = (
  staging:
    | ((operands: readonly Uint8Array[]) => MidgardCekStagingResult)
    | undefined,
) => {
  const asked: (readonly Uint8Array[])[] = [];
  const answer: ValidationTraceStagingLookup = (operands) => {
    asked.push(operands);
    return Promise.resolve(staging?.(operands));
  };
  return { asked, answer };
};

/** A lookup offering each of `stagings`: the first `accepts` holds of,
 * else the first. */
const offering =
  (
    ...stagings: ((
      operands: readonly Uint8Array[],
    ) => MidgardCekStagingResult)[]
  ): ValidationTraceStagingLookup =>
  (operands, accepts) => {
    const offered = stagings.map((staging) => staging(operands));
    return Promise.resolve(
      offered.find((staging) => accepts?.(staging) === true) ?? offered[0],
    );
  };

/** A thread whose datum has the shape every core holder's hop reads. */
const threadUtxo: UTxO = {
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: "addr_test1wz",
  assets: { lovelace: 5_000_000n },
  datum: Data.to(
    new Constr(0, [PROVER, new Constr(0, [new Constr(0, Array(8).fill(0n))])]),
  ),
};

/** The staging output at `ref`. */
const stagingOutput = (ref: {
  transactionId: Uint8Array;
  outputIndex: number;
}): UTxO => ({
  txHash: Buffer.from(ref.transactionId).toString("hex"),
  outputIndex: ref.outputIndex,
  address: "addr_test1wstaging",
  assets: { lovelace: 2_000_000n },
});

describe("our plan of a staged builtin step", () => {
  let material: ValidationTraceClaimMaterial;
  let stateIndex: number;
  let trace: ValidationTraceSteps;
  let witness: StagedWitness;
  let challengerHighHash: string;

  beforeAll(async () => {
    material = await claimMaterialOf(
      await plutusExecutionBlockFixture("failing", {
        spend: { script: stagedScript(STAGED_SHA2_SCRIPT) },
      }),
    );
    stateIndex = await lastStagedStep(material);
    trace = await material.steps(stateIndex);
    witness = stagedWitnessAt(trace, stateIndex)!;
    challengerHighHash = hashMidgardValidationMachineState(
      trace.states[stateIndex + 1]!,
    ).toString("hex");
  });

  const plan = async (
    staging: ValidationTraceStagingLookup | undefined,
    on: ValidationTraceClaimMaterial = material,
    committedEvidenceHash?: () => string,
  ): Promise<ValidationTraceCoreStepPlan> => {
    const planning = await planValidationTraceOneStep({
      material: on,
      stateIndex,
      challengerHighHash,
      builtins: true,
      ...(staging === undefined ? {} : { staging }),
      ...(committedEvidenceHash === undefined ? {} : { committedEvidenceHash }),
    });
    if (planning.kind !== "plan" || planning.plan.kind !== "core")
      throw new Error(`the step is not planned as a core step`);
    return planning.plan;
  };

  it("plans a step whose operands are not staged yet as unstaged: it names the staged operands and no staging outputs", async () => {
    expect(witness.stagedOperands.length).toBeGreaterThan(0);
    const { asked, answer } = lookup(undefined);
    const unstaged = await plan(answer);
    expect(unstaged.core.group).toBe(5);
    expect(unstaged.core.arm).toBe(31);
    expect(asked).toEqual([witness.stagedOperands]);
    expect(unstaged.core.staged).toEqual({
      operands: witness.stagedOperands,
      refs: undefined,
    });
    expect((await plan(undefined)).core.staged).toEqual(unstaged.core.staged);
  });

  it("plans a staged step over its staging: its arm witness names the staging outputs in ledger order", async () => {
    const staging = stagingOn(0x99, 0x11);
    const staged = await plan(lookup(staging).answer);
    const filled = stageMidgardCekStagedBuiltinMaterial(
      witness.material,
      witness.stagedOperands,
      staging(witness.stagedOperands),
    );
    expect(filled.runs.refs.length).toBeGreaterThan(1);
    expect(Buffer.from(filled.runs.refs[0]!.transactionId)[0]).toBe(0x11);
    expect(staged.core.staged).toEqual({
      operands: witness.stagedOperands,
      refs: filled.runs.refs,
    });
    const armWitness = Data.from(staged.core.armWitnessCbor) as Constr<Data>;
    const decoded = decodeCekCoreSemanticMaterial(18n, armWitness.fields[3]!);
    expect(decoded.kind).toBe("staged");
    expect(decoded.kind === "staged" && decoded.material.runs.refs).toEqual(
      filled.runs.refs.map((ref) => ({
        transactionId: Buffer.from(ref.transactionId).toString("hex"),
        outputIndex: BigInt(ref.outputIndex),
      })),
    );
    expect(plutusConstrFieldCbor(staged.auxiliaryCbor, [0])).toBe(
      staged.core.stepCbor,
    );
  });

  it("commits the step's evidence over its staging: the same staging gives the same evidence, another staging other evidence", async () => {
    const unstaged = await plan(undefined);
    const staged = await plan(lookup(stagingOn(0x11)).answer);
    const again = await plan(lookup(stagingOn(0x11)).answer);
    const other = await plan(lookup(stagingOn(0x22)).answer);
    expect(again.evidenceHash).toBe(staged.evidenceHash);
    expect(again.auxiliaryCbor).toBe(staged.auxiliaryCbor);
    expect(staged.evidenceHash).not.toBe(unstaged.evidenceHash);
    expect(other.evidenceHash).not.toBe(staged.evidenceHash);
    // The staging is in the evidence's auxiliary alone: the transition, and
    // so the step's states, do not depend on it.
    expect(staged.transitionCbor).toBe(unstaged.transitionCbor);
    expect(other.transitionCbor).toBe(unstaged.transitionCbor);
    expect(staged.core.preCbor).toBe(unstaged.core.preCbor);
    expect(staged.core.postCbor).toBe(unstaged.core.postCbor);
    expect(staged.core.hops).toEqual(unstaged.core.hops);
  });

  it("leaves our trace as it was", async () => {
    const held = await material.steps(stateIndex);
    const before = stagedWitnessAt(held, stateIndex)!.material;
    await plan(lookup(stagingOn(0x11)).answer, {
      ...material,
      steps: () => Promise.resolve(held),
    });
    expect(stagedWitnessAt(held, stateIndex)!.material).toBe(before);
    expect(before.runs.refs).toEqual([]);
  });

  it("refuses a staging that does not name an output for each run of each staged operand", async () => {
    await expect(plan(() => Promise.resolve([]))).rejects.toThrow(
      /staging does not cover every staged operand/,
    );
    await expect(
      plan((operands) => Promise.resolve(operands.map(() => []))),
    ).rejects.toThrow(/staging does not name one output per run/);
  });

  it("plans a step with no staged operand without asking for a staging", async () => {
    let index = stateIndex - 1;
    let planning;
    const { asked, answer } = lookup(stagingOn(0x11));
    for (; index >= 0; index -= 1) {
      const steps = await material.steps(index);
      planning = await planValidationTraceOneStep({
        material,
        stateIndex: index,
        challengerHighHash: hashMidgardValidationMachineState(
          steps.states[index + 1]!,
        ).toString("hex"),
        builtins: true,
        staging: answer,
      });
      if (planning.kind === "plan" && planning.plan.kind === "core") break;
    }
    expect(planning?.kind === "plan" && planning.plan.kind).toBe("core");
    if (planning?.kind !== "plan" || planning.plan.kind !== "core") return;
    expect(planning.plan.core.staged).toBeUndefined();
    expect(asked).toEqual([]);
    for (const holder of ["core", "settle"] as const)
      expect(
        validationTraceHop({
          plan: planning.plan,
          holder,
          utxo: threadUtxo,
          prover: PROVER,
        }).reads,
      ).toBeUndefined();
  });

  it("needs no staging for a staged step whose operands are all carried inline", async () => {
    const work = trace.witnesses[stateIndex]!;
    if (work.auxiliary?.kind !== "cekCoreStep") throw new Error("no core step");
    const inline: ValidationTraceSteps = {
      states: trace.states,
      witnesses: {
        [stateIndex]: {
          ...work,
          auxiliary: {
            ...work.auxiliary,
            step: {
              ...work.auxiliary.step,
              witness: { ...witness, stagedOperands: [] },
            },
          },
        },
      },
    };
    const { asked, answer } = lookup(stagingOn(0x11));
    const staged = await stagedTraceOf(inline, stateIndex, answer);
    expect(staged.staged).toBeUndefined();
    expect(staged.trace).toBe(inline);
    expect(asked).toEqual([]);
  });

  describe("past its PrepareSelected move", () => {
    it("plans a staged step over the staging whose evidence the move committed", async () => {
      const committed = await plan(lookup(stagingOn(0x11)).answer);
      const offered = offering(stagingOn(0x22), stagingOn(0x11));
      const latest = await plan(offered);
      expect(latest.evidenceHash).not.toBe(committed.evidenceHash);
      const planned = await plan(
        offered,
        material,
        () => committed.evidenceHash,
      );
      expect(planned.evidenceHash).toBe(committed.evidenceHash);
      expect(planned.core.staged).toEqual(committed.core.staged);
    });

    it("refuses a staging other than the one the move's evidence names, and refuses as well when no staging is found", async () => {
      const committed = await plan(lookup(stagingOn(0x11)).answer);
      await expect(
        plan(offering(stagingOn(0x22)), material, () => committed.evidenceHash),
      ).rejects.toThrow(/differs from the evidence its thread committed/);
      await expect(
        plan(lookup(undefined).answer, material, () => committed.evidenceHash),
      ).rejects.toThrow(/differs from the evidence its thread committed/);
    });

    it("reads the evidence the move committed where the core holder and each arm holder keep it", async () => {
      const committed = await plan(lookup(stagingOn(0x11)).answer);
      const outputs = committed.core.staged!.refs!.map(stagingOutput);
      const deployment = {
        references: { oneStep: { cekBuiltins: {} } },
      } as never;
      let holder: ValidationTraceHopHolder = "prepare";
      let datum = Data.to(
        new Constr(0, [
          PROVER,
          new Constr(0, [
            new Constr(0, [0n, 0n, 0n, challengerHighHash, 0n, 0n, 0n, 0n]),
          ]),
        ]),
      );
      const resolved: string[] = [];
      for (;;) {
        const hop = validationTraceHop({
          plan: committed,
          holder,
          utxo: { ...threadUtxo, datum },
          prover: PROVER,
          staging: outputs,
        });
        if (hop.next === "settle" || hop.next === "award") break;
        holder = hop.next as ValidationTraceHopHolder;
        datum = hop.datum;
        const planned = await resolvingPlan({
          deployment,
          material,
          utxo: { ...threadUtxo, datum },
          holder,
          caseThread: undefined,
          carried: undefined,
          staging: offering(stagingOn(0x22), stagingOn(0x11)),
        });
        expect(planned.evidenceHash, holder).toBe(committed.evidenceHash);
        resolved.push(holder);
      }
      expect(resolved).toEqual(["core", ...committed.core.hops.holders]);
    });
  });

  describe("the hops of a staged step", () => {
    const holdersReading = [
      "prepare",
      "core",
      "semanticRoots",
      "semanticResult",
      "semanticBudget",
      "stagedBytes",
    ] as const;

    it("read its staging outputs, in ledger order, from the PrepareSelected move through the arm chain; the settle reads none", async () => {
      const staged = await plan(lookup(stagingOn(0x99, 0x11)).answer);
      const outputs = staged.core.staged!.refs!.map(stagingOutput);
      expect(staged.core.hops.holders).toEqual(holdersReading.slice(2));
      for (const holder of holdersReading) {
        const hop = validationTraceHop({
          plan: staged,
          holder,
          utxo: threadUtxo,
          prover: PROVER,
          staging: outputs,
        });
        expect(hop.reads, holder).toEqual({ references: outputs, yields: [] });
      }
      expect(
        validationTraceHop({
          plan: staged,
          holder: "settle",
          utxo: threadUtxo,
          prover: PROVER,
          staging: outputs,
        }).reads,
      ).toBeUndefined();
    });

    it("are refused while its operands are not staged", async () => {
      const unstaged = await plan(undefined);
      for (const holder of holdersReading)
        expect(
          () =>
            validationTraceHop({
              plan: unstaged,
              holder,
              utxo: threadUtxo,
              prover: PROVER,
            }),
          holder,
        ).toThrow(/staged operands are not staged/);
    });

    it("are refused with staging outputs other than those the plan names", async () => {
      const staged = await plan(lookup(stagingOn(0x99, 0x11)).answer);
      const outputs = staged.core.staged!.refs!.map(stagingOutput);
      for (const staging of [
        undefined,
        outputs.slice(1),
        [...outputs].reverse(),
        [...outputs, { ...outputs[0]!, outputIndex: 77 }],
        outputs.map((output, index) =>
          index === 0 ? { ...output, txHash: "33".repeat(32) } : output,
        ),
      ])
        expect(() =>
          validationTraceHop({
            plan: staged,
            holder: "stagedBytes",
            utxo: threadUtxo,
            prover: PROVER,
            ...(staging === undefined ? {} : { staging }),
          }),
        ).toThrow(/staging outputs/);
    });
  });
});
