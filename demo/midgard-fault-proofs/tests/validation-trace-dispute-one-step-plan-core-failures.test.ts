/** Our plan of the two builtin failures of core group 7 (arm 36) that read
 * more than one hop's carriage: a writeBits failure over an input of at
 * most 512 words, which the semantic failure roots route to the writeBits
 * loop and the loop walks to the index outside the input, and a staged
 * builtin failure whose operand is past the staging chunk, whose evidence
 * names the staging outputs that hold it, as a staged builtin step's does
 * (validation-trace-dispute-one-step-plan-staged.test.ts). */
import { MIDGARD_CEK_STAGED_CHUNK_BYTES } from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type MidgardCekCoreStepWitness,
  midgardCekStagedBuiltinMaterialData,
  stageMidgardCekStagedBuiltinMaterial,
} from "@al-ft/midgard-validation";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import {
  validationTraceHop,
  type ValidationTraceHopHolder,
} from "../src/validation-trace-dispute/one-step-builders.js";
import type { ValidationTraceCoreStepPlan } from "../src/validation-trace-dispute/one-step-plan.js";
import type { ValidationTraceStagingLookup } from "../src/validation-trace-dispute/one-step-staged.js";
import {
  DECODE_UTF8_FAILURE_SCRIPT,
  WRITE_BITS_FAILURE_SCRIPT,
} from "./support/validation-trace-dispute-loop-scripts.js";
import {
  failingScriptMaterial,
  planCoreStep,
  PROVER,
  stagingOn,
  stagingOutput,
  threadUtxo,
} from "./support/validation-trace-dispute-one-step-core-plan.js";

/** Where the bound a core holder keeps carries its progress and facts. */
const PROGRESS = [1, 0, 6];
const FACTS = [1, 0, 7];

const NO_FACTS = Data.to(new Constr(0, []));

/** The step of `material` whose witness is of `kind`, the last one, and
 * that witness. */
const lastStepOf = async <Kind extends MidgardCekCoreStepWitness["kind"]>(
  material: ValidationTraceClaimMaterial,
  kind: Kind,
) => {
  for (let index = Number(material.stepCount) - 1; index >= 0; index -= 1) {
    const auxiliary = (await material.steps(index)).witnesses[index]?.auxiliary;
    if (
      auxiliary?.kind === "cekCoreStep" &&
      auxiliary.step.witness.kind === kind
    )
      return {
        stateIndex: index,
        witness: auxiliary.step.witness as Extract<
          MidgardCekCoreStepWitness,
          { readonly kind: Kind }
        >,
      };
  }
  throw new Error(`our trace has no step of ${kind}`);
};

/** The hops of the thread of `plan` from each arm holder it visits, from
 * the PrepareSelected move to the settle, each built from the datum the one
 * before it wrote, with the progress and facts it writes; every hop reads
 * `staging`. */
const walk = (plan: ValidationTraceCoreStepPlan, staging?: readonly UTxO[]) => {
  let utxo = threadUtxo;
  let holder: ValidationTraceHopHolder = "prepare";
  const visits: { holder: string; progress: Data; facts: string }[] = [];
  for (;;) {
    const hop = validationTraceHop({
      plan,
      holder,
      utxo,
      prover: PROVER,
      ...(staging === undefined ? {} : { staging }),
    });
    expect(hop.reads, holder).toEqual(
      staging === undefined ? undefined : { references: staging, yields: [] },
    );
    if (holder !== "prepare" && holder !== "core")
      visits.push({
        holder,
        progress: Data.from(plutusConstrFieldCbor(hop.datum, PROGRESS)),
        facts: plutusConstrFieldCbor(hop.datum, FACTS),
      });
    if (hop.next === "settle") return visits;
    holder = hop.next as ValidationTraceHopHolder;
    utxo = { ...utxo, datum: hop.datum };
  }
};

describe("our plan of a writeBits failure", () => {
  let plan: ValidationTraceCoreStepPlan;

  beforeAll(async () => {
    const material = await failingScriptMaterial(WRITE_BITS_FAILURE_SCRIPT);
    const { stateIndex, witness } = await lastStepOf(
      material,
      "executeBuiltinWriteBitsFailure",
    );
    expect(witness.indices).toHaveLength(200);
    plan = await planCoreStep(material, stateIndex);
  });

  it("settles through the semantic failure roots, then the writeBits loop once per hop of its walk to the index outside the input", () => {
    const { holders, facts, loops } = plan.core.hops;
    expect(plan.core.arm).toBe(36);
    expect(plan.core.group).toBe(7);
    expect(plan.core.staged).toBeUndefined();
    // The hop budget fits the 200 indices in three hops.
    expect(holders).toEqual([
      "semanticFailureRoots",
      ...Array<string>(3).fill("stagedWriteBitsLoop"),
    ]);
    expect(loops?.[0]).toBeUndefined();
    const walked = loops!.slice(1).map((loop) => loop!);
    const ends = walked.map(({ end }) => end);
    expect(ends).toEqual([...ends].sort((x, y) => x - y));
    // The walk ends on the hop that opens the last index, 64.
    expect(walked.at(-1)).toMatchObject({ end: 200, state: null });
    expect(walked.every(({ hop }) => hop !== undefined)).toBe(true);
    // The roots and the hop that ends the walk write no facts; each hop
    // before it, the loop's facts over none, with its cursor.
    expect([facts[0], facts.at(-1)]).toEqual([NO_FACTS, NO_FACTS]);
    walked.slice(0, -1).forEach(({ end }, index) => {
      const written = Data.from(facts[index + 1]!) as Constr<Data>;
      expect(written.index).toBe(1);
      expect(Data.to(written.fields[0]!)).toBe(NO_FACTS);
      expect(written.fields[1]).toBe(BigInt(end));
    });
  });

  it("moves the thread from the roots to the loop with progress 1, keeps it at 1 through the loop and leaves the loop for the settle with progress 2", () => {
    const visits = walk(plan);
    expect(visits.map(({ holder }) => holder)).toEqual(plan.core.hops.holders);
    expect(visits.map(({ progress }) => progress)).toEqual([
      ...visits.slice(1).map(() => 1n),
      2n,
    ]);
    expect(visits.map(({ facts }) => facts)).toEqual(plan.core.hops.facts);
  });
});

describe("our plan of a staged builtin failure whose operand is past the staging chunk", () => {
  let witness: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "executeBuiltinStagedFailure" }
  >;
  let plan: (
    staging?: ValidationTraceStagingLookup,
  ) => Promise<ValidationTraceCoreStepPlan>;

  beforeAll(async () => {
    const material = await failingScriptMaterial(DECODE_UTF8_FAILURE_SCRIPT);
    const step = await lastStepOf(material, "executeBuiltinStagedFailure");
    witness = step.witness;
    plan = (staging) => planCoreStep(material, step.stateIndex, { staging });
  });

  it("plans the failure before its operand is staged with its staged operand and no staging outputs, through the roots to the staged failure hop", async () => {
    expect(witness.tag).toBe(25n);
    expect(witness.stagedOperands.map(({ length }) => length)).toEqual([
      16_384,
    ]);
    expect(witness.stagedOperands[0]!.length).toBeGreaterThan(
      MIDGARD_CEK_STAGED_CHUNK_BYTES,
    );
    const unstaged = await plan();
    expect(unstaged.core.arm).toBe(36);
    expect(unstaged.core.group).toBe(7);
    expect(unstaged.core.staged).toEqual({
      operands: witness.stagedOperands,
      refs: undefined,
    });
    expect(unstaged.core.hops.holders).toEqual([
      "semanticFailureRoots",
      "stagedFailure",
    ]);
  });

  it("plans it over its staging: its arm witness names the staging outputs, and every hop from the PrepareSelected move through the staged failure hop reads them", async () => {
    const staging = stagingOn(0x11);
    const staged = await plan((operands) => Promise.resolve(staging(operands)));
    const filled = stageMidgardCekStagedBuiltinMaterial(
      witness.material,
      witness.stagedOperands,
      staging(witness.stagedOperands),
    );
    expect(filled.runs.refs.length).toBe(2);
    expect(staged.core.staged).toEqual({
      operands: witness.stagedOperands,
      refs: filled.runs.refs,
    });
    expect(plutusConstrFieldCbor(staged.core.armWitnessCbor, [2])).toBe(
      Data.to(midgardCekStagedBuiltinMaterialData(filled) as Data),
    );
    expect(staged.evidenceHash).not.toBe((await plan()).evidenceHash);
    const outputs = filled.runs.refs.map(stagingOutput);
    expect(walk(staged, outputs).map(({ holder }) => holder)).toEqual([
      "semanticFailureRoots",
      "stagedFailure",
    ]);
  });
});
