/** The CEK core's routes, at route level: for each core group, the routes
 * from its entry stage to the settle that the SDK wires over the real
 * blueprint (midgard-sdk contracts/cek-core.ts) are the routes our planner
 * builds an arm chain through over a pool of witnesses, so every wired route
 * has a planned route and every planned route is wired. A wired route our
 * planner cannot build is a step an honest challenger cannot prove.
 *
 * The pool holds the witnesses our executor writes in the runs of the
 * builtin step and loop scripts and, for every tag, a direct builtin's, a
 * semantic builtin's and a semantic failure's witness over no operands. Our
 * planner routes those by the tag alone, also for a tag our executor never
 * writes such a witness for, and the validators of the route it reaches
 * would refuse that witness. So this does not check that our planner routes
 * each tag, or each operand size class, where the validators accept it. */
import {
  blueprintApplicationRecords,
  buildCekCoreStages,
  CEK_CORE_ARM_HOP_COUNTS,
  cekCoreEntryHashes,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import type {
  MidgardCekCoreStepWitness,
  MidgardCekDirectValueWitness,
  MidgardCekSemanticBuiltinWitness,
} from "@al-ft/midgard-validation";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  cekCoreArmHops,
  isCekCoreGroup,
} from "../src/validation-trace-dispute/one-step-core-chains.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import {
  DECODE_UTF8_FAILURE_SCRIPT,
  PRODUCT_SCRIPT,
  SERIALISE_SCRIPT,
  WRITE_BITS_FAILURE_SCRIPT,
  WRITE_BITS_SCRIPT,
} from "./support/validation-trace-dispute-loop-scripts.js";
import { CEK_BUILTIN_STEP_SCRIPTS } from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";
import { runWitnesses } from "./support/validation-trace-dispute-staged-scripts.js";

/** The stages as the SDK wires them, and the stages each one's parameters
 * name, which are the stages it may hand a thread on to. */
const wiring = () => {
  const blueprint = parseFaultProofBlueprint(readBlueprint(realBlueprintPath));
  const stages = buildCekCoreStages(
    blueprint,
    network,
    "a0".repeat(28),
    "b0".repeat(28),
    "c0".repeat(28),
  );
  const stageOf = new Map(
    Object.entries(stages).map(([stage, { spendingScriptHash }]) => [
      spendingScriptHash,
      stage,
    ]),
  );
  const named = (data: Data): string[] =>
    typeof data === "string"
      ? [stageOf.get(data) ?? []].flat()
      : Array.isArray(data)
        ? data.flatMap(named)
        : [];
  const next = new Map(
    blueprintApplicationRecords(blueprint).flatMap(
      ({ parameters, appliedScriptCborHex }) => {
        const stage = stageOf.get(
          validatorToScriptHash({
            type: "PlutusV3",
            script: appliedScriptCborHex,
          }),
        );
        return stage === undefined
          ? []
          : [
              [
                stage,
                parameters.flatMap(({ cborHex }) => named(Data.from(cborHex))),
              ] as const,
            ];
      },
    ),
  );
  const routesFrom = (stage: string): string[][] =>
    stage === "settle"
      ? [[]]
      : (next.get(stage) ?? []).flatMap((after) =>
          routesFrom(after).map((route) => [stage, ...route]),
        );
  return cekCoreEntryHashes(stages).map((hash) =>
    routesFrom(stageOf.get(hash)!),
  );
};

const ROOT = new Uint8Array(32);
const OPAQUE: MidgardCekDirectValueWitness = { kind: "opaque", root: ROOT };
const NO_MATERIAL: MidgardCekSemanticBuiltinWitness = {
  dataNodes: [],
  listNodes: [],
  pairNodes: [],
  scalarPreimages: [],
};

/** Every builtin tag, and some past the last. */
const TAGS = Array.from({ length: 128 }, (_, tag) => BigInt(tag));

/** The witnesses our executor writes in the runs of the builtin step
 * scripts and the loop scripts, and for every tag a direct builtin's, a
 * semantic builtin's and a semantic failure's witness over no operands, of
 * which our planner routes by the tag alone. */
const witnesses = (): readonly MidgardCekCoreStepWitness[] => [
  ...[
    ...Object.values(CEK_BUILTIN_STEP_SCRIPTS),
    PRODUCT_SCRIPT,
    SERIALISE_SCRIPT,
    WRITE_BITS_SCRIPT,
    WRITE_BITS_FAILURE_SCRIPT,
    DECODE_UTF8_FAILURE_SCRIPT,
  ].flatMap(runWitnesses),
  ...TAGS.flatMap((tag): MidgardCekCoreStepWitness[] => [
    { kind: "executeBuiltinDirect", tag, arguments: [], result: OPAQUE },
    {
      kind: "executeBuiltinSemantic",
      tag,
      arguments: [],
      result: OPAQUE,
      material: NO_MATERIAL,
    },
    {
      kind: "executeBuiltinSemanticFailure",
      tag,
      arguments: [],
      material: NO_MATERIAL,
    },
  ]),
];

/** The routes our planner builds an arm chain through for core group
 * `group` over `pool`, a loop arm once. */
const planned = (
  group: number,
  pool: readonly MidgardCekCoreStepWitness[],
): string[][] => {
  if (!isCekCoreGroup(group)) throw new Error("no such core group");
  const routes = new Map<string, string[]>();
  for (const witness of pool) {
    let holders: readonly string[];
    try {
      ({ holders } = cekCoreArmHops(group, witness, ROOT));
    } catch {
      continue;
    }
    const route = holders.filter((holder, at) => holders[at - 1] !== holder);
    routes.set(route.join(" "), route);
  }
  return [...routes.values()];
};

const sorted = (routes: readonly string[][]) =>
  routes.map((route) => route.join(" ")).sort();

describe("the CEK core's routes", () => {
  const wired = wiring();
  const pool = witnesses();

  it("wires an entry stage for each core group, from which every route takes as many hops as the settle counts", () => {
    expect(wired).toHaveLength(CEK_CORE_ARM_HOP_COUNTS.length);
    wired.forEach((routes, group) => {
      expect(routes.length, `group ${group.toString()}`).toBeGreaterThan(0);
      for (const route of routes)
        expect(BigInt(route.length), route.join(" ")).toBe(
          CEK_CORE_ARM_HOP_COUNTS[group],
        );
    });
    // Group 5's budget hop routes a builtin to each of its stages, and
    // group 7's roots to the material hop, the staged failure hop or the
    // writeBits loop.
    expect(wired[5]).toHaveLength(13);
    expect(sorted(wired[7]!)).toEqual([
      "semanticFailureRoots semanticFailureMaterial",
      "semanticFailureRoots stagedFailure",
      "semanticFailureRoots stagedWriteBitsLoop",
    ]);
  });

  it("are, for each core group, the routes our planner builds an arm chain through over the pool: each wired route is planned and each planned route is wired", () => {
    wired.forEach((routes, group) =>
      expect(sorted(planned(group, pool)), `group ${group.toString()}`).toEqual(
        sorted(routes),
      ),
    );
  });
});
