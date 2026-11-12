/** The CEK core's routes.
 *
 * At route level: for each core group, the routes from its entry stage to
 * the settle that the SDK wires over the real blueprint (midgard-sdk
 * contracts/cek-core.ts) are the routes our planner builds an arm chain
 * through over a pool of witnesses, so every wired route has a planned
 * route and every planned route is wired. A wired route our planner cannot
 * build is a step an honest challenger cannot prove. The pool holds the
 * witnesses our executor writes in the runs of the builtin step and loop
 * scripts and, for every tag, a direct builtin's, a semantic builtin's and
 * a semantic failure's witness over no operands.
 *
 * Per step: for each arm that routes by the builtin's tag (30, 31, 36 and
 * 38), each tag and, on group 7, each size class of the first argument,
 * our planner builds the route the validators take the step on, in the
 * witness our executor writes there; and each witness our executor writes
 * in the runs takes the route its arm, tag and size select. The route the
 * validators take is the SDK's chain table's under `cekCoreGroup`,
 * `midgardCekSemanticGroup` and `cekCoreFailureRoute`, which the
 * `cek-core-routing-v1` golden holds to the Aiken routing. */
import {
  blueprintApplicationRecords,
  buildCekCoreStages,
  CEK_CORE_ARM_HOP_COUNTS,
  type CekCoreChoice,
  cekCoreEntryHashes,
  cekCoreFailureRoute,
  cekCoreGroup,
  cekCoreRoute,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  type MidgardCekCoreStepWitness,
  type MidgardCekDirectValueWitness,
  type MidgardCekSemanticBuiltinWitness,
  midgardCekSemanticGroup,
  type MidgardCekStagedBuiltinMaterial,
} from "@al-ft/midgard-validation";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  cekCoreArmHops,
  cekCoreStagedSemanticStage,
  isCekCoreGroup,
} from "../src/validation-trace-dispute/one-step-core-chains.js";
import { isCekCoreLoopStage } from "../src/validation-trace-dispute/one-step-core-loops.js";
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
  WRITE_BITS_WIDE_FAILURE_SCRIPT,
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
const NO_STAGED_MATERIAL: MidgardCekStagedBuiltinMaterial = {
  runs: { refs: [], operands: [] },
  sources: [],
};

/** Every builtin tag, and some past the last. */
const TAGS = Array.from({ length: 128 }, (_, tag) => BigInt(tag));

/** The witnesses our executor writes in the runs of the builtin step
 * scripts and the loop scripts. */
const runPool = (): readonly MidgardCekCoreStepWitness[] =>
  [
    ...Object.values(CEK_BUILTIN_STEP_SCRIPTS),
    PRODUCT_SCRIPT,
    SERIALISE_SCRIPT,
    WRITE_BITS_SCRIPT,
    WRITE_BITS_FAILURE_SCRIPT,
    WRITE_BITS_WIDE_FAILURE_SCRIPT,
    DECODE_UTF8_FAILURE_SCRIPT,
  ].flatMap(runWitnesses);

/** The run pool, and for every tag a direct builtin's, a semantic builtin's
 * and a semantic failure's witness over no operands. */
const witnesses = (): readonly MidgardCekCoreStepWitness[] => [
  ...runPool(),
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

/** The route of `holders`, a loop arm once. */
const routeOf = (holders: readonly string[]): string[] =>
  holders.filter((holder, at) => holders[at - 1] !== holder);

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
    const route = routeOf(holders);
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

/** The route our planner builds for a step of core group `group` with
 * `witness`, or undefined when it refuses it. */
const plannedRoute = (
  group: number | undefined,
  witness: MidgardCekCoreStepWitness,
): string[] | undefined => {
  if (group === undefined || !isCekCoreGroup(group)) return undefined;
  try {
    return routeOf(cekCoreArmHops(group, witness, ROOT).holders);
  } catch {
    return undefined;
  }
};

/** The first argument's words when it is a semantic constant, which group
 * 7's roots route writeBits by. */
const inputWords = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): bigint | null => {
  const [input] = arguments_;
  return input?.kind === "semanticConstant" ? input.witness.memory : null;
};

/** The core group and the choice along its chain the validators take a
 * builtin mode step with `witness` on, by its arm, tag and size; undefined
 * for a witness that routes by none of them. */
const validatorChoice = (
  witness: MidgardCekCoreStepWitness,
):
  | Readonly<{ group: number | undefined; choice?: CekCoreChoice | null }>
  | undefined => {
  const semantics = (tag: bigint) => {
    const group = midgardCekSemanticGroup(tag);
    return { group: 5, choice: group === null ? null : { semantics: group } };
  };
  const failure = (
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
  ) => {
    const route = cekCoreFailureRoute(tag, inputWords(arguments_));
    return {
      group: cekCoreGroup(3n, 36, tag),
      choice: route === undefined ? null : { failure: route },
    };
  };
  const { kind } = witness;
  if (kind === "executeBuiltinDirect")
    return { group: cekCoreGroup(3n, 30, witness.tag) };
  if (kind === "executeBuiltinFailure")
    return { group: cekCoreGroup(3n, 38, witness.tag) };
  if (kind === "executeBuiltinSemantic" || kind === "executeBuiltinStaged")
    return semantics(witness.tag);
  if (kind === "executeBuiltinSerialise") return semantics(51n);
  if (kind === "executeBuiltinWriteBits") return semantics(80n);
  if (
    kind === "executeBuiltinSemanticFailure" ||
    kind === "executeBuiltinStagedFailure"
  )
    return failure(witness.tag, witness.arguments);
  if (kind === "executeBuiltinWriteBitsFailure")
    return failure(80n, witness.arguments);
  return undefined;
};

/** The route the validators take a step with `witness` on, or undefined
 * for one they refuse. */
const validatorRoute = (
  witness: MidgardCekCoreStepWitness,
): string[] | undefined => {
  const chosen = validatorChoice(witness);
  if (chosen?.group === undefined || chosen.choice === null) return undefined;
  const route = cekCoreRoute(chosen.group, chosen.choice);
  return route === undefined ? undefined : [...route];
};

/** A byte string first argument of `words` words, as a semantic constant. */
const semanticBytes = (words: bigint): MidgardCekDirectValueWitness => ({
  kind: "semanticConstant",
  witness: {
    typeCbor: Uint8Array.of(0x9f, 0x01, 0xff),
    payload: { root: ROOT, cborLength: 8n * words + 3n, memory: words },
    memory: words,
  },
});

/** The size classes of a failure's first argument group 7's roots route
 * writeBits by: no semantic constant, the loop's longest input, and one word
 * past it. */
const FIRST_ARGUMENTS: readonly (readonly MidgardCekDirectValueWitness[])[] = [
  [OPAQUE],
  [semanticBytes(512n)],
  [semanticBytes(513n)],
];

/** For each arm that routes by the tag, the witness our executor writes for
 * a step of `tag` over `arguments_` on the route the validators take it on;
 * none on a loop's route, whose witnesses the runs hold. */
const ARM_WITNESSES: Readonly<
  Record<
    number,
    (
      tag: bigint,
      arguments_: readonly MidgardCekDirectValueWitness[],
    ) => MidgardCekCoreStepWitness | undefined
  >
> = {
  30: (tag, arguments_) => ({
    kind: "executeBuiltinDirect",
    tag,
    arguments: arguments_,
    result: OPAQUE,
  }),
  31: (tag, arguments_) => {
    const witness: MidgardCekCoreStepWitness = {
      kind: "executeBuiltinSemantic",
      tag,
      arguments: arguments_,
      result: OPAQUE,
      material: NO_MATERIAL,
    };
    const stage = validatorRoute(witness)?.at(-1);
    if (stage !== undefined && isCekCoreLoopStage(stage)) return undefined;
    return cekCoreStagedSemanticStage(tag) === null
      ? witness
      : {
          kind: "executeBuiltinStaged",
          tag,
          arguments: arguments_,
          result: OPAQUE,
          material: NO_STAGED_MATERIAL,
          stagedOperands: [],
        };
  },
  36: (tag, arguments_) => {
    const route = cekCoreFailureRoute(tag, inputWords(arguments_));
    if (route === "loop") return undefined;
    return route === "staged"
      ? {
          kind: "executeBuiltinStagedFailure",
          tag,
          arguments: arguments_,
          material: NO_STAGED_MATERIAL,
          stagedOperands: [],
        }
      : {
          kind: "executeBuiltinSemanticFailure",
          tag,
          arguments: arguments_,
          material: NO_MATERIAL,
        };
  },
  38: (tag, arguments_) => ({
    kind: "executeBuiltinFailure",
    tag,
    arguments: arguments_,
  }),
};

describe("the CEK core's routes, per arm, tag and size class", () => {
  it.each(Object.keys(ARM_WITNESSES).map(Number))(
    "plans a step of arm %s for each tag and size class through the route the validators take it on, in the witness our executor writes there",
    (arm) => {
      let planned = 0;
      for (const tag of TAGS)
        for (const arguments_ of arm === 36 ? FIRST_ARGUMENTS : [[OPAQUE]]) {
          const witness = ARM_WITNESSES[arm]!(tag, arguments_);
          if (witness === undefined) continue;
          const expected = validatorRoute(witness);
          const at = `tag ${tag.toString()}, ${witness.kind}, ${String(inputWords(arguments_))} words`;
          expect(plannedRoute(cekCoreGroup(3n, arm, tag), witness), at).toEqual(
            expected,
          );
          if (expected !== undefined) planned += 1;
        }
      expect(planned).toBeGreaterThan(0);
    },
  );

  it("plans each step our executor writes in the runs through the route its arm, tag and size select", () => {
    const pool = runPool();
    const kinds = new Set<string>();
    for (const witness of pool) {
      const chosen = validatorChoice(witness);
      if (chosen === undefined) continue;
      kinds.add(witness.kind);
      const expected = validatorRoute(witness);
      expect(expected, witness.kind).toBeDefined();
      expect(plannedRoute(chosen.group, witness), witness.kind).toEqual(
        expected,
      );
    }
    expect([...kinds].sort()).toEqual(
      [
        "executeBuiltinDirect",
        "executeBuiltinFailure",
        "executeBuiltinSemantic",
        "executeBuiltinSemanticFailure",
        "executeBuiltinStaged",
        "executeBuiltinSerialise",
        "executeBuiltinWriteBits",
        "executeBuiltinStagedFailure",
        "executeBuiltinWriteBitsFailure",
      ].sort(),
    );
  });

  it("refuses a failure whose witness the route group 7's roots take it on does not take, and a builtin the roots refuse", () => {
    const stagedOf = (
      tag: bigint,
      arguments_: readonly MidgardCekDirectValueWitness[],
    ): MidgardCekCoreStepWitness => ({
      kind: "executeBuiltinStagedFailure",
      tag,
      arguments: arguments_,
      material: NO_STAGED_MATERIAL,
      stagedOperands: [],
    });
    // writeBits over an input of at most 512 words goes to the loop, which
    // proves the failure from the walk of a writeBits failure's indices.
    expect(
      plannedRoute(7, stagedOf(80n, [semanticBytes(512n)])),
    ).toBeUndefined();
    // A writeBits failure over a longer input goes to the staged failure hop.
    expect(
      plannedRoute(7, {
        kind: "executeBuiltinWriteBitsFailure",
        arguments: [semanticBytes(513n)],
        input: new Uint8Array(4_104),
        indices: [],
      }),
    ).toBeUndefined();
    // The roots take the protocol version 10 builtins only.
    expect(cekCoreFailureRoute(87n, null)).toBeUndefined();
    expect(plannedRoute(7, stagedOf(87n, [OPAQUE]))).toBeUndefined();
  });
});
