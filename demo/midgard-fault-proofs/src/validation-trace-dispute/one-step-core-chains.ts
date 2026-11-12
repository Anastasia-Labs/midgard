/** The arm chain each CEK core group's step settles through
 * (cek-semantic-chain): the holders a thread of ours visits from the CEK
 * core step binder to the settle, in order, and the facts each hop writes
 * into the bound it carries on. Every hop is redeemed by the arm witness the
 * binder bound and writes the bound with its progress one higher; only the
 * facts differ, and those are a function of the witness alone.
 *
 * The routes, which hop writes which kind of facts (`CekCoreFacts`) and where
 * group 7's roots route a builtin failure (`cekCoreFailureRoute`) are the
 * rows of the CEK core chain table (midgard-sdk contracts/cek-core-chain.ts),
 * read once here. This module picks group 5's semantic stage by the tag and
 * group 7's route by the tag and the size of the first argument, and writes
 * each kind of facts from the witness. Group 7's roots refuse a builtin past
 * the protocol version 10 ones, and only a writeBits failure takes the
 * writeBits loop.
 *
 * A staged builtin step (arm 31, semantics groups 7 to 9 and 11: a
 * byte-string, string, integer or conversion builtin whose operands do not
 * fit the direct carriage bound) settles through the same group-5 chain, its
 * last stage the staged stage `midgardCekSemanticGroup` picks, with the same
 * facts. Its material names the staging outputs that hold its staged
 * operands; those are read as reference inputs, so the driver stages the
 * operands, waits for their confirmation depth and fills the output
 * references in before the plan commits the step's evidence; a staged
 * builtin failure's (group 7) staged operands are staged and read the same
 * way. The tag also picks how the material decodes
 * (`decodeCekCoreSemanticMaterial`). A tag with no stage leaves the step
 * unresolved, with its reason. `cekCoreStagedSemanticStage` names the staged
 * stage of each staged semantics group (7, 8 and 11).
 *
 * The staged product (group 9), serialise (10, serialiseData) and writeBits
 * (12) stages are loops (one-step-core-loops.ts): the chain visits the stage
 * once per hop of its loop, each visit but the last writing the loop's facts
 * and leaving the progress as it was. The writeBits loop also walks a
 * writeBits failure's indices after group 7's roots, to the hop that opens
 * the index outside the input. A serialiseData step reads R, its result's
 * bytes, from staging outputs the driver stages as it stages a staged step's
 * operands. */
import type { MidgardCekStagedOutRef } from "@al-ft/midgard-core";
import {
  CEK_CORE_ARM_HOP_COUNTS,
  CEK_CORE_CHAIN,
  CEK_CORE_GROUP_ENTRIES,
  CEK_CORE_ROLES,
  cekCoreContractName,
  type CekCoreFacts,
  type CekCoreFailureRoute,
  cekCoreFailureRoute,
  type CekCoreRole,
  cekCoreRoute,
  SemanticBuiltinWitness,
  StagedMaterialV1,
} from "@al-ft/midgard-sdk";
import {
  hashMidgardCekDirectArguments,
  hashMidgardCekDirectValueWitness,
  type MidgardCekCoreStepWitness,
  midgardCekDirectBuiltinBudget,
  midgardCekDirectValueData,
  type MidgardCekDirectValueWitness,
  midgardCekSemanticGroup,
  semanticizeMidgardCekDirectConstant,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  type CekCoreLoopHop,
  cekCoreLoopHops,
  isCekCoreLoopStage,
} from "./one-step-core-loops.js";

/** An arm-31 step's material, as the semantic group of its tag reads it. */
export type CekCoreSemanticMaterial =
  | Readonly<{ kind: "semantic"; material: SemanticBuiltinWitness }>
  | Readonly<{ kind: "staged"; material: StagedMaterialV1 }>;

type MaterialDecoder = (cborHex: string) => CekCoreSemanticMaterial;

const stagedMaterial: MaterialDecoder = (cborHex) =>
  Object.freeze({
    kind: "staged",
    material: Data.from(cborHex, StagedMaterialV1),
  });

const semanticMaterial: MaterialDecoder = (cborHex) =>
  Object.freeze({
    kind: "semantic",
    material: Data.from(cborHex, SemanticBuiltinWitness),
  });

/** How the material of each semantics group whose step is not a semantic
 * one decodes, one entry a group; any other group's is the semantic
 * witness. */
const MATERIAL_DECODERS: Readonly<Partial<Record<number, MaterialDecoder>>> =
  Object.freeze({
    7: stagedMaterial,
    8: stagedMaterial,
    9: stagedMaterial,
    11: stagedMaterial,
    12: stagedMaterial,
  });

/**
 * The material of an arm-31 step of builtin `tag`: a staged step's
 * `StagedMaterialV1` for a tag of semantics group 7, 8, 9, 11 or 12, and
 * the semantic witness for any other semantics group. Both shapes are a constructor 0, so
 * the tag picks the schema; throws for a tag no semantics group takes and for
 * material of the other shape.
 */
export const decodeCekCoreSemanticMaterial = (
  tag: bigint,
  material: Data,
): CekCoreSemanticMaterial => {
  const group = midgardCekSemanticGroup(tag);
  if (group === null)
    throw new Error(`builtin ${tag.toString()} has no semantics group`);
  return (MATERIAL_DECODERS[group] ?? semanticMaterial)(Data.to(material));
};

/** The stages of `route`, none of which is the settle. */
const routeOf = (route: readonly CekCoreRole[] | undefined) => {
  if (route === undefined) throw new Error("the CEK core chain has no route");
  return route as readonly ValidationTraceArmHolder[];
};

/** Each core group's route up to the stage its step's witness picks (group
 * 5's through its budget hop), group 7's through the material hop. */
const CHAINS = Object.freeze(
  CEK_CORE_GROUP_ENTRIES.map((_, group) =>
    routeOf(cekCoreRoute(group, { failure: "material" })),
  ),
);

/** A CEK core group: the binder hands its step to the group's entry
 * stage. */
export type CekCoreGroup =
  Exclude<
    keyof typeof CEK_CORE_GROUP_ENTRIES,
    keyof (readonly unknown[])
  > extends infer Group
    ? Group extends `${infer Index extends number}`
      ? Index
      : never
    : never;

/** An arm holder: a CEK core stage a thread visits between the binder and
 * the settle. */
export type ValidationTraceArmHolder = Exclude<CekCoreRole, "settle">;

/** The semantic stage of group 5 each `semantic_group(tag)` selects, in its
 * order (cek-semantic-chain), and group 5's route through each. */
const SEMANTIC_STAGES = CEK_CORE_CHAIN.semanticBudget.to;

const SEMANTIC_ROUTES = Object.freeze(
  SEMANTIC_STAGES.map((_, semantics) =>
    routeOf(cekCoreRoute(5, { semantics })),
  ),
);

/** The staged stage of each staged semantics group that is not a loop (7, 8
 * and 11): a group whose material is a staged step's. */
const STAGED_SEMANTIC_STAGES: ReadonlyMap<number, ValidationTraceArmHolder> =
  new Map(
    SEMANTIC_STAGES.flatMap((stage, group) =>
      MATERIAL_DECODERS[group] === stagedMaterial && !isCekCoreLoopStage(stage)
        ? [[group, stage] as const]
        : [],
    ),
  );

/** Staging hook (arm 31): the staged stage the budget hop routes semantic
 * builtin `tag` to on the staged route, or null for a tag without one. */
export const cekCoreStagedSemanticStage = (
  tag: bigint,
): ValidationTraceArmHolder | null =>
  STAGED_SEMANTIC_STAGES.get(midgardCekSemanticGroup(tag) ?? -1) ?? null;

/** Group 7's route through each stage its roots go on to. */
const FAILURE_ROUTES = Object.freeze({
  material: routeOf(cekCoreRoute(7, { failure: "material" })),
  staged: routeOf(cekCoreRoute(7, { failure: "staged" })),
  loop: routeOf(cekCoreRoute(7, { failure: "loop" })),
} satisfies Record<CekCoreFailureRoute, readonly ValidationTraceArmHolder[]>);

/** Every arm holder, each once. */
export const CEK_CORE_ARM_HOLDERS: readonly ValidationTraceArmHolder[] =
  Object.freeze([
    ...new Set([
      ...CHAINS.flat(),
      ...SEMANTIC_STAGES,
      ...Object.values(FAILURE_ROUTES).flat(),
    ]),
  ]);

const ARM_HOLDERS: ReadonlySet<string> = new Set(CEK_CORE_ARM_HOLDERS);

export const isCekCoreGroup = (group: number): group is CekCoreGroup =>
  Number.isInteger(group) &&
  group >= 0 &&
  group < CEK_CORE_GROUP_ENTRIES.length;

export const isValidationTraceArmHolder = (
  holder: string,
): holder is ValidationTraceArmHolder => ARM_HOLDERS.has(holder);

/** The core groups whose arms are the builtin chains: those of more than one
 * hop. */
export const isCekCoreBuiltinGroup = (group: CekCoreGroup): boolean =>
  CEK_CORE_ARM_HOP_COUNTS[group]! > 1n;

/** The single-hop arms' holders (groups 0, 1, 2 and 11), in group order. */
export const CEK_CORE_SINGLE_HOP_ARM_HOLDERS = Object.freeze(
  CHAINS.filter((_, group) => CEK_CORE_ARM_HOP_COUNTS[group] === 1n).map(
    ([holder]) => holder!,
  ),
) as readonly (typeof CEK_CORE_GROUP_ENTRIES)[0 | 1 | 2 | 11][];

/** The arm holders of the builtin chains, which a deployment installs
 * apart from the single-hop arms. */
export type ValidationTraceBuiltinArmHolder = Exclude<
  ValidationTraceArmHolder,
  (typeof CEK_CORE_SINGLE_HOP_ARM_HOLDERS)[number]
>;

const SINGLE_HOP: ReadonlySet<string> = new Set(
  CEK_CORE_SINGLE_HOP_ARM_HOLDERS,
);

export const CEK_CORE_BUILTIN_ARM_HOLDERS = Object.freeze(
  CEK_CORE_ARM_HOLDERS.filter((holder) => !SINGLE_HOP.has(holder)),
) as readonly ValidationTraceBuiltinArmHolder[];

/** The deployment manifest's name for each arm holder. */
export const CEK_CORE_ARM_CONTRACT_NAMES = Object.freeze(
  Object.fromEntries(
    CEK_CORE_ARM_HOLDERS.map((holder) => [holder, cekCoreContractName(holder)]),
  ) as Record<ValidationTraceArmHolder, string>,
);

/** The facts each stage whose row names them writes. */
const FACTS_OF: ReadonlyMap<CekCoreRole, CekCoreFacts> = new Map(
  CEK_CORE_ROLES.flatMap((role) => {
    const row = CEK_CORE_CHAIN[role];
    return "facts" in row ? [[role, row.facts] as const] : [];
  }),
);

/** The hops of a core step's arm chain: the holders in order, a loop arm
 * once per hop of its loop, and the facts' CBOR each writes; for each visit
 * of a loop arm, its hop (one-step-core-loops.ts), and none for any other
 * hop. */
export type CekCoreArmHops = Readonly<{
  holders: readonly ValidationTraceArmHolder[];
  facts: readonly string[];
  loops?: readonly (CekCoreLoopHop | undefined)[];
}>;

const NO_FACTS: Data = new Constr(0, []);

const cbor = (data: Data): string => Data.to(data);

const valueData = (value: MidgardCekDirectValueWitness): Data =>
  midgardCekDirectValueData(value) as Data;

const compacted = (values: readonly MidgardCekDirectValueWitness[]) =>
  values.map(semanticizeMidgardCekDirectConstant);

const valuesData = (values: readonly MidgardCekDirectValueWitness[]): Data[] =>
  values.map(valueData);

/** `BuiltinRoots { arguments_root, arguments_count, result_root,
 * builtin_root }`. */
const builtinRoots = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  builtinRoot: Uint8Array,
): Data => {
  const { root, count } = hashMidgardCekDirectArguments(arguments_);
  return new Constr(0, [
    Buffer.from(root).toString("hex"),
    count,
    Buffer.from(hashMidgardCekDirectValueWitness(result)).toString("hex"),
    Buffer.from(builtinRoot).toString("hex"),
  ]);
};

/** A semantic builtin's compacted arguments and result. */
const compactedStep = (witness: MidgardCekCoreStepWitness) => {
  if (!("arguments" in witness) || !("result" in witness))
    throw new Error("a semantic builtin step has another witness");
  return {
    arguments_: compacted(witness.arguments),
    result: semanticizeMidgardCekDirectConstant(witness.result),
  };
};

/** A map conversion's start's compacted arguments, its result's data and
 * the builtin's budget. */
const mapStart = (witness: MidgardCekCoreStepWitness) => {
  if (witness.kind !== "startBuiltinMapConversion")
    throw new Error("a map conversion's start has another witness");
  const arguments_ = compacted(witness.arguments);
  return {
    arguments_: valuesData(arguments_),
    result: valueData(semanticizeMidgardCekDirectConstant(witness.result)),
    budget: midgardCekDirectBuiltinBudget(witness.tag, arguments_),
  };
};

/** How each facts kind (midgard-sdk cek-core-chain.ts) is written, from the
 * arm witness and the builtin root. */
const FACTS: Readonly<
  Record<
    CekCoreFacts,
    (witness: MidgardCekCoreStepWitness, builtinRoot: Uint8Array) => Data
  >
> = Object.freeze({
  builtinRoots: (witness, builtinRoot) => {
    if (witness.kind !== "executeBuiltinDirect")
      throw new Error("a direct builtin step has another witness");
    return builtinRoots(witness.arguments, witness.result, builtinRoot);
  },
  semanticArguments: (witness, builtinRoot) => {
    const { arguments_ } = compactedStep(witness);
    const { root, count } = hashMidgardCekDirectArguments(arguments_);
    return new Constr(0, [
      Buffer.from(root).toString("hex"),
      count,
      Buffer.from(builtinRoot).toString("hex"),
      valuesData(arguments_),
    ]);
  },
  semanticResult: (witness, builtinRoot) => {
    const { arguments_, result } = compactedStep(witness);
    return new Constr(0, [
      builtinRoots(arguments_, result, builtinRoot),
      valuesData(arguments_),
      valueData(result),
    ]);
  },
  mapStartArguments: (witness) => {
    const { arguments_, result } = mapStart(witness);
    return new Constr(0, [arguments_, result, NO_FACTS]);
  },
  mapStartBudget: (witness) => {
    const { arguments_, result, budget } = mapStart(witness);
    return new Constr(0, [
      arguments_,
      result,
      new Constr(0, [budget.cpu, budget.memory]),
    ]);
  },
});

/** The facts after each hop of `holders`: a hop whose stage's row names
 * facts writes them, and every other keeps the last written, none before
 * the first; and the last written. */
const factsAlong = (
  holders: readonly ValidationTraceArmHolder[],
  witness: MidgardCekCoreStepWitness,
  builtinRoot: Uint8Array,
): Readonly<{ facts: readonly string[]; last: Data }> => {
  let last: Data = NO_FACTS;
  let written = cbor(last);
  const facts = holders.map((holder) => {
    const kind = FACTS_OF.get(holder);
    if (kind !== undefined) {
      last = FACTS[kind](witness, builtinRoot);
      written = cbor(last);
    }
    return written;
  });
  return Object.freeze({ facts: Object.freeze(facts), last });
};

/** The builtin an arm-31 witness proves: its tag, or serialiseData's and
 * writeBits' for the loops' own witnesses. */
const semanticTag = (witness: MidgardCekCoreStepWitness): bigint => {
  if (
    witness.kind === "executeBuiltinSemantic" ||
    witness.kind === "executeBuiltinStaged"
  )
    return witness.tag;
  if (witness.kind === "executeBuiltinSerialise") return 51n;
  if (witness.kind === "executeBuiltinWriteBits") return WRITE_BITS_TAG;
  throw new Error("a semantic builtin step has another witness");
};

const WRITE_BITS_TAG = 80n;

/** Group 5's route for an arm-31 witness: the one through the stage its
 * builtin's semantics group selects. */
const semanticRoute = (
  witness: MidgardCekCoreStepWitness,
): readonly ValidationTraceArmHolder[] => {
  // A staged witness is taken here under any tag, as a semantic one is: one
  // whose tag's semantics group is not a staged one is refused later, by
  // the staged verifier (midgard-validation cek-staged-builtin.verify.ts),
  // and our executor writes none (cek-executor.staged-builtins.ts).
  const tag = semanticTag(witness);
  const group = midgardCekSemanticGroup(tag);
  const route = group === null ? undefined : SEMANTIC_ROUTES[group];
  if (route === undefined)
    throw new Error(`semantic builtin ${tag.toString()} has no semantic stage`);
  return route;
};

/** Group 7's route for an arm-36 witness: where its roots route the
 * builtin's failure (`cekCoreFailureRoute`), by its tag and the size of its
 * first argument. Only a writeBits failure takes the writeBits loop, and it
 * takes no other route. */
const failureRoute = (
  witness: MidgardCekCoreStepWitness,
): readonly ValidationTraceArmHolder[] => {
  if (
    witness.kind !== "executeBuiltinSemanticFailure" &&
    witness.kind !== "executeBuiltinStagedFailure" &&
    witness.kind !== "executeBuiltinWriteBitsFailure"
  )
    throw new Error("a semantic failure step has another witness");
  const tag =
    witness.kind === "executeBuiltinWriteBitsFailure"
      ? WRITE_BITS_TAG
      : witness.tag;
  const [input] = witness.arguments;
  const route = cekCoreFailureRoute(
    tag,
    input?.kind === "semanticConstant" ? input.witness.memory : null,
  );
  if (route === undefined)
    throw new Error(`group 7's roots refuse builtin ${tag.toString()}`);
  if (
    (route === "loop") !==
    (witness.kind === "executeBuiltinWriteBitsFailure")
  )
    throw new Error(
      `a semantic failure step group 7's roots route to the ${route} hop has another witness`,
    );
  return FAILURE_ROUTES[route];
};

/** The chain of `holders`, which write `facts`, then the loop arm `stage`
 * once per hop of `loop`, over the arm's facts `inner`: each hop but the
 * last writes the loop's facts, and the last restores `inner`. */
const thenLoop = (
  holders: readonly ValidationTraceArmHolder[],
  facts: readonly string[],
  stage: ValidationTraceArmHolder,
  inner: Data,
  loop: readonly CekCoreLoopHop[],
): CekCoreArmHops =>
  Object.freeze({
    holders: Object.freeze([...holders, ...loop.map(() => stage)]),
    facts: Object.freeze([
      ...facts,
      ...loop.map(({ end, state }) =>
        cbor(
          state === null ? inner : new Constr(1, [inner, BigInt(end), state]),
        ),
      ),
    ]),
    loops: Object.freeze([...holders.map(() => undefined), ...loop]),
  });

/**
 * The hops of the core step of group `group` with the arm witness
 * `witness`, from the machine state whose focus is `builtinRoot`; a
 * serialiseData step reads R from the staging outputs `runs` (one a run, in
 * byte order) when given. Throws for a witness the group's chain does not
 * take.
 */
export const cekCoreArmHops = (
  group: CekCoreGroup,
  witness: MidgardCekCoreStepWitness,
  builtinRoot: Uint8Array,
  runs?: readonly MidgardCekStagedOutRef[],
): CekCoreArmHops => {
  const route =
    group === 5
      ? semanticRoute(witness)
      : group === 7
        ? failureRoute(witness)
        : CHAINS[group]!;
  const stage = route.at(-1)!;
  if (!isCekCoreLoopStage(stage))
    return Object.freeze({
      holders: route,
      facts: factsAlong(route, witness, builtinRoot).facts,
    });
  const before = route.slice(0, -1);
  const { facts, last } = factsAlong(before, witness, builtinRoot);
  return thenLoop(
    before,
    facts,
    stage,
    last,
    cekCoreLoopHops(stage, witness, runs),
  );
};
