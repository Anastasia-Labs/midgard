/** The CEK core chain (cek-semantic-chain), as one table: each stage's role
 * and the stages it hands a step on to, the entry stage of each core group,
 * and the group a core step's mode and arm witness select. The stage
 * titles, the settle's hop counts and the order the stages are applied in
 * are derived from the table once, here; `buildCekCoreStages` (cek-core.ts)
 * applies it over a blueprint. The Aiken routing stays the authority: the
 * `cek-core-routing-v1` golden holds `cekCoreGroup` to it. */
import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";

/**
 * What each hop writes into the bound it carries on, past the binder: a
 * stage whose row names `facts` writes them, and every other hop keeps the
 * last written, none before the first.
 *
 * - `builtinRoots`: a direct builtin's argument, result and builtin roots;
 * - `semanticArguments`: a semantic builtin's compacted arguments with their
 *   roots;
 * - `semanticResult`: those arguments' builtin roots, with the compacted
 *   arguments and result;
 * - `mapStartArguments`: a map conversion's compacted arguments and result;
 * - `mapStartBudget`: those, with the builtin's budget.
 */
export type CekCoreFacts =
  | "builtinRoots"
  | "semanticArguments"
  | "semanticResult"
  | "mapStartArguments"
  | "mapStartBudget";

/** Where group 7's roots route a builtin failure: the material hop, the
 * staged failure hop or the writeBits loop. */
export type CekCoreFailureRoute = "material" | "staged" | "loop";

/**
 * Where a stage hands a step on to, which is what it is applied to before
 * the computation-thread policy:
 *
 * - `settle`: the settle itself, applied to the award hash and the hop
 *   counts;
 * - `next`: one stage, the hop writing `facts` when the row names them;
 * - `semantics`: the semantics list a budget hop picks from by the
 *   builtin's semantics group, in group order;
 * - `branch`: group 7's roots, which take the builtins below `tags` and
 *   route those in a `stagedTags` range (inclusive) to the staged failure
 *   hop, `loopTag` among them to the writeBits loop when its input is a
 *   semantic constant of at most `loopInputWords` words, and the rest to the
 *   material hop.
 */
export type CekCoreSuccessor<Role extends string = CekCoreRole> =
  | Readonly<{ kind: "settle" }>
  | Readonly<{ kind: "next"; to: Role; facts?: CekCoreFacts }>
  | Readonly<{ kind: "semantics"; to: readonly Role[] }>
  | Readonly<{
      kind: "branch";
      material: Role;
      staged: Role;
      loop: Role;
      tags: bigint;
      stagedTags: readonly (readonly [bigint, bigint])[];
      loopTag: bigint;
      loopInputWords: bigint;
    }>;

const SETTLES = { kind: "next", to: "settle" } as const;

const ROWS = {
  settle: { kind: "settle" },
  compute: SETTLES,
  machine: SETTLES,
  caseConstant: SETTLES,
  mapConversion: SETTLES,
  directScalar: SETTLES,
  directStructured: SETTLES,
  directScalarBudget: { kind: "semantics", to: ["directScalar"] },
  directStructuredBudget: { kind: "semantics", to: ["directStructured"] },
  directScalarRoots: {
    kind: "next",
    to: "directScalarBudget",
    facts: "builtinRoots",
  },
  directStructuredRoots: {
    kind: "next",
    to: "directStructuredBudget",
    facts: "builtinRoots",
  },
  semanticPair: SETTLES,
  semanticListConstruct: SETTLES,
  semanticListSelect: SETTLES,
  semanticChoose: SETTLES,
  semanticDataConstruct: SETTLES,
  semanticDataScalar: SETTLES,
  semanticDataMisc: SETTLES,
  stagedBytes: SETTLES,
  stagedInteger: SETTLES,
  stagedProduct: SETTLES,
  semanticSerialiseLoop: SETTLES,
  stagedFailure: SETTLES,
  stagedConversion: SETTLES,
  stagedWriteBitsLoop: SETTLES,
  semanticBudget: {
    kind: "semantics",
    to: [
      "semanticPair",
      "semanticListConstruct",
      "semanticListSelect",
      "semanticChoose",
      "semanticDataConstruct",
      "semanticDataScalar",
      "semanticDataMisc",
      "stagedBytes",
      "stagedInteger",
      "stagedProduct",
      "semanticSerialiseLoop",
      "stagedConversion",
      "stagedWriteBitsLoop",
    ],
  },
  semanticResult: {
    kind: "next",
    to: "semanticBudget",
    facts: "semanticResult",
  },
  semanticRoots: {
    kind: "next",
    to: "semanticResult",
    facts: "semanticArguments",
  },
  mapStartNodes: SETTLES,
  mapStartBudget: {
    kind: "next",
    to: "mapStartNodes",
    facts: "mapStartBudget",
  },
  mapStartRoots: {
    kind: "next",
    to: "mapStartBudget",
    facts: "mapStartArguments",
  },
  semanticFailureMaterial: SETTLES,
  semanticFailureRoots: {
    kind: "branch",
    material: "semanticFailureMaterial",
    staged: "stagedFailure",
    loop: "stagedWriteBitsLoop",
    tags: 87n,
    stagedTags: [
      [0n, 25n],
      [52n, 53n],
      [71n, 86n],
    ],
    loopTag: 80n,
    loopInputWords: 512n,
  },
  blsFinal: SETTLES,
  blsRoots: { kind: "next", to: "blsFinal" },
  blsBudget: { kind: "next", to: "blsRoots" },
  failureBudget: SETTLES,
  failureKnown: { kind: "next", to: "failureBudget" },
  failureBlsUncompress: { kind: "next", to: "failureBudget" },
  typeFailureKinds: SETTLES,
  typeFailureRoots: { kind: "next", to: "typeFailureKinds" },
  pv11Roots: { kind: "next", to: "pv11Budget", facts: "builtinRoots" },
  pv11Budget: { kind: "next", to: "pv11Semantics" },
  pv11Semantics: SETTLES,
} as const;

/** A CEK core stage, by its role in the chain. */
export type CekCoreRole = keyof typeof ROWS;

/** Each stage's successor, one row a role. */
export const CEK_CORE_CHAIN = Object.freeze(ROWS) satisfies Readonly<
  Record<CekCoreRole, CekCoreSuccessor>
>;

/** The entry stage of each core group, by group: the stage the binder hands
 * a step of that group to. */
export const CEK_CORE_GROUP_ENTRIES = Object.freeze([
  "compute",
  "machine",
  "mapConversion",
  "directScalarRoots",
  "directStructuredRoots",
  "semanticRoots",
  "mapStartRoots",
  "semanticFailureRoots",
  "blsBudget",
  "failureKnown",
  "typeFailureRoots",
  "caseConstant",
  "failureBlsUncompress",
  "pv11Roots",
  "pv11Budget",
] as const satisfies readonly CekCoreRole[]);

/** Every role, in table order. */
export const CEK_CORE_ROLES = Object.freeze(Object.keys(ROWS) as CekCoreRole[]);

/** The stages `successor` names. */
export const cekCoreSuccessors = (
  successor: CekCoreSuccessor,
): readonly CekCoreRole[] => {
  switch (successor.kind) {
    case "settle":
      return [];
    case "next":
      return [successor.to];
    case "semantics":
      return successor.to;
    case "branch":
      return [successor.material, successor.staged, successor.loop];
  }
};

/** The deployment manifest's member for each role. */
export const cekCoreContractName = <Role extends CekCoreRole>(role: Role) =>
  `validationTraceDisputeCekCore${role[0]!.toUpperCase()}${role.slice(1)}` as `validationTraceDisputeCekCore${Capitalize<Role>}`;

/** The validator each role applies: its manifest member's. */
export const CEK_CORE_STAGE_TITLES: Readonly<Record<CekCoreRole, string>> =
  Object.freeze(
    Object.fromEntries(
      CEK_CORE_ROLES.map((role) => [
        role,
        deployedTitle(cekCoreContractName(role)),
      ]),
    ) as Record<CekCoreRole, string>,
  );

/** The roles with each role after the stages it names, so that a stage is
 * applied after its successors; throws on a cycle. */
const appliedOrder = (): readonly CekCoreRole[] => {
  const applied = new Set<CekCoreRole>();
  const open = new Set<CekCoreRole>();
  const visit = (role: CekCoreRole): void => {
    if (applied.has(role)) return;
    if (open.has(role))
      throw new Error(`the CEK core chain loops through ${role}`);
    open.add(role);
    cekCoreSuccessors(CEK_CORE_CHAIN[role]).forEach(visit);
    open.delete(role);
    applied.add(role);
  };
  CEK_CORE_ROLES.forEach(visit);
  return Object.freeze([...applied]);
};

/** The order `buildCekCoreStages` applies the stages in. */
export const CEK_CORE_APPLIED_ORDER = appliedOrder();

/** Each role's hops to the settle, the settle's own zero; throws for a stage
 * whose successors are not all as far from it. */
const hopsToSettle = (): ReadonlyMap<CekCoreRole, number> => {
  const hops = new Map<CekCoreRole, number>();
  for (const role of CEK_CORE_APPLIED_ORDER) {
    const after = new Set(
      cekCoreSuccessors(CEK_CORE_CHAIN[role]).map((next) => hops.get(next)),
    );
    if (after.size > 1)
      throw new Error(`${role}'s successors are not as far from the settle`);
    const [rest] = after;
    hops.set(role, rest === undefined ? 0 : rest + 1);
  }
  return hops;
};

const HOPS_TO_SETTLE = hopsToSettle();

/** The hops of each core group's chain, by group, which the settle is
 * applied to. */
export const CEK_CORE_ARM_HOP_COUNTS: readonly bigint[] = Object.freeze(
  CEK_CORE_GROUP_ENTRIES.map((role) => BigInt(HOPS_TO_SETTLE.get(role)!)),
);

/** The choices a step makes along its group's chain: the semantics group a
 * semantics list picks by, and the route group 7's roots take. */
export type CekCoreChoice = Readonly<{
  semantics?: number;
  failure?: CekCoreFailureRoute;
}>;

/** The stage `successor` hands a step on to under `choice`: null at the
 * settle or at a pick `choice` leaves open, and undefined for a semantics
 * group the list has no stage for. */
const nextStage = (
  successor: CekCoreSuccessor,
  choice: CekCoreChoice,
): CekCoreRole | null | undefined => {
  switch (successor.kind) {
    case "settle":
      return null;
    case "next":
      return successor.to;
    case "semantics":
      if (successor.to.length === 1) return successor.to[0];
      return choice.semantics === undefined
        ? null
        : successor.to[choice.semantics];
    case "branch":
      return choice.failure === undefined ? null : successor[choice.failure];
  }
};

/**
 * The stages a step of core group `group` visits from its entry towards the
 * settle, in order, a loop once: a semantics list of one stage is followed,
 * a longer one by `choice.semantics` and group 7's roots by
 * `choice.failure`, and the route ends before the settle or at a pick
 * `choice` leaves open. Undefined for no such group, or a semantics group
 * the list has no stage for.
 */
export const cekCoreRoute = (
  group: number,
  choice: CekCoreChoice = {},
): readonly Exclude<CekCoreRole, "settle">[] | undefined => {
  const route: Exclude<CekCoreRole, "settle">[] = [];
  let stage: CekCoreRole | null | undefined = CEK_CORE_GROUP_ENTRIES[group];
  while (stage !== null && stage !== "settle") {
    if (stage === undefined) return undefined;
    route.push(stage);
    stage = nextStage(CEK_CORE_CHAIN[stage], choice);
  }
  return Object.freeze(route);
};

const FAILURE_BRANCH = CEK_CORE_CHAIN.semanticFailureRoots;

/** The builtins group 7's roots route to the staged failure hop. */
const STAGED_FAILURE_TAGS: ReadonlySet<bigint> = new Set(
  FAILURE_BRANCH.stagedTags.flatMap(([low, high]) =>
    Array.from({ length: Number(high - low) + 1 }, (_, at) => low + BigInt(at)),
  ),
);

/**
 * `semantic_failure_roots` (cek-semantic-chain): where group 7's roots route
 * a failure of builtin `tag` whose first argument is a semantic constant of
 * `inputWords` words (null for any other argument), or undefined for a tag
 * the roots refuse.
 */
export const cekCoreFailureRoute = (
  tag: bigint,
  inputWords: bigint | null,
): CekCoreFailureRoute | undefined => {
  if (tag < 0n || tag >= FAILURE_BRANCH.tags) return undefined;
  if (!STAGED_FAILURE_TAGS.has(tag)) return "material";
  return tag === FAILURE_BRANCH.loopTag &&
    inputWords !== null &&
    inputWords <= FAILURE_BRANCH.loopInputWords
    ? "loop"
    : "staged";
};

const MODE = {
  compute: 0n,
  return: 1n,
  lookup: 2n,
  builtin: 3n,
  caseSelect: 6n,
  caseApply: 7n,
  semanticBuiltin: 8n,
} as const;

/**
 * `core_group(mode, witness)` (cek-semantic-chain): the arm chain a core
 * step of `mode` with the witness of constructor `arm` runs, or undefined
 * for a pair the chain refuses.
 */
export const cekCoreGroup = (
  mode: bigint,
  arm: number,
  builtinTag?: bigint,
): number | undefined => {
  switch (mode) {
    case MODE.compute:
      return (arm >= 0 && arm <= 10) || arm === 40 ? 0 : undefined;
    case MODE.lookup:
      return arm === 11 || arm === 12 ? 1 : undefined;
    case MODE.return:
      return arm === 41 ? 11 : arm >= 13 && arm <= 27 ? 1 : undefined;
    case MODE.caseSelect:
      return arm === 28 ? 1 : undefined;
    case MODE.caseApply:
      return arm === 29 ? 1 : undefined;
    case MODE.semanticBuiltin:
      return arm >= 33 && arm <= 35 ? 2 : undefined;
    case MODE.builtin: {
      if (arm === 38)
        return builtinTag === undefined
          ? undefined
          : builtinTag === 59n || builtinTag === 66n
            ? 12
            : builtinTag > 86n
              ? 14
              : 9;
      if (arm === 30)
        return builtinTag === undefined
          ? undefined
          : builtinTag > 86n
            ? 13
            : builtinTag >= 29n &&
                builtinTag <= 69n &&
                builtinTag !== 52n &&
                builtinTag !== 53n
              ? 4
              : 3;
      const groups: Readonly<Record<number, number>> = {
        31: 5,
        32: 6,
        36: 7,
        37: 8,
        39: 10,
      };
      return groups[arm];
    }
    default:
      return undefined;
  }
};
