/** The arm chain each CEK core group's step settles through
 * (cek-semantic-chain): the holders a thread of ours visits from the CEK
 * core step binder to the settle, in order, and the facts each hop writes
 * into the bound it carries on. Every hop is redeemed by the arm witness the
 * binder bound and writes the bound with its progress one higher; only the
 * facts differ, and those are a function of the witness alone:
 *
 * - groups 0, 1, 2 and 11 settle after one arm (compute, machine, map
 *   conversion, case constant), writing no facts;
 * - groups 3 and 4 (a direct builtin): roots, budget, then the scalar or
 *   structured semantics; the roots write the builtin's argument, result and
 *   builtin roots, which the later hops keep;
 * - group 5 (a semantic builtin): roots, result, budget, then the semantic
 *   stage the builtin's tag selects; the roots write the compacted
 *   arguments with their roots, and the result adds the compacted result;
 * - group 6 (a map conversion's start): roots, budget, nodes; the roots
 *   write the compacted arguments and result, and the budget adds the
 *   builtin's budget;
 * - groups 7 to 10 (a semantic, BLS final, direct or type failure): two or
 *   three hops that write no facts.
 *
 * A staged builtin step (arm 31, semantics groups 7 and 8: a byte-string,
 * string or linear integer builtin whose operands do not fit the direct
 * carriage bound) settles through the same group-5 chain, its last stage the
 * staged bytes or staged integer stage `midgardCekSemanticGroup` picks, with
 * the same facts. Its material names the staging outputs that hold its staged
 * operands; those are read as reference inputs, so the driver stages the
 * operands, waits for their confirmation depth and fills the output
 * references in before the plan commits the step's evidence. The tag also
 * picks how the material decodes (`decodeCekCoreSemanticMaterial`). A tag
 * with no stage leaves the step unresolved, with its reason. */
import { SemanticBuiltinWitness, StagedMaterialV1 } from "@al-ft/midgard-sdk";
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

/** The semantic stage of group 5 each `semantic_group(tag)` selects, in its
 * order (cek-semantic-chain); groups 7 and 8 are the staged stages. A later
 * semantics group's stage is the next entry. */
export const CEK_CORE_SEMANTIC_STAGES = Object.freeze([
  "semanticPair",
  "semanticListConstruct",
  "semanticListSelect",
  "semanticChoose",
  "semanticDataConstruct",
  "semanticDataScalar",
  "semanticDataMisc",
  "stagedBytes",
  "stagedInteger",
] as const);

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
  Object.freeze({ 7: stagedMaterial, 8: stagedMaterial });

/**
 * The material of an arm-31 step of builtin `tag`: a staged step's
 * `StagedMaterialV1` for a tag of semantics group 7 or 8, and the semantic
 * witness for any other semantics group. Both shapes are a constructor 0, so
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

/** Each core group's arm holders before its semantic stage, in order. */
const CHAINS = Object.freeze({
  0: ["compute"],
  1: ["machine"],
  2: ["mapConversion"],
  3: ["directScalarRoots", "directScalarBudget", "directScalar"],
  4: ["directStructuredRoots", "directStructuredBudget", "directStructured"],
  5: ["semanticRoots", "semanticResult", "semanticBudget"],
  6: ["mapStartRoots", "mapStartBudget", "mapStartNodes"],
  7: ["semanticFailureRoots", "semanticFailureMaterial"],
  8: ["blsBudget", "blsRoots", "blsFinal"],
  9: ["failureKnown", "failureBudget"],
  10: ["typeFailureRoots", "typeFailureKinds"],
  11: ["caseConstant"],
} as const);

export type CekCoreGroup = keyof typeof CHAINS;

/** An arm holder: a CEK core stage a thread visits between the binder and
 * the settle. */
export type ValidationTraceArmHolder =
  | (typeof CHAINS)[CekCoreGroup][number]
  | (typeof CEK_CORE_SEMANTIC_STAGES)[number];

/** Every arm holder, each once. */
export const CEK_CORE_ARM_HOLDERS: readonly ValidationTraceArmHolder[] =
  Object.freeze([
    ...new Set([...Object.values(CHAINS).flat(), ...CEK_CORE_SEMANTIC_STAGES]),
  ]);

export const isCekCoreGroup = (group: number): group is CekCoreGroup =>
  Object.hasOwn(CHAINS, group);

export const isValidationTraceArmHolder = (
  holder: string,
): holder is ValidationTraceArmHolder =>
  (CEK_CORE_ARM_HOLDERS as readonly string[]).includes(holder);

/** The core groups whose arms are the builtin chains (3 to 10). */
export const isCekCoreBuiltinGroup = (group: CekCoreGroup): boolean =>
  group >= 3 && group <= 10;

/** The arm holders of the builtin chains, which a deployment installs
 * apart from the single-hop arms. */
export type ValidationTraceBuiltinArmHolder = Exclude<
  ValidationTraceArmHolder,
  (typeof CHAINS)[0 | 1 | 2 | 11][number]
>;

export const CEK_CORE_BUILTIN_ARM_HOLDERS = Object.freeze(
  CEK_CORE_ARM_HOLDERS.filter(
    (holder) =>
      !([0, 1, 2, 11] as const).some((group) =>
        (CHAINS[group] as readonly string[]).includes(holder),
      ),
  ),
) as readonly ValidationTraceBuiltinArmHolder[];

/** The single-hop arms' holders. */
export const CEK_CORE_SINGLE_HOP_ARM_HOLDERS = Object.freeze(
  ([0, 1, 2, 11] as const).map((group) => CHAINS[group][0]),
);

/** The deployment manifest's name for each arm holder. */
export const CEK_CORE_ARM_CONTRACT_NAMES = Object.freeze(
  Object.fromEntries(
    CEK_CORE_ARM_HOLDERS.map((holder) => [
      holder,
      `validationTraceDisputeCekCore${holder[0]!.toUpperCase()}${holder.slice(1)}`,
    ]),
  ) as Record<ValidationTraceArmHolder, string>,
);

/** The hops of a core step's arm chain: the holders in order, and the
 * facts' CBOR each writes. */
export type CekCoreArmHops = Readonly<{
  holders: readonly ValidationTraceArmHolder[];
  facts: readonly string[];
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

/** The facts after each hop of a chain whose first `written` hops write
 * `written[i]` and the rest keep the last. */
const keptAfter = (
  length: number,
  written: readonly Data[],
): readonly string[] => {
  const facts = written.map(cbor);
  const last = facts.at(-1) ?? cbor(NO_FACTS);
  return Object.freeze(
    Array.from({ length }, (_, index) => facts[index] ?? last),
  );
};

/**
 * The hops of the core step of group `group` with the arm witness
 * `witness`, from the machine state whose focus is `builtinRoot`; throws
 * for a witness the group's chain does not take.
 */
export const cekCoreArmHops = (
  group: CekCoreGroup,
  witness: MidgardCekCoreStepWitness,
  builtinRoot: Uint8Array,
): CekCoreArmHops => {
  if (group === 5) {
    if (
      witness.kind !== "executeBuiltinSemantic" &&
      witness.kind !== "executeBuiltinStaged"
    )
      throw new Error("a semantic builtin step has another witness");
    const stage = midgardCekSemanticGroup(witness.tag);
    const semantic =
      stage === null ? undefined : CEK_CORE_SEMANTIC_STAGES[stage];
    if (semantic === undefined)
      throw new Error(
        `semantic builtin ${witness.tag.toString()} has no semantic stage`,
      );
    const arguments_ = compacted(witness.arguments);
    const result = semanticizeMidgardCekDirectConstant(witness.result);
    const { root, count } = hashMidgardCekDirectArguments(arguments_);
    const rootHex = Buffer.from(root).toString("hex");
    const builtinHex = Buffer.from(builtinRoot).toString("hex");
    return Object.freeze({
      holders: Object.freeze([...CHAINS[5], semantic]),
      facts: keptAfter(4, [
        new Constr(0, [rootHex, count, builtinHex, valuesData(arguments_)]),
        new Constr(0, [
          builtinRoots(arguments_, result, builtinRoot),
          valuesData(arguments_),
          valueData(result),
        ]),
      ]),
    });
  }
  const holders = CHAINS[group];
  if (group === 3 || group === 4) {
    if (witness.kind !== "executeBuiltinDirect")
      throw new Error("a direct builtin step has another witness");
    return Object.freeze({
      holders,
      facts: keptAfter(holders.length, [
        builtinRoots(witness.arguments, witness.result, builtinRoot),
      ]),
    });
  }
  if (group === 6) {
    if (witness.kind !== "startBuiltinMapConversion")
      throw new Error("a map conversion's start has another witness");
    const arguments_ = compacted(witness.arguments);
    const result = valueData(
      semanticizeMidgardCekDirectConstant(witness.result),
    );
    const budget = midgardCekDirectBuiltinBudget(witness.tag, arguments_);
    return Object.freeze({
      holders,
      facts: keptAfter(holders.length, [
        new Constr(0, [valuesData(arguments_), result, NO_FACTS]),
        new Constr(0, [
          valuesData(arguments_),
          result,
          new Constr(0, [budget.cpu, budget.memory]),
        ]),
      ]),
    });
  }
  return Object.freeze({ holders, facts: keptAfter(holders.length, []) });
};
