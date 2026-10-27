/** The `distinctAssetAccumulationLimit` fold, from the family entry to the
 * terminal's input: the spent inputs (taken, then resolved under the event
 * pre-root), the outputs and the mint, each variable field certified before
 * it is counted. Every step's datum is the one its validator requires. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { grammarPages } from "../direct-purpose-family/custody.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { keyValuePhasMembershipProofs } from "../transition-trace/phas.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL } from "./contracts.js";
import type {
  DistinctAssetAccumulationLimitFieldPlan,
  DistinctAssetAccumulationLimitTransition,
} from "./custody.js";
import { countOutput, outputValueStart, spentDescriptor } from "./fold.js";
import {
  type FoldPosition,
  mintStep,
  outputPauses,
  outputsStep,
  rankedCrossing,
  resolveStep,
  spentEntries,
} from "./walk.steps.js";
import { distinctAssetWire } from "./walk.wire.js";

const LABEL = DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL;

type Transition = DistinctAssetAccumulationLimitTransition;

/** The most work one fold transaction takes: spent inputs per take, output
 * work units per output step, mint policies per mint step, and items per
 * grammar batch. A paged step that exceeds one transaction's budget is taken
 * smaller (`fitDistinctAssetAccumulationLimitPage`). */
export type DistinctAssetAccumulationLimitWalkBatch = Readonly<{
  inputs: number;
  outputs: number;
  mint: number;
  grammar: number;
}>;

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_WALK_BATCH: DistinctAssetAccumulationLimitWalkBatch =
  Object.freeze({ inputs: 8, outputs: 128, mint: 16, grammar: 8 });

/** The fold roles whose step size is chosen per transaction. A spend page is
 * taken by `inputs` and resolved by `inputsResolve`. */
export type DistinctAssetAccumulationLimitPagedRole =
  | "inputs"
  | "outputs"
  | "mint";

const PAGED_FIELD = { inputs: 0, outputs: 2, mint: 5 } as const;

export const isDistinctAssetAccumulationLimitPagedRole = (
  role: string,
): role is DistinctAssetAccumulationLimitPagedRole =>
  Object.hasOwn(PAGED_FIELD, role);

/** One page of a paged role at the size taken: the take and its resolve for
 * spent inputs, one fold transaction otherwise. */
export type DistinctAssetAccumulationLimitPage = Readonly<{
  size: number;
  transitions: readonly Transition[];
}>;

export type DistinctAssetAccumulationLimitWalk = Readonly<{
  /** The fold at the configured batch sizes. */
  transitions: readonly Transition[];
  /** The first refused entry, if any, as the terminal datum records it. */
  crossing: SDK.ValueFaultPosition | null;
  /** The datum the terminal reads. */
  final: string;
  /** The page continuing a thread that holds `datum` at `role`, of `size`
   * units (the configured maximum by default). */
  page: (
    role: DistinctAssetAccumulationLimitPagedRole,
    datum: string,
    size?: number,
  ) => DistinctAssetAccumulationLimitPage;
  /** The transition continuing a thread that holds `datum` at a paged role,
   * at the configured maximum, or the resolve of the take that wrote
   * `datum`. */
  transitionAt: (
    role: DistinctAssetAccumulationLimitPagedRole | "inputsResolve",
    datum: string,
  ) => Transition;
}>;

const START: FoldPosition = {
  index: 0,
  entryCount: 0n,
  resume: null,
  mintIndex: 0n,
};

/** Larger than any output's work: counts a whole output in one call. */
const WHOLE_OUTPUT = 1n << 62n;

/** The fold position at the start of every item of each field, up to the
 * first crossing; a field the fold never reaches has none. */
const itemStarts = (
  material: DirectScriptMaterial,
  entries: readonly bigint[],
): Readonly<Record<0 | 2 | 5, readonly FoldPosition[]>> => {
  const inputs: FoldPosition[] = [START];
  const outputs: FoldPosition[] = [];
  const mint: FoldPosition[] = [];
  const starts = { 0: inputs, 2: outputs, 5: mint };
  let position = START;
  for (let index = 0; index < entries.length; index += 1) {
    const step = resolveStep(entries, position, 1);
    if (step.kind === "crosses") return starts;
    position = step.position;
    inputs.push(position);
  }
  position = { ...position, index: 0 };
  outputs.push(position);
  for (const [index, item] of material.fields[2]!.entries()) {
    const outcome = countOutput(
      item,
      outputValueStart(item),
      position.entryCount,
      WHOLE_OUTPUT,
    );
    if (outcome.kind !== "counted") return starts;
    position = {
      ...position,
      index: index + 1,
      entryCount: outcome.entryCount,
    };
    outputs.push(position);
  }
  position = { ...position, index: 0 };
  mint.push(position);
  for (let index = 0; index < material.fields[5]!.length; index += 1) {
    const step = mintStep(material.fields[5]!, position, 1);
    if (step.kind !== "walked") return starts;
    position = step.position;
    mint.push(position);
  }
  return starts;
};

/** Each spent input's descriptor and its membership under the event
 * pre-root, in field order. */
const inputWitnesses = async (
  material: DirectScriptMaterial,
): Promise<readonly SDK.DistinctAssetInputWitness[]> => {
  const descriptors = material.fields[0]!.map((item) => {
    const entry = spentDescriptor(material, item);
    if (entry === undefined)
      throw new Error(`${LABEL}: spent descriptor absent`);
    return entry;
  });
  const proofs = await keyValuePhasMembershipProofs(
    material.ledger.root,
    descriptors,
  );
  return descriptors.map((entry, index) => ({
    descriptor_cbor: entry.value.toString("hex"),
    proof: proofs[index]!,
  }));
};

export const distinctAssetAccumulationLimitWalkTransitions = async ({
  material,
  prover,
  plan,
  inputsScriptHash,
  batch = DISTINCT_ASSET_ACCUMULATION_LIMIT_WALK_BATCH,
}: {
  readonly material: DirectScriptMaterial;
  readonly prover: string;
  readonly plan: (field: number) => DistinctAssetAccumulationLimitFieldPlan;
  /** The take step's own hash, recorded as the resume hash of each take. */
  readonly inputsScriptHash: string;
  readonly batch?: DistinctAssetAccumulationLimitWalkBatch;
}): Promise<DistinctAssetAccumulationLimitWalk> => {
  const wire = distinctAssetWire({ material, prover, plan });
  const { count } = wire;
  const entries = spentEntries(material);
  const witnesses = await inputWitnesses(material);
  const outputs = plan(2).items;
  const mints = plan(5).items;
  const starts = itemStarts(material, entries);

  type Step = Readonly<{ transition: Transition; after: FoldPosition | null }>;

  const take = (from: FoldPosition, size: number): Transition => ({
    label: `inputs take ${from.index.toString()} (${size.toString()})`,
    role: "inputs",
    next: "inputsResolve",
    datum: wire.pending(inputsScriptHash, from, from.index + size),
    fields: [0],
    redeemer: wire.fieldRedeemer(0, from.index, BigInt(size)),
  });
  const resolve = (from: FoldPosition, size: number): Step => {
    const step = resolveStep(entries, from, size);
    const base = {
      label: `inputs resolve ${from.index.toString()} (${size.toString()})`,
      role: "inputsResolve",
      fields: [],
      redeemer: wire.resolveRedeemer(
        witnesses.slice(from.index, from.index + size),
      ),
    } as const;
    if (step.kind === "crosses")
      return {
        transition: {
          ...base,
          next: "inputRank",
          datum: wire.rank(step.ordinal, step.assetIndex),
        },
        after: null,
      };
    return {
      transition: {
        ...base,
        next: step.position.index === count(0) ? "outputsGrammar" : "inputs",
        datum: wire.walk(0, step.position),
      },
      after: step.position,
    };
  };
  const rank = (datum: string): Transition => {
    const held = Data.from(datum, SDK.DistinctAssetRankDatum).data!;
    const crossing = rankedCrossing(
      material,
      Number(held.input_ordinal),
      held.asset_index,
    );
    if (crossing.kind !== "input")
      throw new Error(`${LABEL}: a rank names an input`);
    return {
      label: "input rank",
      role: "inputRank",
      next: "terminal",
      datum: wire.terminal(crossing),
      fields: [1],
      redeemer: wire.rankRedeemer(crossing.inputIndex - held.input_ordinal),
    };
  };
  const outputsAt = (from: FoldPosition, budget: number): Step => {
    const step = outputsStep(outputs, from, BigInt(budget));
    const base = {
      label: `outputs ${from.index.toString()} (${budget.toString()})`,
      role: "outputs",
      fields: [2],
      redeemer: wire.fieldRedeemer(2, from.index, BigInt(budget)),
    } as const;
    if (step.kind === "crosses")
      return {
        transition: {
          ...base,
          next: "terminal",
          datum: wire.terminal(step.crossing),
        },
        after: null,
      };
    return {
      transition: {
        ...base,
        next: step.position.index === count(2) ? "mintGrammar" : "outputs",
        datum: wire.walk(2, step.position),
      },
      after: step.position,
    };
  };
  const mintAt = (from: FoldPosition, size: number): Step => {
    const step = mintStep(mints, from, size);
    const base = {
      label: `mint ${from.index.toString()} (${size.toString()})`,
      role: "mint",
      fields: [5],
      redeemer: wire.fieldRedeemer(5, from.index, BigInt(size)),
    } as const;
    if (step.kind === "walked")
      return {
        transition: {
          ...base,
          next: "mint",
          datum: wire.walk(5, step.position),
        },
        after: step.position,
      };
    return {
      transition: {
        ...base,
        next: "terminal",
        datum: wire.terminal(step.kind === "crosses" ? step.crossing : null),
      },
      after: null,
    };
  };
  /** A take never runs past the field; a mint step counts at least one
   * policy, and the step at the field's end only closes it. */
  const mintSize = (from: FoldPosition, size: number) =>
    Math.max(1, Math.min(size, count(5) - from.index));
  const pageAt = (
    role: DistinctAssetAccumulationLimitPagedRole,
    from: FoldPosition,
    size: number,
  ): DistinctAssetAccumulationLimitPage => {
    if (role === "inputs") {
      const taken = Math.min(size, count(0) - from.index);
      return {
        size: taken,
        transitions: [take(from, taken), resolve(from, taken).transition],
      };
    }
    if (role === "outputs")
      return { size, transitions: [outputsAt(from, size).transition] };
    const taken = mintSize(from, size);
    return { size: taken, transitions: [mintAt(from, taken).transition] };
  };

  // The planned fold at the configured batch sizes.
  const out: Transition[] = [];
  const next = () => out.at(-1)!.next;
  let position = START;
  out.push({
    label: "entry",
    role: "familyEntry",
    next: count(0) === 0 ? "outputsGrammar" : "inputs",
    datum: wire.walk(0, START),
    fields: [0],
    redeemer: wire.entryRedeemer,
  });
  while (next() === "inputs") {
    const size = Math.min(batch.inputs, count(0) - position.index);
    out.push(take(position, size));
    const step = resolve(position, size);
    out.push(step.transition);
    if (step.after === null) out.push(rank(step.transition.datum));
    else position = step.after;
  }

  /** Opens and certifies a variable field before it is counted. */
  const certify = (
    field: 2 | 5,
    role: "outputsGrammar" | "mintGrammar",
    walking: "outputs" | "mint",
  ) => {
    position = { ...position, index: 0 };
    out.push({
      label: `field ${field.toString()} open`,
      role,
      next: role,
      datum: wire.certifying(field, 0, position),
      fields: [field],
      redeemer: wire.openRedeemer(field),
    });
    const total = count(field);
    for (const [start, budget] of grammarPages(total, batch.grammar)) {
      const base = {
        label: `field ${field.toString()} grammar ${start.toString()}`,
        role,
        fields: [field],
        redeemer: wire.certifyRedeemer(field, start, budget),
      } as const;
      if (start + budget < total)
        out.push({
          ...base,
          next: role,
          datum: wire.certifying(field, start + budget, position),
        });
      else if (total > 0)
        out.push({ ...base, next: walking, datum: wire.walk(field, position) });
      else if (role === "outputsGrammar")
        out.push({
          ...base,
          next: "mintGrammar",
          datum: wire.walk(field, position),
        });
      else out.push({ ...base, next: "terminal", datum: wire.terminal(null) });
    }
  };

  if (next() === "outputsGrammar") {
    certify(2, "outputsGrammar", "outputs");
    while (next() === "outputs") {
      const step = outputsAt(position, batch.outputs);
      out.push(step.transition);
      if (step.after !== null) position = step.after;
    }
  }
  if (next() === "mintGrammar") {
    certify(5, "mintGrammar", "mint");
    while (next() === "mint") {
      const step = mintAt(position, mintSize(position, batch.mint));
      out.push(step.transition);
      if (step.after !== null) position = step.after;
    }
  }
  if (next() !== "terminal")
    throw new Error(`${LABEL}: the fold does not reach the terminal`);
  const final = out.at(-1)!.datum;
  const terminal = Data.from(final, SDK.DistinctAssetTerminalDatum).data!;

  // A thread datum names its page by the walk checkpoint it holds.
  const checkpoints = new Map<number, ReadonlyMap<string, number>>();
  const walkedTo = (cursor: SDK.DirectFieldCursor, field: number): number => {
    if (cursor.field_index !== BigInt(field) || cursor.phase !== "Walking")
      throw new Error(
        `${LABEL}: thread datum does not walk field ${field.toString()}`,
      );
    let index = checkpoints.get(field);
    if (index === undefined) {
      index = new Map(
        Array.from({ length: count(field) + 1 }, (_, at) => [
          plan(field)
            .position("Walking", at)
            .cursor.checkpoint_hash.toLowerCase(),
          at,
        ]),
      );
      checkpoints.set(field, index);
    }
    const at = index.get(cursor.checkpoint_hash.toLowerCase());
    if (at === undefined)
      throw new Error(`${LABEL}: thread datum names no walk checkpoint`);
    return at;
  };
  const same = (left: string, right: string) =>
    left.toLowerCase() === right.toLowerCase();
  /** The fold position a thread datum holds, refused unless the planned fold
   * reaches exactly that state and the role still has work there. */
  const heldPosition = (
    role: DistinctAssetAccumulationLimitPagedRole,
    datum: string,
  ): FoldPosition => {
    const field = PAGED_FIELD[role];
    const held = Data.from(datum, SDK.DistinctAssetWalkDatum).data;
    if (held === null) throw new Error(`${LABEL}: thread datum holds no walk`);
    const index = walkedTo(held.cursor, field);
    const before = starts[field][index];
    const open = role === "mint" || index < count(field);
    const candidates =
      before === undefined || !open
        ? []
        : role === "outputs"
          ? [
              before,
              ...outputPauses(outputs[index]!, before.entryCount).map(
                (pause) => ({
                  ...before,
                  entryCount: pause.entryCount,
                  resume: pause.resume,
                }),
              ),
            ]
          : [before];
    const found = candidates.find((candidate) =>
      same(wire.walk(field, candidate), datum),
    );
    if (found === undefined)
      throw new Error(`${LABEL}: thread datum differs from the planned fold`);
    return found;
  };
  const page: DistinctAssetAccumulationLimitWalk["page"] = (
    role,
    datum,
    size,
  ) => {
    const taken = size ?? batch[role];
    if (!Number.isSafeInteger(taken) || taken < 1)
      throw new Error(`${LABEL}: a ${role} page takes at least one unit`);
    return pageAt(role, heldPosition(role, datum), taken);
  };
  return {
    transitions: out,
    crossing: SDK.distinctAssetCrossingPosition(terminal.crossing),
    final,
    page,
    transitionAt: (role, datum) => {
      if (role !== "inputsResolve") return page(role, datum).transitions[0]!;
      const held = Data.from(datum, SDK.DistinctAssetPendingDatum).data;
      if (held === null)
        throw new Error(`${LABEL}: thread datum holds no pending inputs`);
      const from = starts[0][Number(held.first_ordinal)];
      const size = held.pending.length;
      if (
        from === undefined ||
        size < 1 ||
        !same(take(from, size).datum, datum)
      )
        throw new Error(`${LABEL}: thread datum differs from the planned take`);
      return resolve(from, size).transition;
    },
  };
};
