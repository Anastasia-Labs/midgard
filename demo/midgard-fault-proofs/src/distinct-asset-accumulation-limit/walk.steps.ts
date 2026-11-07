/** The planner's model of each `distinctAssetAccumulationLimit` fold step:
 * from a step's input state and its batch, the state the validator requires
 * it to write. */
import { decodeMidgardLedgerOutputCommitment } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL as LABEL } from "./contracts.js";
import {
  countOutput,
  inputScheduleIndex,
  mintDeclaredCount,
  type OutputResume,
  outputValueStart,
  spentDescriptor,
} from "./fold.js";

const MAX = SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_MAX_ENTRIES;

/** A position in one field's fold: the item under the cursor, the entries
 * folded so far, a part-counted output's resume point and the flat index of
 * the next mint asset. */
export type FoldPosition = Readonly<{
  index: number;
  entryCount: bigint;
  resume: OutputResume | null;
  mintIndex: bigint;
}>;

export type FoldStep =
  | Readonly<{ kind: "walked"; position: FoldPosition }>
  | Readonly<{ kind: "crosses"; crossing: SDK.ValueFaultPosition }>;

/** Each spent input's descriptor asset count, in field order. */
export const spentEntries = (
  material: DirectScriptMaterial,
): readonly bigint[] =>
  material.fields[0]!.map((item) => {
    const entry = spentDescriptor(material, item);
    if (entry === undefined)
      throw new Error(`${LABEL}: spent descriptor absent`);
    return BigInt(decodeMidgardLedgerOutputCommitment(entry.value).assetCount);
  });

/** `resolve_inputs_v1` over the items `[start, start + size)`. A crossing
 * names the input by its field-0 ordinal; the rank step moves it to its
 * schedule position. */
export const resolveStep = (
  entries: readonly bigint[],
  from: FoldPosition,
  size: number,
):
  | Readonly<{ kind: "walked"; position: FoldPosition }>
  | Readonly<{ kind: "crosses"; ordinal: number; assetIndex: bigint }> => {
  let entryCount = from.entryCount;
  for (let ordinal = from.index; ordinal < from.index + size; ordinal += 1) {
    const unit = entries[ordinal]!;
    if (entryCount + unit > MAX)
      return { kind: "crosses", ordinal, assetIndex: MAX - entryCount };
    entryCount += unit;
  }
  return {
    kind: "walked",
    position: { ...from, index: from.index + size, entryCount },
  };
};

/** The input crossing at its schedule position. */
export const rankedCrossing = (
  material: DirectScriptMaterial,
  ordinal: number,
  assetIndex: bigint,
): SDK.ValueFaultPosition => ({
  kind: "input",
  inputIndex: inputScheduleIndex(material, ordinal),
  assetIndex,
});

/** `outputs_loop`: opening an output costs one unit, each counted policy
 * and each skipped asset one more; the step stops when the field is
 * exhausted or the budget is spent. */
export const outputsStep = (
  items: readonly Uint8Array[],
  from: FoldPosition,
  budget: bigint,
): FoldStep => {
  if (budget <= 0n) throw new Error(`${LABEL}: an output step needs budget`);
  let { index, entryCount, resume } = from;
  let left = budget;
  for (;;) {
    const item = items[index];
    if (item === undefined)
      throw new Error(`${LABEL}: the output walk is already exhausted`);
    const outcome =
      resume === null
        ? countOutput(item, outputValueStart(item), entryCount, left - 1n)
        : countOutput(item, resume, entryCount, left);
    if (outcome.kind === "crosses")
      return {
        kind: "crosses",
        crossing: {
          kind: "output",
          outputIndex: BigInt(index),
          assetIndex: outcome.assetIndex,
        },
      };
    if (outcome.kind === "paused")
      return {
        kind: "walked",
        position: {
          ...from,
          index,
          entryCount: outcome.entryCount,
          resume: outcome.resume,
        },
      };
    index += 1;
    entryCount = outcome.entryCount;
    resume = null;
    left = outcome.budget;
    if (index === items.length || left === 0n)
      return {
        kind: "walked",
        position: { ...from, index, entryCount, resume },
      };
  }
};

/** Every state an output's count can pause at, one unit at a time from its
 * opening, in order. */
export const outputPauses = (
  item: Uint8Array,
  entryCount: bigint,
): readonly Readonly<{ entryCount: bigint; resume: OutputResume }>[] => {
  const pauses: { entryCount: bigint; resume: OutputResume }[] = [];
  let outcome = countOutput(item, outputValueStart(item), entryCount, 0n);
  while (outcome.kind === "paused") {
    pauses.push({ entryCount: outcome.entryCount, resume: outcome.resume });
    outcome = countOutput(item, outcome.resume, outcome.entryCount, 1n);
  }
  return pauses;
};

/** `mint_loop`: the field's end is the derivation's end, checked before the
 * policy count. */
export const mintStep = (
  items: readonly Uint8Array[],
  from: FoldPosition,
  count: number,
): FoldStep | Readonly<{ kind: "done" }> => {
  if (count <= 0) throw new Error(`${LABEL}: a mint step needs a policy`);
  let { index, entryCount, mintIndex } = from;
  for (let left = count; ; left -= 1) {
    if (index === items.length) return { kind: "done" };
    if (left === 0)
      return {
        kind: "walked",
        position: { ...from, index, entryCount, mintIndex },
      };
    const declared = mintDeclaredCount(items[index]!);
    if (entryCount + declared > MAX)
      return {
        kind: "crosses",
        crossing: { kind: "mint", mintIndex: mintIndex + MAX - entryCount },
      };
    entryCount += declared;
    mintIndex += declared;
    index += 1;
  }
};
