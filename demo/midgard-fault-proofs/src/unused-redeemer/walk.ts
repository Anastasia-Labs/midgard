/** The field-8 walk of the `unusedRedeemer` thread (`unused-redeemer/walk.ak`),
 * from the family entry to claim start or the terminal: grammar
 * certification, then the pointer walk up to the named redeemer. */
import { readCborUnsigned } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type DirectPurposeTransition,
  grammarPages,
  pages,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import {
  boundIndex,
  DIRECT_PURPOSE_SCAN_BATCH,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";

export type UnusedRedeemerPointer = {
  readonly tag: bigint;
  readonly index: bigint;
};

/** Every field-8 item's pointer, in raw field order. */
export const unusedRedeemerPointers = (
  material: DirectScriptMaterial,
): readonly UnusedRedeemerPointer[] =>
  material.fields[8]!.map((item) => {
    const tag = readCborUnsigned(item, 1, "redeemer purpose tag");
    const index = readCborUnsigned(item, tag.nextOffset, "redeemer index");
    return { tag: tag.value, index: index.value };
  });

const KIND_OF_TAG = new Map<bigint, bigint>([
  [0n, 0n],
  [1n, 1n],
  [3n, 2n],
  [6n, 3n],
]);

/** `coordinate_of_pointer_v1`: a tag with no purpose kind maps to an invalid
 * coordinate, which claim custody resolves as an impossible purpose. */
export const unusedRedeemerCoordinate = (
  pointer: UnusedRedeemerPointer,
): SDK.DirectPurposeCoordinate => ({
  kind: KIND_OF_TAG.get(pointer.tag) ?? -1n,
  index: pointer.index,
});

const samePointer = (
  left: UnusedRedeemerPointer,
  right: UnusedRedeemerPointer,
) => left.tag === right.tag && left.index === right.index;

/** A declared pointer for an index past the field: the walk never reads it. */
const OUT_OF_RANGE_POINTER: UnusedRedeemerPointer = { tag: 0n, index: 0n };

/** What the walk proves about the redeemer at `redeemerIndex`, without
 * custody: its pointer, whether an earlier redeemer carries that pointer, and
 * whether the field reaches the index. */
export const unusedRedeemerWalkResult = (
  material: DirectScriptMaterial,
  redeemerIndex: bigint,
) => {
  if (redeemerIndex < 0n)
    throw new Error("unused-redeemer: redeemer index must be non-negative");
  const pointers = unusedRedeemerPointers(material);
  const reached = redeemerIndex < BigInt(pointers.length);
  const at = reached ? Number(redeemerIndex) : pointers.length;
  const pointer = pointers[at] ?? OUT_OF_RANGE_POINTER;
  const repeated =
    reached &&
    pointers.slice(0, at).some((earlier) => samePointer(earlier, pointer));
  return { pointers, pointer, reached, repeated, at };
};

/** The claim context a walked, unrepeated redeemer hands to claim start. */
export const unusedRedeemerClaimContext = (
  inputs: RouteInputs,
  redeemerIndex: bigint,
  pointer: UnusedRedeemerPointer,
): SDK.DirectClaimContext => ({
  bound: boundIndex(inputs),
  observed: null,
  target: {
    RedeemerPointer: {
      redeemer_index: redeemerIndex,
      coordinate: unusedRedeemerCoordinate(pointer),
    },
  },
});

export type UnusedRedeemerWalk = {
  readonly transitions: readonly DirectPurposeTransition[];
  /** Set when the walk hands the redeemer's pointer to claim custody. */
  readonly context: SDK.DirectClaimContext | null;
  /** Set when the walk alone decides: a repeat, or an index past the field. */
  readonly final: SDK.DirectClaimFinalState | null;
};

/** Entry, grammar pages, walk pages and the finish, for the redeemer at raw
 * field-8 index `redeemerIndex`. */
export const unusedRedeemerWalk = (
  inputs: RouteInputs,
  redeemerIndex: bigint,
): UnusedRedeemerWalk => {
  const { material, plan, prover } = inputs;
  const batch = inputs.batch ?? DIRECT_PURPOSE_SCAN_BATCH;
  const { pointers, pointer, reached, repeated, at } = unusedRedeemerWalkResult(
    material,
    redeemerIndex,
  );
  const count = pointers.length;
  const context = unusedRedeemerClaimContext(inputs, redeemerIndex, pointer);
  const walkDatum = (value: SDK.UnusedRedeemerWalkState) =>
    stepDatum(prover, value, SDK.UnusedRedeemerWalkDatum);
  let state: SDK.UnusedRedeemerWalkState = {
    bound: boundIndex(inputs),
    redeemer_index: redeemerIndex,
    pointer_tag: pointer.tag,
    pointer_index: pointer.index,
    cursor: plan(8).position("Certifying", 0).cursor,
    repeated: false,
    reached: false,
    complete: false,
  };
  const progress =
    (phase: "Certifying" | "Walking", start: number, budget: number) =>
    ({ indices, open }: Parameters<DirectPurposeTransition["redeemer"]>[0]) =>
      ({
        ...indices,
        opening: open(8),
        checkpoint_bytes: plan(8).position(phase, start).checkpointBytes,
        budget: BigInt(budget),
      }) satisfies SDK.DirectProgressArgs;
  const out: DirectPurposeTransition[] = [
    {
      label: "entry walk",
      role: "familyEntry",
      next: "redeemerGrammar",
      datum: walkDatum(state),
      fields: [8],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                redeemer_index: redeemerIndex,
                pointer_tag: pointer.tag,
                pointer_index: pointer.index,
                opening: open(8),
              },
            ],
          },
          SDK.UnusedRedeemerEntryRedeemer,
        ),
    },
  ];
  for (const [start, budget] of grammarPages(count, batch)) {
    const last = start + budget >= count;
    state = {
      ...state,
      cursor: plan(8).position(
        last ? "Walking" : "Certifying",
        last ? 0 : start + budget,
      ).cursor,
      complete: last && count === 0,
    };
    const args = progress("Certifying", start, budget);
    out.push({
      label: `walk grammar ${start.toString()}`,
      role: "redeemerGrammar",
      next: last ? "redeemerScan" : "redeemerGrammar",
      datum: walkDatum(state),
      fields: [8],
      redeemer: (redeemerArgs) =>
        Data.to(
          { Continue: [args(redeemerArgs)] },
          SDK.UnusedRedeemerGrammarRedeemer,
        ),
    });
  }
  // A walk stops reading at its redeemer, so it pages only up to it.
  const walked = reached ? at + 1 : count;
  for (const [start, budget] of pages(walked, batch)) {
    const end = start + budget;
    state = {
      ...state,
      cursor: plan(8).position("Walking", end).cursor,
      repeated:
        state.repeated ||
        pointers
          .slice(start, Math.min(end, at))
          .some((earlier) => samePointer(earlier, pointer)),
      reached: reached && at < end,
      complete: end === count,
    };
    const args = progress("Walking", start, budget);
    out.push({
      label: `walk scan ${start.toString()}`,
      role: "redeemerScan",
      next: "redeemerScan",
      datum: walkDatum(state),
      fields: [8],
      redeemer: (redeemerArgs) =>
        Data.to(
          { Continue: [{ Scan: { args: args(redeemerArgs) } }] },
          SDK.UnusedRedeemerScanRedeemer,
        ),
    });
  }
  if (state.repeated !== repeated || state.reached !== reached)
    throw new Error("unused-redeemer: planned walk differs from its result");
  const finish = (
    next: DirectPurposeTransition["next"],
    datum: string,
  ): DirectPurposeTransition => ({
    label: "walk finish",
    role: "redeemerScan",
    next,
    datum,
    fields: [],
    redeemer: ({ indices }) =>
      Data.to(
        { Continue: [{ Finish: indices }] },
        SDK.UnusedRedeemerScanRedeemer,
      ),
  });
  if (reached && !repeated)
    return {
      transitions: [
        ...out,
        finish(
          "claimStart",
          stepDatum(prover, context, SDK.DirectClaimStartDatum),
        ),
      ],
      context,
      final: null,
    };
  const final: SDK.DirectClaimFinalState = {
    context,
    claim: reached ? "RedeemerRepeated" : "RedeemerOutOfRange",
  };
  return {
    transitions: [
      ...out,
      finish("terminal", stepDatum(prover, final, SDK.DirectClaimFinalDatum)),
    ],
    context: null,
    final,
  };
};
