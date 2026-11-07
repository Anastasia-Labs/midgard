/** The family routes of `executionNativeScriptInvalid` after the shared
 * source walk: the family entry, the shared purpose route to the source
 * lookup, the lookup, and either the paged evaluation of a native source or
 * a final state with no native execution at the coordinate. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type DirectPurposeIndices,
  type DirectPurposeTransition,
  stepDatum,
} from "../direct-purpose-family/custody.js";
import {
  directPurposeSpendEvidence,
  spendRankPass,
} from "../direct-purpose-family/domain-passes.js";
import {
  boundIndex,
  DIRECT_PURPOSE_SCAN_BATCH,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import { directPurposeObservedSelection } from "../direct-purpose-family/purpose-routes.js";
import { directPurposeAt } from "../direct-script-derivation/index.js";
import { executionNativeScriptInvalidAbsentSelection } from "./absent-selection.js";
import type { ExecutionNativeScriptInvalidTransition } from "./custody.js";
import {
  type ExecutionNativeScriptInvalidWalk,
  executionNativeScriptInvalidWalk,
  type ExecutionNativeScriptInvalidWalkBatch,
} from "./walk.js";

type Coordinate = SDK.DirectPurposeCoordinate;
type Final = SDK.ExecutionNativeScriptInvalidFinalState;

/** How the family entry continues: through the shared purpose route at a
 * coordinate, or by proving the recorded coordinate has no native execution
 * (an invalid coordinate, an observer or spend ordinal past its field, or a
 * key-locked spend). */
export type ExecutionNativeScriptInvalidEntry =
  | { readonly Observe: Coordinate }
  | "Contradict"
  | "ObserveAbsent"
  | "SpendAbsent"
  | "SpendKey";

export type ExecutionNativeScriptInvalidRoute = Readonly<{
  /** Kit-role custody from the family entry to the source lookup; empty
   * when the entry decides alone. */
  shared: readonly DirectPurposeTransition[];
  /** Family custody after `shared`, up to the walk or the terminal. */
  local: readonly ExecutionNativeScriptInvalidTransition[];
  walk: ExecutionNativeScriptInvalidWalk | null;
  final: Final;
}>;

const entryRedeemer = (
  action: Data.Static<typeof SDK.ExecutionNativeScriptInvalidEntryActionSchema>,
) =>
  Data.to(
    { Continue: [action] },
    SDK.ExecutionNativeScriptInvalidEntryRedeemer,
  );

const finalDatum = (prover: string, final: Final) =>
  stepDatum(prover, final, SDK.ExecutionNativeScriptInvalidFinalDatum);

/** The coordinate the leaf records as `ExecutionNativeScriptFalse`. */
export const executionNativeScriptInvalidRecorded = (
  bound: SDK.DirectScriptBound,
): Coordinate | null => {
  const reason = bound.subject.rejection_reason;
  return reason !== null &&
    typeof reason === "object" &&
    "ExecutionNativeScriptFalse" in reason
    ? {
        kind: reason.ExecutionNativeScriptFalse.purpose_kind,
        index: reason.ExecutionNativeScriptFalse.purpose_index,
      }
    : null;
};

const requireRecorded = (inputs: RouteInputs): Coordinate => {
  const recorded = executionNativeScriptInvalidRecorded(inputs.material.bound);
  if (recorded === null)
    throw new Error(
      "execution-native-script-invalid: the leaf records no native script coordinate",
    );
  return recorded;
};

const noExecution = (inputs: RouteInputs, coordinate: Coordinate): Final => ({
  bound: inputs.material.bound,
  fact: { NoExecution: { coordinate } },
});

/** The entry decides alone over the recorded coordinate. */
const absentAtEntry = (
  inputs: RouteInputs,
  action: "Contradict" | "ObserveAbsent" | "SpendAbsent",
): ExecutionNativeScriptInvalidRoute => {
  const final = noExecution(inputs, requireRecorded(inputs));
  const field = action === "ObserveAbsent" ? 3 : 0;
  return {
    shared: [],
    local: [
      {
        label: `entry ${action}`,
        role: "familyEntry",
        next: "terminal",
        datum: finalDatum(inputs.prover, final),
        fields: action === "Contradict" ? [] : [field],
        redeemer: ({ indices, open }) =>
          entryRedeemer(
            action === "Contradict"
              ? { Contradict: indices }
              : action === "ObserveAbsent"
                ? { ObserveAbsent: { ...indices, opening: open(field) } }
                : { SpendAbsent: { ...indices, opening: open(field) } },
          ),
      },
    ],
    walk: null,
    final,
  };
};

/** The key-locked spend at the recorded coordinate: opened with its first
 * rank page, ranked over the rest of field 0, finished at its rank. */
const keySpend = async (
  inputs: RouteInputs,
): Promise<ExecutionNativeScriptInvalidRoute> => {
  const { prover } = inputs;
  const coordinate = requireRecorded(inputs);
  if (coordinate.kind !== 0n)
    throw new Error(
      "execution-native-script-invalid: the leaf records no spend",
    );
  const spend = await directPurposeSpendEvidence(inputs, coordinate.index);
  if (spend === null)
    throw new Error("execution-native-script-invalid: the spend is absent");
  const opened = Math.min(
    inputs.batch ?? DIRECT_PURPOSE_SCAN_BATCH,
    inputs.plan(0).items.length,
  );
  const state = (progress: {
    readonly cursor: SDK.DirectFieldCursor;
    readonly rank: bigint;
    readonly complete: boolean;
  }): SDK.ExecutionNativeScriptInvalidSpendRankState => ({
    ...progress,
    subject: spend.item.toString("hex"),
  });
  const datum = (rank: SDK.ExecutionNativeScriptInvalidSpendRankState) =>
    stepDatum(prover, rank, SDK.ExecutionNativeScriptInvalidSpendRankDatum);
  const rank = spendRankPass(inputs, {
    label: "key spend rank",
    role: "spendScan",
    subject: spend.item,
    from: opened,
    state,
    datum,
    redeemer: (args) =>
      Data.to(
        { Continue: [{ Scan: { args } }] },
        SDK.ExecutionNativeScriptInvalidSpendRankRedeemer,
      ),
  });
  if (rank.final.rank !== coordinate.index)
    throw new Error(
      "execution-native-script-invalid: the key-locked spend ranks elsewhere",
    );
  const final = noExecution(inputs, coordinate);
  return {
    shared: [],
    local: [
      {
        label: "entry spend key",
        role: "familyEntry",
        next: "spendKey",
        datum: stepDatum(
          prover,
          boundIndex(inputs),
          SDK.DirectPurposeSelectDatum,
        ),
        fields: [],
        redeemer: ({ indices }) => entryRedeemer({ SpendKey: indices }),
      },
      {
        label: "spend key open",
        role: "spendKey",
        next: "spendRank",
        datum: datum(state(rank.at(opened))),
        fields: [0],
        membership: spend.membership,
        redeemer: (args) =>
          Data.to(
            {
              Continue: [
                {
                  ...args.indices,
                  raw_item_index: spend.rawItemIndex,
                  opening: args.open(0),
                  descriptor_cbor: spend.descriptorCbor,
                  membership: spend.carriage(args),
                  budget: BigInt(opened),
                },
              ],
            },
            SDK.ExecutionNativeScriptInvalidSpendKeyRedeemer,
          ),
      },
      ...rank.transitions.map(
        (transition): ExecutionNativeScriptInvalidTransition => ({
          label: transition.label,
          role: "spendRank",
          next: "spendRank",
          datum: transition.datum,
          fields: transition.fields,
          redeemer: transition.redeemer,
        }),
      ),
      {
        label: "spend rank finish",
        role: "spendRank",
        next: "terminal",
        datum: finalDatum(prover, final),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            { Continue: [{ Finish: indices }] },
            SDK.ExecutionNativeScriptInvalidSpendRankRedeemer,
          ),
      },
    ],
    walk: null,
    final,
  };
};

const lookupRedeemer =
  (
    action: (
      indices: DirectPurposeIndices,
    ) =>
      | { Lookup: DirectPurposeIndices & Record<string, unknown> }
      | { Resolve: DirectPurposeIndices },
  ) =>
  ({ indices }: { readonly indices: DirectPurposeIndices }) =>
    Data.to(
      { Continue: [action(indices)] } as never,
      SDK.DirectClaimAdjudicateRedeemer,
    );

/** The shared purpose route at `coordinate`, its source lookup and, for a
 * native source, the paged evaluation. */
const observed = async (
  inputs: RouteInputs,
  coordinate: Coordinate,
  batch: ExecutionNativeScriptInvalidWalkBatch | undefined,
): Promise<ExecutionNativeScriptInvalidRoute> => {
  const { material, evidence, prover } = inputs;
  const purpose = directPurposeAt(material, coordinate);
  const { selection, transitions } =
    purpose === null
      ? await executionNativeScriptInvalidAbsentSelection(inputs, coordinate)
      : await directPurposeObservedSelection(inputs, coordinate);
  const entry: DirectPurposeTransition = {
    label: "entry observe",
    role: "familyEntry",
    next: "purposeSelect",
    datum: stepDatum(prover, boundIndex(inputs), SDK.DirectPurposeSelectDatum),
    fields: [],
    redeemer: ({ indices }) => entryRedeemer({ Observe: indices }),
  };
  const shared = [entry, ...transitions];
  const none = noExecution(inputs, coordinate);
  if (selection.purpose === null)
    return {
      shared,
      local: [
        {
          label: "lookup resolve",
          role: "sourceLookup",
          next: "terminal",
          datum: finalDatum(prover, none),
          fields: [],
          redeemer: lookupRedeemer((indices) => ({ Resolve: indices })),
        },
      ],
      walk: null,
      final: none,
    };
  const { selected: source, proof } = await evidence.lookup(
    selection.purpose.script_hash,
  );
  const lookup = lookupRedeemer((indices) => ({
    Lookup: { ...indices, source, proof },
  }));
  if (source === null || source.language_tag !== 0n)
    return {
      shared,
      local: [
        {
          label: "lookup no native source",
          role: "sourceLookup",
          next: "terminal",
          datum: finalDatum(prover, none),
          fields: [],
          redeemer: lookup,
        },
      ],
      walk: null,
      final: none,
    };
  const walk = executionNativeScriptInvalidWalk({
    material,
    prover,
    coordinate,
    source: evidence.selected.get(selection.purpose.script_hash)!,
    ...(batch === undefined ? {} : { batch }),
  });
  return {
    shared,
    local: [
      {
        label: "lookup native source",
        role: "sourceLookup",
        next: "signers",
        datum: walk.initial,
        fields: [],
        redeemer: lookup,
      },
    ],
    walk,
    final: walk.final,
  };
};

export const executionNativeScriptInvalidRoute = async (
  inputs: RouteInputs,
  entry: ExecutionNativeScriptInvalidEntry,
  batch?: ExecutionNativeScriptInvalidWalkBatch,
): Promise<ExecutionNativeScriptInvalidRoute> => {
  if (typeof entry === "object")
    return await observed(inputs, entry.Observe, batch);
  if (entry === "SpendKey") return await keySpend(inputs);
  return absentAtEntry(inputs, entry);
};
