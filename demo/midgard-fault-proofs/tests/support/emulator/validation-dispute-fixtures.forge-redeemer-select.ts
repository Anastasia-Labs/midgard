import {
  asArray,
  asBytes,
  buildMidgardValidationTraceTree,
  decodeSingleCbor,
  encodeCbor,
  hashMidgardRedeemerItemProofControl,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import { hashMidgardScriptPurposeLeaf } from "@al-ft/midgard-core/script-proof";
import {
  type DeterministicValidationMachineTrace,
  hashMidgardCekRedeemerContextControl,
  type MidgardCekRedeemerContextControl,
  type ValidationMachineWorkWitness,
} from "@al-ft/midgard-validation";

type Auxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;

/**
 * Replace the disputed step with `auxiliary` and its successor with the
 * state that yields `forgedControl`. Every later state carries a fabricated
 * work root.
 */
const forgeRedeemerFoldStep = ({
  trace,
  disputedLowIndex,
  auxiliary,
  forgedControl,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
  readonly auxiliary: Auxiliary;
  readonly forgedControl: MidgardCekRedeemerContextControl;
}): DeterministicValidationMachineTrace => {
  const successorIndex = disputedLowIndex + 1;
  const adjacent = trace.witnesses[successorIndex]!;
  const work = asArray(decodeSingleCbor(adjacent.cbor), "select successor");
  const nextContext = asArray(
    decodeSingleCbor(asBytes(work[1], "context control")),
    "context control",
  );
  nextContext[9] = hashMidgardCekRedeemerContextControl(forgedControl);
  work[1] = encodeCbor(nextContext);
  const cbor = encodeCbor(work);
  const witnesses = [...trace.witnesses];
  witnesses[disputedLowIndex] = {
    ...trace.witnesses[disputedLowIndex]!,
    auxiliary,
  };
  witnesses[successorIndex] = { ...adjacent, cbor };
  const states = trace.states.map((state, index) =>
    index < successorIndex
      ? state
      : index === successorIndex
        ? {
            ...state,
            workRoot: hashMidgardValidationWorkWitness({
              phase: adjacent.phase,
              programCounter: adjacent.programCounter,
              witnessCbor: cbor,
            }),
          }
        : { ...state, workRoot: Buffer.alloc(32, 0x7c) },
  );
  return {
    ...trace,
    states,
    witnesses,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      trace.verdict,
      states.at(-1)!.rejectionCodeHash,
    ),
  };
};

/**
 * The challenger's forged redeemer select for
 * `cekRedeemerSelectDonorOrdinal`: from the honest low state at
 * `disputedLowIndex` (a select or a skip) it selects what the honest select
 * step at `donorIndex` selected, and its successor is exactly the one that
 * selection yields. The execution leaf at the disputed frontier names some
 * other step, so only `[execution-leaf]` can refuse it.
 */
export const forgeRedeemerSelectTrace = ({
  trace,
  disputedLowIndex,
  donorIndex,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
  readonly donorIndex: number | undefined;
}): DeterministicValidationMachineTrace => {
  const disputed = trace.witnesses[disputedLowIndex]!.auxiliary;
  const donor =
    donorIndex === undefined
      ? undefined
      : trace.witnesses[donorIndex]!.auxiliary;
  // The donor's first item step carries the redeemer control its select
  // produced; its active scan, leaf and purpose depend only on the
  // selected item and purpose, not on the state it was selected from.
  const donorItem =
    donorIndex === undefined
      ? undefined
      : trace.witnesses[donorIndex + 1]!.auxiliary;
  const produced =
    donorItem?.kind === "redeemerItemStep" ? donorItem.redeemerControl : null;
  if (
    (disputed?.kind !== "cekRedeemerContextSelect" &&
      disputed?.kind !== "cekRedeemerContextSkip") ||
    donor?.kind !== "cekRedeemerContextSelect" ||
    produced === null
  )
    throw new Error("redeemer select forgery needs an honest fold step");
  return forgeRedeemerFoldStep({
    trace,
    disputedLowIndex,
    auxiliary: { ...donor, control: disputed.control },
    forgedControl: {
      ...disputed.control,
      activeScanHash: produced.activeScanHash,
      activeRedeemerLeaf: produced.activeRedeemerLeaf,
      activePurpose: produced.activePurpose,
      purposeBound: donor.control.purposeBound - 1,
    },
  });
};

/**
 * The challenger's forged skip for `cekRedeemerSkipForgery`: at the honest
 * select it claims the frontier purpose is a native execution, naming that
 * purpose's own leaf, source leaf and genuine execution siblings, and moves
 * the purpose bound down without selecting. Only `[native-execution]` can
 * refuse it.
 */
export const forgeRedeemerSkipTrace = ({
  trace,
  disputedLowIndex,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
}): DeterministicValidationMachineTrace => {
  const disputed = trace.witnesses[disputedLowIndex]!.auxiliary;
  if (disputed?.kind !== "cekRedeemerContextSelect")
    throw new Error("redeemer skip forgery needs an honest select step");
  return forgeRedeemerFoldStep({
    trace,
    disputedLowIndex,
    auxiliary: {
      kind: "cekRedeemerContextSkip",
      control: disputed.control,
      purposeLeaf: hashMidgardScriptPurposeLeaf(disputed.purpose),
      sourceLeaf: disputed.sourceLeaf,
      executionSiblings: disputed.executionSiblings,
    },
    forgedControl: {
      ...disputed.control,
      purposeBound: disputed.control.purposeBound - 1,
    },
  });
};

/**
 * The challenger's forged select for `cekRedeemerSelectLengthDelta`: at the
 * honest select it keeps the disputed control, the genuine execution leaf,
 * siblings, item commitment and item frontier root, and changes only
 * `totalLength` by `lengthDelta`. Its successor is exactly the one that
 * length yields: the honest successor with the active scan rebuilt from the
 * item control carrying the forged length. The control hash, the active scan
 * and `[execution-leaf]` all pass, so only `[item-length]` can refuse it.
 */
export const forgeRedeemerLengthTrace = ({
  trace,
  disputedLowIndex,
  lengthDelta,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
  readonly lengthDelta: number;
}): DeterministicValidationMachineTrace => {
  const disputed = trace.witnesses[disputedLowIndex]!.auxiliary;
  // The first item step after the select carries the redeemer control the
  // select produced and the item control it pinned.
  const item = trace.witnesses[disputedLowIndex + 1]!.auxiliary;
  if (
    disputed?.kind !== "cekRedeemerContextSelect" ||
    item?.kind !== "redeemerItemStep" ||
    item.redeemerControl === null ||
    item.control.totalLength !== disputed.totalLength
  )
    throw new Error("redeemer length forgery needs an honest select step");
  const totalLength = disputed.totalLength + lengthDelta;
  return forgeRedeemerFoldStep({
    trace,
    disputedLowIndex,
    auxiliary: { ...disputed, totalLength },
    forgedControl: {
      ...item.redeemerControl,
      activeScanHash: hashMidgardRedeemerItemProofControl({
        ...item.control,
        totalLength,
      }),
    },
  });
};

/**
 * The challenger's forged redeemer fold step the fixture options name, or
 * `undefined` when the challenger is honest: a skip in place of the disputed
 * select (`cekRedeemerSkipForgery`), the disputed select with its length
 * moved (`cekRedeemerSelectLengthDelta`), or the select of another step
 * (`cekRedeemerSelectDonorOrdinal`). The operator's claim is the honest
 * trace.
 */
export const forgeRedeemerFoldTrace = ({
  trace,
  disputedLowIndex,
  skipForgery,
  lengthDelta,
  donorOrdinal,
  selectIndices,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
  readonly skipForgery: boolean;
  readonly lengthDelta: number | undefined;
  readonly donorOrdinal: number | undefined;
  readonly selectIndices: readonly number[];
}): DeterministicValidationMachineTrace | undefined =>
  skipForgery
    ? forgeRedeemerSkipTrace({ trace, disputedLowIndex })
    : lengthDelta !== undefined
      ? forgeRedeemerLengthTrace({ trace, disputedLowIndex, lengthDelta })
      : donorOrdinal === undefined
        ? undefined
        : forgeRedeemerSelectTrace({
            trace,
            disputedLowIndex,
            donorIndex: selectIndices[donorOrdinal],
          });
