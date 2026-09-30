import {
  asArray,
  asBytes,
  buildMidgardValidationTraceTree,
  decodeSingleCbor,
  encodeCbor,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import {
  type DeterministicValidationMachineTrace,
  hashMidgardCekRedeemerContextControl,
} from "@al-ft/midgard-validation";

/**
 * The challenger's forged redeemer select for
 * `cekRedeemerSelectDonorOrdinal`: from the honest low state at
 * `disputedLowIndex` it selects what the honest select step at `donorIndex`
 * selected, and its successor is exactly the one that selection yields.
 * Every later state carries a fabricated work root.
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
    disputed?.kind !== "cekRedeemerContextSelect" ||
    donor?.kind !== "cekRedeemerContextSelect" ||
    produced === null
  )
    throw new Error("redeemer select forgery needs two honest select steps");
  const forgedControl = {
    ...disputed.control,
    activeScanHash: produced.activeScanHash,
    activeRedeemerLeaf: produced.activeRedeemerLeaf,
    activePurpose: produced.activePurpose,
    purposeBound: donor.purposeFrontierIndex,
  };
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
    auxiliary: { ...donor, control: disputed.control },
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
