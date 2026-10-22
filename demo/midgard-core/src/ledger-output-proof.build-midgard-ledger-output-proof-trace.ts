import {
  buildMidgardBlake2b224Trace,
  encodeMidgardBlake2b224TraceControl,
  MidgardBlake2b224TraceStages,
} from "./blake2b-224-trace.js";
import {
  buildMidgardBoundedItem,
  commitMidgardBoundedItem,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  midgardBoundedItemExpectedChunkLength,
} from "./bounded-item.js";
import {
  buildMidgardCekDataTraverseTrace,
  driveMidgardCekDataTraverse,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  type MidgardCekDataTraverseTraceStep,
  nextMidgardCekDataTraverseSpan,
} from "./cek-data-traverse.js";
import { advanceMidgardLedgerOutputProof } from "./ledger-output-proof.advance-midgard-ledger-output-proof.js";
import {
  midgardLedgerOutputAttachWindowLength,
  midgardLedgerOutputWindowCovers,
} from "./ledger-output-proof.authenticated-output-span.js";
import { initialMidgardLedgerOutputProofControl } from "./ledger-output-proof.is-well-formed-midgard-ledger-output-proof-control.js";
import {
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofResultKinds,
  MidgardLedgerOutputProofStages,
  type MidgardLedgerOutputProofStepResult,
  type MidgardLedgerOutputProofTrace,
  type MidgardLedgerOutputProofTraceStep,
  type MidgardLedgerOutputProofWitness,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import {
  chunkWitness,
  isExactMidgardLedgerOutputProofTerminal,
  spanChunkWitness,
} from "./ledger-output-proof.span-chunk-witness.js";
import {
  encodeMidgardLedgerOutputScanControl,
  traceMidgardLedgerOutputScan,
} from "./ledger-output-scan.js";
import {
  buildMidgardLedgerOutputValueTrace,
  encodeMidgardLedgerOutputValueControl,
} from "./ledger-output-value.js";
import {
  encodeMidgardNativeScriptStructureControl,
  type MidgardNativeScriptScanFrame,
  type MidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureStages,
  traceMidgardNativeScriptStructure,
} from "./native-script-scan.js";

/** A step result that refuses the output. */
export type MidgardLedgerOutputProofRefusalKind = Exclude<
  NonNullable<MidgardLedgerOutputProofStepResult>["kind"],
  typeof MidgardLedgerOutputProofResultKinds.Advanced
>;

/**
 * The output proof of one output: the whole trace when the proof reaches its
 * terminal, or, when a step refuses the output, the advancing steps and the
 * refusing step (its control, its witness and the refusal kind).
 */
export type MidgardLedgerOutputProofOutcome =
  | { readonly kind: "accepted"; readonly trace: MidgardLedgerOutputProofTrace }
  | {
      readonly kind: "refused";
      readonly refusal: MidgardLedgerOutputProofRefusalKind;
      readonly item: MidgardLedgerOutputProofTrace["item"];
      readonly initial: MidgardLedgerOutputProofControl;
      readonly steps: readonly MidgardLedgerOutputProofTraceStep[];
      readonly refusingStep: {
        readonly control: MidgardLedgerOutputProofControl;
        readonly witness: MidgardLedgerOutputProofWitness;
      };
    };

/** Unwinds the proof run at the step whose result is a refusal. */
class Refused extends Error {
  constructor(
    readonly refusal: MidgardLedgerOutputProofRefusalKind,
    readonly control: MidgardLedgerOutputProofControl,
    readonly witness: MidgardLedgerOutputProofWitness,
  ) {
    super(`Canonical V1 ledger output proof refused: ${refusal}`);
  }
}

export const buildMidgardLedgerOutputProofTrace = (input: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputProofTrace => {
  const outcome = traceMidgardLedgerOutputProof(input);
  if (outcome.kind === "refused") {
    throw new Error(
      `Canonical V1 ledger output proof failed: ${outcome.refusal}`,
    );
  }
  return outcome.trace;
};

/**
 * Runs the output proof the machine runs over `outputCbor`, step by step,
 * with the honest witness for each step: the proof is the one that decides
 * whether, and how, the output is refused.
 */
export const traceMidgardLedgerOutputProof = ({
  outputIndex,
  outputCbor,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): MidgardLedgerOutputProofOutcome => {
  const bytes = Buffer.from(outputCbor);
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
    itemIndex: outputIndex,
    bytes,
  });
  const initial = initialMidgardLedgerOutputProofControl({
    outputIndex,
    totalLength: bytes.length,
    itemCommitment: item.commitment,
  });
  const steps: MidgardLedgerOutputProofTraceStep[] = [];
  let control = initial;
  const append = (witness: MidgardLedgerOutputProofWitness): void => {
    const result = advanceMidgardLedgerOutputProof({
      control,
      witness,
    });
    if (result === null) {
      throw new Error(
        "Canonical V1 ledger output proof failed: malformed evidence",
      );
    }
    if (result.kind !== MidgardLedgerOutputProofResultKinds.Advanced) {
      throw new Refused(result.kind, control, witness);
    }
    steps.push({ control, witness, next: result.control });
    control = result.control;
  };
  const run = (): MidgardLedgerOutputProofTrace => {
    /**
     * The span-attach discipline of the restructured step family: the chunk
     * merkle verification of an output span runs once, in a dedicated
     * span-attach step recording the maximal window at the span the consuming
     * stage demands, `(start, min(chunkBytes, total - start))`; every
     * subsequent span-consuming step binds the whole recorded window bytes by
     * digest. The step derives the window itself, and a new window is
     * attached only when the demanded span leaves the recorded one.
     */
    const windowBytesFor = (absoluteStart: number, length: number): Buffer => {
      if (
        !midgardLedgerOutputWindowCovers({
          spanWindow: control.spanWindow,
          absoluteStart,
          length,
        })
      ) {
        const windowLength = midgardLedgerOutputAttachWindowLength(
          bytes.length,
          absoluteStart,
        );
        const chunks = spanChunkWitness({
          item,
          absoluteStart,
          length: windowLength,
        });
        if (chunks === null || chunks.kind !== "chunks") {
          throw new Error("V1 output proof lost span-attach chunks");
        }
        append({
          kind: "spanAttach",
          chunkProof: chunks.chunkProof,
          nextChunkProof: chunks.nextChunkProof,
        });
      }
      const window = control.spanWindow;
      if (
        window === null ||
        !midgardLedgerOutputWindowCovers({
          spanWindow: window,
          absoluteStart,
          length,
        })
      ) {
        throw new Error("V1 output proof span window does not cover the span");
      }
      return Buffer.from(
        bytes.subarray(window.start, window.start + window.length),
      );
    };

    const outputScanTrace = traceMidgardLedgerOutputScan(bytes);
    for (const scanStep of outputScanTrace.steps) {
      append(
        scanStep.chunkIndex === null
          ? null
          : chunkWitness({
              item,
              chunkIndex: scanStep.chunkIndex,
              nextChunkIndex: scanStep.nextChunkIndex,
            }),
      );
      if (
        !encodeMidgardLedgerOutputScanControl(control.outputScan).equals(
          encodeMidgardLedgerOutputScanControl(scanStep.next),
        )
      ) {
        throw new Error("V1 output proof diverged from output scan");
      }
    }
    if (outputScanTrace.refusal !== null) {
      append(chunkWitness({ item, ...outputScanTrace.refusal }));
      throw new Error("V1 output proof admitted a refused output scan");
    }
    append(null);
    const valueAssets = outputScanTrace.steps.flatMap(({ asset }) =>
      asset === null ? [] : [asset],
    );
    const valueTrace = buildMidgardLedgerOutputValueTrace({
      assets: valueAssets,
      lovelace: outputScanTrace.terminal.lovelace,
    });
    if (
      valueTrace.frontier.count !==
        outputScanTrace.terminal.assetFrontier.count ||
      valueTrace.frontier.peaks.length !==
        outputScanTrace.terminal.assetFrontier.peaks.length ||
      valueTrace.frontier.peaks.some(
        (peak, index) =>
          peak.height !==
            outputScanTrace.terminal.assetFrontier.peaks[index]?.height ||
          !Buffer.from(peak.hash).equals(
            Buffer.from(
              outputScanTrace.terminal.assetFrontier.peaks[index]!.hash,
            ),
          ),
      )
    ) {
      throw new Error("V1 output proof diverged from the asset frontier");
    }
    for (const valueStep of valueTrace.steps) {
      append(
        valueStep.witness === null
          ? null
          : {
              kind: "value",
              assetIndex: valueStep.witness.assetIndex,
              policyId: valueStep.witness.policyId,
              assetName: valueStep.witness.assetName,
              quantity: valueStep.witness.quantity,
              siblings: valueStep.witness.siblings,
              previous: valueStep.witness.previous,
            },
      );
      if (
        control.value === null ||
        !encodeMidgardLedgerOutputValueControl(control.value).equals(
          encodeMidgardLedgerOutputValueControl(valueStep.next),
        )
      ) {
        throw new Error("V1 output proof diverged from Value fold");
      }
    }
    append(null);
    if (isExactMidgardLedgerOutputProofTerminal(control)) {
      return { item, initial, steps, terminal: control };
    }

    const datumOffset = outputScanTrace.terminal.datumOffset;
    const datumLength = outputScanTrace.terminal.datumLength;
    if (control.stage === MidgardLedgerOutputProofStages.DatumTraversal) {
      const source = bytes.subarray(datumOffset, datumOffset + datumLength);
      const datumWitness = (
        step: Pick<MidgardCekDataTraverseTraceStep, "control" | "action">,
      ): MidgardLedgerOutputProofWitness => {
        const span = nextMidgardCekDataTraverseSpan(step.control);
        // An empty span reads no output bytes: it takes no window.
        return {
          kind: "datum",
          action: step.action,
          window:
            span === null || span.length === 0
              ? null
              : windowBytesFor(span.absoluteStart, span.length),
        };
      };
      let datumTrace;
      try {
        datumTrace = buildMidgardCekDataTraverseTrace({
          sourceStart: datumOffset,
          source,
        });
      } catch (cause) {
        // Data that is not canonical: the step `Unreadable` holds at refuses it.
        const outcome = driveMidgardCekDataTraverse({
          sourceStart: datumOffset,
          source,
        });
        if (outcome.verdict !== "unreadable") throw cause;
        for (const datumStep of outcome.steps) append(datumWitness(datumStep));
        append(datumWitness(outcome.refusal));
        throw new Error("V1 output proof admitted unreadable datum");
      }
      for (const datumStep of datumTrace.steps) {
        append(datumWitness(datumStep));
        if (
          control.datum === null ||
          !encodeMidgardCekDataTraverseControl(control.datum).equals(
            encodeMidgardCekDataTraverseControl(datumStep.next),
          )
        ) {
          throw new Error("V1 output proof diverged from datum traversal");
        }
      }
      append(null);
      if (
        control.datum === null ||
        finalizeMidgardCekDataTraverse(control.datum) === null
      ) {
        throw new Error(
          "V1 output proof did not authenticate the inline datum",
        );
      }
    }
    if (isExactMidgardLedgerOutputProofTerminal(control)) {
      return { item, initial, steps, terminal: control };
    }

    const referenceItemOffset =
      outputScanTrace.terminal.referenceScriptItemOffset;
    const referenceItemLength = bytes.length - referenceItemOffset;
    const referenceItem = buildMidgardBoundedItem({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
      itemIndex: outputIndex,
      bytes: bytes.subarray(referenceItemOffset),
    });
    for (
      let chunkIndex = 0;
      chunkIndex < referenceItem.frontier.count;
      chunkIndex += 1
    ) {
      append({
        kind: "window",
        bytes: windowBytesFor(
          referenceItemOffset + chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
          midgardBoundedItemExpectedChunkLength({
            totalLength: referenceItemLength,
            chunkIndex,
          }),
        ),
      });
    }
    append(null);
    if (
      !commitMidgardBoundedItem({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
        itemIndex: outputIndex,
        totalLength: referenceItemLength,
        frontier: control.referenceScriptFrontier,
      }).equals(referenceItem.commitment)
    ) {
      throw new Error(
        "V1 output proof diverged from reference-script commitment",
      );
    }

    const referenceOffset = outputScanTrace.terminal.referenceScriptOffset;
    const referenceLength = outputScanTrace.terminal.referenceScriptLength;
    const referenceLanguage = outputScanTrace.terminal.referenceScriptLanguage;
    const scriptBytes = bytes.subarray(
      referenceOffset,
      referenceOffset + referenceLength,
    );
    const identityMessage = Buffer.concat([
      Buffer.from([referenceLanguage]),
      scriptBytes,
    ]);
    const hashTrace = buildMidgardBlake2b224Trace(identityMessage);
    for (const hashStep of hashTrace) {
      const includesLanguage =
        hashStep.control.stage === MidgardBlake2b224TraceStages.Ready &&
        hashStep.control.cursor === 0;
      const contentLength =
        hashStep.block === null
          ? 0
          : hashStep.block.length - (includesLanguage ? 1 : 0);
      append(
        contentLength === 0
          ? null
          : {
              kind: "window",
              bytes: windowBytesFor(
                referenceOffset +
                  hashStep.control.cursor -
                  (includesLanguage ? 0 : 1),
                contentLength,
              ),
            },
      );
      if (
        control.scriptHash === null ||
        !encodeMidgardBlake2b224TraceControl(control.scriptHash).equals(
          encodeMidgardBlake2b224TraceControl(hashStep.next),
        )
      ) {
        throw new Error("V1 output proof diverged from script hash trace");
      }
    }
    append(null);
    if (isExactMidgardLedgerOutputProofTerminal(control)) {
      return { item, initial, steps, terminal: control };
    }

    const nativeWitness = (step: {
      readonly control: MidgardNativeScriptStructureControl;
      readonly frame: MidgardNativeScriptScanFrame | null;
    }): MidgardLedgerOutputProofWitness => {
      if (step.control.stage === MidgardNativeScriptStructureStages.Token) {
        const chunkIndex = Math.floor(
          step.control.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        );
        const chunkCount = midgardBoundedItemChunkCount(bytes.length);
        return chunkWitness({
          item,
          chunkIndex,
          nextChunkIndex: chunkIndex + 1 < chunkCount ? chunkIndex + 1 : null,
        });
      }
      if (step.control.stage === MidgardNativeScriptStructureStages.Frame) {
        if (step.frame === null) {
          throw new Error("V1 native output proof lost a frame");
        }
        return { kind: "nativeFrame", frame: step.frame };
      }
      return null;
    };
    const nativeTrace = traceMidgardNativeScriptStructure(
      scriptBytes,
      referenceOffset,
    );
    for (const nativeStep of nativeTrace.steps) {
      append(nativeWitness(nativeStep));
      if (
        control.nativeScript === null ||
        !encodeMidgardNativeScriptStructureControl(control.nativeScript).equals(
          encodeMidgardNativeScriptStructureControl(nativeStep.next),
        )
      ) {
        throw new Error("V1 output proof diverged from native scan");
      }
    }
    if (nativeTrace.refusal !== null) {
      append(nativeWitness(nativeTrace.refusal));
      throw new Error("V1 output proof admitted a refused native script");
    }
    append(null);
    if (!isExactMidgardLedgerOutputProofTerminal(control)) {
      throw new Error("Canonical V1 ledger output proof did not terminate");
    }
    return { item, initial, steps, terminal: control };
  };
  try {
    return { kind: "accepted", trace: run() };
  } catch (cause) {
    if (!(cause instanceof Refused)) throw cause;
    return {
      kind: "refused",
      refusal: cause.refusal,
      item,
      initial,
      steps,
      refusingStep: { control: cause.control, witness: cause.witness },
    };
  }
};
