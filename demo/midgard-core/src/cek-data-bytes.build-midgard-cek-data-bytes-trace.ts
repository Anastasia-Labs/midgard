import {
  advanceMidgardCekDataBytes,
  nextMidgardCekDataBytesSpan,
} from "./cek-data-bytes.content-plan.js";
import {
  initialMidgardCekDataBytesControl,
  isWellFormedMidgardCekDataBytesControl,
  MidgardCekDataBytesStages,
  type MidgardCekDataBytesTrace,
  type MidgardCekDataBytesTraceStep,
} from "./cek-data-bytes.parse-midgard-cek-data-bytes-syntax.js";

export const buildMidgardCekDataBytesTrace = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekDataBytesTrace => {
  const bytes = Buffer.from(source);
  const initial = initialMidgardCekDataBytesControl({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekDataBytesTraceStep[] = [];
  let control = initial;
  while (control.stage !== MidgardCekDataBytesStages.Terminal) {
    const span = nextMidgardCekDataBytesSpan(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekDataBytes({
      control,
      sourceBytes,
    });
    if (next === null || !isWellFormedMidgardCekDataBytesControl(next)) {
      throw new Error("V1 CEK Data bytes trace failed closed");
    }
    steps.push({ control, sourceBytes, next });
    control = next;
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};
