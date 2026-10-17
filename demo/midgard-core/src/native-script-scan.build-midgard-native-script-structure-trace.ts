import {
  initialMidgardNativeScriptStructureControl,
  isMidgardNativeScriptContainerKind,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
  type MidgardNativeScriptScanFrame,
  MidgardNativeScriptStructureResultKinds,
  MidgardNativeScriptStructureStages,
  type MidgardNativeScriptStructureStepResult,
  type MidgardNativeScriptStructureTraceStep,
} from "./native-script-scan.is-well-formed-midgard-native-script-structure-control.js";
import {
  advanceMidgardNativeScriptStructureFrame,
  advanceMidgardNativeScriptStructureToken,
  finalizeMidgardNativeScriptStructure,
  isExactMidgardNativeScriptStructureTerminal,
  readToken,
} from "./native-script-scan.read-token.js";

export const buildMidgardNativeScriptStructureTrace = (
  scriptBytes: Uint8Array,
  startOffset = 0,
): readonly MidgardNativeScriptStructureTraceStep[] => {
  const bytes = Buffer.from(scriptBytes);
  let control = initialMidgardNativeScriptStructureControl({
    startOffset,
    totalLength: bytes.length,
  });
  const frames: MidgardNativeScriptScanFrame[] = [];
  const steps: MidgardNativeScriptStructureTraceStep[] = [];
  const maximumSteps = MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES * 2 + 1;
  while (
    control.stage !== MidgardNativeScriptStructureStages.Terminal &&
    steps.length < maximumSteps
  ) {
    let result: MidgardNativeScriptStructureStepResult | null;
    let frame: MidgardNativeScriptScanFrame | null = null;
    if (control.stage === MidgardNativeScriptStructureStages.Token) {
      const token = readToken({
        control,
        window: bytes,
        windowOffset: control.cursor - startOffset,
      });
      result = advanceMidgardNativeScriptStructureToken({
        control,
        window: bytes,
        windowOffset: control.cursor - startOffset,
      });
      if (
        isMidgardNativeScriptContainerKind(token.kind) &&
        token.childCount > 0
      ) {
        frames.push({
          tail: control.stackRoot,
          kind: token.kind,
          childCount: token.childCount,
          remaining: token.childCount,
          validCount: 0,
          required: token.required,
        });
      }
    } else if (control.stage === MidgardNativeScriptStructureStages.Frame) {
      frame = frames.at(-1) ?? null;
      if (frame === null) {
        throw new Error("V1 native-script trace lost its stack frame");
      }
      result = advanceMidgardNativeScriptStructureFrame({
        control,
        frame,
      });
      if (frame.remaining === 1) {
        frames.pop();
      } else {
        frames[frames.length - 1] = {
          ...frame,
          remaining: frame.remaining - 1,
        };
      }
    } else {
      result = finalizeMidgardNativeScriptStructure(control);
    }
    if (
      result === null ||
      result.kind !== MidgardNativeScriptStructureResultKinds.Advanced
    ) {
      throw new Error(
        `Canonical V1 native-script scan failed: ${result?.kind ?? "malformed"}`,
      );
    }
    steps.push({ control, next: result.control, frame });
    control = result.control;
  }
  if (
    frames.length !== 0 ||
    !isExactMidgardNativeScriptStructureTerminal(control)
  ) {
    throw new Error("Canonical V1 native-script scan did not terminate");
  }
  return steps;
};
