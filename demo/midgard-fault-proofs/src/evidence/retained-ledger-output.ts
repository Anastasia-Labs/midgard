import {
  buildMidgardLedgerOutputScanTrace,
  buildMidgardNativeScriptDecodingTrace,
  decodeMidgardSpendInputItem,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
} from "@al-ft/midgard-core";
import { buildOpaqueNativeReferenceScriptLedgerOutputMaterial } from "@al-ft/midgard-validation";

/** Exact raw wrapper bytes; the bounded output scanner never interprets its native AST. */
export const retainedOutputReferenceScript = (
  output: Buffer,
): Buffer | null => {
  const trace = buildMidgardLedgerOutputScanTrace(output);
  const offset = trace.terminal.referenceScriptItemOffset;
  return offset < 0
    ? null
    : output.subarray(
        offset,
        trace.terminal.referenceScriptOffset +
          trace.terminal.referenceScriptLength,
      );
};

/** The ledger descriptor of an outer-canonical output whose native reference
 * script does not decode, derived from its bytes alone. A narrow fault-evidence
 * path: it does not make the output canonically valid, and the enclosing
 * ledger root must still match the L1 header/transition before callers use it. */
export const undecodableNativeOutputDescriptor = ({
  key,
  output,
}: {
  key: Buffer;
  output: Buffer;
}): Buffer => {
  const item = retainedOutputReferenceScript(output);
  if (item === null) throw new Error("retained output has no reference script");
  const trace = buildMidgardNativeScriptDecodingTrace(item);
  if (
    trace.bind.kind === MidgardNativeScriptDecodingBindKinds.NonNative ||
    (trace.bind.kind !== MidgardNativeScriptDecodingBindKinds.Malformed &&
      trace.outcome?.kind !==
        MidgardNativeScriptDecodingTraceOutcomeKinds.Refused)
  )
    throw new Error("retained output is not an undecodable native script");
  return buildOpaqueNativeReferenceScriptLedgerOutputMaterial({
    outputIndex: decodeMidgardSpendInputItem(key).outputIndex,
    outputCbor: output,
  }).descriptorCbor;
};
