import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputScanTrace,
  buildMidgardNativeScriptDecodingTrace,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
  encodeMidgardSpendInputItem,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
} from "@al-ft/midgard-core";
import {
  type DaPayloadEntry,
  decodeRetainedValidationWitness,
} from "@al-ft/midgard-sdk";

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

/** Candidates carry no authority. Their enclosing ledger root must independently
 * match the L1 header/transition before callers may use them. */
export const retainedLedgerDescriptorCandidates = (
  entries: readonly DaPayloadEntry[],
) => {
  const candidates = new Map<string, Map<string, Buffer>>();
  for (const [, value] of entries) {
    const retained = decodeRetainedValidationWitness(Buffer.from(value, "hex"));
    const auxiliary = retained.auxiliary;
    if (typeof auxiliary !== "object") continue;
    let key: Buffer, descriptor: Buffer;
    if ("ScheduledLedgerMembershipWitness" in auxiliary) {
      key = Buffer.from(auxiliary.ScheduledLedgerMembershipWitness.key, "hex");
      descriptor = Buffer.from(
        auxiliary.ScheduledLedgerMembershipWitness.value,
        "hex",
      );
    } else if ("LedgerDeltaOutputWitness" in auxiliary) {
      key = encodeMidgardSpendInputItem({
        txId: Buffer.from(retained.machine_state.transaction_id, "hex"),
        outputIndex: Number(auxiliary.LedgerDeltaOutputWitness.output_index),
      });
      descriptor = Buffer.from(
        auxiliary.LedgerDeltaOutputWitness.descriptor_cbor,
        "hex",
      );
    } else continue;
    const values =
      candidates.get(key.toString("hex")) ?? new Map<string, Buffer>();
    values.set(descriptor.toString("hex"), descriptor);
    candidates.set(key.toString("hex"), values);
  }
  return candidates;
};

/** A narrow fault-evidence path for an outer-canonical output whose native
 * script structure is invalid. It does not make the output canonically valid. */
export const retainedUndecodableOutputDescriptor = ({
  key,
  output,
  candidates,
}: {
  key: Buffer;
  output: Buffer;
  candidates: ReturnType<typeof retainedLedgerDescriptorCandidates>;
}) => {
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
  const outpoint = decodeMidgardSpendInputItem(key);
  const boundOutput = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: outpoint.outputIndex,
    bytes: output,
  });
  const boundScript = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: outpoint.outputIndex,
    bytes: item,
  });
  const matching = [
    ...(candidates.get(key.toString("hex"))?.values() ?? []),
  ].filter((value) => {
    const d = decodeMidgardLedgerOutputCommitment(value);
    return (
      d.outputIndex === outpoint.outputIndex &&
      d.totalLength === output.length &&
      d.itemCommitment.equals(boundOutput.commitment) &&
      d.referenceScriptLanguage === 0 &&
      d.referenceScriptTotalLength === item.length &&
      d.referenceScriptItemCommitment.equals(boundScript.commitment)
    );
  });
  if (matching.length !== 1)
    throw new Error("retained output descriptor is absent or ambiguous");
  return matching[0]!;
};
