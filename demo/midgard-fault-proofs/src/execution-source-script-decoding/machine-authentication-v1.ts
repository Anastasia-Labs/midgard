import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  type EventKey,
  EventKeySchema,
  Proof,
  ROOT_DOMAINS,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import type {
  DeterministicValidationMachineTrace,
  RejectCode,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import { NativeScriptsControlV1Schema } from "./schemas-v1.js";
import type { ExecutionSourceAuthenticationDataV1 } from "./submit-step-02-v1.js";

export type ExecutionSourceMachineAuthenticationV1 = Readonly<{
  validationTracesRoot: string;
  validationTraceCount: bigint;
  authentication: ExecutionSourceAuthenticationDataV1;
}>;

export const executionSourceNativeControlFromCborV1 = (
  bytes: Buffer,
): Data.Static<typeof NativeScriptsControlV1Schema> => {
  const decoded = decodeSingleCbor(bytes);
  if (!Array.isArray(decoded) || decoded.length !== 26)
    throw new Error(
      "executionSourceScriptDecoding native control shape changed",
    );
  const hex = (value: unknown) =>
    Buffer.from(value as Uint8Array).toString("hex");
  const integer = (value: unknown) => BigInt(value as bigint | number);
  const frontier = (value: unknown) =>
    (value as readonly (readonly [bigint, Uint8Array])[]).map(
      ([height, hash]) => ({ height: BigInt(height), hash: hex(hash) }),
    );
  return {
    compact_cbor: hex(decoded[0]),
    witness_set_compact_cbor: hex(decoded[1]),
    field_preimage_lengths_cbor: hex(decoded[2]),
    context_cbor: hex(decoded[3]),
    resolved_input_count: integer(decoded[4]),
    resolved_inputs_accumulator: hex(decoded[5]),
    spend_input_count: integer(decoded[6]),
    resolved_item_peaks: frontier(decoded[7]),
    signer_count: integer(decoded[8]),
    signer_frontier_commitment: hex(decoded[9]),
    source_count: integer(decoded[10]),
    source_peaks: frontier(decoded[11]),
    redeemer_count: integer(decoded[12]),
    redeemer_peaks: frontier(decoded[13]),
    purpose_count: integer(decoded[14]),
    purpose_peaks: frontier(decoded[15]),
    output_count: integer(decoded[16]),
    output_peaks: frontier(decoded[17]),
    output_descriptor_peaks: frontier(decoded[18]),
    mint_count: integer(decoded[19]),
    mint_peaks: frontier(decoded[20]),
    execution_count: integer(decoded[21]),
    execution_peaks: frontier(decoded[22]),
    execution_cursor: integer(decoded[23]),
    language_bitmap: integer(decoded[24]),
    resolution_schedule_hash: hex(decoded[25]),
  };
};

/** Reconstructs step-02 solely from deterministic public replay output. */
export const buildExecutionSourceMachineAuthenticationV1 = async ({
  trace,
  eventKey,
  claimedVerdict,
  claimedRejectionCode,
  authenticatedValidationTraceEntries,
  expectedValidationTracesRoot,
}: {
  trace: DeterministicValidationMachineTrace;
  eventKey: EventKey;
  claimedVerdict: "accepted" | "rejected";
  claimedRejectionCode: RejectCode | null;
  authenticatedValidationTraceEntries?: readonly Readonly<{
    key: Uint8Array;
    value: Uint8Array;
  }>[];
  expectedValidationTracesRoot?: string;
}): Promise<ExecutionSourceMachineAuthenticationV1> => {
  const stateIndex = trace.witnesses.findIndex(
    ({ phase, auxiliary }) =>
      phase === "nativeScripts" &&
      auxiliary?.kind === "nativeExecutionDescriptor",
  );
  if (stateIndex < 0)
    throw new Error(
      "executionSourceScriptDecoding replay has no native execution descriptor",
    );
  const witness = trace.witnesses[stateIndex]!;
  const auxiliary = witness.auxiliary;
  if (
    auxiliary?.kind !== "nativeExecutionDescriptor" ||
    auxiliary.languageTag !== 0
  )
    throw new Error(
      "executionSourceScriptDecoding selected source is not native",
    );
  const rejectionCodeHash =
    claimedRejectionCode === null
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCodeV1(claimedRejectionCode);
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineStateV1),
    claimedVerdict,
    rejectionCodeHash,
  );
  const keyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const descriptor = validationTraceDescriptorDataFromCore(tree.descriptor);
  const valueCbor = Buffer.from(
    Data.to(descriptor as never, ValidationTraceDescriptorV1Schema),
    "hex",
  );
  const entries =
    authenticatedValidationTraceEntries === undefined
      ? [{ key: keyCbor, value: valueCbor }]
      : authenticatedValidationTraceEntries.map((entry) => ({
          key: Buffer.from(entry.key),
          value: Buffer.from(entry.value),
        }));
  const selected = entries.filter(({ key }) => key.equals(keyCbor));
  if (selected.length !== 1 || !selected[0]!.value.equals(valueCbor))
    throw new Error(
      "executionSourceScriptDecoding retained validation descriptor changed",
    );
  const root = await buildCountedRoot(ROOT_DOMAINS.validationTraces, entries);
  if (
    expectedValidationTracesRoot !== undefined &&
    root.root !== expectedValidationTracesRoot
  )
    throw new Error(
      "executionSourceScriptDecoding retained validation root changed",
    );
  const proof = await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    keyCbor,
    valueCbor,
  );
  const authentication: ExecutionSourceAuthenticationDataV1 = {
    trace_membership: {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key: eventKey,
      value: descriptor,
      proof: Data.from(Data.to(proof, Proof), Proof),
    },
    machine_state: validationMachineStateDataFromCore(
      trace.states[stateIndex]!,
    ),
    trace_proof: validationTraceProofDataFromCore(tree.proofs[stateIndex]!),
    control: executionSourceNativeControlFromCborV1(witness.cbor),
    purpose_kind: BigInt(auxiliary.purpose.purposeKind),
    purpose_index: auxiliary.purpose.purposeIndex,
    script_hash: auxiliary.purpose.scriptHash.toString("hex"),
    purpose_subject: auxiliary.purpose.subject.toString("hex"),
    purpose_siblings: auxiliary.purpose.siblings.map((value) =>
      value.toString("hex"),
    ),
    source_index: BigInt(auxiliary.source.sourceIndex),
    origin_kind: auxiliary.source.originKind === "inline" ? 0n : 1n,
    source_key: auxiliary.source.sourceKey.toString("hex"),
    language_tag: BigInt(auxiliary.languageTag),
    total_length: BigInt(auxiliary.source.scriptTotalLength),
    item_commitment: auxiliary.source.scriptItemCommitment.toString("hex"),
    source_siblings: auxiliary.source.siblings.map((value) =>
      value.toString("hex"),
    ),
    redeemer_leaf: auxiliary.redeemerLeaf.toString("hex"),
    execution_siblings: auxiliary.executionSiblings.map((value) =>
      value.toString("hex"),
    ),
  };
  return Object.freeze({
    validationTracesRoot: root.root,
    validationTraceCount: root.count,
    authentication,
  });
};
