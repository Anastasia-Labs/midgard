import { computeHash32, encodeCbor } from "@al-ft/midgard-core";
import { encodeVerdictSubjectV1 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  AcceptedSelectedPurposeV1Schema,
  AcceptedSelectedSourceV1Schema,
  ExecutionNativeScriptInvalidAcceptedStateV1Schema,
  ExecutionNativeScriptInvalidBoundV1Schema,
} from "./schemas-v1.js";

export const EXECUTION_NATIVE_ACCEPTED_CHECKPOINT_DOMAIN_V1 =
  "midgard/fraud-proofs/execution-native-script-invalid/accepted-reconstruction-v1";

export type AcceptedReconstructionBoundV1 = Data.Static<
  typeof ExecutionNativeScriptInvalidBoundV1Schema
>;
export type AcceptedReconstructionStateV1 = Data.Static<
  typeof ExecutionNativeScriptInvalidAcceptedStateV1Schema
>;
export type AcceptedSelectedPurposeV1 = Data.Static<
  typeof AcceptedSelectedPurposeV1Schema
>;
export type AcceptedSelectedSourceV1 = Data.Static<
  typeof AcceptedSelectedSourceV1Schema
>;

const cbor = (value: unknown): Buffer =>
  Buffer.from(encodeCbor(value as never));
const plutusOption = (
  value: AcceptedSelectedPurposeV1 | AcceptedSelectedSourceV1 | null,
  schema:
    | typeof AcceptedSelectedPurposeV1Schema
    | typeof AcceptedSelectedSourceV1Schema,
): Buffer =>
  Buffer.from(Data.to(value as never, Data.Nullable(schema) as never), "hex");

export const checkpointAcceptedReconstructionV1 = (
  state: Omit<AcceptedReconstructionStateV1, "checkpoint_hash">,
): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from(EXECUTION_NATIVE_ACCEPTED_CHECKPOINT_DOMAIN_V1),
      Buffer.from(encodeVerdictSubjectV1(state.bound.subject)),
      cbor(Buffer.from(state.bound.compact_cbor, "hex")),
      Buffer.from(state.bound.prior_ledger_root, "hex"),
      cbor(state.bound.execution_index),
      cbor(state.phase),
      cbor(state.field_cursor),
      cbor(state.execution_cursor),
      cbor(Buffer.from(state.previous_key, "hex")),
      cbor(Buffer.from(state.receive_candidate, "hex")),
      cbor(state.source_base_index),
      cbor(state.source_cursor),
      plutusOption(state.selected_purpose, AcceptedSelectedPurposeV1Schema),
      plutusOption(state.selected_source, AcceptedSelectedSourceV1Schema),
      cbor(Buffer.from(state.next_expected_script_hash, "hex")),
    ]),
  ).toString("hex");

export const sealAcceptedReconstructionStateV1 = (
  state: Omit<AcceptedReconstructionStateV1, "checkpoint_hash">,
): AcceptedReconstructionStateV1 =>
  Object.freeze({
    ...state,
    checkpoint_hash: checkpointAcceptedReconstructionV1(state),
  });

export const initialAcceptedReconstructionStateV1 = ({
  bound,
  nextScriptHash,
}: {
  bound: AcceptedReconstructionBoundV1;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 =>
  sealAcceptedReconstructionStateV1({
    bound,
    phase: 0n,
    field_cursor: 0n,
    execution_cursor: 0n,
    previous_key: "",
    receive_candidate: "",
    source_base_index: 0n,
    source_cursor: 0n,
    selected_purpose: null,
    selected_source: null,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedAdvanceNonScriptV1 = ({
  state,
  canonicalKey,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  canonicalKey: string;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 =>
  sealAcceptedReconstructionStateV1({
    ...state,
    field_cursor: state.field_cursor + 1n,
    previous_key: canonicalKey,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedFinishPurposePhaseV1 = ({
  state,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 =>
  sealAcceptedReconstructionStateV1({
    ...state,
    phase: state.selected_purpose === null ? state.phase + 1n : 4n,
    field_cursor: 0n,
    previous_key: "",
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedAppendPurposeV1 = ({
  state,
  purposeKind,
  purposeIndex,
  scriptHash,
  subject,
  canonicalKey,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  purposeKind: bigint;
  purposeIndex: bigint;
  scriptHash: string;
  subject: string;
  canonicalKey: string;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 => {
  const selected =
    state.execution_cursor === state.bound.execution_index
      ? {
          purpose_kind: purposeKind,
          purpose_index: purposeIndex,
          script_hash: scriptHash,
          subject,
        }
      : null;
  return sealAcceptedReconstructionStateV1({
    ...state,
    phase: selected === null ? state.phase : 4n,
    field_cursor: selected === null ? state.field_cursor + 1n : 0n,
    execution_cursor: state.execution_cursor + 1n,
    previous_key: selected === null ? canonicalKey : "",
    selected_purpose: selected,
    next_expected_script_hash: nextScriptHash,
  });
};

export const acceptedAppendSourceV1 = ({
  state,
  source,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  source: AcceptedSelectedSourceV1;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 =>
  sealAcceptedReconstructionStateV1({
    ...state,
    field_cursor: state.field_cursor + 1n,
    source_cursor: state.source_cursor + 1n,
    selected_source:
      source.script_hash === state.selected_purpose?.script_hash
        ? source
        : null,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedScanReceiveOutputV1 = ({
  state,
  candidate,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  candidate: string | null;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 => {
  const receiveCandidate =
    candidate !== null &&
    (state.previous_key === "" || state.previous_key < candidate) &&
    (state.receive_candidate === "" || candidate < state.receive_candidate)
      ? candidate
      : state.receive_candidate;
  return sealAcceptedReconstructionStateV1({
    ...state,
    field_cursor: state.field_cursor + 1n,
    receive_candidate: receiveCandidate,
    next_expected_script_hash: nextScriptHash,
  });
};

export const acceptedFinishReceivePassV1 = ({
  state,
  nextScanScriptHash,
  nextSourceScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  nextScanScriptHash: string;
  nextSourceScriptHash: string;
}): AcceptedReconstructionStateV1 => {
  if (state.receive_candidate === "")
    throw new Error("accepted receive pass has no script candidate");
  const emitted = acceptedAppendPurposeV1({
    state,
    purposeKind: 3n,
    purposeIndex: state.execution_cursor,
    scriptHash: state.receive_candidate,
    subject: state.receive_candidate,
    canonicalKey: state.receive_candidate,
    nextScriptHash:
      state.execution_cursor === state.bound.execution_index
        ? nextSourceScriptHash
        : nextScanScriptHash,
  });
  return sealAcceptedReconstructionStateV1({
    ...emitted,
    field_cursor: 0n,
    receive_candidate: "",
  });
};

export const acceptedAdvanceReferenceWithoutSourceV1 = ({
  state,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 =>
  sealAcceptedReconstructionStateV1({
    ...state,
    field_cursor: state.field_cursor + 1n,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedFinishInlineSourcesV1 = ({
  state,
  nextScriptHash,
}: {
  state: AcceptedReconstructionStateV1;
  nextScriptHash: string;
}): AcceptedReconstructionStateV1 =>
  sealAcceptedReconstructionStateV1({
    ...state,
    phase: 5n,
    field_cursor: 0n,
    next_expected_script_hash: nextScriptHash,
  });
