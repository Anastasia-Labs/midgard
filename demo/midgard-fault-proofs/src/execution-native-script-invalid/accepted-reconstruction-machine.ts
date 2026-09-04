import { computeHash32, encodeCbor } from "@al-ft/midgard-core";
import { encodeVerdictSubject } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  AcceptedSelectedPurposeSchema,
  AcceptedSelectedSourceSchema,
  ExecutionNativeScriptInvalidAcceptedStateSchema,
  ExecutionNativeScriptInvalidBoundSchema,
} from "./schemas.js";

export const EXECUTION_NATIVE_ACCEPTED_CHECKPOINT_DOMAIN =
  "midgard/fraud-proofs/execution-native-script-invalid/accepted-reconstruction-v1";

export type AcceptedReconstructionBound = Data.Static<
  typeof ExecutionNativeScriptInvalidBoundSchema
>;
export type AcceptedReconstructionState = Data.Static<
  typeof ExecutionNativeScriptInvalidAcceptedStateSchema
>;
export type AcceptedSelectedPurpose = Data.Static<
  typeof AcceptedSelectedPurposeSchema
>;
export type AcceptedSelectedSource = Data.Static<
  typeof AcceptedSelectedSourceSchema
>;

const cbor = (value: unknown): Buffer =>
  Buffer.from(encodeCbor(value as never));
const plutusOption = (
  value: AcceptedSelectedPurpose | AcceptedSelectedSource | null,
  schema:
    | typeof AcceptedSelectedPurposeSchema
    | typeof AcceptedSelectedSourceSchema,
): Buffer =>
  Buffer.from(Data.to(value as never, Data.Nullable(schema) as never), "hex");

export const checkpointAcceptedReconstruction = (
  state: Omit<AcceptedReconstructionState, "checkpoint_hash">,
): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from(EXECUTION_NATIVE_ACCEPTED_CHECKPOINT_DOMAIN),
      Buffer.from(encodeVerdictSubject(state.bound.subject)),
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
      plutusOption(state.selected_purpose, AcceptedSelectedPurposeSchema),
      plutusOption(state.selected_source, AcceptedSelectedSourceSchema),
      cbor(Buffer.from(state.next_expected_script_hash, "hex")),
    ]),
  ).toString("hex");

export const sealAcceptedReconstructionState = (
  state: Omit<AcceptedReconstructionState, "checkpoint_hash">,
): AcceptedReconstructionState =>
  Object.freeze({
    ...state,
    checkpoint_hash: checkpointAcceptedReconstruction(state),
  });

export const initialAcceptedReconstructionState = ({
  bound,
  nextScriptHash,
}: {
  bound: AcceptedReconstructionBound;
  nextScriptHash: string;
}): AcceptedReconstructionState =>
  sealAcceptedReconstructionState({
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

export const acceptedAdvanceNonScript = ({
  state,
  canonicalKey,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  canonicalKey: string;
  nextScriptHash: string;
}): AcceptedReconstructionState =>
  sealAcceptedReconstructionState({
    ...state,
    field_cursor: state.field_cursor + 1n,
    previous_key: canonicalKey,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedFinishPurposePhase = ({
  state,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  nextScriptHash: string;
}): AcceptedReconstructionState =>
  sealAcceptedReconstructionState({
    ...state,
    phase: state.selected_purpose === null ? state.phase + 1n : 4n,
    field_cursor: 0n,
    previous_key: "",
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedAppendPurpose = ({
  state,
  purposeKind,
  purposeIndex,
  scriptHash,
  subject,
  canonicalKey,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  purposeKind: bigint;
  purposeIndex: bigint;
  scriptHash: string;
  subject: string;
  canonicalKey: string;
  nextScriptHash: string;
}): AcceptedReconstructionState => {
  const selected =
    state.execution_cursor === state.bound.execution_index
      ? {
          purpose_kind: purposeKind,
          purpose_index: purposeIndex,
          script_hash: scriptHash,
          subject,
        }
      : null;
  return sealAcceptedReconstructionState({
    ...state,
    phase: selected === null ? state.phase : 4n,
    field_cursor: selected === null ? state.field_cursor + 1n : 0n,
    execution_cursor: state.execution_cursor + 1n,
    previous_key: selected === null ? canonicalKey : "",
    selected_purpose: selected,
    next_expected_script_hash: nextScriptHash,
  });
};

export const acceptedAppendSource = ({
  state,
  source,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  source: AcceptedSelectedSource;
  nextScriptHash: string;
}): AcceptedReconstructionState =>
  sealAcceptedReconstructionState({
    ...state,
    field_cursor: state.field_cursor + 1n,
    source_cursor: state.source_cursor + 1n,
    selected_source:
      source.script_hash === state.selected_purpose?.script_hash
        ? source
        : null,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedScanReceiveOutput = ({
  state,
  candidate,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  candidate: string | null;
  nextScriptHash: string;
}): AcceptedReconstructionState => {
  const receiveCandidate =
    candidate !== null &&
    (state.previous_key === "" || state.previous_key < candidate) &&
    (state.receive_candidate === "" || candidate < state.receive_candidate)
      ? candidate
      : state.receive_candidate;
  return sealAcceptedReconstructionState({
    ...state,
    field_cursor: state.field_cursor + 1n,
    receive_candidate: receiveCandidate,
    next_expected_script_hash: nextScriptHash,
  });
};

export const acceptedFinishReceivePass = ({
  state,
  nextScanScriptHash,
  nextSourceScriptHash,
}: {
  state: AcceptedReconstructionState;
  nextScanScriptHash: string;
  nextSourceScriptHash: string;
}): AcceptedReconstructionState => {
  if (state.receive_candidate === "")
    throw new Error("accepted receive pass has no script candidate");
  const emitted = acceptedAppendPurpose({
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
  return sealAcceptedReconstructionState({
    ...emitted,
    field_cursor: 0n,
    receive_candidate: "",
  });
};

export const acceptedAdvanceReferenceWithoutSource = ({
  state,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  nextScriptHash: string;
}): AcceptedReconstructionState =>
  sealAcceptedReconstructionState({
    ...state,
    field_cursor: state.field_cursor + 1n,
    next_expected_script_hash: nextScriptHash,
  });

export const acceptedFinishInlineSources = ({
  state,
  nextScriptHash,
}: {
  state: AcceptedReconstructionState;
  nextScriptHash: string;
}): AcceptedReconstructionState =>
  sealAcceptedReconstructionState({
    ...state,
    phase: 5n,
    field_cursor: 0n,
    next_expected_script_hash: nextScriptHash,
  });
