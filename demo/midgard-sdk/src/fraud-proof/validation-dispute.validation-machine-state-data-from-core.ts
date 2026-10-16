import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  decodeMidgardValidationTraceDescriptor,
  encodeMidgardValidationTraceDescriptor,
  type MidgardValidationMachineState,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import {
  ValidationTraceDescriptor,
  ValidationTraceDescriptorSchema,
} from "../ledger-state.js";
import { ValidationAuxiliaryWitnessSchema } from "./validation-auxiliary-witness.js";
import {
  cancelActionSchema,
  ValidationBoundarySpendRedeemerSchema,
} from "./validation-dispute.validation-game-action-schema.js";
import {
  ValidationMachineState,
  ValidationOneStepWitnessSchema,
  ValidationTraceProof,
} from "./validation-dispute.validation-machine-phase-schema.js";

export type ValidationBoundarySpendRedeemer = Data.Static<
  typeof ValidationBoundarySpendRedeemerSchema
>;

export const ValidationBoundarySpendRedeemer =
  asDataType<ValidationBoundarySpendRedeemer>(
    ValidationBoundarySpendRedeemerSchema,
  );

const ValidationPrepareSelectedFieldsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  semantic_resolver_index: Data.Integer(),
  transition: ValidationOneStepWitnessSchema,
  auxiliary: ValidationAuxiliaryWitnessSchema,
});

export const ValidationPrepareSelectedActionSchema =
  ValidationPrepareSelectedFieldsSchema;

export type ValidationPrepareSelectedAction = Data.Static<
  typeof ValidationPrepareSelectedActionSchema
>;

export const ValidationPrepareSelectedAction =
  asDataType<ValidationPrepareSelectedAction>(
    ValidationPrepareSelectedActionSchema,
  );

export const ValidationPrepareSelectedSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationPrepareSelectedActionSchema]),
  }),
]);

export type ValidationPrepareSelectedSpendRedeemer = Data.Static<
  typeof ValidationPrepareSelectedSpendRedeemerSchema
>;

export const ValidationPrepareSelectedSpendRedeemer =
  asDataType<ValidationPrepareSelectedSpendRedeemer>(
    ValidationPrepareSelectedSpendRedeemerSchema,
  );

// Option B (#620): the canonical-decode preparation commits to the transition
// alone — the validator computes `hash_one_step_evidence(transition,
// NoAuxiliaryWitness)` on-chain, so its `PrepareSelected` carries no auxiliary
// and the retired `PrepareSelectedByEvidenceHash` arm is gone. Single Aiken
// constructor: modeled as Data.Object (Lucid unwraps a one-member Data.Enum),
// which still emits the required constructor-0 wire shape.
export const ValidationCanonicalDecodePrepareSelectedActionSchema = Data.Object(
  {
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    semantic_resolver_index: Data.Integer(),
    transition: ValidationOneStepWitnessSchema,
  },
);

export type ValidationCanonicalDecodePrepareSelectedAction = Data.Static<
  typeof ValidationCanonicalDecodePrepareSelectedActionSchema
>;

export const ValidationCanonicalDecodePrepareSelectedAction =
  asDataType<ValidationCanonicalDecodePrepareSelectedAction>(
    ValidationCanonicalDecodePrepareSelectedActionSchema,
  );

export const ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema =
  Data.Enum([
    cancelActionSchema,
    Data.Object({
      Continue: Data.Tuple([
        ValidationCanonicalDecodePrepareSelectedActionSchema,
      ]),
    }),
  ]);

export type ValidationCanonicalDecodePrepareSelectedSpendRedeemer = Data.Static<
  typeof ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema
>;

export const ValidationCanonicalDecodePrepareSelectedSpendRedeemer =
  asDataType<ValidationCanonicalDecodePrepareSelectedSpendRedeemer>(
    ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema,
  );

/**
 * CEK complete-material carriage named by the `material_route` field of the
 * CEK execution-selection semantic action
 * (`cek_execution_selection_semantic_v1.VerifyExecutionSelection`). The route
 * is resolver evidence, never part of the hashed step witness.
 */
export const ValidationCekMaterialRouteSchema = Data.Enum([
  Data.Literal("NoCekMaterial"),
  Data.Object({
    DirectCekMaterial: Data.Object({
      envelope_cbor: Data.Bytes(),
      sidecar_cbor: Data.Bytes(),
    }),
  }),
  Data.Object({
    SinglePublicationCekMaterial: Data.Object({
      envelope_cbor: Data.Bytes(),
      reference_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    MinimumMultiOutputCekMaterial: Data.Object({
      envelope_cbor: Data.Bytes(),
      reference_input_indices: Data.Array(Data.Integer()),
    }),
  }),
  Data.Object({
    IncrementalCekMaterial: Data.Object({
      program_envelope_hash: H32Schema,
    }),
  }),
]);

export type ValidationCekMaterialRoute = Data.Static<
  typeof ValidationCekMaterialRouteSchema
>;

export const ValidationCekMaterialRoute =
  asDataType<ValidationCekMaterialRoute>(ValidationCekMaterialRouteSchema);

export const bytesHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const safeNumber = (value: bigint, field: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return number;
};

const verdictData = (
  verdict: MidgardValidationTraceDescriptor["verdict"],
): "Accepted" | "Rejected" =>
  verdict === "accepted" ? "Accepted" : "Rejected";

const verdictCore = (
  verdict: Data.Static<typeof ValidationTraceDescriptorSchema>["verdict"],
): MidgardValidationTraceDescriptor["verdict"] => {
  if (verdict === "Accepted") {
    return "accepted";
  }
  if (verdict === "Rejected") {
    return "rejected";
  }
  throw new Error("descriptor.verdict must be Accepted or Rejected");
};

export const validationTraceDescriptorDataFromCore = (
  descriptor: MidgardValidationTraceDescriptor,
): Data.Static<typeof ValidationTraceDescriptorSchema> => ({
  schema_version: BigInt(descriptor.schemaVersion),
  machine_version: BigInt(descriptor.machineVersion),
  trace_root: bytesHex(descriptor.traceRoot),
  step_count: BigInt(descriptor.stepCount),
  initial_state_hash: bytesHex(descriptor.initialStateHash),
  terminal_state_hash: bytesHex(descriptor.terminalStateHash),
  verdict: verdictData(descriptor.verdict),
  rejection_code_hash: bytesHex(descriptor.rejectionCodeHash),
});

export const validationTraceDescriptorCoreFromData = (
  descriptor: Data.Static<typeof ValidationTraceDescriptorSchema>,
): MidgardValidationTraceDescriptor => ({
  schemaVersion: safeNumber(
    descriptor.schema_version,
    "descriptor.schema_version",
  ) as MidgardValidationTraceDescriptor["schemaVersion"],
  machineVersion: safeNumber(
    descriptor.machine_version,
    "descriptor.machine_version",
  ) as MidgardValidationTraceDescriptor["machineVersion"],
  traceRoot: Buffer.from(descriptor.trace_root, "hex"),
  stepCount: safeNumber(descriptor.step_count, "descriptor.step_count"),
  initialStateHash: Buffer.from(descriptor.initial_state_hash, "hex"),
  terminalStateHash: Buffer.from(descriptor.terminal_state_hash, "hex"),
  verdict: verdictCore(descriptor.verdict),
  rejectionCodeHash: Buffer.from(descriptor.rejection_code_hash, "hex"),
});

/** Which stage refused a `validation_traces` value. */
export class ValidationTraceDescriptorDecodeError extends Error {
  constructor(
    /** `data`: not canonical Plutus Data; `bounds`: not a bounded V1 descriptor. */
    readonly stage: "data" | "bounds",
    message: string,
    readonly cause: unknown,
  ) {
    super(message);
    this.name = "ValidationTraceDescriptorDecodeError";
  }
}

/**
 * Decodes one `validation_traces` value: the canonical Plutus Data form of
 * `ValidationTraceDescriptorV1` (re-encoding must give back exactly
 * `valueHex`), held to the frozen V1 bounds (versions, step-count cap,
 * terminal verdict, verdict/rejection binding) by a core-codec round trip.
 * The hex is taken as given: a caller that admits other spellings normalises
 * it first.
 */
export const decodeCanonicalValidationTraceDescriptorData = (
  valueHex: string,
): MidgardValidationTraceDescriptor => {
  let data: Data.Static<typeof ValidationTraceDescriptorSchema>;
  try {
    data = Data.from(valueHex, ValidationTraceDescriptor);
    if (Data.to(data, ValidationTraceDescriptor) !== valueHex)
      throw new Error("re-encoding gives other bytes");
  } catch (cause) {
    throw new ValidationTraceDescriptorDecodeError(
      "data",
      `validation descriptor is not canonical for its schema: ${String(cause)}`,
      cause,
    );
  }
  try {
    return decodeMidgardValidationTraceDescriptor(
      encodeMidgardValidationTraceDescriptor(
        validationTraceDescriptorCoreFromData(data),
      ),
    );
  } catch (cause) {
    throw new ValidationTraceDescriptorDecodeError(
      "bounds",
      `validation descriptor is not a bounded V1 descriptor: ${String(cause)}`,
      cause,
    );
  }
};

export const validationTraceProofDataFromCore = (
  proof: MidgardValidationTraceProof,
): ValidationTraceProof => ({
  state_index: BigInt(proof.stateIndex),
  state_hash: bytesHex(proof.stateHash),
  siblings: proof.siblings.map(bytesHex),
});

export const validationTraceProofCoreFromData = (
  proof: ValidationTraceProof,
): MidgardValidationTraceProof => ({
  stateIndex: safeNumber(proof.state_index, "proof.state_index"),
  stateHash: Buffer.from(proof.state_hash, "hex"),
  siblings: proof.siblings.map((sibling) => Buffer.from(sibling, "hex")),
});

export const validationMachineStateDataFromCore = (
  state: MidgardValidationMachineState,
): ValidationMachineState => ({
  machine_version: BigInt(state.machineVersion),
  event_key_hash: bytesHex(state.eventKeyHash),
  transaction_id: bytesHex(state.transactionId),
  transaction_commitment: bytesHex(state.transactionCommitment),
  validation_context_hash: bytesHex(state.validationContextHash),
  source_kind: state.sourceKind === "normal" ? "Normal" : "Forced",
  prior_ledger_root: bytesHex(state.priorLedgerRoot),
  phase:
    state.phase === "canonicalDecode"
      ? "CanonicalDecode"
      : state.phase === "compactBinding"
        ? "CompactBinding"
        : state.phase === "staticLedgerRules"
          ? "StaticLedgerRules"
          : state.phase === "inputSets"
            ? "InputSets"
            : state.phase === "signatures"
              ? "Signatures"
              : state.phase === "phaseANativeScripts"
                ? "PhaseANativeScripts"
                : state.phase === "phaseAScriptPreconditions"
                  ? "PhaseAScriptPreconditions"
                  : state.phase === "resolveInputs"
                    ? "ResolveInputs"
                    : state.phase === "scriptSources"
                      ? "ScriptSources"
                      : state.phase === "nativeScripts"
                        ? "NativeScripts"
                        : state.phase === "scriptIntegrity"
                          ? "ScriptIntegrity"
                          : state.phase === "cek"
                            ? "Cek"
                            : state.phase === "valueAndMint"
                              ? "ValueAndMint"
                              : "Terminal",
  program_counter: BigInt(state.programCounter),
  work_root: bytesHex(state.workRoot),
  execution_cpu: state.executionCpu,
  execution_memory: state.executionMemory,
  verdict:
    state.verdict === "pending"
      ? "Pending"
      : state.verdict === "accepted"
        ? "Accepted"
        : "Rejected",
  rejection_code_hash: bytesHex(state.rejectionCodeHash),
});
