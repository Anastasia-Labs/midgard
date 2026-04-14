import { ProcessedTx } from "@/utils.js";
import { CML, type UTxO } from "@lucid-evolution/lucid";

/**
 * Stable rejection codes used by Midgard phase-A and phase-B validation.
 *
 * The validation pipeline persists these codes for observability and replay, so
 * they should change only when the corresponding rejection semantics change.
 */
export const RejectCodes = {
  CborDeserialization: "E_CBOR_DESERIALIZATION",
  TxHashMismatch: "E_TX_HASH_MISMATCH",
  UnsupportedFieldNonEmpty: "E_UNSUPPORTED_FIELD_NONEMPTY",
  EmptyInputs: "E_EMPTY_INPUTS",
  DuplicateInputInTx: "E_DUPLICATE_INPUT_IN_TX",
  InvalidOutput: "E_INVALID_OUTPUT",
  InvalidFieldType: "E_INVALID_FIELD_TYPE",
  InputNotFound: "E_INPUT_NOT_FOUND",
  DoubleSpend: "E_DOUBLE_SPEND",
  DependencyCycle: "E_DEPENDENCY_CYCLE",
  DependsOnRejectedTx: "E_DEPENDS_ON_REJECTED_TX",
  InvalidValidityIntervalFormat: "E_INVALID_VALIDITY_INTERVAL_FORMAT",
  ValidityIntervalMismatch: "E_VALIDITY_INTERVAL_MISMATCH",
  MinFee: "E_MIN_FEE",
  ValueNotPreserved: "E_VALUE_NOT_PRESERVED",
  MissingRequiredWitness: "E_MISSING_REQUIRED_WITNESS",
  InvalidSignature: "E_INVALID_SIGNATURE",
  NativeScriptInvalid: "E_NATIVE_SCRIPT_INVALID",
  PlutusScriptInvalid: "E_PLUTUS_SCRIPT_INVALID",
  PlutusEvaluationUnavailable: "E_PLUTUS_EVALUATION_UNAVAILABLE",
  IsValidFalseForbidden: "E_IS_VALID_FALSE_FORBIDDEN",
  AuxDataForbidden: "E_AUX_DATA_FORBIDDEN",
  CertificatesForbidden: "E_CERTIFICATES_FORBIDDEN",
  NonZeroWithdrawal: "E_NONZERO_WITHDRAWAL",
  NetworkIdMismatch: "E_NETWORK_ID_MISMATCH",
} as const;

export type RejectCode = (typeof RejectCodes)[keyof typeof RejectCodes];

/**
 * Transaction as queued for validation before any expensive decoding or
 * signature work has happened.
 */
export type QueuedTx = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly arrivalSeq: bigint;
  readonly createdAt: Date;
};

/**
 * Phase-A output for transactions that passed structural and witness checks and
 * can progress to dependency-aware validation.
 */
export type PhaseAAccepted = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly arrivalSeq: bigint;
  readonly fee: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly referenceInputs: readonly Buffer[];
  readonly outputSum: InstanceType<typeof CML.Value>;
  readonly witnessKeyHashes: readonly string[];
  readonly requiredObserverHashes: readonly string[];
  readonly mintPolicyHashes: readonly string[];
  readonly mintedValue: InstanceType<typeof CML.Value>;
  readonly burnedValue: InstanceType<typeof CML.Value>;
  readonly nativeScriptHashes: readonly string[];
  readonly plutusScriptHashes: readonly string[];
  readonly requiresPlutusEvaluation: boolean;
  readonly processedTx: ProcessedTx;
};

/**
 * Canonical rejection record emitted by both validation phases.
 */
export type RejectedTx = {
  readonly txId: Buffer;
  readonly code: RejectCode;
  readonly detail: string | null;
};

/**
 * Result shape for stateless / per-transaction validation.
 */
export type PhaseAResult = {
  readonly accepted: readonly PhaseAAccepted[];
  readonly rejected: readonly RejectedTx[];
};

/**
 * Result shape for dependency-aware validation.
 */
export type PhaseBResult = {
  readonly accepted: readonly PhaseAAccepted[];
  readonly rejected: readonly RejectedTx[];
};

/**
 * Result of Plutus witness evaluation once the transaction has been
 * reconstructed into Cardano shape.
 *
 * Script-invalid outcomes are part of normal transaction validation. Anything
 * that prevents the evaluator from reaching a trustworthy answer must be raised
 * as an exception instead so the caller can retry or surface infrastructure
 * failure without permanently rejecting the tx.
 */
export type PlutusEvaluationResult =
  | {
      readonly kind: "accepted";
    }
  | {
      readonly kind: "script_invalid";
      readonly detail: string;
    };

/**
 * Configuration knobs for phase-A validation.
 */
export type PhaseAConfig = {
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly concurrency: number;
  readonly strictnessProfile: string;
};

/**
 * Configuration knobs for phase-B validation.
 */
export type PhaseBConfig = {
  readonly nowCardanoSlotNo: bigint;
  readonly bucketConcurrency: number;
  readonly evaluatePlutusTx?: (args: {
    readonly txId: Buffer;
    readonly txCborHex: string;
    readonly additionalUtxos: readonly UTxO[];
  }) => Promise<PlutusEvaluationResult>;
};

/**
 * Serializable subset of queued-transaction metadata used at process
 * boundaries.
 */
export type QueuedTxPayload = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly createdAtMillis: number;
};
