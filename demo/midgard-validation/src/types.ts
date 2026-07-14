import type { MidgardValue } from "@al-ft/midgard-core/codec";
import type { Effect } from "effect";

import type { LedgerEntry } from "./ledger.js";
import type {
  MidgardLedgerTx,
  MidgardLedgerVKeyWitness,
} from "./ledger-tx/types.js";
import type { LocalScriptEvalResult } from "./local-script-eval.js";

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
 * Signed value delta used for ledger accounting.
 *
 * `MidgardValue` represents spendable output values and therefore uses only
 * positive asset quantities. Minting is a signed ledger delta, so Phase B uses
 * this shape when checking value preservation.
 */
export type MidgardValueDelta = {
  /** Signed lovelace delta. Minting currently leaves this at zero. */
  readonly lovelace: bigint;

  /** Signed per-asset deltas keyed by policy id hex and asset-name hex. */
  readonly assets: ReadonlyMap<string, ReadonlyMap<string, bigint>>;
};

/**
 * Phase-A output for transactions that passed stateless validation and can
 * progress to dependency-aware ledger validation.
 *
 * `ledgerTx` is authoritative for transaction semantics. `submission` carries
 * admission/proof material. `graph` and `derived` are Phase-A projections used
 * by Phase B for throughput and must be produced from `ledgerTx`, not authored
 * independently by callers.
 */
export type PhaseAValidatedTx = {
  /** Fully decoded ledger transaction accepted by Phase A. */
  readonly ledgerTx: MidgardLedgerTx;

  /** Non-ledger submission metadata retained for ordering and persistence. */
  readonly submission: {
    readonly txCbor: Buffer;
    readonly arrivalSeq: bigint;
    readonly createdAt: Date;
  };

  /** Dependency graph and state-patch projection consumed by Phase B. */
  readonly graph: {
    readonly spentOutRefHexes: readonly string[];
    readonly referenceOutRefHexes: readonly string[];
    readonly produced: readonly LedgerEntry[];
  };

  /** Non-authoritative projections derived from `ledgerTx` for validation. */
  readonly derived: {
    readonly outputSum: MidgardValue;
    readonly mintDelta: MidgardValueDelta;
    readonly witnessKeyHashHexes: readonly string[];
    readonly nativeScriptHashHexes: readonly string[];
    readonly plutusScriptHashHexes: readonly string[];
    readonly requiredObserverHashHexes: readonly string[];
    readonly mintPolicyHashHexes: readonly string[];
    readonly redeemerWitnessHash: Buffer;
    readonly requiresScriptEvaluation: boolean;
    readonly requiresLocalScriptDiscovery: boolean;
  };
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
  readonly accepted: readonly PhaseAValidatedTx[];
  readonly rejected: readonly RejectedTx[];
};

/**
 * Result shape for dependency-aware validation.
 */
export type PhaseBResult = {
  readonly accepted: readonly PhaseAValidatedTx[];
  readonly rejected: readonly RejectedTx[];
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
 * Process-local Phase-A extension points.
 *
 * This context is deliberately separate from {@link PhaseAConfig}: functions
 * must never enter the serializable worker configuration or wire protocol.
 * Browser and inline callers omit it and retain the CML reference verifier.
 */
export type PhaseALocalContext = {
  readonly verifyVKeyWitnessSignature?: (
    txBodyHash: Buffer,
    witness: MidgardLedgerVKeyWitness,
  ) => boolean;
};

/**
 * Configuration knobs for phase-B validation.
 */
export type PhaseBConfig = {
  readonly nowCardanoSlotNo: bigint;
  readonly bucketConcurrency: number;
  readonly enforceScriptBudget?: boolean;
  readonly evaluateScript?: (
    scriptBytes: Uint8Array,
    contextCbor: Uint8Array,
  ) => Effect.Effect<LocalScriptEvalResult, Error>;
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
