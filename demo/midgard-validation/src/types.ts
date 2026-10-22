import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import type { MidgardValue } from "@al-ft/midgard-core/codec";
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type { Effect } from "effect";

import type {
  MidgardCekExecutionGraph,
  MidgardCekExecutionMode,
  MidgardCekStructuralExecution,
} from "./cek-executor.js";
import type { LedgerEntry } from "./ledger.js";
import type { MidgardForcedTxOutputFault } from "./ledger-tx/codec.locate-forced-tx-item-fault.js";
import type { MidgardLedgerTx } from "./ledger-tx/types.js";
import type { MidgardCekVerdictRequest } from "./phase-b.evaluate-cek-script.js";
import type { RejectSubject } from "./reject-subject.js";
import type { ScriptDecodePhase2Class } from "./script-decode.step-types.js";

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
  MinAda: "E_MIN_ADA",
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
  TxVersion: "E_TX_VERSION",
  TxSize: "E_TX_SIZE",
  ValueSize: "E_VALUE_SIZE",
  InputCount: "E_INPUT_COUNT",
  ReferenceInputCount: "E_REFERENCE_INPUT_COUNT",
  OutputCount: "E_OUTPUT_COUNT",
  AddressWitnessCount: "E_ADDRESS_WITNESS_COUNT",
  RequiredSignerCount: "E_REQUIRED_SIGNER_COUNT",
  ScriptExecutionCount: "E_SCRIPT_EXECUTION_COUNT",
  ObserverCount: "E_OBSERVER_COUNT",
  FieldPreimageSize: "E_FIELD_PREIMAGE_SIZE",
  LedgerOutputSize: "E_LEDGER_OUTPUT_SIZE",
  DatumSize: "E_DATUM_SIZE",
  ScriptProgramSize: "E_SCRIPT_PROGRAM_SIZE",
  ScriptProgramEncoding: "E_SCRIPT_PROGRAM_ENCODING",
  ScriptProgramAggregateSize: "E_SCRIPT_PROGRAM_AGGREGATE_SIZE",
  NativeScriptDepth: "E_NATIVE_SCRIPT_DEPTH",
  NativeScriptNodeCount: "E_NATIVE_SCRIPT_NODE_COUNT",
  RedeemerSize: "E_REDEEMER_SIZE",
  AssetCount: "E_ASSET_COUNT",
  MintForbidden: "E_MINT_FORBIDDEN",
  ReferenceInputForbidden: "E_REFERENCE_INPUT_FORBIDDEN",
  ScriptFeatureForbidden: "E_SCRIPT_FEATURE_FORBIDDEN",
  TxExUnits: "E_TX_EX_UNITS",
} as const;

export type RejectCode = (typeof RejectCodes)[keyof typeof RejectCodes];

/**
 * Transaction as queued for validation before any expensive decoding or
 * signature work has happened.
 */
export type QueuedTx = {
  readonly sourceKind?: "normal" | "forced";
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
    readonly sourceKind: "normal" | "forced";
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
    /** Deployment network admitted with this Phase-A candidate. */
    readonly expectedNetworkId: bigint;
    readonly outputSum: MidgardValue;
    readonly mintDelta: MidgardValueDelta;
    readonly witnessKeyHashHexes: readonly string[];
    readonly nativeScriptHashHexes: readonly string[];
    readonly plutusScriptHashHexes: readonly string[];
    readonly requiredObserverHashHexes: readonly string[];
    readonly mintPolicyHashHexes: readonly string[];
    readonly redeemerWitnessHash: Buffer;
    readonly requiresLocalScriptDiscovery: boolean;
  };

  /**
   * A forced transaction's first faulty output, with its class.
   * `ledgerTx.outputs` stops before it, and Phase B refuses the transaction
   * at the ScriptSources output scan, where the machine reads it.
   */
  readonly heldOutputFault?: MidgardForcedTxOutputFault;
};

/**
 * Canonical rejection record emitted by both validation phases.
 */
export type RejectedTx = {
  readonly txId: Buffer;
  readonly code: RejectCode;
  readonly detail: string | null;
  /**
   * Exact consensus-machine instruction that produced this rejection.
   *
   * V1 trace construction fails closed when it is absent. The code alone is
   * not sufficient because, for example, a missing witness can be discovered
   * during signer, input-resolution, or script-source validation.
   */
  readonly consensusPhase?: MidgardValidationPhaseName;
  /**
   * The arm and subject coordinates of the failed rule, where the rule names
   * one. A forced verdict cites them; see {@link RejectSubject}.
   */
  readonly subject?: RejectSubject;
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
  readonly consensusProfile?: MidgardConsensusProfile;
};

/** Process-local evidence from the single script evaluation used for its verdict. */
export type LocalScriptEvaluation = {
  readonly scriptBytes: Buffer;
  readonly contextCbor: Buffer;
  readonly executionIndex: bigint;
  readonly executionBudget:
    | { readonly cpu: bigint; readonly memory: bigint }
    | undefined;
  readonly result: LocalScriptEvalResult;
  /** Present on proof-mode evaluation, before any CEK graph is constructed. */
  readonly phase2Class?: ScriptDecodePhase2Class;
  readonly graph: MidgardCekExecutionGraph | null;
  readonly execution: MidgardCekStructuralExecution | null;
};

/** The verdict and spent budget of one script evaluation. */
export type LocalScriptEvalResult =
  | {
      readonly kind: "accepted";
      readonly budget: {
        readonly cpu: bigint;
        readonly memory: bigint;
      };
    }
  | { readonly kind: "script_invalid"; readonly detail: string }
  /**
   * The evaluator itself failed: a throw, one of its own bounds, or a
   * self-check. Not a verdict: Phase B fails with
   * `ScriptEvaluationIndeterminateError` carrying `cause`.
   */
  | {
      readonly kind: "indeterminate";
      readonly detail: string;
      readonly cause?: unknown;
    };

/**
 * Configuration knobs for phase-B validation.
 */
export type PhaseBConfig = {
  readonly nowCardanoSlotNo: bigint;
  /**
   * The deployment manifest's slot mapping. Plutus contexts carry the body's
   * slot interval in POSIX milliseconds through it, as the deployed
   * transaction-info validator does.
   */
  readonly l1SlotConfig: L1SlotConfig;
  readonly bucketConcurrency: number;
  readonly enforceScriptBudget?: boolean;
  readonly maxScriptExecutionSteps?: number;
  /**
   * `verdict` (the default) runs CEK scripts without per-step verification or
   * retention, so each `onScriptEvaluated` report has `execution: null`. A
   * caller that builds a validation trace from the reported executions must
   * pass `proof`.
   */
  readonly cekExecutionMode?: MidgardCekExecutionMode;
  readonly onScriptEvaluated?: (
    txId: Buffer,
    evaluation: LocalScriptEvaluation,
  ) => void;
  readonly evaluateProofScript?: (
    scriptBytes: Uint8Array,
    contextCbor: Uint8Array,
    executionBudget?: {
      readonly cpu: bigint;
      readonly memory: bigint;
    },
    executionIndex?: bigint,
  ) => Effect.Effect<LocalScriptEvalResult, Error>;
  /**
   * Computes a verdict-mode script run elsewhere, such as on a worker thread.
   * It must return exactly what `evaluateMidgardCekVerdict` returns for the
   * request. Used only in verdict mode without `evaluateProofScript`.
   */
  readonly evaluateCekVerdict?: (
    request: MidgardCekVerdictRequest,
  ) => Effect.Effect<LocalScriptEvalResult, Error>;
  /**
   * Applies the transaction by its committed Accepted verdict: each
   * non-native script run is handed here instead of running, without its
   * script context being built, and counts as accepted within its declared
   * budget. Takes precedence over every evaluation hook.
   */
  readonly committedScriptRun?: (run: MidgardCommittedScriptRun) => void;
};

/** A script run a committed-verdict replay hands on: the run without its
 * script context, which a later check rebuilds. */
export type MidgardCommittedScriptRun = Omit<
  MidgardCekVerdictRequest,
  "contextCbor"
>;

/**
 * Serializable subset of queued-transaction metadata used at process
 * boundaries.
 */
export type QueuedTxPayload = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly createdAtMillis: number;
};
