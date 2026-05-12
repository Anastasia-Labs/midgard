import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { Transaction, OutputReference, TransactionOutput } from "../index";

// ---------------------------------------------------------------------------
// Reject codes
// ---------------------------------------------------------------------------

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
  IsValidFalseForbidden: "E_IS_VALID_FALSE_FORBIDDEN",
  AuxDataForbidden: "E_AUX_DATA_FORBIDDEN",
  MintForbidden: "E_MINT_FORBIDDEN",
  NetworkIdMismatch: "E_NETWORK_ID_MISMATCH",
} as const;

export type RejectCode = (typeof RejectCodes)[keyof typeof RejectCodes];

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

/**
 * A decoded transaction submitted for validation.
 *
 * `txId` is the expected 32-byte Cardano transaction body hash
 * (blake2b-256 of the CBOR-encoded tx body).  Phase A verifies that the
 * decoded transaction actually hashes to this value (R2).
 */
export type QueuedTx = {
  readonly txId: Uint8Array;
  readonly tx: Transaction;
  readonly arrivalSeq: bigint;
};

// ---------------------------------------------------------------------------
// Output
// ---------------------------------------------------------------------------

/** One output produced by an accepted transaction. */
export type ProducedEntry = {
  readonly outRef: OutputReference;
  readonly output: TransactionOutput;
};

/**
 * Result of a successful Phase A validation for one transaction.
 * Carries everything Phase B needs for stateful checks.
 */
export type PhaseAAccepted = {
  readonly txId: Uint8Array;
  readonly tx: Transaction;
  readonly arrivalSeq: bigint;
  readonly fee: bigint;
  readonly validityIntervalStart: number | undefined;
  readonly validityIntervalEnd: number | undefined;
  readonly referenceInputs: readonly OutputReference[];
  /** Sum of all output values, as a CML.Value (used for value preservation). */
  readonly outputSum: InstanceType<typeof CML.Value>;
  readonly witnessKeyHashes: readonly string[];
  readonly nativeScriptHashes: readonly string[];
  readonly spent: readonly OutputReference[];
  readonly produced: readonly ProducedEntry[];
};

export type RejectedTx = {
  readonly txId: Uint8Array;
  readonly code: RejectCode;
  readonly detail: string | null;
};

export type PhaseAResult = {
  readonly accepted: readonly PhaseAAccepted[];
  readonly rejected: readonly RejectedTx[];
};

export type PhaseBResult = {
  readonly accepted: readonly PhaseAAccepted[];
  readonly rejected: readonly RejectedTx[];
};

// ---------------------------------------------------------------------------
// Config
// ---------------------------------------------------------------------------

export type PhaseAConfig = {
  /** 0 = testnet, 1 = mainnet.  Transactions with a different network_id are
   * rejected (R24). */
  readonly expectedNetworkId: number;
  /** Lovelace per encoded byte (Cardano minFeeA parameter). */
  readonly minFeeA: bigint;
  /** Constant lovelace term (Cardano minFeeB parameter). */
  readonly minFeeB: bigint;
  /**
   * Cardano network byte used when reconstructing zero-ADA withdrawals from
   * required_observers during tx body hash computation (R2).
   * Defaults to 1 (mainnet).
   */
  readonly cardanoNetwork?: number;
};

export type PhaseBConfig = {
  /** Current wall-clock time in milliseconds (POSIX ms). Used for R10. */
  readonly nowMillis: number;
};

// ---------------------------------------------------------------------------
// UTxO state
// ---------------------------------------------------------------------------

/**
 * UTxO state: maps an outref key (see `outRefKey`) to a TransactionOutput.
 *
 * Key format: `<txid-hex>:<output-index>`
 * e.g. `"aabb...00:0"`.
 */
export type UTxOState = Map<string, TransactionOutput>;

export type UTxOStatePatch = {
  readonly deletedOutRefs: readonly string[];
  readonly upsertedOutRefs: readonly [string, TransactionOutput][];
};

export type PhaseBResultWithPatch = PhaseBResult & {
  readonly statePatch: UTxOStatePatch;
};
