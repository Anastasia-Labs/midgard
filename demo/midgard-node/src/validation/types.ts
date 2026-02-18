import { ProcessedTx } from "@/utils.js";

export const RejectCodes = {
  CborDeserialization: "E_CBOR_DESERIALIZATION",
  TxHashMismatch: "E_TX_HASH_MISMATCH",
  UnsupportedFieldNonEmpty: "E_UNSUPPORTED_FIELD_NONEMPTY",
  EmptyInputs: "E_EMPTY_INPUTS",
  DuplicateInputInTx: "E_DUPLICATE_INPUT_IN_TX",
  InvalidOutput: "E_INVALID_OUTPUT",
  InputNotFound: "E_INPUT_NOT_FOUND",
  DoubleSpend: "E_DOUBLE_SPEND",
  InvalidValidityIntervalFormat: "E_INVALID_VALIDITY_INTERVAL_FORMAT",
  ValidityIntervalMismatch: "E_VALIDITY_INTERVAL_MISMATCH",
  MinFee: "E_MIN_FEE",
  ValueNotPreserved: "E_VALUE_NOT_PRESERVED",
  MissingRequiredWitness: "E_MISSING_REQUIRED_WITNESS",
  InvalidSignature: "E_INVALID_SIGNATURE",
  NativeScriptInvalid: "E_NATIVE_SCRIPT_INVALID",
  IsValidFalseForbidden: "E_IS_VALID_FALSE_FORBIDDEN",
  AuxDataForbidden: "E_AUX_DATA_FORBIDDEN",
  CertificatesForbidden: "E_CERTIFICATES_FORBIDDEN",
  NonZeroWithdrawal: "E_NONZERO_WITHDRAWAL",
  MintForbidden: "E_MINT_FORBIDDEN",
  NetworkIdMismatch: "E_NETWORK_ID_MISMATCH",
} as const;

export type RejectCode = (typeof RejectCodes)[keyof typeof RejectCodes];

export type QueuedTx = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly arrivalSeq: bigint;
  readonly createdAt: Date;
};

export type PhaseAAccepted = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly arrivalSeq: bigint;
  readonly fee: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly referenceInputs: readonly Buffer[];
  readonly witnessKeyHashes: readonly string[];
  readonly nativeScriptHashes: readonly string[];
  readonly processedTx: ProcessedTx;
};

export type RejectedTx = {
  readonly txId: Buffer;
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

export type PhaseAConfig = {
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly concurrency: number;
  readonly strictnessProfile: string;
};

export type PhaseBConfig = {
  readonly nowMillis: bigint;
};
