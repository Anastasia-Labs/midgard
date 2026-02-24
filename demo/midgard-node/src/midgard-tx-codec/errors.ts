export const MidgardTxCodecErrorCodes = {
  CborDecode: "E_CBOR_DECODE",
  CborEncode: "E_CBOR_ENCODE",
  InvalidMapKey: "E_INVALID_MAP_KEY",
  InvalidFieldType: "E_INVALID_FIELD_TYPE",
  MissingRequiredField: "E_MISSING_REQUIRED_FIELD",
  DisallowedField: "E_DISALLOWED_FIELD",
  HashMismatch: "E_HASH_MISMATCH",
  SchemaMismatch: "E_SCHEMA_MISMATCH",
  UnsupportedCompactToCardano: "E_UNSUPPORTED_COMPACT_TO_CARDANO",
  ConversionUnsupportedFeature: "E_CONVERSION_UNSUPPORTED_FEATURE",
} as const;

export type MidgardTxCodecErrorCode =
  (typeof MidgardTxCodecErrorCodes)[keyof typeof MidgardTxCodecErrorCodes];

export class MidgardTxCodecError extends Error {
  readonly code: MidgardTxCodecErrorCode;
  readonly detail: string | null;

  constructor(code: MidgardTxCodecErrorCode, message: string, detail?: string) {
    super(message);
    this.name = "MidgardTxCodecError";
    this.code = code;
    this.detail = detail ?? null;
  }
}
