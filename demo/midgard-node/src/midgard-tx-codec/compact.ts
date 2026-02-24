import {
  MidgardTransactionBodyCompact,
  MidgardTransactionCompact,
  MidgardTransactionCompactLegacy,
  MidgardTransactionWitnessSetCompact,
  DecodeMode,
  EncodeMode,
} from "./types.js";
import {
  asBigInt,
  asBoolean,
  asBytes,
  asMap,
  decodeSingleCbor,
  encodeCbor,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32 } from "./hash.js";

const STRICT_TX_COMPACT_KEYS = new Set([
  "transaction_body_hash",
  "transaction_witness_set_hash",
  "is_valid",
]);

const LEGACY_TX_COMPACT_KEYS = new Set(["body", "wits", "validity"]);

const BODY_COMPACT_ALLOWED_KEYS = new Set([
  0, 1, 2, 3, 7, 8, 9, 11, 14, 15, 18, 23,
]);
const WITNESS_COMPACT_ALLOWED_KEYS = new Set([0, 1, 5, 7]);

const normalizeUnsigned = (value: bigint, fieldName: string): bigint => {
  if (value < 0n) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be unsigned`,
      value.toString(),
    );
  }
  return value;
};

const mapGetRequired = (
  map: Map<unknown, unknown>,
  key: string | number,
  fieldName: string,
): unknown => {
  if (!map.has(key)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.MissingRequiredField,
      `Missing required field ${fieldName}`,
    );
  }
  return map.get(key);
};

const ensureNoUnexpectedKeys = (
  map: Map<unknown, unknown>,
  expectedKeys: Set<string> | Set<number>,
  fieldName: string,
): void => {
  for (const key of map.keys()) {
    if (!expectedKeys.has(key as never)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidMapKey,
        `Unexpected key in ${fieldName}`,
        String(key),
      );
    }
  }
};

const decodeLegacyCompact = (
  map: Map<unknown, unknown>,
): MidgardTransactionCompact => {
  ensureNoUnexpectedKeys(
    map,
    LEGACY_TX_COMPACT_KEYS,
    "transaction_compact_legacy",
  );

  const validityRaw = mapGetRequired(map, "validity", "validity");
  let isValid = false;
  if (typeof validityRaw === "boolean") {
    isValid = validityRaw;
  } else if (typeof validityRaw === "number") {
    isValid = validityRaw === 1;
  } else if (typeof validityRaw === "string") {
    isValid =
      validityRaw === "TxIsValid" || validityRaw.toLowerCase() === "true";
  } else {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "legacy validity must be boolean, number, or string",
    );
  }

  const parsedLegacy: MidgardTransactionCompactLegacy = {
    body: ensureHash32(
      asBytes(mapGetRequired(map, "body", "body"), "body"),
      "body",
    ),
    wits: ensureHash32(
      asBytes(mapGetRequired(map, "wits", "wits"), "wits"),
      "wits",
    ),
    validity: validityRaw,
  };

  return {
    transactionBodyHash: parsedLegacy.body,
    transactionWitnessSetHash: parsedLegacy.wits,
    isValid,
  };
};

export const decodeMidgardTransactionCompact = (
  bytes: Uint8Array,
  mode: DecodeMode = "strict",
): MidgardTransactionCompact => {
  const decoded = decodeSingleCbor(bytes);
  const map = asMap(decoded, "transaction_compact");

  const maybeLegacy = map.has("body") || map.has("wits") || map.has("validity");
  if (maybeLegacy) {
    if (mode !== "dual") {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        "Legacy compact transaction is not allowed in strict mode",
      );
    }
    return decodeLegacyCompact(map);
  }

  ensureNoUnexpectedKeys(map, STRICT_TX_COMPACT_KEYS, "transaction_compact");

  return {
    transactionBodyHash: ensureHash32(
      asBytes(
        mapGetRequired(map, "transaction_body_hash", "transaction_body_hash"),
        "transaction_body_hash",
      ),
      "transaction_body_hash",
    ),
    transactionWitnessSetHash: ensureHash32(
      asBytes(
        mapGetRequired(
          map,
          "transaction_witness_set_hash",
          "transaction_witness_set_hash",
        ),
        "transaction_witness_set_hash",
      ),
      "transaction_witness_set_hash",
    ),
    isValid: asBoolean(mapGetRequired(map, "is_valid", "is_valid"), "is_valid"),
  };
};

export const encodeMidgardTransactionCompact = (
  tx: MidgardTransactionCompact,
  mode: EncodeMode = "strict",
): Buffer => {
  if (mode === "legacy") {
    return encodeCbor(
      new Map<unknown, unknown>([
        ["body", ensureHash32(tx.transactionBodyHash, "transaction_body_hash")],
        [
          "wits",
          ensureHash32(
            tx.transactionWitnessSetHash,
            "transaction_witness_set_hash",
          ),
        ],
        ["validity", tx.isValid],
      ]),
    );
  }

  return encodeCbor(
    new Map<unknown, unknown>([
      [
        "transaction_body_hash",
        ensureHash32(tx.transactionBodyHash, "transaction_body_hash"),
      ],
      [
        "transaction_witness_set_hash",
        ensureHash32(
          tx.transactionWitnessSetHash,
          "transaction_witness_set_hash",
        ),
      ],
      ["is_valid", tx.isValid],
    ]),
  );
};

const decodeOptionalHash = (
  map: Map<unknown, unknown>,
  key: number,
  fieldName: string,
): Buffer | undefined => {
  const value = map.get(key);
  if (value === undefined) {
    return undefined;
  }
  return ensureHash32(asBytes(value, fieldName), fieldName);
};

const decodeOptionalUnsigned = (
  map: Map<unknown, unknown>,
  key: number,
  fieldName: string,
): bigint | undefined => {
  const value = map.get(key);
  if (value === undefined) {
    return undefined;
  }
  return normalizeUnsigned(asBigInt(value, fieldName), fieldName);
};

export const decodeMidgardTransactionBodyCompact = (
  bytes: Uint8Array,
): MidgardTransactionBodyCompact => {
  const decoded = decodeSingleCbor(bytes);
  const map = asMap(decoded, "transaction_body_compact");

  ensureNoUnexpectedKeys(
    map,
    BODY_COMPACT_ALLOWED_KEYS,
    "transaction_body_compact",
  );

  return {
    inputsHash: ensureHash32(asBytes(mapGetRequired(map, 0, "0"), "0"), "0"),
    outputsHash: ensureHash32(asBytes(mapGetRequired(map, 1, "1"), "1"), "1"),
    fee: normalizeUnsigned(asBigInt(mapGetRequired(map, 2, "2"), "2"), "2"),
    validityIntervalEnd: decodeOptionalUnsigned(map, 3, "3"),
    auxiliaryDataHash: decodeOptionalHash(map, 7, "7"),
    validityIntervalStart: decodeOptionalUnsigned(map, 8, "8"),
    mintHash: decodeOptionalHash(map, 9, "9"),
    scriptDataHash: decodeOptionalHash(map, 11, "11"),
    requiredSignersHash: decodeOptionalHash(map, 14, "14"),
    networkId: decodeOptionalUnsigned(map, 15, "15"),
    referenceInputsHash: decodeOptionalHash(map, 18, "18"),
    requiredObserversHash: decodeOptionalHash(map, 23, "23"),
  };
};

export const encodeMidgardTransactionBodyCompact = (
  body: MidgardTransactionBodyCompact,
): Buffer => {
  const map = new Map<unknown, unknown>([
    [0, ensureHash32(body.inputsHash, "0")],
    [1, ensureHash32(body.outputsHash, "1")],
    [2, normalizeUnsigned(body.fee, "2")],
  ]);

  const putUnsigned = (
    key: number,
    value: bigint | undefined,
    fieldName: string,
  ) => {
    if (value !== undefined) {
      map.set(key, normalizeUnsigned(value, fieldName));
    }
  };

  const putHash = (
    key: number,
    value: Buffer | undefined,
    fieldName: string,
  ) => {
    if (value !== undefined) {
      map.set(key, ensureHash32(value, fieldName));
    }
  };

  putUnsigned(3, body.validityIntervalEnd, "3");
  putHash(7, body.auxiliaryDataHash, "7");
  putUnsigned(8, body.validityIntervalStart, "8");
  putHash(9, body.mintHash, "9");
  putHash(11, body.scriptDataHash, "11");
  putHash(14, body.requiredSignersHash, "14");
  putUnsigned(15, body.networkId, "15");
  putHash(18, body.referenceInputsHash, "18");
  putHash(23, body.requiredObserversHash, "23");

  return encodeCbor(map);
};

export const decodeMidgardTransactionWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardTransactionWitnessSetCompact => {
  const decoded = decodeSingleCbor(bytes);
  const map = asMap(decoded, "transaction_witness_set_compact");

  ensureNoUnexpectedKeys(
    map,
    WITNESS_COMPACT_ALLOWED_KEYS,
    "transaction_witness_set_compact",
  );

  return {
    vkeyWitnessesHash: decodeOptionalHash(map, 0, "0"),
    nativeScriptsHash: decodeOptionalHash(map, 1, "1"),
    redeemersHash: decodeOptionalHash(map, 5, "5"),
    plutusV3ScriptsHash: decodeOptionalHash(map, 7, "7"),
  };
};

export const encodeMidgardTransactionWitnessSetCompact = (
  witnessSet: MidgardTransactionWitnessSetCompact,
): Buffer => {
  const map = new Map<unknown, unknown>();

  if (witnessSet.vkeyWitnessesHash !== undefined) {
    map.set(0, ensureHash32(witnessSet.vkeyWitnessesHash, "0"));
  }
  if (witnessSet.nativeScriptsHash !== undefined) {
    map.set(1, ensureHash32(witnessSet.nativeScriptsHash, "1"));
  }
  if (witnessSet.redeemersHash !== undefined) {
    map.set(5, ensureHash32(witnessSet.redeemersHash, "5"));
  }
  if (witnessSet.plutusV3ScriptsHash !== undefined) {
    map.set(7, ensureHash32(witnessSet.plutusV3ScriptsHash, "7"));
  }

  return encodeCbor(map);
};
