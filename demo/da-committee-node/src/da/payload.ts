import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { sha256 } from "@noble/hashes/sha2.js";

import type { Header, PayloadRootSet, ValidationSummary } from "../domain.js";
import { bytesToHex, hexToBytes, normalizeHex } from "../utils/hex.js";

export type TransactionRootValueProjector = (
  fullTransactionCbor: Buffer,
) => Buffer | Promise<Buffer>;

export type PayloadVerificationOptions = {
  readonly transactionProjector?: TransactionRootValueProjector;
  readonly stateQueueOutRef: string;
};

export type VerifiedDaPayload = {
  readonly payload: SDK.DaPayloadV1;
  readonly payloadCbor: Buffer;
  readonly payloadSha256: string;
  readonly roots: PayloadRootSet;
  readonly validation: ValidationSummary;
};

export class DaPayloadValidationError extends Error {
  readonly code:
    | "malformed_da"
    | "non_canonical"
    | "wrong_version"
    | "duplicate_key"
    | "unsorted_key"
    | "header_hash_mismatch"
    | "malformed_transaction"
    | "root_mismatch";

  constructor(
    code: DaPayloadValidationError["code"],
    message: string,
    options?: ErrorOptions,
  ) {
    super(message, options);
    this.name = "DaPayloadValidationError";
    this.code = code;
  }
}

export const decodeDaPayloadV1Strict = (
  payloadCbor: Uint8Array,
): SDK.DaPayloadV1 => {
  const payloadBuffer = Buffer.from(payloadCbor);
  let payload: SDK.DaPayloadV1;
  try {
    payload = SDK.decodeDaPayloadV1(payloadBuffer);
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_da",
      "failed to decode DaPayloadV1 canonical CBOR",
      { cause },
    );
  }
  const canonical = SDK.encodeDaPayloadV1(payload);
  if (!canonical.equals(payloadBuffer)) {
    throw new DaPayloadValidationError(
      "non_canonical",
      "payload CBOR was not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_V1_VERSION) {
    throw new DaPayloadValidationError(
      "wrong_version",
      `expected DaPayloadV1 version ${SDK.DA_PAYLOAD_V1_VERSION.toString()}, got ${payload.version.toString()}`,
    );
  }
  normalizeHex(payload.header_hash, {
    fieldName: "payload header_hash",
    byteLength: 28,
  });
  validateEntries("utxos", payload.block_body.utxos);
  validateEntries("transactions", payload.block_body.transactions);
  validateEntries("deposits", payload.block_body.deposits);
  validateEntries("withdrawals", payload.block_body.withdrawals);
  return payload;
};

export const computeDaPayloadRoots = async (
  payload: SDK.DaPayloadV1,
  transactionProjector: TransactionRootValueProjector =
    defaultTransactionProjector,
): Promise<PayloadRootSet> => {
  const transactionValues: Buffer[] = [];
  for (const [, value] of payload.block_body.transactions) {
    try {
      transactionValues.push(
        Buffer.from(await transactionProjector(hexToBytes(value, "tx value"))),
      );
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        "failed to project full transaction CBOR to compact root value",
        { cause },
      );
    }
  }
  const [utxosRoot, transactionsRoot, depositsRoot, withdrawalsRoot] =
    await Promise.all([
      keyValuePhasRoot(payload.block_body.utxos),
      keyValuePhasRootWithValues(
        payload.block_body.transactions.map(([key]) =>
          hexToBytes(key, "tx key"),
        ),
        transactionValues,
      ),
      keyValuePhasRoot(payload.block_body.deposits),
      keyValuePhasRoot(payload.block_body.withdrawals),
    ]);
  return { utxosRoot, transactionsRoot, depositsRoot, withdrawalsRoot };
};

export const verifyDaPayloadAgainstHeader = async (
  payloadCbor: Uint8Array,
  expectedHeaderHash: string,
  header: Header,
  options: PayloadVerificationOptions,
): Promise<VerifiedDaPayload> => {
  const normalizedHeaderHash = normalizeHex(expectedHeaderHash, {
    fieldName: "expected header hash",
    byteLength: 28,
  });
  const payload = decodeDaPayloadV1Strict(payloadCbor);
  if (payload.header_hash !== normalizedHeaderHash) {
    throw new DaPayloadValidationError(
      "header_hash_mismatch",
      `payload header_hash ${payload.header_hash} does not match L1 header hash ${normalizedHeaderHash}`,
    );
  }
  const roots = await computeDaPayloadRoots(
    payload,
    options.transactionProjector,
  );
  const mismatches = rootMismatches(header, roots);
  if (mismatches.length > 0) {
    throw new DaPayloadValidationError(
      "root_mismatch",
      `payload roots do not match L1 header: ${mismatches.join(",")}`,
    );
  }
  const payloadBuffer = Buffer.from(payloadCbor);
  return {
    payload,
    payloadCbor: payloadBuffer,
    payloadSha256: bytesToHex(sha256(payloadBuffer)),
    roots,
    validation: {
      payloadVersion: Number(payload.version),
      rootsMatch: true,
      stateQueueOutRef: options.stateQueueOutRef,
      headerHash: normalizedHeaderHash,
      rootSummary: roots,
      l1Header: {
        startTime: header.startTime.toString(),
        endTime: header.endTime.toString(),
        operatorVkey: header.operatorVkey,
        prevHeaderHash: header.prevHeaderHash,
        protocolVersion: header.protocolVersion.toString(),
      },
    },
  };
};

export const daPayloadSha256 = (payloadCbor: Uint8Array): string =>
  bytesToHex(sha256(payloadCbor));

const validateEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): void => {
  let previousKey: string | undefined;
  for (const [index, [key, value]] of entries.entries()) {
    const normalizedKey = normalizeHex(key, {
      fieldName: `${fieldName}[${index.toString()}].key`,
    });
    normalizeHex(value, {
      fieldName: `${fieldName}[${index.toString()}].value`,
    });
    if (previousKey !== undefined) {
      if (normalizedKey === previousKey) {
        throw new DaPayloadValidationError(
          "duplicate_key",
          `${fieldName} contains duplicate key ${normalizedKey}`,
        );
      }
      if (normalizedKey < previousKey) {
        throw new DaPayloadValidationError(
          "unsorted_key",
          `${fieldName} keys must be sorted ascending`,
        );
      }
    }
    previousKey = normalizedKey;
  }
};

const keyValuePhasRoot = async (
  entries: readonly SDK.DaPayloadEntry[],
): Promise<string> =>
  keyValuePhasRootWithValues(
    entries.map(([key]) => hexToBytes(key, "entry key")),
    entries.map(([, value]) => hexToBytes(value, "entry value")),
  );

const keyValuePhasRootWithValues = async (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Promise<string> => {
  if (keys.length !== values.length) {
    throw new Error(
      `cannot build PHAS root for ${keys.length.toString()} keys and ${values.length.toString()} values`,
    );
  }
  if (keys.length === 0) {
    return SDK.EMPTY_MERKLE_TREE_ROOT;
  }
  const trie = await Trie.fromList(
    keys.map((key, index) => ({
      key: Buffer.from(key),
      value: Buffer.from(values[index]!),
    })),
  );
  return Buffer.from(trie.hash).toString("hex");
};

const defaultTransactionProjector: TransactionRootValueProjector = (
  fullTransactionCbor,
) =>
  Buffer.from(
    encodeMidgardNativeTxCompact(
      decodeMidgardNativeTxFullFromCanonicalCbor(fullTransactionCbor).compact,
    ),
  );

const rootMismatches = (
  header: Header,
  roots: PayloadRootSet,
): readonly string[] =>
  [
    header.utxosRoot === roots.utxosRoot ? undefined : "utxos_root",
    header.transactionsRoot === roots.transactionsRoot
      ? undefined
      : "transactions_root",
    header.depositsRoot === roots.depositsRoot ? undefined : "deposits_root",
    header.withdrawalsRoot === roots.withdrawalsRoot
      ? undefined
      : "withdrawals_root",
  ].filter((field): field is string => field !== undefined);
