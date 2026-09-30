import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { transitionTraceError } from "./errors.js";
import {
  canonicalizeKeyValuePhasEntries,
  type KeyValuePhasEntry,
} from "./phas.js";
import {
  type DataSchema,
  decodeData,
  type DecodedRootEntry,
  type DecodedTransactionEntry,
  entryBuffer,
  normalizeHeaderHash,
  validateDeclaredCounts,
  validatePayloadEntryArray,
} from "./reconstruct.transition-trace-reconstruction.js";

export const decodePayloadStrict = (payloadCbor: Uint8Array): SDK.DaPayload => {
  const buffer = Buffer.from(payloadCbor);
  let payload: SDK.DaPayload;
  try {
    payload = SDK.decodeDaPayload(buffer);
  } catch (cause) {
    throw transitionTraceError(
      "malformedPayload",
      "Failed to decode DaPayloadV1 canonical CBOR.",
      cause,
    );
  }
  if (!SDK.encodeDaPayload(payload).equals(buffer)) {
    throw transitionTraceError(
      "nonCanonicalPayload",
      "DA payload CBOR is not canonical for DaPayloadV1.",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
    throw transitionTraceError(
      "wrongPayloadVersion",
      `Expected DA payload version ${SDK.DA_PAYLOAD_VERSION.toString()}, got ${payload.version.toString()}.`,
    );
  }
  normalizeHeaderHash(payload.block_body.header_hash, "payload header_hash");
  validatePayloadEntryArray("utxos", payload.block_body.utxos);
  validatePayloadEntryArray("withdrawals", payload.block_body.withdrawals);
  validatePayloadEntryArray(
    "forced_transactions",
    payload.block_body.forced_transactions,
  );
  validatePayloadEntryArray("transactions", payload.block_body.transactions);
  validatePayloadEntryArray("deposits", payload.block_body.deposits);
  validatePayloadEntryArray(
    "transition_trace",
    payload.block_body.transition_trace,
  );
  validatePayloadEntryArray("event_to_step", payload.block_body.event_to_step);
  validatePayloadEntryArray(
    "transaction_preimages",
    payload.block_body.transaction_preimages,
  );
  validatePayloadEntryArray(
    "forced_transaction_preimages",
    payload.block_body.forced_transaction_preimages,
  );
  validatePayloadEntryArray(
    "cek_program_material",
    payload.block_body.cek_program_material,
  );
  validatePayloadEntryArray(
    "validation_traces",
    payload.block_body.validation_traces,
  );
  validateDeclaredCounts(payload);
  return payload;
};

export const validateDenseTransitionTrace = (
  entries: readonly DecodedRootEntry<bigint, SDK.TransitionStep>[],
  expectedCount: bigint,
): void => {
  const seen = new Set<bigint>();
  for (const [index, entry] of entries.entries()) {
    if (entry.key !== entry.value.step_index) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `transition_trace[${index.toString()}].key must equal value.step_index.`,
      );
    }
    if (entry.key < 0n || entry.key >= expectedCount) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `transition_trace[${index.toString()}].key ${entry.key.toString()} is outside the declared transition step range.`,
      );
    }
    if (seen.has(entry.key)) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `transition_trace contains duplicate decoded key ${entry.key.toString()}.`,
      );
    }
    seen.add(entry.key);
  }
  for (let index = 0n; index < expectedCount; index += 1n) {
    if (!seen.has(index)) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `transition_trace is missing decoded key ${index.toString()}.`,
      );
    }
  }
};

export const rawEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): readonly KeyValuePhasEntry[] =>
  canonicalizeKeyValuePhasEntries(
    entries.map(([key, value], index) => ({
      key: entryBuffer(key, `${fieldName}[${index.toString()}].key`),
      value: entryBuffer(value, `${fieldName}[${index.toString()}].value`),
    })),
  );

export const decodeTypedEntries = <K, V>({
  fieldName,
  entries,
  keySchema,
  valueSchema,
  valueEncoder,
}: {
  readonly fieldName: string;
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
  readonly valueEncoder?: (value: V, raw: string) => string;
}): readonly DecodedRootEntry<K, V>[] =>
  entries.map(([keyHex, valueHex], index) => {
    const keyBytes = entryBuffer(keyHex, `${fieldName}[${index}].key`);
    const valueBytes = entryBuffer(valueHex, `${fieldName}[${index}].value`);
    return {
      key: decodeData<K>(keyHex, keySchema, `${fieldName}[${index}].key`),
      value: decodeData<V>(
        valueHex,
        valueSchema,
        `${fieldName}[${index}].value`,
        valueEncoder,
      ),
      keyBytes,
      valueBytes,
    };
  });

export const decodeTransactions = async ({
  entries,
  preimages,
}: {
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly preimages: readonly SDK.DaPayloadEntry[];
}): Promise<readonly DecodedTransactionEntry[]> => {
  const preimagesByTxId = new Map(
    preimages.map(([key, value], index) => [
      normalizeHex(key, {
        fieldName: `transaction_preimages[${index.toString()}].key`,
        byteLength: 32,
        trim: false,
      }),
      entryBuffer(value, `transaction_preimages[${index.toString()}].value`),
    ]),
  );
  if (preimagesByTxId.size !== preimages.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "transaction_preimages contains duplicate transaction IDs.",
    );
  }
  const decoded: DecodedTransactionEntry[] = [];
  for (const [index, [keyHex, valueHex]] of entries.entries()) {
    const txId = normalizeHex(keyHex, {
      fieldName: `transactions[${index.toString()}].key`,
      byteLength: 32,
      trim: false,
    });
    const valueBytes = entryBuffer(
      valueHex,
      `transactions[${index.toString()}].value`,
    );
    const value = decodeData<SDK.L2TransactionSource>(
      valueHex,
      SDK.L2TransactionSourceSchema,
      `transactions[${index.toString()}].value`,
    );
    const fullTransactionCbor = preimagesByTxId.get(txId);
    if (fullTransactionCbor === undefined) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `transaction_preimages is missing tx_id ${txId}.`,
      );
    }
    let raw: ReturnType<typeof deriveMidgardNativeTxFaultEvidenceMaterial>;
    try {
      raw = deriveMidgardNativeTxFaultEvidenceMaterial(fullTransactionCbor);
      const expectedTxId = raw.transactionId.toString("hex");
      const expected: SDK.L2TransactionSource = {
        tx_id: expectedTxId,
        source: {
          compact_cbor: raw.proofSource.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            raw.proofSource.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            raw.proofSource.fieldPreimageLengthsCbor.toString("hex"),
        },
      };
      if (
        expectedTxId !== txId ||
        Data.to(value, SDK.L2TransactionSource) !==
          Data.to(expected, SDK.L2TransactionSource)
      ) {
        throw new Error(
          "source or commitment does not match canonical preimage",
        );
      }
    } catch (cause) {
      throw transitionTraceError(
        "malformedPayload",
        `Failed to authenticate transactions[${index.toString()}] against its canonical preimage.`,
        cause,
      );
    }
    const committedFieldDefect = raw.fieldPreimages
      .map((preimage, fieldIndex) =>
        SDK.canonicalDecodabilityEvidenceFromCommittedField({
          badTxId: txId,
          fieldIndex,
          committedPreimage: preimage,
        }),
      )
      .find(({ isViolation }) => isViolation);
    if (committedFieldDefect !== undefined) {
      throw transitionTraceError(
        "authenticatedCommittedFieldDefect",
        `L1-authenticated transaction ${txId} commits Q17 field ${committedFieldDefect.fieldIndex.toString()} verdict ${committedFieldDefect.verdict.toString()}.`,
      );
    }
    let full: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>;
    try {
      full = decodeMidgardNativeTxFullFromCanonicalCbor(fullTransactionCbor);
    } catch (cause) {
      throw transitionTraceError(
        "malformedPayload",
        `Failed to decode authenticated transaction ${txId} after its field-envelope check.`,
        cause,
      );
    }
    decoded.push({
      txId,
      keyBytes: Buffer.from(txId, "hex"),
      value,
      valueBytes,
      fullTransactionCbor,
      validity: full.validity,
      spendInputsPreimage: Buffer.from(full.body.spendInputsPreimageCbor),
      outputsPreimage: Buffer.from(full.body.outputsPreimageCbor),
    });
  }
  if (preimagesByTxId.size !== decoded.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "transaction_preimages contains entries without a matching transaction source.",
    );
  }
  return decoded;
};
