import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  type MidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec/forced";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  DaPayloadValidationError,
  type DataSchema,
  decodeCanonicalData,
} from "./payload.da-payload-validation-error.js";

export const validateDaPayloadConsensus = (body: SDK.DaPayloadBody): void => {
  if (body.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
    throw new DaPayloadValidationError(
      "version_mismatch",
      `embedded V1 header protocol_version must equal ${MIDGARD_PROTOCOL_VERSION.toString()}, got ${body.header.protocolVersion.toString()}`,
    );
  }

  const limits = MIDGARD_CONSENSUS_LIMITS;
  const countBounds = [
    [
      "withdrawal_count",
      body.counts.withdrawalCount,
      limits.maxWithdrawalCount,
    ],
    [
      "forced_transaction_count",
      body.counts.forcedTransactionCount,
      limits.maxForcedTransactionCount,
    ],
    [
      "l2_transaction_count",
      body.counts.l2TransactionCount,
      limits.maxL2TransactionCount,
    ],
    ["deposit_count", body.counts.depositCount, limits.maxDepositCount],
    [
      "total_event_count",
      body.counts.totalEventCount,
      limits.maxTotalEventCount,
    ],
    [
      "transition_step_count",
      body.counts.transitionStepCount,
      limits.maxTransitionStepCount,
    ],
    [
      "validation_trace_count",
      body.counts.validationTraceCount,
      limits.maxValidationTraceCount,
    ],
  ] as const;
  for (const [field, value, maximum] of countBounds) {
    if (value > BigInt(maximum)) {
      throw new DaPayloadValidationError(
        "consensus_bound",
        `${field} ${value.toString()} exceeds V1 maximum ${maximum.toString()}`,
      );
    }
  }

  if (body.transactions.length !== body.transaction_preimages.length) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "every committed normal transaction source must have exactly one canonical transaction preimage",
    );
  }
  if (
    body.forced_transactions.length !== body.forced_transaction_preimages.length
  ) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "every committed forced transaction source must have exactly one canonical transaction preimage",
    );
  }
  const transactionPreimages = new Map(body.transaction_preimages);
  const forcedTransactionPreimages = new Map(body.forced_transaction_preimages);

  let canonicalTransactionBytes = 0;
  let ledgerOperationCount = body.deposits.length;
  const validateFullTransaction = (
    txCbor: Buffer,
    fieldName: string,
  ): ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor> => {
    canonicalTransactionBytes += txCbor.length;
    let tx;
    try {
      tx = decodeMidgardNativeTxFullFromCanonicalCbor(txCbor);
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName} is not a canonical full Midgard transaction`,
        { cause },
      );
    }
    const violation = validateMidgardConsensusTxCbor(txCbor);
    if (violation !== null) {
      throw new DaPayloadValidationError(
        violation.code === "E_TX_SIZE" ||
        violation.code === "E_FIELD_PREIMAGE_SIZE" ||
        violation.code === "E_LEDGER_OUTPUT_SIZE"
          ? "consensus_bound"
          : "unsupported_feature",
        `${fieldName} violates proof consensus profile: ${violation.code} ${violation.featureId} ${violation.detail}`,
      );
    }
    return tx;
  };
  const countLedgerOperations = (
    tx: {
      readonly body: {
        readonly spendInputsPreimageCbor: Buffer;
        readonly outputsPreimageCbor: Buffer;
      };
    },
    fieldName: string,
  ): void => {
    ledgerOperationCount +=
      decodeMidgardNativeByteListPreimage(
        tx.body.spendInputsPreimageCbor,
        `${fieldName}.spend_inputs`,
      ).length +
      decodeMidgardNativeByteListPreimage(
        tx.body.outputsPreimageCbor,
        `${fieldName}.outputs`,
      ).length;
  };
  const assertSourceBinding = (
    source: SDK.L2TransactionSource | SDK.ForcedInclusionTxV1,
    decoded: {
      readonly transactionId: Buffer;
      readonly proofSource: MidgardForcedTxProofSource;
    },
    fieldName: string,
  ): string => {
    const decodedTxId = decoded.transactionId.toString("hex");
    const committedTxId = normalizeHex(source.tx_id, {
      fieldName: `${fieldName}.tx_id`,
      byteLength: 32,
    });
    if (committedTxId !== decodedTxId) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName}.tx_id ${committedTxId} does not match decoded transaction id ${decodedTxId}`,
      );
    }
    const submitted =
      "submitted_source" in source ? source.submitted_source : source.source;
    const derived = decoded.proofSource;
    const compactCbor = normalizeHex(submitted.compact_cbor, {
      fieldName: `${fieldName}.source.compact_cbor`,
    });
    const witnessSetCompactCbor = normalizeHex(
      submitted.witness_set_compact_cbor,
      {
        fieldName: `${fieldName}.source.witness_set_compact_cbor`,
      },
    );
    const fieldPreimageLengthsCbor = normalizeHex(
      submitted.field_preimage_lengths_cbor,
      {
        fieldName: `${fieldName}.source.field_preimage_lengths_cbor`,
      },
    );
    if (
      compactCbor !== derived.compactCbor.toString("hex") ||
      witnessSetCompactCbor !== derived.witnessSetCompactCbor.toString("hex") ||
      fieldPreimageLengthsCbor !==
        derived.fieldPreimageLengthsCbor.toString("hex")
    ) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName}.source does not match the canonical transaction field commitments`,
      );
    }
    // No `transaction_commitment` to check: the committed source carries the
    // proof-source triple and nothing derived from it, so the three equalities
    // above are the whole of the binding. The retired field was
    // `computeMidgardNativeTxProofCommitmentV1(derived)` by construction, and a
    // check of a value against its own derivation could only ever pass.
    return decodedTxId;
  };

  for (const [index, [keyHex, valueHex]] of body.transactions.entries()) {
    const fieldName = `transactions[${index.toString()}]`;
    const committedTxId = normalizeHex(keyHex, {
      fieldName: `${fieldName}.key`,
      byteLength: 32,
    });
    const preimageHex = transactionPreimages.get(keyHex);
    if (preimageHex === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `${fieldName} has no same-key transaction_preimages entry`,
      );
    }
    const source = decodeCanonicalData<SDK.L2TransactionSource>(
      valueHex,
      SDK.L2TransactionSourceSchema as never,
      `${fieldName}.value`,
    );
    const tx = validateFullTransaction(
      hexToBytes(preimageHex, `transaction_preimages[${index.toString()}]`),
      `transaction_preimages[${index.toString()}]`,
    );
    if (
      assertSourceBinding(
        source,
        {
          transactionId: computeMidgardNativeTxId(tx.compact),
          proofSource: deriveMidgardNativeTxProofSource(tx),
        },
        fieldName,
      ) !== committedTxId
    ) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName}.key does not match the committed transaction source`,
      );
    }
    countLedgerOperations(tx, fieldName);
  }

  for (const [
    index,
    [keyHex, valueHex],
  ] of body.forced_transactions.entries()) {
    const fieldName = `forced_transactions[${index.toString()}]`;
    const preimageHex = forcedTransactionPreimages.get(keyHex);
    if (preimageHex === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `${fieldName} has no same-key forced_transaction_preimages entry`,
      );
    }
    const forced = decodeCanonicalData<SDK.ForcedInclusionTxV1>(
      valueHex,
      SDK.ForcedInclusionTxV1Schema as never,
      `${fieldName}.value`,
    );
    // A forced transaction is bound by its §5.1 envelope alone: its items
    // may not decode, and its verdict then rejects it, so only a valid one
    // has its ledger operations counted.
    const preimageField = `forced_transaction_preimages[${index.toString()}]`;
    const preimage = hexToBytes(preimageHex, preimageField);
    canonicalTransactionBytes += preimage.length + 1;
    let material;
    try {
      material = deriveMidgardForcedTxFaultEvidenceMaterial(preimage);
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${preimageField} is not a canonical forced Midgard transaction envelope`,
        { cause },
      );
    }
    assertSourceBinding(forced, material, fieldName);
    if (forced.verdict === "ForcedTxValid") {
      try {
        countLedgerOperations(material.canonical, fieldName);
      } catch (cause) {
        throw new DaPayloadValidationError(
          "malformed_transaction",
          `${fieldName} is ForcedTxValid but its inputs or outputs do not decode`,
          { cause },
        );
      }
    }
  }

  if (canonicalTransactionBytes > limits.maxCanonicalTransactionBytesPerBlock) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `canonical transaction bytes ${canonicalTransactionBytes.toString()} exceed V1 maximum ${limits.maxCanonicalTransactionBytesPerBlock.toString()}`,
    );
  }
  if (ledgerOperationCount > limits.maxLedgerOperationCount) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `ledger operations ${ledgerOperationCount.toString()} exceed V1 maximum ${limits.maxLedgerOperationCount.toString()}`,
    );
  }
};

export const dataHex = <A>(value: A, schema: DataSchema): string =>
  LucidData.to(value as never, schema as never);
