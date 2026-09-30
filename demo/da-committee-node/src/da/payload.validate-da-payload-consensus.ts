import {
  assertMidgardCekProgramMaterialBundle,
  decodeMidgardCekProgramMaterialDaEntry,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  type MidgardForcedTxFull,
} from "@al-ft/midgard-core/codec/forced";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusForcedTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  collectProofProgramEnvelopes,
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
  const resolvedOutputsByOutRef = new Map(
    body.utxos.map(([outRefHex, outputHex]) => [
      normalizeHex(outRefHex, { fieldName: "utxos.key" }),
      hexToBytes(outputHex, "utxos.value"),
    ]),
  );

  let canonicalTransactionBytes = 0;
  let ledgerOperationCount = body.deposits.length;
  const programEnvelopes = new Map<string, MidgardCekProgramEnvelope>();
  const validateFullTransaction = (
    txCbor: Buffer,
    fieldName: string,
    sourceKind: "normal" | "forced" = "normal",
  ):
    | ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>
    | MidgardForcedTxFull => {
    canonicalTransactionBytes +=
      txCbor.length + (sourceKind === "forced" ? 1 : 0);
    let tx;
    try {
      tx = (
        sourceKind === "forced"
          ? decodeMidgardForcedTxFullFromCanonicalCbor
          : decodeMidgardNativeTxFullFromCanonicalCbor
      )(txCbor);
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName} is not a canonical full Midgard transaction`,
        { cause },
      );
    }
    const violation = (
      sourceKind === "forced"
        ? validateMidgardConsensusForcedTxCbor
        : validateMidgardConsensusTxCbor
    )(txCbor);
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
    collectProofProgramEnvelopes(
      tx,
      fieldName,
      programEnvelopes,
      resolvedOutputsByOutRef,
    );
    return tx;
  };
  const countLedgerOperations = (
    tx:
      | ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>
      | MidgardForcedTxFull,
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
    tx:
      | ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>
      | MidgardForcedTxFull,
    fieldName: string,
  ): string => {
    const decodedTxId = computeMidgardNativeTxId(tx.compact).toString("hex");
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
    const derived =
      "submitted_source" in source
        ? deriveMidgardForcedTxProofSource(tx)
        : deriveMidgardNativeTxProofSource(
            "validity" in tx
              ? tx
              : (() => {
                  throw new Error("Normal source requires native material");
                })(),
          );
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
    if (assertSourceBinding(source, tx, fieldName) !== committedTxId) {
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
    const tx = validateFullTransaction(
      hexToBytes(
        preimageHex,
        `forced_transaction_preimages[${index.toString()}]`,
      ),
      `forced_transaction_preimages[${index.toString()}]`,
      "forced",
    );
    assertSourceBinding(forced, tx, fieldName);
    if (forced.verdict === "ForcedTxValid") {
      countLedgerOperations(tx, fieldName);
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
  try {
    const material = body.cek_program_material.map(([rootHex, valueHex]) =>
      decodeMidgardCekProgramMaterialDaEntry(
        hexToBytes(rootHex, "cek_program_material.root"),
        hexToBytes(valueHex, "cek_program_material.value"),
      ),
    );
    assertMidgardCekProgramMaterialBundle(
      [...programEnvelopes.values()],
      material,
    );
  } catch (cause) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "CEK program material does not exactly cover every inline and newly referenced V1 program",
      { cause },
    );
  }
};

export const dataHex = <A>(value: A, schema: DataSchema): string =>
  LucidData.to(value as never, schema as never);
