/**
 * Step-submitter support shared by every fault-proof family: the native
 * transaction-inclusion material a first step binds, the computation-thread
 * token and initial-datum checks, fee-input selection, and the blueprint
 * titles and ledger-key encoding the membership/exclusion withdrawals use.
 *
 * Lifted out of the double-spend and non-existent-input step builders, which
 * grew them first; nothing here is specific to either family.
 */

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxCompact,
  formatUnknownError,
  type MidgardNativeTxCompact as CoreNativeTxCompact,
  MidgardTxValidityCodes,
  normalizeHex,
  verifyMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  FraudProofComputationThreadStepDatum,
  type L2TransactionSource,
  L2TransactionSourceSchema,
  type MidgardTxInput,
  NativeTxCompact,
  type NativeTxCompact as NativeTxCompactData,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import {
  parseHex,
  parseInteger,
  parseSignedInteger,
  requireRecord,
} from "./json-file.js";
import {
  compareUtxoOutRefs,
  outRefLabel,
  type ResolvedProverSigner,
} from "./runtime.js";

export const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";
const MIN_FEE_INPUT_LOVELACE = 10_000_000n;

export type SubmitStep01TxInclusion = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  /** Exact canonical `Data(L2TransactionSource)` committed by transactions_root. */
  readonly l2TransactionSourceCbor: string;
  // Raw transactions MPF root the membership proof opens. Authenticated on-chain
  // against the block header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProof: Proof;
  readonly txMembershipProofCbor: string;
};

const bytesHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const forcedTxFromCoreCompact = (
  tx: Pick<
    CoreNativeTxCompact,
    "transactionBody" | "transactionWitnessSetHash"
  >,
): Omit<NativeTxCompactData, "validity_code"> => ({
  body: {
    spend_inputs_hash: bytesHex(tx.transactionBody.spendInputsHash),
    reference_inputs_hash: bytesHex(tx.transactionBody.referenceInputsHash),
    outputs_hash: bytesHex(tx.transactionBody.outputsHash),
    fee: tx.transactionBody.fee,
    validity_interval_start: tx.transactionBody.validityIntervalStart,
    validity_interval_end: tx.transactionBody.validityIntervalEnd,
    required_observers_hash: bytesHex(tx.transactionBody.requiredObserversHash),
    required_signers_hash: bytesHex(tx.transactionBody.requiredSignersHash),
    mint_hash: bytesHex(tx.transactionBody.mintHash),
    script_integrity_hash: bytesHex(tx.transactionBody.scriptIntegrityHash),
    auxiliary_data_hash: bytesHex(tx.transactionBody.auxiliaryDataHash),
    network_id: tx.transactionBody.networkId,
  },
  witness_set_hash: bytesHex(tx.transactionWitnessSetHash),
});

export const nativeTxFromCoreCompact = (
  tx: CoreNativeTxCompact,
): NativeTxCompactData => ({
  ...forcedTxFromCoreCompact(tx),
  validity_code: MidgardTxValidityCodes[tx.validity],
});

const parseNativeTxCompact = (
  value: unknown,
  label: string,
): NativeTxCompactData => {
  const record = requireRecord(value, label);
  const bodyRecord = requireRecord(record.body, `${label}.body`);
  const tx: NativeTxCompactData = {
    body: {
      spend_inputs_hash: parseHex(
        bodyRecord.spend_inputs_hash,
        `${label}.body.spend_inputs_hash`,
        32,
      ),
      reference_inputs_hash: parseHex(
        bodyRecord.reference_inputs_hash,
        `${label}.body.reference_inputs_hash`,
        32,
      ),
      outputs_hash: parseHex(
        bodyRecord.outputs_hash,
        `${label}.body.outputs_hash`,
        32,
      ),
      fee: parseInteger(bodyRecord.fee, `${label}.body.fee`),
      validity_interval_start: parseSignedInteger(
        bodyRecord.validity_interval_start,
        `${label}.body.validity_interval_start`,
      ),
      validity_interval_end: parseSignedInteger(
        bodyRecord.validity_interval_end,
        `${label}.body.validity_interval_end`,
      ),
      required_observers_hash: parseHex(
        bodyRecord.required_observers_hash,
        `${label}.body.required_observers_hash`,
        32,
      ),
      required_signers_hash: parseHex(
        bodyRecord.required_signers_hash,
        `${label}.body.required_signers_hash`,
        32,
      ),
      mint_hash: parseHex(bodyRecord.mint_hash, `${label}.body.mint_hash`, 32),
      script_integrity_hash: parseHex(
        bodyRecord.script_integrity_hash,
        `${label}.body.script_integrity_hash`,
        32,
      ),
      auxiliary_data_hash: parseHex(
        bodyRecord.auxiliary_data_hash,
        `${label}.body.auxiliary_data_hash`,
        32,
      ),
      network_id: parseInteger(
        bodyRecord.network_id,
        `${label}.body.network_id`,
      ),
    },
    witness_set_hash: parseHex(
      record.witness_set_hash,
      `${label}.witness_set_hash`,
      32,
    ),
    validity_code: parseInteger(record.validity_code, `${label}.validity_code`),
  };
  return Data.from(Data.to(tx, NativeTxCompact), NativeTxCompact);
};

export const parseSubmitStep01TxInclusion = (
  value: unknown,
): SubmitStep01TxInclusion => {
  const record = requireRecord(value, "--tx-inclusion");
  const nativeTxId = parseHex(
    record.nativeTxId,
    "--tx-inclusion.nativeTxId",
    32,
  );
  const nativeTx = parseNativeTxCompact(
    record.nativeTx,
    "--tx-inclusion.nativeTx",
  );
  const nativeTxCompactCbor = parseHex(
    record.nativeTxCompactCbor,
    "--tx-inclusion.nativeTxCompactCbor",
  );
  const l2TransactionSourceCbor = parseHex(
    record.l2TransactionSourceCbor,
    "--tx-inclusion.l2TransactionSourceCbor",
  );
  let l2TransactionSource: L2TransactionSource;
  try {
    l2TransactionSource = Data.from(
      l2TransactionSourceCbor,
      L2TransactionSourceSchema as never,
    ) as L2TransactionSource;
  } catch (cause) {
    throw new Error(
      `--tx-inclusion.l2TransactionSourceCbor is not Data(L2TransactionSourceV1): ${formatUnknownError(cause)}`,
    );
  }
  if (
    Data.to(
      l2TransactionSource as never,
      L2TransactionSourceSchema as never,
    ) !== l2TransactionSourceCbor
  ) {
    throw new Error(
      "--tx-inclusion.l2TransactionSourceCbor is not canonical Data(L2TransactionSourceV1).",
    );
  }
  if (l2TransactionSource.tx_id !== nativeTxId) {
    throw new Error(
      "--tx-inclusion.l2TransactionSourceCbor tx_id does not match nativeTxId.",
    );
  }
  if (l2TransactionSource.source.compact_cbor !== nativeTxCompactCbor) {
    throw new Error(
      "--tx-inclusion.l2TransactionSourceCbor compact_cbor does not match nativeTxCompactCbor.",
    );
  }
  try {
    verifyMidgardNativeTxProofSource({
      transactionId: Buffer.from(nativeTxId, "hex"),
      source: {
        compactCbor: Buffer.from(
          l2TransactionSource.source.compact_cbor,
          "hex",
        ),
        witnessSetCompactCbor: Buffer.from(
          l2TransactionSource.source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          l2TransactionSource.source.field_preimage_lengths_cbor,
          "hex",
        ),
      },
    });
  } catch (cause) {
    throw new Error(
      `--tx-inclusion.l2TransactionSourceCbor does not authenticate an exact native proof source: ${formatUnknownError(cause)}`,
    );
  }
  const transactionsPhasRoot = parseHex(
    record.transactionsPhasRoot,
    "--tx-inclusion.transactionsPhasRoot",
    32,
  );
  const txMembershipProofCbor = parseHex(
    record.txMembershipProofCbor,
    "--tx-inclusion.txMembershipProofCbor",
  );
  return {
    nativeTxId,
    nativeTx,
    nativeTxCompactCbor,
    l2TransactionSourceCbor,
    transactionsPhasRoot,
    txMembershipProof: Data.from(txMembershipProofCbor, Proof),
    txMembershipProofCbor,
  };
};

export const requireNativeTxMatchesCompactCbor = (
  inclusion: SubmitStep01TxInclusion,
): CoreNativeTxCompact => {
  let decoded: CoreNativeTxCompact;
  try {
    decoded = decodeMidgardNativeTxCompact(
      Buffer.from(inclusion.nativeTxCompactCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `--tx-inclusion.nativeTxCompactCbor is not a valid native compact transaction: ${formatUnknownError(cause)}`,
    );
  }
  const canonicalCbor = encodeMidgardNativeTxCompact(decoded).toString("hex");
  if (canonicalCbor !== inclusion.nativeTxCompactCbor) {
    throw new Error(
      "--tx-inclusion.nativeTxCompactCbor is not canonical native compact CBOR.",
    );
  }
  const decodedNativeTx = nativeTxFromCoreCompact(decoded);
  if (
    Data.to(decodedNativeTx, NativeTxCompact) !==
    Data.to(inclusion.nativeTx, NativeTxCompact)
  ) {
    throw new Error(
      "--tx-inclusion.nativeTx does not match nativeTxCompactCbor.",
    );
  }
  const computedTxId = computeMidgardNativeTxId(decoded).toString("hex");
  if (computedTxId !== inclusion.nativeTxId) {
    throw new Error(
      `--tx-inclusion.nativeTxId mismatch: provided=${inclusion.nativeTxId}, computed=${computedTxId}.`,
    );
  }
  return decoded;
};

const singlePositiveNonAdaAsset = (
  utxo: UTxO,
  label: string,
): readonly [string, bigint] => {
  const nonAdaAssets = Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );
  if (nonAdaAssets.length !== 1) {
    throw new Error(
      `Expected ${label} ${outRefLabel(utxo)} to carry exactly one non-ADA asset, found ${nonAdaAssets.length.toString()}.`,
    );
  }
  return nonAdaAssets[0]!;
};

export const requireComputationThreadToken = ({
  utxo,
  computationThreadPolicyId,
  categoryId,
  categoryLabel,
}: {
  readonly utxo: UTxO;
  readonly computationThreadPolicyId: string;
  readonly categoryId: string;
  readonly categoryLabel: string;
}): {
  readonly unit: string;
  readonly assetName: string;
  readonly fraudulentHeaderHash: string;
} => {
  const [unit, amount] = singlePositiveNonAdaAsset(utxo, "thread UTxO");
  if (amount !== 1n) {
    throw new Error(
      `Expected computation-thread token amount 1 at ${outRefLabel(utxo)}, found ${amount.toString()}.`,
    );
  }
  const expectedPrefix = `${computationThreadPolicyId}${categoryId}`;
  if (!unit.startsWith(expectedPrefix)) {
    throw new Error(
      `Thread UTxO ${outRefLabel(utxo)} does not carry a ${categoryLabel} computation-thread token for policy ${computationThreadPolicyId}.`,
    );
  }
  const assetName = unit.slice(computationThreadPolicyId.length);
  const fraudulentHeaderHash = normalizeHex(unit.slice(expectedPrefix.length), {
    fieldName: `Computation-thread asset name ${assetName} suffix`,
    byteLength: 28,
    trim: false,
  });
  return { unit, assetName, fraudulentHeaderHash };
};

export const requireInitialStepDatum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): void => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data !== null) {
    throw new Error("Step 01 input datum must have null state data.");
  }
};

export const selectFeeInput = (walletUtxos: readonly UTxO[]): UTxO => {
  const candidates = walletUtxos
    .filter((utxo) => {
      const nonAdaAssets = Object.entries(utxo.assets).filter(
        ([unit, amount]) => unit !== "lovelace" && amount > 0n,
      );
      return (
        utxo.datum == null &&
        utxo.datumHash == null &&
        utxo.scriptRef == null &&
        nonAdaAssets.length === 0 &&
        (utxo.assets.lovelace ?? 0n) >= MIN_FEE_INPUT_LOVELACE
      );
    })
    .sort((left, right) => {
      const leftLovelace = left.assets.lovelace ?? 0n;
      const rightLovelace = right.assets.lovelace ?? 0n;
      if (rightLovelace > leftLovelace) {
        return 1;
      }
      if (rightLovelace < leftLovelace) {
        return -1;
      }
      return compareUtxoOutRefs(left, right);
    });
  const feeInput = candidates[0];
  if (feeInput === undefined) {
    throw new Error(
      `Prover wallet must contain a pure-ADA UTxO with at least ${MIN_FEE_INPUT_LOVELACE.toString()} lovelace for fees.`,
    );
  }
  return feeInput;
};

export const PEXCLUDES_EXCLUSION_WITHDRAW_TITLE =
  "pexcludes.exclusion.withdraw";

/**
 * Encodes a `MidgardTxInput` as the node's ledger MPF key: the §5.3 field-0/1
 * item form `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes with a
 * deliberately non-minimal uint16 output index. These are the bytes on-chain
 * `ledger_outref_key` derives via `encode_midgard_tx_input`, not CML's
 * minimal-index `TransactionInput` CBOR, and NOT
 * `cbor.serialise(OutputReference)`.
 */
export const ledgerKeyBytesHex = (input: MidgardTxInput): string =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(input.tx_id, "hex"),
    outputIndex: Number(input.output_index),
  }).toString("hex");
