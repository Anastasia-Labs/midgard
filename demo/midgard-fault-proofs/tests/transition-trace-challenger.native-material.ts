import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  computeMidgardForcedTxProofCommitment,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  encodeData,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/index.js";

export const outRef = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

export const address = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});

export const depositInfo = (byte: number): SDK.DepositInfo => ({
  l2_address: address(byte),
  l2_network_id: 0n,
  l2_datum: null,
});

export const withdrawalInfo = (
  byte: number,
  validity: SDK.WithdrawalValidity = "IncorrectWithdrawalSignature",
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: outRef(byte),
    l2_owner: h28(byte + 1),
    l2_value: new Map(),
    l1_address: address(byte + 2),
    l1_datum: "NoDatum",
  },
  signature: [h32(byte + 3), h32(byte + 4)],
  validity,
});

/**
 * Fixture-local index from a native proof source back to the canonical CBOR the
 * fixture built it from.
 *
 * Keyed by `native_tx_proof_commitment_v1` rather than by tx id even though #584
 * retired `transaction_commitment` from the on-chain leaves. The commitment
 * covers the compact body, the compact witness set and the validity code; the tx
 * id covers the body alone. Every fixture here happens to pin `TxIsValid` with
 * an empty witness set, so the two keys are injective over today's vectors — but
 * a later vector that varies validity or witnesses would silently collide under
 * a tx-id key and hand back the wrong preimage. Nothing outside this file sees
 * the commitment: it is derived here from the source and is deliberately not
 * re-exposed on {@link nativeMaterial}'s result.
 */
export const canonicalPreimageByCommitment = new Map<string, Buffer>();

export const nativeMaterial = (
  byte: number,
  preimages: {
    readonly spendInputsPreimageCbor?: Buffer;
    readonly outputsPreimageCbor?: Buffer;
  } = {},
) => {
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor:
        preimages.spendInputsPreimageCbor ?? EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: preimages.outputsPreimageCbor ?? EMPTY_CBOR_LIST,
      fee: BigInt(byte),
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const full = materializeMidgardNativeTxFromCanonical(canonical);
  const canonicalCbor = encodeMidgardNativeTxCanonical(full);
  const source =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxId(full).toString("hex");
  canonicalPreimageByCommitment.set(
    computeMidgardNativeTxProofCommitment(source).toString("hex"),
    canonicalCbor,
  );
  return {
    txId,
    canonicalCbor,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

/**
 * The #640 verdict standing in for the pre-format `FailedScript` arm: a
 * forced transaction the operator rejected for a failed Plutus execution at
 * execution index 0.
 */
export const forcedTxInvalidPlutus: SDK.OperatorVerdict = {
  ForcedTxInvalid: {
    reason: { PlutusExecutionFailed: { execution_index: 0n } },
  },
};

export const forcedTx = (
  byte: number,
  verdict: SDK.OperatorVerdict = forcedTxInvalidPlutus,
): SDK.ForcedInclusionTxV1 => {
  const material = nativeMaterial(byte);
  const adjudicated = deriveMidgardForcedTxProofSource(
    materializeMidgardForcedTxFromCanonical(
      decodeMidgardNativeTxFullFromCanonicalCbor(material.canonicalCbor),
    ),
  );
  canonicalPreimageByCommitment.set(
    computeMidgardForcedTxProofCommitment(adjudicated).toString("hex"),
    encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical(
        decodeMidgardNativeTxFullFromCanonicalCbor(material.canonicalCbor),
      ),
    ),
  );
  return {
    tx_id: material.txId,
    submitted_source: {
      compact_cbor: adjudicated.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        adjudicated.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        adjudicated.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict,
  };
};

export const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

export const sorted = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const LEDGER_OUTPUT_CBOR =
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0";

const TAG4_OUTPUT_ADDRESS = Buffer.from(`70${"aa".repeat(28)}`, "hex");

const tag4OutputRequiredFields = (lovelaceCbor = Buffer.from([0])): Buffer =>
  Buffer.concat([
    Buffer.from([0]),
    encodeCbor(TAG4_OUTPUT_ADDRESS),
    Buffer.from([1, 0x82]),
    lovelaceCbor,
    Buffer.from([0xa0]),
  ]);

export const TAG4_OUTPUT_REQUIRED_FIELDS = tag4OutputRequiredFields();

export const tag4OutputWithNonMinimalLovelace = (): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2]),
    tag4OutputRequiredFields(Buffer.from([0x18, 0])),
  ]);

const tag4OutputWithAssetOrder = (firstQuantityCbor: Buffer): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0]),
    encodeCbor(TAG4_OUTPUT_ADDRESS),
    Buffer.from([1, 0x82, 0, 0xa2, 0x58, 28]),
    Buffer.alloc(28, 0xbb),
    Buffer.from([0xa2]),
    encodeCbor(Buffer.from([0xff])),
    firstQuantityCbor,
    encodeCbor(Buffer.from([0])),
    Buffer.from([2, 0x58, 28]),
    Buffer.alloc(28, 0xaa),
    Buffer.from([0xa1]),
    encodeCbor(Buffer.from([1])),
    Buffer.from([3]),
  ]);

export const tag4OutputWithNonMinimalQuantity = (): Buffer =>
  tag4OutputWithAssetOrder(Buffer.from([0x18, 1]));

export const tag4OutputWithPreservedAssetOrder = (firstQuantity = 1): Buffer =>
  tag4OutputWithAssetOrder(Buffer.from([firstQuantity]));

export const tag4OutputWithOpaqueDatum = (): Buffer =>
  Buffer.concat([
    Buffer.from([0xa3]),
    TAG4_OUTPUT_REQUIRED_FIELDS,
    Buffer.from([2]),
    encodeCbor(Buffer.from([0xff])),
  ]);

export const tag4OutputWithOpaqueNativeScript = (): Buffer =>
  Buffer.concat([
    Buffer.from([0xa3]),
    TAG4_OUTPUT_REQUIRED_FIELDS,
    Buffer.from([3, 0x82, 0]),
    encodeCbor(Buffer.from([0xde, 0xad, 0xff])),
  ]);

export const spendInputItem = (txIdHex: string, outputIndex: number): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txIdHex, "hex"),
    outputIndex,
  });

export const rawLedgerEntry = (byte: number): SDK.DaPayloadEntry => [
  spendInputItem(h32(byte), 0).toString("hex"),
  LEDGER_OUTPUT_CBOR,
];

/// The exact bytes one output occupies in `utxos_root`: its
/// `LedgerOutputCommitmentV1` descriptor, keyed by the out-ref the entry is
/// filed under (spec §5.3 — "not with the full output bytes"). Fixtures that
/// feed deliberately malformed or non-canonical output bytes have no
/// descriptor at all; the challenger refuses those before it reaches MPF
/// replay, so the trie value it never reads falls back to the raw bytes rather
/// than making the fixture unbuildable.
export const ledgerTrieValue = (outRef: Buffer, outputCbor: Buffer): Buffer => {
  try {
    return Buffer.from(
      buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef,
        outputCbor,
      }).descriptorCbor,
    );
  } catch {
    return outputCbor;
  }
};

export const utxoRootWithDescriptors = (
  utxos: readonly SDK.DaPayloadEntry[],
): Promise<Awaited<ReturnType<typeof keyValuePhasRootWithCount>>> =>
  keyValuePhasRootWithCount(
    utxos.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: Buffer.from(key, "hex"),
        outputCbor: Buffer.from(value, "hex"),
      }).descriptorCbor,
    })),
  );

export const encodedEntry = <K, V>({
  key,
  keySchema,
  value,
  valueSchema,
}: {
  readonly key: K;
  readonly keySchema: Parameters<typeof Data.Nullable>[0];
  readonly value: V;
  readonly valueSchema: Parameters<typeof Data.Nullable>[0];
}): SDK.DaPayloadEntry =>
  entry(
    encodeData(key, keySchema),
    valueSchema === SDK.WithdrawalInfoSchema
      ? Buffer.from(
          SDK.committedWithdrawalValueBytes(value as SDK.WithdrawalInfo),
          "hex",
        )
      : encodeData(value, valueSchema),
  );

export const traceEntryWithKey = (
  key: bigint,
  step: SDK.TransitionStep,
): SDK.DaPayloadEntry =>
  encodedEntry({
    key,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: SDK.TransitionStepSchema,
  });

export const traceEntry = (step: SDK.TransitionStep): SDK.DaPayloadEntry =>
  traceEntryWithKey(step.step_index, step);

export const eventToStepEntry = (
  key: SDK.EventKey,
  value: SDK.EventToStepValue,
): SDK.DaPayloadEntry =>
  encodedEntry({
    key,
    keySchema: SDK.EventKeySchema,
    value,
    valueSchema: SDK.EventToStepValueSchema,
  });
