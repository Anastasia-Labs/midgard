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
import {
  computeMidgardForcedTxProofCommitment,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  h28 as byteHex28,
  h32 as byteHex32,
} from "@al-ft/midgard-test-support/hex";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  encodeData,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/index.js";

// ---------------------------------------------------------------------------
// Fixture machinery. Mirrors demo/midgard-fault-proofs/tests/transition-trace-
// challenger.test.ts (trimmed to what a dense-totality suite needs — no tag-4
// output encoding, no MPF branch replay). Kept self-contained rather than
// imported so this file has no coupling to the other test file's internals.
// ---------------------------------------------------------------------------

/**
 * Fixture labels here are grouped by scheme — 6xx for the block fixtures, 7xx
 * and 9xx for the per-test traces — so they run past the 0..255 that a byte
 * holds. The shared helpers take a byte value and reject anything else, so fold
 * the label into a byte explicitly at this one place instead of letting the
 * helper do it silently. The fold is not injective: labels 256 apart name the
 * same bytes (730/986 and 731/987 do, in unrelated fields), which is exactly
 * what a silent mask used to hide.
 */
const label = (n: number): number => n & 0xff;

export const h32 = (n: number): string => byteHex32(label(n));

export const h28 = (n: number): string => byteHex28(label(n));

export const outRef = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const address = (byte: number): SDK.AddressData => ({
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
  validity: SDK.WithdrawalValidity,
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
 * Fixture-local index from a native proof source back to the canonical CBOR
 * the fixture built it from — needed to reconstruct
 * `forced_transaction_preimages` the same way `buildPayloadFixture` does in
 * the reference challenger suite.
 */
export const canonicalPreimageByCommitment = new Map<string, Buffer>();

export const nativeMaterial = (byte: number) => {
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
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
  verdict: SDK.OperatorVerdict,
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

const spendInputItem = (txIdHex: string, outputIndex: number): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txIdHex, "hex"),
    outputIndex,
  });

export const rawLedgerEntry = (byte: number): SDK.DaPayloadEntry => [
  spendInputItem(h32(byte), 0).toString("hex"),
  LEDGER_OUTPUT_CBOR,
];

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
  entry(encodeData(key, keySchema), encodeData(value, valueSchema));

const traceEntryWithKey = (
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

export type PayloadFixtureInput = {
  readonly prevUtxosRoot?: string;
  readonly utxos?: readonly SDK.DaPayloadEntry[];
  readonly withdrawals?: readonly SDK.DaPayloadEntry[];
  readonly forcedTransactions?: readonly SDK.DaPayloadEntry[];
  readonly transactions?: readonly SDK.DaPayloadEntry[];
  readonly transactionPreimages?: readonly SDK.DaPayloadEntry[];
  readonly deposits?: readonly SDK.DaPayloadEntry[];
  readonly steps?: readonly SDK.TransitionStep[];
  readonly transitionTraceEntries?: readonly SDK.DaPayloadEntry[];
  readonly eventToStep?: readonly SDK.DaPayloadEntry[];
};
