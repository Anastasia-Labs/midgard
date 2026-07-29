import {
  decodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialEntryV1,
  type MidgardCekProgramMaterialEntryV1,
  midgardCekProgramMaterialKindTagV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  deriveMidgardTxFieldReceiptAssetNameV1,
  deriveMidgardV1TxFieldChunks,
  validateMidgardConsensusV1TxCbor,
} from "@al-ft/midgard-core/consensus-validation-v1";
import {
  type Assets,
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  toUnit,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  Bech32DeserializationError,
  CredentialSchema,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
  OutputReference,
  outputReferenceFromUTxO,
  POSIXTimeSchema,
} from "@/common.js";
import { HubOracleError } from "@/hub-oracle.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";
import {
  BoundedCollectionItemProofV1,
  CardanoDatum,
  CardanoDatumSchema,
  CekProgramMaterialDatumV1,
  CekProgramMaterialDatumV1Schema,
  MidgardTxValiditySchema,
  NativeTxProofSourceV1,
  NativeTxProofSourceV1Schema,
  TxFieldPreimageV1,
  TxFieldPreimageV1Schema,
  TxFieldReceiptV1,
  TxFieldReceiptV1Schema,
  TxOrderEventV1Schema,
} from "@/ledger-state.js";
import { RawRootMembershipProofSchema } from "@/transition-trace.js";
import {
  requireInputIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";

import {
  buildCompletedUserEventMintTxProgram,
  encodeUserEventWitnessMintOrBurnRedeemer,
  fetchUserEventUTxOsProgram,
  outputReferenceToPlutusDataCbor,
  prepareUserEventMintContext,
  UserEventBuildError,
  userEventCborFieldsFromInlineDatum,
  UserEventExtraFields,
  UserEventFetchConfig,
} from "./internals.js";

export const TxOrderRefundAddressV1Schema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(
    Data.Enum([
      Data.Object({
        Inline: Data.Tuple([CredentialSchema]),
      }),
      Data.Object({
        Pointer: Data.Object({
          slotNumber: Data.Integer(),
          transactionIndex: Data.Integer(),
          certificateIndex: Data.Integer(),
        }),
      }),
    ]),
  ),
});
export type TxOrderRefundAddressV1 = Data.Static<
  typeof TxOrderRefundAddressV1Schema
>;
export const TxOrderRefundAddressV1 =
  TxOrderRefundAddressV1Schema as unknown as TxOrderRefundAddressV1;

export const TxOrderDatumV1Schema = Data.Object({
  event: TxOrderEventV1Schema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: TxOrderRefundAddressV1Schema,
  refund_datum: CardanoDatumSchema,
});
export type TxOrderDatumV1 = Data.Static<typeof TxOrderDatumV1Schema>;
export const TxOrderDatumV1 = TxOrderDatumV1Schema as unknown as TxOrderDatumV1;

type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

const encodeCanonicalPlutusDataV1 = <A>(
  value: A,
  schema: PlutusDataSchema,
): Buffer => Buffer.from(Data.to(value as never, schema as never), "hex");

const decodeCanonicalPlutusDataV1 = <A>(
  bytes: Uint8Array,
  schema: PlutusDataSchema,
  label: string,
): A => {
  const input = Buffer.from(bytes);
  const decoded = Data.from(input.toString("hex"), schema as never) as A;
  if (!encodeCanonicalPlutusDataV1(decoded, schema).equals(input)) {
    throw new Error(`${label} CBOR must use its exact canonical encoding`);
  }
  return decoded;
};

export const encodeTxOrderDatumV1Cbor = (datum: TxOrderDatumV1): Buffer =>
  encodeCanonicalPlutusDataV1(datum, TxOrderDatumV1Schema);

export const decodeTxOrderDatumV1Cbor = (bytes: Uint8Array): TxOrderDatumV1 =>
  decodeCanonicalPlutusDataV1(bytes, TxOrderDatumV1Schema, "TxOrderDatumV1");

export const decodeTxFieldPreimageV1Cbor = (
  bytes: Uint8Array,
): TxFieldPreimageV1 =>
  decodeCanonicalPlutusDataV1(
    bytes,
    TxFieldPreimageV1Schema,
    "TxFieldPreimageV1",
  );

export const decodeTxFieldReceiptV1Cbor = (
  bytes: Uint8Array,
): TxFieldReceiptV1 =>
  decodeCanonicalPlutusDataV1(
    bytes,
    TxFieldReceiptV1Schema,
    "TxFieldReceiptV1",
  );

export const decodeCekProgramMaterialDatumV1Cbor = (
  bytes: Uint8Array,
): CekProgramMaterialDatumV1 =>
  decodeCanonicalPlutusDataV1(
    bytes,
    CekProgramMaterialDatumV1Schema,
    "CekProgramMaterialDatumV1",
  );

export const TxOrderSpendRedeemerV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  membership_proof: RawRootMembershipProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  validity_override: MidgardTxValiditySchema,
});
export type TxOrderSpendRedeemerV1 = Data.Static<
  typeof TxOrderSpendRedeemerV1Schema
>;
export const TxOrderSpendRedeemerV1 =
  TxOrderSpendRedeemerV1Schema as unknown as TxOrderSpendRedeemerV1;

export type TxOrderUTxOV1 = AuthenticUTxO<TxOrderDatumV1, UserEventExtraFields>;

export const utxosToTxOrderUTxOsV1 = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<TxOrderUTxOV1[]> =>
  authenticateUTxOs<TxOrderDatumV1, UserEventExtraFields>(
    utxos,
    nftPolicy,
    TxOrderDatumV1,
    (datum, utxo) => {
      decodeTxOrderDatumV1Cbor(Buffer.from(utxo.datum!, "hex"));
      return {
        ...userEventCborFieldsFromInlineDatum(utxo),
        inclusionTime: new Date(Number(datum.inclusion_time)),
      };
    },
  );

export const fetchTxOrderUTxOsV1Program = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
): Effect.Effect<TxOrderUTxOV1[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToTxOrderUTxOsV1(utxos, config.eventPolicyId),
  );

export const fetchTxOrderUTxOsV1 = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
) => makeReturn(fetchTxOrderUTxOsV1Program(lucid, config));

export type SubmitTxOrderReferenceScripts = {
  readonly txOrderMinting: UTxO;
};

export type SubmitTxOrderV1Config = {
  /** Exact bounded canonical native-V1 transaction bytes. */
  readonly nativeTxCbor: string;
  /** Reserved while all field fragments and their L1 receipts are prepared. */
  readonly nonceInput: UTxO;
  /** Exact terminal receipt for non-empty material; absent only for nine empty fields. */
  readonly terminalFieldReceiptUtxo?: UTxO;
  readonly refundAddress: TxOrderRefundAddressV1;
  readonly refundDatum?: CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitTxOrderReferenceScripts;
};

export type TxOrderFieldFragmentV1 = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly chunkIndex: number;
  readonly fieldName: string;
  readonly fieldEncodedSize: number;
  readonly datum: TxFieldPreimageV1;
  readonly datumCbor: string;
};

export type TxOrderFragmentBundleV1 = {
  readonly transactionId: string;
  readonly transactionCommitment: string;
  readonly source: NativeTxProofSourceV1;
  readonly fragments: readonly TxOrderFieldFragmentV1[];
};

const fieldChunkKeyV1 = (
  fieldIndex: number,
  itemIndex: number,
  chunkIndex: number,
): string =>
  `${fieldIndex.toString()}:${itemIndex.toString()}:${chunkIndex.toString()}`;

export const deriveTxOrderFragmentBundleV1 = ({
  nativeTxCbor,
  fieldReceiptPolicyId,
  txOrderPolicyId,
  txOrderId,
}: {
  readonly nativeTxCbor: Uint8Array;
  readonly fieldReceiptPolicyId: string;
  readonly txOrderPolicyId: string;
  readonly txOrderId: OutputReference;
}): TxOrderFragmentBundleV1 => {
  if (!/^[0-9a-f]{56}$/u.test(fieldReceiptPolicyId)) {
    throw new Error("field-receipt policy id must be 28-byte lowercase hex");
  }
  if (!/^[0-9a-f]{56}$/u.test(txOrderPolicyId)) {
    throw new Error("tx-order policy id must be 28-byte lowercase hex");
  }
  const violation = validateMidgardConsensusV1TxCbor(nativeTxCbor);
  if (violation !== null) {
    throw new Error(
      `${violation.code} ${violation.featureId}: ${violation.detail}`,
    );
  }
  const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor);
  const transactionId = computeMidgardNativeTxIdV1(tx).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSourceV1(tx);
  const transactionCommitment =
    computeMidgardNativeTxProofCommitmentV1(proofSource).toString("hex");
  const source: NativeTxProofSourceV1 = {
    compact_cbor: proofSource.compactCbor.toString("hex"),
    witness_set_compact_cbor: proofSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      proofSource.fieldPreimageLengthsCbor.toString("hex"),
  };
  const fragments = deriveMidgardV1TxFieldChunks(nativeTxCbor).map((field) => {
    const proof = field.proof;
    const datum: TxFieldPreimageV1 = {
      field_receipt_policy_id: fieldReceiptPolicyId,
      tx_order_policy_id: txOrderPolicyId,
      tx_order_id: txOrderId,
      transaction_commitment: transactionCommitment,
      collection_proof: {
        version: BigInt(field.collectionProof.version),
        field_index: BigInt(field.collectionProof.fieldIndex),
        item_count: BigInt(field.collectionProof.itemCount),
        item_index: BigInt(field.collectionProof.itemIndex),
        item_length: BigInt(field.collectionProof.itemLength),
        item_commitment: field.collectionProof.itemCommitment.toString("hex"),
        frontier: field.collectionProof.frontier.peaks.map((peak) => ({
          height: BigInt(peak.height),
          hash: peak.hash.toString("hex"),
        })),
        siblings: field.collectionProof.siblings.map((sibling) =>
          sibling.toString("hex"),
        ),
      },
      proof: {
        version: BigInt(proof.version),
        field_index: BigInt(proof.fieldIndex),
        item_index: BigInt(proof.itemIndex),
        total_length: BigInt(proof.totalLength),
        chunk_index: BigInt(proof.chunkIndex),
        chunk: proof.chunk.toString("hex"),
        frontier: proof.frontier.peaks.map((peak) => ({
          height: BigInt(peak.height),
          hash: peak.hash.toString("hex"),
        })),
        siblings: proof.siblings.map((sibling) => sibling.toString("hex")),
      },
    };
    return {
      fieldIndex: proof.fieldIndex,
      itemIndex: proof.itemIndex,
      chunkIndex: proof.chunkIndex,
      fieldName: field.fieldName,
      fieldEncodedSize: field.fieldEncodedSize,
      datum,
      datumCbor: Data.to(datum, TxFieldPreimageV1),
    };
  });
  return {
    transactionId,
    transactionCommitment,
    source,
    fragments,
  };
};

export const TxFieldReceiptMintRedeemerV1Schema = Data.Enum([
  Data.Object({
    PublishField: Data.Object({
      field_reference_input_index: Data.Integer(),
      predecessor_receipt_reference_input_index: Data.Integer(),
      receipt_output_index: Data.Integer(),
      transaction_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
      source: NativeTxProofSourceV1Schema,
    }),
  }),
  Data.Object({
    BurnReceipts: Data.Object({
      receipt_input_indices: Data.Array(Data.Integer()),
    }),
  }),
]);
export type TxFieldReceiptMintRedeemerV1 = Data.Static<
  typeof TxFieldReceiptMintRedeemerV1Schema
>;
export const TxFieldReceiptMintRedeemerV1 =
  TxFieldReceiptMintRedeemerV1Schema as unknown as TxFieldReceiptMintRedeemerV1;

export type PublishTxOrderFieldFragmentV1Config = {
  readonly fragment: TxOrderFieldFragmentV1;
  readonly lovelace?: bigint;
};

export type PublishCekProgramMaterialV1Config = {
  readonly entries: readonly MidgardCekProgramMaterialEntryV1[];
  readonly lovelacePerEntry?: bigint;
};

export type CekProgramMaterialPublicationV1 = {
  readonly entry: MidgardCekProgramMaterialEntryV1;
  readonly datum: CekProgramMaterialDatumV1;
  readonly datumCbor: string;
};

export const deriveCekProgramMaterialPublicationsV1 = (
  entries: readonly MidgardCekProgramMaterialEntryV1[],
): readonly CekProgramMaterialPublicationV1[] => {
  if (entries.length === 0) {
    throw new Error("CEK program-material publication cannot be empty");
  }
  const seen = new Set<string>();
  return Object.freeze(
    entries.map((entry) => {
      const exact = decodeMidgardCekProgramMaterialEntryV1(
        encodeMidgardCekProgramMaterialEntryV1(entry),
      );
      const root = Buffer.from(exact.root).toString("hex");
      if (seen.has(root)) {
        throw new Error(`duplicate CEK program-material root ${root}`);
      }
      seen.add(root);
      const datum: CekProgramMaterialDatumV1 = {
        kind: midgardCekProgramMaterialKindTagV1(exact.kind),
        root,
        preimage: exact.preimage.toString("hex"),
      };
      const datumCbor = Data.to(datum, CekProgramMaterialDatumV1);
      const datumBytes = Buffer.byteLength(datumCbor, "hex");
      if (datumBytes > MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes) {
        throw new Error(
          `CEK program-material datum ${root} exceeds the independently revealable L1 proof field bound`,
        );
      }
      return Object.freeze({ entry: exact, datum, datumCbor });
    }),
  );
};

export type PublishTxOrderFieldReceiptV1Config = {
  readonly nativeTxCbor: string;
  readonly fieldPreimageUtxo: UTxO;
  /** Required for every fragment except the first canonical chain position. */
  readonly predecessorReceiptUtxo?: UTxO;
  readonly lovelace?: bigint;
  readonly receiptMintingReferenceScript?: UTxO;
};

export type TxOrderFieldReceiptPublicationV1 = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly chunkIndex: number;
  readonly fieldReference: OutputReference;
  readonly transactionId: string;
  readonly source: NativeTxProofSourceV1;
  readonly receiptAssetName: string;
  readonly receiptUnit: string;
  readonly datum: TxFieldReceiptV1;
  readonly datumCbor: string;
};

const receiptAssetNameV1 = ({
  txOrderPolicyId,
  txOrderId,
  transactionCommitment,
  fieldIndex,
  itemIndex,
  chunkIndex,
}: {
  readonly txOrderPolicyId: string;
  readonly txOrderId: OutputReference;
  readonly transactionCommitment: string;
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly chunkIndex: number;
}): string =>
  deriveMidgardTxFieldReceiptAssetNameV1({
    txOrderPolicyId: Buffer.from(txOrderPolicyId, "hex"),
    txOrderTransactionId: Buffer.from(txOrderId.transactionId, "hex"),
    txOrderOutputIndex: txOrderId.outputIndex,
    transactionCommitment: Buffer.from(transactionCommitment, "hex"),
    fieldIndex,
    itemIndex,
    chunkIndex,
  }).toString("hex");

export const deriveTxOrderFieldReceiptPublicationV1 = ({
  contracts,
  nativeTxCbor,
  fieldPreimageUtxo,
  predecessorReceiptUtxo,
}: {
  readonly contracts: MidgardValidators;
  readonly nativeTxCbor: Uint8Array;
  readonly fieldPreimageUtxo: UTxO;
  readonly predecessorReceiptUtxo?: UTxO;
}): TxOrderFieldReceiptPublicationV1 => {
  const outRef = `${fieldPreimageUtxo.txHash}#${fieldPreimageUtxo.outputIndex.toString()}`;
  if (
    fieldPreimageUtxo.address !==
    contracts.txOrderFieldPreimage.spendingScriptAddress
  ) {
    throw new Error(
      `field fragment ${outRef} is not locked by the compiled V1 fragment validator`,
    );
  }
  if (fieldPreimageUtxo.datum == null) {
    throw new Error(`field fragment ${outRef} has no inline datum`);
  }
  const field = decodeTxFieldPreimageV1Cbor(
    Buffer.from(fieldPreimageUtxo.datum, "hex"),
  );
  if (
    field.field_receipt_policy_id !== contracts.txOrderFieldReceipt.policyId
  ) {
    throw new Error(
      `field fragment ${outRef} is bound to another receipt policy`,
    );
  }
  const fieldIndex = Number(field.proof.field_index);
  const itemIndex = Number(field.proof.item_index);
  const chunkIndex = Number(field.proof.chunk_index);
  if (!Number.isSafeInteger(fieldIndex) || fieldIndex < 0 || fieldIndex >= 9) {
    throw new Error(
      `field fragment ${outRef} has invalid field index ${field.proof.field_index.toString()}`,
    );
  }
  if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
    throw new Error(
      `field fragment ${outRef} has invalid item index ${field.proof.item_index.toString()}`,
    );
  }
  if (!Number.isSafeInteger(chunkIndex) || chunkIndex < 0) {
    throw new Error(
      `field fragment ${outRef} has invalid chunk index ${field.proof.chunk_index.toString()}`,
    );
  }
  const bundle = deriveTxOrderFragmentBundleV1({
    nativeTxCbor,
    fieldReceiptPolicyId: contracts.txOrderFieldReceipt.policyId,
    txOrderPolicyId: field.tx_order_policy_id,
    txOrderId: field.tx_order_id,
  });
  const expected = bundle.fragments.find(
    (fragment) =>
      fragment.fieldIndex === fieldIndex &&
      fragment.itemIndex === itemIndex &&
      fragment.chunkIndex === chunkIndex,
  );
  if (
    expected === undefined ||
    Data.to(field, TxFieldPreimageV1) !== expected.datumCbor
  ) {
    throw new Error(
      `field fragment ${outRef} does not match the exact transaction commitment at field ${fieldIndex.toString()}, item ${itemIndex.toString()}, chunk ${chunkIndex.toString()}`,
    );
  }
  const ordinal = bundle.fragments.indexOf(expected);
  const predecessorReference =
    predecessorReceiptUtxo === undefined
      ? null
      : outputReferenceFromUTxO(predecessorReceiptUtxo);
  if (ordinal === 0) {
    if (predecessorReceiptUtxo !== undefined) {
      throw new Error("first V1 field receipt cannot have a predecessor");
    }
  } else {
    if (predecessorReceiptUtxo === undefined) {
      throw new Error(
        `field receipt at ordinal ${ordinal.toString()} requires its immediate predecessor`,
      );
    }
    const predecessorOutRef = `${predecessorReceiptUtxo.txHash}#${predecessorReceiptUtxo.outputIndex.toString()}`;
    if (
      predecessorReceiptUtxo.address !==
      contracts.txOrderFieldReceipt.spendingScriptAddress
    ) {
      throw new Error(
        `predecessor receipt ${predecessorOutRef} is not locked by the compiled V1 receipt validator`,
      );
    }
    if (predecessorReceiptUtxo.datum == null) {
      throw new Error(`predecessor receipt ${predecessorOutRef} has no datum`);
    }
    const predecessor = decodeTxFieldReceiptV1Cbor(
      Buffer.from(predecessorReceiptUtxo.datum, "hex"),
    );
    const predecessorFragment = bundle.fragments[ordinal - 1]!;
    const predecessorAssetName = receiptAssetNameV1({
      txOrderPolicyId: field.tx_order_policy_id,
      txOrderId: field.tx_order_id,
      transactionCommitment: field.transaction_commitment,
      fieldIndex: predecessorFragment.fieldIndex,
      itemIndex: predecessorFragment.itemIndex,
      chunkIndex: predecessorFragment.chunkIndex,
    });
    const predecessorUnit = toUnit(
      contracts.txOrderFieldReceipt.policyId,
      predecessorAssetName,
    );
    if (
      predecessor.field_receipt_policy_id !==
        contracts.txOrderFieldReceipt.policyId ||
      predecessor.tx_order_policy_id !== field.tx_order_policy_id ||
      Data.to(predecessor.tx_order_id, OutputReference) !==
        Data.to(field.tx_order_id, OutputReference) ||
      predecessor.transaction_commitment !== field.transaction_commitment ||
      Data.to(predecessor.collection_proof, BoundedCollectionItemProofV1) !==
        Data.to(
          predecessorFragment.datum.collection_proof,
          BoundedCollectionItemProofV1,
        ) ||
      predecessor.chunk_index !== predecessorFragment.datum.proof.chunk_index ||
      predecessor.field_encoded_size !==
        BigInt(predecessorFragment.fieldEncodedSize) ||
      (predecessorReceiptUtxo.assets[predecessorUnit] ?? 0n) !== 1n
    ) {
      throw new Error(
        `predecessor receipt ${predecessorOutRef} does not authenticate ordinal ${(ordinal - 1).toString()}`,
      );
    }
  }
  const fieldReference = outputReferenceFromUTxO(fieldPreimageUtxo);
  const receiptAssetName = receiptAssetNameV1({
    txOrderPolicyId: field.tx_order_policy_id,
    txOrderId: field.tx_order_id,
    transactionCommitment: field.transaction_commitment,
    fieldIndex,
    itemIndex,
    chunkIndex,
  });
  const receiptUnit = toUnit(
    contracts.txOrderFieldReceipt.policyId,
    receiptAssetName,
  );
  const datum: TxFieldReceiptV1 = {
    field_receipt_policy_id: contracts.txOrderFieldReceipt.policyId,
    tx_order_policy_id: field.tx_order_policy_id,
    tx_order_id: field.tx_order_id,
    transaction_commitment: field.transaction_commitment,
    collection_proof: field.collection_proof,
    chunk_index: field.proof.chunk_index,
    field_reference: fieldReference,
    predecessor_receipt_reference: predecessorReference,
    field_encoded_size: BigInt(expected.fieldEncodedSize),
  };
  return {
    fieldIndex,
    itemIndex,
    chunkIndex,
    fieldReference,
    transactionId: bundle.transactionId,
    source: bundle.source,
    receiptAssetName,
    receiptUnit,
    datum,
    datumCbor: Data.to(datum, TxFieldReceiptV1),
  };
};

const receiptUnitFromUtxoV1 = (
  utxo: UTxO,
  fieldReceiptPolicyId: string,
): string => {
  if (!/^[0-9a-f]{56}$/u.test(fieldReceiptPolicyId)) {
    throw new Error("field-receipt policy id must be 28-byte lowercase hex");
  }
  const matchingUnits = Object.entries(utxo.assets).filter(
    ([unit, quantity]) =>
      unit.startsWith(fieldReceiptPolicyId) &&
      unit.length === fieldReceiptPolicyId.length + 64 &&
      quantity === 1n,
  );
  if (matchingUnits.length !== 1) {
    throw new Error(
      `V1 receipt ${utxo.txHash}#${utxo.outputIndex.toString()} must contain exactly one receipt NFT`,
    );
  }
  return matchingUnits[0]![0];
};

export const txOrderFieldReceiptBurnAssetsV1 = (
  receiptUtxos: readonly UTxO[],
  fieldReceiptPolicyId: string,
): Assets => {
  if (receiptUtxos.length === 0) {
    throw new Error("V1 receipt burn requires at least one UTxO");
  }
  const units = receiptUtxos.map((utxo) =>
    receiptUnitFromUtxoV1(utxo, fieldReceiptPolicyId),
  );
  if (new Set(units).size !== units.length) {
    throw new Error("V1 receipt burn contains duplicate NFTs");
  }
  return Object.fromEntries(units.map((unit) => [unit, -1n]));
};

/**
 * Builds the receipt-policy burn redeemer after Lucid has fixed the canonical
 * input ordering. Indices follow receipt asset-name order, matching Aiken's
 * ordered token dictionary traversal.
 */
export const txOrderFieldReceiptBurnRedeemerV1 = (
  receiptUtxos: readonly UTxO[],
  fieldReceiptPolicyId: string,
): BuildTxWithRedeemer => {
  const ordered = receiptUtxos
    .map((utxo) => ({
      utxo,
      unit: receiptUnitFromUtxoV1(utxo, fieldReceiptPolicyId),
    }))
    .sort((left, right) => left.unit.localeCompare(right.unit));
  if (
    ordered.length === 0 ||
    new Set(ordered.map(({ unit }) => unit)).size !== ordered.length
  ) {
    throw new Error("V1 receipt burn requires distinct receipt NFTs");
  }
  return (ctx) => {
    requireOwnMintPurpose(ctx, fieldReceiptPolicyId, "V1 field receipt burn");
    return Data.to(
      {
        BurnReceipts: {
          receipt_input_indices: ordered.map(({ utxo }) =>
            requireInputIndex(ctx, utxo, "V1 field receipt burn"),
          ),
        },
      } satisfies TxFieldReceiptMintRedeemerV1,
      TxFieldReceiptMintRedeemerV1,
    );
  };
};

export const buildUnsignedTxOrderFieldFragmentV1Program = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishTxOrderFieldFragmentV1Config,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      if (
        config.fragment.datum.field_receipt_policy_id !==
        contracts.txOrderFieldReceipt.policyId
      ) {
        throw new Error("V1 field fragment is bound to another receipt policy");
      }
      if (
        Data.to(config.fragment.datum, TxFieldPreimageV1) !==
        config.fragment.datumCbor
      ) {
        throw new Error("V1 field fragment datum CBOR is not canonical");
      }
      return lucid
        .newTx()
        .pay.ToAddressWithData(
          contracts.txOrderFieldPreimage.spendingScriptAddress,
          {
            kind: "inline",
            value: config.fragment.datumCbor,
          },
          { lovelace: config.lovelace ?? DEFAULT_TX_ORDER_LOVELACE },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 field fragment",
        cause,
      }),
  });

export const buildUnsignedCekProgramMaterialV1Program = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialV1Config,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const publications = deriveCekProgramMaterialPublicationsV1(
        config.entries,
      );
      let tx = lucid.newTx();
      for (const publication of publications) {
        tx = tx.pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace: config.lovelacePerEntry ?? DEFAULT_TX_ORDER_LOVELACE,
          },
        );
      }
      return tx.complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 CEK program material",
        cause,
      }),
  });

export const buildUnsignedTxOrderFieldReceiptV1Program = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishTxOrderFieldReceiptV1Config,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      if (
        config.nativeTxCbor.length === 0 ||
        config.nativeTxCbor.length % 2 !== 0 ||
        !/^[0-9a-f]+$/iu.test(config.nativeTxCbor)
      ) {
        throw new Error(
          "nativeTxCbor must be non-empty, even-length hexadecimal",
        );
      }
      const nativeTxCbor = Buffer.from(config.nativeTxCbor, "hex");
      const publication = deriveTxOrderFieldReceiptPublicationV1({
        contracts,
        nativeTxCbor,
        fieldPreimageUtxo: config.fieldPreimageUtxo,
        predecessorReceiptUtxo: config.predecessorReceiptUtxo,
      });
      const materialReferenceInputs =
        config.predecessorReceiptUtxo === undefined
          ? [config.fieldPreimageUtxo]
          : [config.fieldPreimageUtxo, config.predecessorReceiptUtxo];
      const referenceInputs =
        config.receiptMintingReferenceScript === undefined
          ? materialReferenceInputs
          : [...materialReferenceInputs, config.receiptMintingReferenceScript];
      type ReceiptLayout = {
        readonly fieldReferenceInputIndex: bigint;
        readonly predecessorReceiptReferenceInputIndex: bigint;
        readonly receiptOutputIndex: bigint;
      };
      const encodeRedeemer = (layout: ReceiptLayout): string =>
        Data.to(
          {
            PublishField: {
              field_reference_input_index: layout.fieldReferenceInputIndex,
              predecessor_receipt_reference_input_index:
                layout.predecessorReceiptReferenceInputIndex,
              receipt_output_index: layout.receiptOutputIndex,
              transaction_id: publication.transactionId,
              source: publication.source,
            },
          } satisfies TxFieldReceiptMintRedeemerV1,
          TxFieldReceiptMintRedeemerV1,
        );
      let resolvedLayout: ReceiptLayout | undefined;
      const dynamicRedeemer = ((ctx) => {
        requireOwnMintPurpose(
          ctx,
          contracts.txOrderFieldReceipt.policyId,
          "V1 field receipt",
        );
        const layout: ReceiptLayout = {
          fieldReferenceInputIndex: requireReferenceInputIndex(
            ctx,
            config.fieldPreimageUtxo,
            "V1 field receipt",
          ),
          predecessorReceiptReferenceInputIndex:
            config.predecessorReceiptUtxo === undefined
              ? -1n
              : requireReferenceInputIndex(
                  ctx,
                  config.predecessorReceiptUtxo,
                  "V1 field receipt predecessor",
                ),
          receiptOutputIndex: requireUniqueOutputIndex(
            ctx.outputs,
            (output) => (output.assets[publication.receiptUnit] ?? 0n) === 1n,
            "V1 field receipt",
          ),
        };
        resolvedLayout = layout;
        return encodeRedeemer(layout);
      }) satisfies BuildTxWithRedeemer;
      const makeTx = (
        redeemer: BuildTxWithRedeemer | string,
      ): ReturnType<LucidEvolution["newTx"]> => {
        const baseTx = lucid.newTx().readFrom(referenceInputs);
        const withPolicy =
          config.receiptMintingReferenceScript === undefined
            ? baseTx.attach.MintingPolicy(
                contracts.txOrderFieldReceipt.mintingScript,
              )
            : baseTx;
        return withPolicy
          .mintAssets({ [publication.receiptUnit]: 1n }, redeemer)
          .pay.ToAddressWithData(
            contracts.txOrderFieldReceipt.spendingScriptAddress,
            { kind: "inline", value: publication.datumCbor },
            {
              lovelace: config.lovelace ?? DEFAULT_TX_ORDER_LOVELACE,
              [publication.receiptUnit]: 1n,
            },
          );
      };
      await makeTx(dynamicRedeemer).complete({ localUPLCEval: true });
      if (resolvedLayout === undefined) {
        throw new Error(
          "failed to resolve V1 field receipt transaction layout",
        );
      }
      return makeTx(encodeRedeemer(resolvedLayout)).complete({
        localUPLCEval: true,
      });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to mint V1 field receipt",
        cause,
      }),
  });

export type TxOrderBuildMetadata = {
  readonly txOrderAddress: string;
  readonly txOrderId: OutputReference;
  readonly authNonceCbor: string;
  readonly txOrderAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const DEFAULT_TX_ORDER_LOVELACE = 3_000_000n;

export const buildUnsignedTxOrderTxV1WithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderV1Config,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: TxOrderBuildMetadata;
  },
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  Effect.gen(function* () {
    const nativeTxCbor = yield* Effect.try({
      try: () => {
        if (
          config.nativeTxCbor.length === 0 ||
          config.nativeTxCbor.length % 2 !== 0 ||
          !/^[0-9a-f]+$/iu.test(config.nativeTxCbor)
        ) {
          throw new Error(
            "nativeTxCbor must be non-empty, even-length hexadecimal",
          );
        }
        return Buffer.from(config.nativeTxCbor, "hex");
      },
      catch: (cause) =>
        new UserEventBuildError({
          message:
            "V1 tx order requires exact bounded canonical native V1 bytes",
          cause,
        }),
    });
    const context = yield* prepareUserEventMintContext({
      lucid,
      contracts,
      label: "tx order",
      eventPolicyId: contracts.txOrder.policyId,
      hubOraclePolicyField: "tx_order",
      hubOracleAddressField: "tx_order_addr",
      nonceInput: config.nonceInput,
    });
    const {
      eventUnit: txOrderUnit,
      hubOracleRefInput,
      inclusionTime,
      network,
      nonceInput,
      validTo,
      witnessScript,
      witnessScriptHash,
    } = context;
    const txOrderId = outputReferenceFromUTxO(nonceInput);
    const authNonceCbor = outputReferenceToPlutusDataCbor(nonceInput);
    const bundle = yield* Effect.try({
      try: () =>
        deriveTxOrderFragmentBundleV1({
          nativeTxCbor,
          fieldReceiptPolicyId: contracts.txOrderFieldReceipt.policyId,
          txOrderPolicyId: contracts.txOrder.policyId,
          txOrderId,
        }),
      catch: (cause) =>
        new UserEventBuildError({
          message: "Failed to derive V1 tx-order fragments",
          cause,
        }),
    });
    const terminalFieldReceiptUtxo = yield* Effect.try({
      try: () => {
        if (bundle.fragments.length === 0) {
          if (config.terminalFieldReceiptUtxo !== undefined) {
            throw new Error(
              "nine empty dynamic fields cannot specify a terminal receipt",
            );
          }
          return undefined;
        }
        const utxo = config.terminalFieldReceiptUtxo;
        if (utxo === undefined) {
          throw new Error("non-empty V1 material requires a terminal receipt");
        }
        const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
        if (
          utxo.address !== contracts.txOrderFieldReceipt.spendingScriptAddress
        ) {
          throw new Error(
            `terminal receipt ${outRef} is not locked by the compiled V1 receipt validator`,
          );
        }
        if (utxo.datum == null) {
          throw new Error(`terminal receipt ${outRef} has no inline datum`);
        }
        const receipt = decodeTxFieldReceiptV1Cbor(
          Buffer.from(utxo.datum, "hex"),
        );
        const terminal = bundle.fragments.at(-1)!;
        const expectedAssetName = receiptAssetNameV1({
          txOrderPolicyId: contracts.txOrder.policyId,
          txOrderId,
          transactionCommitment: bundle.transactionCommitment,
          fieldIndex: terminal.fieldIndex,
          itemIndex: terminal.itemIndex,
          chunkIndex: terminal.chunkIndex,
        });
        const expectedUnit = toUnit(
          contracts.txOrderFieldReceipt.policyId,
          expectedAssetName,
        );
        if (
          receipt.field_receipt_policy_id !==
            contracts.txOrderFieldReceipt.policyId ||
          receipt.tx_order_policy_id !== contracts.txOrder.policyId ||
          Data.to(receipt.tx_order_id, OutputReference) !==
            Data.to(txOrderId, OutputReference) ||
          receipt.transaction_commitment !== bundle.transactionCommitment ||
          Data.to(receipt.collection_proof, BoundedCollectionItemProofV1) !==
            Data.to(
              terminal.datum.collection_proof,
              BoundedCollectionItemProofV1,
            ) ||
          receipt.chunk_index !== terminal.datum.proof.chunk_index ||
          receipt.field_encoded_size !== BigInt(terminal.fieldEncodedSize) ||
          (bundle.fragments.length === 1
            ? receipt.predecessor_receipt_reference !== null
            : receipt.predecessor_receipt_reference === null) ||
          (utxo.assets[expectedUnit] ?? 0n) !== 1n
        ) {
          throw new Error(
            `terminal receipt ${outRef} does not authenticate ${fieldChunkKeyV1(
              terminal.fieldIndex,
              terminal.itemIndex,
              terminal.chunkIndex,
            )}`,
          );
        }
        return utxo;
      },
      catch: (cause) =>
        new UserEventBuildError({
          message: "V1 tx-order terminal receipt is missing or unauthenticated",
          cause,
        }),
    });
    const txOrderDatum: TxOrderDatumV1 = {
      event: {
        id: txOrderId,
        tx: {
          tx_id: bundle.transactionId,
          transaction_commitment: bundle.transactionCommitment,
          source: bundle.source,
          terminal_receipt_reference:
            terminalFieldReceiptUtxo === undefined
              ? null
              : outputReferenceFromUTxO(terminalFieldReceiptUtxo),
        },
      },
      inclusion_time: BigInt(inclusionTime),
      witness: witnessScriptHash,
      refund_address: config.refundAddress,
      refund_datum: config.refundDatum ?? "NoDatum",
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatumV1);
    const outputAssets: Assets = {
      lovelace: config.lovelace ?? DEFAULT_TX_ORDER_LOVELACE,
      [txOrderUnit]: 1n,
    };
    const referenceInputs =
      config.referenceScripts === undefined
        ? terminalFieldReceiptUtxo === undefined
          ? [hubOracleRefInput]
          : [hubOracleRefInput, terminalFieldReceiptUtxo]
        : terminalFieldReceiptUtxo === undefined
          ? [hubOracleRefInput, config.referenceScripts.txOrderMinting]
          : [
              hubOracleRefInput,
              config.referenceScripts.txOrderMinting,
              terminalFieldReceiptUtxo,
            ];
    const witnessRegistrationRedeemer =
      encodeUserEventWitnessMintOrBurnRedeemer(contracts.txOrder.policyId);
    const tx = yield* buildCompletedUserEventMintTxProgram({
      lucid,
      network,
      nonceInput,
      eventUnit: txOrderUnit,
      eventAddress: contracts.txOrder.spendingScriptAddress,
      eventDatumCbor: txOrderDatumCBOR,
      outputAssets,
      validTo,
      mintingPolicy: contracts.txOrder.mintingScript,
      attachMintingPolicy: config.referenceScripts === undefined,
      referenceInputs,
      hubOracleRefInput,
      witnessScript,
      witnessRegistrationRedeemer,
      label: "tx order",
    });
    return {
      tx,
      metadata: {
        txOrderAddress: contracts.txOrder.spendingScriptAddress,
        txOrderId,
        authNonceCbor,
        txOrderAuthUnit: txOrderUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
  }).pipe(
    Effect.catchAllDefect((defect) =>
      Effect.fail(
        new LucidError({
          message: "Caught defect from V1 txOrderTxBuilder",
          cause: defect,
        }),
      ),
    ),
  );

export const unsignedTxOrderTxV1Program = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderV1Config,
) =>
  buildUnsignedTxOrderTxV1WithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedTxOrderTxV1Program = unsignedTxOrderTxV1Program;

export const unsignedTxOrderTxV1 = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  txOrderParams: SubmitTxOrderV1Config,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedTxOrderTxV1Program(lucid, contracts, txOrderParams),
  ).unsafeRun();

export const unsignedTxOrderFieldFragmentV1 = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishTxOrderFieldFragmentV1Config,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedTxOrderFieldFragmentV1Program(lucid, contracts, config),
  ).unsafeRun();

export const unsignedTxOrderFieldReceiptV1 = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishTxOrderFieldReceiptV1Config,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedTxOrderFieldReceiptV1Program(lucid, contracts, config),
  ).unsafeRun();

export const unsignedCekProgramMaterialV1 = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialV1Config,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekProgramMaterialV1Program(lucid, contracts, config),
  ).unsafeRun();
