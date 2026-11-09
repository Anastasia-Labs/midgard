import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FieldCarriage } from "../native-tx-field-access.js";
import {
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME_HEX,
  FieldGrammarCertificate,
  FieldGrammarCertificateMintRedeemer,
  FieldGrammarCertificateSpendRedeemer,
} from "./field-grammar-certificate.schemas.js";
import {
  resolveProtocolParameters,
  stabilizedMinimumLovelace,
} from "./field-preimage-carriage.build-unsigned-field-preimage-publication-program.js";

/** The L2 transaction a certificate's walk authenticates its field against. */
export type FieldGrammarCertificateSource = Readonly<{
  sourceKind: 0n | 1n;
  compactCbor: string;
  /**
   * The transaction's witness-set compact CBOR. Every open and advance decodes
   * it, so it must always be well formed; for a witness field (6–8) it must
   * also hash to the compact structure's `witness_set_hash`.
   */
  witnessSetCompactCbor: string;
}>;

/**
 * How the field's preimage reaches the transaction, resolved against the
 * transaction's complete, canonically ordered reference-input set.
 */
export type FieldGrammarCertificateCarriage = (
  referenceInputs: readonly UTxO[],
) => FieldCarriage;

const requireBatch = (batch: number): bigint => {
  if (!Number.isSafeInteger(batch) || batch <= 0) {
    throw new Error("a field grammar batch must be a positive item count");
  }
  return BigInt(batch);
};

/** The mint `Open` redeemer. */
export const openFieldGrammarCertificateRedeemer = ({
  source,
  carriage,
  batch,
  outputIndex,
}: {
  readonly source: FieldGrammarCertificateSource;
  readonly carriage: FieldCarriage;
  readonly batch: number;
  readonly outputIndex: number;
}): string =>
  Data.to(
    {
      Open: {
        source_kind: source.sourceKind,
        compact_cbor: source.compactCbor,
        witness_set_compact_cbor: source.witnessSetCompactCbor,
        carriage,
        batch: requireBatch(batch),
        output_index: BigInt(outputIndex),
      },
    },
    FieldGrammarCertificateMintRedeemer,
  );

/** The mint `Retire` redeemer — the burn; the spend handler checks the owner. */
export const retireFieldGrammarCertificateRedeemer = (): string =>
  Data.to("Retire", FieldGrammarCertificateMintRedeemer);

/** The spend `Advance` redeemer, resuming from `checkpoint`. */
export const advanceFieldGrammarCertificateRedeemer = ({
  source,
  checkpoint,
  carriage,
  batch,
  outputIndex,
}: {
  readonly source: FieldGrammarCertificateSource;
  /** The checkpoint whose digest the spent datum holds, as hex. */
  readonly checkpoint: string;
  readonly carriage: FieldCarriage;
  readonly batch: number;
  readonly outputIndex: number;
}): string =>
  Data.to(
    {
      Advance: {
        source_kind: source.sourceKind,
        compact_cbor: source.compactCbor,
        witness_set_compact_cbor: source.witnessSetCompactCbor,
        checkpoint,
        carriage,
        batch: requireBatch(batch),
        output_index: BigInt(outputIndex),
      },
    },
    FieldGrammarCertificateSpendRedeemer,
  );

/** The spend `Reclaim` redeemer. */
export const reclaimFieldGrammarCertificateRedeemer = (): string =>
  Data.to("Reclaim", FieldGrammarCertificateSpendRedeemer);

/** The smallest lovelace a certificate output holding `datumCbor` needs. */
export const minimumLovelaceForFieldGrammarCertificate = ({
  certificateAddress,
  certificatePolicyId,
  datumCbor,
  coinsPerUtxoByte,
}: {
  readonly certificateAddress: string;
  readonly certificatePolicyId: string;
  readonly datumCbor: string;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(certificatePolicyId),
    CML.AssetName.from_hex(FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME_HEX),
    1n,
  );
  return stabilizedMinimumLovelace({
    addressBech32: certificateAddress,
    datumCbor,
    coinsPerUtxoByte,
    assets,
    label: "field grammar certificate",
  });
};

/** The certificate token's unit under `certificatePolicyId`. */
export const fieldGrammarCertificateUnit = (certificatePolicyId: string) =>
  `${certificatePolicyId}${FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME_HEX}`;

/**
 * Whether `utxo` is a `Complete` certificate of the policy for this field:
 * at the policy's own address, exactly one token, and a datum naming the same
 * transaction, field, commitment and length. Two such certificates differ at
 * most in `owner`, so any one serves.
 */
export const isCompleteFieldGrammarCertificate = (
  utxo: UTxO,
  {
    certificatePolicyId,
    txIdHex,
    fieldIndex,
    fieldHashHex,
    totalLength,
  }: {
    readonly certificatePolicyId: string;
    readonly txIdHex: string;
    readonly fieldIndex: number;
    readonly fieldHashHex: string;
    readonly totalLength: number;
  },
): boolean => {
  if (
    (utxo.assets[fieldGrammarCertificateUnit(certificatePolicyId)] ?? 0n) !==
      1n ||
    utxo.datum === null ||
    utxo.datum === undefined
  ) {
    return false;
  }
  try {
    const credential = CML.Address.from_bech32(utxo.address).payment_cred();
    if (credential?.as_script()?.to_hex() !== certificatePolicyId) return false;
    const certificate = Data.from(utxo.datum, FieldGrammarCertificate);
    return (
      "Complete" in certificate &&
      certificate.Complete.tx_id === txIdHex &&
      certificate.Complete.field_index === BigInt(fieldIndex) &&
      certificate.Complete.field_hash === fieldHashHex &&
      certificate.Complete.total_length === BigInt(totalLength)
    );
  } catch {
    return false;
  }
};

/**
 * The positional index of a `Complete` certificate for this field in a
 * transaction's reference-input set — the `cert_ref_input_index` a consumer's
 * redeemer names. Indices count into the ledger's canonically ordered list, so
 * `referenceInputs` must be the transaction's complete set.
 */
export const resolveFieldGrammarCertificateReferenceIndex = ({
  referenceInputs,
  label,
  ...field
}: Parameters<typeof isCompleteFieldGrammarCertificate>[1] & {
  readonly referenceInputs: readonly UTxO[];
  readonly label: string;
}): number => {
  const index = [...referenceInputs]
    .sort(compareOutRefs)
    .findIndex((utxo) => isCompleteFieldGrammarCertificate(utxo, field));
  if (index === -1) {
    throw new Error(
      `${label} field grammar certificate (policy ${field.certificatePolicyId}, tx ${field.txIdHex}, field ${field.fieldIndex.toString()}) is not among the transaction's reference inputs`,
    );
  }
  return index;
};

type CertificateOutput = Readonly<{
  certificatePolicyId: string;
  certificateAddress: string;
  /** The datum the validator computes for this batch (the core mirror's). */
  datumCbor: string;
  lovelace: bigint;
}>;

const certificateOutputIndex = (
  outputs: Parameters<typeof requireUniqueOutputIndex>[0],
  { certificateAddress, certificatePolicyId }: CertificateOutput,
): number =>
  Number(
    requireUniqueOutputIndex(
      outputs,
      (output) =>
        output.address === certificateAddress &&
        output.assets[fieldGrammarCertificateUnit(certificatePolicyId)] === 1n,
      "field grammar certificate",
    ),
  );

const requireDeposit = async (
  lucid: LucidEvolution,
  output: CertificateOutput,
): Promise<bigint> => {
  const { coinsPerUtxoByte } = await resolveProtocolParameters(lucid);
  const minimum = minimumLovelaceForFieldGrammarCertificate({
    ...output,
    coinsPerUtxoByte,
  });
  return output.lovelace > minimum ? output.lovelace : minimum;
};

const failure = (action: string) => (cause: unknown) =>
  new Error(
    `Failed to ${action} a field grammar certificate: ${
      cause instanceof Error ? cause.message : String(cause)
    }`,
  );

/**
 * Opens a certificate and runs its first `batch` items: one token minted
 * into the datum the validator computes, at the policy's own address.
 * `referenceInputs` carries the policy's reference script and the field's
 * carriage; nothing else is spent from the certificate address.
 */
export const buildUnsignedFieldGrammarCertificateOpenProgram = (
  lucid: LucidEvolution,
  {
    source,
    carriage,
    batch,
    referenceInputs,
    ...output
  }: CertificateOutput & {
    readonly source: FieldGrammarCertificateSource;
    readonly carriage: FieldGrammarCertificateCarriage;
    readonly batch: number;
    readonly referenceInputs: readonly UTxO[];
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const lovelace = await requireDeposit(lucid, output);
      const unit = fieldGrammarCertificateUnit(output.certificatePolicyId);
      return await lucid
        .newTx()
        .readFrom([...referenceInputs])
        .mintAssets({ [unit]: 1n }, ((ctx) =>
          openFieldGrammarCertificateRedeemer({
            source,
            carriage: carriage(ctx.referenceInputs),
            batch,
            outputIndex: certificateOutputIndex(ctx.outputs, output),
          })) satisfies BuildTxWithRedeemer)
        .pay.ToAddressWithData(
          output.certificateAddress,
          { kind: "inline", value: output.datumCbor },
          { lovelace, [unit]: 1n },
        )
        .complete({ localUPLCEval: true });
    },
    catch: failure("open"),
  });

/**
 * Runs the next `batch` items of an `InProgress` certificate: the token moves
 * to a continuing output holding the datum the validator computes, and the
 * deposit never shrinks. Anyone may advance; no signature is needed.
 */
export const buildUnsignedFieldGrammarCertificateAdvanceProgram = (
  lucid: LucidEvolution,
  {
    certificateUtxo,
    source,
    checkpoint,
    carriage,
    batch,
    referenceInputs,
    ...output
  }: CertificateOutput & {
    readonly certificateUtxo: UTxO;
    readonly source: FieldGrammarCertificateSource;
    /** The checkpoint whose digest the spent datum holds, as hex. */
    readonly checkpoint: string;
    readonly carriage: FieldGrammarCertificateCarriage;
    readonly batch: number;
    readonly referenceInputs: readonly UTxO[];
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const spent = certificateUtxo.assets.lovelace ?? 0n;
      const lovelace = await requireDeposit(lucid, {
        ...output,
        lovelace: output.lovelace > spent ? output.lovelace : spent,
      });
      const unit = fieldGrammarCertificateUnit(output.certificatePolicyId);
      return await lucid
        .newTx()
        .readFrom([...referenceInputs])
        .collectFrom([certificateUtxo], ((ctx) => {
          requireOwnSpendPurpose(
            ctx,
            certificateUtxo,
            "field grammar certificate",
          );
          return advanceFieldGrammarCertificateRedeemer({
            source,
            checkpoint,
            carriage: carriage(ctx.referenceInputs),
            batch,
            outputIndex: certificateOutputIndex(ctx.outputs, output),
          });
        }) satisfies BuildTxWithRedeemer)
        .pay.ToAddressWithData(
          output.certificateAddress,
          { kind: "inline", value: output.datumCbor },
          { lovelace, [unit]: 1n },
        )
        .complete({ localUPLCEval: true });
    },
    catch: failure("advance"),
  });

/**
 * The owner takes a certificate's deposit back and its token is burnt.
 * `referenceInputs` carries the policy's reference script.
 */
export const buildUnsignedFieldGrammarCertificateReclaimProgram = (
  lucid: LucidEvolution,
  {
    certificateUtxo,
    certificatePolicyId,
    ownerKeyHash,
    referenceInputs,
  }: {
    readonly certificateUtxo: UTxO;
    readonly certificatePolicyId: string;
    readonly ownerKeyHash: string;
    readonly referenceInputs: readonly UTxO[];
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () =>
      await lucid
        .newTx()
        .readFrom([...referenceInputs])
        .collectFrom(
          [certificateUtxo],
          reclaimFieldGrammarCertificateRedeemer(),
        )
        .mintAssets(
          { [fieldGrammarCertificateUnit(certificatePolicyId)]: -1n },
          retireFieldGrammarCertificateRedeemer(),
        )
        .addSignerKey(ownerKeyHash)
        .complete({ localUPLCEval: true }),
    catch: failure("reclaim"),
  });
