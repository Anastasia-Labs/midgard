import {
  decodeMidgardCekProgramEnvelopeV1,
  decodeMidgardCekProgramMaterialEntryV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  hashMidgardCekProgramEnvelopeV1,
  type MidgardCekProgramMaterialEntryV1,
  midgardCekProgramMaterialKindTagV1,
  verifyMidgardCekProgramMaterialBundleV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core/codec";
import {
  type MidgardFieldCarriagePlanV1,
  planMidgardFieldCarriageV1,
} from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import {
  encodeMidgardFieldArrayHeaderV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  deriveMidgardV1TxFieldPreimages,
  validateMidgardConsensusV1TxCbor,
} from "@al-ft/midgard-core/consensus-validation-v1";
import {
  type Assets,
  CML,
  Data,
  LucidEvolution,
  type ProtocolParameters,
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
  CardanoDatum,
  CardanoDatumSchema,
  CekProgramMaterialDatumV1,
  CekProgramMaterialDatumV1Schema,
  MidgardTxValiditySchema,
  NativeTxProofSourceV1,
  TxOrderEventV1Schema,
} from "@/ledger-state.js";
import { RawRootMembershipProofSchema } from "@/transition-trace.js";

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

export const decodeCekProgramMaterialDatumV1Cbor = (
  bytes: Uint8Array,
): CekProgramMaterialDatumV1 =>
  decodeCanonicalPlutusDataV1(
    bytes,
    CekProgramMaterialDatumV1Schema,
    "CekProgramMaterialDatumV1",
  );

export const CEK_SINGLE_PUBLICATION_DATUM_V1_VERSION = 1n;

/** Exact datum ABI for one immutable, reference-only complete CEK graph. */
export const CekSinglePublicationDatumV1Schema = Data.Object({
  version: Data.Integer(),
  program_envelope_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  sidecar_cbor: Data.Bytes(),
});
export type CekSinglePublicationDatumV1 = Data.Static<
  typeof CekSinglePublicationDatumV1Schema
>;
export const CekSinglePublicationDatumV1 =
  CekSinglePublicationDatumV1Schema as unknown as CekSinglePublicationDatumV1;

const assertCekSinglePublicationDatumV1 = (
  datum: CekSinglePublicationDatumV1,
): void => {
  if (datum.version !== CEK_SINGLE_PUBLICATION_DATUM_V1_VERSION) {
    throw new Error("CEK single-publication datum must use version 1");
  }
};

export const encodeCekSinglePublicationDatumV1Cbor = (
  datum: CekSinglePublicationDatumV1,
): Buffer => {
  assertCekSinglePublicationDatumV1(datum);
  const encoded = encodeCanonicalPlutusDataV1(
    datum,
    CekSinglePublicationDatumV1Schema,
  );
  if (
    encoded.length >
    MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationDatumBytes
  ) {
    throw new Error(
      "CEK single-publication datum exceeds the reliable complete-item datum envelope",
    );
  }
  return encoded;
};

export const decodeCekSinglePublicationDatumV1Cbor = (
  bytes: Uint8Array,
): CekSinglePublicationDatumV1 => {
  const datum = decodeCanonicalPlutusDataV1(
    bytes,
    CekSinglePublicationDatumV1Schema,
    "CekSinglePublicationDatumV1",
  ) as CekSinglePublicationDatumV1;
  assertCekSinglePublicationDatumV1(datum);
  // Reuse the encoder so decoded data is also bounded by the pinned
  // single-publication datum envelope.
  encodeCekSinglePublicationDatumV1Cbor(datum);
  return Object.freeze({ ...datum });
};

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
  /** Reserved while the order's §8 field carriage is prepared. */
  readonly nonceInput: UTxO;
  readonly refundAddress: TxOrderRefundAddressV1;
  readonly refundDatum?: CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitTxOrderReferenceScripts;
};

/**
 * One non-empty field of a forced order's material, with the §8 carriage its
 * preimage requires.
 *
 * `plan` comes straight from {@link planMidgardFieldCarriageV1}, so the tier is
 * §8.4's partition rather than this module's choice, and `publications` is the
 * exact set of raw carriage UTxOs a publisher has to create.
 */
export type TxOrderFieldCarriageV1 = {
  readonly fieldIndex: number;
  readonly fieldName: string;
  /** The §5.1 enveloped field preimage. */
  readonly preimage: Buffer;
  /**
   * The §4 flat commitment over {@link preimage}.
   *
   * **This is not yet what the compact structure beside it carries.**
   * `deriveNativeTxBodyCompact` still derives the nine field commitments under
   * the retired counted bounded-collection scheme — a residual it documents in
   * full and that #585 owns — so a field's committed hash and its §4 commitment
   * disagree in TypeScript today while the Aiken side has derived §4 since #567.
   * The value here is the §4 one, because that is what the §8 carriage plan and
   * the on-chain door authenticate against; it is deliberately not asserted
   * equal to the compact structure's hash, which would only turn one lane's
   * known residual into another lane's red.
   */
  readonly commitment: string;
  readonly plan: MidgardFieldCarriagePlanV1;
};

/**
 * What a forced order binds itself to: the §3 transaction id, the proof-source
 * triple its datum carries, and the §8 carriage of every field with material in
 * it.
 *
 * This replaced `deriveTxOrderFragmentBundleV1`, which produced counted per-item
 * `TxFieldPreimageV1` fragments for the retired publication receipt chain
 * (#587). The nine field preimages are the same bytes either way — what changed
 * is that a field is now committed by one flat hash over the whole preimage
 * (§4), so there are no per-item openings to publish and the unit of carriage is
 * the field, not the item.
 */
export type TxOrderMaterialV1 = {
  readonly transactionId: string;
  readonly transactionCommitment: string;
  readonly source: NativeTxProofSourceV1;
  /**
   * One entry per field whose §5.1 preimage is not the empty field `80`, in
   * ascending field index. Empty for a transaction with nine empty fields.
   */
  readonly carriage: readonly TxOrderFieldCarriageV1[];
};

/**
 * Derives a forced order's transaction binding and the §8 carriage its material
 * requires.
 *
 * `owner` is the §8.6 min-Ada reclaim authority a tier-3 plan records. The only
 * caller today is `buildTxOrderV1`, which passes the publish transaction's
 * `witnessScriptHash` — the event witness script's hash, not a key hash — and
 * nothing reads the field back, because no tier-3 plan is publishable while the
 * §8.6 certificate carriage is undeployable (#589). Whether the reclaim
 * authority should be the publisher's own key hash or the witness script is a
 * decision that belongs with the step that will first spend a carriage output,
 * so it is settled there rather than guessed here; #589 owns it.
 */
export const deriveTxOrderMaterialV1 = ({
  nativeTxCbor,
  owner,
}: {
  readonly nativeTxCbor: Uint8Array;
  readonly owner: Uint8Array;
}): TxOrderMaterialV1 => {
  const violation = validateMidgardConsensusV1TxCbor(nativeTxCbor);
  if (violation !== null) {
    throw new Error(
      `${violation.code} ${violation.featureId}: ${violation.detail}`,
    );
  }
  const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor);
  const transactionId = computeMidgardNativeTxIdV1(tx);
  const proofSource = deriveMidgardNativeTxProofSourceV1(tx);
  const source: NativeTxProofSourceV1 = {
    compact_cbor: proofSource.compactCbor.toString("hex"),
    witness_set_compact_cbor: proofSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      proofSource.fieldPreimageLengthsCbor.toString("hex"),
  };
  const carriage: TxOrderFieldCarriageV1[] = [];
  // §5.1's empty field is the one-byte definite-array header `80`. The on-chain
  // `next_non_empty_field` decides the same thing by comparing the committed
  // hash against `empty_field_commitment`, which is the *derived* form of this
  // test; it cannot be reproduced here while `deriveNativeTxBodyCompact` is
  // still counted (#585), and the preimage bytes are the thing both spellings are
  // about anyway.
  const emptyFieldPreimage = encodeMidgardFieldArrayHeaderV1(0);
  for (const field of deriveMidgardV1TxFieldPreimages(nativeTxCbor)) {
    if (field.preimageCbor.equals(emptyFieldPreimage)) {
      continue;
    }
    carriage.push({
      fieldIndex: field.fieldIndex,
      fieldName: field.fieldName,
      preimage: field.preimageCbor,
      commitment: midgardFieldCommitmentV1(field.preimageCbor).toString("hex"),
      plan: planMidgardFieldCarriageV1({
        owner,
        txId: transactionId,
        fieldIndex: field.fieldIndex,
        preimage: field.preimageCbor,
      }),
    });
  }
  return {
    transactionId: transactionId.toString("hex"),
    transactionCommitment:
      computeMidgardNativeTxProofCommitmentV1(proofSource).toString("hex"),
    source,
    carriage: Object.freeze(carriage),
  };
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

export type CekSinglePublicationV1 = {
  readonly programEnvelopeHash: string;
  readonly datum: CekSinglePublicationDatumV1;
  readonly datumCbor: string;
};

/**
 * Derives the sole immutable datum for a complete CEK graph. Both inputs are
 * copied before validation so later caller mutation cannot alter publication
 * identity or bytes.
 */
export const deriveCekSinglePublicationV1 = ({
  envelopeCbor,
  sidecarCbor,
}: {
  readonly envelopeCbor: Uint8Array;
  readonly sidecarCbor: Uint8Array;
}): CekSinglePublicationV1 => {
  const exactEnvelopeCbor = Buffer.from(envelopeCbor);
  const exactSidecarCbor = Buffer.from(sidecarCbor);
  const envelope = decodeMidgardCekProgramEnvelopeV1(exactEnvelopeCbor);
  const material = decodeMidgardCekProgramMaterialSidecarV1(exactSidecarCbor);
  if (
    !encodeMidgardCekProgramMaterialSidecarV1(material).equals(exactSidecarCbor)
  ) {
    throw new Error("CEK single-publication sidecar CBOR is not canonical");
  }
  verifyMidgardCekProgramMaterialBundleV1([envelope], material);
  const programEnvelopeHash = Buffer.from(
    hashMidgardCekProgramEnvelopeV1(envelope),
  ).toString("hex");
  const datum: CekSinglePublicationDatumV1 = Object.freeze({
    version: CEK_SINGLE_PUBLICATION_DATUM_V1_VERSION,
    program_envelope_hash: programEnvelopeHash,
    sidecar_cbor: exactSidecarCbor.toString("hex"),
  });
  return Object.freeze({
    programEnvelopeHash,
    datum,
    datumCbor: encodeCekSinglePublicationDatumV1Cbor(datum).toString("hex"),
  });
};

export type PublishCekSinglePublicationV1Config = {
  readonly envelopeCbor: Uint8Array;
  readonly sidecarCbor: Uint8Array;
  /** May increase funding, but cannot underfund the exact minimum Ada. */
  readonly lovelace?: bigint;
};

const MIN_ADA_STABILIZATION_LIMIT = 8;

const resolveProtocolParameters = async (
  lucid: LucidEvolution,
): Promise<ProtocolParameters> => {
  const config = lucid.config();
  if (config.protocolParameters !== undefined) {
    return config.protocolParameters;
  }
  if (config.provider === undefined) {
    throw new Error("Lucid provider is not configured.");
  }
  return await config.provider.getProtocolParameters();
};

/**
 * Calculates the exact stabilized minimum Ada for a CEK material UTxO with
 * its actual script address and inline datum.
 */
export const minimumLovelaceForCekProgramMaterialPublicationV1 = ({
  contracts,
  publication,
  coinsPerUtxoByte,
}: {
  readonly contracts: Pick<MidgardValidators, "cekProgramMaterial">;
  readonly publication: CekProgramMaterialPublicationV1;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const address = CML.Address.from_bech32(
    contracts.cekProgramMaterial.spendingScriptAddress,
  );
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(publication.datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(lovelace),
        datum,
        undefined,
      ),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize CEK program-material publication min-Ada calculation.",
  );
};

/**
 * Calculates the exact stabilized minimum Ada for an immutable complete CEK
 * material datum at its actual reference-only script address.
 */
export const minimumLovelaceForCekSinglePublicationV1 = ({
  contracts,
  publication,
  coinsPerUtxoByte,
}: {
  readonly contracts: Pick<MidgardValidators, "cekProgramMaterial">;
  readonly publication: CekSinglePublicationV1;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const address = CML.Address.from_bech32(
    contracts.cekProgramMaterial.spendingScriptAddress,
  );
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(publication.datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(lovelace),
        datum,
        undefined,
      ),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize CEK single-publication min-Ada calculation.",
  );
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
      const protocolParameters = await resolveProtocolParameters(lucid);
      let tx = lucid.newTx();
      for (const publication of publications) {
        const minimumLovelace =
          minimumLovelaceForCekProgramMaterialPublicationV1({
            contracts,
            publication,
            coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
          });
        tx = tx.pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace:
              config.lovelacePerEntry === undefined
                ? minimumLovelace
                : config.lovelacePerEntry > minimumLovelace
                  ? config.lovelacePerEntry
                  : minimumLovelace,
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

/**
 * Publishes exactly one complete CEK graph as an immutable reference-only
 * inline datum. It has no spending path and therefore creates no mutable
 * state transition.
 */
export const buildUnsignedCekSinglePublicationV1Program = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekSinglePublicationV1Config,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const publication = deriveCekSinglePublicationV1(config);
      const protocolParameters = await resolveProtocolParameters(lucid);
      const minimumLovelace = minimumLovelaceForCekSinglePublicationV1({
        contracts,
        publication,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      return lucid
        .newTx()
        .pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace:
              config.lovelace === undefined
                ? minimumLovelace
                : config.lovelace > minimumLovelace
                  ? config.lovelace
                  : minimumLovelace,
          },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 complete CEK program material",
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
    const material = yield* Effect.try({
      try: () => {
        const derived = deriveTxOrderMaterialV1({
          nativeTxCbor,
          owner: Buffer.from(witnessScriptHash, "hex"),
        });
        if (derived.carriage.length > 0) {
          // Fail closed, and say why here rather than at submission. The tx-order
          // mint's `verify_order_material` admits only the canonically-empty
          // transaction, because the §8 availability re-expression it needs — a
          // §8.6 `FieldPreimageCertificateV1` per non-empty field, checked
          // through `authenticated_field_view` — cannot be wired yet: the
          // certificate validator is not in the frozen blueprint and has no
          // deployment role, and §8.4's ladder routes preimages under the tier-1
          // bound to redeemer carriage, which does not outlive the transaction
          // that carried it. Both are recorded as a Deviation on #587 and owned
          // by #589, which is where this refusal lifts. Building an order the
          // mint will refuse would only move the refusal somewhere less
          // informative.
          throw new Error(
            `forced order carries material in ${derived.carriage.length.toString()} field(s) ` +
              `(${derived.carriage
                .map((field) => field.fieldName)
                .join(", ")}); the §8.6-certified carriage the tx-order mint ` +
              "needs for non-empty material is not deployable yet (see #587's " +
              "Deviation and issue #589, which owns the blocker). Only a " +
              "transaction with nine empty fields can be ordered today.",
          );
        }
        return derived;
      },
      catch: (cause) =>
        new UserEventBuildError({
          message: "Failed to derive V1 tx-order material carriage",
          cause,
        }),
    });
    const txOrderDatum: TxOrderDatumV1 = {
      event: {
        id: txOrderId,
        tx: {
          tx_id: material.transactionId,
          transaction_commitment: material.transactionCommitment,
          source: material.source,
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
        ? [hubOracleRefInput]
        : [hubOracleRefInput, config.referenceScripts.txOrderMinting];
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

export const unsignedCekProgramMaterialV1 = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialV1Config,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekProgramMaterialV1Program(lucid, contracts, config),
  ).unsafeRun();

export const unsignedCekSinglePublicationV1 = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekSinglePublicationV1Config,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekSinglePublicationV1Program(lucid, contracts, config),
  ).unsafeRun();
