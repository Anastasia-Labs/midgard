import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import {
  MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
  type MidgardFieldCarriagePlan,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldCommitment,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  buildUnsignedFieldPreimagePublicationProgram,
  fieldPreimagePublicationDatumCbor,
  type FieldPreimagePublicationOutput,
  fieldPreimagePublicationOutputs,
  minimumLovelaceForFieldPreimagePublication,
} from "@al-ft/midgard-sdk/state-queue";
import {
  CML,
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Carriage measurements and the §8.7 healing path, against a real
 * emulator ledger — `docs/spec/midgard-tx.md` §8.
 *
 * Three things happen here, and only the first is a test in the ordinary sense.
 *
 *   1. **The ladder is exercised end to end.** A preimage of each tier is
 *      planned, published where publication is required, resolved back out of
 *      the ledger, and read through the single field-access door by *one*
 *      consumer function that never mentions a tier.
 *   2. **The §8.7 healing path is driven.** A published chunk is clobbered — its
 *      UTxO spent out from under the dispute — and re-published by a second
 *      identity with no relationship to the first. The original certificate
 *      still describes the healed bytes, and the read that broke succeeds again.
 *   3. **The tier-2 byte frontier is measured**: the largest preimage whose
 *      real signed key-address publication clears `maxTxSize`, and the
 *      largest that clears it with the 512-byte reliability reserve, which is
 *      §8.3's `K`.
 *
 * **One emulator, at the real limit.**
 * `setupEmulator` runs at the **real** `maxTxSize` (16,384) by default, so
 * anything that submits a publication is judged by the ledger itself and not by
 * an assertion written to pass. **Exactly one block raises it**: the
 * frontier sweep, which has to build transactions *past* the limit in order to
 * find where the limit is. Every other block — the tier ladder, the tier-3
 * corner healing, the min-Ada measurements, the certification round trip —
 * publishes full-`K` chunks through the real ledger (§8.3). Every
 * verdict is taken against `MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes`,
 * never against whatever the emulator was willing to accept.
 *
 * **The certificate is minted, under a stand-in policy.** The certificate
 * validator's *content rules* are proved on the Aiken side
 * (`validators/field-preimage-certificate-handlers.test.ak`), not here. What is
 * proved here is everything the script is not: the certification transaction
 * really mints the certificate token, really lands the manifest as an inline
 * datum at a script address, and
 * a consuming step really resolves that certificate **out of the ledger** by
 * decoding the datum and matching the token, rather than reading the plan it
 * was built from. The stand-in is an always-satisfiable native script, and the
 * one thing it does not check is the one thing the Aiken suite does. Where a
 * claim depends on the compiled script, it is stated as depending on it.
 */

// The 512-byte transaction-side allowance that separates an
// "exact" frontier (lands on maxTxSize) from a "reliable" one (lands on
// maxTxSize with room for the variability a real submission path introduces).
// Named by the codec rather than restated here, so the frontier this file
// measures and the frontier the SDK guard enforces are the same reserve.
export const RELIABILITY_RESERVE_BYTES =
  MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES;

export const MAX_L1_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/** Field 1 (reference inputs): §5.3 stride 40, 38-byte items. */
export const FIELD_INDEX = 1;

export const STRIDE = 40;

const ITEM_BYTES = 38;

export const PUBLISHER_KEY = CML.PrivateKey.from_normal_bytes(
  Buffer.alloc(32, 11),
);

export const HEALER_KEY = CML.PrivateKey.from_normal_bytes(
  Buffer.alloc(32, 22),
);

export const keyHash = (key: CML.PrivateKey): Buffer =>
  Buffer.from(key.to_public().hash().to_raw_bytes());

const enterpriseAddress = (key: CML.PrivateKey): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  )
    .to_address()
    .to_bech32();

export const PUBLISHER_ADDRESS = enterpriseAddress(PUBLISHER_KEY);

export const HEALER_ADDRESS = enterpriseAddress(HEALER_KEY);

export const TX_ID = Buffer.alloc(32, 0x5a);

/**
 * A §5.1 preimage of `itemCount` well-formed field-1 items. Item payloads are an
 * affine byte pattern keyed by index, so two items are never equal and a read
 * that lands one item off returns bytes that do not match anything.
 */
export const fieldPreimage = (itemCount: number): Buffer => {
  const header =
    itemCount <= 23
      ? Buffer.from([0x80 + itemCount])
      : itemCount <= 255
        ? Buffer.from([0x98, itemCount])
        : Buffer.from([0x99, itemCount >> 8, itemCount & 0xff]);
  const items = Array.from({ length: itemCount }, (_unused, index) => {
    const payload = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x20]),
      Buffer.from(
        Array.from(
          { length: 32 },
          (_byte, offset) => (index * 7 + offset) & 0xff,
        ),
      ),
      Buffer.from([0x19, (index >> 8) & 0xff, index & 0xff]),
    ]);
    if (payload.length !== ITEM_BYTES) {
      throw new Error("field-1 item must be 38 bytes");
    }
    return Buffer.concat([Buffer.from([0x58, ITEM_BYTES]), payload]);
  });
  return Buffer.concat([header, ...items]);
};

/** The item count whose §5.1 preimage is the largest at or under `bytes`. */
export const itemCountForPreimageBytes = (bytes: number): number => {
  for (let count = Math.floor(bytes / STRIDE) + 1; count >= 0; count -= 1) {
    if (fieldPreimage(count).length <= bytes) {
      return count;
    }
  }
  throw new Error("no item count fits");
};

export const itemPayloadAt = (preimage: Buffer, index: number): Buffer => {
  const headerLength = preimage[0] === 0x99 ? 3 : preimage[0] === 0x98 ? 2 : 1;
  const offset = headerLength + STRIDE * index + 2;
  return preimage.subarray(offset, offset + ITEM_BYTES);
};

export type Harness = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
};

/**
 * The `maxTxSize` the frontier sweep asks for, because it builds transactions
 * the real ledger would refuse.
 */
export const MEASUREMENT_MAX_TX_BYTES = 65_536;

/**
 * The publication-size budget the frontier sweep hands the SDK builder. The
 * builder refuses anything over the §8.3 frontier by default; a measurement is the one caller entitled to raise it, and
 * it does so explicitly rather than by calling something else.
 */
export const MEASUREMENT_PUBLICATION_BUDGET_BYTES =
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;

/**
 * `maxTxSize` defaults to the **real** deployment floor. A block that wants
 * more has to say so and say why; that is what stops a gate that cannot fail
 * from being written by accident.
 */
export const setupEmulator = async ({
  maxTxSize = MAX_L1_TX_BYTES,
}: { readonly maxTxSize?: number } = {}): Promise<Harness> => {
  const emulator = new Emulator(
    [
      {
        seedPhrase: "",
        privateKey: PUBLISHER_KEY.to_bech32(),
        address: PUBLISHER_ADDRESS,
        assets: { lovelace: 500_000_000_000n },
      },
      {
        seedPhrase: "",
        privateKey: HEALER_KEY.to_bech32(),
        address: HEALER_ADDRESS,
        assets: { lovelace: 500_000_000_000n },
      },
    ],
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxTxSize },
  );
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromPrivateKey(PUBLISHER_KEY.to_bech32());
  return { lucid, emulator };
};

/**
 * The publication output a plan requires for one chunk — the whole output, not
 * just its datum, which is why it is not called `publicationDatum`.
 */
export const publicationOutputFor = (
  plan: MidgardFieldCarriagePlan,
  chunkIndex: number,
): FieldPreimagePublicationOutput => {
  const output = fieldPreimagePublicationOutputs(plan).find(
    (candidate) => candidate.chunkIndex === chunkIndex,
  );
  if (output === undefined) {
    throw new Error(`plan has no chunk ${chunkIndex.toString()}`);
  }
  return output;
};

/**
 * A raw §8.5 publication output for arbitrary payload bytes, used only by the
 * byte-frontier sweep. It bypasses the plan on purpose: the frontier question
 * is what a nothing-but-bytes publication transaction costs at a given payload
 * size, and a plan below the tier-1 bound would answer "nothing to publish"
 * rather than answering the question.
 */
export const rawPublicationOutput = (
  bytes: Buffer,
): FieldPreimagePublicationOutput => ({
  chunkIndex: 0,
  datumCbor: fieldPreimagePublicationDatumCbor(bytes),
  byteLength: bytes.length,
  digestHex: midgardFieldCommitment(bytes).toString("hex"),
});

type PublicationResult = {
  readonly utxo: UTxO;
  readonly signedBytes: number;
  readonly minAdaLovelace: bigint;
  readonly feeLovelace: bigint;
};

/**
 * Builds, signs and submits one raw carriage publication, and reports what it
 * cost.
 *
 * The builder under test really is the SDK's —
 * {@link buildUnsignedFieldPreimagePublicationProgram} — and nothing here
 * reimplements it, so its §8.3 size guard is exercised here.
 *
 * `maxPublicationBytes` is threaded through rather than defaulted away, so a
 * block that intends to build past the frontier says so.
 */
export const publish = async ({
  harness,
  publication,
  signerKey,
  address,
  submit = true,
  maxPublicationBytes,
}: {
  readonly harness: Harness;
  readonly publication: FieldPreimagePublicationOutput;
  readonly signerKey: CML.PrivateKey;
  readonly address: string;
  readonly submit?: boolean;
  readonly maxPublicationBytes?: number;
}): Promise<PublicationResult> => {
  harness.lucid.selectWallet.fromPrivateKey(signerKey.to_bech32());
  const protocolParameters = harness.lucid.config().protocolParameters;
  if (protocolParameters === undefined) {
    throw new Error("emulator protocol parameters are unavailable");
  }
  const minAdaLovelace = minimumLovelaceForFieldPreimagePublication({
    publisherAddress: address,
    output: publication,
    coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
  });
  const unsigned = await Effect.runPromise(
    buildUnsignedFieldPreimagePublicationProgram(harness.lucid, {
      publication,
      publisherAddress: address,
      ...(maxPublicationBytes === undefined ? {} : { maxPublicationBytes }),
    }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const signedBytes = signedCbor.length / 2;
  const feeLovelace = CML.Transaction.from_cbor_hex(signedCbor).body().fee();
  if (!submit) {
    return {
      utxo: {
        txHash: "",
        outputIndex: 0,
        address,
        assets: { lovelace: minAdaLovelace },
        datum: publication.datumCbor,
      },
      signedBytes,
      minAdaLovelace,
      feeLovelace,
    };
  }
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const published = (await harness.lucid.utxosAt(address)).find(
    (utxo) => utxo.txHash === txHash && utxo.datum === publication.datumCbor,
  );
  if (published === undefined) {
    throw new Error("published carriage UTxO was not found");
  }
  return { utxo: published, signedBytes, minAdaLovelace, feeLovelace };
};
