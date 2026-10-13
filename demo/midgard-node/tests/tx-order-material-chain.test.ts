import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import type { MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  carriageFieldPreimages,
  reconstructTxOrderMaterial,
} from "../src/forced-orders/index.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

/** The mint admits only a transaction that spends something: field 0. */
const SPEND_INPUTS = encodeCbor([makeOutRefCbor(0x33)]);

const transactionCbor = ({
  addrTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  outputFills = [0x11, 0x22],
}: {
  readonly addrTxWitsPreimageCbor?: Buffer;
  readonly scriptTxWitsPreimageCbor?: Buffer;
  /**
   * One 5 kB-datum output per fill byte. Two puts field 2 under §8.3's `K` and
   * therefore in tier 1/2 territory; four puts it above `K`, where §8.4's
   * partition makes tier 3 the *only* admissible carriage.
   */
  readonly outputFills?: readonly number[];
} = {}): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: SPEND_INPUTS,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          outputFills.map((fill) =>
            encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x60]),
                Buffer.alloc(28, fill),
              ]),
              value: { lovelace: 2_000_000n, assets: new Map() },
              datum: {
                kind: "inline",
                cbor: Buffer.from(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    encodeCbor(Buffer.alloc(5_000, fill)).toString("hex"),
                  ),
                  "hex",
                ),
              },
            }),
          ),
        ),
        fee: 0n,
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
        addrTxWitsPreimageCbor,
        scriptTxWitsPreimageCbor,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

const spendOnlyTransactionCbor = (): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: SPEND_INPUTS,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        fee: 0n,
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
    }),
  );

const payloadFor = (submittedTxCbor: Buffer): SDK.TxOrderPayload => {
  const material = SDK.deriveTxOrderMaterial({
    submittedTxCbor,
    owner: Buffer.alloc(28, 0x66),
  });
  return {
    tx_id: material.transactionId,
    transaction_commitment: material.transactionCommitment,
    submitted_source: material.submitted_source,
  };
};

/**
 * Rebuilds an order's transaction the way ingestion does: the carriage
 * yields the nine field preimages, each hashed against the payload's own
 * commitment, and the preimages rebuild the transaction. Reference input
 * `i` holds `referenceDatums[i]` (raw carriage as its publication datum).
 */
const reconstruct = (
  payload: SDK.TxOrderPayload,
  carriage: readonly MidgardFieldCarriage[] = [],
  referenceDatums: readonly (Uint8Array | null)[] = [],
): Buffer =>
  reconstructTxOrderMaterial({
    payload,
    fieldPreimages: carriageFieldPreimages({
      payload,
      carriage,
      referenceInputs: referenceDatums.map((_datum, index) => ({
        txHash: Buffer.alloc(32, index + 1),
        index,
      })),
      datumOf: (outRef) => {
        const bytes = referenceDatums[outRef.index];
        return bytes == null
          ? null
          : Buffer.from(SDK.fieldPreimagePublicationDatumCbor(bytes), "hex");
      },
    }),
  });

/**
 * The mint redeemer's carriage vector for an order, read back the way ingestion
 * receives it: positional over the non-empty fields in ascending field index.
 *
 * Built from `deriveTxOrderMaterial`'s own per-field plans rather than from a
 * hand-listed set of field indices, so the vector cannot drift out of the order
 * the on-chain walk reads the nine commitments in.
 */
const inlineCarriageFor = (
  material: SDK.TxOrderMaterial,
): readonly MidgardFieldCarriage[] =>
  material.carriage.map((field) => ({
    carriage: "Inline",
    preimage: field.preimage,
  }));

const FIELD_HASH_REFUSAL =
  /field preimage does not match the committed field hash/u;

describe("V1 tx-order §8 field carriage", () => {
  it("reconstructs an order whose only non-empty field is its spend inputs", () => {
    const submittedTxCbor = spendOnlyTransactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });

    // Eight empty fields carry nothing (§7.1: an untouched field is never
    // authenticated), so the vector has the one spend-input entry.
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([0]);
    expect(
      reconstruct(payloadFor(submittedTxCbor), inlineCarriageFor(material)),
    ).toEqual(submittedTxCbor);
  });

  it("reassembles a material-bearing forced order from inline carriage", () => {
    // `transactionCbor()` puts one spend input in field 0 and two 5 kB-datum
    // outputs in field 2: each preimage authenticated once against the flat
    // commitment the payload's compact body carries.
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([0, 2]);
    expect(
      reconstruct(payloadFor(submittedTxCbor), inlineCarriageFor(material)),
    ).toEqual(submittedTxCbor);
  });

  it("reassembles the same order from tier-2 predeployed carriage", () => {
    // The tier is an encoding detail: the same bytes reached from a reference
    // input's nothing-but-bytes inline datum authenticate identically (§8.11).
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    const [spends, outputs] = material.carriage;
    expect(outputs).toBeDefined();
    expect(
      reconstruct(
        payloadFor(submittedTxCbor),
        [
          { carriage: "Inline", preimage: spends!.preimage },
          { carriage: "RawUtxo", refInputIndex: 1 },
        ],
        [Buffer.from("deadbeef", "hex"), outputs!.preimage],
      ),
    ).toEqual(submittedTxCbor);
  });

  it("refuses a tier-2 reference input that is not raw carriage", () => {
    const submittedTxCbor = transactionCbor();
    expect(() =>
      carriageFieldPreimages({
        payload: payloadFor(submittedTxCbor),
        carriage: [{ carriage: "RawUtxo", refInputIndex: 0 }],
        referenceInputs: [{ txHash: Buffer.alloc(32, 1), index: 0 }],
        datumOf: () => Buffer.from("d87980", "hex"),
      }),
    ).toThrow(/nothing-but-bytes/u);
  });

  it("reassembles the same order from tier-3 certified carriage", () => {
    // The only carriage whose bytes arrive split. Four 5 kB-datum outputs put
    // field 2 above §8.3's `K`, so §8.4's partition leaves tier 3 as the only
    // admissible carriage. The node never reads the certificate: the chunks'
    // concatenation is hashed whole against the field's commitment.
    const submittedTxCbor = transactionCbor({
      outputFills: [0x11, 0x22, 0x33, 0x44],
    });
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([0, 2]);
    const [spends, field] = material.carriage;
    expect(field!.plan.tier).toBe("Certified");
    expect(field!.plan.publications.length).toBeGreaterThan(1);

    const chunks = field!.plan.publications.map(
      (publication) => publication.bytes,
    );
    const certifiedCarriage: readonly MidgardFieldCarriage[] = [
      { carriage: "Inline", preimage: spends!.preimage },
      {
        carriage: "Certified",
        certRefInputIndex: 0,
        chunkRefInputIndices: chunks.map((_chunk, index) => index + 1),
      },
    ];
    expect(
      reconstruct(payloadFor(submittedTxCbor), certifiedCarriage, [
        null,
        ...chunks,
      ]),
    ).toEqual(submittedTxCbor);

    // Tier 3's wrong-bytes refusal: only the last chunk's content differs, at
    // the same length, so the whole-field commitment is what refuses it.
    const lastChunk = Buffer.from(chunks[chunks.length - 1]!);
    lastChunk[lastChunk.length - 1] ^= 0xff;
    expect(() =>
      reconstruct(payloadFor(submittedTxCbor), certifiedCarriage, [
        null,
        ...chunks.slice(0, -1),
        lastChunk,
      ]),
    ).toThrow(FIELD_HASH_REFUSAL);
  });

  it("fails closed on a material-bearing forced order with no carriage supplied", () => {
    expect(() => reconstruct(payloadFor(transactionCbor()))).toThrow(
      /no §8 carriage for it/u,
    );
  });

  it("fails closed on carriage whose bytes are not the committed preimage", () => {
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    const [field] = material.carriage;
    expect(field).toBeDefined();
    // One byte of the §5.1 payload changed, the envelope and the length intact —
    // so only the flat §4 commitment refuses it.
    const corrupted = Buffer.from(field!.preimage);
    corrupted[corrupted.length - 1] ^= 0xff;
    expect(() =>
      reconstruct(payloadFor(submittedTxCbor), [
        { carriage: "Inline", preimage: corrupted },
      ]),
    ).toThrow(FIELD_HASH_REFUSAL);
  });

  it("fails closed on a carriage vector the nine commitments do not exhaust", () => {
    // The mint's exhaustion rule, re-derived: a spare entry means the vector being
    // read is not the one the mint authenticated.
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(() =>
      reconstruct(payloadFor(submittedTxCbor), [
        ...inlineCarriageFor(material),
        { carriage: "Inline", preimage: Buffer.from("80", "hex") },
      ]),
    ).toThrow(/carriage entries more than its commitments name/u);
  });

  it("fails closed when the committed field lengths do not describe the source", () => {
    const payload = payloadFor(spendOnlyTransactionCbor());
    const lengths = Buffer.from(
      payload.submitted_source.field_preimage_lengths_cbor,
      "hex",
    );
    // The eight empty fields encode as one-byte `01` lengths; claiming two bytes
    // for the first of them still passes the header check and is wrong against
    // the source.
    const index = lengths.indexOf(0x01);
    expect(index).toBeGreaterThan(0);
    const mutated = Buffer.from(lengths);
    mutated[index] = 0x02;
    expect(() =>
      reconstruct({
        ...payload,
        submitted_source: {
          ...payload.submitted_source,
          field_preimage_lengths_cbor: mutated.toString("hex"),
        },
      }),
    ).toThrow();
  });

  it("fails closed when the payload's transaction id is not the source's", () => {
    const payload = payloadFor(spendOnlyTransactionCbor());
    expect(() => reconstruct({ ...payload, tx_id: "11".repeat(32) })).toThrow();
  });

  it("fails closed when the payload's commitment is not the source's", () => {
    const payload = payloadFor(spendOnlyTransactionCbor());
    expect(() =>
      reconstruct({ ...payload, transaction_commitment: "22".repeat(32) }),
    ).toThrow();
  });
});
