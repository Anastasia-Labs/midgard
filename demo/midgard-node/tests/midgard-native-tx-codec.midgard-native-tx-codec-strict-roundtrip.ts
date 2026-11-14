import {
  computeHash32,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardFieldPreimageForField,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardVersionedScriptListPreimage,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  verifyMidgardNativeTxFullConsistency,
} from "@al-ft/midgard-core/codec";
import {
  deriveMidgardTxFieldPreimages,
  verifyMidgardTxFieldPreimage,
} from "@al-ft/midgard-core/consensus-validation";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

export const mkHash = (tag: string): Buffer =>
  computeHash32(Buffer.from(tag, "utf8"));

export const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encodeCbor(items.map((item) => Buffer.from(item))));

export const makePlutusIntegerData = (value: bigint): CML.PlutusData =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString(10)));

const mkBody = (): MidgardNativeTxBodyCanonical => {
  const spendInputsPreimageCbor = encodeByteList([
    Buffer.from([1]),
    Buffer.from([2]),
  ]);
  const referenceInputsPreimageCbor = encodeByteList([Buffer.from([3])]);
  const outputsPreimageCbor = encodeByteList([
    Buffer.from([4]),
    Buffer.from([5]),
    Buffer.from([6]),
  ]);
  const requiredObserversPreimageCbor = Buffer.from("80", "hex");
  const requiredSignersPreimageCbor = encodeByteList([
    Buffer.from([7]),
    Buffer.from([8]),
  ]);
  const mintPreimageCbor = Buffer.from("80", "hex");

  return {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: 42n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: 1_735_000_000_000n,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash: mkHash("script-integrity"),
    auxiliaryDataHash: mkHash("aux-data"),
    networkId: 1n,
  };
};

const mkWitnessSet = (): MidgardNativeTxWitnessSetCanonical => {
  const addrTxWitsPreimageCbor = Buffer.from("81420102", "hex");
  // §5.1: fields 6 and 8 wrap each item in a definite byte string like the other
  // seven. The retired counted scheme concatenated raw item CBOR here — an
  // array-of-arrays for the scripts, a bare uint list for the redeemers — and
  // both forms are now refused by the §5.1 gate, so the two fields are built
  // with the production encoders.
  const scriptTxWitsPreimageCbor = encodeMidgardVersionedScriptListPreimage([
    { language: "PlutusV3", scriptBytes: Buffer.from("deadbeef", "hex") },
  ]);
  const redeemerTxWitsPreimageCbor = encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: [
      {
        purpose: "Spend",
        index: 3n,
        redeemerCbor: Buffer.from("00", "hex"),
        executionUnits: { memory: 0n, steps: 0n },
      },
    ],
  });

  return {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };
};

const mkFull = (): MidgardNativeTxFull => {
  const body = mkBody();
  const witnessSet = mkWitnessSet();
  const compact = deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid");
  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    compact,
    body,
    witnessSet,
  };
};

describe("midgard native tx codec - strict roundtrip", () => {
  it("roundtrips compact tx/body/witness and full tx", () => {
    const full = mkFull();

    const bodyCompact = deriveMidgardNativeTxBodyCompact(full.body);
    const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(
      full.witnessSet,
    );

    expect(
      decodeMidgardNativeTxBodyCompact(
        encodeMidgardNativeTxBodyCompact(bodyCompact),
      ),
    ).toEqual(bodyCompact);
    expect(
      decodeMidgardNativeTxWitnessSetCompact(
        encodeMidgardNativeTxWitnessSetCompact(witnessCompact),
      ),
    ).toEqual(witnessCompact);
    expect(
      decodeMidgardNativeTxCompact(encodeMidgardNativeTxCompact(full.compact)),
    ).toEqual(full.compact);

    const encodedCanonical = encodeMidgardNativeTxCanonical(full);
    const decodedFull =
      decodeMidgardNativeTxFullFromCanonicalCbor(encodedCanonical);
    expect(decodedFull).toEqual(full);
  });

  it("uses the canonical V1 compact-body domain for the transaction id", () => {
    const full = mkFull();

    expect(computeMidgardNativeTxId(full)).toEqual(
      computeHash32(
        Buffer.concat([
          Buffer.from("MidgardNativeTxBodyV1", "ascii"),
          Buffer.from([1]),
          encodeMidgardNativeTxBodyCompact(full.compact.transactionBody),
        ]),
      ),
    );
  });

  it("rejects witness-set compact encodings with an extra datum witness bucket", () => {
    const witnessCompact =
      deriveMidgardNativeTxWitnessSetCompact(mkWitnessSet());
    const unsupportedShape = Buffer.from(
      encodeCbor([
        Buffer.from(witnessCompact.addrTxWitsHash),
        Buffer.from(witnessCompact.scriptTxWitsHash),
        Buffer.from(witnessCompact.redeemerTxWitsHash),
        Buffer.from(mkHash("extra-datum-wits")),
      ]),
    );

    expect(() =>
      decodeMidgardNativeTxWitnessSetCompact(unsupportedShape),
    ).toThrow(/exactly 3 elements/i);
  });

  it("keeps the witness tuple ABI while projecting scripts to field 6 and vkeys to field 7", () => {
    const full = mkFull();
    const canonicalCbor = encodeMidgardNativeTxCanonical(full);
    const fields = deriveMidgardTxFieldPreimages(canonicalCbor);
    const source =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
    const transactionCommitment = computeMidgardNativeTxProofCommitment(source);

    expect(fields[6]).toMatchObject({
      fieldIndex: 6,
      fieldName: "script_witnesses",
      preimageCbor: full.witnessSet.scriptTxWitsPreimageCbor,
    });
    expect(fields[7]).toMatchObject({
      fieldIndex: 7,
      fieldName: "address_witnesses",
      preimageCbor: full.witnessSet.addrTxWitsPreimageCbor,
    });

    const compactWitnessSet = deriveMidgardNativeTxWitnessSetCompact(
      full.witnessSet,
    );
    const encodedTuple = Buffer.from(
      encodeCbor([
        Buffer.from(compactWitnessSet.addrTxWitsHash),
        Buffer.from(compactWitnessSet.scriptTxWitsHash),
        Buffer.from(compactWitnessSet.redeemerTxWitsHash),
      ]),
    );
    expect(encodeMidgardNativeTxWitnessSetCompact(compactWitnessSet)).toEqual(
      encodedTuple,
    );

    for (const [fieldIndex, substitutedPreimage] of [
      [6, fields[7]!.preimageCbor],
      [7, fields[6]!.preimageCbor],
    ] as const) {
      expect(() =>
        verifyMidgardTxFieldPreimage({
          transactionId: computeMidgardNativeTxId(full),
          transactionCommitment,
          source,
          fieldIndex,
          preimageCbor: substitutedPreimage,
        }),
      ).toThrow(
        /(preimage (length does not match|hash mismatch)|must be a CBOR byte string)/u,
      );
    }
  });
});

describe("midgard native tx codec - consistency checks", () => {
  it("rejects inconsistent compact hash commitments", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      compact: {
        ...full.compact,
        transactionBody: {
          ...full.compact.transactionBody,
          outputsHash: Buffer.from(full.compact.transactionBody.outputsHash),
        },
      },
    };

    tampered.compact.transactionBody.outputsHash[0] ^= 0xff;

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow();
  });

  it("rejects inconsistent body hash/preimage pairs", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      body: {
        ...full.body,
        outputsPreimageCbor: Buffer.from(full.body.outputsPreimageCbor),
      },
    };

    tampered.body.outputsPreimageCbor[0] ^= 0xff;

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow();
  });

  it("accepts when explicit consistency verification passes", () => {
    const full = mkFull();
    expect(() => verifyMidgardNativeTxFullConsistency(full)).not.toThrow();
  });

  it("rejects mismatched outer and compact versions", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      compact: {
        ...full.compact,
        version: 23n,
      },
    };

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow(
      /transaction_full.version must match transaction_compact.version/i,
    );
  });
});
