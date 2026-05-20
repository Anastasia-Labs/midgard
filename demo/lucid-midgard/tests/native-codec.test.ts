import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFull,
  encodeCbor,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxFull,
  materializeMidgardNativeTxFromCanonical,
  computeHash32,
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ScriptLanguageTags,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

const makeCanonical = (): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
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
});

const compactValue = (tx: MidgardNativeTxFull): readonly unknown[] => [
  tx.compact.version,
  computeMidgardNativeTxId(tx),
  tx.compact.transactionWitnessSetHash,
  0n,
];

const bodyFullValue = (tx: MidgardNativeTxFull): readonly unknown[] => [
  tx.compact.transactionBody.spendInputsHash,
  tx.body.spendInputsPreimageCbor,
  tx.compact.transactionBody.referenceInputsHash,
  tx.body.referenceInputsPreimageCbor,
  tx.compact.transactionBody.outputsHash,
  tx.body.outputsPreimageCbor,
  tx.body.fee,
  tx.body.validityIntervalStart,
  tx.body.validityIntervalEnd,
  tx.compact.transactionBody.requiredObserversHash,
  tx.body.requiredObserversPreimageCbor,
  tx.compact.transactionBody.requiredSignersHash,
  tx.body.requiredSignersPreimageCbor,
  tx.compact.transactionBody.mintHash,
  tx.body.mintPreimageCbor,
  tx.body.scriptIntegrityHash,
  tx.body.auxiliaryDataHash,
  tx.body.networkId,
];

describe("Midgard native v1 codec", () => {
  it("exposes shared script language view helpers", () => {
    const redeemerTxWitsHash = Buffer.from(
      "509a422cbd3d2fdca7c6521277d3117b305aa7578bdcf1627df36382429743d1",
      "hex",
    );

    expect(ScriptLanguageTags.PlutusV3).toBe(2);
    expect(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW).toHaveLength(297);
    expect(
      computeScriptIntegrityHashForLanguages(redeemerTxWitsHash, [
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("e2ebd40127c1f2fc48fc46388895edf309bdda534dfc1b1a1c0fceb94a43c60e");
  });

  it("round trips a canonical full transaction", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const encoded = encodeMidgardNativeTxFull(tx);
    const decoded = decodeMidgardNativeTxFull(encoded);

    expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
    expect(decoded.compact.transactionBody).toEqual(tx.compact.transactionBody);
    expect(decoded.compact.transactionWitnessSetHash).toEqual(
      tx.compact.transactionWitnessSetHash,
    );
    expect(decoded.compact.transactionBody.mintHash).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
  });

  it("uses the compact body hash as the transaction id", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());

    expect(computeMidgardNativeTxId(tx)).toEqual(
      computeHash32(
        encodeMidgardNativeTxBodyCompact(tx.compact.transactionBody),
      ),
    );
  });

  it("rejects the old embedded-compact full transaction envelope", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const legacyWitnessSet = [
      tx.compact.transactionWitnessSetHash,
      tx.witnessSet.addrTxWitsPreimageCbor,
      tx.compact.transactionWitnessSetHash,
      tx.witnessSet.scriptTxWitsPreimageCbor,
      tx.compact.transactionWitnessSetHash,
      tx.witnessSet.redeemerTxWitsPreimageCbor,
    ];
    const encoded = encodeCbor([
      MIDGARD_NATIVE_TX_VERSION,
      compactValue(tx),
      bodyFullValue(tx),
      legacyWitnessSet,
    ]);

    expect(() => decodeMidgardNativeTxFull(encoded)).toThrow(
      /transaction\[1\] must have exactly 12 elements/,
    );
  });

  it("rejects derived compact body drift", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const tampered: MidgardNativeTxFull = {
      ...tx,
      compact: {
        ...tx.compact,
        transactionBody: {
          ...tx.compact.transactionBody,
          outputsHash: Buffer.alloc(32, 1),
        },
      },
    };

    expect(() => encodeMidgardNativeTxFull(tampered)).toThrow(
      /transaction_compact\.transaction_body must match the derived compact body/,
    );
  });

  it("rejects trailing CBOR bytes", () => {
    const encoded = encodeMidgardNativeTxFull(
      materializeMidgardNativeTxFromCanonical(makeCanonical()),
    );

    expect(() =>
      decodeMidgardNativeTxFull(Buffer.concat([encoded, EMPTY_CBOR_NULL])),
    ).toThrow(/cbor has trailing bytes/);
  });
});
