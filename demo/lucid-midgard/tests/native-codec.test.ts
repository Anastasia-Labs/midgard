import { describe, expect, it } from "vitest";
import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFull,
  encodeCbor,
  encodeMidgardNativeTxFull,
  materializeMidgardNativeTxFromCanonical,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
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
} from "../src/index.js";

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
  tx.compact.transactionBodyHash,
  tx.compact.transactionWitnessSetHash,
  0n,
];

const bodyFullValue = (tx: MidgardNativeTxFull): readonly unknown[] => [
  tx.body.spendInputsRoot,
  tx.body.spendInputsPreimageCbor,
  tx.body.referenceInputsRoot,
  tx.body.referenceInputsPreimageCbor,
  tx.body.outputsRoot,
  tx.body.outputsPreimageCbor,
  tx.body.fee,
  tx.body.validityIntervalStart,
  tx.body.validityIntervalEnd,
  tx.body.requiredObserversRoot,
  tx.body.requiredObserversPreimageCbor,
  tx.body.requiredSignersRoot,
  tx.body.requiredSignersPreimageCbor,
  tx.body.mintRoot,
  tx.body.mintPreimageCbor,
  tx.body.scriptIntegrityHash,
  tx.body.auxiliaryDataHash,
  tx.body.networkId,
];

describe("Midgard native v1 codec", () => {
  it("exposes shared script language view helpers", () => {
    const redeemerTxWitsRoot = Buffer.from(
      "509a422cbd3d2fdca7c6521277d3117b305aa7578bdcf1627df36382429743d1",
      "hex",
    );

    expect(ScriptLanguageTags.PlutusV3).toBe(2);
    expect(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW).toHaveLength(297);
    expect(
      computeScriptIntegrityHashForLanguages(redeemerTxWitsRoot, [
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("e2ebd40127c1f2fc48fc46388895edf309bdda534dfc1b1a1c0fceb94a43c60e");
  });

  it("round trips a canonical full transaction", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const encoded = encodeMidgardNativeTxFull(tx);
    const decoded = decodeMidgardNativeTxFull(encoded);

    expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
    expect(decoded.compact.transactionBodyHash).toEqual(
      tx.compact.transactionBodyHash,
    );
    expect(decoded.witnessSet.redeemerTxWitsRoot).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
  });

  it("uses the compact body hash as the transaction id", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());

    expect(computeMidgardNativeTxIdFromFull(tx)).toEqual(
      tx.compact.transactionBodyHash,
    );
  });

  it("rejects legacy four-bucket witness full tuples", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const legacyWitnessSet = [
      tx.witnessSet.addrTxWitsRoot,
      tx.witnessSet.addrTxWitsPreimageCbor,
      tx.witnessSet.scriptTxWitsRoot,
      tx.witnessSet.scriptTxWitsPreimageCbor,
      tx.witnessSet.redeemerTxWitsRoot,
      tx.witnessSet.redeemerTxWitsPreimageCbor,
      computeHash32(EMPTY_CBOR_LIST),
      EMPTY_CBOR_LIST,
    ];
    const encoded = encodeCbor([
      MIDGARD_NATIVE_TX_VERSION,
      compactValue(tx),
      bodyFullValue(tx),
      legacyWitnessSet,
    ]);

    expect(() => decodeMidgardNativeTxFull(encoded)).toThrow(
      /transaction_full\[3\] must have exactly 6 elements/,
    );
  });

  it("rejects root and preimage drift", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const tampered: MidgardNativeTxFull = {
      ...tx,
      body: {
        ...tx.body,
        outputsRoot: Buffer.alloc(32, 1),
      },
    };
    const encoded = encodeMidgardNativeTxFull(tampered, {
      enforceConsistency: false,
    });

    expect(() => decodeMidgardNativeTxFull(encoded)).toThrow(
      /outputs_root does not match payload hash/,
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
