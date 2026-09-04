import {
  computeHash32,
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxCanonical,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ScriptLanguageTags,
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

  it("round trips canonical transaction CBOR into a materialized full transaction", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const encoded = encodeMidgardNativeTxCanonical(tx);
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(encoded);

    expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
    expect(decoded.compact.transactionBody).toEqual(tx.compact.transactionBody);
    expect(decoded.compact.transactionWitnessSetHash).toEqual(
      tx.compact.transactionWitnessSetHash,
    );
    // §4/§5.6: an empty mint is exactly `80`, and its commitment is the plain
    // `blake2b_256` of those bytes — the same value every empty field commits to,
    // because §4's hash input carries no field index. Under the retired counted
    // scheme this was a domain-tagged Merkle root and deliberately *not* the hash
    // of `80`; that inequality is now an equality.
    expect(decoded.compact.transactionBody.mintHash).toEqual(
      MIDGARD_EMPTY_FIELD_COMMITMENT,
    );
    expect(decoded.compact.transactionBody.mintHash).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
  });

  it("decodes canonical transaction CBOR without deriving compact fields", () => {
    const canonical = makeCanonical();
    const decoded = decodeMidgardNativeTxCanonical(
      encodeMidgardNativeTxCanonical(canonical),
    );

    expect(decoded).toEqual(canonical);
    expect("compact" in decoded).toBe(false);
  });

  it("uses the canonical V1 compact-body domain as the transaction id", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());

    expect(computeMidgardNativeTxId(tx)).toEqual(
      computeHash32(
        Buffer.concat([
          Buffer.from("MidgardNativeTxBodyV1", "ascii"),
          Buffer.from([1]),
          encodeMidgardNativeTxBodyCompact(tx.compact.transactionBody),
        ]),
      ),
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

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow(
      /transaction_compact\.transaction_body must match the derived compact body/,
    );
  });

  it("rejects trailing CBOR bytes", () => {
    const encoded = encodeMidgardNativeTxCanonical(
      materializeMidgardNativeTxFromCanonical(makeCanonical()),
    );

    expect(() =>
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.concat([encoded, EMPTY_CBOR_NULL]),
      ),
    ).toThrow(/cbor has trailing bytes/);
  });
});
