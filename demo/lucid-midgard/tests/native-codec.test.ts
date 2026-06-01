import {
  computeHash32,
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxCanonical,
  decodeMidgardNativeTxFullFromCanonicalBinary,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ScriptLanguageTags,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

const makeCanonical = (): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputs: [],
    referenceInputs: [],
    outputs: [],
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObservers: [],
    requiredSigners: [],
    mint: new Map(),
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWits: [],
    scriptTxWits: [],
    redeemerTxWits: Buffer.alloc(0),
  },
});

describe("Midgard native v1 binary codec", () => {
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

  it("round trips canonical transaction binary into a materialized full transaction", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const encoded = encodeMidgardNativeTxCanonical(tx);
    const decoded = decodeMidgardNativeTxFullFromCanonicalBinary(encoded);

    expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
    expect(decoded.compact.transactionBody).toEqual(tx.compact.transactionBody);
    expect(decoded.compact.transactionWitnessSetHash).toEqual(
      tx.compact.transactionWitnessSetHash,
    );
  });

  it("decodes canonical transaction binary without deriving compact fields", () => {
    const canonical = makeCanonical();
    const decoded = decodeMidgardNativeTxCanonical(
      encodeMidgardNativeTxCanonical(canonical),
    );

    expect(decoded).toEqual(canonical);
    expect("compact" in decoded).toBe(false);
  });

  it("uses the compact body hash as the transaction id", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());

    expect(computeMidgardNativeTxId(tx)).toEqual(
      computeHash32(
        encodeMidgardNativeTxBodyCompact(tx.compact.transactionBody),
      ),
    );
  });

  it("rejects trailing bytes after canonical envelope", () => {
    const encoded = encodeMidgardNativeTxCanonical(
      materializeMidgardNativeTxFromCanonical(makeCanonical()),
    );

    expect(() =>
      decodeMidgardNativeTxFullFromCanonicalBinary(
        Buffer.concat([encoded, Buffer.alloc(8)]),
      ),
    ).toThrow(/Trailing bytes after transaction/);
  });
});
