/** The removal builder's record lookup by the derived out-ref: it cites only
 * a record at the fraud-proof address holding exactly one of the header's
 * unit, with a datum that decodes. Anything else at that out-ref is refused,
 * so a wrong out-ref can never become the removal's cited record. */
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { requireFraudProofRecord } from "../src/fraud-proof-record-selection.js";

const ADDRESS = "addr_test1wrecord";
const UNIT = `${"a1".repeat(28)}${"b2".repeat(4)}${"c3".repeat(28)}`;
const PROVER = "d4".repeat(28);
const OUT_REF = `${"e5".repeat(32)}#0`;

const record = (overrides: Partial<UTxO> = {}): UTxO => ({
  txHash: "e5".repeat(32),
  outputIndex: 0,
  address: ADDRESS,
  assets: { lovelace: 2_000_000n, [UNIT]: 1n },
  datum: Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum),
  ...overrides,
});

/** A provider that holds `utxo` at `OUT_REF`. */
const lucidHolding = (utxo: UTxO) =>
  ({
    utxosByOutRef: async () => [utxo],
    utxosAtWithUnit: async () => [utxo],
  }) as unknown as LucidEvolution;

const lookUp = (utxo: UTxO) =>
  requireFraudProofRecord({
    lucid: lucidHolding(utxo),
    address: ADDRESS,
    unit: UNIT,
    outRef: OUT_REF,
    prover: PROVER,
  });

describe("the removal's record by its derived out-ref", () => {
  it("cites the record at the fraud-proof address holding one of the unit", async () => {
    const utxo = record();
    await expect(lookUp(utxo)).resolves.toEqual({ utxo, prover: PROVER });
  });

  it.each([
    ["at another address", record({ address: "addr_test1wother" })],
    ["holding none of the unit", record({ assets: { lovelace: 2_000_000n } })],
    [
      "holding two of the unit",
      record({ assets: { lovelace: 2_000_000n, [UNIT]: 2n } }),
    ],
    ["with a datum that does not decode", record({ datum: "d87980" })],
  ])("refuses an out-ref %s", async (_name, utxo) => {
    await expect(lookUp(utxo)).rejects.toThrow(
      `${OUT_REF} is not a fraud-proof record of ${UNIT} with a decodable datum`,
    );
  });
});
