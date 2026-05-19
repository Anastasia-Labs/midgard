import { describe, expect, it } from "vitest";
import {
  CML,
  Emulator,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  generateEmulatorAccount,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import {
  ensureSpendInputsReferenceWitness,
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
  spendInputsWitnessFromCbors,
} from "../src/spend-input-witness.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

const inputCbor = (index: number): string =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(index.toString(16).padStart(64, "0")),
      BigInt(index),
    ).to_cbor_bytes(),
  ).toString("hex");

describe("spend-input reference witnesses", () => {
  it(
    "publishes high-cardinality witnesses with calculated min ADA",
    async () => {
      const prover = generateEmulatorAccount({ lovelace: 30_000_000_000n });
      const emulator = new Emulator(
        [prover],
        EMULATOR_PROTOCOL_PARAMETERS,
      );
      const lucid = await Lucid(emulator, "Custom");
      lucid.selectWallet.fromSeed(prover.seedPhrase);
      const address = await lucid.wallet().address();
      const paymentCredential = getAddressDetails(address).paymentCredential;
      if (
        paymentCredential === undefined ||
        paymentCredential.type !== "Key"
      ) {
        throw new Error("Expected emulator wallet to expose a payment key hash");
      }

      const witness = spendInputsWitnessFromCbors(
        Array.from({ length: 180 }, (_, index) => inputCbor(index + 1)),
        "test.inputs",
      );
      const protocolParameters = await resolveProtocolParameters(lucid);
      const expectedLovelace = minimumLovelaceForInlineDatumOutput({
        address,
        datum: witness.datum,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      expect(expectedLovelace).toBeGreaterThan(5_000_000n);

      const result = await ensureSpendInputsReferenceWitness({
        lucid,
        address,
        paymentKeyHash: paymentCredential.hash,
        witness,
        awaitConfirmation: true,
      });

      expect(result.created).toBe(true);
      expect(result.lovelace).toBe(expectedLovelace);
      expect(result.utxo.assets.lovelace).toBe(expectedLovelace);
      expect(result.utxo.datum).toBe(witness.datum);
    },
    30_000,
  );
});
