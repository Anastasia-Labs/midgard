import { encodeMidgardSpendInputItemV1 } from "@al-ft/midgard-core/codec";
import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ensureSpendInputsReferenceWitness,
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
  spendInputsWitnessFromCbors,
} from "../src/spend-input-witness.js";

// Hold the emulator to the literal 16,384-byte L1 envelope. A relaxed
// `maxTxSize` here would be load-bearing (Lucid caches the provider's protocol
// parameters at construction and CML rejects the build with "Max transaction
// size of N exceeded"), so raising it would silently let this publication grow
// past what any Cardano node would accept. Measured today: the 180-input
// witness publication is 7,518 bytes, i.e. 8,866 bytes of margin.
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  maxCollateralInputs: 3,
} as const;

const inputCbor = (index: number): string =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(index.toString(16).padStart(64, "0"), "hex"),
    outputIndex: index,
  }).toString("hex");

describe("spend-input reference witnesses", () => {
  it("publishes high-cardinality witnesses with calculated min ADA", async () => {
    const prover = generateEmulatorAccount({ lovelace: 30_000_000_000n });
    const emulator = new Emulator([prover], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(prover.seedPhrase);
    const address = await lucid.wallet().address();
    const paymentCredential = getAddressDetails(address).paymentCredential;
    if (paymentCredential === undefined || paymentCredential.type !== "Key") {
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
  }, 30_000);
});
