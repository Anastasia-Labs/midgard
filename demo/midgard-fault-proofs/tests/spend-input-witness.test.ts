/**
 * What survives of the spend-input witness module after #604.
 *
 * The test this replaces published a 180-input typed witness UTxO through
 * `ensureSpendInputsReferenceWitness` and pinned its min-Ada. That publication
 * is deleted: §8's carriage ladder replaced it with the §8.5 nothing-but-bytes
 * publication that `field-opening.ts` builds, and the redeemer indices it fed
 * (`tx1_spend_inputs_ref_input_index` and its twin) no longer exist on-chain.
 *
 * The min-Ada calculation itself is still live — `publish-proof-chunks.ts` uses
 * it — and so is the canonical witness decoding, so both stay pinned here. The
 * §8 publication route has its own coverage in
 * `tests/submit-input-no-idx-step-02.test.ts` and the input-no-idx emulator leg.
 */
import {
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
  spendInputsWitnessFromCbors,
} from "../src/spend-input-witness.js";

// Hold the emulator to the literal 16,384-byte L1 envelope, for the same reason
// the retired test did: Lucid caches the provider's protocol parameters at
// construction, so a relaxed `maxTxSize` here would be load-bearing.
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  maxCollateralInputs: 3,
} as const;

const inputCbor = (index: number): string =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(index.toString(16).padStart(64, "0"), "hex"),
    outputIndex: index,
  }).toString("hex");

describe("spend-input witness helpers", () => {
  it("decodes a high-cardinality witness and prices its inline datum output", async () => {
    const prover = generateEmulatorAccount({ lovelace: 30_000_000_000n });
    const emulator = new Emulator([prover], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(prover.seedPhrase);
    const address = await lucid.wallet().address();

    const witness = spendInputsWitnessFromCbors(
      Array.from({ length: 180 }, (_, index) => inputCbor(index + 1)),
      "test.inputs",
    );
    expect(witness.inputs).toHaveLength(180);

    // #604 deleted the module's typed `datum` field along with the publication
    // that consumed it, so what is priced here is the shape min-Ada is still
    // asked about: a §8.5 nothing-but-bytes inline datum over the §5.1 preimage
    // of the same 180 items.
    const protocolParameters = await resolveProtocolParameters(lucid);
    const expectedLovelace = minimumLovelaceForInlineDatumOutput({
      address,
      datum: Data.to(
        encodeMidgardFieldPreimage(
          witness.inputs.map((input) =>
            encodeMidgardSpendInputItem({
              txId: Buffer.from(input.tx_id, "hex"),
              outputIndex: Number(input.output_index),
            }),
          ),
        ).toString("hex"),
      ),
      coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
    });
    // The fixpoint terminates above the bare minimum, which is the property the
    // publication builders depend on.
    expect(expectedLovelace).toBeGreaterThan(5_000_000n);
  }, 30_000);
});
