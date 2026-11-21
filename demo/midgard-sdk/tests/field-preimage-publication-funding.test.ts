import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  scriptFromNative,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildUnsignedFieldPreimagePublicationProgram } from "../src/fraud-proof/field-preimage-carriage.js";

describe("field preimage publication funding", () => {
  it("preserves published evidence at the proof-materials address and script references at the funding wallet", async () => {
    const account = generateEmulatorAccount({ lovelace: 1000000000n });
    const emulator = new Emulator([account]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const key = getAddressDetails(account.address).paymentCredential!;
    const script = scriptFromNative({ type: "sig", keyHash: key.hash });
    // The key's proof-materials address: the enterprise address of its
    // sig(key) native script.
    const materials = validatorToAddress("Custom", script);
    const setup = await (
      await lucid
        .newTx()
        .pay.ToAddressWithData(
          materials,
          { kind: "inline", value: Data.to("ab") },
          { lovelace: 3000000n },
        )
        .pay.ToAddressWithData(
          account.address,
          undefined,
          { lovelace: 3000000n },
          script,
        )
        .complete()
    ).sign
      .withWallet()
      .complete();
    await lucid.awaitTx(await setup.submit());
    const protectedOutputs = [
      ...(await lucid.utxosAt(materials)),
      ...(await lucid.wallet().getUtxos()).filter(
        (utxo) => utxo.scriptRef != null,
      ),
    ];
    expect(protectedOutputs).toHaveLength(2);
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimagePublicationProgram(lucid, {
        publication: {
          chunkIndex: 0,
          datumCbor: Data.to("cd".repeat(4096)),
          byteLength: 4096,
          digestHex: "00".repeat(32),
        },
        publisherAddress: materials,
      }),
    );
    const signed = await unsigned.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
    expect(await lucid.utxosByOutRef(protectedOutputs)).toHaveLength(2);
  });
});
