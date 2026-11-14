import { resolveProverSigner } from "@al-ft/midgard-fault-proofs";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

it("builds fixture and prover transactions from disjoint addresses sharing one payment key", async () => {
  const fixture = generateEmulatorAccount({ lovelace: 40_000_000n });
  const signer = resolveProverSigner({
    network: "Custom",
    walletSeedPhrase: fixture.seedPhrase,
  });
  const recipient = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator([
    fixture,
    { ...fixture, address: signer.address, assets: { lovelace: 50_000_000n } },
  ]);
  const fixtureLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  // Match loadJourneyContext's default Base wallet and the actual prover signer.
  fixtureLucid.selectWallet.fromSeed(fixture.seedPhrase);
  signer.selectWallet(proverLucid);

  expect(await fixtureLucid.wallet().address()).toBe(fixture.address);
  expect(await proverLucid.wallet().address()).toBe(signer.address);
  expect(fixture.address).not.toBe(signer.address);
  expect(paymentCredentialOf(fixture.address).hash).toBe(signer.paymentKeyHash);
  expect(paymentCredentialOf(signer.address).hash).toBe(signer.paymentKeyHash);

  const fixtureInputs = await fixtureLucid.wallet().getUtxos();
  const proverInputs = await proverLucid.wallet().getUtxos();
  expect(fixtureInputs).toHaveLength(1);
  expect(proverInputs).toHaveLength(1);
  expect(fixtureInputs[0]!.address).toBe(fixture.address);
  expect(proverInputs[0]!.address).toBe(signer.address);
  const outRef = (value: { txHash: string; outputIndex: number }) =>
    `${value.txHash}#${value.outputIndex}`;
  expect(outRef(fixtureInputs[0]!)).not.toBe(outRef(proverInputs[0]!));

  // Build both before submitting either: selection must not need a shared pause.
  const transactions = await Promise.all(
    [fixtureLucid, proverLucid].map(async (lucid) => {
      const tx = await lucid
        .newTx()
        .pay.ToAddress(recipient.address, { lovelace: 4_000_000n })
        .complete({ localUPLCEval: true });
      return await tx.sign.withWallet().complete();
    }),
  );
  for (const [index, signed] of transactions.entries()) {
    const transaction = CML.Transaction.from_cbor_hex(signed.toCBOR());
    const inputs = transaction.body().inputs();
    expect(inputs.len()).toBe(1);
    const expected = index === 0 ? fixtureInputs : proverInputs;
    expect(
      `${inputs.get(0).transaction_id().to_hex()}#${inputs.get(0).index()}`,
    ).toBe(outRef(expected[0]!));
    const expectedChangeAddress =
      index === 0 ? fixture.address : signer.address;
    const outputs = transaction.body().outputs();
    expect(
      Array.from({ length: outputs.len() }, (_, i) =>
        outputs.get(i).address().to_bech32(),
      ),
    ).toContain(expectedChangeAddress);
  }

  await transactions[0]!.submit();
  emulator.awaitBlock(1);
  expect((await proverLucid.wallet().getUtxos()).map(outRef)).toEqual(
    proverInputs.map(outRef),
  );
  await transactions[1]!.submit();
  emulator.awaitBlock(1);
  expect(await emulator.getUtxos(recipient.address)).toHaveLength(2);
});

it.each(["fixture", "prover"] as const)(
  "does not borrow the other address's funds when only %s is funded",
  async (funded) => {
    const fixture = generateEmulatorAccount({ lovelace: 40_000_000n });
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: fixture.seedPhrase,
    });
    const fundedAddress =
      funded === "fixture" ? fixture.address : signer.address;
    const emulator = new Emulator([{ ...fixture, address: fundedAddress }]);
    const lucid = await Lucid(emulator, "Custom");
    if (funded === "fixture") signer.selectWallet(lucid);
    else lucid.selectWallet.fromSeed(fixture.seedPhrase);
    expect(await emulator.getUtxos(fundedAddress)).toHaveLength(1);
    expect(await lucid.wallet().getUtxos()).toHaveLength(0);
    await expect(
      lucid
        .newTx()
        .pay.ToAddress(fundedAddress, { lovelace: 4_000_000n })
        .complete({ localUPLCEval: true }),
    ).rejects.toThrow();
  },
);
