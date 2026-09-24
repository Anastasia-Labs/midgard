import {
  applyDoubleCborEncoding,
  CML,
  Constr,
  Data,
  Emulator,
  Lucid,
  type SpendingValidator,
  utxoToCore,
  validatorToAddress,
  type WalletApi,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

// This validator isolates wallet construction; protocol validators are exercised
// separately by the installed watcher journey.
const validator: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("5101010023259800a518a4d136564004ae69"),
};

it.each([
  { mode: "delayed", emptyCollateral: false },
  { mode: "canonical", emptyCollateral: false },
  { mode: "default", emptyCollateral: false },
  { mode: "delayed", emptyCollateral: true },
] as const)(
  "uses exact output allocation and explicit collateral: $mode, empty reserve=$emptyCollateral",
  async ({ mode, emptyCollateral }) => {
    const key = CML.PrivateKey.from_normal_bytes(
      Buffer.from("31".repeat(32), "hex"),
    );
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const scriptAddress = validatorToAddress("Preprod", validator);
    const account = { address, seedPhrase: "", privateKey: key.to_bech32() };
    const emulator = new Emulator([
      { ...account, assets: { lovelace: 100_000_000n } },
      { ...account, assets: { lovelace: 30_000_000n } },
      {
        ...account,
        address: scriptAddress,
        assets: { lovelace: 2_000_000n },
        outputData: { inline: Data.void() },
      },
    ]);
    const lucid = await Lucid(emulator, "Preprod");
    lucid.selectWallet.fromPrivateKey(key.to_bech32());
    const original = lucid.wallet();
    const inputs = await original.getUtxos();
    const funding = inputs.filter(({ outputIndex }) => outputIndex === 0);
    const collateral = emptyCollateral
      ? []
      : inputs.filter(({ outputIndex }) => outputIndex === 1);
    const addressHex = CML.Address.from_bech32(address).to_hex();
    const api: WalletApi = {
      getNetworkId: async () => 0,
      getUtxos: async () =>
        funding.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
      getBalance: async () => CML.Value.from_coin(100_000_000n).to_cbor_hex(),
      getUsedAddresses: async () => [addressHex],
      getUnusedAddresses: async () => [],
      getChangeAddress: async () => addressHex,
      getRewardAddresses: async () => [],
      signTx: async (cbor) =>
        (
          await original.signTx(CML.Transaction.from_cbor_hex(cbor))
        ).to_cbor_hex(),
      signData: async (_address, payload) =>
        original.signMessage(address, payload),
      submitTx: async (cbor) => original.submitTx(cbor),
      getCollateral: async () =>
        collateral.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
      experimental: {
        getCollateral: async () => [],
        on: () => undefined,
        off: () => undefined,
      },
    };
    lucid.selectWallet.fromAPI(api);
    const datum = Data.to(new Constr(0, ["12".repeat(28), new Constr(1, [])]));
    // Ledger canonicalization must retain the exact requested Plutus datum.
    expect(datum).not.toBe(
      CML.PlutusData.from_cbor_hex(datum).to_canonical_cbor_hex(),
    );
    const build = lucid
      .newTx()
      .collectFrom(
        await emulator.getUtxos(scriptAddress),
        mode === "delayed" ? () => Data.void() : Data.void(),
      )
      .attach.SpendingValidator(validator)
      .pay.ToAddress(address, { lovelace: 3_000_000n })
      .pay.ToContract(
        scriptAddress,
        {
          kind: "inline",
          value: datum,
        },
        {},
      )
      .complete({ canonical: mode === "canonical", localUPLCEval: true });
    if (emptyCollateral) {
      await expect(build).rejects.toThrow("collateral");
      return;
    }
    const signed = await (await build).sign.withWallet().complete();
    const body = signed.toTransaction().body();
    const outputs = body.outputs();
    const custody = Array.from({ length: outputs.len() }, (_, index) =>
      outputs.get(index),
    ).find((output) => output.address().to_bech32() === scriptAddress)!;
    expect(custody.amount().coin()).toBe(
      CML.min_ada_required(
        custody,
        (await emulator.getProtocolParameters()).coinsPerUtxoByte,
      ),
    );
    expect(custody.datum()?.as_datum()?.to_cbor_hex()).toBe(datum);
    const ordinary = body.inputs();
    expect(
      Array.from({ length: ordinary.len() }, (_, index) =>
        Number(ordinary.get(index).index()),
      ),
    ).toEqual([0, 2]);
    expect(body.collateral_inputs()?.len()).toBe(1);
    expect(body.collateral_inputs()?.get(0).index()).toBe(1n);
    expect(body.collateral_return()?.address().to_bech32()).toBe(address);
    expect(body.total_collateral()).toBeLessThanOrEqual(30_000_000n);
    const txHash = await signed.submit();
    await emulator.awaitTx(txHash);
    expect((await emulator.getTransactionStatus(txHash)).status).toBe(
      "confirmed",
    );
    expect(await emulator.getUtxosByOutRef(collateral)).toHaveLength(1);
  },
);
