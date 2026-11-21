import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  scriptFromNative,
  type TxSignBuilder,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildUnsignedFieldPreimagePublicationProgram,
  minimumLovelaceForFieldPreimagePublication,
} from "../src/fraud-proof/field-preimage-carriage.js";

const ADA = 1_000_000n;

const PUBLICATION = {
  chunkIndex: 0,
  datumCbor: Data.to("cd".repeat(4096)),
  byteLength: 4096,
  digestHex: "00".repeat(32),
};

/**
 * A wallet whose plain coins are `coins` lovelace each, plus change, and a
 * donor that can pay it more.
 */
const walletOf = async (lovelace: bigint, coins: readonly bigint[]) => {
  const account = generateEmulatorAccount({ lovelace });
  const donor = generateEmulatorAccount({ lovelace: 10_000n * ADA });
  const emulator = new Emulator([account, donor]);
  const lucid = await Lucid(emulator, "Custom");
  const submit = async (seedPhrase: string, paid: readonly bigint[]) => {
    lucid.selectWallet.fromSeed(seedPhrase);
    let paying = lucid.newTx();
    for (const coin of paid)
      paying = paying.pay.ToAddress(account.address, { lovelace: coin });
    const signed = await (await paying.complete()).sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    lucid.selectWallet.fromSeed(account.seedPhrase);
    return txHash;
  };
  const txHash = await submit(account.seedPhrase, coins);
  return {
    lucid,
    address: account.address,
    txHash,
    donate: async (coin: bigint) => await submit(donor.seedPhrase, [coin]),
  };
};

const publicationTo = async (lucid: LucidEvolution, address: string) =>
  await Effect.runPromise(
    buildUnsignedFieldPreimagePublicationProgram(lucid, {
      publication: PUBLICATION,
      publisherAddress: address,
    }),
  );

const spent = (unsigned: TxSignBuilder) => {
  const inputs = unsigned.toTransaction().body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

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

  it("is funded from the largest plain coin alone, equal coins by out-ref, so a rebuild over an unchanged wallet is identical", async () => {
    const { lucid, address, txHash } = await walletOf(1_000n * ADA, [
      400n * ADA,
      400n * ADA,
    ]);
    const first = await publicationTo(lucid, address);
    expect(spent(first)).toEqual([`${txHash}#0`]);
    const rebuilt = await publicationTo(lucid, address);
    expect(rebuilt.toHash()).toBe(first.toHash());
    expect(rebuilt.toCBOR()).toBe(first.toCBOR());
  });

  it("is funded from the new largest coin once it has changed, so both copies land", async () => {
    const wallet = await walletOf(1_000n * ADA, [400n * ADA]);
    const first = await publicationTo(wallet.lucid, wallet.address);
    const donated = await wallet.donate(800n * ADA);
    const second = await publicationTo(wallet.lucid, wallet.address);
    expect(spent(second)).toEqual([`${donated}#0`]);
    expect(spent(first)).not.toEqual(spent(second));
    for (const unsigned of [first, second]) {
      const signed = await unsigned.sign.withWallet().complete();
      await wallet.lucid.awaitTx(await signed.submit());
    }
    const copies = (await wallet.lucid.utxosAt(wallet.address)).filter(
      (utxo) => utxo.datum === PUBLICATION.datumCbor,
    );
    expect(copies.map((utxo) => utxo.txHash).sort()).toEqual(
      [first.toHash(), second.toHash()].sort(),
    );
  });

  it("falls back to every plain coin when no one coin covers the publication", async () => {
    const lucid = await Lucid(new Emulator([]), "Custom");
    const { coinsPerUtxoByte } = lucid.config().protocolParameters!;
    const required = minimumLovelaceForFieldPreimagePublication({
      publisherAddress: generateEmulatorAccount({}).address,
      output: PUBLICATION,
      coinsPerUtxoByte,
    });
    const coin = (required * 2n) / 3n;
    const wallet = await walletOf(4n * coin, [coin, coin, coin]);
    const coins = await wallet.lucid.wallet().getUtxos();
    expect(coins.every((utxo) => (utxo.assets.lovelace ?? 0n) < required)).toBe(
      true,
    );
    const unsigned = await publicationTo(wallet.lucid, wallet.address);
    expect(spent(unsigned).length).toBeGreaterThan(1);
  });
});
