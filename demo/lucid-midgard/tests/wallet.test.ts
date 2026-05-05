import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  addressNetworkId,
  assertAddressNetwork,
  assertVKeyWitness,
  makeVKeyWitness,
  verifyVKeyWitness,
  walletFromExternalSigner,
  walletFromPrivateKey,
  walletFromSeedPhrase,
  BuilderInvariantError,
  SigningError,
} from "../src/index.js";

const seedPhrase = "test test test test test test test test test test test junk";
const expectedPreviewAddress =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";
const bodyHash = Buffer.from("11".repeat(32), "hex");
const otherHash = Buffer.from("22".repeat(32), "hex");

const enterpriseAddressFor = (
  privateKey: InstanceType<typeof CML.PrivateKey>,
): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();

describe("Midgard wallets and signers", () => {
  it("derives seed wallet address and key hash", async () => {
    const wallet = walletFromSeedPhrase(seedPhrase, {
      network: "Preview",
      expectedNetworkId: 0,
    });

    await expect(wallet.address()).resolves.toBe(expectedPreviewAddress);
    await expect(wallet.keyHash()).resolves.toMatch(/^[0-9a-f]{56}$/);
  });

  it("signs and verifies Midgard-native body hashes with private keys", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const wallet = walletFromPrivateKey(
      privateKey,
      enterpriseAddressFor(privateKey),
      { expectedNetworkId: 0 },
    );

    const witness = await wallet.signBodyHash(bodyHash);

    expect(verifyVKeyWitness(bodyHash, witness)).toBe(true);
    expect(verifyVKeyWitness(otherHash, witness)).toBe(false);
    await expect(wallet.keyHash()).resolves.toBe(
      privateKey.to_public().hash().to_hex(),
    );
  });

  it("rejects invalid external signer witnesses", async () => {
    const firstKey = CML.PrivateKey.generate_ed25519();
    const secondKey = CML.PrivateKey.generate_ed25519();
    const wallet = walletFromExternalSigner({
      address: enterpriseAddressFor(firstKey),
      keyHash: firstKey.to_public().hash().to_hex(),
      signBodyHash: () => makeVKeyWitness(otherHash, secondKey),
    });

    await expect(wallet.signBodyHash(bodyHash)).rejects.toBeInstanceOf(
      SigningError,
    );
  });

  it("binds external signer witnesses to the declared identity", async () => {
    const firstKey = CML.PrivateKey.generate_ed25519();
    const secondKey = CML.PrivateKey.generate_ed25519();
    const wallet = walletFromExternalSigner({
      address: enterpriseAddressFor(firstKey),
      keyHash: firstKey.to_public().hash().to_hex(),
      signBodyHash: () => makeVKeyWitness(bodyHash, secondKey),
    });

    await expect(wallet.signBodyHash(bodyHash)).rejects.toBeInstanceOf(
      SigningError,
    );
  });

  it("rejects private-key wallets with unrelated addresses", () => {
    const firstKey = CML.PrivateKey.generate_ed25519();
    const secondKey = CML.PrivateKey.generate_ed25519();

    expect(() =>
      walletFromPrivateKey(firstKey, enterpriseAddressFor(secondKey)),
    ).toThrow(BuilderInvariantError);
  });

  it("validates external signer network before signing", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const wallet = walletFromExternalSigner(
      {
        address: enterpriseAddressFor(privateKey),
        keyHash: privateKey.to_public().hash().to_hex(),
        signBodyHash: () => makeVKeyWitness(bodyHash, privateKey),
      },
      { expectedNetworkId: 1 },
    );

    await expect(wallet.signBodyHash(bodyHash)).rejects.toBeInstanceOf(
      BuilderInvariantError,
    );
  });

  it("does not accept a Cardano-domain witness for a Midgard body hash", () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const cardanoBody = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      0n,
    );
    const cardanoTxHash = CML.hash_transaction(cardanoBody).to_raw_bytes();
    expect(Buffer.from(cardanoTxHash)).not.toEqual(bodyHash);
    const cardanoDomainWitness = CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(cardanoTxHash),
      privateKey,
    );

    expect(verifyVKeyWitness(bodyHash, cardanoDomainWitness)).toBe(false);
    expect(() => assertVKeyWitness(bodyHash, cardanoDomainWitness)).toThrow(
      SigningError,
    );
  });

  it("validates wallet address network before use", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const address = enterpriseAddressFor(privateKey);

    expect(addressNetworkId(address)).toBe(0);
    expect(() => assertAddressNetwork(address, 1)).toThrow(
      BuilderInvariantError,
    );
    expect(() =>
      walletFromPrivateKey(privateKey, address, { expectedNetworkId: 1 }),
    ).toThrow(BuilderInvariantError);
  });
});
