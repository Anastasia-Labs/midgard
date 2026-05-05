import { CML, walletFromSeed, type Network } from "@lucid-evolution/lucid";
import type { Address } from "./core/types.js";
import { BuilderInvariantError, SigningError } from "./core/errors.js";
import {
  decodeMidgardAddressBytes,
  ensureHash32,
  midgardAddressFromText,
  type Hash32,
} from "./codec/index.js";

export type VKeyWitness = InstanceType<typeof CML.Vkeywitness>;
export type PrivateKey = InstanceType<typeof CML.PrivateKey>;

export type ExternalBodyHashSigner = {
  readonly address?: (() => Promise<Address> | Address) | Address;
  readonly keyHash?: (() => Promise<string> | string) | string;
  readonly signBodyHash: (
    bodyHash: Hash32,
  ) => Promise<VKeyWitness> | VKeyWitness;
};

export type MidgardWallet = {
  readonly address: () => Promise<Address>;
  readonly rewardAddress?: () => Promise<Address>;
  readonly keyHash: () => Promise<string>;
  readonly signBodyHash: (bodyHash: Uint8Array) => Promise<VKeyWitness>;
};

export type WalletNetworkValidation = {
  readonly expectedNetworkId?: number;
};

const validateNetworkId = (networkId: number): number => {
  if (!Number.isInteger(networkId) || networkId < 0 || networkId > 15) {
    throw new BuilderInvariantError(
      "Invalid Cardano network id",
      `networkId=${networkId.toString()}`,
    );
  }
  return networkId;
};

export const addressNetworkId = (address: Address): number => {
  try {
    return decodeMidgardAddressBytes(midgardAddressFromText(address)).networkId;
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid wallet address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export const paymentKeyHashFromAddress = (address: Address): string => {
  let decoded: ReturnType<typeof decodeMidgardAddressBytes>;
  try {
    decoded = decodeMidgardAddressBytes(midgardAddressFromText(address));
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid wallet address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  if (decoded.paymentCredential.kind !== "PubKey") {
    throw new BuilderInvariantError(
      "Wallet address must use a public-key payment credential",
    );
  }
  return decoded.paymentCredential.hash.toString("hex");
};

export const assertAddressNetwork = (
  address: Address,
  expectedNetworkId: number | undefined,
): void => {
  if (expectedNetworkId === undefined) {
    return;
  }
  const actual = addressNetworkId(address);
  const expected = validateNetworkId(expectedNetworkId);
  if (actual !== expected) {
    throw new BuilderInvariantError(
      "Wallet address network mismatch",
      `expected=${expected.toString()} actual=${actual.toString()}`,
    );
  }
};

const normalizeBodyHash = (bodyHash: Uint8Array): Hash32 =>
  ensureHash32(bodyHash, "body_hash");

export const verifyVKeyWitness = (
  bodyHash: Uint8Array,
  witness: VKeyWitness,
): boolean => {
  const normalizedBodyHash = normalizeBodyHash(bodyHash);
  return witness.vkey().verify(normalizedBodyHash, witness.ed25519_signature());
};

export const assertVKeyWitness = (
  bodyHash: Uint8Array,
  witness: VKeyWitness,
): VKeyWitness => {
  if (!verifyVKeyWitness(bodyHash, witness)) {
    throw new SigningError("VKey witness does not verify for body hash");
  }
  return witness;
};

export const makeVKeyWitness = (
  bodyHash: Uint8Array,
  privateKey: PrivateKey,
): VKeyWitness => {
  const normalizedBodyHash = normalizeBodyHash(bodyHash);
  return assertVKeyWitness(
    normalizedBodyHash,
    CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(normalizedBodyHash),
      privateKey,
    ),
  );
};

const keyHashOfPrivateKey = (privateKey: PrivateKey): string =>
  privateKey.to_public().hash().to_hex();

const assertAddressMatchesKeyHash = (
  address: Address,
  keyHash: string,
): void => {
  const addressKeyHash = paymentKeyHashFromAddress(address);
  if (addressKeyHash !== keyHash) {
    throw new BuilderInvariantError(
      "Wallet address does not match signing key",
      `address_key_hash=${addressKeyHash} signer_key_hash=${keyHash}`,
    );
  }
};

const normalizeKeyHash = (keyHash: string): string => {
  if (!/^[0-9a-fA-F]{56}$/.test(keyHash)) {
    throw new SigningError("Invalid signing key hash");
  }
  return keyHash.toLowerCase();
};

const normalizeSeedPhrase = (seedPhrase: string): string => {
  const normalized = seedPhrase
    .trim()
    .split(/\s+/)
    .filter((word) => word.length > 0)
    .join(" ");
  if (normalized.length === 0) {
    throw new BuilderInvariantError("Seed phrase must not be empty");
  }
  return normalized;
};

export const walletFromPrivateKey = (
  privateKey: PrivateKey | string,
  address: Address,
  options: WalletNetworkValidation = {},
): MidgardWallet => {
  const parsedPrivateKey =
    typeof privateKey === "string"
      ? CML.PrivateKey.from_bech32(privateKey)
      : privateKey;
  const keyHash = keyHashOfPrivateKey(parsedPrivateKey);
  assertAddressNetwork(address, options.expectedNetworkId);
  assertAddressMatchesKeyHash(address, keyHash);
  return {
    address: async () => address,
    keyHash: async () => keyHash,
    signBodyHash: async (bodyHash) =>
      makeVKeyWitness(bodyHash, parsedPrivateKey),
  };
};

export const walletFromSeedPhrase = (
  seedPhrase: string,
  options: {
    readonly network: Network;
    readonly expectedNetworkId?: number;
  },
): MidgardWallet => {
  const wallet = walletFromSeed(normalizeSeedPhrase(seedPhrase), {
    network: options.network,
  });
  const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const keyHash = keyHashOfPrivateKey(privateKey);
  assertAddressNetwork(wallet.address, options.expectedNetworkId);
  assertAddressMatchesKeyHash(wallet.address, keyHash);
  return {
    address: async () => wallet.address,
    ...(wallet.rewardAddress === null
      ? {}
      : { rewardAddress: async () => wallet.rewardAddress as Address }),
    keyHash: async () => keyHash,
    signBodyHash: async (bodyHash) => makeVKeyWitness(bodyHash, privateKey),
  };
};

const resolveExternalIdentity = async (
  signer: ExternalBodyHashSigner,
  options: WalletNetworkValidation,
): Promise<string> => {
  let addressKeyHash: string | undefined;
  if (signer.address !== undefined) {
    if (signer.address === undefined) {
      throw new SigningError("External signer does not expose an address");
    }
    const address =
      typeof signer.address === "function"
        ? await signer.address()
        : signer.address;
    assertAddressNetwork(address, options.expectedNetworkId);
    addressKeyHash = paymentKeyHashFromAddress(address);
  }

  let declaredKeyHash: string | undefined;
  if (signer.keyHash !== undefined) {
    const keyHash =
      typeof signer.keyHash === "function"
        ? await signer.keyHash()
        : signer.keyHash;
    declaredKeyHash = normalizeKeyHash(keyHash);
  }

  const resolved = declaredKeyHash ?? addressKeyHash;
  if (resolved === undefined) {
    throw new SigningError(
      "External signer must expose an address or key hash for identity binding",
    );
  }
  if (
    declaredKeyHash !== undefined &&
    addressKeyHash !== undefined &&
    declaredKeyHash !== addressKeyHash
  ) {
    throw new SigningError("External signer address/key hash mismatch");
  }
  return resolved;
};

export const walletFromExternalSigner = (
  signer: ExternalBodyHashSigner,
  options: WalletNetworkValidation = {},
): MidgardWallet => ({
  address: async () => {
    if (signer.address === undefined) {
      throw new SigningError("External signer does not expose an address");
    }
    const address =
      typeof signer.address === "function"
        ? await signer.address()
        : signer.address;
    assertAddressNetwork(address, options.expectedNetworkId);
    return address;
  },
  keyHash: async () => resolveExternalIdentity(signer, options),
  signBodyHash: async (bodyHash) => {
    const normalizedBodyHash = normalizeBodyHash(bodyHash);
    const expectedKeyHash = await resolveExternalIdentity(signer, options);
    const witness = assertVKeyWitness(
      normalizedBodyHash,
      await signer.signBodyHash(normalizedBodyHash),
    );
    const actualKeyHash = witness.vkey().hash().to_hex();
    if (actualKeyHash !== expectedKeyHash) {
      throw new SigningError("External signer witness key hash mismatch");
    }
    return witness;
  },
});
