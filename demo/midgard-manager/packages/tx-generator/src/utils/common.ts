import { Assets, CML, Data, walletFromSeed } from '@lucid-evolution/lucid';

/**
 * Core utility helpers shared across tx-generator modules.
 */

type SerializedAssets = Record<string, string>;

/**
 * Serializes bigint asset quantities into JSON-safe string values.
 */
export const serializeAssets = (assets: Assets): SerializedAssets => {
  return Object.fromEntries(
    Object.entries(assets).map(([asset, value]) => [asset, value.toString()])
  );
};

/**
 * Normalizes either a mnemonic phrase or an already-encoded key into a bech32
 * private key.
 */
export const parseUnknownKeytoBech32PrivateKey = (unknownKey: unknown): string => {
  if (typeof unknownKey !== 'string')
    throw new Error('Expected a string value for the private key');

  const key = unknownKey.trim();
  if (key.includes(' ')) {
    const wallet = walletFromSeed(key, {
      accountIndex: 0,
      addressType: 'Base',
    });
    return wallet.paymentKey;
  }

  try {
    return CML.PrivateKey.from_normal_bytes(Buffer.from(key.substring(4), 'hex')).to_bech32();
  } catch {
    return CML.PrivateKey.from_bech32(key).to_bech32();
  }
};

/**
 * Derives the payment public-key hash from a bech32 private key.
 */
export const getPublicKeyHashFromPrivateKey = (privateKey: string): string =>
  CML.PrivateKey.from_bech32(privateKey).to_public().hash().to_hex();

/**
 * Encodes a private key as the CBOR hex format expected by some test tooling.
 */
export const getPrivateKeyCborHex = (privateKey: string): string =>
  Data.to(Buffer.from(CML.PrivateKey.from_bech32(privateKey).to_raw_bytes()).toString('hex'));

export const formatError = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * Resolves once a writable stream can accept more data.
 */
export const waitWritable = (writable: NodeJS.WritableStream): Promise<void> =>
  new Promise((resolve) => (writable.writable ? resolve() : writable.once('drain', resolve)));
