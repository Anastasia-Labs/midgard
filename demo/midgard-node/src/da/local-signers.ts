import { CML, type Network, walletFromSeed } from "@lucid-evolution/lucid";

/** Hex length of a packed 32-byte Ed25519 verification key. */
export const VERIFICATION_KEY_HEX_LENGTH = 64;

/** Hex length of a 28-byte payment key hash (an `owners` entry). */
export const VERIFICATION_KEY_HASH_HEX_LENGTH = 56;

/**
 * Which configured seed a local DA key came from. Diagnostics only — the
 * governor knows nothing about roles, it only sees keys.
 */
export type DaLocalSignerRole = "operator" | "cosigner";

/**
 * One DA governance key this process holds in memory and can sign with.
 */
export type DaLocalSigner = {
  readonly role: DaLocalSignerRole;
  /** 32-byte Ed25519 verification key, hex — a `committee` member entry. */
  readonly verificationKeyHex: string;
  /** 28-byte payment key hash, hex — an `owners` entry. */
  readonly keyHashHex: string;
  /** Raw Ed25519 signature over `message`, hex (64 bytes). */
  readonly sign: (message: Uint8Array) => string;
};

export type DaLocalSignerConfig = {
  readonly L1_OPERATOR_SEED_PHRASE: string;
  readonly NETWORK: Network;
  readonly DA_COSIGNER_SEED_PHRASE?: string;
};

const signerFromSeed = (
  role: DaLocalSignerRole,
  seedPhrase: string,
  network: Network,
): DaLocalSigner => {
  const wallet = walletFromSeed(seedPhrase, { network });
  const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const publicKey = privateKey.to_public();
  return {
    role,
    verificationKeyHex: Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
    keyHashHex: publicKey.hash().to_hex(),
    sign: (message) => privateKey.sign(message).to_hex(),
  };
};

/**
 * The DA governance keys this process itself holds, operator first.
 *
 * Q63 (F04 §4) made a 1-of-1 DA configuration unrepresentable: `da_threshold`
 * and `update_threshold` both carry a floor of at least two. A node that
 * bootstraps its own deployment therefore needs a *second* key before it can
 * write a governor-valid `DaParamsDatum`, and before it can reach an
 * attestation threshold without peers.
 *
 * `DA_COSIGNER_SEED_PHRASE` is that second key, and it is always explicit
 * configuration:
 *
 * - Dev and emulator bootstrap generate it next to the operator wallet, so one
 *   process can produce a genuine 2-of-2 attestation with two distinct keys.
 * - A real deployment leaves it unset: the committee is other machines' keys,
 *   named by `DA_COMMITTEE_HEX`, the owner set by `DA_OWNERS_HEX`, and peers
 *   contribute their own signatures over libp2p.
 *
 * Nothing here invents a key. When neither a configured committee/owner set nor
 * a cosigner seed is present, the callers below fail closed rather than fall
 * back to the 1-of-1 shape the governor rejects.
 */
export const daLocalSigners = (
  config: DaLocalSignerConfig,
): readonly DaLocalSigner[] => {
  const signers = [
    signerFromSeed("operator", config.L1_OPERATOR_SEED_PHRASE, config.NETWORK),
  ];
  const cosignerSeed = (config.DA_COSIGNER_SEED_PHRASE ?? "").trim();
  if (cosignerSeed.length > 0) {
    const cosigner = signerFromSeed("cosigner", cosignerSeed, config.NETWORK);
    // Deduplicate by key, not by seed: a cosigner seed that derives the
    // operator's own payment key contributes no second committee member and no
    // second signature, so keeping it would misrepresent a 1-of-1 as a 2-of-2.
    if (
      !signers.some(
        (signer) => signer.verificationKeyHex === cosigner.verificationKeyHex,
      )
    ) {
      signers.push(cosigner);
    }
  }
  return signers;
};

/**
 * Splits a packed hex string into fixed-width chunks.
 *
 * Throws when the input is not lowercase hex or not an exact multiple of the
 * chunk width — both are encoding faults the governor would reject on-chain
 * with no usable diagnostic.
 */
export const splitPackedHex = (
  packed: string,
  chunkHexLength: number,
  fieldName: string,
): readonly string[] => {
  const normalized = packed.trim().toLowerCase();
  if (!/^[0-9a-f]*$/.test(normalized)) {
    throw new Error(`${fieldName} must be hex`);
  }
  if (normalized.length === 0 || normalized.length % chunkHexLength !== 0) {
    throw new Error(
      `${fieldName} must be a non-empty multiple of ${(chunkHexLength / 2).toString()} bytes`,
    );
  }
  const chunks: string[] = [];
  for (let offset = 0; offset < normalized.length; offset += chunkHexLength) {
    chunks.push(normalized.slice(offset, offset + chunkHexLength));
  }
  return chunks;
};

/**
 * True when `values` is strictly ascending, which is exactly the sorted-unique
 * encoding `valid_datum` requires of both `committee` and `owners` in
 * `da-params-governor.ak` (`expect bytearray.compare(prev, key) == Less`).
 *
 * Equal-length lowercase hex compares bytewise under plain string ordering,
 * because `0`-`9` and `a`-`f` are each monotonic in nibble value and ordered
 * relative to one another. Callers guarantee that shape by feeding this
 * {@link splitPackedHex} output.
 */
export const isStrictlyAscending = (values: readonly string[]): boolean =>
  values.every((value, index) => index === 0 || values[index - 1]! < value);

/**
 * Position of `verificationKeyHex` in a packed committee, or `null` when the
 * key is not a member.
 *
 * The index is the signer index the attestation validator uses to select a
 * verification key and to set the attested-signer bit, so it must be read off
 * the on-chain committee rather than assumed.
 */
export const committeeSignerIndex = (
  committeeHex: string,
  verificationKeyHex: string,
): number | null => {
  const members = splitPackedHex(
    committeeHex,
    VERIFICATION_KEY_HEX_LENGTH,
    "DA committee",
  );
  const index = members.indexOf(verificationKeyHex.trim().toLowerCase());
  return index < 0 ? null : index;
};
