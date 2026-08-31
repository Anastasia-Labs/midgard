import {
  createPrivateKey,
  createPublicKey,
  type KeyObject,
  sign,
  verify,
} from "node:crypto";
import { readFile } from "node:fs/promises";

import {
  daAvailabilityAttestationMessageV1,
  type DaAvailabilityCommitmentV1,
} from "@al-ft/midgard-sdk";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type { DaParamsConfig } from "./config.js";
import { bytesToHex, hexToBytes, normalizeHex } from "./utils/hex.js";

export type DaSigner = {
  readonly publicKeyHex: string;
  readonly sign: (message: Buffer) => Buffer;
  readonly privateKey?: KeyObject;
};

export type DaCommitteeValidation = {
  readonly committeeKeys: readonly string[];
  readonly committeeSignersHash: string;
  readonly threshold: number;
};

export type DaSignerValidation = DaCommitteeValidation & {
  readonly signerIndex: number;
  readonly signerPublicKeyHex: string;
};

export const loadDaSigner = async (keySource: string): Promise<DaSigner> => {
  const cardanoSigner = await tryLoadCardanoDaSigner(keySource);
  if (cardanoSigner !== undefined) {
    return cardanoSigner;
  }
  const keyHex = await readRawKeySource(keySource);
  const seed = parseEd25519Seed(keyHex);
  const privateKey = createPrivateKey({
    key: Buffer.concat([ED25519_PKCS8_SEED_PREFIX, seed]),
    format: "der",
    type: "pkcs8",
  });
  return {
    privateKey,
    publicKeyHex: extractRawEd25519PublicKey(createPublicKey(privateKey)),
    sign: (message) => sign(null, message, privateKey),
  };
};

export const validateDaSignerMembership = ({
  daParams,
  signer,
  signerIndex,
}: {
  readonly daParams: DaParamsConfig;
  readonly signer: DaSigner;
  readonly signerIndex: number;
}): DaSignerValidation => {
  const committeeValidation = validateDaCommittee({ daParams });
  if (signerIndex >= committeeValidation.committeeKeys.length) {
    throw new Error("DA_SIGNER_INDEX is outside the DA committee");
  }
  if (committeeValidation.committeeKeys[signerIndex] !== signer.publicKeyHex) {
    throw new Error(
      `signer public key does not match committee index ${signerIndex.toString()}`,
    );
  }
  return {
    ...committeeValidation,
    signerIndex,
    signerPublicKeyHex: signer.publicKeyHex,
  };
};

export const validateDaCommittee = ({
  daParams,
}: {
  readonly daParams: DaParamsConfig;
}): DaCommitteeValidation => {
  const committee = hexToBytes(daParams.committeeHex, "DA committee");
  if (committee.length % 32 !== 0 || committee.length === 0) {
    throw new Error("DA committee must contain packed 32-byte keys");
  }
  const committeeKeys = Array.from(
    { length: committee.length / 32 },
    (_, index) =>
      committee.subarray(index * 32, (index + 1) * 32).toString("hex"),
  );
  const computedCommitteeHash = bytesToHex(blake2b(committee, { dkLen: 32 }));
  if (computedCommitteeHash !== daParams.committeeSignersHash) {
    throw new Error(
      `DA committee hash mismatch: computed=${computedCommitteeHash}, configured=${daParams.committeeSignersHash}`,
    );
  }
  if (daParams.threshold > committeeKeys.length) {
    throw new Error("DA threshold cannot exceed committee length");
  }
  return {
    committeeKeys,
    committeeSignersHash: daParams.committeeSignersHash,
    threshold: daParams.threshold,
  };
};

export const signDaAttestation = ({
  signer,
  signerIndex,
  availabilityCommitment,
}: {
  readonly signer: DaSigner;
  readonly signerIndex: number;
  readonly availabilityCommitment: DaAvailabilityCommitmentV1;
}): string => {
  if (
    !Number.isSafeInteger(signerIndex) ||
    signerIndex < 0 ||
    signerIndex > 255
  ) {
    throw new Error("signer index must fit in one byte");
  }
  const signature = signer.sign(
    Buffer.from(daAvailabilityAttestationMessageV1(availabilityCommitment)),
  );
  if (signature.length !== 64) {
    throw new Error("Ed25519 signature was not 64 bytes");
  }
  return Buffer.concat([Buffer.from([signerIndex]), signature]).toString("hex");
};

export const verifyDaSignatureWitness = ({
  publicKeyHex,
  availabilityCommitment,
  witnessHex,
}: {
  readonly publicKeyHex: string;
  readonly availabilityCommitment: DaAvailabilityCommitmentV1;
  readonly witnessHex: string;
}): boolean => {
  const witness = hexToBytes(witnessHex, "signature witness", 65);
  const publicKey = createPublicKey({
    key: Buffer.concat([
      ED25519_SPKI_PUBLIC_PREFIX,
      hexToBytes(publicKeyHex, "public key", 32),
    ]),
    format: "der",
    type: "spki",
  });
  return verify(
    null,
    Buffer.from(daAvailabilityAttestationMessageV1(availabilityCommitment)),
    publicKey,
    witness.subarray(1),
  );
};

export const verifyEd25519Signature = ({
  publicKeyHex,
  message,
  signatureHex,
}: {
  readonly publicKeyHex: string;
  readonly message: Buffer;
  readonly signatureHex: string;
}): boolean => {
  const publicKey = createPublicKey({
    key: Buffer.concat([
      ED25519_SPKI_PUBLIC_PREFIX,
      hexToBytes(publicKeyHex, "public key", 32),
    ]),
    format: "der",
    type: "spki",
  });
  return verify(
    null,
    message,
    publicKey,
    hexToBytes(signatureHex, "Ed25519 signature", 64),
  );
};

const tryLoadCardanoDaSigner = async (
  source: string,
): Promise<DaSigner | undefined> => {
  if (source.startsWith("cardano-seed:")) {
    return cardanoDaSignerFromSeedPhrase(source.slice("cardano-seed:".length));
  }
  if (source.startsWith("cardano-mnemonic:")) {
    return cardanoDaSignerFromSeedPhrase(
      source.slice("cardano-mnemonic:".length),
    );
  }
  if (source.startsWith("cardano-seed-file:")) {
    return cardanoDaSignerFromSeedPhrase(
      (
        await readFile(source.slice("cardano-seed-file:".length), "utf8")
      ).trim(),
    );
  }
  if (source.startsWith("cardano-private-key:")) {
    return cardanoDaSignerFromPrivateKeyBech32(
      source.slice("cardano-private-key:".length),
    );
  }
  if (source.startsWith("cardano-private-key-file:")) {
    return cardanoDaSignerFromPrivateKeyBech32(
      (
        await readFile(source.slice("cardano-private-key-file:".length), "utf8")
      ).trim(),
    );
  }
  return undefined;
};

const readRawKeySource = async (source: string): Promise<string> => {
  if (source.startsWith("hex:")) {
    return source.slice("hex:".length);
  }
  if (source.startsWith("file:")) {
    return (await readFile(source.slice("file:".length), "utf8")).trim();
  }
  const normalized = source.trim();
  if (/^[0-9a-fA-F]+$/.test(normalized)) {
    return normalized;
  }
  return (await readFile(normalized, "utf8")).trim();
};

const cardanoDaSignerFromSeedPhrase = (seedPhrase: string): DaSigner => {
  const wallet = walletFromSeed(seedPhrase.trim().replace(/\s+/g, " "), {
    network: "Preview",
  });
  return cardanoDaSignerFromPrivateKeyBech32(wallet.paymentKey);
};

const cardanoDaSignerFromPrivateKeyBech32 = (
  privateKeyBech32: string,
): DaSigner => {
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32.trim());
  return {
    publicKeyHex: Buffer.from(privateKey.to_public().to_raw_bytes()).toString(
      "hex",
    ),
    sign: (message) => Buffer.from(privateKey.sign(message).to_hex(), "hex"),
  };
};

const parseEd25519Seed = (keyHex: string): Buffer => {
  const normalized = normalizeHex(keyHex, { fieldName: "DA signer key" });
  if (normalized.length === 64) {
    return Buffer.from(normalized, "hex");
  }
  if (normalized.length === 128) {
    return Buffer.from(normalized.slice(0, 64), "hex");
  }
  throw new Error(
    "DA signer key source must be a 32-byte seed or 64-byte secret",
  );
};

const extractRawEd25519PublicKey = (publicKey: KeyObject): string => {
  const der = publicKey.export({ format: "der", type: "spki" });
  const prefix = ED25519_SPKI_PUBLIC_PREFIX;
  if (
    der.length !== prefix.length + 32 ||
    !Buffer.from(der.subarray(0, prefix.length)).equals(prefix)
  ) {
    throw new Error("unexpected Ed25519 public key DER encoding");
  }
  return Buffer.from(der.subarray(prefix.length)).toString("hex");
};

const ED25519_PKCS8_SEED_PREFIX = Buffer.from(
  "302e020100300506032b657004220420",
  "hex",
);

const ED25519_SPKI_PUBLIC_PREFIX = Buffer.from(
  "302a300506032b6570032100",
  "hex",
);
