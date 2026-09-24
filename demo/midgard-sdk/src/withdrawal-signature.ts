import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { WithdrawalBody, type WithdrawalSignature } from "./ledger-state.js";

const WITHDRAWAL_SIGNATURE_DOMAIN = "MidgardWithdrawalV1";
const PUBLIC_KEY_HEX_LENGTH = 64;
const SIGNATURE_HEX_LENGTH = 128;

export type WithdrawalSignatureVerification =
  | {
      readonly valid: true;
      readonly publicKeyHash: string;
    }
  | {
      readonly valid: false;
      readonly reason: string;
      readonly publicKeyHash?: string;
    };

const isHex = (value: string): boolean => /^[0-9a-fA-F]*$/.test(value);

export const withdrawalSigningMessage = (body: WithdrawalBody): Uint8Array =>
  withdrawalSigningMessageCbor(LucidData.to(body, WithdrawalBody));

export const withdrawalSigningMessageCbor = (
  rawBodyCbor: string,
): Uint8Array => {
  // Validate the structure without re-encoding the opaque destination datum.
  LucidData.from(rawBodyCbor, WithdrawalBody);
  // The signed preimage is the body exactly as `cbor.serialise` renders it
  // on-chain (`withdrawal_signature_is_valid_v1`): definite asset maps, which
  // Lucid's own encoder does not emit.
  const bodyCbor = Buffer.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(rawBodyCbor),
    "hex",
  );
  const preimage = Buffer.concat([
    Buffer.from(WITHDRAWAL_SIGNATURE_DOMAIN, "utf8"),
    bodyCbor,
  ]);
  return blake2b(preimage, { dkLen: 32 });
};

export const signWithdrawalBody = (
  privateKey: ReturnType<typeof CML.PrivateKey.from_bech32>,
  body: WithdrawalBody,
): WithdrawalSignature =>
  signWithdrawalBodyCbor(privateKey, LucidData.to(body, WithdrawalBody));

export const signWithdrawalBodyCbor = (
  privateKey: ReturnType<typeof CML.PrivateKey.from_bech32>,
  bodyCbor: string,
): WithdrawalSignature => {
  const message = withdrawalSigningMessageCbor(bodyCbor);
  const publicKey = privateKey.to_public();
  const signature = privateKey.sign(message);
  return [
    Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
    signature.to_hex(),
  ];
};

export const publicKeyHashFromWithdrawalSignature = (
  signature: WithdrawalSignature,
): string => {
  const [publicKeyHex] = signature;
  if (
    typeof publicKeyHex !== "string" ||
    publicKeyHex.length !== PUBLIC_KEY_HEX_LENGTH ||
    !isHex(publicKeyHex)
  ) {
    throw new Error("Withdrawal signature public key must be 32-byte hex.");
  }
  return CML.PublicKey.from_bytes(Buffer.from(publicKeyHex, "hex"))
    .hash()
    .to_hex();
};

export const verifyWithdrawalSignature = (
  body: WithdrawalBody,
  signature: WithdrawalSignature,
  expectedOwnerHash: string,
): WithdrawalSignatureVerification =>
  verifyWithdrawalSignatureCbor(
    LucidData.to(body, WithdrawalBody),
    signature,
    expectedOwnerHash,
  );

export const verifyWithdrawalSignatureCbor = (
  bodyCbor: string,
  signature: WithdrawalSignature,
  expectedOwnerHash: string,
): WithdrawalSignatureVerification => {
  const [publicKeyHex, signatureHex] = signature;
  if (
    typeof publicKeyHex !== "string" ||
    publicKeyHex.length !== PUBLIC_KEY_HEX_LENGTH ||
    !isHex(publicKeyHex)
  ) {
    return {
      valid: false,
      reason: "malformed_public_key",
    };
  }
  if (
    typeof signatureHex !== "string" ||
    signatureHex.length !== SIGNATURE_HEX_LENGTH ||
    !isHex(signatureHex)
  ) {
    return {
      valid: false,
      reason: "malformed_signature",
    };
  }

  try {
    const publicKey = CML.PublicKey.from_bytes(
      Buffer.from(publicKeyHex, "hex"),
    );
    const publicKeyHash = publicKey.hash().to_hex();
    if (publicKeyHash.toLowerCase() !== expectedOwnerHash.toLowerCase()) {
      return {
        valid: false,
        reason: "owner_hash_mismatch",
        publicKeyHash,
      };
    }
    const verified = publicKey.verify(
      withdrawalSigningMessageCbor(bodyCbor),
      CML.Ed25519Signature.from_hex(signatureHex),
    );
    return verified
      ? { valid: true, publicKeyHash }
      : {
          valid: false,
          reason: "invalid_signature",
          publicKeyHash,
        };
  } catch (cause) {
    return {
      valid: false,
      reason: cause instanceof Error ? cause.message : String(cause),
    };
  }
};
