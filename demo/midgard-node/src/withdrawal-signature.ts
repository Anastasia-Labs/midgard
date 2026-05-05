import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

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

export const withdrawalSigningMessage = (
  body: SDK.WithdrawalBody,
): Uint8Array => {
  const bodyCbor = Buffer.from(LucidData.to(body, SDK.WithdrawalBody), "hex");
  const preimage = Buffer.concat([
    Buffer.from(WITHDRAWAL_SIGNATURE_DOMAIN, "utf8"),
    bodyCbor,
  ]);
  return blake2b(preimage, { dkLen: 32 });
};

export const signWithdrawalBody = (
  privateKey: ReturnType<typeof CML.PrivateKey.from_bech32>,
  body: SDK.WithdrawalBody,
): SDK.WithdrawalSignature => {
  const message = withdrawalSigningMessage(body);
  const publicKey = privateKey.to_public();
  const signature = privateKey.sign(message);
  return [
    Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
    signature.to_hex(),
  ];
};

export const publicKeyHashFromWithdrawalSignature = (
  signature: SDK.WithdrawalSignature,
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
  body: SDK.WithdrawalBody,
  signature: SDK.WithdrawalSignature,
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
      withdrawalSigningMessage(body),
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
