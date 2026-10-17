import {
  ledgerSignatureVerifierUnavailable,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { lucidDataFromCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
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

/**
 * The message `withdrawal_signature_is_valid_v1` checks, built from the body's
 * own bytes: the body exactly as `cbor.serialise` renders it on-chain (definite
 * asset maps, which Lucid's own encoder does not emit). The reader is
 * iterative, so a datum nested as deep as a transaction allows still reads.
 */
const withdrawalSignedMessageCbor = (rawBodyCbor: string): Uint8Array =>
  withdrawalSignedMessageOfBody(onChainBodyBytes(rawBodyCbor));

/** The body's bytes as `cbor.serialise` renders them; raises on non-Data. */
const onChainBodyBytes = (rawBodyCbor: string): Buffer =>
  Buffer.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(rawBodyCbor),
    "hex",
  );

/** The domain-separated hash of the body's on-chain bytes. */
const withdrawalSignedMessageOfBody = (bodyCbor: Buffer): Uint8Array => {
  const preimage = Buffer.concat([
    Buffer.from(WITHDRAWAL_SIGNATURE_DOMAIN, "utf8"),
    bodyCbor,
  ]);
  return blake2b(preimage, { dkLen: 32 });
};

export const withdrawalSigningMessageCbor = (
  rawBodyCbor: string,
): Uint8Array => {
  // Validate the structure without re-encoding the opaque destination datum.
  LucidData.from(rawBodyCbor, WithdrawalBody);
  return withdrawalSignedMessageCbor(rawBodyCbor);
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
  // `blake2b_224(verification_key)` over the raw key bytes, as the chain
  // hashes it: any 32 bytes hash, curve point or not.
  return Buffer.from(
    blake2b(Buffer.from(publicKeyHex, "hex"), { dkLen: 28 }),
  ).toString("hex");
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

  const publicKeyHash = publicKeyHashFromWithdrawalSignature(signature);
  if (publicKeyHash.toLowerCase() !== expectedOwnerHash.toLowerCase()) {
    return {
      valid: false,
      reason: "owner_hash_mismatch",
      publicKeyHash,
    };
  }
  let body: Buffer;
  try {
    // A withdrawal body, or no signature check: Data of another shape is
    // refused as the body's, however it is signed. Read without recursion
    // (as `LucidData.from` reads it), so a deeply nested datum still reads.
    LucidData.castFrom(lucidDataFromCborIterative(bodyCbor), WithdrawalBody);
    body = onChainBodyBytes(bodyCbor);
  } catch (cause) {
    // Only the read is refused here: bytes that are not a withdrawal body
    // are the body's fault. Nothing after it depends on the body's content.
    return {
      valid: false,
      reason: cause instanceof Error ? cause.message : String(cause),
    };
  }
  // The ledger's own check, as `withdrawal_signature_is_valid_v1` runs it.
  // A failure from here on (the preimage, its hash, the check itself) is a
  // fault of this process: raised, never read as an invalid signature.
  let verified: boolean;
  try {
    verified = verifyLedgerEd25519Signature(
      Buffer.from(publicKeyHex, "hex"),
      withdrawalSignedMessageOfBody(body),
      Buffer.from(signatureHex, "hex"),
    );
  } catch (cause) {
    throw ledgerSignatureVerifierUnavailable(cause);
  }
  return verified
    ? { valid: true, publicKeyHash }
    : {
        valid: false,
        reason: "invalid_signature",
        publicKeyHash,
      };
};
