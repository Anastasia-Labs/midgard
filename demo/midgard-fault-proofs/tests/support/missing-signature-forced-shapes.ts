import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  encodeAddressWitnessPreimage,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import { buildMissingSignatureSubject } from "./missing-signature-emulator.js";
export const buildMissingSignatureForcedTransaction = ({
  witnessCount = 1,
  signerCount = 1,
  forged = false,
}: { witnessCount?: number; signerCount?: number; forged?: boolean } = {}) => {
  const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 31));
  const verification_key = Buffer.from(key.to_public().to_raw_bytes()).toString(
    "hex",
  );
  const hash = missingSignatureVkeyHash(verification_key);
  const requiredSignerHashes = Array.from(
    { length: signerCount },
    (_, index) =>
      index === signerCount - 1 ? hash : index.toString(16).padStart(56, "0"),
  );
  const base = buildMissingSignatureSubject().nativeTx;
  const unsigned = materializeMidgardNativeTxFromCanonical({
    ...base,
    validity: "TxIsInvalid",
    body: {
      ...base.body,
      requiredSignersPreimageCbor: encodeCbor(
        requiredSignerHashes.map((h) => Buffer.from(h, "hex")),
      ),
    },
  });
  const transactionId = computeMidgardNativeTxId(unsigned).toString("hex");
  const signature = forged
    ? "00".repeat(64)
    : Buffer.from(
        key.sign(Buffer.from(transactionId, "hex")).to_raw_bytes(),
      ).toString("hex");
  const addrTxWits = Array.from({ length: witnessCount }, (_, index) =>
    index === witnessCount - 1
      ? { verification_key, signature }
      : {
          verification_key: index.toString(16).padStart(64, "0"),
          signature: "00".repeat(64),
        },
  );
  const transaction = materializeMidgardNativeTxFromCanonical({
    ...unsigned,
    witnessSet: {
      ...unsigned.witnessSet,
      addrTxWitsPreimageCbor: encodeAddressWitnessPreimage(addrTxWits),
    },
  });
  const source = deriveMidgardNativeTxProofSource(transaction);
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    transaction.witnessSet,
  );
  return {
    transactionId,
    transaction,
    requiredSignerHashes,
    addrTxWits,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    witnessSetCompact: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
  };
};
