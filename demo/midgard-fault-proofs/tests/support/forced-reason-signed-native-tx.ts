import {
  computeMidgardNativeTxId,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeAddressWitnessPreimage,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

/**
 * One key the forced-coordinate suites spend from. A spend input resolves to
 * an output at this key's enterprise address and the transaction carries its
 * signature, so Phase B passes that input and reaches the rule under test.
 */
const KEY_BYTES = Buffer.alloc(32, 0x3c);

const verificationKey = (): string => {
  const key = CML.PrivateKey.from_normal_bytes(KEY_BYTES);
  try {
    return Buffer.from(key.to_public().to_raw_bytes()).toString("hex");
  } finally {
    key.free();
  }
};

/** A canonical output at the signing key's enterprise address. */
export const signedKeyOutput = (lovelace = 2_000_000n): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60]),
      Buffer.from(missingSignatureVkeyHash(verificationKey()), "hex"),
    ]),
    value: { lovelace, assets: new Map() },
  });

/** `unsigned` with the signing key's one address witness over its id. */
export const signNativeTx = (
  unsigned: MidgardNativeTxFull,
): MidgardNativeTxFull => {
  const key = CML.PrivateKey.from_normal_bytes(KEY_BYTES);
  try {
    const signature = Buffer.from(
      key.sign(computeMidgardNativeTxId(unsigned)).to_raw_bytes(),
    ).toString("hex");
    return materializeMidgardNativeTxFromCanonical({
      version: unsigned.version,
      validity: unsigned.validity,
      body: unsigned.body,
      witnessSet: {
        ...unsigned.witnessSet,
        addrTxWitsPreimageCbor: encodeAddressWitnessPreimage([
          { verification_key: verificationKey(), signature },
        ]),
      },
    });
  } finally {
    key.free();
  }
};
