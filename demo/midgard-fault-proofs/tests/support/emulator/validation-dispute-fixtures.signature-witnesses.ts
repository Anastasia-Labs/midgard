import { encodeCbor } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

/** Valid signatures with distinct seed keys even beyond the one-byte boundary. */
export const signedAddressWitnessesCbor = (
  transactionId: Buffer,
  spendingKey: CML.PrivateKey,
  count: number,
): Buffer =>
  encodeCbor(
    [
      spendingKey,
      ...Array.from({ length: count - 1 }, (_, index) => {
        const seed = Buffer.alloc(32);
        seed.writeUInt32BE(index + 1, 28);
        return CML.PrivateKey.from_normal_bytes(seed);
      }),
    ].map((key) =>
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(transactionId),
          key,
        ).to_cbor_bytes(),
      ),
    ),
  );
