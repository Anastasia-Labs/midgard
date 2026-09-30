import { createHash } from "node:crypto";

import { compareOutRefs, parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { type AddressData } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

/**
 * Ordinary user-event order bytes and private local publication for synthetic
 * local blocks. The transactions below are semantic unit bytes: they make no
 * ledger-acceptance or public-chain inclusion claim. Replay authority comes
 * only from the watcher's own local user-event publication, which is the
 * production originating-authority path for W25.
 */

export const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

export const h32 = (byte: string): string => byte.repeat(32);

export const addressHex = (address: AddressData): string => {
  const credential = address.paymentCredential;
  if (address.stakeCredential !== null || !("ScriptCredential" in credential))
    throw new Error("fixture needs an enterprise script address");
  return `70${credential.ScriptCredential[0]}`;
};

export const transactionInput = (outRef: string) => {
  const [transactionId, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(transactionId!),
    BigInt(index!),
  );
};

export const ledgerReferenceIndex = (
  inputs: CML.TransactionInputList,
  outRef: string,
): bigint => {
  const ordered = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  const target = parseOutRefLabel(outRef);
  const index = ordered.findIndex(
    (input) => compareOutRefs(input, target) === 0,
  );
  if (index < 0) throw new Error("Missing fixture reference input");
  return BigInt(index);
};

export const syntheticUserEventTransaction = (
  body: CML.TransactionBody,
  values: readonly Readonly<{
    tag: CML.RedeemerTag;
    index: bigint;
    cbor: string;
  }>[],
  preserveDataEncoding = false,
): string => {
  const witness = CML.TransactionWitnessSet.new();
  const redeemers = CML.LegacyRedeemerList.new();
  for (const value of values)
    redeemers.add(
      CML.LegacyRedeemer.new(
        value.tag,
        value.index,
        CML.PlutusData.from_cbor_hex(value.cbor),
        CML.ExUnits.new(0n, 0n),
      ),
    );
  if (values.length > 0) {
    witness.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(Buffer.alloc(32, 0x6a)),
    );
  }
  const complete = CML.Transaction.new(body, witness, true, undefined);
  return preserveDataEncoding
    ? complete.to_cbor_hex()
    : complete.to_canonical_cbor_hex();
};
