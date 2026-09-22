import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/** Verify the actual included apply receipt even if a proof already spent its output.
 * The caller supplies bytes from the independent native recorder; locally signed
 * bytes alone cannot establish whether the ledger applied the valid branch. */
export const verifyPublishedDaAttestationReceipt = async (input: {
  txHash: string;
  signedCbor: string;
  readConfirmedTransaction?: (txHash: string) => Promise<{ cbor: string }>;
  readLiveAttestation(): Promise<SDK.StateQueueNode["da_attestation"]>;
  stateQueueAddress: string;
  stateQueueUnit: string;
  headerHash: string;
  bondAssetName: string;
}): Promise<void> => {
  if (input.readConfirmedTransaction === undefined) {
    if ((await input.readLiveAttestation()) === SDK.NO_DA_ATTESTATION)
      throw new Error(
        "Accepted DA attestation did not attach to the state queue",
      );
    return;
  }
  const receipt = await input.readConfirmedTransaction(input.txHash);
  const signed = CML.Transaction.from_cbor_hex(input.signedCbor);
  const included = CML.Transaction.from_cbor_hex(receipt.cbor);
  if (
    !included.is_valid() ||
    CML.hash_transaction(signed.body()).to_hex() !== input.txHash ||
    CML.hash_transaction(included.body()).to_hex() !== input.txHash
  )
    throw new Error(
      "DA apply receipt does not confirm the exact valid signed body",
    );
  const outputs = included.body().outputs();
  const matches = Array.from({ length: outputs.len() }, (_, outputIndex) => ({
    ...coreToTxOutput(outputs.get(outputIndex)),
    txHash: input.txHash,
    outputIndex,
  })).filter(
    (output) =>
      output.address === input.stateQueueAddress &&
      output.assets[input.stateQueueUnit] === 1n,
  );
  if (matches.length !== 1)
    throw new Error("DA apply receipt omitted the unique state queue output");
  const datum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(matches[0]!),
  );
  const node = await Effect.runPromise(
    SDK.getStateQueueNodeFromStateQueueDatum(datum),
  );
  if (
    datum.key === "Empty" ||
    datum.key.Key.key !== input.headerHash ||
    typeof node.da_attestation !== "object" ||
    !("Attested" in node.da_attestation) ||
    node.da_attestation.Attested.da_bond_asset_name !== input.bondAssetName
  )
    throw new Error("DA apply receipt did not attach the expected attestation");
};
