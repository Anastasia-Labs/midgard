import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import { CML } from "@lucid-evolution/lucid";

export const availabilityOperationIntent = () => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    0n,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    signedCbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
    spentOutRefs: [],
    collateralOutRefs: [],
  } as unknown as AvailabilityOperationIntent;
};
export const deferredAvailabilityRead = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};
