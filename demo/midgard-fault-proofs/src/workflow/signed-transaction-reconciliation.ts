import { CML } from "@lucid-evolution/lucid";

export type SignedWorkflowTransaction = Readonly<{
  transactionHash: string;
  signedTransactionCborHex: string;
}>;

export const inspectSignedWorkflowTransaction = (
  input: SignedWorkflowTransaction,
) => {
  if (
    !/^[0-9a-f]{64}$/u.test(input.transactionHash) ||
    !/^(?:[0-9a-f]{2})+$/u.test(input.signedTransactionCborHex)
  )
    throw new Error(
      "Workflow recovery requires exact canonical signed transaction bytes",
    );
  const transaction = CML.Transaction.from_cbor_hex(
    input.signedTransactionCborHex,
  );
  const body = transaction.body();
  if (
    !transaction.is_valid() ||
    CML.hash_transaction(body).to_hex() !== input.transactionHash ||
    transaction.to_cbor_hex() !== input.signedTransactionCborHex
  )
    throw new Error(
      "Workflow recovery signed bytes differ from their durable transaction identity",
    );
  const ordinaryInputs = body.inputs();
  if (ordinaryInputs.len() === 0)
    throw new Error("Workflow recovery transaction has no ordinary inputs");
  const inputs = new Set<string>();
  for (const group of [
    ordinaryInputs,
    body.collateral_inputs(),
    body.reference_inputs(),
  ]) {
    if (group === undefined) continue;
    for (let index = 0; index < group.len(); index += 1) {
      const item = group.get(index);
      inputs.add(
        `${item.transaction_id().to_hex()}#${item.index().toString()}`,
      );
    }
  }
  return {
    transaction,
    body,
    inputOutRefs: [...inputs].sort(),
    expiresAtSlot: body.ttl(),
    validFromSlot: body.validity_interval_start(),
  };
};
