import { CML } from "@lucid-evolution/lucid";

import { uncoveredSupersededAttempts } from "../../src/funding/sqlite-prover-funding-reservation-store.superseded-exclusion.js";
import { signedTransition } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

/** A recorded attempt spending `inputHashes` (output 0 of each) and any
 * further `inputOutRefs`, such as another attempt's outputs. */
export const attempt = (
  inputHashes: readonly string[],
  consumedOutRefs: readonly string[],
  inputOutRefs: readonly string[] = [],
) => {
  const [inputHash, ...protocolInputHashes] = inputHashes;
  const signed = signedTransition({ inputHash, protocolInputHashes });
  const body = CML.Transaction.from_cbor_hex(signed.signedTransactionCborHex)
    .body()
    .to_cbor_hex();
  if (inputOutRefs.length === 0)
    return { ...signed, consumedOutRefs } as unknown as Transition;
  // Spend outputs of another attempt as well, by rebuilding the body.
  const rebuilt = CML.TransactionBody.from_cbor_hex(body);
  const inputs = rebuilt.inputs();
  for (const outRef of inputOutRefs) {
    const [txHash, index] = outRef.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(index!),
      ),
    );
  }
  const next = CML.TransactionBody.new(
    inputs,
    rebuilt.outputs(),
    rebuilt.fee(),
  );
  return {
    ...signed,
    transactionHash: CML.hash_transaction(next).to_hex(),
    signedTransactionCborHex: CML.Transaction.new(
      next,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
    consumedOutRefs,
  } as unknown as Transition;
};
export type Transition = Parameters<
  typeof uncoveredSupersededAttempts
>[0]["submissions"][number];
export const hash = (byte: string) => byte.repeat(32);
export const ref = (byte: string) => `${hash(byte)}#0`;
export const abandoned = (transition: Transition, retired = false) =>
  ({
    transition,
    handoff: { reconciliation: retired ? { retirement: {} } : {} },
  }) as unknown as Parameters<
    typeof uncoveredSupersededAttempts
  >[0]["abandoned"][number];
