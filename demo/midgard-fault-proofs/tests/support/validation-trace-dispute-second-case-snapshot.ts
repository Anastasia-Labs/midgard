/** A raw L1 view of a removed block rewritten so a second case stands on
 * the block: the outputs of the header's computation thread also carry
 * another case's token. */
import { CML, toUnit } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type {
  admitFraudProofRawL1Snapshot,
  FraudProofRawL1FamilyDefinition,
} from "../../src/workflow/index.js";
import { rebodied } from "./validation-trace-dispute-funded-move-walk.js";

export type Snapshot = ReturnType<typeof admitFraudProofRawL1Snapshot>;

/** `admitted` with a second case on the block: the outputs of the header's
 * computation thread also carry another case's token. */
export const withSecondCase = (
  admitted: Snapshot,
  definition: FraudProofRawL1FamilyDefinition,
): Snapshot => {
  const threadUnit = toUnit(
    definition.computationThread.policyId,
    `${definition.categoryId}${definition.headerHash}`,
  );
  const hashes = new Set(
    admitted.history.find(({ unit }) => unit === threadUnit)!.transactionHashes,
  );
  let carried = 0;
  const other = CML.MultiAsset.new();
  other.set(
    CML.ScriptHash.from_hex(definition.lostDefense!.casePolicyId),
    CML.AssetName.from_hex("ab".repeat(28)),
    1n,
  );
  const transactions = admitted.transactions.map((transaction) => {
    if (!hashes.has(transaction.txHash)) return transaction;
    const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
    const outputs = CML.TransactionOutputList.new();
    for (let index = 0; index < body.outputs().len(); index += 1) {
      const output = body.outputs().get(index);
      const held = output.amount().multi_asset();
      const policy = CML.ScriptHash.from_hex(threadUnit.slice(0, 56));
      const name = CML.AssetName.from_hex(threadUnit.slice(56));
      const threadHeld = held?.get(policy, name) === 1n;
      if (threadHeld) carried += 1;
      outputs.add(
        threadHeld
          ? CML.TransactionOutput.new(
              output.address(),
              output.amount().checked_add(CML.Value.new(0n, other)),
              output.datum(),
              output.script_ref(),
            )
          : output,
      );
    }
    return {
      ...transaction,
      bodyCbor: rebodied(body, { outputs }).to_canonical_cbor_hex(),
    };
  });
  expect(carried).toBeGreaterThan(0);
  return { ...admitted, transactions };
};
