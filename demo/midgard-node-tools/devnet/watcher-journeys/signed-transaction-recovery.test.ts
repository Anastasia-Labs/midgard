import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  classifySignedTransaction,
  signedTransactionFacts,
} from "./signed-transaction-recovery.js";

const input = `${"aa".repeat(32)}#1`;
const signed = (ttl?: bigint) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("aa".repeat(32)), 1n),
  );
  const outputs = CML.TransactionOutputList.new();
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex("11".repeat(28))),
  ).to_address();
  for (let index = 0; index < 2; index++)
    outputs.add(
      CML.TransactionOutput.new(address, CML.Value.from_coin(2_000_000n)),
    );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  if (ttl !== undefined) body.set_ttl(ttl);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    txHash: CML.hash_transaction(transaction.body()).to_hex(),
    cbor: transaction.to_cbor_hex(),
  };
};

describe("signed transaction facts", () => {
  it("binds the recorded hash and lists inputs, outputs and TTL", () => {
    const { txHash, cbor } = signed(500n);
    expect(signedTransactionFacts(txHash, cbor)).toEqual({
      inputs: [input],
      outputs: [`${txHash}#0`, `${txHash}#1`],
      ttl: 500n,
    });
  });

  it("refuses bytes whose identity differs from the recorded hash", () => {
    const { cbor } = signed();
    expect(() => signedTransactionFacts("bb".repeat(32), cbor)).toThrow(
      "changed their identity",
    );
  });
});

describe("signed transaction classification", () => {
  const { txHash, cbor } = signed(500n);
  const facts = signedTransactionFacts(txHash, cbor);
  const classify = (
    present: string[],
    tipSlotBefore: number,
    includedThrough?: (slot: number) => boolean | undefined,
  ) =>
    classifySignedTransaction({
      facts,
      present: new Set(present),
      tipSlotBefore,
      tipSlotAfter: tipSlotBefore + 1,
      ...(includedThrough === undefined ? {} : { includedThrough }),
    }).status;

  it("is included when an output is in the ledger or the recorder saw it", () => {
    expect(classify([`${txHash}#1`], 10)).toBe("included");
    expect(classify([], 10, () => true)).toBe("included");
  });

  it("expires only once the tip before the read reached the TTL", () => {
    expect(classify([input], 499)).toBe("rebroadcast");
    expect(classify([input], 500)).toBe("expired");
  });

  it("settles a spent input only through the recorder", () => {
    expect(classify([], 10)).toBe("pending");
    expect(classify([], 10, () => undefined)).toBe("pending");
    expect(classify([], 10, () => false)).toBe("invalidated");
  });

  it("rebroadcasts without a TTL while every input is unspent", () => {
    const open = signed();
    expect(
      classifySignedTransaction({
        facts: signedTransactionFacts(open.txHash, open.cbor),
        present: new Set([input]),
        tipSlotBefore: 1_000_000,
        tipSlotAfter: 1_000_000,
      }).status,
    ).toBe("rebroadcast");
  });
});
