import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  EMPTY_SPEND_INPUTS_HASH,
  invalidRangeViolationReason,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  h32,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
  trieRootHex,
} from "../submit-init-emulator-shared.js";
import {
  insertAdversarialMembershipSiblings,
  MembershipProofShape,
  membershipProofShape,
} from "./membership.js";
import {
  compactTxEntry,
  outputReferenceCbor,
  spendInputsOfCardinality,
  type TestOutputReference,
  type TransactionInclusionEntry,
  tx1InputsPreimage,
  tx2InputsPreimage,
} from "./transaction-material.js";

export const buildInvalidRangeTransactionInclusionFixture = async ({
  blockSlot,
  adversarialBranchLevels = 0,
}: {
  readonly blockSlot: bigint;
  readonly adversarialBranchLevels?: number;
}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
  readonly badTxMembershipProof: MembershipProofShape;
  readonly normalizedValidityRange: ReturnType<
    typeof normalizeNativeTxValidityRange
  >;
  readonly violationReason: NonNullable<
    ReturnType<typeof invalidRangeViolationReason>
  >;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [outputReferenceCbor(tx1InputsPreimage[0]!)],
    fee: 3n,
    referenceByte: "41",
    outputByte: "42",
    witnessByte: "43",
    validityIntervalStart: blockSlot + 1n,
    validityIntervalEnd: blockSlot + 101n,
  });
  const badTx = compactTxEntry(badNativeTx);
  const badTxSourceCbor = l2TransactionSourceCborV1(badNativeTx);
  const normalizedValidityRange = normalizeNativeTxValidityRange(
    badTx.nativeTx.body,
  );
  const violationReason = invalidRangeViolationReason({
    blockSlot,
    normalizedRange: normalizedValidityRange,
  });
  if (violationReason === null) {
    throw new Error(
      "Invalid-range fixture transaction does not exclude the block slot.",
    );
  }

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  const siblingCount = await insertAdversarialMembershipSiblings({
    trie,
    targets: [{ key: Buffer.from(badTx.nativeTxId, "hex"), domain: 0x0c01 }],
    branchLevels: adversarialBranchLevels,
  });
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: BigInt(1 + siblingCount),
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          badNativeTx.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: badTxSourceCbor,
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
    badTxMembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(badTx.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
    normalizedValidityRange,
    violationReason,
  };
};

// Zero-input fixture: a bad L2 tx that spends nothing at all, violating the
// "at least one input" ledger rule. Its `spend_inputs_hash` is the hash of the
// empty definite-length CBOR array, which is precisely the constant step-02
// compares against.
export const buildZeroInputTransactionInclusionFixture = async ({
  adversarialBranchLevels = 0,
}: {
  readonly adversarialBranchLevels?: number;
} = {}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
  readonly badTxMembershipProof: MembershipProofShape;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 5n,
    referenceByte: "51",
    outputByte: "52",
    witnessByte: "53",
  });
  const badTx = compactTxEntry(badNativeTx);
  const badTxSourceCbor = l2TransactionSourceCborV1(badNativeTx);

  if (
    !nativeTxBodyHasZeroInputViolation({ txBody: badTx.nativeTx.body }) ||
    badTx.spendInputCbors.length !== 0
  ) {
    throw new Error(
      "Zero-input fixture transaction does not spend an empty input list.",
    );
  }
  expect(badTx.nativeTx.body.spend_inputs_hash).toBe(EMPTY_SPEND_INPUTS_HASH);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  const siblingCount = await insertAdversarialMembershipSiblings({
    trie,
    targets: [{ key: Buffer.from(badTx.nativeTxId, "hex"), domain: 0x0e01 }],
    branchLevels: adversarialBranchLevels,
  });
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: BigInt(1 + siblingCount),
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          badNativeTx.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: badTxSourceCbor,
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
    badTxMembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(badTx.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
  };
};

export const buildTransactionInclusionFixture = async ({
  adversarialBranchLevels = 0,
  spendInputCardinality,
  emptyAddressWitnesses = false,
  canonicalTransactions,
  honestBlock = false,
}: {
  readonly adversarialBranchLevels?: number;
  readonly canonicalTransactions?: readonly [
    MidgardNativeTxFull,
    MidgardNativeTxFull,
  ];
  /**
   * Use canonical empty address-witness fields when this block also drives a
   * real field-opening family. The legacy 32-byte marker is useful only as a
   * distinct hash preimage; it is not a canonical address-witness item.
   */
  readonly emptyAddressWitnesses?: boolean;
  /**
   * How many inputs each conflicting transaction spends. The default is the
   * fixture's minimal two; larger values drive the spend-input preimage
   * cardinality axis (finding Q1X-F6) with the double-spent input last.
   */
  readonly spendInputCardinality?: number;
  /**
   * Give tx2 an input of its own in place of the one tx1 also spends, so the
   * two transactions spend disjoint inputs, as in a valid block.
   */
  readonly honestBlock?: boolean;
} = {}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly tx1: TransactionInclusionEntry;
  readonly tx2: TransactionInclusionEntry;
  readonly tx1Full: MidgardNativeTxFull;
  readonly tx2Full: MidgardNativeTxFull;
  readonly tx1InputsPreimage: readonly TestOutputReference[];
  readonly tx2InputsPreimage: readonly TestOutputReference[];
  readonly tx1SpendInputCbors: readonly string[];
  readonly tx2SpendInputCbors: readonly string[];
  readonly tx1MembershipProof: MembershipProofShape;
  readonly tx2MembershipProof: MembershipProofShape;
}> => {
  // The double-spent input is the one both transactions carry, and it sits
  // last so the selection walk is at its longest on both sides.
  const doubleSpentInput = tx1InputsPreimage[1]!;
  const tx1Inputs =
    spendInputCardinality === undefined
      ? tx1InputsPreimage
      : spendInputsOfCardinality({
          selected: doubleSpentInput,
          cardinality: spendInputCardinality,
          domain: 0x0a01,
        });
  const tx2SharedInputs =
    spendInputCardinality === undefined
      ? tx2InputsPreimage
      : spendInputsOfCardinality({
          selected: doubleSpentInput,
          cardinality: spendInputCardinality,
          domain: 0x0a02,
        });
  const tx2Inputs = honestBlock
    ? tx2SharedInputs.map((input) =>
        input === doubleSpentInput
          ? { transactionId: h32("b2"), outputIndex: 1n }
          : input,
      )
    : tx2SharedInputs;
  const tx1Native =
    canonicalTransactions?.[0] ??
    makeNativeTx({
      spendInputCbors: tx1Inputs.map(outputReferenceCbor),
      fee: 0n,
      referenceByte: "13",
      outputByte: "14",
      ...(emptyAddressWitnesses ? {} : { witnessByte: "20" }),
    });
  const tx2Native =
    canonicalTransactions?.[1] ??
    makeNativeTx({
      spendInputCbors: tx2Inputs.map(outputReferenceCbor),
      fee: 1n,
      referenceByte: "23",
      outputByte: "24",
      ...(emptyAddressWitnesses ? {} : { witnessByte: "30" }),
    });
  const tx1 = compactTxEntry(tx1Native);
  const tx2 = compactTxEntry(tx2Native);
  const tx1SourceCbor = l2TransactionSourceCborV1(tx1Native);
  const tx2SourceCbor = l2TransactionSourceCborV1(tx2Native);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [tx1, tx2]) {
    await trie.insert(
      Buffer.from(entry.nativeTxId, "hex"),
      Buffer.from(entry === tx1 ? tx1SourceCbor : tx2SourceCbor, "hex"),
    );
  }
  const siblingCount = await insertAdversarialMembershipSiblings({
    trie,
    targets: [
      { key: Buffer.from(tx1.nativeTxId, "hex"), domain: 0x0a01 },
      { key: Buffer.from(tx2.nativeTxId, "hex"), domain: 0x0a02 },
    ],
    branchLevels: adversarialBranchLevels,
  });
  const withProof = async (
    entry: typeof tx1,
  ): Promise<TransactionInclusionEntry> => {
    const txKey = Buffer.from(entry.nativeTxId, "hex");
    const proof = await trie.prove(txKey);
    return {
      inclusion: {
        nativeTxId: entry.nativeTxId,
        nativeTx: entry.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: entry === tx1 ? tx1SourceCbor : tx2SourceCbor,
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: entry.nativeTx,
      nativeTxId: entry.nativeTxId,
      spendInputCbors: entry.spendInputCbors,
    };
  };
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: BigInt(2 + siblingCount),
    tx1: await withProof(tx1),
    tx2: await withProof(tx2),
    tx1Full: tx1Native,
    tx2Full: tx2Native,
    tx1InputsPreimage: tx1Inputs,
    tx2InputsPreimage: tx2Inputs,
    tx1SpendInputCbors: tx1.spendInputCbors,
    tx2SpendInputCbors: tx2.spendInputCbors,
    tx1MembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(tx1.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
    tx2MembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(tx2.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount,
    }),
  };
};
