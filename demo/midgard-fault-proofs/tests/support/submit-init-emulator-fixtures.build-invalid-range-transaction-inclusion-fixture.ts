import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import {
  EMPTY_SPEND_INPUTS_HASH,
  invalidRangeViolationReason,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  insertAdversarialMembershipSiblings,
  type MembershipProofShape,
  membershipProofShape,
} from "./submit-init-emulator-fixtures.build-transaction-inclusion-fixture.js";
import {
  compactTxEntry,
  outputReferenceCbor,
  type TransactionInclusionEntry,
  tx1InputsPreimage,
} from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

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
