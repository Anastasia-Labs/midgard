import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardNativeTxCompact } from "@al-ft/midgard-core";

import {
  buildNonMembershipProof,
  type TrieEntry,
} from "../../../src/ne-proofs.js";
import { type NeInputPreimageEntry } from "../../../src/prepare-non-existent-input.js";
import { parseSubmitStep01TxInclusion } from "../legacy-submit-emulator.js";
import {
  h32,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
  trieRootHex,
} from "../submit-init-emulator-shared.js";
import {
  ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE,
  adversarialMembershipSiblingKeys,
  type MembershipProofShape,
  membershipProofShape,
} from "./membership.js";
import {
  compactTxEntry,
  outputReferenceCbor,
  spendInputsOfCardinality,
  type TestOutputReference,
} from "./transaction-material.js";

// Non-existent-input fixture: a bad L2 tx spends an input whose producing
// transaction never existed. The transactions trie is keyed by the raw native
// tx id (matching the node); the ledger non-membership is proven against the
// empty prev-ledger (`EMPTY_MERKLE_TREE_ROOT`, the genesis confirmed-state root
// the setup block builds on); and the phantom input's producing tx id is proven
// absent from the block's transactions.
export const buildNonExistentInputFixture = async ({
  adversarialBranchLevels = 0,
  spendInputCardinality,
}: {
  readonly adversarialBranchLevels?: number;
  /**
   * How many inputs the challenged transaction spends. The default is the
   * fixture's minimal one; larger values drive the spend-input preimage
   * cardinality axis (finding Q1X-F6) with the phantom input last.
   */
  readonly spendInputCardinality?: number;
} = {}): Promise<{
  readonly nativeTx: ReturnType<typeof makeNativeTx>;
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  readonly inputsPreimage: readonly NeInputPreimageEntry[];
  readonly badInputIndex: bigint;
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly missingInputTxId: string;
  readonly nativeTxId: string;
  readonly badTxMembershipProof: MembershipProofShape;
  readonly txsNonMembershipProofCborBytes: number;
}> => {
  const phantomOutRef: TestOutputReference = {
    transactionId: h32("de"),
    outputIndex: 0n,
  };
  // The phantom input sits last, which is the worst case for the selection
  // walk; the whole preimage is re-hashed either way.
  const badTxInputs =
    spendInputCardinality === undefined
      ? [phantomOutRef]
      : spendInputsOfCardinality({
          selected: phantomOutRef,
          cardinality: spendInputCardinality,
          domain: 0x0b03,
        });
  const badTxNative = makeNativeTx({
    spendInputCbors: badTxInputs.map(outputReferenceCbor),
    fee: 0n,
    referenceByte: "e3",
    outputByte: "e4",
    witnessByte: "e5",
  });
  const badTx = compactTxEntry(badTxNative);
  const badTxCompactCbor = encodeMidgardNativeTxCompact(badTxNative.compact);
  const badTxSourceCbor = l2TransactionSourceCborV1(badTxNative);

  // A second, well-formed L2 tx so the transactions trie is non-trivial (proofs
  // for a single-element trie are degenerate).
  const otherTxNative = makeNativeTx({
    spendInputCbors: [
      outputReferenceCbor({ transactionId: h32("c1"), outputIndex: 0n }),
    ],
    fee: 1n,
    referenceByte: "c3",
    outputByte: "c4",
    witnessByte: "c5",
  });
  const otherTx = compactTxEntry(otherTxNative);
  const otherTxSourceCbor = l2TransactionSourceCborV1(otherTxNative);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(otherTx.nativeTxId, "hex"),
    Buffer.from(otherTxSourceCbor, "hex"),
  );
  // Both proof-carrying legs of this family are pushed together: the step-01
  // membership proof of the challenged transaction, and the step-04
  // non-membership proof of the phantom input's producing transaction id.
  const adversarialSiblings =
    adversarialBranchLevels === 0
      ? []
      : [
          ...adversarialMembershipSiblingKeys({
            targetKey: Buffer.from(badTx.nativeTxId, "hex"),
            branchLevels: adversarialBranchLevels,
            domain: 0x0b01,
          }),
          ...adversarialMembershipSiblingKeys({
            targetKey: Buffer.from(phantomOutRef.transactionId, "hex"),
            branchLevels: adversarialBranchLevels,
            domain: 0x0b02,
          }),
        ];
  for (const key of adversarialSiblings) {
    await trie.insert(key, ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE);
  }
  const transactionsRoot = trieRootHex(trie);
  const membershipProof = await trie.prove(
    Buffer.from(badTx.nativeTxId, "hex"),
  );

  const txsEntries: TrieEntry[] = [
    {
      key: Buffer.from(badTx.nativeTxId, "hex"),
      value: Buffer.from(badTxSourceCbor, "hex"),
    },
    {
      key: Buffer.from(otherTx.nativeTxId, "hex"),
      value: Buffer.from(otherTxSourceCbor, "hex"),
    },
    ...adversarialSiblings.map((key) => ({
      key,
      value: ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE,
    })),
  ];
  const txsNonMembershipProofCbor = await buildNonMembershipProof(
    txsEntries,
    Buffer.from(phantomOutRef.transactionId, "hex"),
  );
  const ledgerNonMembershipProofCbor = await buildNonMembershipProof(
    [],
    outputReferenceCbor(phantomOutRef),
  );

  return {
    nativeTx: badTxNative,
    transactionsRoot,
    l2TransactionCount: BigInt(2 + adversarialSiblings.length),
    badTxMembershipProof: await membershipProofShape({
      trie,
      key: Buffer.from(badTx.nativeTxId, "hex"),
      branchLevels: adversarialBranchLevels,
      siblingCount: adversarialSiblings.length,
    }),
    txsNonMembershipProofCborBytes: txsNonMembershipProofCbor.length / 2,
    inclusion: parseSubmitStep01TxInclusion({
      nativeTxId: badTx.nativeTxId,
      nativeTx: badTx.nativeTx,
      nativeTxCompactCbor: badTxCompactCbor.toString("hex"),
      l2TransactionSourceCbor: badTxSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: membershipProof.toCBOR().toString("hex"),
    }),
    inputsPreimage: badTxInputs.map((input) => ({
      txId: input.transactionId,
      index: input.outputIndex,
    })),
    badInputIndex: BigInt(badTxInputs.length - 1),
    ledgerNonMembershipProofCbor,
    txsNonMembershipProofCbor,
    missingInputTxId: phantomOutRef.transactionId,
    nativeTxId: badTx.nativeTxId,
  };
};
