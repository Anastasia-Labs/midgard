import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";

import {
  ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE,
  adversarialMembershipSiblingKeys,
  compactTxEntry,
  outputReferenceCbor,
  PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
  spendInputsOfCardinality,
  type TestOutputReference,
  type TransactionInclusionEntry,
  tx1InputsPreimage,
  tx2InputsPreimage,
} from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

/**
 * The deepest branch level at which the L1 envelope still admits the proof,
 * derived from a MEASURED transaction at a measured branch depth plus the
 * measured constant marginal cost of one further level.
 */
export const membershipProofBranchLevelByteCeiling = ({
  measuredTransactionBytes,
  measuredBranchLevels,
  l1MaxTxSize,
}: {
  readonly measuredTransactionBytes: number;
  readonly measuredBranchLevels: number;
  readonly l1MaxTxSize: number;
}): number =>
  measuredBranchLevels +
  Math.floor(
    (l1MaxTxSize - measuredTransactionBytes) /
      PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
  );

/**
 * The deepest branch level reachable by an adversary willing to spend `2^n`
 * digest evaluations. Forcing a branch at level `i` means finding a key whose
 * hashed path agrees with the challenged transaction's path on `i` chosen
 * nibbles, which is a fixed-target search costing ~16^i = 2^(4i).
 */
export const membershipProofBranchLevelsReachableWithWork = (
  log2Work: number,
): number => Math.floor(log2Work / 4);

export type MembershipProofShape = {
  readonly branchLevels: number;
  readonly siblingCount: number;
  readonly proofSteps: number;
  readonly proofCborBytes: number;
};

/**
 * Insert the grinded siblings of every named target into `trie` and return how
 * many keys were added. A zero `branchLevels` leaves the trie untouched, which
 * is exactly the minimal fixture the four families already measured.
 */
export const insertAdversarialMembershipSiblings = async ({
  trie,
  targets,
  branchLevels,
}: {
  readonly trie: Trie;
  readonly targets: readonly {
    readonly key: Buffer;
    readonly domain: number;
  }[];
  readonly branchLevels: number;
}): Promise<number> => {
  if (branchLevels === 0) {
    return 0;
  }
  const reserved = new Set(targets.map(({ key }) => key.toString("hex")));
  let inserted = 0;
  for (const target of targets) {
    for (const key of adversarialMembershipSiblingKeys({
      targetKey: target.key,
      branchLevels,
      domain: target.domain,
    })) {
      const label = key.toString("hex");
      if (reserved.has(label)) {
        throw new Error(
          `Grinded adversarial sibling ${label} collides with an existing trie key.`,
        );
      }
      reserved.add(label);
      await trie.insert(key, ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE);
      inserted += 1;
    }
  }
  return inserted;
};

export const membershipProofShape = async ({
  trie,
  key,
  branchLevels,
  siblingCount,
}: {
  readonly trie: Trie;
  readonly key: Buffer;
  readonly branchLevels: number;
  readonly siblingCount: number;
}): Promise<MembershipProofShape> => {
  const proof = await trie.prove(key);
  const steps = proof.toJSON() as readonly { readonly type: string }[];
  if (branchLevels > 0 && steps.length < branchLevels) {
    throw new Error(
      `Adversarial fixture asked for ${branchLevels.toString()} branch levels but the membership proof carries only ${steps.length.toString()} steps.`,
    );
  }
  return {
    branchLevels,
    siblingCount,
    proofSteps: steps.length,
    proofCborBytes: Buffer.from(proof.toCBOR()).length,
  };
};

export const buildTransactionInclusionFixture = async ({
  adversarialBranchLevels = 0,
  spendInputCardinality,
  emptyAddressWitnesses = false,
}: {
  readonly adversarialBranchLevels?: number;
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
  const tx2Inputs =
    spendInputCardinality === undefined
      ? tx2InputsPreimage
      : spendInputsOfCardinality({
          selected: doubleSpentInput,
          cardinality: spendInputCardinality,
          domain: 0x0a02,
        });
  const tx1Native = makeNativeTx({
    spendInputCbors: tx1Inputs.map(outputReferenceCbor),
    fee: 0n,
    referenceByte: "13",
    outputByte: "14",
    ...(emptyAddressWitnesses ? {} : { witnessByte: "20" }),
  });
  const tx2Native = makeNativeTx({
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
