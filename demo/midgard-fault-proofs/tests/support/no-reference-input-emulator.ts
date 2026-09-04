/**
 * `no-reference-input` (Q18) emulator support: the committed block fixture the
 * four-step chain disputes, and the family's reference-script publisher.
 *
 * The fault is a committed transaction that names a reference input which
 * never existed — absent from the block's `prev_utxos_root` **and** not
 * produced by any transaction the block committed. Those are two separate
 * non-membership claims, which is why the chain has a step for each:
 *
 *   * step-03 excludes the challenged out-ref (under `encode_midgard_tx_input`
 *     key bytes) from `blocks_prev_utxos_root`;
 *   * step-04 excludes the challenged out-ref's **producing transaction id**
 *     from `blocks_transactions_root`, then finalizes.
 *
 * Every block this module commits carries two transactions — the disputed
 * subject and one companion — so the raw transactions MPF is a real branch
 * rather than the degenerate single-leaf trie, and so an honest block can be
 * built by pointing the subject's reference input at the companion. That
 * honest shape is what the adversarial journey attacks: step-04's exclusion
 * key is then a key the trie genuinely holds, and no witness can make a
 * present key look absent.
 *
 * Nothing here forces a §8.4 carriage tier. Field 1 is the disputed
 * transaction's own reference-input list, so its §5.1 preimage size is a
 * property of the committed data; a caller that wants tier 2 commits a fat
 * list and `planMidgardFieldCarriageV1` selects `RawUtxo` on its own.
 */
import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { type Script, type UTxO } from "@lucid-evolution/lucid";

import {
  buildMembershipProof,
  buildNonMembershipProof,
  computeTrieRoot,
  type TrieEntry,
} from "../../src/ne-proofs.js";
import { ledgerKeyBytesHex } from "../../src/ne-submit-step-03.js";
import type { NoReferenceInputPreimageEntry } from "../../src/prepare-no-reference-input.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import { l2TransactionSourceCbor as l2TransactionSourceCborV1 } from "./emulator/native-tx.js";
import { decodingSubjectTransaction } from "./native-script-decoding-emulator.js";
import { publishPlainReferenceScriptUtxo } from "./submit-init-emulator-shared.js";

/**
 * The producing transaction id of a reference input that never existed. It is
 * deliberately not the disputed transaction's own id and not the companion's,
 * so step-04's exclusion against the block's one-branch transactions trie is a
 * genuine absence rather than an artefact of the fixture.
 */
export const NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID = "aa".repeat(32);

export const noReferenceInputOutRef = (
  txId: string,
  outputIndex: number,
): SDK.MidgardTxInput => ({
  tx_id: txId,
  output_index: BigInt(outputIndex),
});

/**
 * The block's second committed transaction. Deterministic in its fee alone, so
 * a caller can compute its id before building the subject transaction that
 * references it — which is exactly what an honest block looks like.
 */
export const noReferenceInputCompanionTx = (fee: bigint): MidgardNativeTxFull =>
  decodingSubjectTransaction({
    spendInputCbors: [
      SDK.encodeMidgardTxInputCanonical(
        noReferenceInputOutRef("c1".repeat(32), 0),
      ),
    ],
    fee,
  });

export const noReferenceInputTxId = (tx: MidgardNativeTxFull): string =>
  computeMidgardNativeTxId(tx).toString("hex");

export type NoReferenceInputFixture = {
  /** The disputed transaction: the one the block committed with a bad field 1. */
  readonly subjectTxId: string;
  /** The block's other committed transaction. */
  readonly companionTxId: string;
  /** The block's RAW transactions PHAS root, before the counted-root wrap. */
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusion: SubmitStep01TxInclusion;
  readonly nativeTxCompactCbor: string;
  /** §2.5 field 1, in committed order. */
  readonly referenceInputs: readonly SDK.MidgardTxInput[];
  readonly referenceInputsPreimage: readonly NoReferenceInputPreimageEntry[];
  readonly referenceInputItemCbors: readonly Buffer[];
  /** §5.1's envelope over the item bytes — what §8.4 partitions on. */
  readonly fieldPreimage: Buffer;
  readonly badReferenceInputIndex: bigint;
  readonly missingReferenceInput: SDK.MidgardTxInput;
  /** Non-membership of the challenged out-ref in the empty `prev_utxos_root`. */
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsEntries: readonly TrieEntry[];
  /**
   * Whether the challenged reference input's producing transaction is one the
   * block committed. `true` is an HONEST block: the reference input was
   * produced in-block, so step-04 must refuse.
   */
  readonly missingProducerIsCommitted: boolean;
  /**
   * Non-membership of the producing transaction id in the block's transactions
   * trie — `null` exactly when that id is committed, because a present key has
   * no absence witness.
   */
  readonly txsNonMembershipProofCbor: string | null;
  /**
   * The genuine MEMBERSHIP witness for the producing transaction id, present
   * only for an honest block. Adversarial material: a well-formed proof of the
   * opposite claim, which `pexcludes.exclusion.withdraw` must not accept.
   */
  readonly txsMembershipProofCbor: string | null;
};

/**
 * Commits a two-transaction block whose subject transaction names
 * `buildReferenceInputs(companionTxId)` at §2.5 field 1, and returns every
 * piece of evidence the four steps consume.
 *
 * `buildReferenceInputs` receives the companion transaction's id so a caller
 * can decide whether the challenged reference input's producer is in-block (an
 * honest commitment) or nowhere (the real fault).
 */
export const buildNoReferenceInputFixture = async ({
  buildReferenceInputs,
  badReferenceInputIndex,
  subjectFee = 13n,
  companionFee = 17n,
}: {
  readonly buildReferenceInputs: (
    companionTxId: string,
  ) => readonly SDK.MidgardTxInput[];
  readonly badReferenceInputIndex: number;
  readonly subjectFee?: bigint;
  readonly companionFee?: bigint;
}): Promise<NoReferenceInputFixture> => {
  const companionTx = noReferenceInputCompanionTx(companionFee);
  const companionTxId = noReferenceInputTxId(companionTx);
  const referenceInputs = buildReferenceInputs(companionTxId);
  const missingReferenceInput = referenceInputs[badReferenceInputIndex];
  if (missingReferenceInput === undefined) {
    throw new Error(
      `badReferenceInputIndex ${badReferenceInputIndex.toString()} is out of bounds for ${referenceInputs.length.toString()} reference inputs`,
    );
  }
  const referenceInputItemCbors = referenceInputs.map((input) =>
    Buffer.from(SDK.encodeMidgardTxInputCanonical(input)),
  );
  const subjectTx = decodingSubjectTransaction({
    spendInputCbors: [
      SDK.encodeMidgardTxInputCanonical(
        noReferenceInputOutRef("d1".repeat(32), 0),
      ),
    ],
    referenceInputCbors: referenceInputItemCbors,
    fee: subjectFee,
  });
  const subjectTxId = noReferenceInputTxId(subjectTx);
  const subjectCompactCbor = encodeMidgardNativeTxCompact(subjectTx.compact);
  // The header's normative transactions MPF commits `Data(L2TransactionSourceV1)`
  // per transaction id, not the bare compact CBOR, so the trie this fixture
  // proves against must carry the same values step-01 authenticates.
  const subjectSourceCbor = l2TransactionSourceCborV1(subjectTx);
  const companionSourceCbor = l2TransactionSourceCborV1(companionTx);
  const txsEntries: readonly TrieEntry[] = [
    {
      key: Buffer.from(subjectTxId, "hex"),
      value: Buffer.from(subjectSourceCbor, "hex"),
    },
    {
      key: Buffer.from(companionTxId, "hex"),
      value: Buffer.from(companionSourceCbor, "hex"),
    },
  ];
  const transactionsRoot = await computeTrieRoot(txsEntries);
  const txMembershipProofCbor = await buildMembershipProof(
    txsEntries,
    Buffer.from(subjectTxId, "hex"),
  );
  // The step-03 exclusion key is `encode_midgard_tx_input`'s bytes, not
  // `cbor.serialise(OutputReference)`; borrowing the submitter's own twin is
  // what keeps the fixture from proving the wrong key absent.
  const ledgerKey = Buffer.from(
    ledgerKeyBytesHex(missingReferenceInput),
    "hex",
  );
  const ledgerNonMembershipProofCbor = await buildNonMembershipProof(
    [],
    ledgerKey,
  );
  const producerKey = Buffer.from(missingReferenceInput.tx_id, "hex");
  const missingProducerIsCommitted = txsEntries.some((entry) =>
    entry.key.equals(producerKey),
  );
  return {
    subjectTxId,
    companionTxId,
    transactionsRoot,
    l2TransactionCount: BigInt(txsEntries.length),
    inclusion: parseSubmitStep01TxInclusion({
      nativeTxId: subjectTxId,
      nativeTx: nativeTxFromCoreCompact(subjectTx.compact),
      nativeTxCompactCbor: subjectCompactCbor.toString("hex"),
      l2TransactionSourceCbor: subjectSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor,
    }),
    nativeTxCompactCbor: subjectCompactCbor.toString("hex"),
    referenceInputs,
    referenceInputsPreimage: referenceInputs.map((input) => ({
      txId: input.tx_id,
      index: input.output_index,
    })),
    referenceInputItemCbors,
    fieldPreimage: encodeMidgardFieldPreimage(referenceInputItemCbors),
    badReferenceInputIndex: BigInt(badReferenceInputIndex),
    missingReferenceInput,
    ledgerNonMembershipProofCbor,
    txsEntries,
    missingProducerIsCommitted,
    txsNonMembershipProofCbor: missingProducerIsCommitted
      ? null
      : await buildNonMembershipProof(txsEntries, producerKey),
    txsMembershipProofCbor: missingProducerIsCommitted
      ? await buildMembershipProof(txsEntries, producerKey)
      : null,
  };
};

export const requireNoReferenceInputTxsNonMembershipProof = (
  fixture: NoReferenceInputFixture,
): string => {
  if (fixture.txsNonMembershipProofCbor === null) {
    throw new Error(
      "The challenged reference input was produced in-block, so no absence witness exists for its producing transaction id.",
    );
  }
  return fixture.txsNonMembershipProofCbor;
};

export const requireNoReferenceInputTxsMembershipProof = (
  fixture: NoReferenceInputFixture,
): string => {
  if (fixture.txsMembershipProofCbor === null) {
    throw new Error(
      "The challenged reference input's producing transaction is not committed, so it has no membership witness.",
    );
  }
  return fixture.txsMembershipProofCbor;
};

/**
 * Publishes all four step validators as reference scripts, per the standing
 * deployment ruling that fault-proof steps are always referenced and never
 * inline-attached. The returned tuple is positional: index `i` is step
 * `0(i+1)`, which is the order every submitter's `referenceScriptUtxo`
 * argument expects.
 */
export const publishNoReferenceInputReferenceScripts = async ({
  lucid,
  steps,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly steps: readonly SDK.SpendingValidator[];
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  if (steps.length !== 4) {
    throw new Error(
      `no-reference-input publishes a four-step chain, got ${steps.length.toString()}`,
    );
  }
  const published: UTxO[] = [];
  // Sequential: each publication consumes wallet UTxOs the next selects from.
  for (const [index, step] of steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `no-reference-input step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};
