import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { commitCountedRootProgram, ROOT_DOMAINS } from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { prepareL2TxMistagFromTransactions } from "../src/index.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type NodeTransactionPayload,
  transactionSourceTrieItem,
} from "../src/prepare-double-spend.js";

const payload = (
  validity: "TxIsValid" | "TxIsInvalid",
): NodeTransactionPayload => {
  const tx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 7n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  return {
    nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
    txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
  };
};

const expectedRoot = async (
  transactions: readonly NodeTransactionPayload[],
) => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  return Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
};

describe("prepare l2-tx-mistag", () => {
  it("selects a committed code-1 normal leaf and refuses code 0", async () => {
    const mistagged = payload("TxIsInvalid");
    const prepared = await prepareL2TxMistagFromTransactions({
      headerHash: "aa".repeat(28),
      transactions: [mistagged],
      expectedTransactionsRoot: await expectedRoot([mistagged]),
    });
    expect(prepared.tx.nodeTxId).toBe(mistagged.nodeTxId);
    expect(prepared.tx.committedValidityCode).toBe(1n);
    expect(prepared.tx.txInclusion.txMembershipProofCbor).not.toBeUndefined();

    const honest = payload("TxIsValid");
    await expect(
      prepareL2TxMistagFromTransactions({
        headerHash: "bb".repeat(28),
        transactions: [honest],
        expectedTransactionsRoot: await expectedRoot([honest]),
      }),
    ).rejects.toThrow(/No code-1 normal transaction leaf/u);
  });
});
