import {
  computeHash32,
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildMembershipProof,
  buildNonMembershipProof,
  computeTrieRoot,
  type NodeTransactionPayload,
  prepareNonExistentInputFromTransactions,
} from "../src/index.js";

const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_NULL_ROOT = computeHash32(encodeCbor(null));

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      outputIndex,
    ).to_cbor_bytes(),
  );

const makeNativeTx = (inputs: readonly Buffer[]): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(inputs),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

describe("prepare-non-existent-input", () => {
  it("prepares native submit-step material that matches the on-chain proofs", async () => {
    const missingTxId = h32("de");
    const missingInputCbor = inputCbor(missingTxId, 0n);
    const badTx = makeNativeTx([missingInputCbor]);
    const payload: NodeTransactionPayload = {
      nodeTxId: computeMidgardNativeTxId(badTx).toString("hex"),
      txCbor: encodeMidgardNativeTxCanonical(badTx).toString("hex"),
    };

    const output = await prepareNonExistentInputFromTransactions({
      headerHash: "ab".repeat(28),
      transactions: [payload],
      badInputIndex: 0,
    });

    // Identifies the bad tx and its single (non-existent) spend input.
    expect(output.badTxId).toBe(payload.nodeTxId);
    expect(output.missingInput).toEqual({ tx_id: missingTxId, output_index: 0n });
    expect(output.spendInputCbors).toEqual([missingInputCbor.toString("hex")]);

    // The transactions trie is reconstructed with the node's native encoding.
    const txEntries = [
      {
        key: Buffer.from(payload.nodeTxId, "hex"),
        value: encodeMidgardNativeTxCompact(badTx.compact),
      },
    ];
    expect(output.transactionsRoot).toBe(await computeTrieRoot(txEntries));
    expect(output.txInclusion.nativeTxId).toBe(payload.nodeTxId);

    // The three proofs are byte-identical to the independently-built proofs the
    // emulator test verifies on-chain.
    expect(output.txInclusion.txMembershipProofCbor).toBe(
      await buildMembershipProof(txEntries, Buffer.from(payload.nodeTxId, "hex")),
    );
    expect(output.txsNonMembershipProofCbor).toBe(
      await buildNonMembershipProof(txEntries, Buffer.from(missingTxId, "hex")),
    );
    expect(output.ledgerNonMembershipProofCbor).toBe(
      await buildNonMembershipProof([], missingInputCbor),
    );
    expect(output.prevUtxosRoot).toBe(EMPTY_MERKLE_TREE_ROOT);
  });

  it("requires a node URL for a non-empty prev-utxos root", async () => {
    const badTx = makeNativeTx([inputCbor(h32("de"), 0n)]);
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: "ab".repeat(28),
        transactions: [
          {
            nodeTxId: computeMidgardNativeTxId(badTx).toString("hex"),
            txCbor: encodeMidgardNativeTxCanonical(badTx).toString("hex"),
          },
        ],
        prevUtxosRoot: h32("99"),
      }),
    ).rejects.toThrow(/--midgard-node-url/);
  });

  it("fetches the ledger non-membership proof from the node for a non-empty prev-utxos root", async () => {
    const missingTxId = h32("de");
    const missingInputCbor = inputCbor(missingTxId, 0n);
    const badTx = makeNativeTx([missingInputCbor]);
    const payload: NodeTransactionPayload = {
      nodeTxId: computeMidgardNativeTxId(badTx).toString("hex"),
      txCbor: encodeMidgardNativeTxCanonical(badTx).toString("hex"),
    };
    const prevUtxosRoot = h32("99");
    const nodeProofCbor = "abcdef0123456789";

    const calls: string[] = [];
    const originalFetch = globalThis.fetch;
    globalThis.fetch = (async (input: string | URL) => {
      calls.push(String(input));
      return new Response(JSON.stringify({ proofCbor: nodeProofCbor }), {
        status: 200,
        headers: { "content-type": "application/json" },
      });
    }) as typeof globalThis.fetch;

    try {
      const output = await prepareNonExistentInputFromTransactions({
        headerHash: "ab".repeat(28),
        transactions: [payload],
        prevUtxosRoot,
        midgardNodeUrl: "http://node.test/",
        badInputIndex: 0,
      });

      // The ledger non-membership proof comes from the node, not an empty trie.
      expect(output.prevUtxosRoot).toBe(prevUtxosRoot);
      expect(output.ledgerNonMembershipProofCbor).toBe(nodeProofCbor);
      // It asked the node for that exact root and missing-input key.
      expect(calls).toHaveLength(1);
      expect(calls[0]).toContain("/ledger-non-membership?");
      expect(calls[0]).toContain(`prev_utxos_root=${prevUtxosRoot}`);
      expect(calls[0]).toContain(`input=${missingInputCbor.toString("hex")}`);
      // The txs non-membership proof is still built locally from the block txs.
      expect(output.txsNonMembershipProofCbor).toBe(
        await buildNonMembershipProof(
          [
            {
              key: Buffer.from(payload.nodeTxId, "hex"),
              value: encodeMidgardNativeTxCompact(badTx.compact),
            },
          ],
          Buffer.from(missingTxId, "hex"),
        ),
      );
    } finally {
      globalThis.fetch = originalFetch;
    }
  });
});
