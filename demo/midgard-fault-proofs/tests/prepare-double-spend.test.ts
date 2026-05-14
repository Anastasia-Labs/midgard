import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  encodeCbor,
  encodeMidgardNativeTxFull,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  fetchNodeBlockTransactions,
  prepareDoubleSpendFromTransactions,
  type NodeTransactionPayload,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      outputIndex,
    ).to_cbor_bytes(),
  );

const byteList = (items: readonly Buffer[]): Buffer => encodeCbor(items);

const makeNativeTx = (
  inputs: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull => {
  const body = {
    spendInputsPreimageCbor: byteList(inputs),
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: 0n,
  };
  const witnessSet = {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };
  return materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet,
  });
};

const payloadFromTx = (tx: MidgardNativeTxFull): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxIdFromFull(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxFull(tx).toString("hex"),
});

describe("prepare-double-spend", () => {
  it("loads a block's native txs and prepares submit-step material", async () => {
    const sharedInput = inputCbor(h32("11"), 7n);
    const tx1 = makeNativeTx([sharedInput, inputCbor(h32("22"), 0n)], 1n);
    const tx2 = makeNativeTx([inputCbor(h32("33"), 0n), sharedInput], 2n);
    const tx3 = makeNativeTx([inputCbor(h32("44"), 0n)], 3n);

    const output = await prepareDoubleSpendFromTransactions({
      headerHash: h28("aa"),
      transactions: [payloadFromTx(tx1), payloadFromTx(tx2), payloadFromTx(tx3)],
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(3);
    expect(output.doubleSpentInput).toEqual({
      transactionId: h32("11"),
      outputIndex: 7n,
    });
    expect(output.tx1.doubleSpentInputIndex).toBe(0);
    expect(output.tx2.doubleSpentInputIndex).toBe(1);
    expect(output.tx1.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    expect(output.tx2.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    expect(output.tx1.txInclusion.nativeTxId).toBe(output.tx1.nodeTxId);
    expect(output.tx1.txInclusion.nativeTxCompactCbor.length).toBeGreaterThan(0);
    expect(output.tx1.spendInputCbors[0]).toBe(sharedInput.toString("hex"));
    expect(output.commitmentEncodings.nativeNode.transactionsRoot).toMatch(
      /^[0-9a-f]{64}$/,
    );
    expect(output.compatibility.canUseSubmitStepCommands).toBe(true);
    expect(output.compatibility.reasons).toEqual([]);
  });

  it("honors explicit tx pair selection", async () => {
    const sharedInput = inputCbor(h32("55"), 1n);
    const tx1Payload = payloadFromTx(makeNativeTx([sharedInput], 1n));
    const tx2Payload = payloadFromTx(
      makeNativeTx([inputCbor(h32("66"), 0n), sharedInput], 2n),
    );

    const output = await prepareDoubleSpendFromTransactions({
      headerHash: h28("bb"),
      transactions: [tx1Payload, tx2Payload],
      tx1Id: tx1Payload.nodeTxId,
      tx2Id: tx2Payload.nodeTxId,
    });

    expect(output.tx1.nodeTxId).toBe(tx1Payload.nodeTxId);
    expect(output.tx2.nodeTxId).toBe(tx2Payload.nodeTxId);
    expect(output.tx2.doubleSpentInputIndex).toBe(1);
  });

  it("rejects blocks without two distinct transactions spending the same input", async () => {
    await expect(
      prepareDoubleSpendFromTransactions({
        headerHash: h28("cc"),
        transactions: [
          payloadFromTx(makeNativeTx([inputCbor(h32("77"), 0n)], 1n)),
          payloadFromTx(makeNativeTx([inputCbor(h32("88"), 0n)], 2n)),
        ],
      }),
    ).rejects.toThrow("No double spend found");
  });

  it("fetches node block and transaction payloads through public node endpoints", async () => {
    const tx1Payload = payloadFromTx(makeNativeTx([inputCbor(h32("99"), 0n)], 1n));
    const tx2Payload = payloadFromTx(makeNativeTx([inputCbor(h32("aa"), 0n)], 2n));
    const fetchImpl = async (input: string | URL): Promise<Response> => {
      const url = new URL(String(input));
      if (url.pathname === "/block") {
        return new Response(
          JSON.stringify({ hashes: [tx1Payload.nodeTxId, tx2Payload.nodeTxId] }),
          { status: 200 },
        );
      }
      if (url.pathname === "/tx") {
        const txHash = url.searchParams.get("tx_hash");
        const tx =
          txHash === tx1Payload.nodeTxId
            ? tx1Payload.txCbor
            : txHash === tx2Payload.nodeTxId
              ? tx2Payload.txCbor
              : undefined;
        return new Response(JSON.stringify({ tx }), {
          status: tx === undefined ? 404 : 200,
        });
      }
      return new Response("not found", { status: 404 });
    };

    await expect(
      fetchNodeBlockTransactions({
        midgardNodeUrl: "http://node.local/",
        headerHash: h28("dd"),
        fetchImpl,
      }),
    ).resolves.toEqual([tx1Payload, tx2Payload]);
  });
});
