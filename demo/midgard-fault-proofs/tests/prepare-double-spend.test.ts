import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  prepareDoubleSpendFromFile,
  prepareDoubleSpendFromTransactions,
  prepareSampleDoubleSpend,
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

const makeNativeTx = (
  inputs: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull => {
  const body = {
    spendInputsPreimageCbor: encodeCbor(inputs),
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
  nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
});

const payloadFromInputs = (
  inputs: readonly Buffer[],
  fee: bigint,
): NodeTransactionPayload => payloadFromTx(makeNativeTx(inputs, fee));

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-fault-proof-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("prepare-double-spend", () => {
  it("loads a block's native txs and prepares submit-step material", async () => {
    const sharedInput = inputCbor(h32("11"), 7n);

    const output = await prepareDoubleSpendFromTransactions({
      headerHash: h28("aa"),
      transactions: [
        payloadFromInputs([sharedInput, inputCbor(h32("22"), 0n)], 1n),
        payloadFromInputs([inputCbor(h32("33"), 0n), sharedInput], 2n),
        payloadFromInputs([inputCbor(h32("44"), 0n)], 3n),
      ],
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(3);
    expect(output.doubleSpentInput).toEqual({
      transactionId: h32("11"),
      outputIndex: 7n,
    });
    expect(output.tx1.doubleSpentInputIndex).toBe(0);
    expect(output.tx2.doubleSpentInputIndex).toBe(1);
    expect(output.tx1.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.tx2.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.tx1.txInclusion.nativeTxId).toBe(output.tx1.nodeTxId);
    expect(output.tx1.txInclusion.nativeTxCompactCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.tx1.spendInputCbors[0]).toBe(sharedInput.toString("hex"));
    expect(output.commitmentEncodings.nativeNode.transactionsRoot).toMatch(
      /^[0-9a-f]{64}$/,
    );
    expect(output.compatibility.canUseSubmitStepCommands).toBe(true);
    expect(output.compatibility.reasons).toEqual([]);
  });

  it("honors explicit tx pair selection", async () => {
    const sharedInput = inputCbor(h32("55"), 1n);
    const tx1Payload = payloadFromInputs([sharedInput], 1n);
    const tx2Payload = payloadFromInputs(
      [inputCbor(h32("66"), 0n), sharedInput],
      2n,
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
          payloadFromInputs([inputCbor(h32("77"), 0n)], 1n),
          payloadFromInputs([inputCbor(h32("88"), 0n)], 2n),
        ],
      }),
    ).rejects.toThrow("No double spend found");
  });

  it("fetches node block and transaction payloads through public node endpoints", async () => {
    const tx1Payload = payloadFromInputs([inputCbor(h32("99"), 0n)], 1n);
    const tx2Payload = payloadFromInputs([inputCbor(h32("aa"), 0n)], 2n);
    const fetchImpl = async (input: string | URL): Promise<Response> => {
      const url = new URL(String(input));
      if (url.pathname === "/block") {
        return new Response(
          JSON.stringify({
            hashes: [tx1Payload.nodeTxId, tx2Payload.nodeTxId],
          }),
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

  it("prepares submit-step material from an explicit transactions file", async () => {
    await withTempDir(async (dir) => {
      const sharedInput = inputCbor(h32("ab"), 3n);
      const tx1Payload = payloadFromInputs([sharedInput], 1n);
      const tx2Payload = payloadFromInputs(
        [inputCbor(h32("cd"), 0n), sharedInput],
        2n,
      );
      const transactionsPath = join(dir, "block-transactions.json");
      await writeFile(
        transactionsPath,
        JSON.stringify([tx1Payload, tx2Payload]),
      );

      const output = await prepareDoubleSpendFromFile({
        headerHash: h28("ee"),
        transactionsPath,
        outputDir: dir,
      });

      expect(output.doubleSpentInput).toEqual({
        transactionId: h32("ab"),
        outputIndex: 3n,
      });
      expect(output.files?.tx1InputsPath).toBe(join(dir, "tx1-inputs.json"));
      expect(output.compatibility.canUseSubmitStepCommands).toBe(true);
    });
  });

  it("writes a deterministic sample double-spend block transaction file", async () => {
    await withTempDir(async (dir) => {
      const output = await prepareSampleDoubleSpend({
        headerHash: h28("ef"),
        outputDir: dir,
      });

      expect(output.txCount).toBe(3);
      expect(output.doubleSpentInput).toEqual({
        transactionId: h32("11"),
        outputIndex: 7n,
      });
      expect(output.files?.blockTransactionsPath).toBe(
        join(dir, "block-transactions.json"),
      );
      const payloads = JSON.parse(
        await readFile(join(dir, "block-transactions.json"), "utf8"),
      ) as unknown[];
      expect(payloads).toHaveLength(3);
      expect(output.compatibility.canUseSubmitStepCommands).toBe(true);
    });
  });
});
