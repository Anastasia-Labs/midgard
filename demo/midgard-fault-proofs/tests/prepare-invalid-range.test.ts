import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type NodeTransactionPayload,
  prepareInvalidRangeFromFile,
  prepareInvalidRangeFromNode,
  prepareInvalidRangeFromTransactions,
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

const makeNativeTx = ({
  validityIntervalStart,
  validityIntervalEnd,
  fee,
}: {
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly fee: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([inputCbor(h32("11"), fee)]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee,
      validityIntervalStart,
      validityIntervalEnd,
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

const payloadFromTx = (tx: MidgardNativeTxFullV1): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxIdV1(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
});

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-invalid-range-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("prepare-invalid-range", () => {
  it("prepares submit-step material for a transaction whose lower bound starts before the block", async () => {
    const validTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 100n,
        validityIntervalEnd: 200n,
        fee: 1n,
      }),
    );
    const badTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 99n,
        validityIntervalEnd: 150n,
        fee: 2n,
      }),
    );

    const output = await prepareInvalidRangeFromTransactions({
      headerHash: h28("aa"),
      transactions: [validTx, badTx],
      blockValidFrom: 100n,
      blockValidTo: 200n,
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(2);
    expect(output.tx.nodeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.violationReason).toBe("lower-before-block");
    expect(output.tx.normalizedValidityRange).toEqual({
      ClosedRange: { lower: 99n, upper: 149n },
    });
    expect(output.tx.txInclusion.nativeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.commitmentEncodings.nativeNode.transactionsRoot).toMatch(
      /^[0-9a-f]{64}$/,
    );
  });

  it("honors explicit tx selection and rejects a selected non-violating tx", async () => {
    const validTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 100n,
        validityIntervalEnd: 200n,
        fee: 1n,
      }),
    );
    const upperBadTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 100n,
        validityIntervalEnd: 201n,
        fee: 2n,
      }),
    );

    await expect(
      prepareInvalidRangeFromTransactions({
        headerHash: h28("bb"),
        transactions: [validTx, upperBadTx],
        blockValidFrom: 100n,
        blockValidTo: 200n,
        txId: validTx.nodeTxId,
      }),
    ).rejects.toThrow("does not violate the block validity range");

    const output = await prepareInvalidRangeFromTransactions({
      headerHash: h28("bb"),
      transactions: [validTx, upperBadTx],
      blockValidFrom: 100n,
      blockValidTo: 200n,
      txId: upperBadTx.nodeTxId,
    });
    expect(output.tx.nodeTxId).toBe(upperBadTx.nodeTxId);
    expect(output.tx.violationReason).toBe("upper-at-or-after-block");
  });

  it("fails closed when the expected V1 transactions root differs", async () => {
    const badTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 99n,
        validityIntervalEnd: 150n,
        fee: 8n,
      }),
    );
    await expect(
      prepareInvalidRangeFromTransactions({
        headerHash: h28("bc"),
        expectedTransactionsRoot: h32("00"),
        transactions: [badTx],
        blockValidFrom: 100n,
        blockValidTo: 200n,
      }),
    ).rejects.toThrow("Expected V1 transactions root");
  });

  it("rejects blocks without invalid-range transactions", async () => {
    await expect(
      prepareInvalidRangeFromTransactions({
        headerHash: h28("cc"),
        transactions: [
          payloadFromTx(
            makeNativeTx({
              validityIntervalStart: 100n,
              validityIntervalEnd: 200n,
              fee: 1n,
            }),
          ),
          payloadFromTx(
            makeNativeTx({
              validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
              validityIntervalEnd: 199n,
              fee: 2n,
            }),
          ),
        ],
        blockValidFrom: 100n,
        blockValidTo: 200n,
      }),
    ).rejects.toThrow("No invalid-range transaction found");
  });

  it("writes deterministic tx inclusion and plan files from an explicit transactions file", async () => {
    await withTempDir(async (dir) => {
      const badTx = payloadFromTx(
        makeNativeTx({
          validityIntervalStart: 150n,
          validityIntervalEnd: 150n,
          fee: 3n,
        }),
      );
      const transactionsPath = join(dir, "block-transactions.json");
      await writeFile(transactionsPath, JSON.stringify([badTx]));

      const output = await prepareInvalidRangeFromFile({
        headerHash: h28("dd"),
        transactionsPath,
        blockValidFrom: 100n,
        blockValidTo: 200n,
        outputDir: dir,
      });

      expect(output.tx.violationReason).toBe("invalid-range");
      expect(output.files?.txInclusionPath).toBe(
        join(dir, "tx-inclusion.json"),
      );
      expect(output.files?.planPath).toBe(join(dir, "plan.json"));
      const inclusion = JSON.parse(
        await readFile(join(dir, "tx-inclusion.json"), "utf8"),
      ) as { readonly nativeTxId: string };
      expect(inclusion.nativeTxId).toBe(badTx.nodeTxId);
      const plan = JSON.parse(
        await readFile(join(dir, "plan.json"), "utf8"),
      ) as {
        readonly violationReason: string;
        readonly txNodeTxId: string;
      };
      expect(plan).toMatchObject({
        violationReason: "invalid-range",
        txNodeTxId: badTx.nodeTxId,
      });
    });
  });

  it("fetches node block transactions before preparing invalid-range material", async () => {
    const badTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: 250n,
        fee: 4n,
      }),
    );
    const fetchImpl = async (input: string | URL): Promise<Response> => {
      const url = new URL(String(input));
      if (url.pathname === "/block") {
        return new Response(JSON.stringify({ hashes: [badTx.nodeTxId] }), {
          status: 200,
        });
      }
      if (url.pathname === "/tx") {
        return new Response(JSON.stringify({ tx: badTx.txCbor }), {
          status: 200,
        });
      }
      return new Response("not found", { status: 404 });
    };

    const output = await prepareInvalidRangeFromNode({
      midgardNodeUrl: "http://node.local/",
      headerHash: h28("ee"),
      blockValidFrom: 100n,
      blockValidTo: 200n,
      fetchImpl,
    });

    expect(output.tx.nodeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.violationReason).toBe("upper-at-or-after-block");
  });
});
