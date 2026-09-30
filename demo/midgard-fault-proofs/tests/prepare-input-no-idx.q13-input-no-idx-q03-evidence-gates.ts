import "./prepare-input-no-idx.q13-input-no-idx-canonical-evidence.js";

import { readFile } from "node:fs/promises";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  authenticateTransactionsInclusionRoots,
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/index.js";
import * as FaultProofs from "../src/index.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  transactionSourceTrieItem,
} from "../src/prepare-double-spend.js";
import {
  prepareInputNoIdxFromCanonicalEvidence,
  prepareInputNoIdxFromTransactions,
} from "../src/prepare-input-no-idx.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type CanonicalBlockFixture,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  committedTransactionsRoot,
  inputCbor,
  producerTx,
  rejectionCode,
  spenderTx,
  violatingBlock,
  withTempDir,
} from "./prepare-input-no-idx.make-native-tx.js";

describe("Q13 input-no-idx valid-block negatives", () => {
  it("refuses to prove non-existence of an input the producing transaction really created", async () => {
    const producer = producerTx(3, 1n);
    const spender = spenderTx(producer.nodeTxId, 2n, 2n);
    const transactions = [producer, spender];
    const expectedTransactionsRoot =
      await committedTransactionsRoot(transactions);

    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28(0xaa),
          transactions,
          expectedTransactionsRoot,
        }),
      ),
    ).toBe("input_exists_in_producing_tx");
  });

  it("refuses when the input's producing transaction is not committed in this block", async () => {
    const spender = spenderTx(h32(0x77), 0n, 2n);
    const transactions = [spender];
    const expectedTransactionsRoot =
      await committedTransactionsRoot(transactions);

    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28(0xaa),
          transactions,
          expectedTransactionsRoot,
        }),
      ),
    ).toBe("producing_tx_not_committed");
  });

  it("refuses a pinned transaction that is not committed in the block", async () => {
    const block = await violatingBlock();
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28(0xaa),
          transactions: block.transactions,
          expectedTransactionsRoot: block.expectedTransactionsRoot,
          badTxId: h32(0xee),
        }),
      ),
    ).toBe("bad_tx_not_committed");
  });

  it("refuses a pinned input position the transaction does not have", async () => {
    const block = await violatingBlock();
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28(0xaa),
          transactions: block.transactions,
          expectedTransactionsRoot: block.expectedTransactionsRoot,
          badTxId: block.spender.nodeTxId,
          badInputsIndex: 4,
        }),
      ),
    ).toBe("bad_input_index_out_of_range");
  });

  it("refuses a root the block header does not commit", async () => {
    const block = await violatingBlock();
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28(0xaa),
          transactions: block.transactions,
          expectedTransactionsRoot: h32(0xff),
        }),
      ),
    ).toBe("transactions_root_mismatch");
  });

  it("refuses the raw PHAS root where the counted header root is required", async () => {
    const block = await violatingBlock();
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromTransactions({
          headerHash: h28(0xaa),
          transactions: block.transactions,
          expectedTransactionsRoot: trie.root,
        }),
      ),
    ).toBe("transactions_root_mismatch");
  });
});

describe("Q13 input-no-idx resumable artifacts", () => {
  it("writes the four submit-step artifacts and the plan", async () => {
    const block = await violatingBlock();
    await withTempDir(async (dir) => {
      const output = await prepareInputNoIdxFromTransactions({
        headerHash: h28(0xaa),
        transactions: block.transactions,
        expectedTransactionsRoot: block.expectedTransactionsRoot,
        outputDir: dir,
      });

      expect(output.files).toEqual({
        badTxInclusionPath: join(dir, "bad-tx-inclusion.json"),
        producingTxInclusionPath: join(dir, "producing-tx-inclusion.json"),
        inputsPreimagePath: join(dir, "inputs-preimage.json"),
        outputsPreimagePath: join(dir, "outputs-preimage.json"),
        planPath: join(dir, "plan.json"),
      });

      const badTxInclusion = JSON.parse(
        await readFile(output.files!.badTxInclusionPath, "utf8"),
      ) as { readonly nativeTxId: string };
      expect(badTxInclusion.nativeTxId).toBe(block.spender.nodeTxId);

      const producingTxInclusion = JSON.parse(
        await readFile(output.files!.producingTxInclusionPath, "utf8"),
      ) as { readonly nativeTxId: string };
      expect(producingTxInclusion.nativeTxId).toBe(block.producer.nodeTxId);

      const inputsPreimage = JSON.parse(
        await readFile(output.files!.inputsPreimagePath, "utf8"),
      ) as { readonly badInputsIndex: number };
      expect(inputsPreimage.badInputsIndex).toBe(0);

      const outputsPreimage = JSON.parse(
        await readFile(output.files!.outputsPreimagePath, "utf8"),
      ) as { readonly badInputOutputIndex: string };
      expect(outputsPreimage.badInputOutputIndex).toBe("7");

      const plan = JSON.parse(
        await readFile(output.files!.planPath, "utf8"),
      ) as {
        readonly committedTransactionsRoot: string;
        readonly proofFit: { readonly step02CarriageTier: string };
      };
      expect(plan.committedTransactionsRoot).toBe(
        output.committedTransactionsRoot,
      );
      expect(plan.proofFit.step02CarriageTier).toBe("Inline");
    });
  });

  it("is exported from the package root for the resumable prepare/submit flow", () => {
    expect(FaultProofs.prepareInputNoIdxFromTransactions).toBeTypeOf(
      "function",
    );
    expect(FaultProofs.prepareInputNoIdxFromCanonicalEvidence).toBeTypeOf(
      "function",
    );
    expect(FaultProofs.prepareInputNoIdxFromNode).toBeTypeOf("function");
    expect(FaultProofs.prepareInputNoIdxFromFile).toBeTypeOf("function");
    expect(SDK.INPUT_NO_IDX_VIOLATION_ID).toBe("input-no-idx");
    expect(SDK.INPUT_NO_IDX_CATALOGUE_CATEGORY).toBe("nonExistentInputNoIndex");
  });
});

describe("Q13 input-no-idx Q03 evidence gates", () => {
  const DA_PROVENANCE: SDK.EvidenceProvenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "libp2p/peer-a",
    grade: "security",
  };

  /**
   * A block whose producer commits no outputs at all, so its single spender
   * challenges index 0. Built through the shared Q03 payload fixture so the
   * evidence really is DA + L1 derived.
   */
  const canonicalViolatingFixture = async (
    transactionsRootMode: "payloadSource" | "nativeCompact",
  ): Promise<CanonicalBlockFixture> => {
    const producer = buildFixtureTransaction({
      spendInputs: [inputCbor(h32(0x99), 0n)],
      fee: 1n,
    });
    const spender = buildFixtureTransaction({
      spendInputs: [inputCbor(producer.txId, 0n)],
      fee: 2n,
    });
    return await buildCanonicalBlockFixture({
      transactionsRootMode,
      transactions: [producer, spender],
    });
  };

  const evidenceFor = async (
    fixture: CanonicalBlockFixture,
  ): Promise<CanonicalBlockEvidence> => {
    const payloadFixture =
      fixture.transactionsRootMode === "nativeCompact"
        ? await buildCanonicalBlockFixture({
            transactions: fixture.transactions,
            startTime: fixture.header.startTime,
            endTime: fixture.header.endTime,
          })
        : fixture;
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(payloadFixture),
      payloadEnvelopeCbor: payloadFixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    if (fixture.transactionsRootMode !== "nativeCompact") {
      return evidence;
    }
    return {
      ...evidence,
      observation: authenticatedHeaderObservation(fixture),
      headerHash: fixture.headerHash,
      header: fixture.header,
      inclusionRootAuthentication: await authenticateTransactionsInclusionRoots(
        {
          header: fixture.header,
          reconstruction: evidence.reconstruction,
          transactions: evidence.transactions,
        },
      ),
    };
  };

  it("builds the proof from retained public DA bound to an authenticated L1 header", async () => {
    // The header's normative transactions MPF commits
    // `Data(L2TransactionSourceV1)` per transaction id, which is the shape the
    // DA payload's `transactions` map carries, so `payloadSource` is the
    // authenticating combination and `nativeCompact` is its refusal twin below.
    const fixture = await canonicalViolatingFixture("payloadSource");
    const output = await prepareInputNoIdxFromCanonicalEvidence({
      evidence: await evidenceFor(fixture),
    });

    expect(output.txCount).toBe(2);
    expect(output.evidence.isViolation).toBe(true);
    expect(output.evidence.producingTxOutputCount).toBe(0);
    expect(output.evidence.badInput.output_index).toBe(0n);
    expect(output.headerHash).toBe(fixture.headerHash);
    expect(output.committedTransactionsRoot).toBe(
      fixture.header.transactionsRoot,
    );
  });

  it("refuses evidence whose native inclusion root cannot be authenticated on-chain", async () => {
    // A header whose transactions root was counted over bare compact CBOR no
    // longer re-commits the source values the DA payload carries, so the
    // inclusion gate must refuse it.
    const evidence = await evidenceFor(
      await canonicalViolatingFixture("nativeCompact"),
    );
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromCanonicalEvidence({ evidence }),
      ),
    ).toBe("transaction_source_inclusion_root_unauthenticated");
  });

  it("refuses evidence whose DA record was downgraded to an operator diagnostic", async () => {
    const evidence = await evidenceFor(
      await canonicalViolatingFixture("nativeCompact"),
    );
    const downgraded: CanonicalBlockEvidence = {
      ...evidence,
      provenance: {
        ...evidence.provenance,
        da: {
          trustClass: "operator_admin_api",
          sourceId: "midgard-node-url",
          grade: "diagnostic",
          diagnosticLabel: "operator REST diagnostic",
        },
      },
    };
    expect(
      await rejectionCode(async () =>
        prepareInputNoIdxFromCanonicalEvidence({ evidence: downgraded }),
      ),
    ).toBe("prohibited_trust_class");
  });
});
