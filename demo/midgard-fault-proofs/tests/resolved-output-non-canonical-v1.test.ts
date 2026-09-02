import {
  buildMidgardBoundedItemV1,
  computeMidgardNativeTxIdV1,
  decodeMidgardLedgerOutputCommitmentV1,
  encodeMidgardLedgerOutputCommitmentV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  classifyResolvedOutputNonCanonicalFindingV1,
  detectResolvedOutputNonCanonicalCompleteReplayV1,
  RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY_V1,
  RESOLVED_OUTPUT_NON_CANONICAL_ID_V1,
} from "../src/resolved-output-non-canonical/resolved-output-non-canonical-v1.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const txId = "11".repeat(32);
const accepted = acceptedVerdictSubjectV1(txId);
const rejected = (sourceKind = 1, inputIndex = 4) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: {
      InputSpentOutputNonCanonical: {
        source_kind: BigInt(sourceKind),
        input_index: BigInt(inputIndex),
      },
    },
  });

describe("resolvedOutputNonCanonical V1 family boundary", () => {
  it("freezes category identity 00000026", () => {
    expect(RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY_V1).toBe(
      "resolvedOutputNonCanonical",
    );
    expect(RESOLVED_OUTPUT_NON_CANONICAL_ID_V1).toBe("00000026");
  });

  it("admits either exact prior-input collection for accepted subjects", () => {
    expect(() =>
      classifyResolvedOutputNonCanonicalFindingV1({
        subject: accepted,
        coordinate: { sourceKind: 0, inputIndex: 0 },
      }),
    ).not.toThrow();
    expect(() =>
      classifyResolvedOutputNonCanonicalFindingV1({
        subject: accepted,
        coordinate: { sourceKind: 1, inputIndex: 9 },
      }),
    ).not.toThrow();
  });

  it("binds the forced reason constructor and coordinate exactly", () => {
    expect(() =>
      classifyResolvedOutputNonCanonicalFindingV1({
        subject: rejected(),
        coordinate: { sourceKind: 1, inputIndex: 4 },
      }),
    ).not.toThrow();
    expect(() =>
      classifyResolvedOutputNonCanonicalFindingV1({
        subject: rejected(),
        coordinate: { sourceKind: 0, inputIndex: 4 },
      }),
    ).toThrow(/coordinate was substituted/u);
    expect(() =>
      classifyResolvedOutputNonCanonicalFindingV1({
        subject: rejected(),
        coordinate: { sourceKind: 1, inputIndex: 3 },
      }),
    ).toThrow(/coordinate was substituted/u);
  });

  it("refuses an adjacent input-not-found reason independently", () => {
    const other = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
      rejectionReason: {
        InputNotFound: { source_kind: 1n, input_index: 4n },
      },
    });
    expect(() =>
      classifyResolvedOutputNonCanonicalFindingV1({
        subject: other,
        coordinate: { sourceKind: 1, inputIndex: 4 },
      }),
    ).toThrow(/wrong typed rejection reason/u);
  });

  it("complete replay distinguishes wrongful and honest forced reasons and rejects identity mutation", () => {
    const priorTxId = "33".repeat(32);
    const outRef = encodeMidgardSpendInputItemV1({
      txId: Buffer.from(priorTxId, "hex"),
      outputIndex: 0,
    });
    const transaction = makeNativeTx({ spendInputCbors: [outRef], fee: 7n });
    const transactionCbor = encodeMidgardNativeTxCanonicalV1(transaction);
    const transactionId =
      computeMidgardNativeTxIdV1(transaction).toString("hex");
    const canonicalOutput = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const malformedOutput = Buffer.concat([canonicalOutput, Buffer.from([0])]);
    const resolved = (output: Buffer) => {
      const template = decodeMidgardLedgerOutputCommitmentV1(
        buildCanonicalMidgardLedgerOutputMaterialV1({
          outputIndex: 0,
          outputCbor: canonicalOutput,
        }).descriptorCbor,
      );
      const item = buildMidgardBoundedItemV1({
        fieldIndex: 2,
        itemIndex: 0,
        bytes: output,
      });
      return {
        transactionId: priorTxId,
        outputIndex: 0,
        descriptorCborHex: encodeMidgardLedgerOutputCommitmentV1({
          ...template,
          totalLength: output.length,
          itemCommitment: item.commitment,
        }).toString("hex"),
        outputCborHex: output.toString("hex"),
        membershipProofCborHex: "80",
        membershipProof: [],
      };
    };
    const priorRoot = "44".repeat(32);
    const block = (
      forcedTransactions: readonly unknown[] = [],
      accepted = false,
    ) =>
      ({
        header: { prevUtxosRoot: priorRoot },
        transactions: accepted
          ? [
              {
                nodeTxId: transactionId,
                txCbor: transactionCbor.toString("hex"),
              },
            ]
          : [],
        reconstruction: { forcedTransactions },
      }) as never;
    const reason = {
      InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
    } as const;
    const forced = (txId = transactionId) => ({
      key: { transactionId: "55".repeat(32), outputIndex: 0n },
      fullTransactionCbor: transactionCbor,
      value: {
        tx_id: txId,
        verdict: { ForcedTxInvalid: { reason } },
      },
    });
    const ledger = (output: Buffer) => ({
      priorRoot,
      outputs: new Map([[`${priorTxId}#0`, resolved(output)]]),
    });

    expect(
      detectResolvedOutputNonCanonicalCompleteReplayV1({
        block: block([], true),
        priorLedger: ledger(malformedOutput),
      }),
    ).toHaveLength(1);
    expect(
      detectResolvedOutputNonCanonicalCompleteReplayV1({
        block: block([forced()]),
        priorLedger: ledger(canonicalOutput),
      }),
    ).toHaveLength(1);
    expect(
      detectResolvedOutputNonCanonicalCompleteReplayV1({
        block: block([forced()]),
        priorLedger: ledger(malformedOutput),
      }),
    ).toEqual([]);
    expect(() =>
      detectResolvedOutputNonCanonicalCompleteReplayV1({
        block: block([forced("66".repeat(32))]),
        priorLedger: ledger(canonicalOutput),
      }),
    ).toThrow(/transaction identity was substituted/u);
  });
});
