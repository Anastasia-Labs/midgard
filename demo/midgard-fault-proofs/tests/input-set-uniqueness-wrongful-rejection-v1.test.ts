import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, expectTypeOf, it } from "vitest";

import { planFaultProofFieldOpeningV1 } from "../src/field-opening-v1.js";
import type { InputSetUniquenessForcedProductionInputV1 } from "../src/input-set-uniqueness/production-v1.js";
import { detectInputSetUniquenessForcedReplayV1 } from "../src/input-set-uniqueness/replay-v1.js";
import {
  bindForcedDuplicateInputV1,
  inputSetUnionIsStrictlyIncreasingV1,
  inputSetUniquenessCheckpointV1,
} from "../src/input-set-uniqueness/wrongful-rejection-v1.js";
import {
  buildInputSetUniquenessFixtureV1,
  isuItemCborV1,
  isuOutRefV1,
} from "./support/input-set-uniqueness-emulator-v1.js";

const reason = {
  DuplicateInput: {
    first_field_index: 0n,
    first_item_index: 0n,
    second_field_index: 1n,
    second_item_index: 0n,
  },
} as const;
const subject = forcedVerdictSubjectV1({
  transactionId: "aa".repeat(32),
  sourceKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
  rejectionReason: reason,
});

describe("input-set-uniqueness wrongful-rejection rules", () => {
  it("accepts only a globally strict spend/reference union", () => {
    expect(
      inputSetUnionIsStrictlyIncreasingV1({
        spendInputItemCbors: [
          isuItemCborV1(isuOutRefV1("11", 0)),
          isuItemCborV1(isuOutRefV1("22", 0)),
        ],
        referenceInputItemCbors: [isuItemCborV1(isuOutRefV1("33", 0))],
      }),
    ).toBe(true);
  });

  it("rejects adjacent equality, boundary equality, and descending order", () => {
    const low = isuItemCborV1(isuOutRefV1("11", 0));
    const high = isuItemCborV1(isuOutRefV1("22", 0));
    expect(
      inputSetUnionIsStrictlyIncreasingV1({
        spendInputItemCbors: [low, low],
        referenceInputItemCbors: [],
      }),
    ).toBe(false);
    expect(
      inputSetUnionIsStrictlyIncreasingV1({
        spendInputItemCbors: [low],
        referenceInputItemCbors: [low],
      }),
    ).toBe(false);
    expect(
      inputSetUnionIsStrictlyIncreasingV1({
        spendInputItemCbors: [high],
        referenceInputItemCbors: [low],
      }),
    ).toBe(false);
  });

  it("rejects malformed items instead of accepting an incomplete universe", () => {
    expect(() =>
      inputSetUnionIsStrictlyIncreasingV1({
        spendInputItemCbors: ["80"],
        referenceInputItemCbors: [],
      }),
    ).toThrow(/canonical 38-byte out-ref/);
  });

  it("binds the complete authenticated forced reason and all positions", () => {
    expect(bindForcedDuplicateInputV1(subject)).toStrictEqual({
      subject,
      ...reason.DuplicateInput,
    });
    expect(() =>
      bindForcedDuplicateInputV1(acceptedVerdictSubjectV1("aa".repeat(32))),
    ).toThrow(/forced subject/);
  });

  it("checkpoints every resume coordinate and successor script", () => {
    const bound = bindForcedDuplicateInputV1(subject);
    const base = {
      bound,
      spendCount: 1n,
      referenceCount: 1n,
      cursor: 1n,
      previousItem: isuItemCborV1(isuOutRefV1("11", 0)),
      nextExpectedScriptHash: "cc".repeat(28),
    };
    const checkpoint = inputSetUniquenessCheckpointV1(base);
    expect(checkpoint).toMatch(/^[0-9a-f]{64}$/u);
    expect(inputSetUniquenessCheckpointV1({ ...base, cursor: 2n })).not.toBe(
      checkpoint,
    );
    expect(
      inputSetUniquenessCheckpointV1({
        ...base,
        nextExpectedScriptHash: "dd".repeat(28),
      }),
    ).not.toBe(checkpoint);
  });

  it("derives the contradiction only from an authenticated forced leaf", async () => {
    const fixture = await buildInputSetUniquenessFixtureV1({
      spendInputs: [isuOutRefV1("11", 0)],
      referenceInputs: [isuOutRefV1("22", 0)],
      validity: "TxIsInvalid",
    });
    const detections = detectInputSetUniquenessForcedReplayV1({
      headerHash: "ee".repeat(28),
      reconstruction: {
        forcedTransactions: [
          {
            key: { transactionId: "bb".repeat(32), outputIndex: 0n },
            value: {
              tx_id: fixture.nativeTxId,
              source: fixture.forcedSource,
              verdict: { ForcedTxInvalid: { reason } },
            },
            fullTransactionCbor: fixture.fullTransactionCbor,
          },
        ],
      },
    } as never);
    expect(detections).toHaveLength(1);
    expect(detections[0]!.spendInputItemCbors).toStrictEqual(
      fixture.spendInputItemCbors,
    );
    expect(detections[0]!.referenceInputItemCbors).toStrictEqual(
      fixture.referenceInputItemCbors,
    );
  });

  it("refuses the first reference-input field above the consensus byte bound", async () => {
    const references = Array.from({ length: 820 }, (_, index) =>
      isuOutRefV1("aa", index),
    );
    const fixture = await buildInputSetUniquenessFixtureV1({
      spendInputs: [isuOutRefV1("00", 0)],
      referenceInputs: references,
      validity: "TxIsInvalid",
    });
    expect(() =>
      planFaultProofFieldOpeningV1({
        fieldIndex: 1,
        anchorTxId: fixture.nativeTxId,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        itemCbors: fixture.referenceInputItemCbors.map((item) =>
          Buffer.from(item, "hex"),
        ),
        owner: "11".repeat(28),
        label: "input-set-uniqueness adjacent over-bound",
      }),
    ).toThrow(/32,?768|aggregate (field )?bound/i);
  });

  it("keeps the production authority boundary callback-free", () => {
    expectTypeOf<
      keyof InputSetUniquenessForcedProductionInputV1
    >().toEqualTypeOf<"block">();
    expect(Object.keys({ block: null })).toStrictEqual(["block"]);
  });
});
