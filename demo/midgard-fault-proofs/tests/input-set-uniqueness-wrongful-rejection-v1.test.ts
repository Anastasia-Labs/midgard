import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, expectTypeOf, it } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening-v1.js";
import type { InputSetUniquenessForcedInput } from "../src/input-set-uniqueness/production-v1.js";
import { detectInputSetUniquenessForcedReplay } from "../src/input-set-uniqueness/replay-v1.js";
import {
  bindForcedDuplicateInput,
  inputSetUnionIsStrictlyIncreasing,
  inputSetUniquenessCheckpoint,
} from "../src/input-set-uniqueness/wrongful-rejection-v1.js";
import {
  buildInputSetUniquenessFixture,
  isuItemCbor,
  isuOutRef,
} from "./support/input-set-uniqueness-emulator-v1.js";

const reason = {
  DuplicateInput: {
    first_field_index: 0n,
    first_item_index: 0n,
    second_field_index: 1n,
    second_item_index: 0n,
  },
} as const;
const subject = forcedVerdictSubject({
  transactionId: "aa".repeat(32),
  sourceKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
  rejectionReason: reason,
});

describe("input-set-uniqueness wrongful-rejection rules", () => {
  it("accepts only a globally strict spend/reference union", () => {
    expect(
      inputSetUnionIsStrictlyIncreasing({
        spendInputItemCbors: [
          isuItemCbor(isuOutRef("11", 0)),
          isuItemCbor(isuOutRef("22", 0)),
        ],
        referenceInputItemCbors: [isuItemCbor(isuOutRef("33", 0))],
      }),
    ).toBe(true);
  });

  it("rejects adjacent equality, boundary equality, and descending order", () => {
    const low = isuItemCbor(isuOutRef("11", 0));
    const high = isuItemCbor(isuOutRef("22", 0));
    expect(
      inputSetUnionIsStrictlyIncreasing({
        spendInputItemCbors: [low, low],
        referenceInputItemCbors: [],
      }),
    ).toBe(false);
    expect(
      inputSetUnionIsStrictlyIncreasing({
        spendInputItemCbors: [low],
        referenceInputItemCbors: [low],
      }),
    ).toBe(false);
    expect(
      inputSetUnionIsStrictlyIncreasing({
        spendInputItemCbors: [high],
        referenceInputItemCbors: [low],
      }),
    ).toBe(false);
  });

  it("rejects malformed items instead of accepting an incomplete universe", () => {
    expect(() =>
      inputSetUnionIsStrictlyIncreasing({
        spendInputItemCbors: ["80"],
        referenceInputItemCbors: [],
      }),
    ).toThrow(/canonical 38-byte out-ref/);
  });

  it("binds the complete authenticated forced reason and all positions", () => {
    expect(bindForcedDuplicateInput(subject)).toStrictEqual({
      subject,
      ...reason.DuplicateInput,
    });
    expect(() =>
      bindForcedDuplicateInput(acceptedVerdictSubject("aa".repeat(32))),
    ).toThrow(/forced subject/);
  });

  it("checkpoints every resume coordinate and successor script", () => {
    const bound = bindForcedDuplicateInput(subject);
    const base = {
      bound,
      spendCount: 1n,
      referenceCount: 1n,
      cursor: 1n,
      previousItem: isuItemCbor(isuOutRef("11", 0)),
      nextExpectedScriptHash: "cc".repeat(28),
    };
    const checkpoint = inputSetUniquenessCheckpoint(base);
    expect(checkpoint).toMatch(/^[0-9a-f]{64}$/u);
    expect(inputSetUniquenessCheckpoint({ ...base, cursor: 2n })).not.toBe(
      checkpoint,
    );
    expect(
      inputSetUniquenessCheckpoint({
        ...base,
        nextExpectedScriptHash: "dd".repeat(28),
      }),
    ).not.toBe(checkpoint);
  });

  it("derives the contradiction only from an authenticated forced leaf", async () => {
    const fixture = await buildInputSetUniquenessFixture({
      spendInputs: [isuOutRef("11", 0)],
      referenceInputs: [isuOutRef("22", 0)],
      validity: "TxIsInvalid",
    });
    const detections = detectInputSetUniquenessForcedReplay({
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
      isuOutRef("aa", index),
    );
    const fixture = await buildInputSetUniquenessFixture({
      spendInputs: [isuOutRef("00", 0)],
      referenceInputs: references,
      validity: "TxIsInvalid",
    });
    expect(() =>
      planFaultProofFieldOpening({
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
      keyof InputSetUniquenessForcedInput
    >().toEqualTypeOf<"block">();
    expect(Object.keys({ block: null })).toStrictEqual(["block"]);
  });
});
