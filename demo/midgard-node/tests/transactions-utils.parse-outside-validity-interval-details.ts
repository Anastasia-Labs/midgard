import { CML, OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  inspectSignedTxValidityInterval,
  isNoInlineSubmitDefer,
  NoInlineSubmitDefer,
  parseOutsideValidityIntervalDetails,
} from "../src/transactions/utils.js";

describe("parseOutsideValidityIntervalDetails", () => {
  it("parses typed Kupmios/Ogmios early-validity submit errors", () => {
    const error = new OgmiosJsonRpcError({
      code: 3118,
      message: "The transaction is outside of its validity interval.",
      data: {
        validityInterval: {
          invalidBefore: 123415253,
          invalidAfter: 123415372,
        },
        currentSlot: 123415249,
      },
      method: "submitTransaction",
      id: null,
    });

    expect(parseOutsideValidityIntervalDetails(error)).toEqual({
      invalidBeforeSlot: 123415253,
      invalidHereafterSlot: 123415372,
      currentSlot: 123415249,
    });
  });

  it("parses structured Ogmios 3118 errors through causes", () => {
    expect(
      parseOutsideValidityIntervalDetails(
        new Error("submit failed", {
          cause: {
            error: {
              code: 3118,
              data: {
                validityInterval: {
                  invalidBefore: 126544954,
                  invalidAfter: 126545100,
                },
                currentSlot: 126544938,
              },
            },
          },
        }),
      ),
    ).toEqual({
      invalidBeforeSlot: 126544954,
      invalidHereafterSlot: 126545100,
      currentSlot: 126544938,
    });
  });

  it("parses lower-bound-only Ogmios 3118 errors as early-validity details", () => {
    expect(
      parseOutsideValidityIntervalDetails({
        error: {
          code: 3118,
          data: {
            validityInterval: {
              invalidBefore: 126544954,
            },
            currentSlot: 126544938,
          },
        },
      }),
    ).toEqual({
      invalidBeforeSlot: 126544954,
      currentSlot: 126544938,
    });
  });
});

export const expectNoInlineSubmitDefer = (
  value: unknown,
): NoInlineSubmitDefer => {
  expect(isNoInlineSubmitDefer(value)).toBe(true);
  return value as NoInlineSubmitDefer;
};

export const signedTxCbor = ({
  invalidBeforeSlot,
  invalidHereafterSlot,
}: {
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
}): string => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  if (invalidBeforeSlot !== undefined) {
    body.set_validity_interval_start(BigInt(invalidBeforeSlot));
  }
  if (invalidHereafterSlot !== undefined) {
    body.set_ttl(BigInt(invalidHereafterSlot));
  }
  const tx = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const cbor = tx.to_cbor_hex();
  expect(inspectSignedTxValidityInterval(cbor)).toEqual({
    ...(invalidBeforeSlot === undefined ? {} : { invalidBeforeSlot }),
    ...(invalidHereafterSlot === undefined ? {} : { invalidHereafterSlot }),
  });
  return cbor;
};
