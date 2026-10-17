import { L1SubmitRejectedError } from "@al-ft/midgard-l1-follower/provider";
import { CML, OgmiosJsonRpcError, TxSubmitError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  inspectSignedTxValidityInterval,
  isNoInlineSubmitDefer,
  NoInlineSubmitDefer,
  parseOutsideValidityIntervalDetails,
  submitSignedTxWithRecovery,
} from "../src/transactions/utils.js";
import { runWithoutFollower, TEST_INTENT } from "./helpers/intent-journal.js";

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

// The follower and local-node accesses (the only L1 access of a role
// process) refuse with the ledger's own bytes, which lucid wraps in a
// TxSubmitError. Rejections cardano-node returned in devnet run r4.
describe("follower-shaped submit rejections", () => {
  const followerRefusal = (hex: string) =>
    new TxSubmitError({
      cause: new L1SubmitRejectedError(
        "ab".repeat(32),
        Buffer.from(hex, "hex"),
      ),
    });
  // A merge with no upper bound, 19 slots ahead of the ledger slot.
  const mergeTooEarly = "81820681820182008302828119fb688019fb55";
  const badInputs =
    "81820684820182008201d90102818258205aabeb87fda9424f6be6669f1d9b15f3f519b5143109356ce085398d3a2253cd008201820083061b000046e71af4b3cd821b000046e71b086e95a1581c50bcd6fdccb571a0c90e63a0dc9c81c19dc4d3794b5e2ba12c6ce436a1514d4944474152445f5343484544554c4552018201831282815820b78fcf7fc71b5e46a01235ec40d98c0dbc6b4e0871f03771cc8bc2aebece0b9c81582053649b6e51217dcf0f698622efe62fbd4c46f3b649c4101f017cf2a2acbc89de81581d81840000d8799f0000d87a9f020103ffff821a00053c8c1a06fe8bbba08201820f81820000";
  const mempoolInputsSpent =
    "8182068182077844416c6c20696e7075747320617265207370656e742e205472616e73616374696f6e206861732070726f6261626c7920616c7265616479206265656e20696e636c75646564";

  it("parses an OutsideValidityIntervalUTxO refusal from its ledger bytes", () => {
    expect(
      parseOutsideValidityIntervalDetails(followerRefusal(mergeTooEarly)),
    ).toEqual({ invalidBeforeSlot: 64_360, currentSlot: 64_341 });
    expect(
      parseOutsideValidityIntervalDetails(
        followerRefusal("81820681820182008302828119dbd38119ddb319dbd1"),
      ),
    ).toEqual({
      invalidBeforeSlot: 56_275,
      invalidHereafterSlot: 56_755,
      currentSlot: 56_273,
    });
  });

  it("parses no validity details from any other refusal", () => {
    // BadInputsUTxO (with the failures it brings), and the mempool's text.
    for (const hex of [badInputs, mempoolInputsSpent]) {
      expect(parseOutsideValidityIntervalDetails(followerRefusal(hex))).toBe(
        null,
      );
    }
  });

  const submitOnce = (noInline = false, refusal = mergeTooEarly) => {
    const waits: number[] = [];
    const sleep = (milliseconds: number) =>
      Effect.sync(() => waits.push(milliseconds));
    let calls = 0;
    const submitProgram = vi.fn(() =>
      ++calls === 1 ? Effect.fail(followerRefusal(refusal)) : Effect.void,
    );
    const awaitTxConfirmation = vi.fn(async (txHash: string) => ({ txHash }));
    const run = runWithoutFollower(
      Effect.either(
        submitSignedTxWithRecovery(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
          } as never,
          { submitProgram, toCBOR: () => signedTxCbor({}) } as never,
          "tx-follower-too-early",
          TEST_INTENT,
          noInline
            ? {
                inlineWaitPolicy: "defer_positive_wait",
                noInlineSubmitDefer: {
                  key: "merge-key",
                  dependencyKey: "merge-dep",
                  invalidationKey: "merge-inv",
                },
                sleep,
              }
            : { sleep },
        ),
      ),
    );
    return { run, waits, submitProgram, awaitTxConfirmation };
  };

  it("takes the bounded early-validity wait for a refusal before the lower bound", async () => {
    const { run, waits, submitProgram } = submitOnce();
    expect((await run)._tag).toBe("Right");
    // Until the lower bound plus the 2-slot submit margin.
    expect(waits).toEqual([21_000]);
    expect(submitProgram).toHaveBeenCalledTimes(2);
  });

  it("defers it, without sleeping or resending, in no-inline mode", async () => {
    const { run, waits, submitProgram } = submitOnce(true);
    const result = await run;
    expect(
      result._tag === "Left" && expectNoInlineSubmitDefer(result.left),
    ).toMatchObject({
      kind: "early_validity_recovery",
      currentSlot: 64_341,
      targetSlot: 64_362,
      invalidBeforeSlot: 64_360,
    });
    expect(waits).toEqual([]);
    expect(submitProgram).toHaveBeenCalledTimes(1);
  });

  it("confirms the exact transaction, without resending, when the ledger finds an input spent", async () => {
    const { run, waits, submitProgram, awaitTxConfirmation } = submitOnce(
      false,
      badInputs,
    );
    expect((await run)._tag).toBe("Right");
    expect(awaitTxConfirmation).toHaveBeenCalledWith("tx-follower-too-early", {
      timeout: 90_000,
      checkInterval: 5_000,
    });
    expect(waits).toEqual([]);
    expect(submitProgram).toHaveBeenCalledTimes(1);
  });

  // The mempool's "All inputs are spent" refusal is not a ledger verdict, so it
  // keeps the generic path, as Ogmios's 3997 does.
  it.each([
    ["the mempool's all-inputs-spent text", mempoolInputsSpent],
    ["a fee refusal", "818206818201820083050201"],
  ])("keeps the generic retry for %s", async (_name, refusal) => {
    const { run, waits, submitProgram, awaitTxConfirmation } = submitOnce(
      false,
      refusal,
    );
    expect((await run)._tag).toBe("Right");
    expect(waits).toEqual([2_000]);
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(awaitTxConfirmation).not.toHaveBeenCalled();
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
