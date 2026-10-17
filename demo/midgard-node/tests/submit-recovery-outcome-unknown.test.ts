/**
 * After a send whose outcome is unknown (the follower provider's
 * `L1SubmitOutcomeUnknownError`), the submit recovery reads the exact id's
 * status before sending the same bytes again: a transaction that landed is
 * not sent again, and when the retries run out the outcome-unknown error is
 * the one reported, not a later retry's.
 */
import { L1SubmitOutcomeUnknownError } from "@al-ft/midgard-l1-follower/provider";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { submitSignedTxWithRecovery } from "../src/transactions/utils.js";
import { runWithoutFollower, TEST_INTENT } from "./helpers/intent-journal.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

const TX_HASH = "37".repeat(32);

/** What Lucid's `submitProgram` fails with: its `TxSubmitError` around the provider's. */
const lucidSubmitFailure = (cause: unknown) => ({
  _tag: "TxSubmitError",
  message: "Failed to submit transaction",
  cause,
});

/**
 * Sends the signed bytes with the provider's answers to each send (`sends`)
 * and to each status read of the id (`statuses`), on a provider whose slot
 * evidence never holds the send back.
 */
const submitWith = async (
  sends: readonly unknown[],
  statuses: readonly ("confirmed" | "pending" | "not_found" | "failed")[],
) => {
  const submitProgram = vi.fn(() => {
    const answer = sends[submitProgram.mock.calls.length - 1];
    return answer === undefined ? Effect.void : Effect.fail(answer);
  });
  const transactionStatus = vi.fn(async (txHash: string) => ({
    txHash,
    status: statuses[transactionStatus.mock.calls.length - 1] ?? "not_found",
  }));
  const result = await runWithoutFollower(
    Effect.either(
      submitSignedTxWithRecovery(
        {
          config: () => ({ provider: undefined }),
          transactionStatus,
        } as never,
        { submitProgram, toCBOR: () => signedTxCbor({}) } as never,
        TX_HASH,
        TEST_INTENT,
        { sleep: () => Effect.void },
      ),
    ),
  );
  return { result, submitProgram, transactionStatus };
};

describe("submit recovery after a send whose outcome is unknown", () => {
  const outcomeUnknown = () =>
    lucidSubmitFailure(
      new L1SubmitOutcomeUnknownError(TX_HASH, "request_timeout"),
    );

  it("does not send the bytes again once the transaction landed", async () => {
    const { result, submitProgram, transactionStatus } = await submitWith(
      [outcomeUnknown()],
      ["confirmed"],
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(transactionStatus).toHaveBeenCalledWith(TX_HASH);
  });

  it("sends the same bytes again while the transaction has not landed", async () => {
    const { result, submitProgram, transactionStatus } = await submitWith(
      [outcomeUnknown()],
      ["pending"],
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(transactionStatus).toHaveBeenCalledTimes(1);
  });

  it("reports the outcome-unknown error, not the later retry's, when the retries run out", async () => {
    const first = outcomeUnknown();
    const later = lucidSubmitFailure(new Error("socket hang up"));
    const { result, submitProgram } = await submitWith(
      [first, later],
      ["not_found"],
    );

    expect((result as { readonly left: unknown }).left).toBe(first);
    expect(submitProgram).toHaveBeenCalledTimes(2);
  });

  it("fails without sending again once the transaction landed phase-2 invalid", async () => {
    const first = outcomeUnknown();
    const { result, submitProgram } = await submitWith([first], ["failed"]);

    expect(result._tag).toBe("Left");
    const failure = (result as { readonly left: Error }).left;
    expect(failure.message).toBe(
      `Tx ${TX_HASH} landed phase-2 invalid after a submit with an unknown outcome`,
    );
    expect(failure.cause).toBe(first);
    expect(submitProgram).toHaveBeenCalledTimes(1);
  });

  it("reads no status after a send whose outcome is known", async () => {
    const refused = lucidSubmitFailure(new Error("socket hang up"));
    const { result, submitProgram, transactionStatus } = await submitWith(
      [refused, refused],
      [],
    );

    expect((result as { readonly left: unknown }).left).toBe(refused);
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(transactionStatus).not.toHaveBeenCalled();
  });
});
