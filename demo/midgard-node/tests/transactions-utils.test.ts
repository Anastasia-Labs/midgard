import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  parseOutsideValidityIntervalDetails,
  resolveEarlyValidityRetryDelayMs,
  submitSignedTxWithRecovery,
} from "@/transactions/utils.js";

describe("parseOutsideValidityIntervalDetails", () => {
  it("parses escaped Kupmios/Ogmios early-validity submit errors", () => {
    const message =
      'KupmiosError: ResponseError: {"jsonrpc":"2.0","method":"submitTransaction","error":{"code":3118,"message":"The transaction is outside of its validity interval.","data":{"validityInterval":{"invalidBefore":123415253,"invalidAfter":123415372},"currentSlot":123415249}},"id":null}'.replace(
        /"/g,
        '\\"',
      );

    expect(parseOutsideValidityIntervalDetails(message)).toEqual({
      invalidBeforeSlot: 123415253,
      invalidHereafterSlot: 123415372,
      currentSlot: 123415249,
    });
  });
});

describe("validity-window submit recovery", () => {
  const outsideValidityError = new Error(
    "OutsideValidityIntervalUTxO (ValidityInterval {invalidBefore = SJust (SlotNo 10), invalidHereafter = SJust (SlotNo 20)}) (SlotNo 7)",
  );

  const fakeLucid = {
    awaitTx: vi.fn(),
  };

  it("computes bounded retry delays for early-validity provider errors", () => {
    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 10,
          invalidHereafterSlot: 20,
          currentSlot: 7,
        },
        0,
      ),
    ).toBe(5_000);

    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 100,
          invalidHereafterSlot: 105,
          currentSlot: 50,
        },
        0,
      ),
    ).toBe(2_000);

    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 10,
          invalidHereafterSlot: 20,
          currentSlot: 7,
        },
        6,
      ),
    ).toBeNull();
  });

  it("does not reset exhausted early-validity retries through a second outer retry", async () => {
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.fail(outsideValidityError));
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-validity",
          {
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitProgram).toHaveBeenCalledTimes(7);
    expect(waits).toEqual([5_000, 5_000, 5_000, 5_000, 5_000, 5_000]);
  });

  it("keeps a single generic provider retry in the submit recovery loop", async () => {
    const waits: number[] = [];
    let calls = 0;
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls === 1 ? Effect.fail(new Error("fetch failed")) : Effect.void;
    });
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-provider",
          {
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(waits).toEqual([2_000]);
  });
});
