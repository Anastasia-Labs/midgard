import { OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  isUnknownOutputReferenceSubmitError,
  submitSignedTxWithRecovery,
} from "../src/transactions/utils.js";
import { runWithoutFollower, TEST_INTENT } from "./helpers/intent-journal.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

/** The Ogmios 3117 rejection a devnet node logged for a signed commit whose
 * base tail another transaction had spent. The only unknown-input evidence
 * is in the text: the `cause` chain is not enumerable, so no structured
 * `unknownOutputReferences` field is reachable. */
const OGMIOS_3117_DESCRIPTION =
  "Ogmios JSON-RPC error 3117: The transaction contains unknown UTxO references as inputs. This can happen if the inputs you're trying to spend have already been spent, or if you've simply referred to non-existing UTxO altogether. The field 'data.unknownOutputReferences' indicates all unknown inputs.";
const OGMIOS_3117_DATA =
  '{"unknownOutputReferences":[{"transaction":{"id":"781ab70de6d012c855658d05353de5ef7af55169196be31b7de5fa1e9e55435e"},"index":0}]}';
const OGMIOS_3117_BODY =
  '{"body":{"jsonrpc":"2.0","method":"submitTransaction","id":null,"error":{"code":3117,"message":"The transaction contains unknown UTxO references as inputs. This can happen if the inputs you\'re trying to spend have already been spent, or if you\'ve simply referred to non-existing UTxO altogether. The field \'data.unknownOutputReferences\' indicates all unknown inputs.","data":{"unknownOutputReferences":[{"transaction":{"id":"781ab70de6d012c855658d05353de5ef7af55169196be31b7de5fa1e9e55435e"},"index":0}]}}},"status":400,"headers":{"access-control-allow-origin":"*","content-type":"application/json; charset=utf-8","date":"Wed, 30 Sep 2026 17:01:09 GMT","server":"Warp/3.4.13","transfer-encoding":"chunked"}}';
const OGMIOS_3117_INNER = `OgmiosJsonRpcError: ${OGMIOS_3117_DESCRIPTION}: ${OGMIOS_3117_DATA}`;
const LIVE_3117_SUBMIT_ERROR = `TxSubmitError: ${OGMIOS_3117_INNER}; cause=${OGMIOS_3117_INNER}; cause=${OGMIOS_3117_BODY}`;
const LIVE_3117_TX_HASH =
  "378e3ff088ab820ca7454704bb9ba6c1584c6df8e33eddaa59982995750fff09";
const LIVE_3117_WRAPPER = `Tx ${LIVE_3117_TX_HASH} submit failed with provider error in no-inline mode; refusing provider retry sleep under ownership: ${LIVE_3117_SUBMIT_ERROR}`;

/** An error chain shaped like the live one: messages only, with each cause
 * attached non-enumerably, as `Error` does. */
const live3117ErrorChain = (): Error => {
  const body = new Error(OGMIOS_3117_BODY);
  const ogmios = new Error(OGMIOS_3117_INNER, { cause: body });
  return new Error(OGMIOS_3117_INNER, { cause: ogmios });
};

const noInline = {
  inlineWaitPolicy: "defer_positive_wait",
  noInlineSubmitDefer: {
    key: "commit-block:unknown-input",
    dependencyKey: "dep-unknown-input",
    invalidationKey: "inv-unknown-input",
  },
  unknownInputsFailFast: true,
} as const;

describe("unknown output reference submit errors", () => {
  it("matches the live Ogmios 3117 rejection carried only as text", () => {
    expect(isUnknownOutputReferenceSubmitError(LIVE_3117_SUBMIT_ERROR)).toBe(
      true,
    );
    expect(isUnknownOutputReferenceSubmitError(LIVE_3117_WRAPPER)).toBe(true);
    expect(
      isUnknownOutputReferenceSubmitError(new Error(LIVE_3117_WRAPPER)),
    ).toBe(true);
    expect(isUnknownOutputReferenceSubmitError(live3117ErrorChain())).toBe(
      true,
    );
  });

  it("does not match validity-interval or generic provider errors", () => {
    for (const error of [
      new Error(
        "OutsideValidityIntervalUTxO (ValidityInterval {invalidBefore = SJust (SlotNo 10), invalidHereafter = SJust (SlotNo 20)}) (SlotNo 7)",
      ),
      new OgmiosJsonRpcError({
        code: 3118,
        message: "The transaction is outside of its validity interval.",
        data: {
          validityInterval: { invalidBefore: 10, invalidAfter: 20 },
          currentSlot: 7,
        },
        method: "submitTransaction",
        id: null,
      }),
      new Error("Lower bound (120) not in slot range (100)"),
      new Error("fetch failed"),
      new Error("Ogmios JSON-RPC error 31170: unrelated"),
      '{"code":31171,"message":"unrelated"}',
      new Error("socket hang up", { cause: new Error("ECONNRESET") }),
    ]) {
      expect(isUnknownOutputReferenceSubmitError(error)).toBe(false);
    }
  });

  it("fails fast for a no-inline commit caller without an inline confirmation wait", async () => {
    for (const submitFailure of [
      live3117ErrorChain(),
      new OgmiosJsonRpcError({
        code: 3117,
        message: "Unknown inputs",
        data: {
          unknownOutputReferences: [{ transaction: { id: "abc" }, index: 0 }],
        },
        method: "submitTransaction",
        id: null,
      }),
    ]) {
      const waits: number[] = [];
      const submitProgram = vi.fn(() => Effect.fail(submitFailure));
      const awaitTxConfirmation = vi.fn(async () => ({
        txHash: LIVE_3117_TX_HASH,
      }));

      const result = await runWithoutFollower(
        Effect.either(
          submitSignedTxWithRecovery(
            {
              config: () => ({ provider: undefined }),
              awaitTxConfirmation,
            } as never,
            { submitProgram, toCBOR: () => signedTxCbor({}) } as never,
            LIVE_3117_TX_HASH,
            TEST_INTENT,
            {
              ...noInline,
              sleep: (milliseconds) =>
                Effect.sync(() => waits.push(milliseconds)),
            },
          ),
        ),
      );

      expect(result._tag).toBe("Left");
      const failure = (result as { readonly left: Error }).left;
      expect(failure.message).toContain(
        "submit reported unknown inputs in no-inline mode",
      );
      expect(failure.cause).toBe(submitFailure);
      expect(submitProgram).toHaveBeenCalledTimes(1);
      expect(awaitTxConfirmation).not.toHaveBeenCalled();
      expect(waits).toEqual([]);
    }
  });

  it("keeps the exact-confirmation check for other no-inline callers (merge, scheduler refresh)", async () => {
    const submitProgram = vi.fn(() => Effect.fail(live3117ErrorChain()));
    const awaitTxConfirmation = vi.fn(async () => ({
      txHash: LIVE_3117_TX_HASH,
    }));
    const { unknownInputsFailFast: _, ...otherCaller } = noInline;

    const result = await runWithoutFollower(
      Effect.either(
        submitSignedTxWithRecovery(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
          } as never,
          { submitProgram, toCBOR: () => signedTxCbor({}) } as never,
          LIVE_3117_TX_HASH,
          TEST_INTENT,
          { ...otherCaller, sleep: () => Effect.void },
        ),
      ),
    );

    // Its own earlier submission was accepted: the race is a success.
    expect(result._tag).toBe("Right");
    expect(awaitTxConfirmation).toHaveBeenCalledTimes(1);
  });

  it("keeps the generic no-inline refusal for errors that are not unknown inputs", async () => {
    const submitProgram = vi.fn(() => Effect.fail(new Error("fetch failed")));
    const awaitTxConfirmation = vi.fn();

    const result = await runWithoutFollower(
      Effect.either(
        submitSignedTxWithRecovery(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
          } as never,
          { submitProgram, toCBOR: () => signedTxCbor({}) } as never,
          LIVE_3117_TX_HASH,
          TEST_INTENT,
          { ...noInline, sleep: () => Effect.void },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(String((result as { readonly left: unknown }).left)).toContain(
      "refusing provider retry sleep under ownership",
    );
    expect(awaitTxConfirmation).not.toHaveBeenCalled();
  });
});
