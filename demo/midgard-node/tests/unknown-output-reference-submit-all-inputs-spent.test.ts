/**
 * Ogmios refuses a body whose inputs its mempool view already consumed with
 * JSON-RPC 3997 "All inputs are spent. Transaction has probably already been
 * included". On lc1 that was the node's own pending settlement body, resubmitted
 * while it sat in the mempool. It is not the ledger's unknown-input failure
 * (3117): the commit submitters abandon their attempt and rebuild over a tail
 * the unknown-input error names, which is wrong while their own transaction may
 * still land, so the unknown-input matcher leaves 3997 alone. Settlement reads
 * 3997 through its own rule (services/settlement-call.ts).
 */
import { OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  isUnknownOutputReferenceSubmitError,
  submitSignedTxWithRecovery,
  TxSubmitError,
} from "../src/transactions/utils.js";
import { submitErrorReferencesOutRef } from "../src/workers/commit-block-header/submission.run-with-stale-operator-wallet-retry.js";

const TX_HASH =
  "378e3ff088ab820ca7454704bb9ba6c1584c6df8e33eddaa59982995750fff09";
const TAIL_TX_HASH =
  "781ab70de6d012c855658d05353de5ef7af55169196be31b7de5fa1e9e55435e";
const TAIL_OUT_REF = `${TAIL_TX_HASH}#0`;

const allInputsSpent = () =>
  new OgmiosJsonRpcError({
    code: 3997,
    message:
      "All inputs are spent. Transaction has probably already been included",
    method: "submitTransaction",
    id: null,
  });
const ALL_INPUTS_SPENT_TEXT =
  "Ogmios JSON-RPC error 3997: All inputs are spent. Transaction has probably already been included";
const ALL_INPUTS_SPENT_BODY =
  '{"body":{"jsonrpc":"2.0","method":"submitTransaction","id":null,"error":{"code":3997,"message":"All inputs are spent. Transaction has probably already been included"}},"status":400}';

const unknownTail = () =>
  new OgmiosJsonRpcError({
    code: 3117,
    message: "The transaction contains unknown UTxO references as inputs.",
    data: {
      unknownOutputReferences: [
        { transaction: { id: TAIL_TX_HASH }, index: 0 },
      ],
    },
    method: "submitTransaction",
    id: null,
  });

const noInlineCommit = {
  inlineWaitPolicy: "defer_positive_wait",
  noInlineSubmitDefer: {
    key: "commit-block:all-inputs-spent",
    dependencyKey: "dep-all-inputs-spent",
    invalidationKey: "inv-all-inputs-spent",
  },
  unknownInputsFailFast: true,
} as const;

describe("Ogmios 3997 'All inputs are spent' submit errors", () => {
  it("are not unknown-input errors in any form a provider carries them", () => {
    for (const error of [
      allInputsSpent(),
      new Error(ALL_INPUTS_SPENT_TEXT, {
        cause: new Error(ALL_INPUTS_SPENT_BODY),
      }),
      ALL_INPUTS_SPENT_TEXT,
      ALL_INPUTS_SPENT_BODY,
      `TxSubmitError: OgmiosJsonRpcError: ${ALL_INPUTS_SPENT_TEXT}; cause=${ALL_INPUTS_SPENT_BODY}`,
    ]) {
      expect(isUnknownOutputReferenceSubmitError(error)).toBe(false);
    }
    expect(isUnknownOutputReferenceSubmitError(unknownTail())).toBe(true);
  });

  it("never make a commit submitter abandon its attempt over the tail, even when the text names it", () => {
    const naming = (cause: unknown) =>
      new TxSubmitError({
        message: `Failed to submit the commit spending ${TAIL_OUT_REF}`,
        cause,
        txHash: TX_HASH,
      });
    expect(
      submitErrorReferencesOutRef(naming(allInputsSpent()), TAIL_OUT_REF),
    ).toBe(false);
    expect(
      submitErrorReferencesOutRef(naming(unknownTail()), TAIL_OUT_REF),
    ).toBe(true);
  });

  it("keep the generic no-inline refusal for a fail-fast commit, without a confirmation wait", async () => {
    const submitProgram = vi.fn(() => Effect.fail(allInputsSpent()));
    const awaitTxConfirmation = vi.fn();

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
          } as never,
          { submitProgram } as never,
          TX_HASH,
          { ...noInlineCommit, sleep: () => Effect.void },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    const failure = String((result as { readonly left: unknown }).left);
    expect(failure).toContain("refusing provider retry sleep under ownership");
    expect(failure).toContain(ALL_INPUTS_SPENT_TEXT);
    expect(failure).not.toContain("reported unknown inputs");
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(awaitTxConfirmation).not.toHaveBeenCalled();
  });
});
