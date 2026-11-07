import { describe, expect, it } from "vitest";

import {
  NoInlineSubmitDefer,
  type NoInlineSubmitDeferKind,
  TxSubmitError,
} from "../src/transactions/utils.js";
import { commitSubmitDeferError } from "../src/workers/commit-block-header/build-unsigned-tx.js";

const HEADER_HASH = "ab".repeat(28);

const defer = (kind: NoInlineSubmitDeferKind) =>
  new NoInlineSubmitDefer({
    callerLabel: "commit-block",
    kind,
    key: `commit-block:${HEADER_HASH}`,
    currentSlot: 32_862,
    targetSlot: 32_882,
    dueSlot: 32_883,
    waitMs: 21_000,
    slotSource: "ogmios",
    dependencyKey: `commit-block:${HEADER_HASH}`,
    invalidationKey: `commit-block:${HEADER_HASH}`,
  });

describe("commit submit defer error", () => {
  it("carries the due slot of a provider refusal of the persisted intent as a field", () => {
    const error = commitSubmitDeferError(
      defer("provider_slot_wait"),
      HEADER_HASH,
    );
    expect(error).toBeInstanceOf(TxSubmitError);
    expect(error.dueSlot).toBe(32_883);
    expect(error.txHash).toBe(HEADER_HASH);
    expect(error.cause).toBeInstanceOf(NoInlineSubmitDefer);
    expect(error.message).toMatch(
      /retained for rebroadcast of its exact bytes/,
    );
  });

  it("carries the due slot of a pre-submit defer, which sent nothing", () => {
    const error = commitSubmitDeferError(
      defer("pre_submit_validity"),
      HEADER_HASH,
    );
    expect(error.dueSlot).toBe(32_883);
    expect(error.message).toMatch(/nothing was sent/);
    expect(error.message).not.toMatch(/rebroadcast/);
  });

  it("leaves the due slot unset on a submit error that was not a defer", () => {
    const error = new TxSubmitError({
      message: "provider refused",
      txHash: HEADER_HASH,
      cause: new Error("provider refused"),
    });
    expect(error.dueSlot).toBeUndefined();
  });
});
