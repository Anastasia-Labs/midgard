import "./da-bond-pool-live-port.da-bond-pool-live-port-availability-inclusion-wait.js";

import { OgmiosJsonRpcError, TxSubmitError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  PublishedTransactionExpiredError,
  PublishedTransactionSubmissionError,
} from "midgard-watcher/tests/support/published-block-actor";
import { describe, expect, it } from "vitest";

import {
  commitWithinLedgerValidity,
  errorChainTexts,
  settleExpiredCommitReads,
  validityIntervalRefusal,
} from "./da-bond-pool-live-port.js";

describe("DA bond pool live port: header commit validity", () => {
  // What `signed.submit()` rejects with: Lucid's TxSubmitError around the
  // provider's error, inside the FiberFailure of Effect.runPromise. The data
  // is the refusal the devnet's Ogmios logged for the failed B3 commit.
  const submitFailure = (code: number, message: string, data: unknown) =>
    Effect.runPromise(
      Effect.fail(
        new TxSubmitError({
          cause: new OgmiosJsonRpcError({
            code,
            message,
            data,
            method: "submitTransaction",
            id: null,
          }),
        }),
      ),
    ).catch((error: unknown) => error);
  const outsideValidity = () =>
    submitFailure(
      3118,
      "The transaction is outside of its validity interval.",
      {
        validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
        currentSlot: 3193,
      },
    );
  const spentInputs = () =>
    submitFailure(3997, "The transaction couldn't be added to the mempool.", {
      error: "All inputs are spent.",
    });
  const refused = async (cause: Promise<unknown>) =>
    new PublishedTransactionSubmissionError("e1bb026a", await cause);

  it("finds the refusal through the FiberFailure and the cause chain", async () => {
    const error = await refused(outsideValidity());
    expect(error.message).toBe("Header submission e1bb026a is unresolved");
    expect(errorChainTexts(error).join(" | ")).toContain('"currentSlot":3193');
    expect(validityIntervalRefusal(error)).toContain('"invalidBefore":3210');
    expect(validityIntervalRefusal(await refused(spentInputs()))).toBe(
      undefined,
    );
    expect(validityIntervalRefusal(new Error("ScriptFailure"))).toBe(undefined);
  });

  const expired = (hash = "e1bb026a") =>
    new PublishedTransactionExpiredError("header commit B3", hash, 1_000);
  const run = (
    outcomes: readonly (() => Promise<unknown>)[],
    maxAttempts = 3,
    settle: (
      error: PublishedTransactionExpiredError,
    ) => Promise<unknown> = async () => undefined,
  ) => {
    const calls: string[] = [];
    const lines: string[] = [];
    const result = commitWithinLedgerValidity({
      label: "commit B3",
      awaitFreshTip: async () => {
        calls.push("fresh");
      },
      settleExpired: async (error) => {
        calls.push(`settle ${error.txHash}`);
        return settle(error);
      },
      refreshWallet: async () => {
        calls.push("refresh");
      },
      submit: async (attempt) => {
        calls.push(`submit ${attempt}`);
        const outcome = await outcomes[attempt - 1]!();
        if (outcome instanceof Error) throw outcome;
        return outcome;
      },
      maxAttempts,
      log: (line) => lines.push(line),
    });
    return { result, calls, lines };
  };

  it("rebuilds on a fresh tip after a validity-interval refusal", async () => {
    const { result, calls, lines } = run([
      () => refused(outsideValidity()),
      async () => "landed",
    ]);
    await expect(result).resolves.toBe("landed");
    expect(calls).toEqual(["fresh", "submit 1", "fresh", "submit 2"]);
    expect(lines).toHaveLength(1);
    expect(lines[0]).toContain(
      "commit B3: submission e1bb026a refused outside its validity interval",
    );
  });

  it("fails at once, naming the reason, on any other submission refusal", async () => {
    const { result, calls, lines } = run([() => refused(spentInputs())]);
    await expect(result).rejects.toBeInstanceOf(
      PublishedTransactionSubmissionError,
    );
    expect(calls).toEqual(["fresh", "submit 1"]);
    expect(lines[0]).toContain("All inputs are spent");
    expect(calls).not.toContain(expect.stringMatching(/^settle/u));
  });

  it("does not retry an error raised outside the submission", async () => {
    const { result, calls } = run([
      async () =>
        new Error(
          'Header awaited past its bound: {"validityInterval":{"invalidBefore":1}}',
        ),
    ]);
    await expect(result).rejects.toThrow("Header awaited past its bound");
    expect(calls).toEqual(["fresh", "submit 1"]);
  });

  it("rebuilds a commit that expired unminted", async () => {
    const { result, calls, lines } = run([
      async () => expired(),
      async () => "landed",
    ]);
    await expect(result).resolves.toBe("landed");
    expect(calls).toEqual([
      "fresh",
      "submit 1",
      "settle e1bb026a",
      "fresh",
      "submit 2",
    ]);
    expect(lines[0]).toContain("expired unminted past its validity bound");
  });

  it("adopts an expired commit that landed after all", async () => {
    const { result, calls, lines } = run(
      [async () => expired(), async () => "rebuilt"],
      3,
      async () => "adopted",
    );
    await expect(result).resolves.toBe("adopted");
    // The actor's wallet pin predates the landed commit: drop it once.
    expect(calls).toEqual(["fresh", "submit 1", "settle e1bb026a", "refresh"]);
    expect(lines[0]).toContain("adopting it");
  });

  it("fails when an expired commit cannot be settled", async () => {
    const failure = expired();
    const { result, calls } = run(
      [async () => failure, async () => "rebuilt"],
      3,
      async (error) => {
        throw error;
      },
    );
    await expect(result).rejects.toBe(failure);
    expect(calls).toEqual(["fresh", "submit 1", "settle e1bb026a"]);
  });

  it("fails when the last attempt expires unminted", async () => {
    const { result, calls } = run(
      [async () => expired(), async () => expired(), async () => expired()],
      3,
    );
    await expect(result).rejects.toBeInstanceOf(
      PublishedTransactionExpiredError,
    );
    expect(calls.filter((call) => call.startsWith("submit"))).toHaveLength(3);
    expect(calls.filter((call) => call.startsWith("settle"))).toHaveLength(3);
  });

  it("stops after the last attempt", async () => {
    const { result, calls, lines } = run(
      [() => refused(outsideValidity()), () => refused(outsideValidity())],
      2,
    );
    await expect(result).rejects.toBeInstanceOf(
      PublishedTransactionSubmissionError,
    );
    expect(calls).toEqual(["fresh", "submit 1", "fresh", "submit 2"]);
    expect(lines).toHaveLength(2);
    expect(lines[1]).toContain('"currentSlot":3193');
  });

  describe("settles an expired commit from its reads", () => {
    const commit = "c0".repeat(32);
    const apply = "a0".repeat(32);
    const other = "0f".repeat(32);
    const settle = (
      reads: Partial<Parameters<typeof settleExpiredCommitReads>[0]>,
    ) =>
      settleExpiredCommitReads({
        txId: commit,
        stable: true,
        anchorSpentBy: null,
        headerHolders: [],
        read: 1,
        maxReads: 3,
        ...reads,
      });

    it("adopts a commit whose own transaction spent the anchor", () => {
      expect(settle({ anchorSpentBy: commit, headerHolders: [commit] })).toBe(
        "adopt",
      );
      // A DA Apply spent the header output and recreated it.
      expect(settle({ anchorSpentBy: commit, headerHolders: [apply] })).toBe(
        "adopt",
      );
      expect(settle({ anchorSpentBy: commit, headerHolders: [] })).toBe(
        "adopt",
      );
    });

    it("rebuilds only when the anchor is unspent and no output holds the header", () => {
      expect(settle({})).toBe("absent");
    });

    it("refuses every other read as a conflict", () => {
      expect(settle({ anchorSpentBy: other })).toBe("conflict");
      expect(settle({ anchorSpentBy: other, headerHolders: [other] })).toBe(
        "conflict",
      );
      expect(settle({ headerHolders: [commit] })).toBe("conflict");
      expect(
        settle({ anchorSpentBy: commit, headerHolders: [commit, apply] }),
      ).toBe("conflict");
    });

    it("rereads a moving boundary up to the cap", () => {
      expect(settle({ stable: false, anchorSpentBy: commit, read: 2 })).toBe(
        "reread",
      );
      expect(settle({ stable: false, read: 3 })).toBe("unsettled");
    });
  });
});
