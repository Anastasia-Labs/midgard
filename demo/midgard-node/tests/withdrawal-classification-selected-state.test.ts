import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as Ledger from "@/database/utils/ledger.js";
import { shouldHydrateCommitBaseEntries } from "@/workers/commit-block-header.js";
import { indexSelectedLedgerOutputs } from "@/workers/utils/mpf/withdrawal-classification.js";

describe("withdrawal classification selected state", () => {
  it("indexes only the authenticated selected ledger snapshot", async () => {
    const outRef = Buffer.from("01", "hex");
    const output = Buffer.from("02", "hex");
    const selected = await Effect.runPromise(
      indexSelectedLedgerOutputs([
        {
          [Ledger.Columns.OUTREF]: outRef,
          [Ledger.Columns.OUTPUT]: output,
        },
      ]),
    );

    outRef[0] = 0xff;
    output[0] = 0xff;
    expect(selected.get("01")?.toString("hex")).toBe("02");
    expect(selected.get("ff")).toBeUndefined();
    expect(selected.get("03")).toBeUndefined();
  });

  it("rejects duplicate outrefs in the selected ledger snapshot", async () => {
    await expect(
      Effect.runPromise(
        indexSelectedLedgerOutputs([
          {
            [Ledger.Columns.OUTREF]: Buffer.from("01", "hex"),
            [Ledger.Columns.OUTPUT]: Buffer.from("02", "hex"),
          },
          {
            [Ledger.Columns.OUTREF]: Buffer.from("01", "hex"),
            [Ledger.Columns.OUTPUT]: Buffer.from("03", "hex"),
          },
        ]),
      ),
    ).rejects.toThrow(
      /Failed to index selected ledger snapshot for withdrawal classification/u,
    );
  });

  it("requires exact base entries whenever a withdrawal is due", () => {
    expect(
      shouldHydrateCommitBaseEntries({
        payloadRootCheck: "periodic",
        recordCorpus: "",
        pendingWithdrawalCount: 1,
      }),
    ).toBe(true);
    expect(
      shouldHydrateCommitBaseEntries({
        payloadRootCheck: "periodic",
        recordCorpus: "",
        pendingWithdrawalCount: 0,
      }),
    ).toBe(false);
    expect(
      shouldHydrateCommitBaseEntries({
        payloadRootCheck: "every_block",
        recordCorpus: "",
        pendingWithdrawalCount: 0,
      }),
    ).toBe(true);
  });
});
