import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { journalMatchesConfirmedHeader } from "../src/workers/commit-block-header/submission.recover-local-finalization-against-confirmed-block.js";
import { buildJournalFixture } from "./da-payload.build-journal-fixture.js";
import { retainedPairs, sourceRoot } from "./da-payload.record.js";

// A journal for a block with one transaction: it keeps the built
// validation-trace pair, and its header pins the pair to the empty tree.
const honestJournal = async () => {
  const builtRoot = await Effect.runPromise(
    sourceRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      retainedPairs("recovered-validation", 1),
    ),
  );
  const { pending, header } = await buildJournalFixture({
    txEntries: retainedPairs("recovered-tx", 1),
    transitionTraceEntries: retainedPairs("recovered-trace", 1),
    eventToStepEntries: retainedPairs("recovered-event-to-step", 1),
    recordRootOverrides: { validationTracesRoot: builtRoot },
  });
  const journal: PendingBlockFinalizationsDB.Record = {
    ...pending,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT]: 1n,
  };
  return { journal, header, builtRoot };
};

describe("local finalization recovery against the confirmed header", () => {
  it("recovers an honest block with a transaction whose header pins the validation-trace pair to the empty tree", async () => {
    const { journal, header } = await honestJournal();
    expect(header.l2TransactionCount).toBe(1n);
    expect(header.validationTracesRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(header.validationTraceCount).toBe(0n);
    expect(
      journalMatchesConfirmedHeader(
        journal,
        header,
        MIDGARD_CONSENSUS_PROFILE_ID,
      ),
    ).toBe(true);
  });

  it.each([
    ["root", { validationTracesRoot: true, validationTraceCount: false }],
    ["count", { validationTracesRoot: false, validationTraceCount: true }],
    ["pair", { validationTracesRoot: true, validationTraceCount: true }],
  ] as const)(
    "refuses a confirmed header that commits the built validation-trace %s",
    async (_, carried) => {
      const { journal, header, builtRoot } = await honestJournal();
      const confirmed: SDK.Header = {
        ...header,
        ...(carried.validationTracesRoot
          ? { validationTracesRoot: builtRoot }
          : {}),
        ...(carried.validationTraceCount ? { validationTraceCount: 1n } : {}),
      };
      expect(
        journalMatchesConfirmedHeader(
          journal,
          confirmed,
          MIDGARD_CONSENSUS_PROFILE_ID,
        ),
      ).toBe(false);
    },
  );
});
