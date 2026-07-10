import { Option } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "@/database/index.js";
import {
  findEarliestCanonicalPayloadJournal,
  localJournalHasPayloadMembers,
  type CanonicalCommittedHeader,
} from "@/services/canonical-journal-recovery.js";

const headerHash = (byte: number): Buffer => Buffer.alloc(28, byte);

const journal = ({
  hash,
  status,
  payloadMembers = false,
}: {
  readonly hash: Buffer;
  readonly status: PendingBlockFinalizationsDB.Status;
  readonly payloadMembers?: boolean;
}): PendingBlockFinalizationsDB.Record =>
  ({
    [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: hash,
    [PendingBlockFinalizationsDB.Columns.STATUS]: status,
    depositEventIds: payloadMembers ? [Buffer.from([1])] : [],
    forcedTransactionEventIds: [],
    withdrawalEventIds: [],
    mempoolTxIds: [],
  }) as unknown as PendingBlockFinalizationsDB.Record;

const canonicalHeader = (
  byte: number,
  localJournal: PendingBlockFinalizationsDB.Record,
): CanonicalCommittedHeader => ({
  headerHash: headerHash(byte),
  endTimeMs: byte,
  journal: Option.some(localJournal),
});

describe("canonical journal recovery", () => {
  it("recognizes local payload-bearing journals", () => {
    expect(
      localJournalHasPayloadMembers(
        journal({
          hash: headerHash(1),
          status: PendingBlockFinalizationsDB.Status.Abandoned,
          payloadMembers: true,
        }),
      ),
    ).toBe(true);
    expect(
      localJournalHasPayloadMembers(
        journal({
          hash: headerHash(2),
          status: PendingBlockFinalizationsDB.Status.Abandoned,
        }),
      ),
    ).toBe(false);
  });

  it("selects the earliest abandoned canonical journal with payload members", () => {
    const emptyAbandoned = journal({
      hash: headerHash(1),
      status: PendingBlockFinalizationsDB.Status.Abandoned,
    });
    const earliestPayload = journal({
      hash: headerHash(2),
      status: PendingBlockFinalizationsDB.Status.Abandoned,
      payloadMembers: true,
    });
    const laterPayload = journal({
      hash: headerHash(3),
      status: PendingBlockFinalizationsDB.Status.Abandoned,
      payloadMembers: true,
    });
    const activePayload = journal({
      hash: headerHash(4),
      status: PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
      payloadMembers: true,
    });

    const selected = findEarliestCanonicalPayloadJournal([
      canonicalHeader(1, emptyAbandoned),
      canonicalHeader(2, earliestPayload),
      canonicalHeader(3, laterPayload),
      canonicalHeader(4, activePayload),
    ]);

    expect(selected._tag).toBe("Some");
    if (selected._tag === "Some") {
      expect(selected.value.headerHash).toEqual(headerHash(2));
    }
  });
});
