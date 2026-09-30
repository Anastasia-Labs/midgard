import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
  WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
  WatcherHeaderRootReconstructionError,
} from "../../src/verification/header-root-reconstruction.js";
import {
  buildFixture,
  commitMutatedHeader,
  evaluateFixture,
  observationFor,
} from "./header-root-reconstruction.build-fixture.js";
import { corpusTransaction } from "./header-root-reconstruction.watcher-header-record.js";

// ---------------------------------------------------------------------------

describe("W22 authenticated header observation (non-circular binding)", () => {
  it("admits a state-queue header record and re-derives its header hash", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const observation = await observationFor(fixture);
    expect(observation.headerHash).toBe(fixture.headerHash);
    expect(observation.header).toStrictEqual(fixture.header);
    expect(observation.provenance.trustClass).toBe("authenticated_cardano_l1");
  });

  it("rejects a header hash that does not re-derive from the header fields", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        record: { ...fixture.record, headerHash: h28(0xab) },
      }),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
  });

  it("rejects a record whose datum bytes are not the re-encoding of its fields", async () => {
    const fixture = await buildFixture();
    const other = await buildFixture({ transactions: [corpusTransaction(0)] });
    await expect(
      observationFor(fixture, {
        record: {
          ...fixture.record,
          headerCborHex: other.record.headerCborHex,
        },
      }),
    ).rejects.toBeInstanceOf(WatcherHeaderRootReconstructionError);
  });

  it("rejects malformed header record fields", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        record: { ...fixture.record, utxosRoot: "not-hex" },
      }),
    ).rejects.toMatchObject({ code: "invalid_header_record" });
    await expect(
      observationFor(fixture, {
        record: { ...fixture.record, blockSlot: "-1" },
      }),
    ).rejects.toMatchObject({ code: "invalid_header_record" });
  });

  it("rejects a confirmation depth below the required minimum", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        confirmationDepth: 2,
        minimumConfirmationDepth: 10,
      }),
    ).rejects.toMatchObject({ code: "insufficient_confirmation_depth" });
  });

  it("rejects an L1 observation that is not authenticated Cardano L1", async () => {
    const fixture = await buildFixture();
    await expect(
      observationFor(fixture, {
        provenance: {
          trustClass: "operator_private_database",
          sourceId: "operator-db",
          grade: "security",
        },
      }),
    ).rejects.toMatchObject({ code: "prohibited_trust_class" });
  });
});

describe("W22 positive reconstruction", () => {
  it("reconstructs all eight header roots for a valid block", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.reconstructedRoots).toStrictEqual(result.headerRoots);
    for (const field of WATCHER_HEADER_ROOT_FIELDS) {
      expect(result.reconstructedRoots?.[field]).toMatch(/^[0-9a-f]{64}$/u);
    }
  });

  it("reconstructs all seven header counts for a valid block", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const result = await evaluateFixture(fixture);
    expect(result.reconstructedCounts).toStrictEqual(result.headerCounts);
    expect(result.headerCounts).toStrictEqual({
      withdrawal_count: "1",
      forced_transaction_count: "0",
      l2_transaction_count: "2",
      deposit_count: "1",
      total_event_count: "4",
      transition_step_count: "0",
      validation_trace_count: "2",
    });
    expect(WATCHER_HEADER_COUNT_FIELDS).toHaveLength(7);
  });

  it("accepts with empty mismatch lists, no reason codes, and both payload digests", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture);
    expect(result.schemaVersion).toBe(
      WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
    );
    expect(result.reasonCodes).toStrictEqual([]);
    expect(result.rootMismatches).toStrictEqual([]);
    expect(result.countMismatches).toStrictEqual([]);
    expect(result.headerHash).toBe(fixture.headerHash);
    expect(result.payloadEnvelopeSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.payloadSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.payloadSha256).not.toBe(result.payloadEnvelopeSha256);
  });

  it("produces a stable result digest across repeated runs", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const first = await evaluateFixture(fixture);
    const second = await evaluateFixture(fixture);
    expect(second).toStrictEqual(first);
    expect(second.resultDigest).toBe(first.resultDigest);
  });

  it("changes the result digest when the outcome changes", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const accepted = await evaluateFixture(fixture);
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      depositsRoot: h32(0x5a),
    }));
    const rejected = await evaluateFixture(mutated);
    expect(rejected.resultDigest).not.toBe(accepted.resultDigest);
  });
});

describe("W22 adjacent boundaries", () => {
  it("accepts an empty-collection block with zero counts", async () => {
    const fixture = await buildFixture();
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.reconstructedCounts).toStrictEqual({
      withdrawal_count: "0",
      forced_transaction_count: "0",
      l2_transaction_count: "0",
      deposit_count: "0",
      total_event_count: "0",
      transition_step_count: "0",
      validation_trace_count: "0",
    });
    expect(result.reconstructedRoots?.utxos_root).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
  });

  it("accepts the one-element neighbour of the empty block", async () => {
    const empty = await buildFixture();
    const single = await buildFixture({
      transactions: [corpusTransaction(0)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const emptyResult = await evaluateFixture(empty);
    const singleResult = await evaluateFixture(single);
    expect(singleResult.action).toBe("accept");
    expect(singleResult.reconstructedRoots?.transactions_root).not.toBe(
      emptyResult.reconstructedRoots?.transactions_root,
    );
    expect(singleResult.reconstructedRoots?.deposits_root).not.toBe(
      emptyResult.reconstructedRoots?.deposits_root,
    );
    expect(singleResult.reconstructedRoots?.withdrawals_root).not.toBe(
      emptyResult.reconstructedRoots?.withdrawals_root,
    );
  });

  it("rejects a count that is off by exactly one", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      depositCount: header.depositCount + 1n,
    }));
    const result = await evaluateFixture(mutated);
    expect(result.action).toBe("reject");
    expect(result.countMismatches).toStrictEqual(["deposit_count"]);
  });

  it("accepts total_event_count at exactly the component sum", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
      depositBytes: [11],
      withdrawalBytes: [21],
    });
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.headerCounts.total_event_count).toBe("3");
  });

  it.each([
    ["above", 1n],
    ["below", -1n],
  ])(
    "rejects total_event_count one %s the component sum",
    async (_label, delta) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
        depositBytes: [11],
        withdrawalBytes: [21],
      });
      const mutated = await commitMutatedHeader(fixture, (header) => ({
        ...header,
        totalEventCount: header.totalEventCount + delta,
      }));
      const result = await evaluateFixture(mutated);
      expect(result.action).toBe("reject");
      expect(result.countMismatches).toStrictEqual(["total_event_count"]);
      expect(result.rootMismatches).toStrictEqual([]);
    },
  );
});

describe("W22 per-root mismatch determinism", () => {
  const rootMutations: readonly [
    (typeof WATCHER_HEADER_ROOT_FIELDS)[number],
    keyof SDK.Header,
  ][] = [
    ["utxos_root", "utxosRoot"],
    ["withdrawals_root", "withdrawalsRoot"],
    ["forced_transactions_root", "forcedTransactionsRoot"],
    ["transactions_root", "transactionsRoot"],
    ["deposits_root", "depositsRoot"],
    ["transition_trace_root", "transitionTraceRoot"],
    ["event_to_step_root", "eventToStepRoot"],
    ["validation_traces_root", "validationTracesRoot"],
  ];

  it.each(rootMutations)(
    "reports exactly %s when that root diverges",
    async (field, headerField) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
        depositBytes: [11],
        withdrawalBytes: [21],
      });
      const mutated = await commitMutatedHeader(fixture, (header) => ({
        ...header,
        [headerField]: h32(0xbe),
      }));
      const result = await evaluateFixture(mutated);
      expect(result.action).toBe("reject");
      expect(result.reasonCodes).toStrictEqual(["root_mismatch"]);
      expect(result.rootMismatches).toStrictEqual([field]);
      expect(result.countMismatches).toStrictEqual([]);
      expect(result.reconstructedRoots).toBeNull();
    },
  );

  it("covers every declared root field exactly once", () => {
    expect(rootMutations.map(([field]) => field)).toStrictEqual([
      ...WATCHER_HEADER_ROOT_FIELDS,
    ]);
  });
});
