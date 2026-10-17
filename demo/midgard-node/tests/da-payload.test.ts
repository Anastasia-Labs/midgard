import "node:crypto";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/database/index.js";
import "../src/database/utils/common.js";
import "../src/mpf/index.js";
import "../src/workers/commit-block-header/da-payload.js";
import "../src/workers/commit-block-header/da-payload-backfill.js";
import "../src/workers/commit-block-header/transition-roots.js";
import "./midgard-output-helpers.js";
import "./utils.js";
import "./da-payload.record.js";
import "./da-payload.build-journal-fixture.js";

import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import {
  DaPayloadContentEncoding,
  decodeDaPayloadEnvelope,
  unwrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { buildDaPayloadInsert } from "../src/workers/commit-block-header/da-payload.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../src/workers/commit-block-header/da-payload-backfill.js";
import { buildJournalFixture } from "./da-payload.build-journal-fixture.js";
import {
  countsFromLengths,
  fixture,
  headerFor,
  ledgerEntries,
  member,
  record,
  retainedPairs,
  sourceRoot,
  type TestRoots,
} from "./da-payload.record.js";

describe("DaPayloadV1 builder", () => {
  it("builds canonical V1 journals with distinct preimage sidecars", async () => {
    const counts = countsFromLengths({});
    const roots: TestRoots = {
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    const header = headerFor(roots, counts);
    const headerHash = Buffer.from(
      await Effect.runPromise(SDK.hashBlockHeader(header)),
      "hex",
    );
    const pending = record({
      headerHash,
      depositMembers: [],
      withdrawalMembers: [],
      transitionTraceMembers: [],
      eventToStepMembers: [],
      roots,
      counts,
      header,
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    });

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
        envelope: { mode: "identity", zstdLevel: 3 },
      }),
    );
    const unwrapped = await unwrapDaPayload(insert.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    expect(decodeDaPayloadEnvelope(insert.payload_cbor).contentEncoding).toBe(
      DaPayloadContentEncoding.identity,
    );
    const payload = SDK.decodeDaPayload(unwrapped.innerBytes);

    expect(insert.version).toBe(1);
    expect(payload.version).toBe(SDK.DA_PAYLOAD_VERSION);
    expect(payload.block_body.transaction_preimages).toEqual([]);
    expect(payload.block_body.forced_transaction_preimages).toEqual([]);
    expect(payload.block_body.header).toEqual(header);
  });

  it("builds a canonical payload whose roots, counts, and header match the journal", async () => {
    const utxoEntries = ledgerEntries("utxo", 3);
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("deposit", 34), fixture("deposit-info", 48)],
    ];
    const withdrawalEntries: readonly [Buffer, Buffer][] = [
      [fixture("withdrawal", 34), fixture("withdrawal-info", 52)],
    ];
    const transitionTraceEntries = retainedPairs("trace", 2);
    const eventToStepEntries = retainedPairs("event-to-step", 2);
    const { pending, roots, header, headerHash } = await buildJournalFixture({
      utxoEntries,
      depositEntries,
      withdrawalEntries,
      transitionTraceEntries,
      eventToStepEntries,
    });

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({ record: pending }),
    );
    const identityUnwrapped = await unwrapDaPayload(insert.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    const payload = SDK.decodeDaPayload(identityUnwrapped.innerBytes);

    expect(payload.block_body.header_hash).toBe(headerHash.toString("hex"));
    expect(payload.block_body.header).toEqual(header);
    expect(Object.keys(payload.block_body)).not.toContain("utxos");
    expect(insert.utxos_root).toBe(roots.utxosRoot);
    expect(insert.forced_transactions_root).toBe(roots.forcedTransactionsRoot);
    expect(insert.transactions_root).toBe(roots.transactionsRoot);
    expect(insert.deposits_root).toBe(roots.depositsRoot);
    expect(insert.withdrawals_root).toBe(roots.withdrawalsRoot);
    expect(insert.transition_trace_root).toBe(roots.transitionTraceRoot);
    expect(insert.event_to_step_root).toBe(roots.eventToStepRoot);
    expect(insert.deposit_count).toBe(1n);
    expect(insert.withdrawal_count).toBe(1n);
    expect(insert.total_event_count).toBe(2n);
    // Independent oracle: node:crypto over the stored envelope bytes, not the
    // SDK helper the builder itself used to fill the column.
    expect(insert.payload_sha256.toString("hex")).toBe(
      createHash("sha256").update(insert.payload_cbor).digest("hex"),
    );

    const zstdInsert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
        envelope: { mode: "zstd", zstdLevel: 3 },
      }),
    );
    const unwrapped = await unwrapDaPayload(zstdInsert.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    expect(
      decodeDaPayloadEnvelope(zstdInsert.payload_cbor).contentEncoding,
    ).toBe(DaPayloadContentEncoding.zstd);
    expect(zstdInsert.version).toBe(1);
    expect(unwrapped.innerBytes).toEqual(identityUnwrapped.innerBytes);
    // The digest binds the ENVELOPE bytes, so the compressed row must carry a
    // different digest from the identity row over the same inner payload.
    expect(zstdInsert.payload_sha256.toString("hex")).toBe(
      createHash("sha256").update(zstdInsert.payload_cbor).digest("hex"),
    );
    expect(zstdInsert.payload_sha256).not.toEqual(insert.payload_sha256);
  });

  it("builds payloads whose size does not depend on the ledger size", async () => {
    const events = {
      depositEntries: [
        [fixture("size-deposit", 34), fixture("size-deposit-info", 48)],
      ] as readonly [Buffer, Buffer][],
      transitionTraceEntries: retainedPairs("size-trace", 1),
      eventToStepEntries: retainedPairs("size-event-to-step", 1),
    };
    const innerBytesOver = async (ledgerSize: number) => {
      const { pending } = await buildJournalFixture({
        utxoEntries: ledgerEntries(`size-ledger-${ledgerSize}`, ledgerSize),
        ...events,
      });
      const insert = await Effect.runPromise(
        buildDaPayloadInsert({
          record: pending,
          envelope: { mode: "identity", zstdLevel: 3 },
        }),
      );
      return (
        await unwrapDaPayload(insert.payload_cbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        })
      ).innerBytes;
    };
    const small = await innerBytesOver(10);
    const large = await innerBytesOver(10_000);
    // Only the header's post-state root and the header hash differ.
    expect(large.length).toBe(small.length);
    const differing = [...small].filter((byte, index) => large[index] !== byte);
    expect(differing.length).toBeLessThanOrEqual(64);
  });

  it("backfills a missing DA payload from complete journal payload members", async () => {
    const utxoEntries = ledgerEntries("backfill-utxo", 2);
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("backfill-deposit", 34), fixture("backfill-deposit-info", 48)],
    ];
    const transitionTraceEntries = retainedPairs("backfill-trace", 1);
    const eventToStepEntries = retainedPairs("backfill-event-to-step", 1);
    const { pending, headerHash } = await buildJournalFixture({
      utxoEntries,
      depositEntries,
      transitionTraceEntries,
      eventToStepEntries,
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          materializeLedgerOutputs: () =>
            Effect.succeed(utxoEntries.map(([, output]) => output)),
          upsertAvailable: (input) =>
            Effect.sync(() => {
              inserts.push(input);
            }),
        },
      }),
    );

    expect(summary).toEqual({
      scanned: 1,
      backfilled: [headerHash.toString("hex")],
      skipped: [],
    });
    expect(inserts).toHaveLength(1);
    const backfilled = await unwrapDaPayload(inserts[0]!.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    expect(
      SDK.decodeDaPayload(backfilled.innerBytes).block_body.header_hash,
    ).toBe(headerHash.toString("hex"));
  });

  it("skips backfill when the V1 delta chain cannot be materialized", async () => {
    const { pending, headerHash } = await buildJournalFixture({
      rootOverrides: {
        utxosRoot: "00".repeat(32),
      },
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          materializeLedgerOutputs: () =>
            Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message: "V1 delta chain is incomplete",
                cause: "test fixture",
              }),
            ),
          upsertAvailable: (input) =>
            Effect.sync(() => {
              inserts.push(input);
            }),
        },
      }),
    );

    expect(summary).toEqual({
      scanned: 1,
      backfilled: [],
      skipped: [
        {
          headerHash: headerHash.toString("hex"),
          reason: expect.stringContaining("V1 delta chain is incomplete"),
        },
      ],
    });
    expect(inserts).toEqual([]);
  });

  it("reports requested journals excluded from DA payload backfill by status", async () => {
    const { pending, headerHash } = await buildJournalFixture({
      depositEntries: [
        [fixture("backfill-excluded-deposit", 34), fixture("info", 48)],
      ],
    });
    const abandoned = {
      ...pending,
      [PendingBlockFinalizationsDB.Columns.STATUS]:
        PendingBlockFinalizationsDB.Status.Abandoned,
    };

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        headerHash,
        deps: {
          retrieveMissingRecords: () => Effect.succeed([]),
          retrieveJournalByHeaderHash: () =>
            Effect.succeed(Option.some(abandoned)),
          materializeLedgerOutputs: () => Effect.succeed([]),
          upsertAvailable: () => Effect.void,
        },
      }),
    );

    expect(summary).toEqual({
      scanned: 0,
      backfilled: [],
      skipped: [
        {
          headerHash: headerHash.toString("hex"),
          reason:
            "journal excluded by status: abandoned; revive and complete local finalization before DA payload backfill",
        },
      ],
    });
  });

  it("persists a journal that carries validation traces under a header that pins the pair to the empty tree", async () => {
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("traced-deposit", 34), fixture("traced-deposit-info", 48)],
    ];
    const validationTraceEntries = retainedPairs("traced-validation", 1);
    const { pending, header, headerHash } = await buildJournalFixture({
      utxoEntries: ledgerEntries("traced-utxo", 1),
      depositEntries,
      transitionTraceEntries: retainedPairs("traced-trace", 1),
      eventToStepEntries: retainedPairs("traced-event-to-step", 1),
    });
    expect(header.validationTracesRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(header.validationTraceCount).toBe(0n);
    const journal: PendingBlockFinalizationsDB.Record = {
      ...pending,
      [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT]:
        await Effect.runPromise(
          sourceRoot(SDK.ROOT_DOMAINS.validationTraces, validationTraceEntries),
        ),
      [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT]: 1n,
      validationTraceMembers: validationTraceEntries.map(
        ([key, value], index) => member(headerHash, key, value, index),
      ),
    };

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({ record: journal }),
    );
    const payload = SDK.decodeDaPayload(
      (
        await unwrapDaPayload(insert.payload_cbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        })
      ).innerBytes,
    );
    expect(payload.block_body.header).toEqual(header);
    expect(payload.block_body.validation_traces).toHaveLength(1);
  });

  it("refuses to persist a journal whose header commits its built validation-trace pair", async () => {
    const validationTraceEntries = retainedPairs("committed-validation", 1);
    const builtRoot = await Effect.runPromise(
      sourceRoot(SDK.ROOT_DOMAINS.validationTraces, validationTraceEntries),
    );
    const { pending, header, headerHash } = await buildJournalFixture({
      utxoEntries: ledgerEntries("committed-utxo", 1),
      depositEntries: [
        [fixture("committed-deposit", 34), fixture("committed-info", 48)],
      ],
      transitionTraceEntries: retainedPairs("committed-trace", 1),
      eventToStepEntries: retainedPairs("committed-event-to-step", 1),
      rootOverrides: { validationTracesRoot: builtRoot },
      countOverrides: { validationTraceCount: 1n },
    });
    expect(header.validationTracesRoot).toBe(builtRoot);
    expect(header.validationTraceCount).toBe(1n);
    const journal: PendingBlockFinalizationsDB.Record = {
      ...pending,
      validationTraceMembers: validationTraceEntries.map(
        ([key, value], index) => member(headerHash, key, value, index),
      ),
    };

    const result = await Effect.runPromise(
      Effect.either(buildDaPayloadInsert({ record: journal })),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(String(result.left.cause)).toMatch(
        /mismatches=header_validation_traces_root,header_validation_trace_count$/,
      );
  });

  it("rejects a payload whose recomputed roots do not match the journal", async () => {
    const utxoEntries = ledgerEntries("bad-utxo", 1);
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("bad-deposit", 34), fixture("bad-deposit-info", 48)],
    ];
    const transitionTraceEntries = retainedPairs("bad-trace", 1);
    const eventToStepEntries = retainedPairs("bad-event-to-step", 1);
    const { pending } = await buildJournalFixture({
      utxoEntries,
      depositEntries,
      transitionTraceEntries,
      eventToStepEntries,
      recordRootOverrides: {
        depositsRoot: "00".repeat(32),
      },
    });

    const result = await Effect.runPromise(
      Effect.either(buildDaPayloadInsert({ record: pending })),
    );

    expect(result._tag).toBe("Left");
  });
});
