import {
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
  mergeMidgardCekProgramMaterialSidecars,
} from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as DepositsDB from "../src/database/deposits.js";
import * as ForcedDB from "../src/database/forcedTransactions.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import type { DaPayloadBlockContent } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import { commitDaPayloadForSizing } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import {
  depositTraceAggregatesByPrefix,
  measureDaPayloadPrefixes,
} from "../src/workers/commit-block-header/submission.measure-da-prefixes.js";
import {
  commitDaFrameStepDownPassBound,
  DA_PAYLOAD_UPPER_BOUND_HEADER,
  DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
  selectCommitTxCandidates,
  stepDownCommitSelectionToDaFrame,
} from "../src/workers/utils/commit-block-planner.js";
import {
  blockContentFor,
  header,
  mkCandidate,
  syntheticCommitPrefixMeasurement,
  traceFor,
} from "./helpers/commit-da-frame-fixtures.js";

const emptySidecar = encodeMidgardCekProgramMaterialSidecar([]);
const term = {
  kind: "term",
  root: hashMidgardCekTermNode({ kind: "error" }),
  preimage: encodeMidgardCekTermNode({ kind: "error" }),
} as const;
const sharedSidecar = encodeMidgardCekProgramMaterialSidecar([term]);
const tuple = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];
const aggregate = (entries: readonly SDK.DaPayloadEntry[]) => ({
  entryCount: entries.length,
  encodedTupleBytes: entries.reduce(
    (bytes, entry) => bytes + SDK.daPayloadEntryEncodedSize(entry),
    0,
  ),
});
const ledgerAt = (prefix: number): SDK.DaPayloadEntry[] =>
  Array.from({ length: (prefix % 7) + 1 }, (_, index) => [
    index.toString(16).padStart(64, "0"),
    "ab".repeat((prefix % 3) * 63),
  ]);
const source = (
  key: SDK.EventKey,
  phase: SDK.TransitionPhase,
  step: number,
) => {
  const value: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: BigInt(step),
    event_key: key,
    phase,
    pre_utxos_root: "aa".repeat(32),
    post_utxos_root: "bb".repeat(32),
  };
  return {
    stepIndex: BigInt(step),
    keyCbor: encodeTransitionIntegerCbor(BigInt(step)),
    valueCbor: encodeTransitionStepCbor(value),
    value,
  };
};
const contentAt = (prefix: number): DaPayloadBlockContent => {
  const txs = Array.from({ length: prefix }, (_, index) =>
    mkCandidate(index + 1),
  );
  const normal = blockContentFor(txs, {
    base: { entryCount: 0, encodedTupleBytes: 0 },
    witnessCount: 2,
    witnessValueBytes: 63,
  });
  const forcedKey: SDK.EventKey = {
    ForcedTransactionEventKey: {
      tx_order_id: { transactionId: "fa".repeat(32), outputIndex: 0n },
    },
  };
  const forced = {
    [ForcedDB.Columns.TX_ORDER_ID]: Buffer.from("fa", "hex"),
    [ForcedDB.Columns.FORCED_INCLUSION_VALUE]: Buffer.from("fb", "hex"),
    [ForcedDB.Columns.NATIVE_TX_CBOR]: Buffer.from("fc", "hex"),
    [ForcedDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: sharedSidecar,
  } as unknown as ForcedDB.Entry;
  const transitionTraceMembers = [
    source(forcedKey, "ForcedTransaction", 0),
    ...normal.transitionTraceMembers.map((member, index) =>
      source(member.value.event_key, "L2Transaction", index + 1),
    ),
    ...[0, 24, 256].map((outputIndex, index) =>
      source(
        {
          DepositEventKey: {
            deposit_id: {
              transactionId: "da".repeat(32),
              outputIndex: BigInt(outputIndex),
            },
          },
        },
        "Deposit",
        prefix + 1 + index,
      ),
    ),
  ];
  const eventToStepMembers = transitionTraceMembers.map((member) => {
    const value: SDK.EventToStepValue = {
      phase: member.value.phase,
      step_index: member.stepIndex,
    };
    return {
      eventKey: member.value.event_key,
      keyCbor: encodeTransitionEventKeyCbor(member.value.event_key),
      valueCbor: encodeEventToStepValueCbor(value),
      value,
    };
  });
  const forcedValidation = { ...traceFor(999, 1, "ff"), eventKey: forcedKey };
  return {
    ...normal,
    utxoPayloadAggregate: aggregate(ledgerAt(prefix)),
    utxoPayloadAggregatesByPrefix: Array.from(
      { length: prefix + 1 },
      (_, index) => aggregate(ledgerAt(index)),
    ),
    includedForcedTransactionEntries: [forced],
    includedDepositEntries: [0, 24, 256].map(
      (index) =>
        ({
          [DepositsDB.Columns.ID]: Buffer.from(
            index.toString(16).padStart(4, "0"),
            "hex",
          ),
          [DepositsDB.Columns.INFO]: Buffer.from("de", "hex"),
        }) as unknown as DepositsDB.Entry,
    ),
    transitionTraceMembers,
    eventToStepMembers,
    validationTraceMembers: [
      forcedValidation,
      ...normal.validationTraceMembers,
    ],
  };
};
const payloadAt = (content: DaPayloadBlockContent) =>
  Effect.runPromise(
    commitDaPayloadForSizing({
      ...content,
      headerHash: DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
      header: DA_PAYLOAD_UPPER_BOUND_HEADER,
      cekProgramMaterial: [],
    }),
  );
const measuredAt = async (prefix: number) => {
  const content = contentAt(prefix);
  const ordinarySidecars = Array.from({ length: prefix }, (_, index) =>
    index % 2 === 0 ? sharedSidecar : emptySidecar,
  );
  return measureDaPayloadPrefixes({
    payload: await payloadAt(content),
    content,
    ordinarySidecars,
    forcedSidecars: [sharedSidecar],
    identityContext: Buffer.from("fixed-base-and-window"),
    rejectedTxIds: [],
  });
};

describe("complete DA prefix canonical sizing", () => {
  it("matches actual canonical encoding across all prefixes, shifted mandatory deposits, count-width boundaries and deduplicated material", async () => {
    // Only accounting is tested: trace values/sources are codec fixtures, not
    // claims that these transactions passed production validation or native MPF.
    const n = 270;
    const measured = await measuredAt(n);
    for (const prefix of [
      0,
      1,
      20,
      21,
      22,
      23,
      24,
      25,
      252,
      253,
      254,
      255,
      256,
      257,
      n,
    ]) {
      const content = contentAt(prefix);
      const payload = await payloadAt(content);
      const counts = {
        withdrawalCount: 0n,
        forcedTransactionCount: 1n,
        l2TransactionCount: BigInt(prefix),
        depositCount: 3n,
        totalEventCount: BigInt(prefix + 4),
        transitionStepCount: BigInt(prefix + 4),
        validationTraceCount: BigInt(prefix + 1),
      };
      const cek = mergeMidgardCekProgramMaterialSidecars([sharedSidecar]).map(
        (entry) =>
          tuple(entry.root, encodeMidgardCekProgramMaterialDaValue(entry)),
      );
      const canonical = {
        ...payload,
        block_body: {
          ...payload.block_body,
          header: { ...payload.block_body.header, ...counts },
          counts,
          utxos: ledgerAt(prefix),
          cek_program_material: cek,
        },
      };
      expect(measured.prefixes[prefix]!.innerBytesUpperBound).toBe(
        SDK.encodeDaPayload(canonical).length,
      );
      expect(measured.prefixes[prefix]!.materialDigest).toBe(
        (await measuredAt(prefix)).prefixes[prefix]!.materialDigest,
      );
    }
  });

  it("schedules deposit width changes exactly without rescanning every prefix", () => {
    const content = contentAt(270);
    const measured = depositTraceAggregatesByPrefix(content, 270, 1);
    for (let prefix = 0; prefix <= 270; prefix += 1) {
      const deposits = contentAt(prefix).transitionTraceMembers.filter(
        (member) => member.value.phase === "Deposit",
      );
      const events = contentAt(prefix).eventToStepMembers.slice(prefix + 1);
      expect(measured[prefix]).toEqual({
        transition: aggregate(
          deposits.map((member) => tuple(member.keyCbor, member.valueCbor)),
        ),
        eventToStep: aggregate(
          events.map((member) => tuple(member.keyCbor, member.valueCbor)),
        ),
      });
    }
  });

  it("refuses missing prefix ledger evidence and inconsistent accepted trace order", async () => {
    const content = contentAt(2);
    const payload = await payloadAt(content);
    const measure = (changed: DaPayloadBlockContent) =>
      measureDaPayloadPrefixes({
        payload,
        content: changed,
        ordinarySidecars: [sharedSidecar, emptySidecar],
        forcedSidecars: [sharedSidecar],
        identityContext: Buffer.alloc(0),
        rejectedTxIds: [],
      });
    expect(() =>
      measure({ ...content, utxoPayloadAggregatesByPrefix: undefined }),
    ).toThrow(/accounting is unavailable/);
    expect(() =>
      measure({
        ...content,
        processedMempoolTxs: [...content.processedMempoolTxs].reverse(),
      }),
    ).toThrow(/order is inconsistent/);
  });
});

it("rebuilds the global minimum for final-header admission when every upper bound overflows", async () => {
  const ledgers: SDK.DaPayloadEntry[][] = Array.from(
    { length: 5 },
    (_, prefix) => [["01", "ab".repeat(prefix === 1 ? 1 : 14_000)]],
  );
  const content = (count: number): DaPayloadBlockContent => ({
    ...contentAt(count),
    utxoPayloadAggregate: aggregate(ledgers[count]!),
    utxoPayloadAggregatesByPrefix: ledgers.slice(0, count + 1).map(aggregate),
  });
  const measure = async (built: DaPayloadBlockContent) =>
    measureDaPayloadPrefixes({
      payload: await payloadAt(built),
      content: built,
      ordinarySidecars: built.processedMempoolTxs.map((_, index) =>
        index % 2 === 0 ? sharedSidecar : emptySidecar,
      ),
      forcedSidecars: [sharedSidecar],
      identityContext: Buffer.from("fixed-window"),
      rejectedTxIds: [],
    });
  const full = await measure(content(4));
  const limit = full.prefixes[1]!.innerBytesUpperBound - 1;
  expect(
    full.prefixes.every((prefix) => prefix.innerBytesUpperBound > limit),
  ).toBe(true);
  const builtCounts: number[] = [];
  const result = await Effect.runPromise(
    stepDownCommitSelectionToDaFrame({
      candidateSelection: selectCommitTxCandidates({
        mempoolTxs: Array.from({ length: 4 }, (_, index) =>
          mkCandidate(index + 1),
        ),
        processedMempoolTxs: [],
      }),
      baseUtxoPayloadAggregate: aggregate(ledgers[0]!),
      maxInnerBytes: limit,
      process: (selection) =>
        Effect.sync(() => {
          builtCounts.push(selection.candidateTxs.length);
          return content(selection.candidateTxs.length);
        }),
      measure: (built) => Effect.tryPromise(() => measure(built)),
      rebase: Effect.void,
    }),
  );
  expect(result.outcome).toBe("exact_check_required");
  expect(builtCounts).toEqual([4, 1]);
  const payload = await payloadAt(result.processed);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 3n,
    totalEventCount: 5n,
    transitionStepCount: 5n,
    validationTraceCount: 2n,
  };
  const exact = SDK.encodeDaPayload({
    ...payload,
    block_body: {
      ...payload.block_body,
      header_hash: "33".repeat(28),
      header: { ...header, ...counts },
      counts,
      utxos: ledgers[1]!,
      cek_program_material: [
        tuple(term.root, encodeMidgardCekProgramMaterialDaValue(term)),
      ],
    },
  });
  expect(exact.length).toBeLessThanOrEqual(limit);
});

it.each([1, 2, 3, 7, 100, 8_000])(
  "checks every prefix within the existing work/pass budget for %i accepted rows",
  async (n) => {
    const chosen = Math.max(1, n - 2);
    const bytes = Array.from({ length: n + 1 }, (_, prefix) =>
      prefix === chosen ? 90 : 200,
    );
    const rows = Array.from({ length: n }, (_, index) =>
      mkCandidate(index + 1),
    );
    let processedRows = 0;
    const result = await Effect.runPromise(
      stepDownCommitSelectionToDaFrame({
        candidateSelection: selectCommitTxCandidates({
          mempoolTxs: rows,
          processedMempoolTxs: [],
        }),
        baseUtxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
        maxInnerBytes: 100,
        process: (selection) =>
          Effect.sync(() => {
            processedRows += selection.candidateTxs.length;
            return selection;
          }),
        measure: (selection) =>
          Effect.succeed(
            syntheticCommitPrefixMeasurement(
              selection.candidateTxHashes,
              bytes,
            ),
          ),
        rebase: Effect.void,
      }),
    );
    expect(result.outcome).toBe("fits");
    expect(result.candidateSelection.candidateTxs).toHaveLength(chosen);
    expect(result.passes).toBeLessThanOrEqual(
      commitDaFrameStepDownPassBound(n),
    );
    expect(processedRows).toBeLessThanOrEqual(2 * n);
  },
);
