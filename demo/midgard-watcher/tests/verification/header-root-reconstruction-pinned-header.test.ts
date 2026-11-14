import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  buildFixture,
  commitMutatedHeader,
  evaluateFixture,
} from "./header-root-reconstruction.build-fixture.js";
import {
  bufferEntries,
  corpusTransaction,
  type Fixture,
  hex,
  sortEntries,
} from "./header-root-reconstruction.watcher-header-record.js";

/** A block with two L2 transactions, so its body carries two traces. */
const builtFixture = async (): Promise<Fixture> =>
  await buildFixture({
    transactions: [corpusTransaction(0), corpusTransaction(1)],
  });

/**
 * Commits the header the state-queue validator requires: the validation-trace
 * pair is the empty root and a zero count, while the payload body keeps its
 * traces and declares their number.
 */
const commitPair = async (
  fixture: Fixture,
  pair: Pick<SDK.Header, "validationTracesRoot" | "validationTraceCount">,
  fields: Partial<SDK.Header> = {},
): Promise<Fixture> =>
  await commitMutatedHeader(fixture, (header) => ({
    ...header,
    ...pair,
    ...fields,
  }));

/** The root of the traces the payload body carries. */
const builtRoot = async (fixture: Fixture): Promise<string> =>
  (
    await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      bufferEntries(fixture.payload.block_body.validation_traces),
    )
  ).root;

const PINNED = {
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTraceCount: 0n,
} as const;

/**
 * A block with one forced and one L2 transaction under a pinned header. The
 * body carries the L2 transaction's trace, plus the forced one's when
 * `forcedTrace` is set, and declares exactly the traces it carries.
 */
const forcedAndL2Fixture = async (forcedTrace: boolean): Promise<Fixture> => {
  const built = await buildFixture({ transactions: [corpusTransaction(0)] });
  const forcedTx = corpusTransaction(1);
  const forced = materializeMidgardForcedTxFromCanonical(
    decodeMidgardNativeTxFullFromCanonicalCbor(forcedTx.canonicalCbor),
  );
  const forcedSource = deriveMidgardForcedTxProofSource(forced);
  const orderId = hex(
    { transactionId: forcedTx.txId, outputIndex: 0n },
    SDK.OutputReferenceSchema,
  );
  const forcedEntries: SDK.DaPayloadEntry[] = [
    [
      orderId,
      hex(
        {
          tx_id: forcedTx.txId,
          submitted_source: {
            compact_cbor: forcedSource.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              forcedSource.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              forcedSource.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: "ForcedTxValid",
        } satisfies SDK.ForcedInclusionTxV1,
        SDK.ForcedInclusionTxV1Schema,
      ),
    ],
  ];
  const step = (
    eventKey: SDK.EventKey,
    stepIndex: bigint,
    phase: SDK.TransitionPhase,
  ): SDK.DaPayloadEntry => [
    hex(eventKey, SDK.EventKeySchema),
    hex(
      { step_index: stepIndex, phase } satisfies SDK.EventToStepValue,
      SDK.EventToStepValueSchema,
    ),
  ];
  const forcedKey: SDK.EventKey = {
    ForcedTransactionEventKey: {
      tx_order_id: { transactionId: forcedTx.txId, outputIndex: 0n },
    },
  };
  const eventToStep = sortEntries([
    step(forcedKey, 0n, "ForcedTransaction"),
    step(
      { L2TransactionEventKey: { tx_id: built.transactions[0]!.txId } },
      1n,
      "L2Transaction",
    ),
  ]);
  const counted = async (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(entries))).root;
  const body = built.payload.block_body;
  const validationTraces = sortEntries([
    ...body.validation_traces,
    ...(forcedTrace
      ? [
          [
            hex(forcedKey, SDK.EventKeySchema),
            body.validation_traces[0]![1],
          ] satisfies SDK.DaPayloadEntry,
        ]
      : []),
  ]);
  const counts = {
    ...body.counts,
    forcedTransactionCount: 1n,
    totalEventCount: body.counts.totalEventCount + 1n,
  };
  const header: SDK.Header = {
    ...built.header,
    ...counts,
    ...PINNED,
    forcedTransactionsRoot: await counted(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedEntries,
    ),
    eventToStepRoot: await counted(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
  };
  const withForced: Fixture = {
    ...built,
    payload: {
      ...built.payload,
      block_body: {
        ...body,
        forced_transactions: forcedEntries,
        forced_transaction_preimages: [
          [orderId, encodeMidgardForcedTxCanonical(forced).toString("hex")],
        ],
        event_to_step: eventToStep,
        validation_traces: validationTraces,
        counts: {
          ...counts,
          validationTraceCount: BigInt(validationTraces.length),
        },
      },
    },
  };
  return await commitPair(withForced, PINNED, header);
};

describe("W22 reconstruction of a header with a pinned validation-trace pair", () => {
  it("accepts an honest pinned header of a block with L2 transactions", async () => {
    const built = await builtFixture();
    const fixture = await commitPair(built, PINNED);
    expect(
      fixture.header.forcedTransactionCount + fixture.header.l2TransactionCount,
    ).toBeGreaterThan(0n);
    expect(fixture.payload.block_body.counts.validationTraceCount).toBe(2n);
    expect(fixture.payload.block_body.validation_traces).toHaveLength(2);
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "accept",
      reasonCodes: [],
      rootMismatches: [],
      countMismatches: [],
    });
    expect(result.reconstructedRoots).toMatchObject({
      validation_traces_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    expect(result.reconstructedCounts).toMatchObject({
      l2_transaction_count: "2",
      validation_trace_count: "0",
    });
  });

  it("rejects a header that commits the empty root with the built count", async () => {
    const built = await builtFixture();
    const fixture = await commitPair(built, {
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTraceCount: 2n,
    });
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["count_mismatch"],
      rootMismatches: [],
      countMismatches: ["validation_trace_count"],
    });
  });

  it.each([
    [
      "the built root and count",
      (root: string) => ({
        validationTracesRoot: root,
        validationTraceCount: 2n,
      }),
    ],
    [
      "a built root with a zero count",
      (root: string) => ({
        validationTracesRoot: root,
        validationTraceCount: 0n,
      }),
    ],
  ])("rejects a header that commits %s", async (_label, pair) => {
    const built = await builtFixture();
    const root = await builtRoot(built);
    expect(root).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    const result = await evaluateFixture(await commitPair(built, pair(root)));
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["root_mismatch"],
      rootMismatches: ["validation_traces_root"],
      countMismatches: [],
    });
  });

  it("accepts a forced-and-L2 body that declares and carries a trace for each", async () => {
    const fixture = await forcedAndL2Fixture(true);
    expect(fixture.payload.block_body.counts.validationTraceCount).toBe(2n);
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "accept",
      reasonCodes: [],
    });
    expect(result.reconstructedCounts).toMatchObject({
      forced_transaction_count: "1",
      l2_transaction_count: "1",
      validation_trace_count: "0",
    });
  });

  it("rejects a forced-and-L2 body that declares and carries a trace for its L2 transaction only", async () => {
    const fixture = await forcedAndL2Fixture(false);
    const body = fixture.payload.block_body;
    expect(body.counts.forcedTransactionCount).toBe(1n);
    expect(body.counts.l2TransactionCount).toBe(1n);
    expect(body.validation_traces).toHaveLength(1);
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["count_mismatch", "unenumerated_count_mismatch"],
      rootMismatches: [],
      countMismatches: [],
    });
  });

  it("still rejects a pinned header whose transition_step_count differs from the payload", async () => {
    const fixture = await commitPair(await builtFixture(), PINNED, {
      transitionStepCount: 1n,
    });
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["count_mismatch"],
      rootMismatches: [],
      countMismatches: ["transition_step_count"],
    });
  });

  it("still rejects a pinned header whose deposits_root differs from the payload", async () => {
    const fixture = await commitPair(await builtFixture(), PINNED, {
      depositsRoot: h32(0x5a),
    });
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["root_mismatch"],
      rootMismatches: ["deposits_root"],
      countMismatches: [],
    });
  });
});
