/**
 * A forced order the chain admitted is screened at intake with the
 * off-chain twin of the tx-order mint's admission check. A refusal is an
 * alarm only: one `ForcedAdmissionDivergence` error event, one increment of
 * `forced_admission_divergence_total`, and a `/readyz` detail
 * `forced_admission_divergence:<orderIdHex>:<refusalCode>` among the latest
 * distinct orders observed. The order is still ingested and classified to a
 * verdict leaf, and the node stays ready. An order the screen admits raises
 * nothing. A transaction the screen cannot read is a divergence too, never
 * an ingestion failure.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, HashMap, Logger, Metric } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  ENVELOPE_UNDECODABLE,
  forcedAdmissionDivergenceCounter,
  forcedAdmissionRefusal,
  SCREEN_FAILED,
} from "../src/forced-orders/admission-divergence.js";
import {
  FORCED_ADMISSION_DIVERGENCES_REPORTED,
  forcedAdmissionDivergences,
  forcedOrderIngestionHook,
  type RunDatabase,
} from "../src/forced-orders/index.js";
import { classifyForcedTransactions } from "../src/mpf/index.js";
import { l1FollowerReadiness } from "../src/services/l1-follower.readiness.js";
import {
  FORCED_CONFIG,
  spendingTransactionCbor,
} from "./helpers/forced-orders-chain.js";
import {
  honest,
  inlineOrder,
  nodeFollowerLifecycle,
  rows,
} from "./helpers/forced-orders-node-chain.js";
import {
  db,
  runDatabase,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { runningFollower } from "./readiness-l1-follower.fixture.js";

const follow = nodeFollowerLifecycle();

type LoggedError = Readonly<{ annotations: Record<string, unknown> }>;

/** How many divergent orders the node has counted under `refusalCode`. */
const divergenceCount = (refusalCode: string) =>
  Effect.runPromise(
    Metric.value(
      Metric.tagged(
        forcedAdmissionDivergenceCounter,
        "refusal_code",
        refusalCode,
      ),
    ),
  ).then((state) => BigInt(state.count));

/** The intake hook with a divergence register, its error events captured. */
const intake = (
  store: Parameters<typeof forcedOrderIngestionHook>[0]["store"],
) => {
  const errors: LoggedError[] = [];
  const capture = Logger.replace(
    Logger.defaultLogger,
    Logger.make(({ logLevel, annotations }) => {
      if (logLevel._tag === "Error")
        errors.push({
          annotations: Object.fromEntries(HashMap.toEntries(annotations)),
        });
    }),
  );
  const run: RunDatabase = (effect) =>
    runDatabase(effect.pipe(Effect.provide(capture)));
  const divergences = forcedAdmissionDivergences();
  const hook = forcedOrderIngestionHook({
    store,
    config: FORCED_CONFIG,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    contentSourcesConfigured: true,
    run,
    divergences,
  });
  const readiness = () =>
    l1FollowerReadiness({
      ...runningFollower(),
      forcedAdmissionDivergences: () => divergences.details(),
    });
  return { hook, errors, readiness };
};

/** The verdict reason classification gives the node's one forced row. */
const classifiedReason = async () => {
  const entries = await db(ForcedTransactionsDB.retrieveAllEntries());
  expect(entries).toHaveLength(1);
  const [classified] = await Effect.runPromise(
    classifyForcedTransactions({
      entries,
      initialState: new Map(),
      blockTime: new Date(Number(10_000n)),
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      validation: {
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        bucketConcurrency: 1,
        slotForUnixTime: () => 100n,
      },
    }),
  );
  const leaf = Data.from(
    classified!.entry[
      ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
    ].toString("hex"),
    SDK.ForcedInclusionTxV1,
  );
  return leaf.verdict === "ForcedTxValid"
    ? "ForcedTxValid"
    : typeof leaf.verdict.ForcedTxInvalid.reason === "string"
      ? leaf.verdict.ForcedTxInvalid.reason
      : Object.keys(leaf.verdict.ForcedTxInvalid.reason)[0]!;
};

describe("the forced-order intake's admission screen", () => {
  it("names a chain-admitted order the screen refuses, keeps the node ready, and still classifies it", async () => {
    const { store, chain } = await follow();
    // An order with no spend input: the screen refuses it.
    await chain.forward([inlineOrder(chain.chain, honest())]);
    const { hook, errors, readiness } = intake(store);
    const counted = await divergenceCount("has_a_spend_input(body)");
    expect(await hook(UNCHANGED)).toBeUndefined();
    const [row] = await rows();
    const orderId = Buffer.from(row!.tx_order_id).toString("hex");
    const detail = `forced_admission_divergence:${orderId}:has_a_spend_input(body)`;
    expect(readiness().reasons).toEqual([]);
    expect(readiness().details).toEqual([detail]);
    expect(await divergenceCount("has_a_spend_input(body)")).toBe(counted + 1n);
    expect(errors).toEqual([
      {
        annotations: expect.objectContaining({
          event: "ForcedAdmissionDivergence",
          orderId,
          refusalCode: "has_a_spend_input(body)",
        }),
      },
    ]);
    expect(await classifiedReason()).toBe("EmptyInputs");

    // A later run raises nothing again for the same order.
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(errors).toHaveLength(1);
    expect(await divergenceCount("has_a_spend_input(body)")).toBe(counted + 1n);
    expect(readiness().details).toEqual([detail]);
  });

  it("raises nothing for an order the screen admits", async () => {
    const { store, chain } = await follow();
    await chain.forward([
      inlineOrder(chain.chain, spendingTransactionCbor([0x22])),
    ]);
    const { hook, errors, readiness } = intake(store);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toHaveLength(1);
    expect(readiness()).toMatchObject({ reasons: [], details: [] });
    expect(errors).toEqual([]);
  });

  it("refuses a transaction whose envelope does not decode, and one the screen throws on, without throwing", () => {
    const admitted = spendingTransactionCbor([0x22]);
    expect(forcedAdmissionRefusal(admitted)).toBeNull();
    expect(forcedAdmissionRefusal(Buffer.from([0x00]))).toMatchObject({
      refusalCode: ENVELOPE_UNDECODABLE,
    });
    expect(
      forcedAdmissionRefusal(admitted, () => {
        throw new RangeError("out of bounds");
      }),
    ).toMatchObject({ refusalCode: SCREEN_FAILED });
  });

  it("names only the latest distinct divergent orders", () => {
    const divergences = forcedAdmissionDivergences();
    const extra = 3;
    for (
      let order = 0;
      order < FORCED_ADMISSION_DIVERGENCES_REPORTED + extra;
      order += 1
    )
      expect(divergences.record(order.toString(16), "check")).toBe(true);
    expect(divergences.record("0", "check")).toBe(true);
    expect(divergences.record((extra + 1).toString(16), "check")).toBe(false);
    expect(divergences.details()).toEqual(
      [
        ...Array.from(
          { length: FORCED_ADMISSION_DIVERGENCES_REPORTED - 1 },
          (_, index) => (extra + 1 + index).toString(16),
        ),
        "0",
      ].map((order) => `forced_admission_divergence:${order}:check`),
    );
  });
});
