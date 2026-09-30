import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type HistoryPredecessor } from "./history-cases.js";
import {
  type HistoryEventBlockInput,
  retainJourneyWithdrawals,
} from "./history-events.retain-journey-withdrawals.js";

/**
 * Repeat a deposit from an actual settled ancestor. The caller must stage and
 * retain the real settlement NFT separately; retained DA alone proves no
 * settlement. The honest control commits an empty continuation.
 */
export const buildJourneyRepeatedDeposit = async (
  input: HistoryEventBlockInput & {
    settled: HistoryPredecessor;
    honest?: boolean;
  },
) => {
  const entry = input.settled.payload.block_body.deposits[0];
  if (entry === undefined)
    throw new Error("Settled ancestor has no retained deposit event");
  if (input.settled.header.endTime > input.predecessor.header.endTime)
    throw new Error("Duplicate-event history is newer than the predecessor");
  const base = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: input.predecessor.payload.block_body.utxos.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
    startTime: input.predecessor.header.endTime,
    endTime: input.endTime,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    minFeeA: input.predecessor.header.minFeeA,
    minFeeB: input.predecessor.header.minFeeB,
  });
  const eventKey: SDK.EventKey = {
    DepositEventKey: { deposit_id: Data.from(entry[0], SDK.OutputReference) },
  };
  const deposits: SDK.DaPayloadEntry[] = input.honest ? [] : [entry];
  const mappings: SDK.DaPayloadEntry[] = input.honest
    ? []
    : [
        [
          Data.to(eventKey, SDK.EventKey),
          Data.to({ step_index: 0n, phase: "Deposit" }, SDK.EventToStepValue),
        ],
      ];
  const transitions: SDK.DaPayloadEntry[] = input.honest
    ? []
    : [
        [
          Data.to(0n),
          Data.to(
            {
              schema_version: 1n,
              step_index: 0n,
              event_key: eventKey,
              phase: "Deposit",
              pre_utxos_root: base.header.prevUtxosRoot,
              post_utxos_root: base.header.utxosRoot,
            },
            SDK.TransitionStep,
          ),
        ],
      ];
  const root = (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [depositRoot, eventRoot, traceRoot] = await Promise.all([
    root(SDK.ROOT_DOMAINS.deposits, deposits),
    root(SDK.ROOT_DOMAINS.eventToStep, mappings),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitions),
  ]);
  const counts = {
    ...base.payload.block_body.counts,
    depositCount: BigInt(deposits.length),
    totalEventCount: BigInt(deposits.length),
    transitionStepCount: BigInt(deposits.length),
  };
  const header: SDK.Header = {
    ...base.header,
    ...counts,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    expectedNetworkId: input.predecessor.header.expectedNetworkId,
    depositsRoot: depositRoot.root,
    eventToStepRoot: eventRoot.root,
    transitionTraceRoot: traceRoot.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      deposits,
      event_to_step: mappings,
      transition_trace: transitions,
    },
  };
  return {
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

/** Deliberately repeat the unchanged withdrawal leaf from an earlier settled block. */
export const buildJourneyRepeatedWithdrawal = async (
  input: HistoryEventBlockInput & { settled: HistoryPredecessor },
) => {
  const entry = input.settled.payload.block_body.withdrawals[0];
  if (entry === undefined)
    throw new Error("Settled ancestor has no retained withdrawal event");
  if (input.settled.header.endTime > input.predecessor.header.endTime)
    throw new Error(
      "Repeated-withdrawal history is newer than the predecessor",
    );
  return retainJourneyWithdrawals({
    ...input,
    claims: [
      {
        id: Data.from(entry[0], SDK.OutputReference),
        infoCbor: entry[1],
      },
    ],
  });
};
