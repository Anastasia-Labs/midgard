import * as SDK from "@al-ft/midgard-sdk";
import { type Assets, Data } from "@lucid-evolution/lucid";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import {
  depositEventsRetainedBlock,
  sealDepositPayload,
} from "./transition-trace-retained.deposit-events-retained-block.js";

/** Structurally complete timing evidence; L1 admission supplies event authority. */
export const transitionTraceTimingRetainedFixture = async (input: {
  operatorVkey: string;
  now: number;
  payload: SDK.EventHistoryPayload;
  originalAssets: Assets;
  omitted: boolean;
}) => {
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey: input.operatorVkey,
    startTime: BigInt(input.now),
    endTime: BigInt(input.now + 60_000),
    blockSlot: 9n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [],
  });
  const current = await depositEventsRetainedBlock({
    operatorVkey: input.operatorVkey,
    startTime: predecessor.header.endTime,
    // An honest late event is admitted after the predecessor commits (from
    // 60 s before its start) yet an event wait before this block's end, so
    // the block must outlast the event wait.
    endTime: BigInt(
      input.now + 60_000 + Math.max(60_000, SDK.EVENT_WAIT_DURATION_MS),
    ),
    blockSlot: 10n,
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
    priorLedger: predecessor.payload.block_body.utxos,
    events:
      !input.omitted && "DepositPayload" in input.payload
        ? [
            {
              event: input.payload.DepositPayload.event,
              originalAssets: input.originalAssets,
              honest: true,
            },
          ]
        : [],
  });
  if (input.omitted || "DepositPayload" in input.payload)
    return { predecessor, current };
  const { event } = input.payload.WithdrawalPayload;
  // The committed verdict is independent of the submitted body. Timing proof
  // validates the source body and signature while preserving that distinction.
  const info: SDK.WithdrawalInfo = {
    ...event.info,
    validity: "IncorrectWithdrawalSignature",
  };
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: event.id },
  };
  const withdrawals: SDK.DaPayloadEntry[] = [
    [
      Data.to(event.id, SDK.OutputReference),
      SDK.committedWithdrawalValueBytes(info),
    ],
  ];
  const eventToStep: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase: "Withdrawal" }, SDK.EventToStepValue),
    ],
  ];
  const transitionTrace: SDK.DaPayloadEntry[] = [
    [
      Data.to(0n),
      Data.to(
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "Withdrawal",
          pre_utxos_root: current.header.utxosRoot,
          post_utxos_root: current.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ],
  ];
  const root = async (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const counts = {
    ...current.payload.block_body.counts,
    withdrawalCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const header = {
    ...current.header,
    ...counts,
    withdrawalsRoot: await root(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
    eventToStepRoot: await root(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
    transitionTraceRoot: await root(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTrace,
    ),
  };
  return {
    predecessor,
    current: await sealDepositPayload({
      ...current.payload,
      block_body: {
        ...current.payload.block_body,
        withdrawals,
        event_to_step: eventToStep,
        transition_trace: transitionTrace,
        header,
        counts,
      },
    }),
  };
};
