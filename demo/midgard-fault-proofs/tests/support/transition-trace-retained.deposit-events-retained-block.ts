import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { type Assets, Data } from "@lucid-evolution/lucid";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { buildCanonicalBlockFixture } from "../helpers/canonical-block-evidence-fixture.js";

export const sealDepositPayload = async (payload: SDK.DaPayload) => {
  const header = payload.block_body.header;
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const sealed = {
    ...payload,
    block_body: { ...payload.block_body, header_hash: headerHash },
  };
  return {
    payload: sealed,
    header,
    headerHash,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(sealed), {
      mode: "identity",
    }),
  };
};

type DepositEventInput =
  | { event: SDK.DepositEvent; eventCbor?: never }
  | { eventCbor: string; event?: never };

export type RetainedDepositEvent = DepositEventInput & {
  originalAssets: Assets;
  honest: boolean;
};

const retainedDepositEvent = (input: DepositEventInput) => {
  if (input.event !== undefined && input.eventCbor !== undefined)
    throw new Error(
      "Retained deposit fixture received competing event encodings",
    );
  const eventCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    input.eventCbor ?? Data.to(input.event!, SDK.DepositEvent),
  );
  const event = Data.from(eventCbor, SDK.DepositEvent);
  return { event, infoCbor: plutusConstrFieldCbor(eventCbor, [1]) };
};

/** Build deposit transitions from actual event outputs and the retained prior ledger. */
export const depositEventsRetainedBlock = async (input: {
  operatorVkey: string;
  startTime: bigint;
  endTime: bigint;
  blockSlot: bigint;
  prevHeaderHash: string;
  prevUtxosRoot: string;
  priorLedger: readonly SDK.DaPayloadEntry[];
  events: readonly RetainedDepositEvent[];
}) => {
  const ledger = new Map(input.priorLedger);
  const buildLedger = () =>
    buildCanonicalBlockFixture({
      transactions: [],
      utxos: [...ledger].map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
      startTime: input.startTime,
      endTime: input.endTime,
      prevHeaderHash: input.prevHeaderHash,
      prevUtxosRoot: input.prevUtxosRoot,
    });
  let base = await buildLedger();
  if (base.header.utxosRoot !== input.prevUtxosRoot)
    throw new Error("Retained prior ledger differs from the predecessor root");
  const entries: Record<
    "deposits" | "event_to_step" | "transition_trace",
    SDK.DaPayloadEntry[]
  > = { deposits: [], event_to_step: [], transition_trace: [] };
  const events = input.events
    .map((item) => ({ ...item, ...retainedDepositEvent(item) }))
    .sort((a, b) =>
      Data.to(a.event.id, SDK.OutputReference).localeCompare(
        Data.to(b.event.id, SDK.OutputReference),
      ),
    );
  for (const [index, item] of events.entries()) {
    const { id, info } = item.event;
    const effect = deriveCanonicalOriginalDepositTransitionEffect({
      configuredNetwork: "Custom",
      eventId: id,
      l2Address: info.l2_address,
      l2NetworkId: info.l2_network_id,
      l2DatumCbor:
        info.l2_datum === null
          ? null
          : Buffer.from(plutusConstrFieldCbor(item.infoCbor, [2, 0]), "hex"),
      originalAssets: {
        ...item.originalAssets,
        lovelace: item.originalAssets.lovelace! + (item.honest ? 0n : 1n),
      },
    });
    const operation = effect.operations[0];
    if (operation?.type !== "insert")
      throw new Error("Deposit fixture has no insertion");
    const key = encodeMidgardSpendInputItem({
      txId: Buffer.from(id.transactionId, "hex"),
      outputIndex: Number(id.outputIndex),
    }).toString("hex");
    if (ledger.has(key))
      throw new Error("Deposit fixture repeats an existing ledger output");
    const preRoot = base.header.utxosRoot;
    ledger.set(key, operation.outputCbor.toString("hex"));
    base = await buildLedger();
    const eventKey: SDK.EventKey = { DepositEventKey: { deposit_id: id } };
    const stepIndex = BigInt(index);
    entries.deposits.push([SDK.committedDepositKeyBytes(id), item.infoCbor]);
    entries.event_to_step.push([
      Data.to(eventKey, SDK.EventKey),
      Data.to(
        { step_index: stepIndex, phase: "Deposit" },
        SDK.EventToStepValue,
      ),
    ]);
    entries.transition_trace.push([
      Data.to(stepIndex),
      Data.to(
        {
          schema_version: 1n,
          step_index: stepIndex,
          event_key: eventKey,
          phase: "Deposit",
          pre_utxos_root: preRoot,
          post_utxos_root: base.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ]);
  }
  for (const values of Object.values(entries))
    values.sort(([a], [b]) => a.localeCompare(b));
  const root = async (domain: SDK.RootDomain, values: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        values.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const counts = {
    ...base.payload.block_body.counts,
    depositCount: BigInt(events.length),
    totalEventCount: BigInt(events.length),
    transitionStepCount: BigInt(events.length),
  };
  const header = {
    ...base.header,
    ...counts,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    depositsRoot: await root(SDK.ROOT_DOMAINS.deposits, entries.deposits),
    eventToStepRoot: await root(
      SDK.ROOT_DOMAINS.eventToStep,
      entries.event_to_step,
    ),
    transitionTraceRoot: await root(
      SDK.ROOT_DOMAINS.transitionTrace,
      entries.transition_trace,
    ),
  };
  return sealDepositPayload({
    ...base.payload,
    block_body: { ...base.payload.block_body, ...entries, counts, header },
  });
};

export const transitionTraceDepositRetainedFixture = async (
  input: DepositEventInput & {
    operatorVkey: string;
    now: number;
    originalAssets: Assets;
    honest?: boolean;
  },
) => {
  const { operatorVkey, now, originalAssets, honest = false } = input;
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: BigInt(now),
    endTime: BigInt(now + 60_000),
    blockSlot: 9n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [],
  });
  const current = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: predecessor.header.endTime,
    endTime: BigInt(now + 120_000),
    blockSlot: 10n,
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
    priorLedger: predecessor.payload.block_body.utxos,
    events: [{ ...input, originalAssets, honest }],
  });
  return { predecessor, current };
};
