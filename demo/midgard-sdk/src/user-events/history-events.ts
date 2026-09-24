import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { type Assets, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type AddressData, LucidError } from "../common.js";
import {
  type CardanoDatum,
  DepositEvent,
  WithdrawalEvent,
} from "../ledger-state.js";
import { valueToAssets } from "../reserve-payout/assets.js";
import { EventHistoryFacts, EventHistoryPayload } from "./history.js";
import { captureEventHistoryWitness } from "./history-proof.js";
import {
  type EventHistoryDeployment,
  type EventHistoryPresence,
  fetchEventHistoryOrders,
} from "./history-query.js";

export type EventHistoryFetchConfig = EventHistoryDeployment & {
  readonly inclusionTimeLowerBound?: bigint;
  readonly inclusionTimeUpperBound?: bigint;
};

type HistoryEventUTxO = {
  readonly utxo: UTxO;
  readonly assetName: string;
  readonly facts: EventHistoryFacts;
  readonly history: EventHistoryPresence;
  /** Original funds, excluding the list NFT and declared structural ADA. */
  readonly originalAssets: Assets;
  readonly idCbor: Buffer;
  readonly infoCbor: Buffer;
  readonly inclusionTime: Date;
};

export type DepositUTxO = HistoryEventUTxO & {
  readonly kind: "Deposit";
  readonly event: DepositEvent;
};

export type WithdrawalUTxO = HistoryEventUTxO & {
  readonly kind: "Withdrawal";
  readonly event: WithdrawalEvent;
  readonly refundAddress: AddressData;
  readonly refundDatum: CardanoDatum;
};

export const historyEventFromPresence = (
  history: EventHistoryPresence,
  deployment: EventHistoryDeployment,
): DepositUTxO | WithdrawalUTxO => {
  const { anchor } = history;
  if (
    anchor.key === null ||
    anchor.node.payload === "RootContent" ||
    !("Order" in anchor.node.payload)
  )
    throw new Error("Expected an authenticated history Order");
  const captured = captureEventHistoryWitness(
    history,
    deployment.policyId,
    "DepositPayload" in Data.from(history.payloadCbor, EventHistoryPayload)
      ? "Deposit"
      : "Withdrawal",
  );
  const payload = captured.payload;
  const facts = Data.from(captured.factsCbor, EventHistoryFacts);
  const time = Number(facts.inclusion_time);
  const inclusionTime = new Date(time);
  if (!Number.isSafeInteger(time) || !Number.isFinite(inclusionTime.getTime()))
    throw new Error(
      "History inclusion time cannot be represented by the event reader",
    );
  const eventCbor = plutusConstrFieldCbor(captured.payloadCbor, [0]);
  const base: HistoryEventUTxO = {
    utxo: anchor.utxo,
    assetName: anchor.key,
    facts,
    history,
    originalAssets: valueToAssets(captured.originalAssets),
    idCbor: Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        plutusConstrFieldCbor(eventCbor, [0]),
      ),
      "hex",
    ),
    infoCbor: Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        plutusConstrFieldCbor(eventCbor, [1]),
      ),
      "hex",
    ),
    inclusionTime,
  };
  return "DepositPayload" in payload
    ? { ...base, kind: "Deposit", event: payload.DepositPayload.event }
    : {
        ...base,
        kind: "Withdrawal",
        event: payload.WithdrawalPayload.event,
        refundAddress: payload.WithdrawalPayload.refund_address,
        refundDatum: payload.WithdrawalPayload.refund_datum,
      };
};

export const historyEventReadError = (cause: unknown) =>
  new LucidError({
    message: `Failed to read authenticated event history: ${String(cause)}`,
    cause,
  });

export const fetchHistoryEventsProgram = <T extends HistoryEventUTxO>(
  provider: { utxosAt(address: string): Promise<UTxO[]> },
  config: EventHistoryFetchConfig,
  convert: (
    history: EventHistoryPresence,
    deployment: EventHistoryDeployment,
  ) => T,
): Effect.Effect<T[], LucidError> =>
  Effect.tryPromise({
    try: async () =>
      (await fetchEventHistoryOrders(provider, config))
        .map((history) => convert(history, config))
        .filter(
          (event) =>
            (config.inclusionTimeLowerBound === undefined ||
              event.facts.inclusion_time >= config.inclusionTimeLowerBound) &&
            (config.inclusionTimeUpperBound === undefined ||
              event.facts.inclusion_time < config.inclusionTimeUpperBound),
        ),
    catch: historyEventReadError,
  });
