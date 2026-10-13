/**
 * User-event ingestion from the event projection (N1): the deposit and
 * withdrawal row content the node's L2 side reads, derived from a
 * projected event's immutable bytes and its location. Pure: no database,
 * no clock. The follower-change driver writes the node's event rows from
 * it (E-N1-2 ruling 1).
 */
import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { ProjectedEvent } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data, type Network } from "@lucid-evolution/lucid";

export type DepositEntryContent = Readonly<{
  idCbor: string;
  infoCbor: string;
  inclusionTimeMs: number;
  l1TxHash: string;
  ledgerTxId: string;
  ledgerOutput: string;
  ledgerAddress: string;
}>;

export type WithdrawalEntryContent = Readonly<{
  idCbor: string;
  rawEventInfo: string;
  inclusionTimeMs: number;
  l1TxHash: string;
  l1OutputIndex: number;
  assetName: string;
  l2Outref: string;
  l2Owner: string;
  l2Value: string;
  l1Address: string;
  l1Datum: string;
  refundAddress: string;
  refundDatum: string;
}>;

export type UserEventEntry =
  | Readonly<{ kind: "deposit"; entry: DepositEntryContent }>
  | Readonly<{ kind: "withdrawal"; entry: WithdrawalEntryContent }>;

const normalised = (cbor: string, path: readonly number[]): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(cbor, path),
  );

/** The Some field of a serialised `Option`, or null for None. */
const optionalFieldCbor = (cbor: string): Buffer | null => {
  if (cbor === NONE_CBOR) return null;
  if (!cbor.startsWith(CONSTR_0_OPEN))
    throw new Error("optional field is neither Some nor None");
  const value = plutusConstrFieldCbor(cbor, [0]);
  if (cbor !== `${CONSTR_0_OPEN}${value}ff`)
    throw new Error("optional field carries more than one value");
  return Buffer.from(value, "hex");
};
const NONE_CBOR = "d87a80";
const CONSTR_0_OPEN = "d8799f";

const inclusionMs = (event: ProjectedEvent): number => {
  const time = Number(event.inclusionTime);
  if (!Number.isSafeInteger(time) || time < 0)
    throw new Error("inclusion time is not a safe POSIX millisecond count");
  return time;
};

const depositEntry = (
  event: ProjectedEvent,
  network: Network,
): DepositEntryContent => {
  const infoCbor = normalised(event.payloadCbor, [0, 1]);
  // `DepositInfo { l2_address, l2_network_id, l2_datum }`, exactly. The
  // address and network id are fixed-shape; the datum is user Data and is
  // carried as its exact bytes, never through a typed view.
  const [addressCbor, networkIdCbor, datumCbor] = [0, 1, 2].map((field) =>
    plutusConstrFieldCbor(infoCbor, [field]),
  ) as [string, string, string];
  if (
    infoCbor !== `${CONSTR_0_OPEN}${addressCbor}${networkIdCbor}${datumCbor}ff`
  )
    throw new Error("deposit info is not a three-field constructor 0");
  const l2NetworkId = Data.from<Data>(networkIdCbor);
  if (typeof l2NetworkId !== "bigint")
    throw new Error("deposit network id is not an integer");
  const effect = deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: network,
    eventId: Data.from(event.idCbor, SDK.OutputReference),
    l2NetworkId,
    l2Address: Data.from(addressCbor, SDK.AddressData),
    l2DatumCbor: optionalFieldCbor(datumCbor),
    originalAssets: SDK.valueToAssets(
      Data.from(event.originalAssetsCbor, SDK.Value),
    ),
  });
  const operation = effect.operations[0];
  if (operation === undefined || operation.type !== "insert")
    throw new Error("canonical deposit projection did not produce an insert");
  const output = Buffer.from(operation.outputCbor);
  return {
    idCbor: event.idCbor,
    infoCbor,
    inclusionTimeMs: inclusionMs(event),
    // Ruling 2: a deposit's L1 tx hash is its admission tx, never the moving
    // location of its list node.
    l1TxHash: event.admission.outRef.txHash.toString("hex"),
    ledgerTxId: computeHash32(Buffer.from(event.idCbor, "hex")).toString("hex"),
    ledgerOutput: output.toString("hex"),
    ledgerAddress: encodeMidgardAddressText(
      decodeMidgardTxOutput(output).address,
    ),
  };
};

const withdrawalEntry = (event: ProjectedEvent): WithdrawalEntryContent => {
  const infoCbor = normalised(event.payloadCbor, [0, 1]);
  const { body } = Data.from(infoCbor, SDK.WithdrawalInfo);
  return {
    idCbor: event.idCbor,
    rawEventInfo: infoCbor,
    inclusionTimeMs: inclusionMs(event),
    l1TxHash: event.location.txHash.toString("hex"),
    l1OutputIndex: event.location.index,
    assetName: event.key,
    l2Outref: Data.to(body.l2_outref, SDK.OutputReference),
    l2Owner: body.l2_owner,
    l2Value: plutusConstrFieldCbor(infoCbor, [0, 2]),
    l1Address: Data.to(body.l1_address, SDK.AddressData),
    l1Datum: plutusConstrFieldCbor(infoCbor, [0, 4]),
    refundAddress: Data.to(
      Data.from(plutusConstrFieldCbor(event.payloadCbor, [1]), SDK.AddressData),
      SDK.AddressData,
    ),
    refundDatum: plutusConstrFieldCbor(event.payloadCbor, [2]),
  };
};

/**
 * The ingestion row content of a projected event. Throws when the event's
 * payload disagrees with its kind or identity (the projection admitted it
 * from authenticated bytes, so that is a decoding fault, not a user error).
 */
export const userEventEntry = (
  event: ProjectedEvent,
  network: Network,
): UserEventEntry => {
  // Payload constructor 0 is a deposit, 1 a withdrawal (`EventHistoryPayload`).
  const tag = aikenSerialisedPlutusDataCborPreservingMapOrder(
    event.payloadCbor,
  ).slice(0, 6);
  if (tag !== (event.kind === "deposit" ? "d8799f" : "d87a9f"))
    throw new Error("event kind disagrees with its payload");
  if (normalised(event.payloadCbor, [0, 0]) !== event.idCbor)
    throw new Error("event identity disagrees with its payload");
  return event.kind === "deposit"
    ? { kind: "deposit", entry: depositEntry(event, network) }
    : { kind: "withdrawal", entry: withdrawalEntry(event) };
};
