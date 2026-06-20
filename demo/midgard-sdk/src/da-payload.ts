import { Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import { HeaderHashSchema, HeaderSchema } from "./ledger-state.js";

export const DA_PAYLOAD_V2_VERSION = 2n;

export const DaPayloadEntrySchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);
export type DaPayloadEntry = Data.Static<typeof DaPayloadEntrySchema>;
export const DaPayloadEntry = DaPayloadEntrySchema as unknown as DaPayloadEntry;

export const DaPayloadCountsV2Schema = Data.Object({
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
});
export type DaPayloadCountsV2 = Data.Static<typeof DaPayloadCountsV2Schema>;
export const DaPayloadCountsV2 =
  DaPayloadCountsV2Schema as unknown as DaPayloadCountsV2;

export const DaPayloadBodyV2Schema = Data.Object({
  header_hash: HeaderHashSchema,
  header: HeaderSchema,
  utxos: Data.Array(DaPayloadEntrySchema),
  withdrawals: Data.Array(DaPayloadEntrySchema),
  forced_transactions: Data.Array(DaPayloadEntrySchema),
  transactions: Data.Array(DaPayloadEntrySchema),
  deposits: Data.Array(DaPayloadEntrySchema),
  transition_trace: Data.Array(DaPayloadEntrySchema),
  event_to_step: Data.Array(DaPayloadEntrySchema),
  counts: DaPayloadCountsV2Schema,
});
export type DaPayloadBodyV2 = Data.Static<typeof DaPayloadBodyV2Schema>;
export const DaPayloadBodyV2 =
  DaPayloadBodyV2Schema as unknown as DaPayloadBodyV2;

export const DaPayloadV2Schema = Data.Object({
  version: Data.Integer(),
  block_body: DaPayloadBodyV2Schema,
});
export type DaPayloadV2 = Data.Static<typeof DaPayloadV2Schema>;
export const DaPayloadV2 = DaPayloadV2Schema as unknown as DaPayloadV2;

export const encodeDaPayloadV2 = (payload: DaPayloadV2): Buffer =>
  Buffer.from(Data.to(payload, DaPayloadV2), "hex");

export const decodeDaPayloadV2 = (payloadCbor: Buffer): DaPayloadV2 =>
  Data.from(toHex(payloadCbor), DaPayloadV2 as never) as DaPayloadV2;

export const daPayloadHashHex = (payloadCbor: Buffer): string =>
  toHex(sha256(payloadCbor));
