import { Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import { HeaderHashSchema } from "./ledger-state.js";

export const DA_PAYLOAD_V1_VERSION = 1n;

export const DaPayloadEntrySchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);
export type DaPayloadEntry = Data.Static<typeof DaPayloadEntrySchema>;
export const DaPayloadEntry = DaPayloadEntrySchema as unknown as DaPayloadEntry;

export const DaPayloadBodyV1Schema = Data.Object({
  utxos: Data.Array(DaPayloadEntrySchema),
  transactions: Data.Array(DaPayloadEntrySchema),
  deposits: Data.Array(DaPayloadEntrySchema),
  withdrawals: Data.Array(DaPayloadEntrySchema),
});
export type DaPayloadBodyV1 = Data.Static<typeof DaPayloadBodyV1Schema>;
export const DaPayloadBodyV1 =
  DaPayloadBodyV1Schema as unknown as DaPayloadBodyV1;

export const DaPayloadV1Schema = Data.Object({
  version: Data.Integer(),
  header_hash: HeaderHashSchema,
  block_body: DaPayloadBodyV1Schema,
});
export type DaPayloadV1 = Data.Static<typeof DaPayloadV1Schema>;
export const DaPayloadV1 = DaPayloadV1Schema as unknown as DaPayloadV1;

export const encodeDaPayloadV1 = (payload: DaPayloadV1): Buffer =>
  Buffer.from(Data.to(payload, DaPayloadV1), "hex");

export const decodeDaPayloadV1 = (payloadCbor: Buffer): DaPayloadV1 =>
  Data.from(toHex(payloadCbor), DaPayloadV1 as never) as DaPayloadV1;

export const daPayloadHashHex = (payloadCbor: Buffer): string =>
  toHex(sha256(payloadCbor));
