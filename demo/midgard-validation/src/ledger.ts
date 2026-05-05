import type { Address } from "@lucid-evolution/lucid";

export enum LedgerColumns {
  TX_ID = "tx_id",
  OUTREF = "outref",
  OUTPUT = "output",
  ADDRESS = "address",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type LedgerEntryNoTimeStamp = {
  [LedgerColumns.TX_ID]: Buffer;
  [LedgerColumns.OUTREF]: Buffer;
  [LedgerColumns.OUTPUT]: Buffer;
  [LedgerColumns.ADDRESS]: Address;
};

export type LedgerEntryWithTimeStamp = LedgerEntryNoTimeStamp & {
  [LedgerColumns.TIMESTAMPTZ]: Date;
};

export type LedgerEntry = LedgerEntryNoTimeStamp | LedgerEntryWithTimeStamp;

export type LedgerMinimalEntry = {
  [LedgerColumns.OUTREF]: Buffer;
  [LedgerColumns.OUTPUT]: Buffer;
};

export type ProcessedTx = {
  txId: Buffer;
  txCbor: Buffer;
  spent: Buffer[];
  produced: LedgerEntry[];
};
