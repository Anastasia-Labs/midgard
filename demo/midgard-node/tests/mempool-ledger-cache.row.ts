import { MempoolLedgerDB } from "../src/database/index.js";
import { type MempoolLedgerState } from "../src/services/mempool-ledger-cache.js";

export const row = (
  outref: Buffer,
  output: Buffer,
): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: Buffer.alloc(32, 0x77),
  [MempoolLedgerDB.Columns.OUTREF]: outref,
  [MempoolLedgerDB.Columns.OUTPUT]: output,
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1_phase2_cache",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer.alloc(32, 0x33),
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
});

export const snapshot = (state: MempoolLedgerState) =>
  new Map([...state].map(([key, value]) => [key, Buffer.from(value)]));
