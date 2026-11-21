import { default as fs } from "node:fs";
import { default as path } from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";

import { LedgerUtils, TxUtils } from "../../src/database/index.js";
import { databaseFixtureBytes, databaseTxHash } from "./fixtures.bytes.js";
import { databaseTestDirectory } from "./fixtures.environment.js";

export const blockHeader1 = databaseFixtureBytes("blocks.header-1", 32);

export const blockHeader2 = databaseFixtureBytes("blocks.header-2", 32);

export const txId1 = databaseTxHash("shared.tx-1");

export const txId2 = databaseTxHash("shared.tx-2");

export const tx1 = databaseFixtureBytes("shared.tx-1-cbor", 64);

export const tx2 = databaseFixtureBytes("shared.tx-2-cbor", 64);

export const tx3 = databaseFixtureBytes("shared.tx-3-cbor", 64);

const outref1 = databaseFixtureBytes("shared.outref-1", 36);

const outref2 = databaseFixtureBytes("shared.outref-2", 36);

const output1 = databaseFixtureBytes("shared.output-1", 80);

const output2 = databaseFixtureBytes("shared.output-2", 80);

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(databaseTestDirectory, "./txs/txs_0.json");

const firstFixture = (
  JSON.parse(fs.readFileSync(fixturePath, "utf8")) as readonly TxFixture[]
)[0];

export const makeValidNativeImmutableEntry = (): TxUtils.Entry => {
  const nativeTx = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    Buffer.from(firstFixture.cborHex, "hex"),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(nativeTx),
  );
  return {
    [TxUtils.Columns.TX_ID]: txId,
    [TxUtils.Columns.TX]: nativeTx,
  };
};

export const address1 =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

export const address2 =
  "addr_test1vzcsc5wzu3vsnjek2n80ayce53r4ha2g6wyetqddrp8z04q3yzv6k";

export const txEntry1: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId1,
  [TxUtils.Columns.TX]: tx1,
};

export const txEntry2: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId2,
  [TxUtils.Columns.TX]: tx2,
};

export const removeTimestampFromTxEntry = (
  e: TxUtils.Entry,
): TxUtils.EntryNoTimeStamp => {
  return {
    [TxUtils.Columns.TX_ID]: e[TxUtils.Columns.TX_ID],
    [TxUtils.Columns.TX]: e[TxUtils.Columns.TX],
  };
};

export const ledgerEntry1: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId1,
  [LedgerUtils.Columns.OUTREF]: outref1,
  [LedgerUtils.Columns.OUTPUT]: output1,
  [LedgerUtils.Columns.ADDRESS]: address1,
};

export const ledgerEntry2: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId2,
  [LedgerUtils.Columns.OUTREF]: outref2,
  [LedgerUtils.Columns.OUTPUT]: output2,
  [LedgerUtils.Columns.ADDRESS]: address2,
};

export const removeTimestampFromLedgerEntry = (
  e: LedgerUtils.Entry,
): LedgerUtils.EntryNoTimeStamp => {
  return {
    [LedgerUtils.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
    [LedgerUtils.Columns.OUTREF]: e[LedgerUtils.Columns.OUTREF],
    [LedgerUtils.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
    [LedgerUtils.Columns.ADDRESS]: e[LedgerUtils.Columns.ADDRESS],
  };
};
