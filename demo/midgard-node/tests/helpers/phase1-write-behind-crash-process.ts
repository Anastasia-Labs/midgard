import { Effect } from "effect";

import * as TxAdmissionsDB from "@/database/txAdmissions.js";
import type * as Ledger from "@/database/utils/ledger.js";
import { NodeConfig } from "@/services/config.js";
import { Database } from "@/services/database.js";
import {
  WriteBehind,
  type WriteBehindService,
} from "@/services/write-behind.js";
import type { ProcessedTx } from "@/utils.js";

type CrashInput = {
  readonly txIdHex: string;
  readonly txCanonicalCborHex: string;
  readonly leaseOwner: string;
  readonly spentOutrefHexes: readonly string[];
  readonly produced: readonly {
    readonly txIdHex: string;
    readonly outrefHex: string;
    readonly outputHex: string;
    readonly address: string;
  }[];
};

const rawInput = process.env.PHASE1_WRITE_BEHIND_CRASH_INPUT;
if (rawInput === undefined || rawInput === "") {
  throw new Error("PHASE1_WRITE_BEHIND_CRASH_INPUT is required");
}
const input = JSON.parse(rawInput) as CrashInput;
const txId = Buffer.from(input.txIdHex, "hex");
const processedTx: ProcessedTx = {
  txId,
  txCbor: Buffer.from(input.txCanonicalCborHex, "hex"),
  spent: input.spentOutrefHexes.map((value) => Buffer.from(value, "hex")),
  produced: input.produced.map(
    (entry): Ledger.Entry => ({
      tx_id: Buffer.from(entry.txIdHex, "hex"),
      outref: Buffer.from(entry.outrefHex, "hex"),
      output: Buffer.from(entry.outputHex, "hex"),
      address: entry.address as Ledger.Entry["address"],
    }),
  ),
};

const crashBoundaryWriteBehind: WriteBehindService = {
  enqueueTxDeltas: () =>
    Effect.sync(() => {
      process.stdout.write("phase1_accept_committed_before_write_behind\n");
    }).pipe(Effect.zipRight(Effect.never)),
  enqueueAddressHistory: () => Effect.dieMessage("unexpected address enqueue"),
  flushNow: Effect.void,
  depths: Effect.succeed({ queueDepth: 0, pendingDepth: 0, totalDepth: 0 }),
  run: Effect.never,
};

Effect.runPromise(
  TxAdmissionsDB.markAccepted({
    rows: [{ tx_id: txId }],
    leaseOwner: input.leaseOwner,
    processedTxs: [processedTx],
  }).pipe(
    Effect.provideService(WriteBehind, crashBoundaryWriteBehind),
    Effect.provide(Database.layer),
    Effect.provide(NodeConfig.layer),
  ),
).catch((error: unknown) => {
  console.error(error);
  process.exitCode = 1;
});
