import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect } from "vitest";

import { foreignRetainedDaInsert } from "../../src/da/foreign-retained-da.js";
import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import { materializeConfirmedLedgerSnapshot } from "../../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import {
  computeDaPayloadRoots,
  headerCounts,
  headerRoots,
} from "../../src/workers/commit-block-header/da-payload.compute-da-payload-roots.js";

/** Retain the actual independent empty child's full ledger payload. Its parent
 * is still pending local finalization, so the durable journal is the ledger
 * source; a matching root alone cannot authenticate the foreign child. */
export const retainSignedIntentContinuationEmptyChildDa = (
  parentHeaderHash: string,
  childHeaderHash: string,
  childHeader: SDK.Header,
) =>
  Effect.gen(function* () {
    expect(childHeader.prevHeaderHash).toBe(parentHeaderHash);
    expect(yield* SDK.hashBlockHeader(childHeader)).toBe(childHeaderHash);
    const parent = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
      Buffer.from(parentHeaderHash, "hex"),
    );
    if (Option.isNone(parent))
      throw new Error("The signed parent has no durable ledger journal");
    const child = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
      Buffer.from(childHeaderHash, "hex"),
    );
    expect(Option.isNone(child)).toBe(true);
    const snapshot = yield* materializeConfirmedLedgerSnapshot(parent.value);
    expect(snapshot.root).toBe(childHeader.prevUtxosRoot);
    const counts: SDK.DaPayloadCounts = {
      depositCount: 0n,
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    };
    const payload: SDK.DaPayload = {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: childHeaderHash,
        header: childHeader,
        utxos: snapshot.entries.map((entry) => [
          entry.outref.toString("hex"),
          entry.output.toString("hex"),
        ]),
        deposits: [],
        withdrawals: [],
        forced_transactions: [],
        transactions: [],
        transaction_preimages: [],
        forced_transaction_preimages: [],
        cek_program_material: [],
        transition_trace: [],
        event_to_step: [],
        validation_traces: [],
        validation_trace_witnesses: [],
        counts,
      },
    };
    const roots = yield* computeDaPayloadRoots(payload);
    expect(roots).toEqual(headerRoots(childHeader));
    expect(counts).toEqual(headerCounts(childHeader));
    const bytes = yield* Effect.tryPromise(() =>
      wrapDaPayload(Buffer.from(Data.to(payload, SDK.DaPayload), "hex"), {
        mode: "identity",
      }),
    );
    yield* DaPayloadsDB.upsertAvailable(
      foreignRetainedDaInsert(childHeaderHash, childHeader, bytes),
    );
    return { payload, bytes, roots, counts };
  });
