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
import { buildAuthenticatedRootFromEncodedEntries } from "../../src/workers/commit-block-header/transition-roots.js";

const CHILD_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: "d5".repeat(32),
  outputIndex: 0n,
};

/** Committed with an invalid-signature verdict, so it leaves the ledger as is. */
const CHILD_WITHDRAWAL_INFO: SDK.WithdrawalInfo = {
  body: {
    l2_outref: CHILD_WITHDRAWAL_ID,
    l2_owner: "d6".repeat(28),
    l2_value: new Map([["", new Map([["", 2_000_000n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["d7".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["d8".repeat(32), "d9".repeat(64)],
  validity: "IncorrectWithdrawalSignature",
};

/**
 * The child's one event, a placeholder withdrawal (a committed block carries
 * at least one event), whose single trace step keeps the ledger at
 * `utxosRoot`.
 */
export const signedIntentContinuationChildEvents = (utxosRoot: string) => {
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: CHILD_WITHDRAWAL_ID },
  };
  return {
    withdrawals: [
      [
        Data.to(CHILD_WITHDRAWAL_ID, SDK.OutputReference),
        SDK.committedWithdrawalValueBytes(CHILD_WITHDRAWAL_INFO),
      ],
    ] as SDK.DaPayloadEntry[],
    event_to_step: [
      [
        Data.to(eventKey, SDK.EventKey),
        Data.to({ step_index: 0n, phase: "Withdrawal" }, SDK.EventToStepValue),
      ],
    ] as SDK.DaPayloadEntry[],
    transition_trace: [
      [
        Data.to(0n),
        Data.to(
          {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Withdrawal",
            pre_utxos_root: utxosRoot,
            post_utxos_root: utxosRoot,
          },
          SDK.TransitionStep,
        ),
      ],
    ] as SDK.DaPayloadEntry[],
  };
};

const entriesRoot = (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
) =>
  buildAuthenticatedRootFromEncodedEntries(
    domain,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  ).pipe(Effect.map(({ root }) => root));

/** The header roots of `signedIntentContinuationChildEvents`. */
export const signedIntentContinuationChildRoots = (
  events: ReturnType<typeof signedIntentContinuationChildEvents>,
) =>
  Effect.all({
    withdrawalsRoot: entriesRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      events.withdrawals,
    ),
    eventToStepRoot: entriesRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      events.event_to_step,
    ),
    transitionTraceRoot: entriesRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      events.transition_trace,
    ),
  });

/** Retain the actual independent child's full ledger payload, which carries
 * the placeholder withdrawal. Its parent is still pending local finalization,
 * so the durable journal is the ledger source; a matching root alone cannot
 * authenticate the foreign child. */
export const retainSignedIntentContinuationChildDa = (
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
      withdrawalCount: 1n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      validationTraceCount: 0n,
    };
    const events = signedIntentContinuationChildEvents(
      childHeader.prevUtxosRoot,
    );
    const payload: SDK.DaPayload = {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: childHeaderHash,
        header: childHeader,
        deposits: [],
        withdrawals: events.withdrawals,
        forced_transactions: [],
        transactions: [],
        transaction_preimages: [],
        forced_transaction_preimages: [],
        cek_program_material: [],
        transition_trace: events.transition_trace,
        event_to_step: events.event_to_step,
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
      foreignRetainedDaInsert(
        childHeaderHash,
        childHeader,
        childHeader.endTime,
        bytes,
        roots,
        counts,
      ),
    );
    return { payload, bytes, roots, counts };
  });
