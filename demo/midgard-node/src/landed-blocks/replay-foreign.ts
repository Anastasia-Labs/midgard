/**
 * Replay of a foreign landed block (plan §7.3, N3): its DA payload, from
 * the node's retained copy or the public DA transport, replayed on its
 * parent's ledger against the events the follower projects at the view and
 * the forced orders the forced-order projection admitted at the view (every
 * admitted order, live or spent since, whatever its resolution status: the
 * facts at the view, never the node's ingested rows). The block's event
 * sets must be exactly the in-window events known there
 * (`start < inclusion <= end`): an id the payload names that is not known
 * there is `event_unknown`; a known in-window id it leaves out, or one it
 * names twice, is `invalid`; an in-window order whose output cannot be read
 * back at the view is `forced_order_pending`. A block that ends past what
 * the view can know (`view time + event wait - 1`) is `missing`, like a DA
 * payload that is not available yet.
 */
import { outRefToCbor } from "@al-ft/lucid-midgard";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { reconstructMidgardTransaction } from "@al-ft/midgard-core/consensus-validation";
import { forcedVerdictForRejection } from "@al-ft/midgard-fault-proofs";
import type { FactStore, View } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import { eventsAt } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  decodeStoredPayload,
  fetchForeignRetainedDa,
  foreignRetainedDaInsert,
} from "../da/foreign-retained-da.js";
import {
  DaPayloadsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  type ForcedOrderConfig,
  forcedOrdersAdmittedAt,
  outRefLabel,
} from "../forced-orders/index.js";
import { userEventEntry } from "../l1-events/entries.js";
import {
  canonicalSlotConfigForLucid,
  unixTimeToSlotForConfig,
} from "../lucid-time.js";
import {
  ForeignBlockVerificationError,
  verifyAndImportBlock,
} from "../mpf/verified-block-import.js";
import type { ImportedBlockReplayContext } from "../mpf/verified-block-import.replay-events.js";
import { NodeConfig } from "../services/config.js";
import {
  runHistoryProducer,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import { Lucid } from "../services/lucid.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";
import { sha256 } from "../sha256.js";
import type { ReplayInput, ReplayOutcome } from "./replay.js";
import type { WithdrawalMembership } from "./store.js";

/** A block's verdict, carried through the replay's error channel. */
class Verdict extends Error {
  constructor(
    readonly kind: Exclude<ReplayOutcome["kind"], "replayed">,
    readonly detail: string,
  ) {
    super(detail);
  }
}

const missing = (detail: string) => new Verdict("missing", detail);
const unknownEvent = (detail: string) => new Verdict("event_unknown", detail);
const invalid = (detail: string) => new Verdict("invalid", detail);

const exact = (
  known: ReadonlySet<string>,
  named: readonly SDK.DaPayloadEntry[],
  kind: string,
) => {
  const ids = named.map(([key]) => key);
  if (new Set(ids).size !== ids.length)
    return invalid(`the block names a ${kind} twice`);
  const unknown = ids.find((id) => !known.has(id));
  if (unknown !== undefined)
    return unknownEvent(`${kind} ${unknown} is not known at the view`);
  const left = [...known].find((id) => !ids.includes(id));
  if (left !== undefined)
    return invalid(`the block leaves out in-window ${kind} ${left}`);
  return undefined;
};

/** The block's DA payload, and the row to retain if it was fetched. */
const payloadOf = (headerHash: string, header: SDK.Header) =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    const stored = yield* DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(headerHash, "hex"),
    );
    if (Option.isNone(stored)) {
      const acquired = yield* fetchForeignRetainedDa(headerHash, header);
      return {
        payload: acquired.payload,
        acquired: foreignRetainedDaInsert(
          headerHash,
          header,
          acquired.payloadBytes,
        ),
      };
    }
    const row = stored.value;
    if (
      !row[DaPayloadsDB.Columns.HEADER_HASH].equals(
        Buffer.from(headerHash, "hex"),
      ) ||
      row[DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID] !==
        identity.consensusProfile.profileId ||
      !sha256(row[DaPayloadsDB.Columns.PAYLOAD_CBOR]).equals(
        row[DaPayloadsDB.Columns.PAYLOAD_SHA256],
      )
    )
      return yield* Effect.fail(
        invalid("the retained DA payload's identity differs from the block"),
      );
    const payload = yield* Effect.tryPromise({
      try: () =>
        decodeStoredPayload({
          payloadCbor: row[DaPayloadsDB.Columns.PAYLOAD_CBOR],
          schemaVersion: row[DaPayloadsDB.Columns.VERSION],
        }),
      catch: (cause) =>
        invalid(`the retained DA payload does not decode: ${String(cause)}`),
    });
    // The body is the block's before its event lists are read, as a fetched
    // payload's is.
    if (
      payload.block_body.header_hash !== headerHash ||
      (yield* SDK.hashBlockHeader(payload.block_body.header).pipe(
        Effect.orElseSucceed(() => undefined),
      )) !== headerHash
    )
      return yield* Effect.fail(
        invalid("the retained DA payload's identity differs from the block"),
      );
    return { payload, acquired: undefined };
  });

/**
 * The forced orders admitted in the block's window at the view: order id
 * hex to raw datum, from the facts. An in-window order whose output cannot
 * be read back there is a wait.
 */
const forcedInWindow = (
  store: FactStore,
  config: ForcedOrderConfig,
  view: View,
  inWindow: (time: bigint) => boolean,
) =>
  Effect.gen(function* () {
    const read = yield* Effect.promise(() =>
      forcedOrdersAdmittedAt(store, config, view.point),
    );
    if (read.kind !== "ok")
      return yield* Effect.fail(
        missing(`the forced orders are unreadable: ${read.kind}`),
      );
    const forced = new Map<string, Buffer>();
    for (const admitted of read.orders) {
      if (!inWindow(admitted.inclusionTime)) continue;
      if (admitted.order === null)
        return yield* Effect.fail(
          new Verdict(
            "forced_order_pending",
            `forced order ${outRefLabel(admitted.outRef)} is admitted in the window but its output cannot be read at the view`,
          ),
        );
      forced.set(admitted.order.idCbor.toString("hex"), admitted.order.datum);
    }
    return forced;
  });

const depositLedgerKey = (idCbor: string) => {
  const outRef = LucidData.from(idCbor, SDK.OutputReference);
  return outRefToCbor({
    txHash: outRef.transactionId,
    outputIndex: Number(outRef.outputIndex),
  });
};

/** The user-event and forced-source checks of the replay. */
const material = (
  deposits: ReadonlyMap<string, { key: string; output: Buffer; info: string }>,
  withdrawals: ReadonlyMap<
    string,
    { l2Outref: string; l2Owner: string; l2Value: string; info: string }
  >,
  forced: ReadonlyMap<string, Buffer>,
  memberships: WithdrawalMembership[],
) => {
  const replayUserEvent: ImportedBlockReplayContext["replayUserEvent"] = ({
    step,
    source,
    ledger,
  }) =>
    Effect.gen(function* () {
      if (step.phase === "Deposit") {
        const entry = deposits.get(source[0]);
        if (entry === undefined)
          return yield* Effect.fail(unknownEvent("a deposit is not known"));
        if (entry.info !== source[1])
          return yield* Effect.fail(
            invalid("a deposit differs from its L1 event"),
          );
        return [{ key: entry.key, output: entry.output }];
      }
      const entry = withdrawals.get(source[0]);
      if (entry === undefined)
        return yield* Effect.fail(unknownEvent("a withdrawal is not known"));
      const outRef = yield* WithdrawalsDB.toLedgerOutRef({
        [WithdrawalsDB.Columns.L2_OUTREF]: Buffer.from(entry.l2Outref, "hex"),
      });
      const classification = yield* SDK.classifyWithdrawalFromLedger({
        l2Owner: entry.l2Owner,
        l2ValueCbor: entry.l2Value,
        eventInfoCbor: entry.info,
        ledgerOutRef: outRef,
        ledgerOutput: ledger.get(outRef.toString("hex")) ?? null,
      });
      const settlement = classification.settlementEventInfo.toString("hex");
      if (settlement !== source[1])
        return yield* Effect.fail(
          invalid("a withdrawal's verdict differs from its replay"),
        );
      memberships.push({
        id: source[0],
        validity: classification.validity,
        detail: classification.validityDetail,
        settlement,
      });
      return classification.shouldDeleteLedgerUtxo
        ? [{ key: outRef.toString("hex"), output: null }]
        : [];
    });
  const verifyForcedSource: ImportedBlockReplayContext["verifyForcedSource"] =
    ({ source, canonicalTransactionCbor, rejection }) =>
      Effect.gen(function* () {
        const datum = forced.get(source[0]);
        if (datum === undefined)
          return yield* Effect.fail(
            unknownEvent("a forced order is not known"),
          );
        yield* Effect.try({
          try: () => {
            const payload = SDK.decodeTxOrderDatumCbor(datum).event.tx;
            const derived = deriveMidgardForcedTxFaultEvidenceMaterial(
              canonicalTransactionCbor,
            );
            const reconstructed = reconstructMidgardTransaction({
              sourceKind: "forced",
              transactionId: Buffer.from(payload.tx_id, "hex"),
              transactionCommitment: Buffer.from(
                payload.transaction_commitment,
                "hex",
              ),
              source: {
                compactCbor: Buffer.from(
                  payload.submitted_source.compact_cbor,
                  "hex",
                ),
                witnessSetCompactCbor: Buffer.from(
                  payload.submitted_source.witness_set_compact_cbor,
                  "hex",
                ),
                fieldPreimageLengthsCbor: Buffer.from(
                  payload.submitted_source.field_preimage_lengths_cbor,
                  "hex",
                ),
              },
              fieldPreimages: derived.fieldPreimages,
            });
            if (!reconstructed.equals(canonicalTransactionCbor))
              throw new Error("it differs from the order's commitments");
          },
          catch: (cause) =>
            invalid(
              `a forced transaction differs from its order: ${String(cause)}`,
            ),
        });
        const encoded =
          yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
            nativeTxCbor: canonicalTransactionCbor,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            verdict:
              rejection === undefined
                ? "ForcedTxValid"
                : forcedVerdictForRejection(rejection),
          });
        if (encoded.value.toString("hex") !== source[1])
          return yield* Effect.fail(
            invalid("a forced transaction's verdict differs from its replay"),
          );
      });
  return { replayUserEvent, verifyForcedSource };
};

/** The replayer over the follower's event projection at the view. */
export const replayForeignBlock =
  (deps: {
    readonly store: FactStore;
    readonly events: EventProjectionConfig;
    readonly forcedOrders: ForcedOrderConfig;
  }) =>
  (input: ReplayInput) =>
    Effect.gen(function* () {
      const config = yield* NodeConfig;
      const lucid = yield* Lucid;
      const { header, headerHash, view } = input;
      const horizon =
        lucid.api.slotToUnixTime(view.point.slot) +
        SDK.EVENT_WAIT_DURATION_MS -
        1;
      if (header.endTime > BigInt(horizon))
        return yield* Effect.fail(
          missing("the block ends past what the follower view can know"),
        );
      const inWindow = (time: bigint) =>
        time > header.startTime && time <= header.endTime;
      const deposits = new Map<
        string,
        { key: string; output: Buffer; info: string }
      >();
      const withdrawals = new Map<
        string,
        { l2Outref: string; l2Owner: string; l2Value: string; info: string }
      >();
      for (const list of deps.events.lists) {
        const read = yield* Effect.promise(() =>
          eventsAt(deps.store, list, view.point),
        );
        if (read.kind !== "ok")
          return yield* Effect.fail(
            missing(`the ${list.kind} events are unreadable: ${read.kind}`),
          );
        for (const event of read.value) {
          if (!inWindow(event.inclusionTime)) continue;
          const decoded = userEventEntry(event, config.NETWORK);
          if (decoded.kind === "deposit")
            deposits.set(decoded.entry.idCbor, {
              key: depositLedgerKey(decoded.entry.idCbor).toString("hex"),
              output: Buffer.from(decoded.entry.ledgerOutput, "hex"),
              info: decoded.entry.infoCbor,
            });
          else
            withdrawals.set(decoded.entry.idCbor, {
              l2Outref: decoded.entry.l2Outref,
              l2Owner: decoded.entry.l2Owner,
              l2Value: decoded.entry.l2Value,
              info: decoded.entry.rawEventInfo,
            });
        }
      }
      const forced = yield* forcedInWindow(
        deps.store,
        deps.forcedOrders,
        view,
        inWindow,
      );
      const { payload, acquired } = yield* payloadOf(headerHash, header);
      const body = payload.block_body;
      const mismatch =
        exact(new Set(deposits.keys()), body.deposits, "deposit") ??
        exact(new Set(withdrawals.keys()), body.withdrawals, "withdrawal") ??
        exact(new Set(forced.keys()), body.forced_transactions, "forced order");
      if (mismatch !== undefined) return yield* Effect.fail(mismatch);
      const memberships: WithdrawalMembership[] = [];
      const imported = yield* verifyAndImportBlock({
        ...material(deposits, withdrawals, forced, memberships),
        header,
        headerHash,
        parentHeaderHash: input.parentHeaderHash,
        parentUtxosRoot: input.parentUtxosRoot,
        parentEntries: input.parentEntries,
        payload,
        expectedNetworkId: config.NETWORK === "Mainnet" ? 1n : 0n,
        minFeeA: config.MIN_FEE_A,
        minFeeB: config.MIN_FEE_B,
        blockSlot: BigInt(
          unixTimeToSlotForConfig(
            Number(header.endTime),
            canonicalSlotConfigForLucid(lucid.api),
          ),
        ),
      });
      // Only a block that replayed keeps the payload it fetched.
      if (acquired !== undefined)
        yield* runHistoryProducer(
          withHistoryWrite(DaPayloadsDB.upsertAvailable(acquired)),
        );
      return {
        kind: "replayed",
        entries: imported.entries,
        root: imported.root,
        depositIds: body.deposits.map(([key]) => Buffer.from(key, "hex")),
        withdrawals: memberships,
        forcedIds: body.forced_transactions.map(([key]) =>
          Buffer.from(key, "hex"),
        ),
        txIds: body.transactions.map(([key]) => Buffer.from(key, "hex")),
      } satisfies ReplayOutcome;
    }).pipe(
      Effect.catchAll((error) =>
        error instanceof Verdict
          ? Effect.succeed({
              kind: error.kind,
              detail: error.detail,
            } satisfies ReplayOutcome)
          : error instanceof ForeignBlockVerificationError
            ? Effect.succeed({
                kind: error.reason,
                detail: error.detail,
              } satisfies ReplayOutcome)
            : Effect.fail(error),
      ),
    );
