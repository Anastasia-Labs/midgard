/**
 * Replay of a foreign landed block (plan §7.3, N3): its DA payload, from
 * the node's retained copy or the public DA transport
 * (`replay-foreign.payload.ts`), replayed on its
 * parent's ledger against the events the follower projects at the view and
 * the forced orders the forced-order projection admitted at the view (every
 * admitted order, live or spent since, whatever its resolution status: the
 * facts at the view, never the node's ingested rows). The block's event
 * sets must be exactly the in-window events known there
 * (`start < inclusion <= end`): a deposit, withdrawal or order id the
 * payload names that is not known there at all is `invalid` where the facts
 * cover the block's range for its kind (`replay-foreign.complete.ts`), else
 * `event_unknown` (see `holds.ts`); a known id outside the window, a known in-window id it leaves out, or one
 * it names twice, is `invalid`; an in-window order whose output cannot be read
 * back at the view is `forced_order_pending`. A block that ends past what
 * the view can know (`view time + event wait - 1`) is `missing`, like a DA
 * payload that is not available yet.
 *
 * While the block's node is Unattested its payload is unbound: no committee
 * commitment fixes the sections its header does not commit. A replay that
 * refuses an unbound payload is `missing`, never `invalid`: the copy is
 * dropped and the next fetch asks the next peer. One that replays imports
 * the block, but its copy is held apart (`replay-foreign.bind.ts`) and never
 * retained or served until the node's commitment is checked against it.
 */
import { outRefToCbor } from "@al-ft/lucid-midgard";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { reconstructMidgardTransaction } from "@al-ft/midgard-core/consensus-validation";
import {
  attributeWithdrawalClassificationFault,
  forcedVerdictForRejection,
} from "@al-ft/midgard-fault-proofs";
import type { FactStore, View } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import { eventsAt } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ForeignDaBinding,
  foreignDaFetchMemo,
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
  type ImportedBlockReplayContext,
  verifyAndImportBlock,
} from "../mpf/verified-block-import.js";
import { NodeConfig } from "../services/config.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { Lucid } from "../services/lucid.js";
import type { ReplayInput, ReplayOutcome } from "./replay.js";
import {
  bindRetainedForeignPayloads,
  keepUnbound,
  type UnboundCopies,
} from "./replay-foreign.bind.js";
import {
  type Absent,
  absentWhere,
  eventCoverage,
  exact,
  unknownAt,
} from "./replay-foreign.complete.js";
import {
  payloadOf,
  type Refetching,
  type Served,
} from "./replay-foreign.payload.js";
import {
  invalid,
  missing,
  unknownEvent,
  Verdict,
} from "./replay-foreign.verdict.js";
import type { WithdrawalMembership } from "./store.js";

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
    const seen = new Set<string>();
    for (const admitted of read.orders) {
      if (admitted.order !== null)
        seen.add(admitted.order.idCbor.toString("hex"));
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
    return { forced, seen };
  });

const depositLedgerKey = (idCbor: string) => {
  const outRef = LucidData.from(idCbor, SDK.OutputReference);
  return outRefToCbor({
    txHash: outRef.transactionId,
    outputIndex: Number(outRef.outputIndex),
  });
};

/** The user-event and forced-source checks of the replay. */
export const foreignReplayMaterial = (
  deposits: ReadonlyMap<string, { key: string; output: Buffer; info: string }>,
  withdrawals: ReadonlyMap<
    string,
    { l2Outref: string; l2Owner: string; l2Value: string; info: string }
  >,
  forced: ReadonlyMap<string, Buffer>,
  memberships: WithdrawalMembership[],
  absent: Absent = unknownAt,
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
          return yield* Effect.fail(absent("deposit", source[0]));
        if (entry.info !== source[1])
          return yield* Effect.fail(
            invalid("a deposit differs from its L1 event"),
          );
        return [{ key: entry.key, output: entry.output }];
      }
      const entry = withdrawals.get(source[0]);
      if (entry === undefined)
        return yield* Effect.fail(absent("withdrawal", source[0]));
      const outRef = yield* WithdrawalsDB.toLedgerOutRef({
        [WithdrawalsDB.Columns.L2_OUTREF]: Buffer.from(entry.l2Outref, "hex"),
      });
      const classified = {
        l2Owner: entry.l2Owner,
        l2ValueCbor: entry.l2Value,
        eventInfoCbor: entry.info,
        ledgerOutRef: outRef,
        ledgerOutput: ledger.get(outRef.toString("hex")) ?? null,
      };
      // A runtime fault of the classifier is this node's (`local_fault`),
      // never the block's.
      const classification = yield* attributeWithdrawalClassificationFault(
        classified,
        SDK.classifyWithdrawalFromLedger(classified),
      );
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
        // Every step reads only the order's L1 bytes and the payload's
        // bytes, so a throw is the block's fault, never a local one.
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

/** What the replayer holds of the payload it is replaying. */
type Held = { bound: boolean; served: Served };

/**
 * The replayer over the follower's event projection at the view, and the
 * pass that binds the copies it imported unbound once their nodes carry a
 * commitment. `availability` gives the deployment's commitment authority.
 */
export const foreignBlockReplayer = (deps: {
  readonly store: FactStore;
  readonly events: EventProjectionConfig;
  readonly forcedOrders: ForcedOrderConfig;
  readonly availability: ForeignDaBinding["authority"];
}) => {
  // One memo per replayer: a header whose DA fetch failed waits out its
  // backoff across driver runs.
  const fetchDa = foreignDaFetchMemo();
  const refetching: Refetching = new Map();
  const unbound: UnboundCopies = new Map();
  /** An unbound copy the replay refused: dropped, so another is fetched. */
  const release = (headerHash: string, held: Held) =>
    Effect.gen(function* () {
      unbound.delete(headerHash);
      if ("peer" in held.served) fetchDa.refuse(headerHash, held.served.peer);
      else
        yield* withFollowerWrite(
          DaPayloadsDB.removeByHeaderHash(Buffer.from(headerHash, "hex")),
        );
    });
  const replay = (input: ReplayInput) => {
    let held: Held | undefined;
    return Effect.gen(function* () {
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
      const seen = new Set<string>();
      for (const list of deps.events.lists) {
        const read = yield* Effect.promise(() =>
          eventsAt(deps.store, list, view.point),
        );
        if (read.kind !== "ok")
          return yield* Effect.fail(
            missing(`the ${list.kind} events are unreadable: ${read.kind}`),
          );
        for (const event of read.value) {
          seen.add(event.idCbor);
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
      const { forced, seen: seenForced } = yield* forcedInWindow(
        deps.store,
        deps.forcedOrders,
        view,
        inWindow,
      );
      // Read after the facts: a prune since only moves it later, and a
      // reset since changes the generation.
      const cursor = yield* Effect.promise(() =>
        deps.store.cursor().catch(() => null),
      );
      const coverage = eventCoverage(
        cursor,
        {
          startTime: header.startTime,
          enteredSlot: input.enteredSlot,
          generation: view.generation,
        },
        (ms) =>
          unixTimeToSlotForConfig(ms, canonicalSlotConfigForLucid(lucid.api)),
      );
      const absent = absentWhere(coverage.user);
      const { payload, acquired, bound, served } = yield* payloadOf(
        fetchDa,
        refetching,
        headerHash,
        header,
        input.blockTime,
        { status: input.daAvailability, authority: deps.availability },
      );
      held = { bound, served };
      const body = payload.block_body;
      const mismatch =
        exact(
          new Set(deposits.keys()),
          seen,
          body.deposits,
          "deposit",
          absent,
        ) ??
        exact(
          new Set(withdrawals.keys()),
          seen,
          body.withdrawals,
          "withdrawal",
          absent,
        ) ??
        exact(
          new Set(forced.keys()),
          seenForced,
          body.forced_transactions,
          "forced order",
          absentWhere(coverage.txOrder),
        );
      if (mismatch !== undefined) return yield* Effect.fail(mismatch);
      const memberships: WithdrawalMembership[] = [];
      const imported = yield* verifyAndImportBlock({
        ...foreignReplayMaterial(
          deposits,
          withdrawals,
          forced,
          memberships,
          absent,
        ),
        header,
        headerHash,
        parentHeaderHash: input.parentHeaderHash,
        parentUtxosRoot: input.parentUtxosRoot,
        parentEntries: input.parentEntries,
        payload,
        expectedNetworkId: config.NETWORK === "Mainnet" ? 1n : 0n,
        minFeeA: config.MIN_FEE_A,
        minFeeB: config.MIN_FEE_B,
        // Validity is judged at the block's time: its node's block_time.
        blockTimeMs: Number(input.blockTime),
        blockSlot: BigInt(
          unixTimeToSlotForConfig(
            Number(input.blockTime),
            canonicalSlotConfigForLucid(lucid.api),
          ),
        ),
        l1SlotConfig: canonicalSlotConfigForLucid(lucid.api),
      });
      // Only a block that replayed keeps the payload it fetched, and only a
      // bound copy is retained; an unbound one waits for its commitment.
      if (acquired !== undefined && bound)
        yield* withFollowerWrite(DaPayloadsDB.upsertAvailable(acquired));
      else if (acquired !== undefined)
        keepUnbound(unbound, headerHash, acquired);
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
        scriptChecks: imported.scriptChecks,
      } satisfies ReplayOutcome;
    }).pipe(
      Effect.catchAll((error) => {
        const outcome =
          error instanceof Verdict
            ? ({ kind: error.kind, detail: error.detail } as const)
            : error instanceof ForeignBlockVerificationError
              ? ({ kind: error.reason, detail: error.detail } as const)
              : undefined;
        if (outcome === undefined) return Effect.fail(error);
        if (outcome.kind !== "invalid" || held === undefined || held.bound)
          return Effect.succeed(outcome satisfies ReplayOutcome);
        return Effect.as(release(input.headerHash, held), {
          kind: "missing",
          detail: `the block's payload is not bound to an attested commitment yet, and this copy does not replay: ${outcome.detail}`,
        } satisfies ReplayOutcome);
      }),
    );
  };
  return {
    replay,
    bind: bindRetainedForeignPayloads({
      fetchDa,
      unbound,
      authority: deps.availability,
    }),
  };
};

/** The replayer alone (`foreignBlockReplayer`'s `replay`). */
export const replayForeignBlock = (
  deps: Parameters<typeof foreignBlockReplayer>[0],
) => foreignBlockReplayer(deps).replay;
