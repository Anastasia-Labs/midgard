/** Reconstruct a transaction event's ledger from authenticated predecessor and
 * committed earlier source effects. Every intermediate root is checked. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  checkLedgerAgainstRoot,
  type LedgerEntry,
  LedgerRootMismatchError,
} from "../ledger-at.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
} from "../transition-trace/reconstruct.js";
import { requireReplayPredecessorLedger } from "../workflow/replay-predecessor-ledger.js";

/** A committed prefix cannot supply the selected event's ledger authority.
 * Detectors surface a gap; no missing-output conviction is inferred from it. */
export class EventLedgerReplayRefusal extends Error {
  constructor(
    readonly reason:
      | "normal_source_not_accepted"
      | "step_out_of_range"
      | "transition_link_changed"
      | "source_event_changed"
      | "phase_order_changed"
      | "withdrawal_input_absent"
      | "transaction_input_absent"
      | "output_already_present"
      | "post_root_changed"
      | "selected_pre_root_changed",
  ) {
    super(`input-existence: event ledger replay refused (${reason})`);
    this.name = "EventLedgerReplayRefusal";
  }
}

const phaseRank = (phase: SDK.TransitionPhase): number =>
  ["Withdrawal", "ForcedTransaction", "L2Transaction", "Deposit"].indexOf(
    phase,
  );

const acceptedEffects = (
  event: SourceEventRecord,
): Readonly<{
  transactionId: string;
  inputs: readonly Buffer[];
  outputs: readonly Buffer[];
}> | null => {
  if (event.phase === "ForcedTransaction") {
    if (event.entry.value.verdict !== "ForcedTxValid") return null;
    const tx = decodeMidgardForcedTxFullFromCanonicalCbor(
      event.entry.fullTransactionCbor,
    );
    return {
      transactionId: event.entry.value.tx_id,
      inputs: decodeMidgardFieldPreimage(tx.body.spendInputsPreimageCbor),
      outputs: decodeMidgardFieldPreimage(tx.body.outputsPreimageCbor),
    };
  }
  if (event.phase === "L2Transaction") {
    if (event.entry.validity !== "TxIsValid") {
      throw new EventLedgerReplayRefusal("normal_source_not_accepted");
    }
    return {
      transactionId: event.entry.txId,
      inputs: decodeMidgardFieldPreimage(event.entry.spendInputsPreimage),
      outputs: decodeMidgardFieldPreimage(event.entry.outputsPreimage),
    };
  }
  return null;
};

/** The ledger walk's position: every step before `next` is applied. */
type WalkCursor = {
  next: bigint;
  ledger: CheckedLedger;
  entries: Map<string, LedgerEntry>;
  priorPhase: number;
  /** The step whose application failed; the walk never passes it. */
  failed?: { readonly step: bigint; readonly error: unknown };
};

/** A step whose phase follows the requested event's: refused for this
 * request only, before the step is applied. */
class LaterPhaseStep extends EventLedgerReplayRefusal {}

export type EventLedgerWalk = Readonly<{
  /** The checked ledger before the transaction event `eventKey`, the
   * block's step `stepIndex`. */
  before: (eventKey: SDK.EventKey, stepIndex: bigint) => Promise<CheckedLedger>;
}>;

/**
 * One block's committed steps applied in order, once: each request continues
 * from the last, so the ledgers before several events of the block cost one
 * walk, and each step's root check reuses the descriptors of the ledger
 * before it. A request for an earlier step starts over. A step that fails to
 * apply stops the walk there: a later request meets the same failure, and a
 * request at or before it starts over.
 */
export const createEventLedgerWalk = ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}): EventLedgerWalk => {
  // These are the existing evidence-grade and source-root authorities. A
  // diagnostic/operator-private reconstruction cannot produce proof material.
  blockTransactionsFromCanonicalEvidence(block);
  SDK.assertTransactionSourceInclusionRootAuthenticated(
    block.inclusionRootAuthentication,
  );
  const fresh = (): WalkCursor => {
    const ledger = requireReplayPredecessorLedger({
      block,
      predecessor,
      label: "input-existence",
    });
    return {
      next: 0n,
      ledger,
      entries: new Map(
        ledger.entries.map((entry) => [entry.outRef.toString("hex"), entry]),
      ),
      priorPhase: -1,
    };
  };
  let cursor: WalkCursor | undefined;

  const apply = async (
    walk: WalkCursor,
    index: bigint,
    targetPhase: number,
  ): Promise<void> => {
    const { entries } = walk;
    const opening = block.reconstruction.traceByStepIndex.get(index);
    const step = opening?.value;
    if (
      opening === undefined ||
      step === undefined ||
      opening.key !== index ||
      step.step_index !== index ||
      step.schema_version !== 1n ||
      step.pre_utxos_root !== walk.ledger.utxosRoot
    )
      throw new EventLedgerReplayRefusal("transition_link_changed");
    const event = block.reconstruction.sourceEventsByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (
      event === undefined ||
      event.phase !== step.phase ||
      Data.to(event.eventKey, SDK.EventKey) !==
        Data.to(step.event_key, SDK.EventKey)
    ) {
      throw new EventLedgerReplayRefusal("source_event_changed");
    }
    const rank = phaseRank(event.phase);
    if (rank < walk.priorPhase || event.phase === "Deposit") {
      throw new EventLedgerReplayRefusal("phase_order_changed");
    }
    if (rank > targetPhase) throw new LaterPhaseStep("phase_order_changed");
    walk.priorPhase = rank;
    if (
      event.phase === "Withdrawal" &&
      event.entry.value.validity === "WithdrawalIsValid"
    ) {
      const ref = event.entry.value.body.l2_outref;
      const key = encodeMidgardSpendInputItem({
        txId: Buffer.from(ref.transactionId, "hex"),
        outputIndex: Number(ref.outputIndex),
      }).toString("hex");
      if (!entries.delete(key)) {
        throw new EventLedgerReplayRefusal("withdrawal_input_absent");
      }
    } else {
      const effects = acceptedEffects(event);
      if (effects !== null) {
        for (const input of effects.inputs) {
          if (!entries.delete(input.toString("hex"))) {
            throw new EventLedgerReplayRefusal("transaction_input_absent");
          }
        }
        for (const [outputIndex, output] of effects.outputs.entries()) {
          const outRef = encodeMidgardSpendInputItem({
            txId: Buffer.from(effects.transactionId, "hex"),
            outputIndex,
          });
          const key = outRef.toString("hex");
          if (entries.has(key))
            throw new EventLedgerReplayRefusal("output_already_present");
          entries.set(key, { outRef, output: Buffer.from(output) });
        }
      }
    }
    // Reuse the ledger's sole full-output -> descriptor conversion, including
    // its existing malformed-reference-script representation. No caller
    // supplies a descriptor or bypasses any committed intermediate root.
    try {
      walk.ledger = await checkLedgerAgainstRoot({
        entries: [...entries.values()],
        utxosRoot: step.post_utxos_root,
        label: `input-existence earlier event ${index.toString()}`,
        from: walk.ledger,
      });
    } catch (cause) {
      if (cause instanceof LedgerRootMismatchError)
        throw new EventLedgerReplayRefusal("post_root_changed");
      throw cause;
    }
  };

  return Object.freeze({
    before: async (eventKey: SDK.EventKey, stepIndex: bigint) => {
      if (
        !("L2TransactionEventKey" in eventKey) &&
        !("ForcedTransactionEventKey" in eventKey)
      )
        throw new Error("input-existence: subject is not a transaction event");
      if (stepIndex < 0n || stepIndex >= block.header.transitionStepCount) {
        throw new EventLedgerReplayRefusal("step_out_of_range");
      }
      if (
        cursor === undefined ||
        stepIndex < cursor.next ||
        (cursor.failed !== undefined && stepIndex <= cursor.failed.step)
      )
        cursor = fresh();
      const walk = cursor;
      if (walk.failed !== undefined) throw walk.failed.error;
      const targetPhase = phaseRank(eventKeyPhase(eventKey));
      // The steps already applied are in phase order, so `priorPhase` is the
      // latest phase among them.
      if (walk.priorPhase > targetPhase)
        throw new EventLedgerReplayRefusal("phase_order_changed");
      while (walk.next < stepIndex) {
        try {
          await apply(walk, walk.next, targetPhase);
        } catch (error) {
          if (!(error instanceof LaterPhaseStep))
            walk.failed = { step: walk.next, error };
          throw error;
        }
        walk.next += 1n;
      }
      const target = block.reconstruction.traceByStepIndex.get(stepIndex);
      if (
        target === undefined ||
        target.key !== stepIndex ||
        target.value.step_index !== stepIndex ||
        target.value.schema_version !== 1n ||
        target.value.phase !== eventKeyPhase(eventKey) ||
        Data.to(target.value.event_key, SDK.EventKey) !==
          Data.to(eventKey, SDK.EventKey) ||
        target.value.pre_utxos_root !== walk.ledger.utxosRoot
      ) {
        throw new EventLedgerReplayRefusal("selected_pre_root_changed");
      }
      return walk.ledger;
    },
  });
};

/** The checked ledger before one transaction event of `block`. */
export const ledgerBeforeTransactionEvent = async ({
  block,
  eventKey,
  stepIndex,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly eventKey: SDK.EventKey;
  readonly stepIndex: bigint;
  readonly predecessor?: CheckedLedger;
}): Promise<CheckedLedger> =>
  await createEventLedgerWalk({
    block,
    ...(predecessor === undefined ? {} : { predecessor }),
  }).before(eventKey, stepIndex);
