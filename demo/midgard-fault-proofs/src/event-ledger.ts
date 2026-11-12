/**
 * The ledger before each transaction event of a block, from the authenticated
 * predecessor ledger and the block's committed earlier steps. Every
 * intermediate root is checked.
 *
 * A walk applies the block's steps in order, once, to one trie it edits in
 * place, and keeps every entry a step creates or spends. The ledger before an
 * event is a view over the predecessor and those changes, so the ledgers
 * before every event of a block cost one pass over its steps. A request's
 * result does not depend on the requests before it: it is what applying the
 * steps before its event from the predecessor gives.
 */
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
} from "./evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  type LedgerEntry,
  ledgerEntryLeaf,
} from "./ledger-at.js";
import {
  type KeyValuePhasEntry,
  type KeyValuePhasRoot,
  mutableKeyValuePhasTrie,
} from "./transition-trace/phas.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
} from "./transition-trace/reconstruct.js";
import { requireReplayPredecessorLedger } from "./workflow/replay-predecessor-ledger.js";

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

const PHASES: readonly SDK.TransitionPhase[] = [
  "Withdrawal",
  "ForcedTransaction",
  "L2Transaction",
  "Deposit",
];

const phaseRank = (phase: SDK.TransitionPhase): number => PHASES.indexOf(phase);

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

/** What step `step` did to one out-ref: created `created`, or spent it. */
type Change = Readonly<{
  step: number;
  created?: Readonly<{ entry: LedgerEntry; leaf: Buffer }>;
}>;

/** The outputs of the ledger before step `before`, read through `latest`. */
class LedgerOutputs implements ReadonlyMap<string, Buffer> {
  constructor(
    private readonly predecessor: CheckedLedger,
    private readonly latest: (key: string) => Change | undefined,
    private readonly sorted: () => readonly LedgerEntry[],
    readonly size: number,
  ) {}

  get(key: string): Buffer | undefined {
    const change = this.latest(key);
    return change === undefined
      ? this.predecessor.outputs.get(key)
      : change.created?.entry.output;
  }

  has(key: string): boolean {
    return this.get(key) !== undefined;
  }

  *entries(): MapIterator<[string, Buffer]> {
    for (const { outRef, output } of this.sorted())
      yield [outRef.toString("hex"), output];
  }

  *keys(): MapIterator<string> {
    for (const [key] of this.entries()) yield key;
  }

  *values(): MapIterator<Buffer> {
    for (const [, output] of this.entries()) yield output;
  }

  [Symbol.iterator](): MapIterator<[string, Buffer]> {
    return this.entries();
  }

  forEach(
    callback: (
      value: Buffer,
      key: string,
      map: ReadonlyMap<string, Buffer>,
    ) => void,
  ): void {
    for (const [key, output] of this.entries()) callback(output, key, this);
  }
}

/** The block's steps applied from `predecessor` up to `next`. */
type Pass = {
  readonly predecessor: CheckedLedger;
  /** Each out-ref's changes, in step order. */
  readonly changes: Map<string, Change[]>;
  /** `roots[k]` and `sizes[k]`: the ledger before step `k`. */
  readonly roots: string[];
  readonly sizes: number[];
  /** `firstAfter[rank]`: the first applied step of a phase after `rank`. */
  readonly firstAfter: (number | undefined)[];
  next: number;
  priorRank: number;
  /** The step that failed to apply, and why. */
  failure?: Readonly<{ step: number; error: unknown }>;
  trie?: Promise<Awaited<ReturnType<typeof mutableKeyValuePhasTrie>>>;
};

export type EventLedgerWalk = Readonly<{
  /** The checked ledger before the transaction event `eventKey`, the
   * block's step `stepIndex`. */
  before: (eventKey: SDK.EventKey, stepIndex: bigint) => Promise<CheckedLedger>;
}>;

const latestBefore = (pass: Pass, key: string, step: number) => {
  const changes = pass.changes.get(key);
  if (changes === undefined) return undefined;
  for (let index = changes.length - 1; index >= 0; index -= 1)
    if (changes[index]!.step < step) return changes[index];
  return undefined;
};

const record = (walk: Pass, key: string, change: Change) => {
  const changes = walk.changes.get(key);
  if (changes === undefined) walk.changes.set(key, [change]);
  else changes.push(change);
};

/** The ledger before step `step` of `pass`, as a checked ledger. Its entries
 * are merged from the predecessor's and the changes only when read. */
const view = (pass: Pass, step: number): CheckedLedger => {
  const { predecessor } = pass;
  let merged:
    | { entries: readonly LedgerEntry[]; leaves: readonly KeyValuePhasEntry[] }
    | undefined;
  const sorted = () => {
    if (merged !== undefined) return merged;
    const kept: { entry: LedgerEntry; leaf: KeyValuePhasEntry }[] = [];
    const leaves = predecessor.trie.entries;
    for (const [index, entry] of predecessor.entries.entries())
      if (latestBefore(pass, entry.outRef.toString("hex"), step) === undefined)
        kept.push({ entry, leaf: leaves[index]! });
    const created: typeof kept = [];
    for (const key of pass.changes.keys()) {
      const leaf = latestBefore(pass, key, step)?.created;
      if (leaf !== undefined)
        created.push({
          entry: leaf.entry,
          leaf: { key: leaf.entry.outRef, value: leaf.leaf },
        });
    }
    created.sort((left, right) =>
      Buffer.compare(left.entry.outRef, right.entry.outRef),
    );
    const all: typeof kept = [];
    let [k, c] = [0, 0];
    while (k < kept.length || c < created.length)
      all.push(
        c === created.length ||
          (k < kept.length &&
            Buffer.compare(kept[k]!.entry.outRef, created[c]!.entry.outRef) < 0)
          ? kept[k++]!
          : created[c++]!,
      );
    merged = {
      entries: Object.freeze(all.map(({ entry }) => entry)),
      leaves: Object.freeze(all.map(({ leaf }) => leaf)),
    };
    return merged;
  };
  const trie: KeyValuePhasRoot = Object.freeze({
    root: pass.roots[step]!,
    count: BigInt(pass.sizes[step]!),
    get entries() {
      return sorted().leaves;
    },
  });
  return Object.freeze({
    utxosRoot: pass.roots[step]!,
    get entries() {
      return sorted().entries;
    },
    trie,
    outputs: new LedgerOutputs(
      predecessor,
      (key) => latestBefore(pass, key, step),
      () => sorted().entries,
      pass.sizes[step]!,
    ),
  }) as unknown as CheckedLedger;
};

/**
 * One block's committed steps, applied in order and each at most once: the
 * ledgers before several events of the block cost one walk. A step whose
 * phase follows the requested event's refuses that request only, and the
 * walk goes no further for it. A step that fails to apply stops the walk
 * there, and every request past it meets that failure.
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
  let pass: Pass | undefined;
  const start = (): Pass => {
    const ledger = requireReplayPredecessorLedger({
      block,
      predecessor,
      label: "input-existence",
    });
    return {
      predecessor: ledger,
      changes: new Map(),
      roots: [ledger.utxosRoot],
      sizes: [ledger.entries.length],
      firstAfter: PHASES.map(() => undefined),
      next: 0,
      priorRank: -1,
    };
  };

  const current = (key: string): boolean => {
    const change = latestBefore(pass!, key, pass!.next + 1);
    return change === undefined
      ? pass!.predecessor.outputs.has(key)
      : change.created !== undefined;
  };

  /** Applies step `pass.next`, or records why it fails. */
  const apply = async (walk: Pass): Promise<void> => {
    const index = walk.next;
    try {
      const opening = block.reconstruction.traceByStepIndex.get(BigInt(index));
      const step = opening?.value;
      if (
        opening === undefined ||
        step === undefined ||
        opening.key !== BigInt(index) ||
        step.step_index !== BigInt(index) ||
        step.schema_version !== 1n ||
        step.pre_utxos_root !== walk.roots[index]
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
      )
        throw new EventLedgerReplayRefusal("source_event_changed");
      const stepRank = phaseRank(event.phase);
      if (stepRank < walk.priorRank || event.phase === "Deposit")
        throw new EventLedgerReplayRefusal("phase_order_changed");
      for (let later = 0; later < stepRank; later += 1)
        walk.firstAfter[later] ??= index;
      const spent: string[] = [];
      const created: { entry: LedgerEntry; leaf: Buffer }[] = [];
      const spend = (key: string, refusal: EventLedgerReplayRefusal) => {
        if (!current(key)) throw refusal;
        record(walk, key, { step: index });
        spent.push(key);
      };
      if (
        event.phase === "Withdrawal" &&
        event.entry.value.validity === "WithdrawalIsValid"
      ) {
        const ref = event.entry.value.body.l2_outref;
        spend(
          encodeMidgardSpendInputItem({
            txId: Buffer.from(ref.transactionId, "hex"),
            outputIndex: Number(ref.outputIndex),
          }).toString("hex"),
          new EventLedgerReplayRefusal("withdrawal_input_absent"),
        );
      } else {
        const effects = acceptedEffects(event);
        if (effects !== null) {
          for (const input of effects.inputs)
            spend(
              input.toString("hex"),
              new EventLedgerReplayRefusal("transaction_input_absent"),
            );
          const outputs = effects.outputs.map((output, outputIndex) => {
            const outRef = encodeMidgardSpendInputItem({
              txId: Buffer.from(effects.transactionId, "hex"),
              outputIndex,
            });
            if (current(outRef.toString("hex")))
              throw new EventLedgerReplayRefusal("output_already_present");
            return { outRef, output: Buffer.from(output) };
          });
          // The ledger's sole full-output -> descriptor conversion, including
          // its existing malformed-reference-script representation.
          for (const entry of outputs)
            created.push(
              ledgerEntryLeaf(entry, `input-existence earlier event ${index}`),
            );
          for (const leaf of created)
            record(walk, leaf.entry.outRef.toString("hex"), {
              step: index,
              created: leaf,
            });
        }
      }
      const trie = await (walk.trie ??= mutableKeyValuePhasTrie(
        walk.predecessor.trie.entries,
      ));
      for (const key of spent) await trie.delete(Buffer.from(key, "hex"));
      for (const { entry, leaf } of created)
        await trie.insert(entry.outRef, leaf);
      if (trie.root() !== step.post_utxos_root)
        throw new EventLedgerReplayRefusal("post_root_changed");
      walk.roots.push(step.post_utxos_root);
      walk.sizes.push(walk.sizes[index]! - spent.length + created.length);
      walk.priorRank = stepRank;
      walk.next += 1;
      if (walk.next === Number(block.header.transitionStepCount))
        delete walk.trie;
    } catch (error) {
      walk.failure = { step: index, error };
      delete walk.trie;
    }
  };

  let applying = Promise.resolve();
  /** Applies steps until step `stepIndex`, a failure, or a step of a phase
   * after `rank`; one request at a time. */
  const advance = (walk: Pass, stepIndex: number, rank: number) =>
    (applying = applying.then(async () => {
      while (
        walk.next < stepIndex &&
        walk.failure === undefined &&
        walk.firstAfter[rank] === undefined
      )
        await apply(walk);
    }));

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
      const walk = (pass ??= start());
      const target = Number(stepIndex);
      const rank = phaseRank(eventKeyPhase(eventKey));
      await advance(walk, target, rank);
      // A step records its phase before it can fail on its effects, so a
      // later-phase step is never past the walk's failure.
      const later = walk.firstAfter[rank];
      if (later !== undefined && later < target)
        throw new EventLedgerReplayRefusal("phase_order_changed");
      const { failure } = walk;
      if (failure !== undefined && failure.step < target) throw failure.error;
      const opening = block.reconstruction.traceByStepIndex.get(stepIndex);
      if (
        opening === undefined ||
        opening.key !== stepIndex ||
        opening.value.step_index !== stepIndex ||
        opening.value.schema_version !== 1n ||
        opening.value.phase !== eventKeyPhase(eventKey) ||
        Data.to(opening.value.event_key, SDK.EventKey) !==
          Data.to(eventKey, SDK.EventKey) ||
        opening.value.pre_utxos_root !== walk.roots[target]
      ) {
        throw new EventLedgerReplayRefusal("selected_pre_root_changed");
      }
      return view(walk, target);
    },
  });
};

/** `createEventLedgerWalk`, created at its first request: a caller whose
 * events may need no ledger meets the walk's evidence checks only when one
 * does. */
export const lazyEventLedgerWalk = (
  input: Parameters<typeof createEventLedgerWalk>[0],
): EventLedgerWalk => {
  let walk: EventLedgerWalk | undefined;
  return Object.freeze({
    before: async (eventKey: SDK.EventKey, stepIndex: bigint) =>
      await (walk ??= createEventLedgerWalk(input)).before(eventKey, stepIndex),
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
