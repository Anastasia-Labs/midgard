import { createHash } from "node:crypto";

import type { OutRefLike } from "@al-ft/midgard-core/out-ref";

import type { BoundHistoryChainBlock } from "./l1-event-history-source.js";
import type {
  HistoryTransition,
  HistoryTransitionEvent,
} from "./l1-event-history-transition.js";

type Placement = Readonly<{
  blockHash: string;
  slot: number;
  height: number;
  transactionHash: string;
  transactionIndex: number;
}>;
type Retirement = NonNullable<HistoryTransition["retirement"]>;

/** The admission output identifies an immutable incarnation. Its canonical
 * placement can change if the identical transaction is included on a new branch.
 * Null placement retains an orphan for audit; it grants no event eligibility. */
export type HistoryIncarnation = Readonly<{
  id: string;
  bindingDigest: string;
  kind: HistoryTransition["kind"];
  event: HistoryTransitionEvent;
  placement: Readonly<{
    admission: Placement;
    current: Readonly<{ outRef: OutRefLike; at: Placement }> | null;
    retirement: Readonly<{
      at: Placement;
      outRef: OutRefLike;
      reason: Retirement["reason"];
      observerRedeemerIndex: number;
      witnessCbor: string;
    }> | null;
  }> | null;
}>;

export type HistoryProvenanceChange = Readonly<{
  before: HistoryIncarnation | null;
  after: HistoryIncarnation;
}>;

const fail = (message: string): never => {
  throw new Error(`Invalid history provenance: ${message}`);
};
const ref = (value: OutRefLike): OutRefLike =>
  Object.freeze({ txHash: value.txHash, outputIndex: value.outputIndex });
const sameRef = (a: OutRefLike, b: OutRefLike) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;
const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const facts = (event: HistoryTransitionEvent) => [
  event.key,
  event.idCbor,
  event.inclusionTime.toString(),
  event.factsCbor,
  event.payloadCbor,
  event.originalAssetsCbor,
];
const sameFacts = (a: HistoryTransitionEvent, b: HistoryTransitionEvent) =>
  JSON.stringify(facts(a)) === JSON.stringify(facts(b));

export const historyIncarnationId = (
  bindingDigest: string,
  kind: HistoryTransition["kind"],
  event: HistoryTransitionEvent,
): string =>
  hash(
    JSON.stringify([
      "midgard-node-history-incarnation-v1",
      bindingDigest,
      kind,
      event.idCbor,
      event.outRef.txHash,
      event.outRef.outputIndex,
    ]),
  );

/** A stable fingerprint for checked before/after images, independent of object
 * property insertion order. This is local integrity, not L1 authentication. */
export const historyIncarnationDigest = (value: HistoryIncarnation): string => {
  const at = (p: Placement) => [
    p.blockHash,
    p.slot,
    p.height,
    p.transactionHash,
    p.transactionIndex,
  ];
  const out = (r: OutRefLike) => [r.txHash, r.outputIndex];
  const p = value.placement;
  return hash(
    JSON.stringify([
      value.id,
      value.bindingDigest,
      value.kind,
      facts(value.event),
      out(value.event.outRef),
      p === null
        ? null
        : [
            at(p.admission),
            p.current === null
              ? null
              : [out(p.current.outRef), at(p.current.at)],
            p.retirement === null
              ? null
              : [
                  at(p.retirement.at),
                  out(p.retirement.outRef),
                  p.retirement.reason,
                  p.retirement.observerRedeemerIndex,
                  p.retirement.witnessCbor,
                ],
          ],
    ]),
  );
};

const freezeIncarnation = (value: HistoryIncarnation): HistoryIncarnation => {
  const p = value.placement;
  return Object.freeze({
    ...value,
    event: Object.freeze({ ...value.event, outRef: ref(value.event.outRef) }),
    placement:
      p === null
        ? null
        : Object.freeze({
            admission: Object.freeze({ ...p.admission }),
            current:
              p.current === null
                ? null
                : Object.freeze({
                    outRef: ref(p.current.outRef),
                    at: Object.freeze({ ...p.current.at }),
                  }),
            retirement:
              p.retirement === null
                ? null
                : Object.freeze({
                    ...p.retirement,
                    outRef: ref(p.retirement.outRef),
                    at: Object.freeze({ ...p.retirement.at }),
                  }),
          }),
  });
};

/** Stage ordered, already validated transitions without touching D/W L2 rows.
 * Caller supplies all stored incarnations for the touched (kind,event key) pairs,
 * including retired/orphaned records, from one cursor revision. An authenticated
 * initialization replay must establish coverage; a current snapshot cannot seed
 * unknown origins. The durable writer must CAS that same revision and commit
 * these images atomically with block/UTxO changes under the authority fence.
 */
export const stageHistoryProvenance = ({
  bindingDigest,
  block,
  transitions,
  incarnations,
}: {
  readonly bindingDigest: string;
  readonly block: Pick<BoundHistoryChainBlock, "point">;
  readonly transitions: readonly Readonly<{
    transactionIndex: number;
    transition: HistoryTransition;
  }>[];
  readonly incarnations: readonly HistoryIncarnation[];
}): readonly HistoryProvenanceChange[] => {
  const current = new Map<string, HistoryIncarnation>();
  const canonical = new Map<string, string>();
  const before = new Map<string, HistoryIncarnation | null>();
  const eventLabel = (kind: HistoryTransition["kind"], key: string) =>
    `${kind}:${key}`;
  for (const value of incarnations) {
    if (
      value.bindingDigest !== bindingDigest ||
      value.id !==
        historyIncarnationId(bindingDigest, value.kind, value.event) ||
      current.has(value.id)
    )
      fail("stored incarnation identity or binding disagrees");
    const frozen = freezeIncarnation(value);
    current.set(value.id, frozen);
    if (value.placement !== null) {
      if (
        (value.placement.current === null) ===
        (value.placement.retirement === null)
      )
        fail("canonical origin must be live or explicitly retired");
      const label = eventLabel(value.kind, value.event.key);
      if (canonical.has(label))
        fail("multiple canonical origins for one event");
      canonical.set(label, value.id);
    }
  }
  const put = (value: HistoryIncarnation) => {
    if (!before.has(value.id))
      before.set(value.id, current.get(value.id) ?? null);
    current.set(value.id, freezeIncarnation(value));
  };
  const live = (
    kind: HistoryTransition["kind"],
    key: string,
    location: OutRefLike,
  ) => {
    const id = canonical.get(eventLabel(kind, key));
    const value = id === undefined ? undefined : current.get(id);
    if (
      value?.placement?.current == null ||
      !sameRef(value.placement.current.outRef, location)
    )
      return fail("missing canonical origin or stale Order location");
    return value;
  };
  let lastIndex = -1;
  let lastHash: string | undefined;
  const seen = new Set<string>();
  const seenTransactions = new Set<string>();
  for (const { transactionIndex, transition } of transitions) {
    if (
      !Number.isSafeInteger(transactionIndex) ||
      transactionIndex < lastIndex ||
      transactionIndex < 0
    )
      fail("transition order is not a ledger transaction order");
    if (
      transactionIndex === lastIndex &&
      transition.transactionHash !== lastHash
    )
      fail("one transaction index names different transactions");
    if (transactionIndex !== lastIndex) {
      if (seenTransactions.has(transition.transactionHash))
        fail("transaction occurs at multiple indices");
      seenTransactions.add(transition.transactionHash);
    }
    const transitionLabel = `${transactionIndex}:${transition.kind}`;
    if (seen.has(transitionLabel))
      fail("duplicate kind transition for one transaction");
    seen.add(transitionLabel);
    lastIndex = transactionIndex;
    lastHash = transition.transactionHash;
    const at: Placement = Object.freeze({
      blockHash: block.point.id,
      slot: block.point.slot,
      height: block.point.height,
      transactionHash: transition.transactionHash,
      transactionIndex,
    });
    for (const continuation of transition.continuations) {
      const old = live(transition.kind, continuation.key, continuation.before);
      if (continuation.after.txHash !== transition.transactionHash)
        fail("continuation output belongs to another transaction");
      put({
        ...old,
        placement: {
          ...old.placement!,
          current: { outRef: ref(continuation.after), at },
        },
      });
    }
    if (transition.admission !== undefined) {
      const event = transition.admission;
      const label = eventLabel(transition.kind, event.key);
      if (canonical.has(label))
        fail("admission reuses a canonical event, including a retired origin");
      if (event.outRef.txHash !== transition.transactionHash)
        fail("admission output belongs to another transaction");
      const id = historyIncarnationId(bindingDigest, transition.kind, event);
      const old = current.get(id);
      if (
        old !== undefined &&
        (!sameFacts(old.event, event) ||
          !sameRef(old.event.outRef, event.outRef))
      )
        fail("re-included incarnation changes immutable admission facts");
      put({
        id,
        bindingDigest,
        kind: transition.kind,
        event,
        placement: {
          admission: at,
          current: { outRef: ref(event.outRef), at },
          retirement: null,
        },
      });
      canonical.set(label, id);
    }
    if (transition.retirement !== undefined) {
      const retirement = transition.retirement;
      const old = live(
        transition.kind,
        retirement.event.key,
        retirement.event.outRef,
      );
      if (!sameFacts(old.event, retirement.event))
        fail("retirement changes immutable admission facts");
      put({
        ...old,
        placement: {
          admission: old.placement!.admission,
          current: null,
          retirement: {
            at,
            outRef: ref(retirement.event.outRef),
            reason: retirement.reason,
            observerRedeemerIndex: retirement.observerRedeemerIndex,
            witnessCbor: retirement.witnessCbor,
          },
        },
      });
    }
  }
  return Object.freeze(
    [...before].map(([id, previous]) =>
      Object.freeze({ before: previous, after: current.get(id)! }),
    ),
  );
};

/** Reverse one committed block's images after the owner has fenced producers
 * and established that this block is the current canonical head. New admissions
 * remain as orphaned audit records; L2 classifications are deliberately absent.
 * Returns staged replacements only, so a late mismatch cannot partially repair.
 */
export const reverseHistoryProvenance = (
  changes: readonly HistoryProvenanceChange[],
  current: readonly HistoryIncarnation[],
): readonly HistoryIncarnation[] => {
  const byId = new Map(current.map((value) => [value.id, value]));
  if (byId.size !== current.length) fail("duplicate current incarnation");
  const seen = new Set<string>();
  return Object.freeze(
    changes.map(({ before, after }) => {
      const actual = byId.get(after.id);
      if (
        seen.has(after.id) ||
        actual === undefined ||
        historyIncarnationDigest(actual) !== historyIncarnationDigest(after)
      )
        fail("rollback poststate does not match its committed block image");
      seen.add(after.id);
      if (
        before !== null &&
        (before.id !== after.id ||
          before.bindingDigest !== after.bindingDigest ||
          before.kind !== after.kind ||
          !sameFacts(before.event, after.event) ||
          !sameRef(before.event.outRef, after.event.outRef))
      )
        fail("rollback image changes immutable incarnation identity");
      return freezeIncarnation(before ?? { ...after, placement: null });
    }),
  );
};
