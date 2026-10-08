import { createHash } from "node:crypto";

import type { PromiseCapacityPoint } from "../availability/promise-capacity-evidence.js";
import type { StateQueueHeaderRecord } from "../domain.js";
import type { StoreData } from "../store.committee-store.js";
import { jsonReplacer } from "../store.parse-stored-record-map.js";
import { finalityHeldHeaderHashes } from "./retention.js";
import {
  authenticatedHeaderEnd,
  type CommitteeRetirementBinding,
  type CommitteeRetirementFloor,
  makeRetirementFloor,
  parseRetirementFloor,
  retirementDigest,
  sameRetirementPoint,
} from "./retirement-model.js";

export const retirementStoreDigest = (data: StoreData): string =>
  retirementDigest(JSON.parse(JSON.stringify(data, jsonReplacer)) as unknown);
export const retirementStoreRecords = (data: StoreData): number =>
  Object.entries(data).reduce(
    (n, [key, value]) =>
      n +
      (value === undefined
        ? 0
        : ["deployment", "chainCursor", "retirementFloor"].includes(key)
          ? 1
          : Object.keys(value).length),
    0,
  );
export const assertRetirementResources = (data: StoreData): void => {
  const b = data.retirementFloor?.binding;
  if (
    b &&
    (retirementStoreRecords(data) > b.maximumRecords ||
      Buffer.byteLength(`${JSON.stringify(data, jsonReplacer, 2)}\n`) >
        b.maximumEncodedBytes)
  )
    throw new Error("Retained store exceeds the unchanged bounded profile");
};
export const assertRetirementBinding = (
  data: StoreData,
  b: CommitteeRetirementBinding,
): void => {
  const d = data.deployment,
    s = data.chainCursor;
  if (
    !d ||
    !s ||
    s.status !== "healthy" ||
    d.marker.manifestId !== b.deploymentFingerprint ||
    d.manifestSha256 !== b.manifestSha256 ||
    s.authoritySha256 !== b.sourceAuthoritySha256 ||
    createHash("sha256").update(d.manifestRaw).digest("hex") !==
      b.manifestSha256
  )
    throw new Error("Retirement deployment/source binding is unavailable");
  const manifest = JSON.parse(d.manifestRaw) as {
    da?: {
      committeeSignersHash?: unknown;
      transportProfile?: { retentionDays?: unknown };
    };
  };
  if (
    manifest.da?.committeeSignersHash !== b.committeeSignersHash ||
    manifest.da.transportProfile?.retentionDays !== b.retentionDays
  )
    throw new Error("Retirement differs from the signed manifest");
  if (
    data.retirementFloor &&
    retirementDigest(data.retirementFloor.binding) !== retirementDigest(b)
  )
    throw new Error("Retirement binding changed");
};
const byHeaderFamilies = [
  "daPayloads",
  "daSignatures",
  "daConflictEvidence",
  "daAttestationCandidates",
  "l1Submissions",
  "peerBroadcasts",
  "decisionOutbox",
  "promiseCapacityEvidence",
] as const;
/** Checks every changed row, including peer ingress and observations hidden in a source save. */
export const assertRetirementWrite = (
  before: StoreData,
  after: StoreData,
): void => {
  const floor = before.retirementFloor;
  if (!floor) return;
  if (floor.breach || after.retirementFloor?.digest !== floor.digest)
    throw new Error("Retirement floor is held or changed outside retirement");
  assertRetirementBinding(after, floor.binding);
  const check = (headerHash: string) => {
    const header = after.stateQueueHeaders[headerHash];
    if (
      !header ||
      header.deploymentFingerprint !== floor.binding.deploymentFingerprint ||
      authenticatedHeaderEnd(header) <= (floor.headerEndTimeMs ?? -1)
    )
      throw new Error("Cannot materialize a retired or unauthenticated header");
  };
  for (const [key, row] of Object.entries(after.stateQueueHeaders))
    if (
      retirementDigest(JSON.parse(JSON.stringify(row, jsonReplacer))) !==
      (before.stateQueueHeaders[key]
        ? retirementDigest(
            JSON.parse(
              JSON.stringify(before.stateQueueHeaders[key], jsonReplacer),
            ),
          )
        : "")
    )
      check(row.headerHash);
  for (const family of byHeaderFamilies)
    for (const [key, row] of Object.entries(after[family])) {
      if (
        JSON.stringify(row, jsonReplacer) ===
        JSON.stringify(before[family][key], jsonReplacer)
      )
        continue;
      check(row.headerHash);
      if (row.deploymentFingerprint !== floor.binding.deploymentFingerprint)
        throw new Error("Retirement write has a foreign deployment");
    }
  const prior = new Map(
    before.chainCursor?.observations.map((o) => [o.headerHash, o]),
  );
  for (const row of after.chainCursor?.observations ?? [])
    if (JSON.stringify(row) !== JSON.stringify(prior.get(row.headerHash)))
      check(row.headerHash);
  for (const [key, row] of Object.entries(after.peerHealth))
    if (
      JSON.stringify(row) !== JSON.stringify(before.peerHealth[key]) &&
      !floor.binding.peerIds.includes(row.peerId)
    )
      throw new Error("Peer health is outside signed membership");
  for (const [key, row] of Object.entries(after.peerNonces))
    if (
      JSON.stringify(row) !== JSON.stringify(before.peerNonces[key]) &&
      (row.deploymentFingerprint !== floor.binding.deploymentFingerprint ||
        !Number.isSafeInteger(row.timestampMs) ||
        row.timestampMs <= floor.nonceTimeFloorMs)
    )
      throw new Error("Nonce is below the durable freshness floor");
  assertRetirementResources(after);
};
export type RetirementVerifiedFacts = Readonly<{
  binding: CommitteeRetirementBinding;
  boundary: PromiseCapacityPoint;
  canonicalTimeMs: number;
  horizonPoint: PromiseCapacityPoint;
  horizonTimeMs: number;
  pinnedHeaderHashes: ReadonlySet<string>;
  /** Every retained financial attempt/workflow remains a pin until SDK retirement removes it. */
  financialHeaderHashes: ReadonlySet<string>;
  /** Actual native transaction body verification places these exact submissions at this C. */
  submittedTransactionPoints: ReadonlyMap<string, PromiseCapacityPoint>;
  /** Exact native selected-chain point and raw height for every old checkpoint. */
  canonicalPoints: ReadonlyMap<string, PromiseCapacityPoint>;
  /** Stored points the follower no longer retains (keys as `retirementPointKey`). */
  pointsBeyondRetention: ReadonlySet<string>;
  /**
   * Per header, the block on the selected chain carrying the transaction
   * that put its node in the queue, re-derived from the follower. A
   * signature is checked at it, never at its immutable stored point.
   */
  headerLandingPoints: ReadonlyMap<string, PromiseCapacityPoint>;
  /** Headers whose landing read reached a block the follower no longer retains. */
  landingsBeyondRetention: ReadonlySet<string>;
  /** Submissions whose landing read reached a block the follower no longer retains. */
  submissionsBeyondRetention: ReadonlySet<string>;
}>;

/**
 * Why retirement stopped at a header otherwise past its retention: evidence
 * the follower cannot give. The header keeps its record; every later one
 * waits with it. A hold is degraded detail (`retention.holds` on `/readyz`),
 * never a readiness failure, since no rule releases it.
 */
export type RetirementHoldCause =
  | "submission_without_valid_landing" // its submission never validly landed
  | "signature_point_not_canonical" // its signatures' landing left the chain
  | "point_beyond_retention" // a point it needs is below the retained window
  | "point_not_canonical"; // a stored point names a block the chain abandoned

export type RetirementHold = Readonly<{
  headerHash: string;
  cause: RetirementHoldCause;
  detail: string;
}>;

/** A hold as its named retention reason: the header and its cause. */
export const COMMITTEE_RETIREMENT_HELD = "committee_retirement_held";

export const retirementHoldReason = (hold: RetirementHold): string =>
  `${COMMITTEE_RETIREMENT_HELD}: header ${hold.headerHash}: ${hold.cause}: ${hold.detail}`;
export const retirementPointKey = (
  p: Pick<PromiseCapacityPoint, "slot" | "blockHash">,
): string => `${p.slot}:${p.blockHash}`;
type StoredPoint = { slot?: number; blockHash?: string; blockHeight?: number };
/**
 * The verified point for a stored point, or why the follower cannot give it
 * (beyond its retention, or not on its chain). A point read at a different
 * height, or above the boundary, is never a hold: it throws.
 */
const lookupPoint = (
  point: StoredPoint,
  facts: RetirementVerifiedFacts,
):
  | Readonly<{ kind: "ok"; point: PromiseCapacityPoint }>
  | Readonly<{
      kind: "point_beyond_retention" | "point_not_canonical";
      key: string;
    }> => {
  if (point.slot === undefined || point.blockHash === undefined)
    throw new Error("Retirement checkpoint is unavailable");
  const key = retirementPointKey({
    slot: point.slot,
    blockHash: point.blockHash,
  });
  const p = facts.canonicalPoints.get(key);
  if (!p)
    return facts.pointsBeyondRetention.has(key)
      ? { kind: "point_beyond_retention", key }
      : { kind: "point_not_canonical", key };
  if (
    (point.blockHeight !== undefined && p.blockNo !== point.blockHeight) ||
    p.blockNo > facts.boundary.blockNo
  )
    throw new Error("Retirement checkpoint lacks exact same-boundary ancestry");
  return { kind: "ok", point: p };
};
const requirePoint = (
  point: StoredPoint,
  facts: RetirementVerifiedFacts,
): PromiseCapacityPoint => {
  const found = lookupPoint(point, facts);
  if (found.kind !== "ok")
    throw new Error("Retirement checkpoint lacks exact same-boundary ancestry");
  return found.point;
};
export type CommitteeRetirementPlan = Readonly<{
  floor: CommitteeRetirementFloor;
  headerHashes: readonly string[];
}>;
/**
 * The retirement plan, or none; and the hold that stopped it at a header
 * already past its retention, when the follower could not give that
 * header's evidence. A header still pinned, unfinalized, inside its
 * retention or shallower than recovery depth is waited on, not held.
 */
export const planRetirement = (
  data: StoreData,
  facts: RetirementVerifiedFacts,
  localPins: ReadonlySet<string>,
  inFlight: (effectId: string) => boolean,
): Readonly<{ plan?: CommitteeRetirementPlan; hold?: RetirementHold }> => {
  assertRetirementBinding(data, facts.binding);
  if (data.retirementFloor?.breach)
    throw new Error("Retirement floor was breached");
  const pins = new Set([
    ...facts.pinnedHeaderHashes,
    ...facts.financialHeaderHashes,
    ...localPins,
    ...finalityHeldHeaderHashes([], Object.values(data.stateQueueHeaders)),
  ]);
  const groups = new Map<number, StateQueueHeaderRecord[]>();
  for (const header of Object.values(data.stateQueueHeaders)) {
    const end = authenticatedHeaderEnd(header);
    const group = groups.get(end) ?? [];
    group.push(header);
    groups.set(end, group);
  }
  const retired: string[] = [];
  const points: PromiseCapacityPoint[] = [];
  let hold: RetirementHold | undefined;
  let end = data.retirementFloor?.headerEndTimeMs ?? -1;
  for (const [time, headers] of [...groups].sort(([a], [b]) => a - b)) {
    if (time <= end) throw new Error("Retired header was reintroduced");
    let eligible = true;
    const cohortPoints: PromiseCapacityPoint[] = [];
    for (const header of headers) {
      const h = header.headerHash;
      // A hold names this header only when nothing else keeps it: a
      // header still waiting (unfinalized, in flight, shallow) is not held.
      let headerHold: RetirementHold | undefined;
      let waiting = false;
      const holdOn = (cause: RetirementHoldCause, detail: string): void => {
        headerHold ??= { headerHash: h, cause, detail };
      };
      const point = (stored: StoredPoint, what: string): void => {
        const found = lookupPoint(stored, facts);
        if (found.kind === "ok") cohortPoints.push(found.point);
        else holdOn(found.kind, `${what} at ${found.key}`);
      };
      if (
        pins.has(h) ||
        header.deploymentFingerprint !== facts.binding.deploymentFingerprint ||
        !header.finalized ||
        !["merged", "removed"].includes(header.status) ||
        header.observedChainPoint.providerSource !==
          "authenticated_state_queue_transition_v1" ||
        facts.horizonTimeMs <= time + facts.binding.retentionDays * 86400000
      ) {
        eligible = false;
        break;
      }
      point(header.observedChainPoint, "its observed chain point");
      for (const observation of data.chainCursor?.observations.filter(
        (o) => o.headerHash === h,
      ) ?? []) {
        if (
          !observation.finalized ||
          !["merged", "removed"].includes(observation.stateQueueStatus)
        ) {
          waiting = true;
          break;
        }
        point(observation, "its L1 source observation");
      }
      // A signature's stored point is immutable; the header's landing on
      // the selected chain is read again from the follower instead.
      if (Object.values(data.daSignatures).some((s) => s.headerHash === h)) {
        const landing = facts.headerLandingPoints.get(h);
        if (landing) cohortPoints.push(landing);
        else if (facts.landingsBeyondRetention.has(h))
          holdOn(
            "point_beyond_retention",
            "its signatures' landing block is no longer retained",
          );
        else
          holdOn(
            "signature_point_not_canonical",
            "the follower's chain carries no landing of it",
          );
      }
      for (const capacity of Object.values(data.promiseCapacityEvidence).filter(
        (c) => c.headerHash === h,
      )) {
        if (capacity.floorViolatedAt)
          throw new Error("Capacity floor was breached");
        if (
          !capacity.certifiedAt ||
          capacity.recoveryDepth !== facts.binding.recoveryDepth ||
          capacity.contractManifestId !== facts.binding.contractManifestId ||
          facts.canonicalTimeMs < capacity.cutoffTimeMs
        ) {
          waiting = true;
          break;
        }
        point(
          { ...capacity.point, blockHeight: capacity.point.blockNo },
          "its capacity evidence point",
        );
        point(
          {
            ...capacity.certifiedAt,
            blockHeight: capacity.certifiedAt.blockNo,
          },
          "its capacity certification point",
        );
      }
      for (const effect of Object.values(data.decisionOutbox).filter(
        (e) => e.headerHash === h,
      )) {
        if (
          inFlight(effect.effectId) ||
          (effect.effectKind === "l1_reconcile" &&
            effect.status !== "reconciled")
        ) {
          waiting = true;
          break;
        }
        point(effect, `its decision effect ${effect.effectId}`);
      }
      for (const submission of Object.values(data.l1Submissions).filter(
        (s) => s.headerHash === h,
      )) {
        const p = facts.submittedTransactionPoints.get(submission.txHash);
        if (p) cohortPoints.push(p);
        else if (facts.submissionsBeyondRetention.has(submission.txHash))
          holdOn(
            "point_beyond_retention",
            `its submission ${submission.txHash} landed in a block no longer retained`,
          );
        else
          holdOn(
            "submission_without_valid_landing",
            `its submission ${submission.txHash} has no valid landing on the follower's chain`,
          );
      }
      if (
        cohortPoints.some(
          (p) =>
            facts.boundary.blockNo - p.blockNo <= facts.binding.recoveryDepth,
        )
      )
        waiting = true;
      if (headerHold !== undefined && !waiting) hold ??= headerHold;
      if (waiting || headerHold !== undefined) {
        eligible = false;
        break;
      }
    }
    if (!eligible) break;
    retired.push(...headers.map((h) => h.headerHash));
    points.push(...cohortPoints);
    end = time;
  }
  const held = hold === undefined ? {} : { hold };
  if (!retired.length) return held;
  if (data.retirementFloor?.point)
    points.push(
      requirePoint(
        {
          ...data.retirementFloor.point,
          blockHeight: data.retirementFloor.point.blockNo,
        },
        facts,
      ),
    );
  const p = facts.horizonPoint;
  if (
    facts.boundary.blockNo - p.blockNo <= facts.binding.recoveryDepth ||
    points.some((q) => q.blockNo > p.blockNo || q.slot > p.slot)
  )
    throw new Error(
      "Retirement horizon point does not cover every removed checkpoint",
    );
  if (points.some((q) => q.blockNo === p.blockNo && !sameRetirementPoint(q, p)))
    throw new Error("Retirement points disagree at the same native height");
  return {
    ...held,
    plan: {
      headerHashes: Object.freeze(retired.sort()),
      floor: makeRetirementFloor({
        schemaVersion: 1,
        binding: facts.binding,
        generation: (data.retirementFloor?.generation ?? 0) + 1,
        headerEndTimeMs: end,
        point: p,
        certifiedAt: facts.boundary,
        nonceTimeFloorMs: Math.max(
          data.retirementFloor?.nonceTimeFloorMs ?? 0,
          facts.horizonTimeMs - 300000,
        ),
      }),
    },
  };
};
export const applyRetirementPlan = (
  data: StoreData,
  plan: CommitteeRetirementPlan,
): StoreData => {
  const deleted = new Set(plan.headerHashes);
  const keep = <T extends { headerHash: string }>(
    rows: Record<string, T>,
  ): Record<string, T> =>
    Object.fromEntries(
      Object.entries(rows).filter(([, r]) => !deleted.has(r.headerHash)),
    );
  const next: StoreData = {
    ...data,
    retirementFloor: parseRetirementFloor(plan.floor),
    stateQueueHeaders: keep(data.stateQueueHeaders),
    daPayloads: keep(data.daPayloads),
    daSignatures: keep(data.daSignatures),
    daConflictEvidence: keep(data.daConflictEvidence),
    daAttestationCandidates: keep(data.daAttestationCandidates),
    l1Submissions: keep(data.l1Submissions),
    peerBroadcasts: keep(data.peerBroadcasts),
    decisionOutbox: keep(data.decisionOutbox),
    promiseCapacityEvidence: keep(data.promiseCapacityEvidence),
    peerHealth: Object.fromEntries(
      Object.entries(data.peerHealth).filter(([, r]) =>
        plan.floor.binding.peerIds.includes(r.peerId),
      ),
    ),
    peerNonces: Object.fromEntries(
      Object.entries(data.peerNonces).filter(
        ([, r]) => r.timestampMs > plan.floor.nonceTimeFloorMs,
      ),
    ),
    chainCursor:
      data.chainCursor === undefined
        ? undefined
        : {
            ...data.chainCursor,
            observations: data.chainCursor.observations.filter(
              (r) => !deleted.has(r.headerHash),
            ),
          },
  };
  assertRetirementResources(next);
  return next;
};
