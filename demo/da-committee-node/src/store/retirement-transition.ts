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
  if (after.chainCursor?.status === "quarantined") {
    if (before.chainCursor?.status !== "healthy")
      throw new Error("Retirement source remains quarantined");
    for (const family of [
      "stateQueueHeaders",
      ...byHeaderFamilies,
      "peerHealth",
      "peerNonces",
    ] as const)
      if (
        Object.keys(after[family]).some((k) => before[family][k] === undefined)
      )
        throw new Error("Quarantine cannot introduce a new retained liability");
    if (
      after.chainCursor.authoritySha256 !==
        floor.binding.sourceAuthoritySha256 ||
      retirementDigest(after.deployment) !== retirementDigest(before.deployment)
    )
      throw new Error("Quarantine changed retirement binding");
    assertRetirementResources(after);
    return;
  }
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
}>;
export const retirementPointKey = (
  p: Pick<PromiseCapacityPoint, "slot" | "blockHash">,
): string => `${p.slot}:${p.blockHash}`;
const requirePoint = (
  point: { slot?: number; blockHash?: string; blockHeight?: number },
  facts: RetirementVerifiedFacts,
): PromiseCapacityPoint => {
  if (point.slot === undefined || point.blockHash === undefined)
    throw new Error("Retirement checkpoint is unavailable");
  const p = facts.canonicalPoints.get(
    retirementPointKey({ slot: point.slot, blockHash: point.blockHash }),
  );
  if (
    !p ||
    (point.blockHeight !== undefined && p.blockNo !== point.blockHeight) ||
    p.blockNo > facts.boundary.blockNo
  )
    throw new Error("Retirement checkpoint lacks exact same-boundary ancestry");
  return p;
};
export type CommitteeRetirementPlan = Readonly<{
  floor: CommitteeRetirementFloor;
  headerHashes: readonly string[];
}>;
export const planRetirement = (
  data: StoreData,
  facts: RetirementVerifiedFacts,
  localPins: ReadonlySet<string>,
  inFlight: (effectId: string) => boolean,
): CommitteeRetirementPlan | undefined => {
  assertRetirementBinding(data, facts.binding);
  if (data.retirementFloor?.breach)
    throw new Error("Retirement floor was breached");
  const pins = new Set([
    ...facts.pinnedHeaderHashes,
    ...facts.financialHeaderHashes,
    ...localPins,
    ...finalityHeldHeaderHashes([], Object.values(data.stateQueueHeaders)),
  ]);
  for (const q of data.chainCursor?.stateQueueReplayAnchor?.queue ?? [])
    if (q.headerHash !== null) pins.add(q.headerHash);
  const groups = new Map<number, StateQueueHeaderRecord[]>();
  for (const header of Object.values(data.stateQueueHeaders)) {
    const end = authenticatedHeaderEnd(header);
    const group = groups.get(end) ?? [];
    group.push(header);
    groups.set(end, group);
  }
  const retired: string[] = [];
  const points: PromiseCapacityPoint[] = [];
  let end = data.retirementFloor?.headerEndTimeMs ?? -1;
  for (const [time, headers] of [...groups].sort(([a], [b]) => a - b)) {
    if (time <= end) throw new Error("Retired header was reintroduced");
    let eligible = true;
    const cohortPoints: PromiseCapacityPoint[] = [];
    for (const header of headers) {
      const h = header.headerHash;
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
      cohortPoints.push(requirePoint(header.observedChainPoint, facts));
      for (const observation of data.chainCursor?.observations.filter(
        (o) => o.headerHash === h,
      ) ?? []) {
        if (
          !observation.finalized ||
          !["merged", "removed"].includes(observation.stateQueueStatus)
        ) {
          eligible = false;
          break;
        }
        cohortPoints.push(requirePoint(observation, facts));
        for (const step of observation.authenticatedSteps ?? [])
          cohortPoints.push(requirePoint(step, facts));
      }
      for (const signature of Object.values(data.daSignatures).filter(
        (s) => s.headerHash === h,
      ))
        cohortPoints.push(requirePoint(signature.l1ChainPoint, facts));
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
          eligible = false;
          break;
        }
        cohortPoints.push(
          requirePoint(
            { ...capacity.point, blockHeight: capacity.point.blockNo },
            facts,
          ),
        );
        cohortPoints.push(
          requirePoint(
            {
              ...capacity.certifiedAt,
              blockHeight: capacity.certifiedAt.blockNo,
            },
            facts,
          ),
        );
      }
      for (const effect of Object.values(data.decisionOutbox).filter(
        (e) => e.headerHash === h,
      )) {
        if (
          inFlight(effect.effectId) ||
          effect.quarantineReason ||
          (effect.effectKind === "l1_reconcile" &&
            effect.status !== "reconciled")
        ) {
          eligible = false;
          break;
        }
        cohortPoints.push(requirePoint(effect, facts));
      }
      for (const submission of Object.values(data.l1Submissions).filter(
        (s) => s.headerHash === h,
      )) {
        const p = facts.submittedTransactionPoints.get(submission.txHash);
        if (!p) {
          eligible = false;
          break;
        }
        cohortPoints.push(p);
      }
      if (
        cohortPoints.some(
          (p) =>
            facts.boundary.blockNo - p.blockNo <= facts.binding.recoveryDepth,
        )
      )
        eligible = false;
      if (!eligible) break;
    }
    if (!eligible) break;
    retired.push(...headers.map((h) => h.headerHash));
    points.push(...cohortPoints);
    end = time;
  }
  if (!retired.length) return undefined;
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
