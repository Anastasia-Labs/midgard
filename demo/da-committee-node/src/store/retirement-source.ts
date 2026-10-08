import type {
  AvailabilityOperationActorSnapshot,
  AvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { PromiseCapacityPoint } from "../availability/promise-capacity-evidence.js";
import type { StateQueueHeaderRecord } from "../domain.js";
import type { CommitteeStore, StoreData } from "../store.committee-store.js";
import { mintRetirementCertificate } from "./retirement-certificate.js";
import {
  authenticatedHeaderEnd,
  type CommitteeRetirementBinding,
  type CommitteeRetirementFloor,
  type CommitteeRetirementGuard,
  type CommitteeRetirementPort,
  makeRetirementFloor,
  parseRetirementBinding,
  parseRetirementPoint,
  retirementDigest,
  sameRetirementPoint,
} from "./retirement-model.js";
import {
  assertRetirementBinding,
  planRetirement,
  retirementPointKey,
  type RetirementVerifiedFacts,
} from "./retirement-transition.js";

export type RetirementPointRequest = Readonly<{
  slot: number;
  blockHash: string;
  blockNo?: number;
}>;
export type RetirementPointProof = Readonly<{
  point: PromiseCapacityPoint;
  tip: PromiseCapacityPoint;
}>;
export type CommitteeRetirementSourceDependencies = Readonly<{
  binding: CommitteeRetirementBinding;
  deployment: SDK.DaAvailabilityDeployment;
  store: CommitteeStore;
  journal: AvailabilityOperationJournal;
  readBoundary: (
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<PromiseCapacityPoint>;
  /** Verified native genesis/era-history conversion, never a host clock. */
  slotTimeMs: (slot: number) => number;
  /** Complete script-address reads from the follower's facts at the current boundary. */
  readRawSnapshot: (
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<SDK.DaAvailabilitySnapshotUtxos>;
  readCanonicalPoint: (
    point: RetirementPointRequest,
    boundary: PromiseCapacityPoint,
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<RetirementPointProof | null>;
  /** The block a valid stored transaction `txHash` landed in, on the follower's chain. */
  readSubmissionPoint: (
    txHash: string,
    boundary: PromiseCapacityPoint,
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<RetirementPointProof | null>;
  /** Includes deferred/non-final scan work and callbacks not represented by rows. */
  readOperationalPins: (
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<readonly string[]>;
  assertClaimsCurrent: (
    boundary: PromiseCapacityPoint,
    scope: SDK.DaAvailabilityReadScope,
    actor: AvailabilityOperationActorSnapshot,
  ) => Promise<void>;
  assertCurrent: (scope: SDK.DaAvailabilityReadScope) => Promise<void>;
}>;
const nativeTime = (
  args: CommitteeRetirementSourceDependencies,
  p: PromiseCapacityPoint,
): number => {
  const time = args.slotTimeMs(p.slot);
  if (!Number.isSafeInteger(time) || time < 0)
    throw new Error("Retirement clock lacks native slot authority");
  return time;
};
const verifiedProof = (
  proof: RetirementPointProof | null,
  p: RetirementPointRequest,
  c: PromiseCapacityPoint,
): PromiseCapacityPoint => {
  if (
    !proof ||
    proof.point.slot !== p.slot ||
    proof.point.blockHash !== p.blockHash ||
    (p.blockNo !== undefined && proof.point.blockNo !== p.blockNo) ||
    !sameRetirementPoint(proof.tip, c) ||
    proof.point.blockNo > c.blockNo
  )
    throw new Error(
      "Retirement point lacks exact raw height at the same selected boundary",
    );
  return parseRetirementPoint(proof.point);
};
const rawPins = async (
  deployment: SDK.DaAvailabilityDeployment,
  raw: SDK.DaAvailabilitySnapshotUtxos,
  headers: readonly StateQueueHeaderRecord[],
): Promise<ReadonlySet<string>> => {
  const root = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
    deployment,
    SDK.makeGenesisConfirmedState(0n).headerHash,
    raw,
  );
  const confirmed = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(root.confirmedState.datum),
  );
  if (
    !root.correctionLock.datum ||
    Data.from(root.correctionLock.datum, SDK.CorrectionLockDatum) !== "Idle"
  )
    throw new Error("Retirement source is under correction");
  const pins = new Set<string>([confirmed.data.headerHash]);
  // Authenticate every retained target against the full raw snapshot. A filtered
  // challenge list cannot prove absence, including stranded record tokens.
  const prefix =
    deployment.contracts.availabilityChallenge.policyId +
    SDK.DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX;
  for (const u of raw.availabilityUtxos)
    for (const [unit, amount] of Object.entries(u.assets))
      if (unit.startsWith(prefix) && unit.length === prefix.length + 56) {
        if (
          amount !== 1n ||
          u.address !==
            deployment.contracts.availabilityChallenge.spendingScriptAddress ||
          !u.datum
        )
          throw new Error("Retirement raw challenge identity is malformed");
        const record = SDK.parseDaAvailabilityChallengeRecordCbor(
          u.datum,
          deployment.parameters,
        );
        if (
          unit !==
            deployment.contracts.availabilityChallenge.policyId +
              record.challenge_asset_name ||
          record.commitment.deployment_identity !== deployment.hubOraclePolicyId
        )
          throw new Error("Retirement raw challenge binding differs");
        pins.add(record.commitment.header_hash);
      }
  for (const header of headers) {
    const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
      deployment,
      header.headerHash,
      raw,
    );
    if (snapshot.queue || snapshot.record) pins.add(header.headerHash);
  }
  return pins;
};
const pointRequests = (data: StoreData): readonly RetirementPointRequest[] => {
  const points = new Map<string, RetirementPointRequest>();
  const add = (p: {
    slot?: number;
    blockHash?: string;
    blockHeight?: number;
    blockNo?: number;
  }) => {
    if (p.slot === undefined || p.blockHash === undefined) return;
    const q = {
      slot: p.slot,
      blockHash: p.blockHash,
      ...((p.blockNo ?? p.blockHeight) !== undefined
        ? { blockNo: p.blockNo ?? p.blockHeight }
        : {}),
    };
    const key = retirementPointKey(q),
      prior = points.get(key);
    if (
      prior?.blockNo !== undefined &&
      q.blockNo !== undefined &&
      prior.blockNo !== q.blockNo
    )
      throw new Error("Retirement checkpoint has conflicting raw heights");
    points.set(key, prior?.blockNo !== undefined ? prior : q);
  };
  for (const h of Object.values(data.stateQueueHeaders))
    add(h.observedChainPoint);
  for (const s of Object.values(data.daSignatures)) add(s.l1ChainPoint);
  for (const e of Object.values(data.decisionOutbox)) add(e);
  for (const c of Object.values(data.promiseCapacityEvidence)) {
    add(c.point);
    if (c.certifiedAt) add(c.certifiedAt);
  }
  for (const o of data.chainCursor?.observations ?? []) add(o);
  if (data.retirementFloor?.point) add(data.retirementFloor.point);
  return [...points.values()];
};

/** No adoption by omission. The runtime supplies the same owned SDK scope and
 * real reconciliation receipt; missing native/source capabilities retain data. */
export const committeeRetirementSource = (
  args: CommitteeRetirementSourceDependencies,
): CommitteeRetirementPort &
  Readonly<{
    compact: (scope: SDK.DaAvailabilityReadScope) => Promise<readonly string[]>;
  }> => {
  const binding = parseRetirementBinding(args.binding);
  const captures = new WeakMap<
    object,
    Readonly<{
      floor: CommitteeRetirementFloor;
      boundary: PromiseCapacityPoint;
    }>
  >();
  const boundary = async (scope: SDK.DaAvailabilityReadScope) =>
    parseRetirementPoint(await args.readBoundary(scope));
  const current = async (
    c: PromiseCapacityPoint,
    scope: SDK.DaAvailabilityReadScope,
  ) => {
    await args.assertCurrent(scope);
    const after = await boundary(scope);
    scope.assertCurrent();
    if (!sameRetirementPoint(c, after))
      throw new Error("Retirement canonical boundary changed");
  };
  const proveFloor = async (
    floor: CommitteeRetirementFloor,
    c: PromiseCapacityPoint,
    scope: SDK.DaAvailabilityReadScope,
  ) => {
    if (
      retirementDigest(floor.binding) !== retirementDigest(binding) ||
      floor.breach
    )
      throw new Error("Retirement floor binding differs or is breached");
    if (!floor.point) return;
    const proof = await args.readCanonicalPoint(floor.point, c, scope);
    if (proof === null) {
      await current(c, scope);
      await args.store.recordRetirementBreach(
        "selected_chain_crossed_retirement_floor",
        c,
      );
      throw new Error("Canonical chain crossed the permanent retirement floor");
    }
    verifiedProof(proof, floor.point, c);
    await current(c, scope);
  };
  const capture = async (
    scope: SDK.DaAvailabilityReadScope,
  ): Promise<CommitteeRetirementGuard> => {
    const token = args.store.captureRetirementGuard();
    const floor = await args.store.getRetirementFloor();
    if (!floor)
      throw new Error(
        "Retirement singleton must be initialized before new promises",
      );
    const c = await boundary(scope);
    await proveFloor(floor, c, scope);
    args.store.assertRetirementGuard(token);
    captures.set(token, { floor, boundary: c });
    return token;
  };
  return {
    capture,
    prove: async (token, scope) => {
      const capture = captures.get(token);
      if (!capture) throw new Error("Retirement capture is unavailable");
      const c = await boundary(scope);
      await proveFloor(capture.floor, c, scope);
      args.store.assertRetirementGuard(token);
    },
    assert: (token, record) => args.store.assertRetirementGuard(token, record),
    compact: async (scope) => {
      scope.assertCurrent();
      if (args.store.retirementDiscoveryActive()) return [];
      const snapshot = await args.store.readRetirementSnapshot(),
        data = snapshot.data;
      assertRetirementBinding(data, binding);
      const c = await boundary(scope),
        time = nativeTime(args, c);
      if (data.retirementFloor)
        await proveFloor(data.retirementFloor, c, scope);
      const actor = args.journal.actorSnapshot(
        binding.actorId,
        binding.contractManifestId,
      );
      await args.assertClaimsCurrent(c, scope, actor);
      const assertCurrent = async () => {
        await current(c, scope);
        if (
          args.journal.actorSnapshot(
            binding.actorId,
            binding.contractManifestId,
          ).stateDigest !== actor.stateDigest
        )
          throw new Error("Retirement financial journal changed");
        await args.assertClaimsCurrent(c, scope, actor);
      };
      const prior = data.retirementFloor;
      if (!prior) {
        const floor = makeRetirementFloor({
          schemaVersion: 1,
          binding,
          generation: 1,
          nonceTimeFloorMs: 0,
        });
        return args.store.applyRetirementCertificate(
          mintRetirementCertificate({
            snapshot,
            plan: { floor, headerHashes: [] },
            assertCurrent,
            assertScopeCurrent: scope.assertCurrent,
          }),
        );
      }
      const headers = Object.values(data.stateQueueHeaders);
      const canExpire = headers.some(
        (h) =>
          authenticatedHeaderEnd(h) + binding.retentionDays * 86400000 < time,
      );
      if (!canExpire) {
        await assertCurrent();
        return [];
      }
      let checkpoint = prior.checkpoint;
      if (checkpoint) {
        const proof = await args.readCanonicalPoint(checkpoint.point, c, scope);
        if (proof === null) {
          await assertCurrent();
          checkpoint = undefined;
        } else {
          verifiedProof(proof, checkpoint.point, c);
          if (nativeTime(args, checkpoint.point) !== checkpoint.canonicalTimeMs)
            throw new Error("Provisional expiry point clock changed");
        }
      }
      if (!checkpoint) {
        const { digest: _digest, ...base } = prior;
        const floor = makeRetirementFloor({
          ...base,
          generation: prior.generation + 1,
          checkpoint: { point: c, canonicalTimeMs: time },
        });
        return args.store.applyRetirementCertificate(
          mintRetirementCertificate({
            snapshot,
            plan: { floor, headerHashes: [] },
            assertCurrent,
            assertScopeCurrent: scope.assertCurrent,
          }),
        );
      }
      if (c.blockNo - checkpoint.point.blockNo <= binding.recoveryDepth) {
        await assertCurrent();
        return [];
      }
      const raw = await args.readRawSnapshot(scope);
      const pins = new Set(await rawPins(args.deployment, raw, headers));
      for (const h of await args.readOperationalPins(scope)) pins.add(h);
      const financial = new Set(
        actor.retainedAttempts.map((a) => a.headerHash),
      );
      for (const w of args.journal.workflows(binding.actorId))
        financial.add(w.headerHash);
      for (const w of args.journal.unsettledReleases(binding.actorId))
        financial.add(w.headerHash);
      // A different actor's opaque intent cannot be treated as zero interference.
      if (args.journal.retainedRecordCount() !== actor.retainedAttempts.length)
        for (const h of headers) financial.add(h.headerHash);
      const canonicalPoints = new Map<string, PromiseCapacityPoint>();
      for (const q of pointRequests(data)) {
        const proof = await args.readCanonicalPoint(q, c, scope);
        if (proof !== null)
          canonicalPoints.set(
            retirementPointKey(q),
            verifiedProof(proof, q, c),
          );
      }
      const transactions = new Map<string, PromiseCapacityPoint>();
      for (const s of Object.values(data.l1Submissions)) {
        const proof = await args.readSubmissionPoint(s.txHash, c, scope);
        if (proof)
          transactions.set(s.txHash, verifiedProof(proof, proof.point, c));
      }
      const facts: RetirementVerifiedFacts = {
        binding,
        boundary: c,
        canonicalTimeMs: time,
        horizonPoint: checkpoint.point,
        horizonTimeMs: checkpoint.canonicalTimeMs,
        pinnedHeaderHashes: pins,
        financialHeaderHashes: financial,
        canonicalPoints,
        submittedTransactionPoints: transactions,
      };
      // The backend independently checks current callback pins under its write lock.
      const plan = planRetirement(data, facts, new Set(), () => false);
      await assertCurrent();
      args.store.assertRetirementGuard(snapshot.guard);
      if (!plan) return [];
      return args.store.applyRetirementCertificate(
        mintRetirementCertificate({
          snapshot,
          plan,
          assertCurrent,
          assertScopeCurrent: scope.assertCurrent,
        }),
      );
    },
  };
};
