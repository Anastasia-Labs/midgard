import { depth, isFinal } from "@al-ft/midgard-l1-follower";

import type { CommitteeStore } from "../store.js";
import {
  type PromiseCapacityEvidence,
  promiseCapacityEvidenceKey,
  type PromiseCapacityPoint,
  promiseCapacityPointId,
} from "./promise-capacity-evidence.js";

export type PromiseCapacityLiability = Readonly<{
  headerHash: string;
  commitmentDigest: string;
  cutoffTimeMs: number;
  hasActiveChallenge?: boolean;
  /** Supplied only after exact authenticated k-safe terminal history validation. */ terminalPoint?: PromiseCapacityPoint;
}>;
/**
 * The point on the follower's chain, read under the boundary, with the
 * boundary's tip; null when the point is not on that chain.
 */
export type PromiseCanonicalPointReader = (
  point: PromiseCapacityPoint,
) => Promise<Readonly<{
  point: PromiseCapacityPoint;
  tip: PromiseCapacityPoint;
}> | null>;

export const retiredPromiseCutoffs = async (
  args: Readonly<{
    store: Pick<
      CommitteeStore,
      "getPromiseCapacityEvidence" | "savePromiseCapacityEvidence"
    >;
    deploymentFingerprint: string;
    contractManifestId: string;
    actorId: string;
    recoveryDepth: number;
    boundary: PromiseCapacityPoint;
    canonicalTimeMs: number;
    slotTimeMs: (slot: number) => number;
    liabilities: readonly PromiseCapacityLiability[];
    readCanonicalPoint: PromiseCanonicalPointReader;
    assertCurrent: () => Promise<void>;
  }>,
): Promise<ReadonlySet<string>> => {
  if (
    !Number.isSafeInteger(args.canonicalTimeMs) ||
    args.canonicalTimeMs < 0 ||
    args.slotTimeMs(args.boundary.slot) !== args.canonicalTimeMs
  )
    throw new Error("Cutoff clock is not bound to its selected-chain slot");
  const retired = new Set<string>();
  const currentId = promiseCapacityPointId(args.boundary);
  const verify = (
    proof: Readonly<{
      point: PromiseCapacityPoint;
      tip: PromiseCapacityPoint;
    }> | null,
    point: PromiseCapacityPoint,
  ) => {
    if (
      proof === null ||
      promiseCapacityPointId(proof.point) !== promiseCapacityPointId(point) ||
      proof.point.blockNo !== point.blockNo ||
      promiseCapacityPointId(proof.tip) !== currentId ||
      proof.tip.blockNo !== args.boundary.blockNo ||
      proof.tip.blockNo < point.blockNo
    )
      throw new Error(
        "Capacity ancestry proof changed its point, height or read boundary",
      );
    return proof;
  };
  for (const liability of args.liabilities) {
    const identity = {
      deploymentFingerprint: args.deploymentFingerprint,
      contractManifestId: args.contractManifestId,
      actorId: args.actorId,
      headerHash: liability.headerHash,
      commitmentDigest: liability.commitmentDigest,
      cutoffTimeMs: liability.cutoffTimeMs,
      recoveryDepth: args.recoveryDepth,
    };
    const key = promiseCapacityEvidenceKey(identity);
    let evidence = await args.store.getPromiseCapacityEvidence(key);
    if (
      evidence !== undefined &&
      (evidence.headerHash !== liability.headerHash ||
        evidence.cutoffTimeMs !== liability.cutoffTimeMs ||
        evidence.recoveryDepth !== args.recoveryDepth)
    )
      throw new Error(
        "Persisted capacity evidence no longer matches the promise",
      );
    if (evidence?.floorViolatedAt !== undefined)
      throw new Error(
        "Protected promise capacity rollback floor was previously crossed",
      );
    if (evidence === undefined) {
      if (liability.hasActiveChallenge) continue;
      const terminal = liability.terminalPoint;
      if (
        terminal === undefined &&
        args.canonicalTimeMs < liability.cutoffTimeMs
      )
        continue;
      const selected = verify(
        await args.readCanonicalPoint(terminal ?? args.boundary),
        terminal ?? args.boundary,
      );
      await args.assertCurrent();
      evidence = await args.store.savePromiseCapacityEvidence({
        ...identity,
        retirementKind: terminal === undefined ? "open_cutoff" : "terminal",
        point: selected.point,
      });
    }
    if (
      evidence.retirementKind === "open_cutoff" &&
      args.slotTimeMs(evidence.point.slot) < liability.cutoffTimeMs
    )
      throw new Error(
        "Recorded cutoff point precedes the authenticated Open deadline",
      );
    const result = await args.readCanonicalPoint(evidence.point);
    if (result === null) {
      await args.assertCurrent();
      if (evidence.certifiedAt !== undefined) {
        await args.store.savePromiseCapacityEvidence(
          { ...evidence, floorViolatedAt: args.boundary },
          promiseCapacityPointId(evidence.point),
        );
        throw new Error(
          "Canonical chain crossed the protected promise capacity rollback floor",
        );
      }
      // Reset only an unretired, owned observation after authoritative rollback.
      if (
        !liability.hasActiveChallenge &&
        args.canonicalTimeMs >= liability.cutoffTimeMs
      ) {
        const selected = verify(
          await args.readCanonicalPoint(args.boundary),
          args.boundary,
        );
        await args.assertCurrent();
        await args.store.savePromiseCapacityEvidence(
          { ...identity, retirementKind: "open_cutoff", point: selected.point },
          promiseCapacityPointId(evidence.point),
        );
        retired.add(liability.commitmentDigest);
      }
      continue;
    }
    const selected = verify(result, evidence.point);
    await args.assertCurrent();
    if (evidence.certifiedAt !== undefined) {
      if (liability.hasActiveChallenge) {
        await args.store.savePromiseCapacityEvidence(
          { ...evidence, floorViolatedAt: args.boundary },
          promiseCapacityPointId(evidence.point),
        );
        throw new Error(
          "Active challenge contradicts protected promise capacity floor",
        );
      }
      retired.add(liability.commitmentDigest);
      continue;
    }
    if (liability.hasActiveChallenge) continue;
    if (
      evidence.retirementKind === "terminal" &&
      (liability.terminalPoint === undefined ||
        promiseCapacityPointId(liability.terminalPoint) !==
          promiseCapacityPointId(evidence.point))
    )
      continue;
    // Capacity is released once the cutoff or terminal is observed on the
    // selected chain. Certification, once the observation is final (deeper
    // than the recovery depth k), is bookkeeping for store retirement and
    // never holds new signing. A rollback of an uncertified observation
    // charges the promise again above until the cutoff is observed on the
    // new chain.
    retired.add(liability.commitmentDigest);
    if (
      !isFinal(depth(selected.tip.blockNo, evidence.point.blockNo), {
        securityParameter: args.recoveryDepth,
      })
    )
      continue;
    const certified: PromiseCapacityEvidence = {
      ...evidence,
      certifiedAt: selected.tip,
    };
    await args.store.savePromiseCapacityEvidence(
      certified,
      promiseCapacityPointId(evidence.point),
    );
  }
  await args.assertCurrent();
  return retired;
};
