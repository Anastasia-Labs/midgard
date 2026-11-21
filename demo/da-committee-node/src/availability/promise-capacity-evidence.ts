import { createHash } from "node:crypto";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { depth, isFinal } from "@al-ft/midgard-l1-follower";

export type PromiseCapacityPoint = Readonly<{
  slot: number;
  blockHash: string;
  blockNo: number;
}>;
export type PromiseCapacityEvidence = Readonly<{
  deploymentFingerprint: string;
  contractManifestId: string;
  actorId: string;
  headerHash: string;
  commitmentDigest: string;
  cutoffTimeMs: number;
  recoveryDepth: number;
  retirementKind: "open_cutoff" | "terminal";
  point: PromiseCapacityPoint;
  /** The first deep certificate establishes a protected monotonic rollback floor. */
  certifiedAt?: PromiseCapacityPoint;
  floorViolatedAt?: PromiseCapacityPoint;
}>;
export const promiseCapacityEvidenceKey = (
  record: Pick<
    PromiseCapacityEvidence,
    | "deploymentFingerprint"
    | "contractManifestId"
    | "actorId"
    | "commitmentDigest"
  >,
): string =>
  createHash("sha256")
    .update(
      canonicalJson(
        {
          deploymentFingerprint: record.deploymentFingerprint,
          contractManifestId: record.contractManifestId,
          actorId: record.actorId,
          commitmentDigest: record.commitmentDigest,
        },
        "promise capacity evidence identity",
      ),
    )
    .digest("hex");
export const promiseCapacityPointId = (point: PromiseCapacityPoint): string =>
  `${point.slot}:${point.blockHash}`;
const natural = (value: unknown): value is number =>
  typeof value === "number" && Number.isSafeInteger(value) && value >= 0;
const hash = (value: unknown, length: number): value is string =>
  typeof value === "string" &&
  new RegExp(`^[0-9a-f]{${length}}$`, "u").test(value);
const point = (value: unknown): PromiseCapacityPoint => {
  if (value === null || typeof value !== "object")
    throw new Error("Capacity evidence point is unavailable");
  const p = value as Record<string, unknown>;
  if (!natural(p.slot) || !natural(p.blockNo) || !hash(p.blockHash, 64))
    throw new Error("Capacity evidence point is malformed");
  return { slot: p.slot, blockNo: p.blockNo, blockHash: p.blockHash };
};
export const parsePromiseCapacityEvidence = (
  value: unknown,
): PromiseCapacityEvidence => {
  if (value === null || typeof value !== "object")
    throw new Error("Capacity evidence is unavailable");
  const r = value as Record<string, unknown>;
  if (
    !hash(r.deploymentFingerprint, 64) ||
    !hash(r.contractManifestId, 64) ||
    !hash(r.actorId, 56) ||
    !hash(r.headerHash, 56) ||
    !hash(r.commitmentDigest, 64) ||
    !natural(r.cutoffTimeMs) ||
    !natural(r.recoveryDepth) ||
    (r.retirementKind !== "open_cutoff" && r.retirementKind !== "terminal")
  )
    throw new Error("Capacity evidence identity is malformed");
  const captured = point(r.point);
  const certificate =
    r.certifiedAt === undefined ? undefined : point(r.certifiedAt);
  if (
    certificate !== undefined &&
    (!isFinal(depth(certificate.blockNo, captured.blockNo), {
      securityParameter: r.recoveryDepth,
    }) ||
      certificate.slot <= captured.slot ||
      certificate.blockHash === captured.blockHash)
  )
    throw new Error(
      "Capacity evidence certificate is within the recovery horizon",
    );
  const violation =
    r.floorViolatedAt === undefined ? undefined : point(r.floorViolatedAt);
  if (violation !== undefined && certificate === undefined)
    throw new Error("Capacity floor violation lacks a prior certificate");
  return {
    deploymentFingerprint: r.deploymentFingerprint,
    contractManifestId: r.contractManifestId,
    actorId: r.actorId,
    headerHash: r.headerHash,
    commitmentDigest: r.commitmentDigest,
    cutoffTimeMs: r.cutoffTimeMs,
    recoveryDepth: r.recoveryDepth,
    retirementKind: r.retirementKind,
    point: captured,
    ...(certificate === undefined ? {} : { certifiedAt: certificate }),
    ...(violation === undefined ? {} : { floorViolatedAt: violation }),
  };
};
/** Atomic compare-and-set; a certified floor never moves backward or changes branch. */
export const mergePromiseCapacityEvidence = (
  existing: PromiseCapacityEvidence | undefined,
  next: PromiseCapacityEvidence,
  expectedPointId?: string,
): PromiseCapacityEvidence => {
  const proposed = parsePromiseCapacityEvidence(next);
  if (existing === undefined) {
    if (expectedPointId !== undefined)
      throw new Error("Capacity evidence compare-and-set lost its row");
    return proposed;
  }
  const current = parsePromiseCapacityEvidence(existing);
  const identity = (r: PromiseCapacityEvidence) => ({
    deploymentFingerprint: r.deploymentFingerprint,
    contractManifestId: r.contractManifestId,
    actorId: r.actorId,
    headerHash: r.headerHash,
    commitmentDigest: r.commitmentDigest,
    cutoffTimeMs: r.cutoffTimeMs,
    recoveryDepth: r.recoveryDepth,
  });
  if (
    canonicalJson(identity(current), "capacity identity") !==
    canonicalJson(identity(proposed), "capacity identity")
  )
    throw new Error("Capacity evidence identity changed");
  if (expectedPointId === undefined) return current;
  if (promiseCapacityPointId(current.point) !== expectedPointId)
    throw new Error("Capacity evidence compare-and-set changed");
  if (current.certifiedAt !== undefined) {
    if (
      promiseCapacityPointId(current.point) !==
        promiseCapacityPointId(proposed.point) ||
      current.point.blockNo !== proposed.point.blockNo ||
      current.retirementKind !== proposed.retirementKind
    )
      throw new Error("Cannot replace a protected capacity rollback floor");
    return current.floorViolatedAt !== undefined ||
      proposed.floorViolatedAt === undefined
      ? current
      : { ...current, floorViolatedAt: proposed.floorViolatedAt };
  }
  return proposed;
};
