import { createHash } from "node:crypto";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type { PromiseCapacityPoint } from "../availability/promise-capacity-evidence.js";
import type { StateQueueHeaderRecord } from "../domain.js";
import { headerHashOf } from "../l1/follower/queue-derivation.js";
import type { StoreData } from "../store.committee-store.js";

export type CommitteeRetirementBinding = Readonly<{
  deploymentFingerprint: string;
  manifestSha256: string;
  contractManifestId: string;
  committeeSignersHash: string;
  actorId: string;
  sourceAuthoritySha256: string;
  peerIds: readonly string[];
  retentionDays: number;
  recoveryDepth: number;
  maximumRecords: number;
  maximumEncodedBytes: number;
}>;
export type CommitteeRetirementBreachPoint = Readonly<{
  slot: number;
  blockHash: string;
  blockNo?: number;
}>;
export type CommitteeRetirementFloor = Readonly<{
  schemaVersion: 1;
  binding: CommitteeRetirementBinding;
  generation: number;
  headerEndTimeMs?: number;
  point?: PromiseCapacityPoint;
  certifiedAt?: PromiseCapacityPoint;
  checkpoint?: Readonly<{
    point: PromiseCapacityPoint;
    canonicalTimeMs: number;
  }>;
  nonceTimeFloorMs: number;
  breach?: Readonly<{
    reason: string;
    observedAt: CommitteeRetirementBreachPoint;
  }>;
  digest: string;
}>;
/** Local opaque capability. It is not chain evidence and cannot release anything. */
export type CommitteeRetirementGuard = Readonly<{
  readonly retirementGuard: unique symbol;
}>;
export type CommitteeRetirementPort = Readonly<{
  capture: (
    scope: DaAvailabilityReadScope,
  ) => Promise<CommitteeRetirementGuard>;
  prove: (
    token: CommitteeRetirementGuard,
    scope: DaAvailabilityReadScope,
  ) => Promise<void>;
  assert: (
    token: CommitteeRetirementGuard,
    record: StateQueueHeaderRecord,
  ) => void;
}>;
export const retirementDigest = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJson(value, "committee retirement"))
    .digest("hex");
const natural = (v: unknown): v is number =>
  typeof v === "number" && Number.isSafeInteger(v) && v >= 0;
const hash = (v: unknown, n: number): v is string =>
  typeof v === "string" && new RegExp(`^[0-9a-f]{${n}}$`, "u").test(v);
export const parseRetirementPoint = (value: unknown): PromiseCapacityPoint => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    throw new Error("Retirement point is malformed");
  const r = value as Record<string, unknown>;
  if (
    Object.keys(r).sort().join() !== "blockHash,blockNo,slot" ||
    !natural(r.slot) ||
    !natural(r.blockNo) ||
    !hash(r.blockHash, 64)
  )
    throw new Error("Retirement point is malformed");
  return { slot: r.slot, blockHash: r.blockHash, blockNo: r.blockNo };
};
export const parseRetirementBreachPoint = (
  value: unknown,
): CommitteeRetirementBreachPoint => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    throw new Error("Retirement breach point is malformed");
  const r = value as Record<string, unknown>;
  if (
    Object.keys(r).some((k) => !["slot", "blockHash", "blockNo"].includes(k)) ||
    !natural(r.slot) ||
    !hash(r.blockHash, 64) ||
    (r.blockNo !== undefined && !natural(r.blockNo))
  )
    throw new Error("Retirement breach point is malformed");
  return {
    slot: r.slot,
    blockHash: r.blockHash,
    ...(r.blockNo === undefined ? {} : { blockNo: r.blockNo }),
  };
};
export const sameRetirementPoint = (
  a: PromiseCapacityPoint,
  b: PromiseCapacityPoint,
): boolean =>
  a.slot === b.slot && a.blockHash === b.blockHash && a.blockNo === b.blockNo;
export const parseRetirementBinding = (
  value: unknown,
): CommitteeRetirementBinding => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    throw new Error("Retirement binding is unavailable");
  const r = value as Record<string, unknown>;
  if (
    Object.keys(r).sort().join() !==
      "actorId,committeeSignersHash,contractManifestId,deploymentFingerprint,manifestSha256,maximumEncodedBytes,maximumRecords,peerIds,recoveryDepth,retentionDays,sourceAuthoritySha256" ||
    !hash(r.deploymentFingerprint, 64) ||
    !hash(r.manifestSha256, 64) ||
    !hash(r.contractManifestId, 64) ||
    !hash(r.committeeSignersHash, 64) ||
    !hash(r.actorId, 56) ||
    !hash(r.sourceAuthoritySha256, 64) ||
    !Array.isArray(r.peerIds) ||
    r.peerIds.some((p) => typeof p !== "string" || !p || p.length > 256) ||
    new Set(r.peerIds).size !== r.peerIds.length ||
    r.peerIds.length > 256 ||
    !natural(r.retentionDays) ||
    r.retentionDays < 15 ||
    !Number.isSafeInteger(r.retentionDays * 86400000) ||
    r.recoveryDepth !== 2160 ||
    r.maximumRecords !== 512 ||
    r.maximumEncodedBytes !== 8 * 1024 * 1024
  )
    throw new Error("Retirement binding is outside the signed bounded profile");
  return Object.freeze({
    deploymentFingerprint: r.deploymentFingerprint,
    manifestSha256: r.manifestSha256,
    contractManifestId: r.contractManifestId,
    committeeSignersHash: r.committeeSignersHash,
    actorId: r.actorId,
    sourceAuthoritySha256: r.sourceAuthoritySha256,
    peerIds: Object.freeze([...r.peerIds].sort()) as readonly string[],
    retentionDays: r.retentionDays,
    recoveryDepth: r.recoveryDepth,
    maximumRecords: r.maximumRecords,
    maximumEncodedBytes: r.maximumEncodedBytes,
  });
};
export const makeRetirementFloor = (
  value: Omit<CommitteeRetirementFloor, "digest">,
): CommitteeRetirementFloor =>
  parseRetirementFloor({ ...value, digest: retirementDigest(value) });
export const parseRetirementFloor = (
  value: unknown,
): CommitteeRetirementFloor => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    throw new Error("Retirement floor is malformed");
  const r = value as Record<string, unknown>;
  if (
    Object.keys(r).some(
      (k) =>
        ![
          "schemaVersion",
          "binding",
          "generation",
          "headerEndTimeMs",
          "point",
          "certifiedAt",
          "nonceTimeFloorMs",
          "checkpoint",
          "breach",
          "digest",
        ].includes(k),
    ) ||
    r.schemaVersion !== 1 ||
    !natural(r.generation) ||
    (r.headerEndTimeMs !== undefined && !natural(r.headerEndTimeMs)) ||
    !natural(r.nonceTimeFloorMs) ||
    !hash(r.digest, 64)
  )
    throw new Error("Retirement floor is malformed");
  const binding = parseRetirementBinding(r.binding);
  const point =
      r.point === undefined ? undefined : parseRetirementPoint(r.point),
    certifiedAt =
      r.certifiedAt === undefined
        ? undefined
        : parseRetirementPoint(r.certifiedAt);
  if (
    (r.headerEndTimeMs === undefined) !== (point === undefined) ||
    (point === undefined) !== (certifiedAt === undefined)
  )
    throw new Error("Retirement F/P/C must be paired");
  if (
    point &&
    certifiedAt &&
    (certifiedAt.blockNo - point.blockNo <= binding.recoveryDepth ||
      certifiedAt.slot <= point.slot ||
      certifiedAt.blockHash === point.blockHash)
  )
    throw new Error("Retirement floor lacks strict recovery depth");
  let checkpoint: CommitteeRetirementFloor["checkpoint"];
  if (r.checkpoint !== undefined) {
    if (
      r.checkpoint === null ||
      typeof r.checkpoint !== "object" ||
      Array.isArray(r.checkpoint)
    )
      throw new Error("Retirement checkpoint is malformed");
    const c = r.checkpoint as Record<string, unknown>;
    if (
      Object.keys(c).sort().join() !== "canonicalTimeMs,point" ||
      !natural(c.canonicalTimeMs)
    )
      throw new Error("Retirement checkpoint is malformed");
    checkpoint = Object.freeze({
      point: Object.freeze(parseRetirementPoint(c.point)),
      canonicalTimeMs: c.canonicalTimeMs,
    });
  }
  let breach: CommitteeRetirementFloor["breach"];
  if (r.breach !== undefined) {
    if (
      r.breach === null ||
      typeof r.breach !== "object" ||
      Array.isArray(r.breach)
    )
      throw new Error("Retirement breach is malformed");
    const b = r.breach as Record<string, unknown>;
    if (
      Object.keys(b).sort().join() !== "observedAt,reason" ||
      typeof b.reason !== "string" ||
      !b.reason ||
      b.reason.length > 512
    )
      throw new Error("Retirement breach is malformed");
    breach = Object.freeze({
      reason: b.reason,
      observedAt: parseRetirementBreachPoint(b.observedAt),
    });
  }
  const result = {
    schemaVersion: 1 as const,
    binding,
    generation: r.generation,
    ...(point === undefined
      ? {}
      : {
          headerEndTimeMs: r.headerEndTimeMs as number,
          point: Object.freeze(point),
          certifiedAt: Object.freeze(certifiedAt!),
        }),
    ...(checkpoint === undefined ? {} : { checkpoint }),
    nonceTimeFloorMs: r.nonceTimeFloorMs,
    ...(breach === undefined ? {} : { breach }),
  };
  if (retirementDigest(result) !== r.digest)
    throw new Error("Retirement floor digest mismatch");
  return Object.freeze({ ...result, digest: r.digest });
};
export const authenticatedHeaderEnd = (
  record: StateQueueHeaderRecord,
): number => {
  const end = record.header.endTime;
  if (
    typeof end !== "bigint" ||
    end < 0n ||
    end > BigInt(Number.MAX_SAFE_INTEGER) ||
    headerHashOf(record.header) !== record.headerHash ||
    record.computedHeaderHash !== record.headerHash ||
    record.validationErrors.length
  )
    throw new Error("Retirement header is not authenticated");
  return Number(end);
};
export class CommitteeRetirementController {
  private floor: CommitteeRetirementFloor | undefined;
  private inFlight = false;
  private discoveries = 0;
  private breachInFlight = false;
  private readonly tokens = new WeakMap<
    object,
    Readonly<{ generation: number; digest: string }>
  >();
  private readonly pins = new Map<string, number>();
  load(floor: CommitteeRetirementFloor | undefined): void {
    if (
      this.floor &&
      (!floor ||
        floor.generation < this.floor.generation ||
        (floor.generation === this.floor.generation &&
          floor.digest !== this.floor.digest))
    )
      throw new Error("Retirement cache cannot move backward");
    this.floor = floor;
  }
  current(): CommitteeRetirementFloor | undefined {
    return this.floor;
  }
  capture(): CommitteeRetirementGuard {
    if (this.inFlight || this.breachInFlight || this.floor?.breach)
      throw new Error("Retirement authority is held");
    const token = Object.freeze({}) as CommitteeRetirementGuard;
    this.tokens.set(token, {
      generation: this.floor?.generation ?? 0,
      digest: this.floor?.digest ?? "",
    });
    return token;
  }
  assert(
    token: CommitteeRetirementGuard,
    record?: StateQueueHeaderRecord,
  ): void {
    const entry = this.tokens.get(token);
    if (
      !entry ||
      this.inFlight ||
      this.breachInFlight ||
      this.floor?.breach ||
      entry.generation !== (this.floor?.generation ?? 0) ||
      entry.digest !== (this.floor?.digest ?? "")
    )
      throw new Error("Retirement generation changed or is held");
    if (
      record &&
      this.floor &&
      (record.deploymentFingerprint !==
        this.floor.binding.deploymentFingerprint ||
        authenticatedHeaderEnd(record) <= (this.floor.headerEndTimeMs ?? -1))
    )
      throw new Error("Header is at or below the permanent retirement floor");
  }
  begin(token: CommitteeRetirementGuard): void {
    this.assert(token);
    if (this.discoveries) throw new Error("Retirement discovery is active");
    this.inFlight = true;
  }
  discoveryActive(): boolean {
    return this.discoveries > 0;
  }
  discovery(): () => void {
    if (this.inFlight || this.breachInFlight || this.floor?.breach)
      throw new Error("Retirement authority is held");
    this.discoveries++;
    let released = false;
    return () => {
      if (!released) {
        released = true;
        this.discoveries--;
      }
    };
  }
  holdBreach(): void {
    this.breachInFlight = true;
  }
  // A failed persistence keeps the process held; restart rechecks native authority.
  persistedBreach(): void {
    this.breachInFlight = false;
  }
  end(): void {
    this.inFlight = false;
  }
  generation(): number {
    if (this.inFlight || this.breachInFlight)
      throw new Error("Retirement transition is in flight");
    return this.floor?.generation ?? 0;
  }
  assertGeneration(generation: number): void {
    if (
      this.inFlight ||
      this.breachInFlight ||
      this.floor?.breach ||
      generation !== (this.floor?.generation ?? 0)
    )
      throw new Error("Stale retirement writer or held authority");
  }
  pin(headerHash: string): () => void {
    if (this.inFlight || this.breachInFlight || this.floor?.breach)
      throw new Error("Retirement authority is held");
    this.pins.set(headerHash, (this.pins.get(headerHash) ?? 0) + 1);
    let released = false;
    return () => {
      if (released) return;
      released = true;
      const n = this.pins.get(headerHash)!;
      if (n === 1) this.pins.delete(headerHash);
      else this.pins.set(headerHash, n - 1);
    };
  }
  pinned(): ReadonlySet<string> {
    return new Set(this.pins.keys());
  }
}
export type CommitteeRetirementSnapshot = Readonly<{
  data: StoreData;
  digest: string;
  guard: CommitteeRetirementGuard;
}>;

/** Prospective byte headroom only; these synthetic maxima are never chain evidence.
 * The binding is fixed. Every optional singleton field and its bounded breach
 * reason fit this enclosing JSON representation (also above PG jsonb growth).
 */
export const retirementMetadataGrowthReserve = (
  value: CommitteeRetirementFloor,
): number => {
  const floor = parseRetirementFloor(value),
    max = Number.MAX_SAFE_INTEGER;
  const point = {
    slot: max - 3000,
    blockNo: max - 3000,
    blockHash: "ff".repeat(32),
  };
  const certifiedAt = { slot: max, blockNo: max, blockHash: "ee".repeat(32) };
  const largest = makeRetirementFloor({
    schemaVersion: 1,
    binding: floor.binding,
    generation: max,
    headerEndTimeMs: max,
    point,
    certifiedAt,
    checkpoint: { point: certifiedAt, canonicalTimeMs: max },
    nonceTimeFloorMs: max,
    breach: { reason: "\u0001".repeat(512), observedAt: certifiedAt },
  });
  const bytes = (record: CommitteeRetirementFloor) =>
    Buffer.byteLength(
      JSON.stringify({ retirementFloor: record }, null, 2) + "\n",
    );
  return Math.max(0, bytes(largest) - bytes(floor));
};
