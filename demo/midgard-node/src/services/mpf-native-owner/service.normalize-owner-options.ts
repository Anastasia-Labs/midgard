import { readFile } from "node:fs/promises";
import { type MessagePort } from "node:worker_threads";

import { positiveSafeInteger } from "../../artifact-schema.js";
import { MPF_EMPTY_ROOT } from "../../mpf/store-primitives.js";
import { createEventFlatDigest } from "../../workers/utils/mpf-event-flat-digest.js";
import {
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  type NativeMpfGenerationHandle,
  type NativeMpfRpcFrame,
  NativeMpfRpcKind,
} from "./protocol.js";

export const EMPTY_ROOT_HEX = MPF_EMPTY_ROOT.toString("hex");

export const HASH_BYTES = 32;

export const FULL_INDEX_HEADER_BYTES = 72;

export const FULL_INDEX_MAX_BYTES = 512 * 1024 * 1024;

export const FULL_INDEX_MAX_RECORDS = 2_000_000;

export const EVENT_LOG_HEADER_BYTES = 92;

export const LOAD_DIGEST_DOMAIN = Buffer.from("MIDGARD-MPF-OWNER-LOAD-V1");

export const EVENT_LOG_DIGEST_DOMAIN = Buffer.from(
  "MIDGARD-MPF-ARCH-G-EVENT-LOG-V1",
);

export const EVENT_STREAM_DIGEST_DOMAIN = Buffer.from(
  "MIDGARD-MPF-ARCH-G-EVENTS-V1",
);

export const PROMOTION_DIGEST_DOMAIN = Buffer.from(
  "MIDGARD-MPF-OWNER-PROMOTION-V1",
);

export const SELF_TEST_DOMAIN = Buffer.from(
  "MIDGARD-MPF-OWNER-BLAKE2B-SELFTEST-V1",
);

export const ZERO_EPOCH = Buffer.alloc(16);

export const SIDECAR_MAGIC = "MGNS";

export const SIDECAR_HEADER_BYTES = 144;

export const SIDECAR_DIGEST_DOMAIN = Buffer.from(
  "MIDGARD-MPF-OWNER-NODE-SIDECAR-V1",
);

const NATIVE_OWNER_MIN_RUNTIME_HEADROOM_BYTES = 1024 * 1024 * 1024;

export const DEFAULT_RESTART_WINDOW_MS = 60 * 60 * 1000;

type StoredLeaf = {
  readonly __kind: "Leaf";
  readonly prefix: string;
  readonly key: string;
  readonly value: string;
};

type StoredBranch = {
  readonly __kind: "Branch";
  readonly prefix: string;
  readonly children: readonly (string | null)[];
  readonly size: number;
};

export type StoredNode = StoredLeaf | StoredBranch;

export type StoredValue = StoredNode | string;

export type DecodedPromotionRecord = {
  readonly hash: Buffer;
  readonly hashHex: string;
  readonly encoded: Buffer;
  readonly stored: StoredNode;
};

export type PendingRequest = {
  readonly expected: ReadonlySet<NativeMpfRpcKind>;
  readonly resolve: (frame: NativeMpfRpcFrame) => void;
  readonly reject: (error: Error) => void;
  readonly timer: NodeJS.Timeout;
};

export type PendingPromotion = {
  readonly chunks: Buffer[];
  readonly resolve: (value: {
    frame: NativeMpfRpcFrame;
    bytes: Buffer;
  }) => void;
  readonly reject: (error: Error) => void;
  readonly timer: NodeJS.Timeout;
};

export type WorkerGenerationLease = {
  readonly port: MessagePort;
  readonly handle: NativeMpfGenerationHandle;
  journalOwned: boolean;
};

export type NativeMpfEventOp =
  | {
      readonly type: "insert";
      readonly key: Uint8Array;
      readonly value: Uint8Array;
    }
  | { readonly type: "delete"; readonly key: Uint8Array };

export type NativeMpfOwnerServiceOptions = {
  readonly levelPath: string;
  readonly binaryPath: string;
  readonly binarySha256: string;
  readonly maxFrameBytes?: number;
  readonly maxChunkBytes?: number;
  readonly requestTimeoutMs?: number;
  readonly restartLimit?: number;
  /** Sliding window in which at most `restartLimit` child restarts may start. */
  readonly restartWindowMs?: number;
  readonly sidecarPath?: string;
  /** Process-crash and interleaving test seam; production callers must leave
   * this undefined. */
  readonly faultInjectionForTests?: (
    point:
      | "diagnostics_before_request"
      | "before_promotion_batch"
      | "after_promotion_batch_before_ack"
      | "before_root_restore_batch"
      | "after_root_restore_batch_before_ack",
  ) => void | Promise<void>;
  /** Process-lifecycle test seam; production callers must leave this undefined. */
  readonly onChildSpawnForTests?: (pid: number) => void;
};

export type NormalizedNativeMpfOwnerServiceOptions =
  NativeMpfOwnerServiceOptions & {
    readonly maxFrameBytes: number;
    readonly maxChunkBytes: number;
    readonly requestTimeoutMs: number;
    readonly restartLimit: number;
    readonly restartWindowMs: number;
  };

export const digest = (...parts: readonly Uint8Array[]): Buffer => {
  const state = createEventFlatDigest();
  for (const part of parts) state.update(part);
  return state.digest();
};

export const normalizeOwnerOptions = (
  options: NativeMpfOwnerServiceOptions,
): NormalizedNativeMpfOwnerServiceOptions => {
  const maxFrameBytes =
    options.maxFrameBytes ?? NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes;
  const maxChunkBytes =
    options.maxChunkBytes ?? NATIVE_MPF_OWNER_DEFAULT_CAPS.maxChunkBytes;
  const requestTimeoutMs =
    options.requestTimeoutMs ?? NATIVE_MPF_OWNER_DEFAULT_CAPS.loadTimeoutMs;
  const restartLimit = options.restartLimit ?? 3;
  const restartWindowMs = options.restartWindowMs ?? DEFAULT_RESTART_WINDOW_MS;
  positiveSafeInteger(maxFrameBytes, "maxFrameBytes");
  positiveSafeInteger(maxChunkBytes, "maxChunkBytes");
  positiveSafeInteger(requestTimeoutMs, "requestTimeoutMs");
  positiveSafeInteger(restartWindowMs, "restartWindowMs");
  if (!Number.isSafeInteger(restartLimit) || restartLimit < 0) {
    throw new Error("restartLimit must be a non-negative safe integer");
  }
  if (maxFrameBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes) {
    throw new Error("maxFrameBytes exceeds the compiled native owner cap");
  }
  if (maxChunkBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxChunkBytes) {
    throw new Error("maxChunkBytes exceeds the compiled native owner cap");
  }
  if (maxChunkBytes > maxFrameBytes - 68) {
    throw new Error("maxChunkBytes must fit inside maxFrameBytes");
  }
  return {
    ...options,
    maxFrameBytes,
    maxChunkBytes,
    requestTimeoutMs,
    restartLimit,
    restartWindowMs,
  };
};

export type NativeOwnerCgroupMemoryBudget =
  | { readonly kind: "finite"; readonly limitBytes: number }
  | { readonly kind: "unlimited" }
  | { readonly kind: "unavailable"; readonly containerized: boolean };

export const parseNativeOwnerCgroupMemoryLimit = (
  value: string,
):
  | Extract<
      NativeOwnerCgroupMemoryBudget,
      { readonly kind: "finite" | "unlimited" }
    >
  | undefined => {
  const normalized = value.trim();
  if (normalized === "max") return { kind: "unlimited" };
  if (!/^[0-9]+$/.test(normalized)) return undefined;
  const parsed = BigInt(normalized);
  if (parsed >= 1n << 60n) return { kind: "unlimited" };
  return parsed > 0n && parsed <= BigInt(Number.MAX_SAFE_INTEGER)
    ? { kind: "finite", limitBytes: Number(parsed) }
    : undefined;
};

export const assertNativeOwnerRuntimeMemoryBudget = ({
  cgroup,
  v8HeapLimitBytes,
}: {
  readonly cgroup: NativeOwnerCgroupMemoryBudget;
  readonly v8HeapLimitBytes: number;
}): void => {
  if (cgroup.kind === "unlimited") return;
  if (cgroup.kind === "unavailable") {
    if (cgroup.containerized) {
      throw new Error(
        "Architecture G cannot prove an enforceable container memory budget",
      );
    }
    return;
  }
  const cgroupLimitBytes = cgroup.limitBytes;
  const requiredBytes =
    v8HeapLimitBytes +
    NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes +
    NATIVE_OWNER_MIN_RUNTIME_HEADROOM_BYTES;
  if (
    !Number.isSafeInteger(cgroupLimitBytes) ||
    cgroupLimitBytes <= 0 ||
    !Number.isSafeInteger(v8HeapLimitBytes) ||
    v8HeapLimitBytes <= 0
  ) {
    throw new Error("Architecture G runtime memory budget is invalid");
  }
  if (cgroupLimitBytes < requiredBytes) {
    throw new Error(
      `Architecture G runtime memory budget is insufficient: cgroup_limit_bytes=${cgroupLimitBytes.toString()},v8_heap_limit_bytes=${v8HeapLimitBytes.toString()},owner_cap_bytes=${NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes.toString()},required_headroom_bytes=${NATIVE_OWNER_MIN_RUNTIME_HEADROOM_BYTES.toString()},required_total_bytes=${requiredBytes.toString()}`,
    );
  }
};

export const readCgroupMemoryBudget =
  async (): Promise<NativeOwnerCgroupMemoryBudget> => {
    const candidates = ["/sys/fs/cgroup/memory.max"];
    try {
      const membership = await readFile("/proc/self/cgroup", "utf8");
      const unified = membership
        .split("\n")
        .find((line) => line.startsWith("0::"))
        ?.slice(3);
      if (unified !== undefined) {
        candidates.unshift(
          `/sys/fs/cgroup${unified === "/" ? "" : unified}/memory.max`,
        );
      }
    } catch {
      // The fixed root candidate still covers ordinary container cgroup mounts.
    }
    candidates.push("/sys/fs/cgroup/memory/memory.limit_in_bytes");
    for (const path of candidates) {
      try {
        const value = (await readFile(path, "utf8")).trim();
        const parsed = parseNativeOwnerCgroupMemoryLimit(value);
        if (parsed !== undefined) return parsed;
      } catch {
        // Try the next supported cgroup layout.
      }
    }
    const containerized = await Promise.any(
      ["/.dockerenv", "/run/.containerenv"].map((path) =>
        readFile(path).then(() => true),
      ),
    ).catch(() => false);
    return { kind: "unavailable", containerized };
  };

export const assertBufferLength = (
  value: Uint8Array,
  length: number,
  field: string,
): Buffer => {
  const buffer = Buffer.from(value);
  if (buffer.byteLength !== length) {
    throw new Error(`${field} must contain exactly ${length.toString()} bytes`);
  }
  return buffer;
};

export const assertStoredHash = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${field} must be canonical 32-byte lowercase hex`);
  }
  return value;
};
