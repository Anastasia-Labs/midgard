import { createHash } from "node:crypto";
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import {
  parseStateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedReplayCheckpoint,
  type StateQueueAuthenticatedTransition,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { correctionRewindRemovedHeaders } from "../database/eventHistoryRecoveryPlans.js";

export const STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION =
  "midgard-node-state-queue-correction-observer-v1" as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type Tip = Readonly<{
  blockHash: string;
  slot: number;
  blockNo: number;
}>;

export type StateQueueCorrectionObserverSource = Readonly<{
  readQueue: () => Promise<readonly StateQueueTransitionNode[]>;
  observeTransitions: (
    previousQueue: readonly StateQueueTransitionNode[],
    nextQueue: readonly StateQueueTransitionNode[],
  ) => Promise<readonly StateQueueAuthenticatedReplayCheckpoint[]>;
  canonicalDepth: (
    transition: StateQueueAuthenticatedTransition,
  ) => Promise<bigint | null>;
}>;

export type StateQueueCorrectionObserverState = Readonly<{
  schemaVersion: typeof STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  cursorQueue: readonly StateQueueTransitionNode[];
  pending: readonly StateQueueAuthenticatedTransition[];
  admitted: readonly StateQueueAuthenticatedTransition[];
  retractedTransactionHashes: readonly string[];
  postFinalityRollbackIncidents: readonly Readonly<{
    transactionHash: string;
    transitionDigest: string;
  }>[];
  stateDigest: string;
}>;

type ObserverStateWithoutDigest = Omit<
  StateQueueCorrectionObserverState,
  "stateDigest"
>;

export type StateQueueCorrectionObserverStore = Readonly<{
  load: () => Promise<unknown | null>;
  save: (state: StateQueueCorrectionObserverState) => Promise<void>;
}>;

export type StateQueueCorrectionObserverResult = Readonly<{
  status: "bootstrapped" | "reconciled";
  admittedTransactionHashes: readonly string[];
  retractedTransactionHashes: readonly string[];
  postFinalityRollbackTransactionHashes: readonly string[];
}>;

const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJson).join(",")}]`;
  }
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};

export const digest = (value: unknown): string =>
  createHash("sha256").update(canonicalJson(value)).digest("hex");

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  return Object.getPrototypeOf(value) === Object.prototype &&
    actual.length === keys.length &&
    actual.every((key) => typeof key === "string" && expected.has(key))
    ? (value as Record<string, unknown>)
    : null;
};

export const parseQueue = (
  value: unknown,
): readonly StateQueueTransitionNode[] | null => {
  if (!Array.isArray(value) || value.length === 0) return null;
  const queue = value.map((candidate) => {
    const node = exactRecord(candidate, ["headerHash", "outRef"]);
    return node !== null &&
      (node.headerHash === null ||
        (typeof node.headerHash === "string" &&
          HEX_28.test(node.headerHash))) &&
      typeof node.outRef === "string" &&
      OUT_REF.test(node.outRef)
      ? ({
          headerHash: node.headerHash as string | null,
          outRef: node.outRef,
        } satisfies StateQueueTransitionNode)
      : null;
  });
  return queue.some((node) => node === null) ||
    queue[0]?.headerHash !== null ||
    new Set(queue.map((node) => node!.headerHash)).size !== queue.length ||
    new Set(queue.map((node) => node!.outRef)).size !== queue.length
    ? null
    : Object.freeze(queue as StateQueueTransitionNode[]);
};

export const makeState = (
  state: ObserverStateWithoutDigest,
): StateQueueCorrectionObserverState =>
  Object.freeze({ ...state, stateDigest: digest(state) });

export const parseStateQueueCorrectionObserverState = (
  input: unknown,
): StateQueueCorrectionObserverState | null => {
  const record = exactRecord(input, [
    "schemaVersion",
    "deploymentIdentityDigest",
    "stateQueuePolicyId",
    "cursorQueue",
    "pending",
    "admitted",
    "retractedTransactionHashes",
    "postFinalityRollbackIncidents",
    "stateDigest",
  ]);
  const cursorQueue = parseQueue(record?.cursorQueue);
  const pending = Array.isArray(record?.pending)
    ? record.pending.map(parseStateQueueAuthenticatedTransition)
    : null;
  const admitted = Array.isArray(record?.admitted)
    ? record.admitted.map(parseStateQueueAuthenticatedTransition)
    : null;
  const incidents = Array.isArray(record?.postFinalityRollbackIncidents)
    ? record.postFinalityRollbackIncidents.map((candidate) =>
        exactRecord(candidate, ["transactionHash", "transitionDigest"]),
      )
    : null;
  if (
    record === null ||
    record.schemaVersion !== STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION ||
    typeof record.deploymentIdentityDigest !== "string" ||
    !HEX_32.test(record.deploymentIdentityDigest) ||
    typeof record.stateQueuePolicyId !== "string" ||
    !HEX_28.test(record.stateQueuePolicyId) ||
    cursorQueue === null ||
    pending === null ||
    pending.some((transition) => transition === null) ||
    admitted === null ||
    admitted.some((transition) => transition === null) ||
    !Array.isArray(record.retractedTransactionHashes) ||
    record.retractedTransactionHashes.some(
      (txHash) => typeof txHash !== "string" || !HEX_32.test(txHash),
    ) ||
    incidents === null ||
    incidents.some(
      (incident) =>
        incident === null ||
        typeof incident.transactionHash !== "string" ||
        !HEX_32.test(incident.transactionHash) ||
        typeof incident.transitionDigest !== "string" ||
        !HEX_32.test(incident.transitionDigest),
    ) ||
    typeof record.stateDigest !== "string" ||
    !HEX_32.test(record.stateDigest)
  ) {
    return null;
  }
  const state = {
    schemaVersion: record.schemaVersion,
    deploymentIdentityDigest: record.deploymentIdentityDigest,
    stateQueuePolicyId: record.stateQueuePolicyId,
    cursorQueue,
    pending: pending as StateQueueAuthenticatedTransition[],
    admitted: admitted as StateQueueAuthenticatedTransition[],
    retractedTransactionHashes: record.retractedTransactionHashes as string[],
    postFinalityRollbackIncidents: incidents.map((incident) => ({
      transactionHash: incident!.transactionHash as string,
      transitionDigest: incident!.transitionDigest as string,
    })),
  } satisfies ObserverStateWithoutDigest;
  const all = [...state.pending, ...state.admitted];
  if (
    all.some(
      (transition) =>
        transition.deploymentIdentityDigest !==
          state.deploymentIdentityDigest ||
        transition.stateQueuePolicyId !== state.stateQueuePolicyId,
    ) ||
    new Set(all.map(({ transactionHash }) => transactionHash)).size !==
      all.length ||
    new Set(state.retractedTransactionHashes).size !==
      state.retractedTransactionHashes.length ||
    digest(state) !== record.stateDigest
  ) {
    return null;
  }
  return Object.freeze({ ...state, stateDigest: record.stateDigest });
};

export const createFileStateQueueCorrectionObserverStore = (
  path: string,
): StateQueueCorrectionObserverStore => ({
  load: async () => {
    try {
      return JSON.parse(await readFile(path, "utf8")) as unknown;
    } catch (cause) {
      if ((cause as NodeJS.ErrnoException).code === "ENOENT") return null;
      throw cause;
    }
  },
  save: async (state) => {
    await mkdir(dirname(path), { recursive: true });
    const temporary = `${path}.tmp-${process.pid.toString()}`;
    await writeFile(temporary, `${JSON.stringify(state, null, 2)}\n`, {
      encoding: "utf8",
      mode: 0o600,
    });
    await rename(temporary, path);
  },
});

/** Explicit integrity failure: an authenticated state-queue view retracted a
 * removal whose native rewind already ran. The node cannot re-apply the
 * removed block; the release depth that admitted the removal is its only
 * rollback bound. */
export class StateQueueCorrectionRewindIntegrityError extends Error {
  readonly headerHash: string;
  constructor(headerHash: string, detail: string) {
    super(
      `State-queue correction integrity failure: block ${headerHash} was rewound out of the native ledger by an admitted correction, but ${detail}. The removal rolled back below its release depth; this node cannot re-apply a rewound block and refuses to continue.`,
    );
    this.name = "StateQueueCorrectionRewindIntegrityError";
    this.headerHash = headerHash;
  }
}

/** Every header a correction rewind removed is still removed by an admitted
 * timeout or fraud correction of `state`. */
export const assertRewoundRemovalsStand = (
  state: StateQueueCorrectionObserverState,
) =>
  Effect.gen(function* () {
    const rewound = yield* correctionRewindRemovedHeaders(
      state.deploymentIdentityDigest,
    );
    if (rewound.size === 0) return;
    const removed = new Set(
      state.admitted
        .filter(
          (transition) =>
            transition.transitionKind === "timeout_correction" ||
            transition.transitionKind === "fraud_removal",
        )
        .flatMap((transition) => transition.removedHeaderHashes),
    );
    for (const header of rewound.keys())
      if (!removed.has(header))
        return yield* Effect.fail(
          new StateQueueCorrectionRewindIntegrityError(
            header,
            "the authenticated state-queue view no longer removes it",
          ),
        );
  });
