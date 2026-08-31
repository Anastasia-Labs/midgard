import { createHash } from "node:crypto";
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import {
  deriveStateQueueAuthenticatedReplayCheckpointV1,
  parseStateQueueAuthenticatedTransitionV1,
  replayStateQueueAuthenticatedCheckpointsV1,
  type StateQueueAuthenticatedReplayCheckpointV1,
  type StateQueueAuthenticatedTransitionV1,
  type StateQueueTransitionNodeV1,
  withStateQueueAuthenticatedTransitionFinalityDepthV1,
} from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as DaPayloadTerminalOutcomesDB from "@/database/daPayloadTerminalOutcomes.js";
import {
  fetchKupoAncestorPointV1,
  fetchKupoSpendV1,
  type FetchLike,
  type KupoSpendV1,
  normalizeKupoHttpUrl,
  type ObservedL1TransactionAtPointV1,
  readOgmiosBlockTransactionV1,
  type WebSocketFactory,
} from "@/l1-tx-order-carriage-v1.js";
import { normalizeOgmiosHttpUrl } from "@/local-ledger-slot.js";

export const STATE_QUEUE_CORRECTION_OBSERVER_V1_SCHEMA_VERSION =
  "midgard-node-state-queue-correction-observer-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

type TipV1 = Readonly<{ blockHash: string; slot: number; blockNo: number }>;

export type StateQueueCorrectionObserverSourceV1 = Readonly<{
  readQueue: () => Promise<readonly StateQueueTransitionNodeV1[]>;
  observeTransitions: (
    previousQueue: readonly StateQueueTransitionNodeV1[],
    nextQueue: readonly StateQueueTransitionNodeV1[],
  ) => Promise<readonly StateQueueAuthenticatedReplayCheckpointV1[]>;
  canonicalDepth: (
    transition: StateQueueAuthenticatedTransitionV1,
  ) => Promise<bigint | null>;
}>;

export type StateQueueCorrectionObserverStateV1 = Readonly<{
  schemaVersion: typeof STATE_QUEUE_CORRECTION_OBSERVER_V1_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  cursorQueue: readonly StateQueueTransitionNodeV1[];
  pending: readonly StateQueueAuthenticatedTransitionV1[];
  admitted: readonly StateQueueAuthenticatedTransitionV1[];
  retractedTransactionHashes: readonly string[];
  postFinalityRollbackIncidents: readonly Readonly<{
    transactionHash: string;
    transitionDigest: string;
  }>[];
  stateDigest: string;
}>;

type ObserverStateWithoutDigest = Omit<
  StateQueueCorrectionObserverStateV1,
  "stateDigest"
>;

export type StateQueueCorrectionObserverStoreV1 = Readonly<{
  load: () => Promise<unknown | null>;
  save: (state: StateQueueCorrectionObserverStateV1) => Promise<void>;
}>;

export type StateQueueCorrectionObserverResultV1 = Readonly<{
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

const digest = (value: unknown): string =>
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

const parseQueue = (
  value: unknown,
): readonly StateQueueTransitionNodeV1[] | null => {
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
        } satisfies StateQueueTransitionNodeV1)
      : null;
  });
  return queue.some((node) => node === null) ||
    queue[0]?.headerHash !== null ||
    new Set(queue.map((node) => node!.headerHash)).size !== queue.length ||
    new Set(queue.map((node) => node!.outRef)).size !== queue.length
    ? null
    : Object.freeze(queue as StateQueueTransitionNodeV1[]);
};

const makeState = (
  state: ObserverStateWithoutDigest,
): StateQueueCorrectionObserverStateV1 =>
  Object.freeze({ ...state, stateDigest: digest(state) });

export const parseStateQueueCorrectionObserverStateV1 = (
  input: unknown,
): StateQueueCorrectionObserverStateV1 | null => {
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
    ? record.pending.map(parseStateQueueAuthenticatedTransitionV1)
    : null;
  const admitted = Array.isArray(record?.admitted)
    ? record.admitted.map(parseStateQueueAuthenticatedTransitionV1)
    : null;
  const incidents = Array.isArray(record?.postFinalityRollbackIncidents)
    ? record.postFinalityRollbackIncidents.map((candidate) =>
        exactRecord(candidate, ["transactionHash", "transitionDigest"]),
      )
    : null;
  if (
    record === null ||
    record.schemaVersion !==
      STATE_QUEUE_CORRECTION_OBSERVER_V1_SCHEMA_VERSION ||
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
    pending: pending as StateQueueAuthenticatedTransitionV1[],
    admitted: admitted as StateQueueAuthenticatedTransitionV1[],
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

export const createFileStateQueueCorrectionObserverStoreV1 = (
  path: string,
): StateQueueCorrectionObserverStoreV1 => ({
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

/**
 * Production observer store. The cursor/admitted set and its terminal-outcome
 * projection commit in one SQL transaction, so a crash cannot leave a newly
 * admitted deletion authority without its durable observer state (or retain a
 * revoked authority after the replacement cursor commits).
 */
export const createDatabaseStateQueueCorrectionObserverStoreV1 = ({
  sql,
  deploymentManifest,
}: {
  readonly sql: SqlClient.SqlClient;
  readonly deploymentManifest: unknown;
}): StateQueueCorrectionObserverStoreV1 => ({
  load: async () => {
    const authority =
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthorityV1(
        deploymentManifest,
      );
    if (authority === null) {
      throw new Error("Observer database store has no authenticated release");
    }
    const rows = await Effect.runPromise(
      sql<{ readonly state_record: unknown }>`
        SELECT state_record
        FROM state_queue_terminal_observer_states
        WHERE deployment_identity_digest = ${authority.deploymentIdentityDigest}
        LIMIT 1`.pipe(Effect.provideService(SqlClient.SqlClient, sql)),
    );
    return rows[0]?.state_record ?? null;
  },
  save: async (stateInput) => {
    const state = parseStateQueueCorrectionObserverStateV1(stateInput);
    const authority =
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthorityV1(
        deploymentManifest,
      );
    if (
      state === null ||
      authority === null ||
      state.deploymentIdentityDigest !==
        authority.deploymentIdentityDigest.toString("hex") ||
      state.stateQueuePolicyId !== authority.stateQueuePolicyId.toString("hex")
    ) {
      throw new Error(
        "Observer database store refused foreign or non-canonical state",
      );
    }
    const program = sql.withTransaction(
      Effect.gen(function* () {
        const txSql = yield* SqlClient.SqlClient;
        yield* txSql`
          DELETE FROM da_payload_terminal_outcomes
          WHERE deployment_identity_digest = ${authority.deploymentIdentityDigest}`;
        for (const transition of state.admitted) {
          yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransitionV1(
            transition,
            deploymentManifest,
          );
        }
        const rows = yield* txSql<{
          readonly deployment_identity_digest: Buffer;
        }>`
          INSERT INTO state_queue_terminal_observer_states (
            deployment_identity_digest,
            state_queue_policy_id,
            state_digest,
            state_record,
            updated_at
          ) VALUES (
            ${authority.deploymentIdentityDigest},
            ${authority.stateQueuePolicyId},
            ${Buffer.from(state.stateDigest, "hex")},
            ${JSON.stringify(state)},
            NOW()
          )
          ON CONFLICT (deployment_identity_digest) DO UPDATE SET
            state_queue_policy_id = EXCLUDED.state_queue_policy_id,
            state_digest = EXCLUDED.state_digest,
            state_record = EXCLUDED.state_record,
            updated_at = NOW()
          WHERE state_queue_terminal_observer_states.state_queue_policy_id = EXCLUDED.state_queue_policy_id
          RETURNING deployment_identity_digest`;
        if (rows.length !== 1) {
          return yield* Effect.fail(
            new Error("Observer database state conflicts with stored policy"),
          );
        }
      }),
    );
    await Effect.runPromise(
      program.pipe(Effect.provideService(SqlClient.SqlClient, sql)),
    );
  },
});

const sameQueue = (
  left: readonly StateQueueTransitionNodeV1[],
  right: readonly StateQueueTransitionNodeV1[],
): boolean =>
  left.length === right.length &&
  left.every(
    (node, index) =>
      node.headerHash === right[index]?.headerHash &&
      node.outRef === right[index]?.outRef,
  );

/**
 * Reconciles the durable cursor before admitting any action. `reinclude` and
 * `restoreAfterRollback` are idempotent database transactions; persisting after
 * them is therefore crash-safe (a retry can repeat the exact mutation).
 */
export const reconcileStateQueueCorrectionObserverV1 = async ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  requiredFinalityDepth,
  source,
  store,
  reinclude,
  restoreAfterRollback,
  persistTerminal,
  revokeTerminal,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly requiredFinalityDepth: bigint;
  readonly source: StateQueueCorrectionObserverSourceV1;
  readonly store: StateQueueCorrectionObserverStoreV1;
  readonly reinclude: (
    transition: StateQueueAuthenticatedTransitionV1,
  ) => Promise<void>;
  readonly restoreAfterRollback: (
    transition: StateQueueAuthenticatedTransitionV1,
  ) => Promise<void>;
  readonly persistTerminal?: (
    transition: StateQueueAuthenticatedTransitionV1,
  ) => Promise<void>;
  readonly revokeTerminal?: (
    transition: StateQueueAuthenticatedTransitionV1,
  ) => Promise<void>;
}): Promise<StateQueueCorrectionObserverResultV1> => {
  if (
    !HEX_32.test(deploymentIdentityDigest) ||
    !HEX_28.test(stateQueuePolicyId) ||
    requiredFinalityDepth <= 0n
  ) {
    throw new Error("Invalid state-queue correction observer authority");
  }
  const queue = await source.readQueue();
  if (parseQueue(queue) === null) {
    throw new Error(
      "State-queue correction observer refused a structurally invalid queue snapshot",
    );
  }
  const loaded = await store.load();
  if (loaded === null) {
    await store.save(
      makeState({
        schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_V1_SCHEMA_VERSION,
        deploymentIdentityDigest,
        stateQueuePolicyId,
        cursorQueue: queue,
        pending: [],
        admitted: [],
        retractedTransactionHashes: [],
        postFinalityRollbackIncidents: [],
      }),
    );
    return {
      status: "bootstrapped",
      admittedTransactionHashes: [],
      retractedTransactionHashes: [],
      postFinalityRollbackTransactionHashes: [],
    };
  }
  const state = parseStateQueueCorrectionObserverStateV1(loaded);
  if (
    state === null ||
    state.deploymentIdentityDigest !== deploymentIdentityDigest ||
    state.stateQueuePolicyId !== stateQueuePolicyId
  ) {
    throw new Error(
      "State-queue correction observer store is non-canonical or belongs to another deployment",
    );
  }

  let pending = [...state.pending];
  let admitted = [...state.admitted];
  const retracted = new Set(state.retractedTransactionHashes);
  const incidents = [...state.postFinalityRollbackIncidents];
  const admittedNow: string[] = [];
  const retractedNow: string[] = [];
  const incidentsNow: string[] = [];
  const persistTerminalTransition = persistTerminal ?? (async () => undefined);
  const revokeTerminalTransition = revokeTerminal ?? (async () => undefined);

  const depthByTransaction = new Map<string, bigint | null>();
  const depthOf = async (
    transition: StateQueueAuthenticatedTransitionV1,
  ): Promise<bigint | null> => {
    if (depthByTransaction.has(transition.transactionHash)) {
      return depthByTransaction.get(transition.transactionHash)!;
    }
    const depth = await source.canonicalDepth(transition);
    depthByTransaction.set(transition.transactionHash, depth);
    return depth;
  };
  let rollbackAnchor: readonly StateQueueTransitionNodeV1[] | null = null;
  const bindRollbackAnchor = (
    transition: StateQueueAuthenticatedTransitionV1,
  ): void => {
    if (
      rollbackAnchor !== null &&
      !sameQueue(rollbackAnchor, transition.previousQueue)
    ) {
      throw new Error(
        "State-queue correction observer found competing rollback anchors",
      );
    }
    rollbackAnchor = transition.previousQueue;
  };

  const pendingAfterRollback: StateQueueAuthenticatedTransitionV1[] = [];
  for (const transition of pending) {
    const depth = await depthOf(transition);
    if (depth === null) {
      if (sameQueue(queue, transition.nextQueue)) {
        throw new Error(
          "State-queue provider reports a terminal transaction absent while its exact post-state remains current",
        );
      }
      bindRollbackAnchor(transition);
      retracted.add(transition.transactionHash);
      retractedNow.push(transition.transactionHash);
    } else {
      pendingAfterRollback.push(transition);
    }
  }
  pending = pendingAfterRollback;

  const admittedAfterRollback: StateQueueAuthenticatedTransitionV1[] = [];
  for (const transition of admitted) {
    const depth = await depthOf(transition);
    if (depth === null) {
      if (sameQueue(queue, transition.nextQueue)) {
        throw new Error(
          "State-queue provider reports a finalized terminal transaction absent while its exact post-state remains current",
        );
      }
      bindRollbackAnchor(transition);
      await revokeTerminalTransition(transition);
      if (
        transition.transitionKind === "timeout_correction" ||
        transition.transitionKind === "fraud_removal"
      ) {
        await restoreAfterRollback(transition);
      }
      retracted.add(transition.transactionHash);
      retractedNow.push(transition.transactionHash);
      if (
        !incidents.some(
          ({ transactionHash }) =>
            transactionHash === transition.transactionHash,
        )
      ) {
        incidents.push({
          transactionHash: transition.transactionHash,
          transitionDigest: transition.transitionDigest,
        });
        incidentsNow.push(transition.transactionHash);
      }
    } else {
      admittedAfterRollback.push(transition);
    }
  }
  admitted = admittedAfterRollback;

  const replayAnchor = rollbackAnchor ?? state.cursorQueue;
  if (!sameQueue(replayAnchor, queue)) {
    const observed = await source.observeTransitions(replayAnchor, queue);
    const replay = replayStateQueueAuthenticatedCheckpointsV1({
      deploymentIdentityDigest,
      stateQueuePolicyId,
      minimumFinalityDepth: 1n,
      anchor: {
        queue: replayAnchor,
        blockNo: "0",
        transactionIndex: "0",
      },
      checkpoints: observed,
    });
    if (replay === null || !sameQueue(replay.queue, queue)) {
      throw new Error(
        "State-queue authenticated checkpoint replay does not reach the current queue",
      );
    }
    for (const candidate of replay.terminals) {
      if (
        !pending.some(
          ({ transactionHash }) =>
            transactionHash === candidate.transactionHash,
        ) &&
        !admitted.some(
          ({ transactionHash }) =>
            transactionHash === candidate.transactionHash,
        )
      ) {
        retracted.delete(candidate.transactionHash);
        pending.push(candidate);
      }
    }
  }

  const stillPending: StateQueueAuthenticatedTransitionV1[] = [];
  for (const transition of pending) {
    const depth = await depthOf(transition);
    if (depth === null) {
      stillPending.push(transition);
      continue;
    }
    if (depth < requiredFinalityDepth) {
      stillPending.push(transition);
      continue;
    }
    const finalized = withStateQueueAuthenticatedTransitionFinalityDepthV1(
      transition,
      depth.toString(),
    );
    if (finalized === null) {
      throw new Error("Failed to bind authenticated correction finality depth");
    }
    if (
      finalized.transitionKind === "timeout_correction" ||
      finalized.transitionKind === "fraud_removal"
    ) {
      await reinclude(finalized);
    }
    await persistTerminalTransition(finalized);
    admitted.push(finalized);
    admittedNow.push(finalized.transactionHash);
  }
  pending = stillPending;

  await store.save(
    makeState({
      schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_V1_SCHEMA_VERSION,
      deploymentIdentityDigest,
      stateQueuePolicyId,
      cursorQueue: queue,
      pending,
      admitted,
      retractedTransactionHashes: [...retracted].sort(),
      postFinalityRollbackIncidents: incidents,
    }),
  );
  return {
    status: "reconciled",
    admittedTransactionHashes: admittedNow,
    retractedTransactionHashes: [...new Set(retractedNow)],
    postFinalityRollbackTransactionHashes: incidentsNow,
  };
};

const outRef = (label: string): { txHash: string; outputIndex: number } => {
  const match = OUT_REF.exec(label);
  if (match === null) throw new Error(`Invalid output reference ${label}`);
  const [txHash, index] = label.split("#") as [string, string];
  return { txHash, outputIndex: Number(index) };
};

const fetchTip = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
): Promise<TipV1> => {
  const response = await fetchImpl(normalizeOgmiosHttpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryNetwork/tip",
      params: {},
      id: "midgard-state-queue-correction-tip-v1",
    }),
  });
  const body = (await response.json()) as {
    result?: {
      id?: unknown;
      slot?: unknown;
      height?: unknown;
      tip?: { id?: unknown; slot?: unknown; height?: unknown };
    };
  };
  if (!response.ok) throw new Error("Ogmios tip query failed");
  const point = body.result?.tip ?? body.result;
  if (
    typeof point?.id !== "string" ||
    !HEX_32.test(point.id) ||
    typeof point.slot !== "number" ||
    !Number.isSafeInteger(point.slot) ||
    point.slot < 0 ||
    typeof point.height !== "number" ||
    !Number.isSafeInteger(point.height) ||
    point.height < 0
  ) {
    throw new Error("Ogmios tip query returned no canonical point");
  }
  return { blockHash: point.id, slot: point.slot, blockNo: point.height };
};

const sameSpend = (left: KupoSpendV1, right: KupoSpendV1): boolean =>
  left.transactionId === right.transactionId &&
  left.point.headerHash === right.point.headerHash &&
  left.point.slot === right.point.slot;

type HistoricalQueueOutputV1 = Readonly<{
  node: StateQueueTransitionNodeV1;
  nextHeaderHash: string | null;
}>;

type HistoricalCorrectionLockOutputV1 = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
}>;

const decodeKupoCorrectionLockMatchV1 = ({
  candidate,
  expectedTransactionHash,
  expectedOutputIndex,
  correctionLockAddress,
  hubOraclePolicyId,
}: {
  readonly candidate: unknown;
  readonly expectedTransactionHash: string;
  readonly expectedOutputIndex: number;
  readonly correctionLockAddress: string;
  readonly hubOraclePolicyId: string;
}): HistoricalCorrectionLockOutputV1 | null => {
  const match = candidate as {
    transaction_id?: unknown;
    output_index?: unknown;
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    match.transaction_id !== expectedTransactionHash ||
    match.output_index !== expectedOutputIndex ||
    typeof match.value !== "object" ||
    match.value === null ||
    typeof match.value.assets !== "object" ||
    match.value.assets === null ||
    Array.isArray(match.value.assets)
  ) {
    return null;
  }
  const nativeAssets = Object.entries(
    match.value.assets as Record<string, unknown>,
  ).map(
    ([rawUnit, quantity]) => [rawUnit.replaceAll(".", ""), quantity] as const,
  );
  const lockUnit = SDK.correctionLockUnit(hubOraclePolicyId);
  if (
    match.address !== correctionLockAddress ||
    nativeAssets.length !== 1 ||
    nativeAssets[0]?.[0] !== lockUnit ||
    (nativeAssets[0]?.[1] !== 1 && nativeAssets[0]?.[1] !== "1") ||
    match.datum_type !== "inline" ||
    typeof match.datum !== "string"
  ) {
    return null;
  }
  try {
    return {
      outRef: `${expectedTransactionHash}#${expectedOutputIndex.toString()}`,
      datum: Data.from(match.datum, SDK.CorrectionLockDatum),
    };
  } catch {
    return null;
  }
};

const fetchKupoResolvedOutputV1 = async ({
  kupoUrl,
  reference,
  fetchImpl,
}: {
  readonly kupoUrl: string;
  readonly reference: { readonly txHash: string; readonly outputIndex: number };
  readonly fetchImpl: FetchLike;
}): Promise<unknown> => {
  const url = `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}/matches/${reference.outputIndex.toString()}@${reference.txHash}?resolve_hashes`;
  const response = await fetchImpl(url);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Kupo resolved-output query failed with HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  let decoded: unknown;
  try {
    decoded = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("Kupo resolved-output query returned malformed JSON", {
      cause,
    });
  }
  if (!Array.isArray(decoded)) {
    throw new Error("Kupo resolved-output query did not return an array");
  }
  const matches = decoded.filter(
    (candidate) =>
      (candidate as { transaction_id?: unknown }).transaction_id ===
        reference.txHash &&
      (candidate as { output_index?: unknown }).output_index ===
        reference.outputIndex,
  );
  if (matches.length !== 1) {
    throw new Error(
      "Kupo resolved-output query did not return one exact match",
    );
  }
  return matches[0];
};

const fetchKupoTransactionCorrectionLockOutputsV1 = async ({
  kupoUrl,
  transactionHash,
  correctionLockAddress,
  hubOraclePolicyId,
  fetchImpl,
}: {
  readonly kupoUrl: string;
  readonly transactionHash: string;
  readonly correctionLockAddress: string;
  readonly hubOraclePolicyId: string;
  readonly fetchImpl: FetchLike;
}): Promise<readonly HistoricalCorrectionLockOutputV1[]> => {
  const url = `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}/matches/*@${transactionHash}?resolve_hashes`;
  const response = await fetchImpl(url);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Kupo correction-lock output query failed with HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  let decoded: unknown;
  try {
    decoded = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error(
      "Kupo correction-lock output query returned malformed JSON",
      { cause },
    );
  }
  if (!Array.isArray(decoded)) {
    throw new Error(
      "Kupo correction-lock output query did not return an array",
    );
  }
  return decoded.flatMap((candidate) => {
    const index = (candidate as { output_index?: unknown }).output_index;
    if (
      typeof index !== "number" ||
      !Number.isSafeInteger(index) ||
      index < 0
    ) {
      return [];
    }
    const lock = decodeKupoCorrectionLockMatchV1({
      candidate,
      expectedTransactionHash: transactionHash,
      expectedOutputIndex: index,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return lock === null ? [] : [lock];
  });
};

const fraudProofAssetNameFromResolvedMatchV1 = ({
  candidate,
  fraudProofAddress,
  fraudProofPolicyId,
  targetHeaderHash,
}: {
  readonly candidate: unknown;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly targetHeaderHash: string;
}): string | null => {
  const match = candidate as {
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    match.address !== fraudProofAddress ||
    match.datum_type !== "inline" ||
    typeof match.datum !== "string" ||
    typeof match.value !== "object" ||
    match.value === null ||
    typeof match.value.assets !== "object" ||
    match.value.assets === null ||
    Array.isArray(match.value.assets)
  ) {
    return null;
  }
  const proofAssets = Object.entries(
    match.value.assets as Record<string, unknown>,
  ).flatMap(([rawUnit, quantity]) => {
    const unit = rawUnit.replaceAll(".", "");
    const assetName = unit.startsWith(fraudProofPolicyId)
      ? unit.slice(fraudProofPolicyId.length)
      : null;
    return assetName !== null &&
      /^[0-9a-f]{64}$/u.test(assetName) &&
      assetName.slice(8) === targetHeaderHash &&
      (quantity === 1 || quantity === "1")
      ? [assetName]
      : unit.startsWith(fraudProofPolicyId)
        ? [null]
        : [];
  });
  return proofAssets.length === 1 ? proofAssets[0] : null;
};

const deriveCorrectionLockWitnessFromRawV1 = async ({
  transaction,
  transactionOutputs,
  stateQueuePolicyId,
  correctionLockAddress,
  hubOraclePolicyId,
  fraudProofAddress,
  fraudProofPolicyId,
  kupoUrl,
  fetchImpl,
}: {
  readonly transaction: ObservedL1TransactionAtPointV1;
  readonly transactionOutputs: readonly HistoricalCorrectionLockOutputV1[];
  readonly stateQueuePolicyId: string;
  readonly correctionLockAddress: string;
  readonly hubOraclePolicyId: string;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly kupoUrl: string;
  readonly fetchImpl: FetchLike;
}): Promise<SDK.StateQueueCorrectionLockWitnessV1> => {
  const spentReferences = transaction.spentInputs ?? [];
  const spentResolved = await Promise.all(
    spentReferences.map(async (reference) => ({
      reference,
      match: await fetchKupoResolvedOutputV1({
        kupoUrl,
        reference,
        fetchImpl,
      }),
    })),
  );
  const referenceResolved = await Promise.all(
    transaction.referenceInputs.map(async (reference) => ({
      reference,
      match: await fetchKupoResolvedOutputV1({
        kupoUrl,
        reference,
        fetchImpl,
      }),
    })),
  );
  const locksIn = spentResolved.flatMap(({ reference, match }) => {
    const lock = decodeKupoCorrectionLockMatchV1({
      candidate: match,
      expectedTransactionHash: reference.txHash,
      expectedOutputIndex: reference.outputIndex,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return lock === null ? [] : [lock];
  });
  const locksReferenced = referenceResolved.flatMap(({ reference, match }) => {
    const lock = decodeKupoCorrectionLockMatchV1({
      candidate: match,
      expectedTransactionHash: reference.txHash,
      expectedOutputIndex: reference.outputIndex,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return lock === null ? [] : [lock];
  });
  const stateQueuePolicyIndex =
    transaction.mintPolicyIds.indexOf(stateQueuePolicyId);
  const mintRedeemers = transaction.redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === stateQueuePolicyIndex,
  );
  if (stateQueuePolicyIndex < 0) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      transactionOutputs.length !== 0
    ) {
      throw new Error(
        "Non-mint state-queue transition unexpectedly mutates or references CorrectionLock",
      );
    }
    return { kind: "none" };
  }
  if (mintRedeemers.length !== 1) {
    throw new Error(
      "State-queue transaction has no unique canonical mint redeemer",
    );
  }
  let decoded: SDK.StateQueueRedeemer;
  try {
    decoded = Data.from(mintRedeemers[0]!.redeemer, SDK.StateQueueRedeemer);
  } catch (cause) {
    throw new Error("State-queue mint redeemer is not canonical data", {
      cause,
    });
  }
  if (typeof decoded === "object" && decoded !== null && "InitV1" in decoded) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      transactionOutputs.length !== 1 ||
      transactionOutputs[0]!.datum !== "Idle"
    ) {
      throw new Error("State-queue Init has invalid CorrectionLock topology");
    }
    return {
      kind: "genesis",
      producedOutRef: transactionOutputs[0]!.outRef,
      nextDatum: transactionOutputs[0]!.datum,
    };
  }
  if (decoded === "Deinit") {
    if (
      locksIn.length !== 1 ||
      locksIn[0]!.datum !== "Idle" ||
      locksReferenced.length !== 0 ||
      transactionOutputs.length !== 0
    ) {
      throw new Error("State-queue Deinit has invalid CorrectionLock topology");
    }
    return {
      kind: "deinit",
      consumedOutRef: locksIn[0]!.outRef,
      previousDatum: locksIn[0]!.datum,
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("CommitBlockHeader" in decoded || "MergeToConfirmedStateV1" in decoded)
  ) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 1 ||
      locksReferenced[0]!.datum !== "Idle" ||
      transactionOutputs.length !== 0
    ) {
      throw new Error(
        "State-queue append/merge has invalid CorrectionLock topology",
      );
    }
    return {
      kind: "idle_reference",
      referenceOutRef: locksReferenced[0]!.outRef,
      datum: locksReferenced[0]!.datum,
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveFraudulentBlockHeader" in decoded)
  ) {
    if (
      locksIn.length !== 1 ||
      transactionOutputs.length !== 1 ||
      locksReferenced.length !== 0
    ) {
      throw new Error("Correction has invalid CorrectionLock topology");
    }
    const targetHeaderHash =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? decoded.RemoveUnattestedBlockAfterTimeout.timed_out_header_hash
        : decoded.RemoveFraudulentBlockHeader.fraudulent_blocks_header_hash;
    const correctionIdentity: SDK.CorrectionIdentity =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? "AttestationTimeout"
        : (() => {
            const proofIndex = Number(
              decoded.RemoveFraudulentBlockHeader.fraud_proof_ref_input_index,
            );
            const proof = referenceResolved[proofIndex];
            if (proof === undefined) {
              throw new Error(
                "Fraud correction proof reference index is out of bounds",
              );
            }
            const assetName = fraudProofAssetNameFromResolvedMatchV1({
              candidate: proof.match,
              fraudProofAddress,
              fraudProofPolicyId,
              targetHeaderHash,
            });
            if (assetName === null) {
              throw new Error(
                "Fraud correction proof reference is not the exact permanent proof identity",
              );
            }
            return {
              FraudProof: { fraud_proof_asset_name: assetName },
            };
          })();
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: transactionOutputs[0]!.outRef,
      targetHeaderHash,
      correctionIdentity,
      previousDatum: locksIn[0]!.datum,
      nextDatum: transactionOutputs[0]!.datum,
    };
  }
  throw new Error("State-queue mint redeemer has no CorrectionLock topology");
};

const fetchKupoTransactionQueueOutputsV1 = async ({
  kupoUrl,
  transactionHash,
  stateQueueAddress,
  stateQueuePolicyId,
  fetchImpl,
}: {
  readonly kupoUrl: string;
  readonly transactionHash: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly fetchImpl: FetchLike;
}): Promise<readonly HistoricalQueueOutputV1[]> => {
  const url = `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}/matches/*@${transactionHash}?resolve_hashes`;
  const response = await fetchImpl(url);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Kupo transaction-output query failed with HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  let decodedBody: unknown;
  try {
    decodedBody = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("Kupo transaction-output query returned malformed JSON", {
      cause,
    });
  }
  if (!Array.isArray(decodedBody)) {
    throw new Error("Kupo transaction-output query did not return an array");
  }
  const queueOutputs: HistoricalQueueOutputV1[] = [];
  for (const [matchIndex, candidate] of decodedBody.entries()) {
    const match = candidate as {
      transaction_id?: unknown;
      output_index?: unknown;
      address?: unknown;
      datum_type?: unknown;
      datum?: unknown;
      value?: { assets?: unknown };
    };
    if (
      match.transaction_id !== transactionHash ||
      typeof match.output_index !== "number" ||
      !Number.isSafeInteger(match.output_index) ||
      match.output_index < 0 ||
      typeof match.address !== "string" ||
      typeof match.value !== "object" ||
      match.value === null ||
      typeof match.value.assets !== "object" ||
      match.value.assets === null ||
      Array.isArray(match.value.assets)
    ) {
      throw new Error(
        `Kupo transaction output ${matchIndex.toString()} is non-canonical`,
      );
    }
    const stateQueueAssets = Object.entries(
      match.value.assets as Record<string, unknown>,
    ).flatMap(([rawUnit, quantity]) => {
      const unit = rawUnit.replaceAll(".", "");
      return unit.startsWith(stateQueuePolicyId) &&
        (quantity === 1 || quantity === "1")
        ? [unit.slice(stateQueuePolicyId.length)]
        : unit.startsWith(stateQueuePolicyId)
          ? [null]
          : [];
    });
    if (stateQueueAssets.length === 0) continue;
    if (
      match.address !== stateQueueAddress ||
      stateQueueAssets.length !== 1 ||
      stateQueueAssets[0] === null ||
      match.datum_type !== "inline" ||
      typeof match.datum !== "string"
    ) {
      throw new Error(
        "State-queue policy output has a foreign address, quantity, or non-inline datum",
      );
    }
    const assetName = stateQueueAssets[0];
    const headerHash =
      assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME
        ? null
        : assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
            HEX_28.test(
              assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
            )
          ? assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length)
          : undefined;
    if (headerHash === undefined) {
      throw new Error(
        "State-queue policy output carries an unknown asset name",
      );
    }
    let view: SDK.LinkedListNodeView;
    try {
      view = SDK.linkedListDatumToNodeView(
        Data.from(match.datum, SDK.LinkedListDatum),
        assetName,
      );
    } catch (cause) {
      throw new Error("State-queue output carries an invalid inline datum", {
        cause,
      });
    }
    const viewHeaderHash = view.key === "Empty" ? null : view.key.Key.key;
    if (viewHeaderHash !== headerHash) {
      throw new Error(
        "State-queue output asset identity disagrees with its datum",
      );
    }
    queueOutputs.push({
      node: {
        headerHash,
        outRef: `${transactionHash}#${match.output_index.toString()}`,
      },
      nextHeaderHash: view.next === "Empty" ? null : view.next.Key.key,
    });
  }
  if (
    new Set(queueOutputs.map(({ node }) => node.headerHash)).size !==
      queueOutputs.length ||
    new Set(queueOutputs.map(({ node }) => node.outRef)).size !==
      queueOutputs.length
  ) {
    throw new Error("Kupo returned duplicate state-queue transaction outputs");
  }
  return queueOutputs;
};

const reconstructQueueAfterTransactionV1 = ({
  previousQueue,
  transactionHash,
  spentInputOutRefs,
  outputs,
}: {
  readonly previousQueue: readonly StateQueueTransitionNodeV1[];
  readonly transactionHash: string;
  readonly spentInputOutRefs: readonly string[];
  readonly outputs: readonly HistoricalQueueOutputV1[];
}): readonly StateQueueTransitionNodeV1[] => {
  const spent = new Set(spentInputOutRefs);
  const previousIdentities = new Set(
    previousQueue.map(({ headerHash }) => headerHash),
  );
  const outputByIdentity = new Map(
    outputs.map(({ node }) => [node.headerHash, node]),
  );
  if (
    outputs.length === 0 ||
    !previousQueue.some(({ outRef }) => spent.has(outRef)) ||
    outputs.some(
      ({ node }) =>
        previousIdentities.has(node.headerHash) &&
        !previousQueue.some(
          (prior) =>
            prior.headerHash === node.headerHash && spent.has(prior.outRef),
        ),
    )
  ) {
    throw new Error("State-queue transaction outputs do not follow its inputs");
  }
  const retained = previousQueue.flatMap((node) => {
    if (!spent.has(node.outRef)) return [node];
    const continuation = outputByIdentity.get(node.headerHash);
    return continuation === undefined ? [] : [continuation];
  });
  const introduced = outputs
    .map(({ node }) => node)
    .filter(({ headerHash }) => !previousIdentities.has(headerHash));
  if (
    introduced.length > 1 ||
    introduced.some(({ headerHash }) => headerHash === null)
  ) {
    throw new Error(
      "State-queue transaction introduced a non-canonical identity set",
    );
  }
  const nextQueue = [...retained, ...introduced];
  if (parseQueue(nextQueue) === null) {
    throw new Error("State-queue transaction reconstructed an invalid queue");
  }
  const expectedNextByIdentity = new Map(
    nextQueue.map((node, index) => [
      node.headerHash,
      nextQueue[index + 1]?.headerHash ?? null,
    ]),
  );
  if (
    outputs.some(
      ({ node, nextHeaderHash }) =>
        expectedNextByIdentity.get(node.headerHash) !== nextHeaderHash,
    )
  ) {
    throw new Error(
      "State-queue output links disagree with reconstructed order",
    );
  }
  if (
    nextQueue.every(({ outRef }) => !outRef.startsWith(`${transactionHash}#`))
  ) {
    throw new Error(
      "State-queue transaction has no authenticated continuation",
    );
  }
  return nextQueue;
};

/** Node-owned local Kupmios/Ogmios source; no watcher process is consulted. */
export const makeLocalKupmiosStateQueueCorrectionSourceV1 = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  stateQueueAddress,
  hubOraclePolicyId,
  correctionLockAddress,
  fraudProofPolicyId,
  fraudProofAddress,
  kupoUrl,
  ogmiosUrl,
  readQueue,
  fetchImpl = fetch,
  webSocketFactory,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly readQueue: () => Promise<readonly StateQueueTransitionNodeV1[]>;
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
}): StateQueueCorrectionObserverSourceV1 => {
  const canonicalDepth = async (
    transition: StateQueueAuthenticatedTransitionV1,
  ): Promise<bigint | null> => {
    const spends = await Promise.all(
      transition.consumedQueueOutRefs.map((label) =>
        fetchKupoSpendV1({ kupoUrl, outRef: outRef(label), fetchImpl }),
      ),
    );
    if (
      spends.some(
        (spend) =>
          spend === null ||
          spend.transactionId !== transition.transactionHash ||
          spend.point.headerHash !== transition.blockHash,
      )
    ) {
      return null;
    }
    const tip = await fetchTip(ogmiosUrl, fetchImpl);
    const blockNo = BigInt(transition.blockNo);
    return BigInt(tip.blockNo) < blockNo
      ? null
      : BigInt(tip.blockNo) - blockNo + 1n;
  };
  return {
    readQueue,
    canonicalDepth,
    observeTransitions: async (previousQueue, nextQueue) => {
      let workingQueue = previousQueue;
      const observations: StateQueueAuthenticatedReplayCheckpointV1[] = [];
      const tip = await fetchTip(ogmiosUrl, fetchImpl);
      for (let replayed = 0; replayed < 1_000; replayed += 1) {
        if (sameQueue(workingQueue, nextQueue)) return observations;
        const spends = await Promise.all(
          workingQueue.map(({ outRef: label }) =>
            fetchKupoSpendV1({ kupoUrl, outRef: outRef(label), fetchImpl }),
          ),
        );
        const uniqueSpends = new Map<string, KupoSpendV1>();
        for (const spend of spends) {
          if (spend === null) continue;
          const prior = uniqueSpends.get(spend.transactionId);
          if (prior !== undefined && !sameSpend(prior, spend)) {
            throw new Error(
              "State-queue Kupo history attached one transaction to competing chain points",
            );
          }
          uniqueSpends.set(spend.transactionId, spend);
        }
        if (uniqueSpends.size === 0) {
          throw new Error(
            "State-queue ordered replay cannot advance from its durable cursor",
          );
        }
        const transactions = await Promise.all(
          [...uniqueSpends.values()].map(async (spend) => {
            const ancestor = await fetchKupoAncestorPointV1({
              kupoUrl,
              slot: spend.point.slot,
              fetchImpl,
            });
            const transaction = await readOgmiosBlockTransactionV1({
              ogmiosUrl,
              intersection: ancestor,
              blockPoint: spend.point,
              txHash: spend.transactionId,
              webSocketFactory,
            });
            return { spend, transaction };
          }),
        );
        transactions.sort(
          (left, right) =>
            left.transaction.blockPoint.blockNo -
              right.transaction.blockPoint.blockNo ||
            left.transaction.transactionIndex -
              right.transaction.transactionIndex ||
            left.transaction.txHash.localeCompare(right.transaction.txHash),
        );
        const { spend, transaction } = transactions[0]!;
        if (
          transaction.blockPoint.headerHash !== spend.point.headerHash ||
          transaction.blockPoint.slot !== spend.point.slot
        ) {
          throw new Error("Kupo/Ogmios state-queue chain points disagree");
        }
        const spentInputOutRefs = (transaction.spentInputs ?? []).map(
          ({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`,
        );
        const historicalOutputs = await fetchKupoTransactionQueueOutputsV1({
          kupoUrl,
          transactionHash: transaction.txHash,
          stateQueueAddress,
          stateQueuePolicyId,
          fetchImpl,
        });
        const correctionLockOutputs =
          await fetchKupoTransactionCorrectionLockOutputsV1({
            kupoUrl,
            transactionHash: transaction.txHash,
            correctionLockAddress,
            hubOraclePolicyId,
            fetchImpl,
          });
        const intermediateQueue = reconstructQueueAfterTransactionV1({
          previousQueue: workingQueue,
          transactionHash: transaction.txHash,
          spentInputOutRefs,
          outputs: historicalOutputs,
        });
        if (tip.blockNo < transaction.blockPoint.blockNo) {
          throw new Error(
            "Ogmios tip precedes an authenticated state-queue transaction",
          );
        }
        const observedDepth = tip.blockNo - transaction.blockPoint.blockNo + 1;
        const localChainPointId = digest({
          source: "midgard-node-local-kupmios-ordered-v1",
          blockHash: transaction.blockPoint.headerHash,
          slot: transaction.blockPoint.slot,
          blockNo: transaction.blockPoint.blockNo,
          transactionIndex: transaction.transactionIndex,
        });
        const correctionLockWitness =
          await deriveCorrectionLockWitnessFromRawV1({
            transaction,
            transactionOutputs: correctionLockOutputs,
            stateQueuePolicyId,
            correctionLockAddress,
            hubOraclePolicyId,
            fraudProofPolicyId,
            fraudProofAddress,
            kupoUrl,
            fetchImpl,
          });
        const transitionInput = {
          deploymentIdentityDigest,
          stateQueuePolicyId,
          transactionHash: transaction.txHash,
          blockHash: transaction.blockPoint.headerHash,
          slot: transaction.blockPoint.slot.toString(),
          blockNo: transaction.blockPoint.blockNo.toString(),
          transactionIndex: transaction.transactionIndex.toString(),
          chainPointId: localChainPointId,
          finalityDepth: observedDepth.toString(),
          mintPolicyIds: transaction.mintPolicyIds,
          redeemers: transaction.redeemers.map((redeemer) => ({
            purpose: redeemer.purpose,
            index: redeemer.index.toString(),
            cborHex: redeemer.redeemer,
          })),
          spentInputOutRefs,
          referenceInputOutRefs: transaction.referenceInputs.map(
            ({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`,
          ),
          correctionLockWitness,
          previousQueue: workingQueue,
          nextQueue: intermediateQueue,
        } as const;
        const checkpoint =
          deriveStateQueueAuthenticatedReplayCheckpointV1(transitionInput);
        if (checkpoint === null) {
          throw new Error(
            "State-queue transaction failed exact authenticated checkpoint derivation",
          );
        }
        observations.push(checkpoint);
        workingQueue = intermediateQueue;
      }
      throw new Error(
        "State-queue ordered replay exceeded its 1000-transition safety bound",
      );
    },
  };
};
