import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { JOURNEY_HEADER_END_AHEAD_MS } from "./fixture.js";
import { httpFailureDetail } from "./http-failure.js";

/**
 * Our node's replay hold on a landed block that does not replay to its
 * header or does not link to its parent (`landed-blocks/holds.ts`).
 */
export const LANDED_BLOCK_INVALID = "landed_block_invalid";
/** Our node is still waiting for a foreign block's DA payload. */
export const LANDED_BLOCK_AWAITING_DA = "landed_block_awaiting_da";
/** Our node's commit fiber has commit work but no block to build it on. */
export const COMMIT_BASE_PENDING = "commit_base_pending";

const HEADER_HASH = /^[0-9a-f]{56}$/u;
const TX_HASH = /^[0-9a-f]{64}$/u;
/** How landed-block processing names the block an invalid hold is on. */
const HELD_BLOCK = /^block ([0-9a-f]{56})\b/u;

/**
 * What our node's `/readyz` says about the fault while it is the queue's tail.
 * - `held`: it refuses to build on the fault (`landed_block_invalid`).
 * - `awaiting_da`: it has not judged the fault yet; its payload is missing.
 * - `not_held`: neither.
 *
 * `binding` is `header` when the hold's detail names the fault's header hash,
 * and `reason_only` when the node does not name the block: a production
 * profile redacts every readiness detail, and a replay verdict's detail need
 * not name the block. A hold whose detail names another block is refused.
 */
export type NodeFaultHold = Readonly<{
  state: "held" | "awaiting_da" | "not_held";
  binding: "header" | "reason_only";
  reasons: readonly string[];
}>;

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

/** Classifies one `/readyz` body of our node against the fault header. */
export const classifyNodeFaultHold = (
  readyz: unknown,
  faultHeaderHash: string,
): NodeFaultHold => {
  if (!HEADER_HASH.test(faultHeaderHash))
    throw new Error(`Malformed fault header hash ${faultHeaderHash}`);
  const reasons = isRecord(readyz) ? readyz["reasons"] : undefined;
  if (
    !Array.isArray(reasons) ||
    !reasons.every((reason) => typeof reason === "string")
  )
    throw new Error("Our node's /readyz carries no readiness reasons");
  const follower = isRecord(readyz) ? readyz["l1Follower"] : undefined;
  const entries =
    isRecord(follower) && Array.isArray(follower["readiness"])
      ? (follower["readiness"] as unknown[]).filter(isRecord)
      : [];
  const named = (reason: string) => {
    const blocks = entries
      .filter((entry) => entry["reason"] === reason)
      .map((entry) =>
        typeof entry["detail"] === "string"
          ? HELD_BLOCK.exec(entry["detail"])?.[1]
          : undefined,
      )
      .filter((hash): hash is string => hash !== undefined);
    const other = blocks.find((hash) => hash !== faultHeaderHash);
    if (other !== undefined)
      throw new Error(
        `Our node holds ${reason} on block ${other}, not on the fault ${faultHeaderHash}`,
      );
    return blocks.length > 0 ? "header" : "reason_only";
  };
  if (reasons.includes(LANDED_BLOCK_INVALID))
    return { state: "held", binding: named(LANDED_BLOCK_INVALID), reasons };
  if (reasons.includes(LANDED_BLOCK_AWAITING_DA))
    return {
      state: "awaiting_da",
      binding: named(LANDED_BLOCK_AWAITING_DA),
      reasons,
    };
  return { state: "not_held", binding: "reason_only", reasons };
};

/** One state-queue header Kupo indexed, spent or not. */
export type IndexedStateQueueHeader = Readonly<{
  headerHash: string;
  header: SDK.Header;
  /** The node's block time (POSIX ms), its DA attestation timeout's start. */
  blockTimeMs: number;
  /** Whether the node carries a DA attestation (any status but unattested). */
  attested: boolean;
  /** The transaction that created this output. */
  txHash: string;
  outputIndex: number;
  createdAtSlot: number;
  /** When it was spent (merged or removed), if it was. */
  spentAtSlot?: number;
  /** The transaction that spent it, where Kupo names it. */
  spentByTxHash?: string;
}>;

type KupoMatch = Readonly<{
  transaction_id: string;
  output_index: number;
  address: string;
  value: { coins: number | string; assets?: Record<string, number | string> };
  datum?: string | null;
  datum_type?: string | null;
  created_at: { slot_no: number };
  spent_at?: { slot_no: number; transaction_id?: unknown } | null;
}>;

const readKupoMatches = async (
  kupoUrl: string,
  path: string,
  fetchImpl: typeof fetch,
): Promise<KupoMatch[]> => {
  const response = await fetchImpl(`${kupoUrl.replace(/\/+$/u, "")}${path}`, {
    signal: AbortSignal.timeout(30_000),
  });
  if (!response.ok)
    throw new Error(
      `Kupo ${path} failed with ${response.status.toString()}${await httpFailureDetail(response)}`,
    );
  const matches: unknown = await response.json();
  if (
    !Array.isArray(matches) ||
    !matches.every(
      (match) =>
        isRecord(match) &&
        typeof match["transaction_id"] === "string" &&
        Number.isSafeInteger(match["output_index"]) &&
        typeof match["address"] === "string" &&
        isRecord(match["value"]) &&
        isRecord(match["created_at"]) &&
        Number.isSafeInteger(match["created_at"]["slot_no"]),
    )
  )
    throw new Error(`Kupo ${path} answered malformed matches`);
  return matches as KupoMatch[];
};

const headerOfMatch = async (
  match: KupoMatch,
  policyId: string,
): Promise<IndexedStateQueueHeader | undefined> => {
  const nodePrefix = `${policyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}`;
  const assets: Record<string, bigint> = {
    lovelace: BigInt(match.value.coins),
  };
  for (const [unit, quantity] of Object.entries(match.value.assets ?? {}))
    assets[unit.replace(".", "")] = BigInt(quantity);
  const unit = Object.keys(assets).find((name) => name.startsWith(nodePrefix));
  // The root and other policy outputs carry no header.
  if (unit === undefined) return undefined;
  if (match.datum_type !== "inline" || typeof match.datum !== "string")
    throw new Error(
      `State-queue output ${match.transaction_id}#${match.output_index.toString()} has no inline datum in Kupo`,
    );
  const utxo: UTxO = {
    txHash: match.transaction_id,
    outputIndex: match.output_index,
    address: match.address,
    assets,
    datum: match.datum,
  };
  const node = await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo));
  const headerHash = unit.slice(nodePrefix.length);
  if (node.key === "Empty" || node.key.Key.key !== headerHash)
    throw new Error(
      `State-queue output ${match.transaction_id}#${match.output_index.toString()} keys another header than its token`,
    );
  const queueNode = await Effect.runPromise(
    SDK.getStateQueueNodeFromStateQueueDatum({ data: node.data as never }),
  );
  return {
    headerHash,
    header: queueNode.header,
    blockTimeMs: Number(queueNode.block_time),
    attested: queueNode.da_attestation !== SDK.NO_DA_ATTESTATION,
    txHash: match.transaction_id,
    outputIndex: match.output_index,
    createdAtSlot: match.created_at.slot_no,
    ...(match.spent_at == null
      ? {}
      : {
          spentAtSlot: match.spent_at.slot_no,
          ...(typeof match.spent_at.transaction_id === "string" &&
          TX_HASH.test(match.spent_at.transaction_id)
            ? { spentByTxHash: match.spent_at.transaction_id }
            : {}),
        }),
  };
};

/**
 * Every state-queue header output Kupo indexed for `headerHash`, spent or
 * not, oldest first. The stack's Kupo keeps spent outputs (no
 * `--prune-utxo`), so a header that was later merged or removed is still
 * read.
 */
export const readIndexedStateQueueHeader = async (
  input: Readonly<{ kupoUrl: string; policyId: string; headerHash: string }>,
  fetchImpl: typeof fetch = fetch,
): Promise<IndexedStateQueueHeader[]> => {
  const matches = await readKupoMatches(
    input.kupoUrl,
    `/matches/${input.policyId}.${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${input.headerHash}?resolve_hashes`,
    fetchImpl,
  );
  const headers: IndexedStateQueueHeader[] = [];
  for (const match of matches) {
    const header = await headerOfMatch(match, input.policyId);
    if (header !== undefined) headers.push(header);
  }
  return headers.sort((a, b) => a.createdAtSlot - b.createdAtSlot);
};

/**
 * The watcher's removal of the fault: the spend of whichever of its outputs
 * the removal transaction spent, named by that transaction and never by the
 * output's position. An attested fault's commit output is spent by its DA
 * apply, and the removal spends the attested output after it. The caller
 * passes the removal the watcher's correction authenticated. Null while
 * Kupo shows no output of the fault spent by it.
 */
export const faultRemovalOf = (
  outputs: readonly IndexedStateQueueHeader[],
  removalTxHash: string,
): Readonly<{ txHash: string; slot: number }> | null => {
  const removed = outputs.find(
    ({ spentByTxHash }) => spentByTxHash === removalTxHash,
  );
  return removed?.spentAtSlot === undefined
    ? null
    : { txHash: removalTxHash, slot: removed.spentAtSlot };
};

/**
 * Observable 2: across the whole window from the fault's commit, no header
 * of our operator on L1 names the fault as its parent. It reads every
 * state-queue output created from the fault's commit slot on, spent ones
 * included, so a header that linked to the fault and was later removed
 * still counts. Returns how many headers it read after the fault.
 */
export const assertNoHonestHeaderOnFault = async (
  input: Readonly<{
    kupoUrl: string;
    policyId: string;
    faultHeaderHash: string;
    honestOperatorKeyHash: string;
  }>,
  fetchImpl: typeof fetch = fetch,
): Promise<{ faultCommittedAtSlot: number; headersRead: number }> => {
  const [fault] = await readIndexedStateQueueHeader(
    {
      kupoUrl: input.kupoUrl,
      policyId: input.policyId,
      headerHash: input.faultHeaderHash,
    },
    fetchImpl,
  );
  if (fault === undefined)
    throw new Error(
      `Kupo indexes no state-queue output of the fault ${input.faultHeaderHash}`,
    );
  const matches = await readKupoMatches(
    input.kupoUrl,
    `/matches/${input.policyId}.*?resolve_hashes&created_after=${Math.max(fault.createdAtSlot - 1, 0).toString()}`,
    fetchImpl,
  );
  let headersRead = 0;
  for (const match of matches) {
    const indexed = await headerOfMatch(match, input.policyId);
    if (indexed === undefined || indexed.headerHash === input.faultHeaderHash)
      continue;
    headersRead += 1;
    if (
      indexed.header.prevHeaderHash === input.faultHeaderHash &&
      indexed.header.operatorVkey === input.honestOperatorKeyHash
    )
      throw new Error(
        `Our node built header ${indexed.headerHash} on the fault ${input.faultHeaderHash} (commit ${indexed.txHash})`,
      );
  }
  return { faultCommittedAtSlot: fault.createdAtSlot, headersRead };
};

/** One shift of our operator's that the scheduler held on L1. */
export type OperatorShiftOnFault = Readonly<{
  txHash: string;
  startTime: number;
  endTime: number;
}>;

/**
 * Our operator's shifts, as L1's scheduler appointed them, that overlap the
 * window in which our node held the fault while it was the queue's tail:
 * from our node's first `landed_block_invalid` report on it, or its first
 * `landed_block_awaiting_da` report where it never reported the fault
 * invalid (`heldFromMs`), to the watcher's removal (`removalTxHash`
 * spending one of its state-queue outputs). An attested fault's DA apply
 * also spends its output, but the fault stays the tail after it. Where Kupo
 * shows no spend by the watcher's removal, the window ends when Kupo shows
 * every output of the fault spent, or at `untilSlot` while one is unspent;
 * the removal then stays unproven. A shift that ended before our node held
 * the fault proves nothing about its refusal to build on it. The fault's
 * commit landed in operator-2's shift, so an appointment of ours that
 * overlaps the window was made after it.
 *
 * Also what tells a fault removed before our node's shift apart: the
 * watcher's removal of the fault, the start of our operator's first shift
 * after the fault's commit, and the shift of the fault's operator that the
 * commit landed in. Each is null when L1 does not show it.
 */
export const readOperatorShiftsOnFault = async (
  input: Readonly<{
    kupoUrl: string;
    stateQueuePolicyId: string;
    schedulerPolicyId: string;
    faultHeaderHash: string;
    /** The watcher's authenticated removal of the fault. */
    removalTxHash: string;
    operatorKeyHash: string;
    shiftDurationMs: number;
    slotToUnixTime(slot: number): number;
    untilSlot: number;
    heldFromMs: number;
  }>,
  fetchImpl: typeof fetch = fetch,
): Promise<{
  faultCommittedMs: number;
  fromMs: number;
  toMs: number;
  ourShifts: OperatorShiftOnFault[];
  /**
   * The watcher's removal of the fault, as Kupo indexed it; null while Kupo
   * shows no output of the fault spent by it.
   */
  removal: Readonly<{
    txHash: string;
    slot: number;
    atMs: number;
  }> | null;
  /** Our operator's first shift starting at or after the fault's commit. */
  ourFirstShiftStartMs: number | null;
  /** The fault's operator's shift that its commit landed in. */
  faultOperatorShift: Readonly<{ startTime: number; endTime: number }> | null;
  /**
   * From the fault's commit to its removal and to the start of our
   * operator's first shift after it; null when L1 has neither yet. The
   * late-shift lead is measured from these.
   */
  measured: Readonly<{
    faultCommitToConvictionMs: number | null;
    faultCommitToOurNextShiftMs: number | null;
  }>;
}> => {
  const outputs = await readIndexedStateQueueHeader(
    {
      kupoUrl: input.kupoUrl,
      policyId: input.stateQueuePolicyId,
      headerHash: input.faultHeaderHash,
    },
    fetchImpl,
  );
  // The oldest output is the fault's commit.
  const [fault] = outputs;
  if (fault === undefined)
    throw new Error(
      `Kupo indexes no state-queue output of the fault ${input.faultHeaderHash}`,
    );
  const removed = faultRemovalOf(outputs, input.removalTxHash);
  const faultCommittedMs = input.slotToUnixTime(fault.createdAtSlot);
  const fromMs = Math.max(faultCommittedMs, input.heldFromMs);
  // Without the watcher's removal on L1, the fault was the tail until no
  // output of it was left unspent, whichever transactions spent them.
  const leftQueueAtSlot = outputs.every(
    ({ spentAtSlot }) => spentAtSlot !== undefined,
  )
    ? Math.max(...outputs.map(({ spentAtSlot }) => spentAtSlot!))
    : undefined;
  const toMs = input.slotToUnixTime(
    removed?.slot ?? leftQueueAtSlot ?? input.untilSlot,
  );
  // The appointment the fault's commit landed in was made less than a shift
  // before it; two shifts back also holds one whose start was its upper
  // validity bound.
  const slotMs =
    input.slotToUnixTime(fault.createdAtSlot + 1) - faultCommittedMs;
  const lookbackSlots = 2 * Math.ceil(input.shiftDurationMs / slotMs);
  const appointments = await readKupoMatches(
    input.kupoUrl,
    `/matches/${input.schedulerPolicyId}.${SDK.SCHEDULER_ASSET_NAME}?resolve_hashes&created_after=${Math.max(fault.createdAtSlot - 1 - lookbackSlots, 0).toString()}`,
    fetchImpl,
  );
  const ourShifts: OperatorShiftOnFault[] = [];
  let ourNextShiftMs: number | undefined;
  let faultOperatorShift: { startTime: number; endTime: number } | null = null;
  for (const match of appointments) {
    if (match.datum_type !== "inline" || typeof match.datum !== "string")
      throw new Error(
        `Scheduler output ${match.transaction_id}#${match.output_index.toString()} has no inline datum in Kupo`,
      );
    const datum = Data.from(match.datum, SDK.SchedulerDatum);
    if (datum === "NoActiveOperators") continue;
    const startTime = Number(datum.ActiveOperator.start_time);
    const endTime = startTime + input.shiftDurationMs;
    if (match.created_at.slot_no < fault.createdAtSlot) {
      if (
        datum.ActiveOperator.operator === fault.header.operatorVkey &&
        startTime <= faultCommittedMs &&
        faultCommittedMs < endTime &&
        startTime >= (faultOperatorShift?.startTime ?? -Infinity)
      )
        faultOperatorShift = { startTime, endTime };
      continue;
    }
    if (datum.ActiveOperator.operator !== input.operatorKeyHash) continue;
    if (startTime < toMs && endTime > fromMs)
      ourShifts.push({ txHash: match.transaction_id, startTime, endTime });
    if (
      startTime >= faultCommittedMs &&
      startTime < (ourNextShiftMs ?? Infinity)
    )
      ourNextShiftMs = startTime;
  }
  return {
    faultCommittedMs,
    fromMs,
    toMs,
    ourShifts,
    removal: removed === null ? null : { ...removed, atMs: toMs },
    ourFirstShiftStartMs: ourNextShiftMs ?? null,
    faultOperatorShift,
    measured: {
      faultCommitToConvictionMs:
        removed === null ? null : toMs - faultCommittedMs,
      faultCommitToOurNextShiftMs:
        ourNextShiftMs === undefined ? null : ourNextShiftMs - faultCommittedMs,
    },
  };
};

/**
 * The shortest late-shift lead: a commit's allowance plus the time the
 * fault's header runs past its build, so that both end inside operator-2's
 * shift.
 */
export const lateShiftMinimumLeadMs = (commitAllowanceMs: number) =>
  commitAllowanceMs + JOURNEY_HEADER_END_AHEAD_MS + 1;

/**
 * When the drill commits the fault in operator-2's shift: late enough that
 * our node's next shift starts while the fault is still the queue's tail,
 * and early enough that the commit and the fault's header end inside
 * operator-2's shift, so no later than the shortest lead before its end, a
 * retried commit included. `leadMs` is how long before the shift's end the
 * commit starts; it is measured on the devnet rehearsal and never guessed
 * here. `commitAllowanceMs` is how long a commit takes to land.
 */
export const planLateShiftFaultCommit = (
  input: Readonly<{
    shift: Readonly<{ startMs: number; endMs: number }>;
    leadMs: number;
    commitAllowanceMs: number;
  }>,
): Readonly<{ notBeforeMs: number; notAfterMs: number }> => {
  const { shift, leadMs, commitAllowanceMs } = input;
  for (const [name, value] of [
    ["shift start", shift.startMs],
    ["shift end", shift.endMs],
    ["lead", leadMs],
    ["commit allowance", commitAllowanceMs],
  ] as const)
    if (!Number.isFinite(value) || value < 0)
      throw new Error(`Late-shift ${name} must be finite and not negative`);
  const minimumLeadMs = lateShiftMinimumLeadMs(commitAllowanceMs);
  if (leadMs < minimumLeadMs)
    throw new Error(
      `Late-shift lead must be at least ${minimumLeadMs.toString()} ms, a commit's allowance plus the time the fault's header runs past its build, or the fault cannot end in operator-2's shift`,
    );
  const notBeforeMs = Math.max(shift.startMs, shift.endMs - leadMs);
  const notAfterMs = shift.endMs - minimumLeadMs + 1;
  if (notAfterMs <= notBeforeMs)
    throw new Error(
      "Operator-2's shift is too short for a late fault commit; wait for its next shift",
    );
  return { notBeforeMs, notAfterMs };
};
