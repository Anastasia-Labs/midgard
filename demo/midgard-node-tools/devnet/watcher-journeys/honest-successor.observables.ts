import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
  txHash: string;
  outputIndex: number;
  createdAtSlot: number;
  /** When it was spent (merged or removed), if it was. */
  spentAtSlot?: number;
}>;

type KupoMatch = Readonly<{
  transaction_id: string;
  output_index: number;
  address: string;
  value: { coins: number | string; assets?: Record<string, number | string> };
  datum?: string | null;
  datum_type?: string | null;
  created_at: { slot_no: number };
  spent_at?: { slot_no: number } | null;
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
    throw new Error(`Kupo ${path} failed with ${response.status.toString()}`);
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
  return {
    headerHash,
    header: await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum({ data: node.data as never }),
    ),
    txHash: match.transaction_id,
    outputIndex: match.output_index,
    createdAtSlot: match.created_at.slot_no,
    ...(match.spent_at == null ? {} : { spentAtSlot: match.spent_at.slot_no }),
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
 * window in which the fault was the queue's tail: from its commit to its
 * removal (its state-queue output spent), or to `untilSlot` while it is
 * unspent. The fault's commit landed in operator-2's shift, so an
 * appointment of ours that overlaps the window was made after it.
 */
export const readOperatorShiftsOnFault = async (
  input: Readonly<{
    kupoUrl: string;
    stateQueuePolicyId: string;
    schedulerPolicyId: string;
    faultHeaderHash: string;
    operatorKeyHash: string;
    shiftDurationMs: number;
    slotToUnixTime(slot: number): number;
    untilSlot: number;
  }>,
  fetchImpl: typeof fetch = fetch,
): Promise<{
  fromMs: number;
  toMs: number;
  ourShifts: OperatorShiftOnFault[];
}> => {
  const [fault] = await readIndexedStateQueueHeader(
    {
      kupoUrl: input.kupoUrl,
      policyId: input.stateQueuePolicyId,
      headerHash: input.faultHeaderHash,
    },
    fetchImpl,
  );
  if (fault === undefined)
    throw new Error(
      `Kupo indexes no state-queue output of the fault ${input.faultHeaderHash}`,
    );
  const fromMs = input.slotToUnixTime(fault.createdAtSlot);
  const toMs = input.slotToUnixTime(fault.spentAtSlot ?? input.untilSlot);
  const appointments = await readKupoMatches(
    input.kupoUrl,
    `/matches/${input.schedulerPolicyId}.${SDK.SCHEDULER_ASSET_NAME}?resolve_hashes&created_after=${Math.max(fault.createdAtSlot - 1, 0).toString()}`,
    fetchImpl,
  );
  const ourShifts: OperatorShiftOnFault[] = [];
  for (const match of appointments) {
    if (match.datum_type !== "inline" || typeof match.datum !== "string")
      throw new Error(
        `Scheduler output ${match.transaction_id}#${match.output_index.toString()} has no inline datum in Kupo`,
      );
    const datum = Data.from(match.datum, SDK.SchedulerDatum);
    if (
      datum === "NoActiveOperators" ||
      datum.ActiveOperator.operator !== input.operatorKeyHash
    )
      continue;
    const startTime = Number(datum.ActiveOperator.start_time);
    const endTime = startTime + input.shiftDurationMs;
    if (startTime < toMs && endTime > fromMs)
      ourShifts.push({ txHash: match.transaction_id, startTime, endTime });
  }
  return { fromMs, toMs, ourShifts };
};

/**
 * When the drill commits the fault in operator-2's shift: late enough that
 * our node's next shift starts while the fault is still the queue's tail,
 * and early enough that the commit lands inside operator-2's shift. `leadMs`
 * is how long before the shift's end the commit starts; it is measured on
 * the devnet rehearsal and never guessed here. `commitAllowanceMs` is how
 * long a commit takes to land.
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
  if (leadMs <= commitAllowanceMs)
    throw new Error(
      "Late-shift lead must exceed the commit allowance, or the commit cannot land in operator-2's shift",
    );
  const notBeforeMs = Math.max(shift.startMs, shift.endMs - leadMs);
  const notAfterMs = shift.endMs - commitAllowanceMs;
  if (notAfterMs <= notBeforeMs)
    throw new Error(
      "Operator-2's shift is too short for a late fault commit; wait for its next shift",
    );
  return { notBeforeMs, notAfterMs };
};
