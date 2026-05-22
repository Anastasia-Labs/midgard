/**
 * Phase B - stateful transaction validation.
 *
 * Validates a batch of Phase-A-accepted candidates against a UTxO pre-state.
 * Builds a dependency graph, detects cycles, then processes transactions in
 * topological waves with conflict-bucket parallelism.
 *
 * Rules enforced here:
 *   R7   - every spend input exists in UTxO state or is produced by an earlier
 *           accepted tx in the same batch
 *   R8   - no double spend across accepted transactions
 *   R8b  - every reference input exists in UTxO state (not spent by same tx)
 *   R10  - validity interval compatible with current time
 *   R12  - value preservation: Σinputs - fee = Σoutputs
 *   R16  - input-witness consistency (pubkey → vkey witness, script → native script)
 *   R17  - no dependency cycles
 *   R18  - cascade-reject descendants of any rejected transaction
 */

import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { midgardValueToCml } from "../cardano";
import {
  PhaseAAccepted,
  PhaseBConfig,
  PhaseBResult,
  PhaseBResultWithPatch,
  RejectedTx,
  RejectCodes,
  UTxOState,
  UTxOStatePatch,
} from "./types";
import { outRefKey } from "./phase-a";
import { TransactionOutput } from "../types/output";

// ---------------------------------------------------------------------------
// Internal types
// ---------------------------------------------------------------------------

type CandidateNode = {
  readonly index: number;
  readonly candidate: PhaseAAccepted;
  readonly spentOutRefs: Set<string>;
  readonly referenceOutRefs: Set<string>;
  readonly producedOutRefs: Set<string>;
  readonly parents: Set<number>;
  readonly children: Set<number>;
};

type CandidateStatus = "pending" | "accepted" | "rejected";

type CandidateDecision = {
  readonly index: number;
  readonly accepted: boolean;
  readonly rejection?: RejectedTx;
};

type MutableStatePatch = {
  readonly deletedOutRefs: Set<string>;
  readonly upsertedOutRefs: Map<string, TransactionOutput>;
};

// ---------------------------------------------------------------------------
// State helpers
// ---------------------------------------------------------------------------

const makeEmptyStatePatch = (): MutableStatePatch => ({
  deletedOutRefs: new Set(),
  upsertedOutRefs: new Map(),
});

const getStateValue = (
  baseState: UTxOState,
  patch: MutableStatePatch,
  key: string,
): TransactionOutput | undefined => {
  const patched = patch.upsertedOutRefs.get(key);
  if (patched !== undefined) return patched;
  if (patch.deletedOutRefs.has(key)) return undefined;
  return baseState.get(key);
};

const materializeStatePatch = (patch: MutableStatePatch): UTxOStatePatch => ({
  deletedOutRefs: Array.from(patch.deletedOutRefs),
  upsertedOutRefs: Array.from(patch.upsertedOutRefs.entries()),
});

export const applyUTxOStatePatch = (
  state: UTxOState,
  patch: UTxOStatePatch,
): void => {
  for (const key of patch.deletedOutRefs) state.delete(key);
  for (const [key, output] of patch.upsertedOutRefs) state.set(key, output);
};

// ---------------------------------------------------------------------------
// Value arithmetic
// ---------------------------------------------------------------------------

const sumCmlValues = (
  values: InstanceType<typeof CML.Value>[],
): InstanceType<typeof CML.Value> => {
  let sum = CML.Value.zero();
  for (const v of values) sum = sum.checked_add(v);
  return sum;
};

// ---------------------------------------------------------------------------
// Dependency graph
// ---------------------------------------------------------------------------

const buildNodes = (
  candidates: readonly PhaseAAccepted[],
): readonly CandidateNode[] => {
  const producerByOutRef = new Map<string, number>();
  for (let i = 0; i < candidates.length; i++) {
    for (const entry of candidates[i].produced) {
      producerByOutRef.set(outRefKey(entry.outRef), i);
    }
  }

  const nodes: CandidateNode[] = [];
  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const spentOutRefs = new Set(candidate.spent.map(outRefKey));
    const referenceOutRefs = new Set(candidate.referenceInputs.map(outRefKey));
    const producedOutRefs = new Set(
      candidate.produced.map((e) => outRefKey(e.outRef)),
    );

    const parents = new Set<number>();
    for (const key of spentOutRefs) {
      const parent = producerByOutRef.get(key);
      if (parent !== undefined && parent !== i) parents.add(parent);
    }
    for (const key of referenceOutRefs) {
      const parent = producerByOutRef.get(key);
      if (parent !== undefined && parent !== i) parents.add(parent);
    }

    nodes.push({
      index: i,
      candidate,
      spentOutRefs,
      referenceOutRefs,
      producedOutRefs,
      parents,
      children: new Set(),
    });
  }

  for (const node of nodes) {
    for (const parent of node.parents) {
      nodes[parent].children.add(node.index);
    }
  }

  return nodes;
};

const findCycleNodes = (nodes: readonly CandidateNode[]): Set<number> => {
  const indegree = new Map(nodes.map((n) => [n.index, n.parents.size]));
  const queue = nodes.filter((n) => n.parents.size === 0).map((n) => n.index);
  let visited = 0;

  while (queue.length > 0) {
    const index = queue.shift()!;
    visited++;
    for (const child of nodes[index].children) {
      const next = (indegree.get(child) ?? 0) - 1;
      indegree.set(child, next);
      if (next === 0) queue.push(child);
    }
  }

  if (visited === nodes.length) return new Set();

  const cycleNodes = new Set<number>();
  for (const node of nodes) {
    if ((indegree.get(node.index) ?? 0) > 0) cycleNodes.add(node.index);
  }
  return cycleNodes;
};

// ---------------------------------------------------------------------------
// Conflict bucketing (for parallel processing within a wave)
// ---------------------------------------------------------------------------

const hasIntersection = (left: Set<string>, right: Set<string>): boolean => {
  const [smaller, larger] =
    left.size <= right.size ? [left, right] : [right, left];
  for (const entry of smaller) if (larger.has(entry)) return true;
  return false;
};

const conflict = (left: CandidateNode, right: CandidateNode): boolean =>
  hasIntersection(left.spentOutRefs, right.spentOutRefs) ||
  hasIntersection(left.spentOutRefs, right.referenceOutRefs) ||
  hasIntersection(right.spentOutRefs, left.referenceOutRefs);

const buildConflictBuckets = (
  readyNodes: readonly CandidateNode[],
): CandidateNode[][] => {
  const buckets: CandidateNode[][] = [];
  for (const node of readyNodes) {
    let placed = false;
    for (const bucket of buckets) {
      if (!bucket.some((n) => conflict(node, n))) {
        bucket.push(node);
        placed = true;
        break;
      }
    }
    if (!placed) buckets.push([node]);
  }
  return buckets;
};

// ---------------------------------------------------------------------------
// Stateful validation for one candidate
// ---------------------------------------------------------------------------

const validateCandidateAgainstState = (
  node: CandidateNode,
  stateValue: (key: string) => TransactionOutput | undefined,
  spentByAccepted: Set<string>,
  nowMillis: number,
): CandidateDecision => {
  const { candidate } = node;
  const fail = (
    code: RejectedTx["code"],
    detail: string | null = null,
  ): CandidateDecision => ({
    index: node.index,
    accepted: false,
    rejection: { txId: candidate.txId, code, detail },
  });

  // R10 - validity interval vs current time
  if (
    candidate.validityIntervalStart !== undefined &&
    nowMillis < candidate.validityIntervalStart
  ) {
    return fail(
      RejectCodes.ValidityIntervalMismatch,
      `now ${nowMillis} < validityStart ${candidate.validityIntervalStart}`,
    );
  }
  if (
    candidate.validityIntervalEnd !== undefined &&
    nowMillis > candidate.validityIntervalEnd
  ) {
    return fail(
      RejectCodes.ValidityIntervalMismatch,
      `now ${nowMillis} > ttl ${candidate.validityIntervalEnd}`,
    );
  }

  // R8b - reference inputs must exist and must not be spent by this tx
  for (const refKey of node.referenceOutRefs) {
    if (node.spentOutRefs.has(refKey)) {
      return fail(
        RejectCodes.InputNotFound,
        `reference input is also spent: ${refKey}`,
      );
    }
    if (stateValue(refKey) === undefined) {
      return fail(
        RejectCodes.InputNotFound,
        `reference input not found: ${refKey}`,
      );
    }
  }

  // R7, R8, R16, R12 - spend inputs
  const witnessKeyHashes = new Set(candidate.witnessKeyHashes);
  const nativeScriptHashes = new Set(candidate.nativeScriptHashes);
  const inputCmlValues: InstanceType<typeof CML.Value>[] = [];

  for (const inputKey of node.spentOutRefs) {
    // R8 - no double spend
    if (spentByAccepted.has(inputKey)) {
      return fail(RejectCodes.DoubleSpend, inputKey);
    }

    // R7 - input must exist
    const inputOutput = stateValue(inputKey);
    if (inputOutput === undefined) {
      return fail(RejectCodes.InputNotFound, inputKey);
    }

    // R16 - input-witness consistency
    let cmlAddr: CML.Address;
    try {
      cmlAddr = CML.Address.from_raw_bytes(inputOutput.address);
    } catch (e) {
      return fail(
        RejectCodes.InvalidOutput,
        `failed to parse address for ${inputKey}: ${String(e)}`,
      );
    }
    const paymentCred = cmlAddr.payment_cred();
    if (paymentCred === undefined) {
      return fail(
        RejectCodes.InvalidOutput,
        `missing payment credential for ${inputKey}`,
      );
    }
    if (paymentCred.kind() === CML.CredentialKind.PubKey) {
      const signerHex = paymentCred.as_pub_key()?.to_hex();
      if (signerHex === undefined) {
        return fail(
          RejectCodes.InvalidOutput,
          `failed to decode pubkey credential for ${inputKey}`,
        );
      }
      if (!witnessKeyHashes.has(signerHex)) {
        return fail(
          RejectCodes.MissingRequiredWitness,
          `missing vkey witness for input ${inputKey} (signer ${signerHex})`,
        );
      }
    } else if (paymentCred.kind() === CML.CredentialKind.Script) {
      const scriptHashHex = paymentCred.as_script()?.to_hex();
      if (scriptHashHex === undefined) {
        return fail(
          RejectCodes.InvalidOutput,
          `failed to decode script credential for ${inputKey}`,
        );
      }
      if (!nativeScriptHashes.has(scriptHashHex)) {
        return fail(
          RejectCodes.MissingRequiredWitness,
          `missing native script witness for input ${inputKey} (script ${scriptHashHex})`,
        );
      }
    }

    // Accumulate input value for R12
    try {
      inputCmlValues.push(midgardValueToCml(inputOutput.value));
    } catch (e) {
      return fail(
        RejectCodes.InvalidOutput,
        `failed to read value for input ${inputKey}: ${String(e)}`,
      );
    }
  }

  // R12 - value preservation: (Σinputs - fee) - Σoutputs = 0
  // Phase A hard-rejects non-empty mint, so minted/burned values are zero here.
  try {
    const inputSum = sumCmlValues(inputCmlValues);
    const lhs = inputSum.checked_sub(CML.Value.from_coin(candidate.fee));
    const delta = lhs.checked_sub(candidate.outputSum);
    if (!delta.is_zero()) {
      return fail(
        RejectCodes.ValueNotPreserved,
        `(inputs - fee) - outputs != 0 (coin delta: ${delta.coin()}, has_multiassets: ${delta.has_multiassets()})`,
      );
    }
  } catch (e) {
    return fail(RejectCodes.ValueNotPreserved, String(e));
  }

  return { index: node.index, accepted: true };
};

// ---------------------------------------------------------------------------
// Cascade rejection
// ---------------------------------------------------------------------------

const cascadeRejectDescendants = (
  nodes: readonly CandidateNode[],
  rejectedRoot: number,
  statusByIndex: CandidateStatus[],
  rejected: RejectedTx[],
): void => {
  const queue = [...nodes[rejectedRoot].children];
  while (queue.length > 0) {
    const child = queue.shift()!;
    if (statusByIndex[child] !== "pending") continue;
    statusByIndex[child] = "rejected";
    rejected.push({
      txId: nodes[child].candidate.txId,
      code: RejectCodes.DependsOnRejectedTx,
      detail: `depends on rejected tx ${Buffer.from(nodes[rejectedRoot].candidate.txId).toString("hex")}`,
    });
    queue.push(...nodes[child].children);
  }
};

// ---------------------------------------------------------------------------
// Batch runner
// ---------------------------------------------------------------------------

export function runPhaseBValidationWithPatch(
  phaseACandidates: readonly PhaseAAccepted[],
  preState: UTxOState,
  config: PhaseBConfig,
): PhaseBResultWithPatch {
  const accepted: PhaseAAccepted[] = [];
  const rejected: RejectedTx[] = [];
  const statePatch = makeEmptyStatePatch();

  if (phaseACandidates.length === 0) {
    return {
      accepted,
      rejected,
      statePatch: materializeStatePatch(statePatch),
    };
  }

  const nodes = buildNodes(phaseACandidates);
  const cycleNodes = findCycleNodes(nodes);

  const statusByIndex: CandidateStatus[] = Array.from(
    { length: nodes.length },
    () => "pending",
  );

  // R17 - reject cycle participants upfront
  for (const cycleIdx of cycleNodes) {
    statusByIndex[cycleIdx] = "rejected";
    rejected.push({
      txId: nodes[cycleIdx].candidate.txId,
      code: RejectCodes.DependencyCycle,
      detail: "transaction is part of a dependency cycle",
    });
  }

  // Indegree counts only pending parents
  const indegree = nodes.map(
    (n) =>
      Array.from(n.parents).filter((p) => statusByIndex[p] === "pending")
        .length,
  );

  const spentByAccepted = new Set<string>();
  const stateValue = (key: string) => getStateValue(preState, statePatch, key);

  // Ready queue: pending nodes with indegree 0
  const readyQueue: number[] = nodes
    .filter(
      (n) => statusByIndex[n.index] === "pending" && indegree[n.index] === 0,
    )
    .map((n) => n.index);

  while (readyQueue.length > 0) {
    const wave = readyQueue.splice(0, readyQueue.length);
    const readyNodes = wave
      .map((i) => nodes[i])
      .filter((n) => statusByIndex[n.index] === "pending");

    if (readyNodes.length === 0) continue;

    const nextReady: number[] = [];

    for (const bucket of buildConflictBuckets(readyNodes)) {
      const pendingBucket = bucket.filter(
        (n) => statusByIndex[n.index] === "pending",
      );
      if (pendingBucket.length === 0) continue;

      // Within a conflict-free bucket all decisions are independent
      const decisions = pendingBucket.map((n) =>
        validateCandidateAgainstState(
          n,
          stateValue,
          spentByAccepted,
          config.nowMillis,
        ),
      );

      const decisionMap = new Map(decisions.map((d) => [d.index, d]));

      for (const node of pendingBucket) {
        if (statusByIndex[node.index] !== "pending") continue;
        const decision = decisionMap.get(node.index);
        if (decision === undefined) continue;

        if (!decision.accepted) {
          statusByIndex[node.index] = "rejected";
          if (decision.rejection !== undefined)
            rejected.push(decision.rejection);
          // R18 - cascade-reject descendants
          cascadeRejectDescendants(nodes, node.index, statusByIndex, rejected);
          continue;
        }

        statusByIndex[node.index] = "accepted";
        accepted.push(node.candidate);

        // Update state patch
        for (const ref of node.candidate.spent) {
          const key = outRefKey(ref);
          spentByAccepted.add(key);
          statePatch.deletedOutRefs.add(key);
          statePatch.upsertedOutRefs.delete(key);
        }
        for (const entry of node.candidate.produced) {
          const key = outRefKey(entry.outRef);
          statePatch.upsertedOutRefs.set(key, entry.output);
          statePatch.deletedOutRefs.delete(key);
        }

        // Decrement indegree of children, enqueue newly ready ones
        for (const child of node.children) {
          if (statusByIndex[child] !== "pending") continue;
          indegree[child] = Math.max(indegree[child] - 1, 0);
          if (indegree[child] === 0) nextReady.push(child);
        }
      }
    }

    if (nextReady.length > 0) readyQueue.push(...nextReady);
  }

  // Any still-pending nodes have an unresolvable ancestor rejection
  for (const node of nodes) {
    if (statusByIndex[node.index] === "pending") {
      statusByIndex[node.index] = "rejected";
      rejected.push({
        txId: node.candidate.txId,
        code: RejectCodes.DependsOnRejectedTx,
        detail: "dependency chain unresolved due to rejected ancestor",
      });
    }
  }

  // Restore arrival order for accepted transactions
  accepted.sort((a, b) =>
    a.arrivalSeq < b.arrivalSeq ? -1 : a.arrivalSeq > b.arrivalSeq ? 1 : 0,
  );

  return { accepted, rejected, statePatch: materializeStatePatch(statePatch) };
}

export function runPhaseBValidation(
  phaseACandidates: readonly PhaseAAccepted[],
  preState: UTxOState,
  config: PhaseBConfig,
): PhaseBResult {
  const { accepted, rejected } = runPhaseBValidationWithPatch(
    phaseACandidates,
    preState,
    config,
  );
  return { accepted, rejected };
}
