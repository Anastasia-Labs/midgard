import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { LedgerUtils } from "@/database/index.js";
import {
  PhaseAAccepted,
  PhaseBConfig,
  PhaseBResult,
  RejectedTx,
  RejectCodes,
} from "./types.js";

type UTxOState = Map<string, Buffer>;
type PreState = readonly LedgerUtils.Entry[] | UTxOState;

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

const buildState = (entries: PreState): UTxOState => {
  if (entries instanceof Map) {
    return entries;
  }
  const state: UTxOState = new Map();
  for (const entry of entries) {
    state.set(entry[LedgerUtils.Columns.OUTREF].toString("hex"), entry.output);
  }
  return state;
};

const reject = (
  txId: Buffer,
  code: RejectedTx["code"],
  detail: string | null = null,
): RejectedTx => ({
  txId,
  code,
  detail,
});

const sumValues = (
  values: readonly InstanceType<typeof CML.Value>[],
): InstanceType<typeof CML.Value> => {
  let sum = CML.Value.zero();
  for (const value of values) {
    sum = sum.checked_add(value);
  }
  return sum;
};

const hasIntersection = (left: Set<string>, right: Set<string>): boolean => {
  const smaller = left.size <= right.size ? left : right;
  const larger = left.size <= right.size ? right : left;
  for (const entry of smaller) {
    if (larger.has(entry)) {
      return true;
    }
  }
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
      let hasConflict = false;
      for (const bucketNode of bucket) {
        if (conflict(node, bucketNode)) {
          hasConflict = true;
          break;
        }
      }

      if (!hasConflict) {
        bucket.push(node);
        placed = true;
        break;
      }
    }

    if (!placed) {
      buckets.push([node]);
    }
  }

  return buckets;
};

const buildNodes = (
  candidates: readonly PhaseAAccepted[],
): readonly CandidateNode[] => {
  const producerByOutRef = new Map<string, number>();

  for (let i = 0; i < candidates.length; i++) {
    for (const produced of candidates[i].processedTx.produced) {
      producerByOutRef.set(
        produced[LedgerUtils.Columns.OUTREF].toString("hex"),
        i,
      );
    }
  }

  const nodes: CandidateNode[] = [];

  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const spentOutRefs = new Set(
      candidate.processedTx.spent.map((outRef) => outRef.toString("hex")),
    );
    const referenceOutRefs = new Set(
      candidate.referenceInputs.map((outRef) => outRef.toString("hex")),
    );
    const producedOutRefs = new Set(
      candidate.processedTx.produced.map((produced) =>
        produced[LedgerUtils.Columns.OUTREF].toString("hex"),
      ),
    );

    const parents = new Set<number>();
    for (const spentOutRef of spentOutRefs) {
      const parent = producerByOutRef.get(spentOutRef);
      if (parent !== undefined && parent !== i) {
        parents.add(parent);
      }
    }
    for (const referenceOutRef of referenceOutRefs) {
      const parent = producerByOutRef.get(referenceOutRef);
      if (parent !== undefined && parent !== i) {
        parents.add(parent);
      }
    }

    nodes.push({
      index: i,
      candidate,
      spentOutRefs,
      referenceOutRefs,
      producedOutRefs,
      parents,
      children: new Set<number>(),
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
  const indegree = new Map<number, number>(
    nodes.map((node) => [node.index, node.parents.size]),
  );
  const queue: number[] = nodes
    .filter((node) => node.parents.size === 0)
    .map((node) => node.index);
  let visited = 0;

  while (queue.length > 0) {
    const index = queue.shift();
    if (index === undefined) {
      continue;
    }

    visited += 1;
    for (const child of nodes[index].children) {
      const next = (indegree.get(child) ?? 0) - 1;
      indegree.set(child, next);
      if (next === 0) {
        queue.push(child);
      }
    }
  }

  if (visited === nodes.length) {
    return new Set<number>();
  }

  const cycleNodes = new Set<number>();
  for (const node of nodes) {
    if ((indegree.get(node.index) ?? 0) > 0) {
      cycleNodes.add(node.index);
    }
  }

  return cycleNodes;
};

const validateCandidateAgainstState = (
  node: CandidateNode,
  state: UTxOState,
  spentByAccepted: Set<string>,
  nowMillis: bigint,
): CandidateDecision => {
  const candidate = node.candidate;
  const fail = (code: RejectedTx["code"], detail: string | null = null) => ({
    index: node.index,
    accepted: false as const,
    rejection: reject(candidate.txId, code, detail),
  });

  if (
    candidate.validityIntervalStart !== undefined &&
    nowMillis < candidate.validityIntervalStart
  ) {
    return fail(
      RejectCodes.ValidityIntervalMismatch,
      `${nowMillis} < ${candidate.validityIntervalStart}`,
    );
  }

  if (
    candidate.validityIntervalEnd !== undefined &&
    nowMillis > candidate.validityIntervalEnd
  ) {
    return fail(
      RejectCodes.ValidityIntervalMismatch,
      `${nowMillis} > ${candidate.validityIntervalEnd}`,
    );
  }

  const inputValues: InstanceType<typeof CML.Value>[] = [];

  for (const referenceOutRefHex of node.referenceOutRefs) {
    if (node.spentOutRefs.has(referenceOutRefHex)) {
      return fail(
        RejectCodes.InputNotFound,
        `reference input is also spent by tx: ${referenceOutRefHex}`,
      );
    }
    if (!state.has(referenceOutRefHex)) {
      return fail(
        RejectCodes.InputNotFound,
        `reference input not found: ${referenceOutRefHex}`,
      );
    }
  }

  const witnessKeyHashes = new Set(candidate.witnessKeyHashes);
  const nativeScriptHashes = new Set(candidate.nativeScriptHashes);

  for (const inputOutRefHex of node.spentOutRefs) {
    if (spentByAccepted.has(inputOutRefHex)) {
      return fail(RejectCodes.DoubleSpend, inputOutRefHex);
    }

    const inputOutput = state.get(inputOutRefHex);
    if (!inputOutput) {
      return fail(RejectCodes.InputNotFound, inputOutRefHex);
    }

    try {
      const output = CML.TransactionOutput.from_cbor_bytes(inputOutput);
      const paymentCred = output.address().payment_cred();
      if (paymentCred === undefined) {
        return fail(
          RejectCodes.InvalidOutput,
          `missing payment credential for spent outref ${inputOutRefHex}`,
        );
      }

      if (paymentCred.kind() === CML.CredentialKind.PubKey) {
        const inputSigner = paymentCred.as_pub_key()?.to_hex();
        if (inputSigner === undefined) {
          return fail(
            RejectCodes.InvalidOutput,
            `failed to decode pubkey credential for spent outref ${inputOutRefHex}`,
          );
        }
        if (!witnessKeyHashes.has(inputSigner)) {
          return fail(
            RejectCodes.MissingRequiredWitness,
            `missing witness for input signer ${inputSigner} (outref ${inputOutRefHex})`,
          );
        }
      } else if (paymentCred.kind() === CML.CredentialKind.Script) {
        const inputScriptHash = paymentCred.as_script()?.to_hex();
        if (inputScriptHash === undefined) {
          return fail(
            RejectCodes.InvalidOutput,
            `failed to decode script credential for spent outref ${inputOutRefHex}`,
          );
        }
        if (!nativeScriptHashes.has(inputScriptHash)) {
          return fail(
            RejectCodes.MissingRequiredWitness,
            `missing native script witness ${inputScriptHash} for outref ${inputOutRefHex}`,
          );
        }
      }

      inputValues.push(output.amount());
    } catch (e) {
      return fail(
        RejectCodes.InvalidOutput,
        `failed to decode input output: ${String(e)}`,
      );
    }
  }

  try {
    const inputSum = sumValues(inputValues);
    const outputSum = candidate.outputSum;
    // phase-A hard-rejects non-empty mint, so minted and burned are both zero here.
    const lhs = inputSum.checked_sub(CML.Value.from_coin(candidate.fee));
    const delta = lhs.checked_sub(outputSum);

    if (!delta.is_zero()) {
      return fail(
        RejectCodes.ValueNotPreserved,
        `equation mismatch: (inputs - fee) - outputs = { coin: ${delta.coin()}, has_multiassets: ${delta.has_multiassets()} }`,
      );
    }
  } catch (e) {
    return fail(RejectCodes.ValueNotPreserved, String(e));
  }

  return {
    index: node.index,
    accepted: true,
  };
};

const cascadeRejectDescendants = (
  nodes: readonly CandidateNode[],
  rejectedRoot: number,
  statusByIndex: CandidateStatus[],
  rejected: RejectedTx[],
): void => {
  const queue = [...nodes[rejectedRoot].children];

  while (queue.length > 0) {
    const child = queue.shift();
    if (child === undefined) {
      continue;
    }

    if (statusByIndex[child] !== "pending") {
      continue;
    }

    statusByIndex[child] = "rejected";
    rejected.push(
      reject(
        nodes[child].candidate.txId,
        RejectCodes.DependsOnRejectedTx,
        `depends on rejected tx ${nodes[rejectedRoot].candidate.txId.toString("hex")}`,
      ),
    );

    queue.push(...nodes[child].children);
  }
};

export const runPhaseBValidation = (
  phaseACandidates: readonly PhaseAAccepted[],
  preStateEntries: PreState,
  config: PhaseBConfig,
): Effect.Effect<PhaseBResult> =>
  Effect.gen(function* () {
    const accepted: PhaseAAccepted[] = [];
    const rejected: RejectedTx[] = [];

    if (phaseACandidates.length === 0) {
      return { accepted, rejected };
    }

    const nodes = buildNodes(phaseACandidates);
    const cycleNodes = findCycleNodes(nodes);

    const statusByIndex: CandidateStatus[] = Array.from(
      { length: nodes.length },
      () => "pending",
    );

    for (const cycleNode of cycleNodes) {
      statusByIndex[cycleNode] = "rejected";
      rejected.push(
        reject(
          nodes[cycleNode].candidate.txId,
          RejectCodes.DependencyCycle,
          "transaction is part of a dependency cycle",
        ),
      );
    }

    const indegree = nodes.map(
      (node) =>
        Array.from(node.parents).filter(
          (parent) => statusByIndex[parent] === "pending",
        ).length,
    );

    const state = buildState(preStateEntries);
    const spentByAccepted = new Set<string>();

    const readyQueue: number[] = nodes
      .filter(
        (node) =>
          statusByIndex[node.index] === "pending" && indegree[node.index] === 0,
      )
      .map((node) => node.index);

    while (readyQueue.length > 0) {
      const readyIndices = readyQueue.splice(0, readyQueue.length);
      const readyNodes = readyIndices
        .map((index) => nodes[index])
        .filter((node) => statusByIndex[node.index] === "pending");

      if (readyNodes.length === 0) {
        continue;
      }

      const buckets = buildConflictBuckets(readyNodes);
      const nextReady: number[] = [];
      for (const bucket of buckets) {
        const pendingBucket = bucket.filter(
          (node) => statusByIndex[node.index] === "pending",
        );
        if (pendingBucket.length === 0) {
          continue;
        }

        const decisions = yield* Effect.forEach(
          pendingBucket,
          (node) =>
            Effect.sync(() =>
              validateCandidateAgainstState(
                node,
                state,
                spentByAccepted,
                config.nowMillis,
              ),
            ),
          {
            concurrency:
              config.bucketConcurrency <= 0
                ? "unbounded"
                : config.bucketConcurrency,
          },
        );

        const decisionsByIndex = new Map<number, CandidateDecision>();
        for (const decision of decisions) {
          decisionsByIndex.set(decision.index, decision);
        }

        for (const node of pendingBucket) {
          const decision = decisionsByIndex.get(node.index);
          if (
            decision === undefined ||
            statusByIndex[node.index] !== "pending"
          ) {
            continue;
          }

          if (!decision.accepted) {
            statusByIndex[node.index] = "rejected";
            if (decision.rejection !== undefined) {
              rejected.push(decision.rejection);
            }
            cascadeRejectDescendants(
              nodes,
              node.index,
              statusByIndex,
              rejected,
            );
            continue;
          }

          statusByIndex[node.index] = "accepted";
          accepted.push(node.candidate);

          for (const inputOutRef of node.candidate.processedTx.spent) {
            const outRefHex = inputOutRef.toString("hex");
            spentByAccepted.add(outRefHex);
            state.delete(outRefHex);
          }
          for (const produced of node.candidate.processedTx.produced) {
            state.set(
              produced[LedgerUtils.Columns.OUTREF].toString("hex"),
              produced[LedgerUtils.Columns.OUTPUT],
            );
          }

          for (const child of node.children) {
            if (statusByIndex[child] !== "pending") {
              continue;
            }
            indegree[child] = Math.max(indegree[child] - 1, 0);
            if (indegree[child] === 0) {
              nextReady.push(child);
            }
          }
        }
      }

      if (nextReady.length > 0) {
        readyQueue.push(...nextReady);
      }
    }

    for (const node of nodes) {
      if (statusByIndex[node.index] === "pending") {
        statusByIndex[node.index] = "rejected";
        rejected.push(
          reject(
            node.candidate.txId,
            RejectCodes.DependsOnRejectedTx,
            "dependency chain unresolved due to rejected ancestor",
          ),
        );
      }
    }

    accepted.sort((left, right) =>
      left.arrivalSeq < right.arrivalSeq
        ? -1
        : left.arrivalSeq > right.arrivalSeq
          ? 1
          : 0,
    );

    return { accepted, rejected };
  });
