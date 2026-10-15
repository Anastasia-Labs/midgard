import { Effect } from "effect";

import { LedgerColumns } from "./ledger.js";
import {
  buildState,
  type CandidateDecision,
  type CandidateNode,
  type CandidateStatus,
  getStateValue,
  makeEmptyStatePatch,
  materializeStatePatch,
  type PhaseBResultWithPatch,
  type PreState,
  reject,
} from "./phase-b.resolve-reference-inputs.js";
import { buildConflictComponents } from "./phase-b.run-local-script-evaluation.js";
import {
  buildNodes,
  findCycleNodes,
  validateCandidateAgainstState,
} from "./phase-b.validate-candidate-against-state.js";
import {
  cascadeRejectDescendants,
  validatePlainComponentAgainstState,
} from "./phase-b.validate-plain-candidate-against-state.js";
import {
  PhaseAValidatedTx,
  PhaseBConfig,
  RejectCodes,
  RejectedTx,
} from "./types.js";

/**
 * Validates candidates sequentially, as Cardano applies a block: every spent
 * and reference input resolves against the ledger state immediately before
 * its own transaction. Candidates run in rounds of in-block dependency depth
 * (a producer precedes every spender and referencer of its outputs) and, in
 * a round, in candidate order; of a referencer and a spender of the same
 * out-ref, the earlier one decides and the other sees that decision.
 *
 * `accepted` is returned in exactly that application order, so a block that
 * commits `accepted` in order replays to the same verdicts.
 */
export const runPhaseBValidationWithPatch = (
  phaseACandidates: readonly PhaseAValidatedTx[],
  preStateEntries: PreState,
  config: PhaseBConfig,
): Effect.Effect<PhaseBResultWithPatch, Error> =>
  Effect.gen(function* () {
    const accepted: PhaseAValidatedTx[] = [];
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

    for (const cycleNode of cycleNodes) {
      statusByIndex[cycleNode] = "rejected";
      rejected.push(
        reject(
          nodes[cycleNode].candidate.ledgerTx.txId,
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

    const baseState = buildState(preStateEntries);
    const spentByAccepted = new Set<string>();
    const stateValue = (outRefHex: string): Buffer | undefined =>
      getStateValue(baseState, statePatch, outRefHex);

    const readyQueue: number[] = nodes
      .filter(
        (node) =>
          statusByIndex[node.index] === "pending" && indegree[node.index] === 0,
      )
      .map((node) => node.index);

    while (readyQueue.length > 0) {
      const readyIndices = readyQueue
        .splice(0, readyQueue.length)
        .sort((left, right) => left - right);
      const readyNodes = readyIndices
        .map((index) => nodes[index])
        .filter((node) => statusByIndex[node.index] === "pending");

      if (readyNodes.length === 0) {
        continue;
      }

      const components = buildConflictComponents(readyNodes);
      const nextReady: number[] = [];
      const plainDecisions: CandidateDecision[][] = [];
      const effectfulComponents: CandidateNode[][] = [];
      for (const component of components) {
        const decisions = validatePlainComponentAgainstState(
          component,
          stateValue,
          spentByAccepted,
          statusByIndex,
          config,
        );
        if (decisions === undefined) effectfulComponents.push(component);
        else plainDecisions.push([...decisions]);
      }
      const decisionsByComponent = yield* Effect.forEach(
        effectfulComponents,
        (component) =>
          Effect.gen(function* () {
            const componentSpent = new Set(spentByAccepted);
            const componentStateValue = (
              outRefHex: string,
            ): Buffer | undefined =>
              componentSpent.has(outRefHex) ? undefined : stateValue(outRefHex);
            const decisions: CandidateDecision[] = [];
            for (const node of component) {
              if (statusByIndex[node.index] !== "pending") continue;
              const decision = yield* validateCandidateAgainstState(
                node,
                componentStateValue,
                componentSpent,
                config,
              );
              decisions.push(decision);
              if (decision.accepted) {
                for (const outRef of node.spentOutRefs) {
                  componentSpent.add(outRef);
                }
              }
            }
            return decisions;
          }),
        {
          concurrency:
            config.bucketConcurrency <= 0
              ? "unbounded"
              : config.bucketConcurrency,
        },
      );
      const decisions = [...plainDecisions, ...decisionsByComponent]
        .flat()
        .sort((left, right) => left.index - right.index);

      for (const decision of decisions) {
        const node = nodes[decision.index];
        if (statusByIndex[node.index] !== "pending") {
          continue;
        }

        if (!decision.accepted) {
          statusByIndex[node.index] = "rejected";
          if (decision.rejection !== undefined) {
            rejected.push(decision.rejection);
          }
          cascadeRejectDescendants(nodes, node.index, statusByIndex, rejected);
          continue;
        }

        statusByIndex[node.index] = "accepted";
        accepted.push(node.candidate);

        for (const outRefHex of node.candidate.graph.spentOutRefHexes) {
          spentByAccepted.add(outRefHex);
          statePatch.deletedOutRefs.add(outRefHex);
          statePatch.upsertedOutRefs.delete(outRefHex);
        }
        for (const produced of node.candidate.graph.produced) {
          const outRefHex = produced[LedgerColumns.OUTREF].toString("hex");
          statePatch.upsertedOutRefs.set(
            outRefHex,
            Buffer.from(produced[LedgerColumns.OUTPUT]),
          );
          statePatch.deletedOutRefs.delete(outRefHex);
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

      if (nextReady.length > 0) {
        readyQueue.push(...nextReady);
      }
    }

    for (const node of nodes) {
      if (statusByIndex[node.index] === "pending") {
        statusByIndex[node.index] = "rejected";
        rejected.push(
          reject(
            node.candidate.ledgerTx.txId,
            RejectCodes.DependsOnRejectedTx,
            "dependency chain unresolved due to rejected ancestor",
          ),
        );
      }
    }

    return {
      accepted,
      rejected,
      statePatch: materializeStatePatch(statePatch),
    };
  });
