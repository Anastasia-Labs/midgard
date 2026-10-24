import {
  inspectMidgardRedeemerData,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import { Effect } from "effect";

import { validateCarriedPlutusOutputScripts } from "./carried-plutus-script-decoding.js";
import { LedgerColumns } from "./ledger.js";
import { checkValueAndMint } from "./phase-b.check-value-and-mint.js";
import {
  type CandidateDecision,
  type CandidateNode,
  reject,
  resolveCandidateInputs,
} from "./phase-b.resolve-reference-inputs.js";
import { runLocalScriptEvaluation } from "./phase-b.run-local-script-evaluation.js";
import { type RejectSubject } from "./reject-subject.js";
import {
  PhaseAValidatedTx,
  PhaseBConfig,
  RejectCodes,
  RejectedTx,
} from "./types.js";

export const buildNodes = (
  candidates: readonly PhaseAValidatedTx[],
): readonly CandidateNode[] => {
  const producerByOutRef = new Map<string, number>();

  for (let i = 0; i < candidates.length; i++) {
    for (const produced of candidates[i].graph.produced) {
      producerByOutRef.set(produced[LedgerColumns.OUTREF].toString("hex"), i);
    }
  }

  const nodes: CandidateNode[] = [];

  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const spentOutRefs = new Set(candidate.graph.spentOutRefHexes);
    const referenceOutRefs = new Set(candidate.graph.referenceOutRefHexes);
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

export const findCycleNodes = (
  nodes: readonly CandidateNode[],
): Set<number> => {
  const indegree = new Map<number, number>(
    nodes.map((node) => [node.index, node.parents.size]),
  );
  const queue: number[] = nodes
    .filter((node) => node.parents.size === 0)
    .map((node) => node.index);
  let visited = 0;

  while (queue.length > 0) {
    const index = queue.shift()!;
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

export const validateCandidateAgainstState = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  spentByAccepted: Set<string>,
  config: PhaseBConfig,
): Effect.Effect<CandidateDecision, Error> =>
  Effect.gen(function* () {
    const candidate = node.candidate;
    const { ledgerTx } = candidate;
    const fail = (
      code: RejectedTx["code"],
      detail: string | null = null,
      consensusPhase: MidgardValidationPhaseName = "resolveInputs",
      subject?: RejectSubject,
    ) => ({
      index: node.index,
      accepted: false as const,
      rejection: reject(ledgerTx.txId, code, detail, consensusPhase, subject),
    });
    if (
      ledgerTx.validityIntervalStart !== undefined &&
      config.nowCardanoSlotNo < ledgerTx.validityIntervalStart
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} < ${ledgerTx.validityIntervalStart}`,
      );
    }

    if (
      ledgerTx.validityIntervalEnd !== undefined &&
      config.nowCardanoSlotNo >= ledgerTx.validityIntervalEnd
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} >= ${ledgerTx.validityIntervalEnd}`,
      );
    }

    const witnessKeyHashes = new Set(candidate.derived.witnessKeyHashHexes);
    const resolvedInputs = resolveCandidateInputs(node, stateValue, {
      spentByAccepted,
      witnessKeyHashes,
    });
    if ("code" in resolvedInputs) {
      return {
        index: node.index,
        accepted: false,
        rejection: resolvedInputs,
      };
    }

    // Stage1 Data audit precedes all purpose/source discovery (including missing sources).
    for (const [index, redeemer] of ledgerTx.redeemers.entries()) {
      const inspection = inspectMidgardRedeemerData(redeemer.dataCbor);
      if (inspection.kind === "refusal") {
        return fail(
          RejectCodes.InvalidFieldType,
          `noncanonical redeemer Data at ${index}:${inspection.offset}`,
          "scriptSources",
          { arm: "RedeemerMalformed", index: BigInt(index) },
        );
      }
      // An unsupported earlier prefix cannot be skipped to cite a later Data fault.
      // Keep its existing classification/mandatory-trace prerequisite behavior.
      if (inspection.kind === "unsupported") break;
    }

    const outputScriptFault = validateCarriedPlutusOutputScripts(ledgerTx);
    if (outputScriptFault !== null)
      return {
        index: node.index,
        accepted: false,
        rejection: outputScriptFault,
      };

    // Plain candidates can reach this fallback through a scripted neighbor.
    // Discovery also checks their output networks and script integrity hash.
    const localScriptEvaluation = yield* runLocalScriptEvaluation(
      node,
      stateValue,
      resolvedInputs,
      witnessKeyHashes,
      config,
    );
    if (localScriptEvaluation.kind === "rejected") {
      return fail(
        localScriptEvaluation.code,
        localScriptEvaluation.detail,
        localScriptEvaluation.consensusPhase,
        localScriptEvaluation.subject,
      );
    }

    const valueAndMint = checkValueAndMint({
      candidate,
      spentOutRefs: node.spentOutRefs,
      referenceOutRefs: node.referenceOutRefs,
      spentValues: resolvedInputs.spentValues,
    });
    if (valueAndMint !== null) {
      return fail(
        valueAndMint.code,
        valueAndMint.detail,
        "valueAndMint",
        valueAndMint.subject,
      );
    }

    return {
      index: node.index,
      accepted: true,
    };
  });
