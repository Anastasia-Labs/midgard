import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { LedgerColumns } from "./ledger.js";
import {
  type CandidateDecision,
  type CandidateNode,
  minAdaViolation,
  reject,
  resolveReferenceInputs,
} from "./phase-b.resolve-reference-inputs.js";
import { runLocalScriptEvaluation } from "./phase-b.run-local-script-evaluation.js";
import {
  PhaseAValidatedTx,
  PhaseBConfig,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import {
  describeValueDelta,
  isZeroValueDelta,
  sumMidgardValues,
  valuePreservationDelta,
} from "./value-accounting.js";

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
    ) => ({
      index: node.index,
      accepted: false as const,
      rejection: reject(ledgerTx.txId, code, detail, consensusPhase),
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
      config.nowCardanoSlotNo > ledgerTx.validityIntervalEnd
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} > ${ledgerTx.validityIntervalEnd}`,
      );
    }

    const inputValues: MidgardValue[] = [];
    let sawScriptInput = false;

    const witnessKeyHashes = new Set(candidate.derived.witnessKeyHashHexes);
    const inlineNativeScriptHashes = new Set(
      candidate.derived.nativeScriptHashHexes,
    );
    const inlinePlutusScriptHashes = new Set(
      candidate.derived.plutusScriptHashHexes,
    );
    const resolvedReferenceInputs = resolveReferenceInputs(node, stateValue);
    if ("code" in resolvedReferenceInputs) {
      return {
        index: node.index,
        accepted: false,
        rejection: resolvedReferenceInputs,
      };
    }

    const hasSatisfiedScriptMaterial = (
      scriptHash: string,
      context: string,
    ): CandidateDecision | true => {
      if (inlineNativeScriptHashes.has(scriptHash)) {
        return true;
      }

      if (
        inlinePlutusScriptHashes.has(scriptHash) ||
        resolvedReferenceInputs.scriptHashes.has(scriptHash)
      ) {
        return true;
      }

      return fail(
        RejectCodes.MissingRequiredWitness,
        `missing script witness ${scriptHash} for ${context}`,
        "scriptSources",
      );
    };

    for (const observerHash of candidate.derived.requiredObserverHashHexes) {
      const observerSatisfied = hasSatisfiedScriptMaterial(
        observerHash,
        `required observer ${observerHash}`,
      );
      if (observerSatisfied !== true) {
        return observerSatisfied;
      }
    }

    for (const mintPolicyHash of candidate.derived.mintPolicyHashHexes) {
      const mintSatisfied = hasSatisfiedScriptMaterial(
        mintPolicyHash,
        `mint policy ${mintPolicyHash}`,
      );
      if (mintSatisfied !== true) {
        return mintSatisfied;
      }
    }

    for (const inputOutRefHex of node.spentOutRefs) {
      if (spentByAccepted.has(inputOutRefHex)) {
        return fail(RejectCodes.DoubleSpend, inputOutRefHex);
      }

      const inputOutput = stateValue(inputOutRefHex);
      if (!inputOutput) {
        return fail(RejectCodes.InputNotFound, inputOutRefHex);
      }

      try {
        const output = decodeMidgardTxOutput(inputOutput);
        const paymentCred = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;

        if (paymentCred.kind === "PubKey") {
          const inputSigner = paymentCred.hash.toString("hex");
          if (!witnessKeyHashes.has(inputSigner)) {
            return fail(
              RejectCodes.MissingRequiredWitness,
              `missing witness for input signer ${inputSigner} (outref ${inputOutRefHex})`,
            );
          }
        } else {
          sawScriptInput = true;
          const inputScriptHash = paymentCred.hash.toString("hex");
          const inputScriptSatisfied = hasSatisfiedScriptMaterial(
            inputScriptHash,
            `outref ${inputOutRefHex}`,
          );
          if (inputScriptSatisfied !== true) {
            return inputScriptSatisfied;
          }
        }

        inputValues.push(output.value);
      } catch (e) {
        return fail(
          RejectCodes.InvalidOutput,
          `failed to decode input output: ${String(e)}`,
        );
      }
    }

    if (sawScriptInput || candidate.derived.requiresLocalScriptDiscovery) {
      const localScriptEvaluation = yield* runLocalScriptEvaluation(
        node,
        stateValue,
        resolvedReferenceInputs,
        witnessKeyHashes,
        config,
      );
      if (localScriptEvaluation.kind === "rejected") {
        return fail(
          localScriptEvaluation.code,
          localScriptEvaluation.detail,
          localScriptEvaluation.consensusPhase,
        );
      }
    }

    const underFundedOutput = minAdaViolation(candidate);
    if (underFundedOutput !== null) {
      return fail(RejectCodes.MinAda, underFundedOutput.detail, "valueAndMint");
    }

    const delta = valuePreservationDelta(
      sumMidgardValues(inputValues),
      ledgerTx.fee,
      candidate.derived.mintDelta,
      candidate.derived.outputSum,
    );
    if (!isZeroValueDelta(delta)) {
      return fail(
        RejectCodes.ValueNotPreserved,
        `equation mismatch: inputs - fee + mint - outputs = ${describeValueDelta(delta)}`,
        "valueAndMint",
      );
    }

    return {
      index: node.index,
      accepted: true,
    };
  });
