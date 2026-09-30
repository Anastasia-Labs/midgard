import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";

import {
  type CandidateDecision,
  type CandidateNode,
  type CandidateStatus,
  minAdaViolation,
  reject,
  resolveReferenceInputs,
} from "./phase-b.resolve-reference-inputs.js";
import { PhaseBConfig, RejectCodes, RejectedTx } from "./types.js";
import {
  describeValueDelta,
  isZeroValueDelta,
  sumMidgardValues,
  valuePreservationDelta,
} from "./value-accounting.js";

const validatePlainCandidateAgainstState = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  spentByAccepted: Set<string>,
  config: PhaseBConfig,
): CandidateDecision | undefined => {
  const candidate = node.candidate;
  if (candidate.derived.requiresLocalScriptDiscovery) return undefined;
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

  const resolvedReferenceInputs = resolveReferenceInputs(node, stateValue);
  if ("code" in resolvedReferenceInputs) {
    return {
      index: node.index,
      accepted: false,
      rejection: resolvedReferenceInputs,
    };
  }

  const witnessKeyHashes = new Set(candidate.derived.witnessKeyHashHexes);
  const inputValues: MidgardValue[] = [];
  for (const inputOutRefHex of node.spentOutRefs) {
    if (spentByAccepted.has(inputOutRefHex)) {
      return fail(RejectCodes.DoubleSpend, inputOutRefHex);
    }
    const inputOutput = stateValue(inputOutRefHex);
    if (inputOutput === undefined) {
      return fail(RejectCodes.InputNotFound, inputOutRefHex);
    }
    try {
      const output = decodeMidgardTxOutput(inputOutput);
      const paymentCred = decodeMidgardAddressBytes(
        output.address,
      ).paymentCredential;
      if (paymentCred.kind === "Script") return undefined;
      const inputSigner = paymentCred.hash.toString("hex");
      if (!witnessKeyHashes.has(inputSigner)) {
        return fail(
          RejectCodes.MissingRequiredWitness,
          `missing witness for input signer ${inputSigner} (outref ${inputOutRefHex})`,
        );
      }
      inputValues.push(output.value);
    } catch (error) {
      return fail(
        RejectCodes.InvalidOutput,
        `failed to decode input output: ${String(error)}`,
      );
    }
  }

  for (const [outputIndex, output] of ledgerTx.outputs.entries()) {
    const observedNetworkId = BigInt(
      decodeMidgardAddressBytes(output.address).networkId,
    );
    if (observedNetworkId !== candidate.derived.expectedNetworkId) {
      return fail(
        RejectCodes.NetworkIdMismatch,
        `output ${outputIndex.toString()} network ${observedNetworkId.toString()} != ${candidate.derived.expectedNetworkId.toString()}`,
        "scriptSources",
      );
    }
  }

  // The plain route has no script executions, but its body still commits the
  // canonical empty language view. Keep the same ScriptIntegrity rejection
  // as the full executor and the validation machine before value checks.
  const expectedIntegrityHash = computeScriptIntegrityHashForLanguages(
    candidate.derived.redeemerWitnessHash,
    [],
  );
  if (!ledgerTx.scriptIntegrityHash.equals(expectedIntegrityHash)) {
    return fail(
      RejectCodes.InvalidFieldType,
      `script_integrity_hash mismatch: expected ${expectedIntegrityHash.toString("hex")} actual ${ledgerTx.scriptIntegrityHash.toString("hex")} required_languages=`,
      "scriptIntegrity",
    );
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
  return { index: node.index, accepted: true };
};

export const validatePlainComponentAgainstState = (
  component: readonly CandidateNode[],
  stateValue: (outRefHex: string) => Buffer | undefined,
  spentByAccepted: Set<string>,
  statusByIndex: readonly CandidateStatus[],
  config: PhaseBConfig,
): readonly CandidateDecision[] | undefined => {
  const componentSpent = new Set(spentByAccepted);
  const componentStateValue = (outRefHex: string): Buffer | undefined =>
    componentSpent.has(outRefHex) ? undefined : stateValue(outRefHex);
  const decisions: CandidateDecision[] = [];
  for (const node of component) {
    if (statusByIndex[node.index] !== "pending") continue;
    const decision = validatePlainCandidateAgainstState(
      node,
      componentStateValue,
      componentSpent,
      config,
    );
    if (decision === undefined) return undefined;
    decisions.push(decision);
    if (decision.accepted) {
      for (const outRef of node.spentOutRefs) componentSpent.add(outRef);
    }
  }
  return decisions;
};

export const cascadeRejectDescendants = (
  nodes: readonly CandidateNode[],
  rejectedRoot: number,
  statusByIndex: CandidateStatus[],
  rejected: RejectedTx[],
): void => {
  const queue = [...nodes[rejectedRoot].children];

  while (queue.length > 0) {
    const child = queue.shift()!;
    if (statusByIndex[child] !== "pending") {
      continue;
    }

    statusByIndex[child] = "rejected";
    rejected.push(
      reject(
        nodes[child].candidate.ledgerTx.txId,
        RejectCodes.DependsOnRejectedTx,
        `depends on rejected tx ${nodes[rejectedRoot].candidate.ledgerTx.txId.toString("hex")}`,
      ),
    );

    queue.push(...nodes[child].children);
  }
};
