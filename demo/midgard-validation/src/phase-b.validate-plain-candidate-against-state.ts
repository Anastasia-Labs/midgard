import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardAddressBytes,
} from "@al-ft/midgard-core/codec";

import { validateCarriedPlutusOutputScripts } from "./carried-plutus-script-decoding.js";
import { checkValueAndMint } from "./phase-b.check-value-and-mint.js";
import {
  type CandidateDecision,
  type CandidateNode,
  type CandidateStatus,
  reject,
  resolveCandidateInputs,
} from "./phase-b.resolve-reference-inputs.js";
import { type RejectSubject } from "./reject-subject.js";
import { PhaseBConfig, RejectCodes, RejectedTx } from "./types.js";

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

  const resolvedInputs = resolveCandidateInputs(node, stateValue, {
    spentByAccepted,
    witnessKeyHashes: new Set(candidate.derived.witnessKeyHashHexes),
  });
  if ("code" in resolvedInputs) {
    return { index: node.index, accepted: false, rejection: resolvedInputs };
  }
  // A script-locked spend needs local script discovery: the full path decides.
  if (resolvedInputs.spendsScriptOutput) return undefined;

  const outputScriptFault = validateCarriedPlutusOutputScripts(ledgerTx);
  if (outputScriptFault !== null)
    return { index: node.index, accepted: false, rejection: outputScriptFault };

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
      { arm: "ScriptIntegrityHashMismatch" },
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
