import {
  inspectMidgardRedeemerData,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import { Effect } from "effect";

import { validateCarriedPlutusOutputScripts } from "../carried-plutus-script-decoding.js";
import { type RejectSubject } from "../reject-subject.js";
import { PhaseBConfig, RejectCodes, RejectedTx } from "../types.js";
import {
  type CandidateDecision,
  type CandidateNode,
} from "./candidate-graph.js";
import { resolveCandidateInputs } from "./input-resolution.js";
import { reject } from "./rejection.js";
import { runLocalScriptEvaluation } from "./script-evaluation.js";
import { checkValueAndMint } from "./value-and-mint.js";

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
