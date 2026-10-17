import { decodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";

import { decodeMidgardSubmittedTxFromCanonicalCbor } from "./ledger-tx/codec.js";
import { findRedeemerByPointer } from "./midgard-redeemers.js";
import { discoverLocalScriptExecutions } from "./phase-b.discover-local-script-executions.js";
import {
  collectScriptSources,
  resolveReferenceInputs,
} from "./phase-b.resolve-reference-inputs.js";
import {
  runScriptExecution,
  scriptExecutionContextCbor,
} from "./phase-b.script-run.js";
import { buildNodes } from "./phase-b.validate-candidate-against-state.js";
import { type PhaseBConfig } from "./types.js";
import { buildPhaseAValidatedTx } from "./validation-candidate.js";

/** The budget one redeemer's script spent. */
export type MidgardRedeemerBudget = {
  readonly tag: number;
  readonly index: bigint;
  readonly memory: bigint;
  readonly steps: bigint;
};

export type MidgardTxRedeemerEvaluation =
  | {
      readonly kind: "evaluated";
      readonly redeemers: readonly MidgardRedeemerBudget[];
    }
  | { readonly kind: "failed"; readonly detail: string };

/**
 * Spends each Plutus redeemer of a draft transaction against the given
 * state, under the cost accounting Phase B charges. Every script runs over
 * the context Phase B builds and with the per-transaction cap as its budget,
 * so the budgets returned are what Phase B spends once declared. Signatures,
 * fee and the script integrity hash are not checked: they depend on the
 * ex-units this call computes. Native scripts are not run.
 */
export const evaluateMidgardTxRedeemers = (input: {
  readonly txCbor: Uint8Array;
  readonly programMaterialSidecarCbor: Uint8Array;
  /**
   * Reads the outputs at the given out-refs from one consistent ledger
   * state; a missing out-ref is left out of the map.
   */
  readonly readState: (
    outRefHexes: readonly string[],
  ) => Effect.Effect<ReadonlyMap<string, Buffer>, Error>;
  readonly expectedNetworkId: bigint;
  readonly limits: MidgardConsensusProfile["limits"];
  readonly config: Pick<
    PhaseBConfig,
    "l1SlotConfig" | "evaluateCekVerdict" | "maxScriptExecutionSteps"
  >;
}): Effect.Effect<MidgardTxRedeemerEvaluation, Error> =>
  Effect.gen(function* () {
    const failed = (detail: string): MidgardTxRedeemerEvaluation => ({
      kind: "failed",
      detail,
    });
    let candidate;
    let material;
    try {
      const submitted = decodeMidgardSubmittedTxFromCanonicalCbor(
        Buffer.from(input.txCbor),
      );
      material = decodeMidgardCekProgramMaterialSidecar(
        input.programMaterialSidecarCbor,
      );
      candidate = buildPhaseAValidatedTx({
        sourceKind: "normal",
        ledgerTx: submitted.ledgerTx,
        expectedNetworkId: input.expectedNetworkId,
        txCbor: submitted.txCbor,
        programMaterialSidecarCbor: Buffer.from(
          input.programMaterialSidecarCbor,
        ),
        arrivalSeq: 0n,
        createdAt: new Date(0),
        redeemerWitnessHash: submitted.commitments.redeemerWitnessHash,
      });
    } catch (cause) {
      return failed(`undecodable transaction: ${String(cause)}`);
    }
    const node = buildNodes([candidate])[0]!;
    const state = yield* input.readState([
      ...node.spentOutRefs,
      ...node.referenceOutRefs,
    ]);
    const stateValue = (outRefHex: string): Buffer | undefined =>
      state.get(outRefHex);
    const references = resolveReferenceInputs(node, stateValue);
    if ("code" in references) {
      return failed(`${references.code}: ${references.detail ?? ""}`);
    }
    const discovered = discoverLocalScriptExecutions(
      node,
      stateValue,
      collectScriptSources(node, stateValue, references),
      references.inputs,
      new Set(candidate.derived.witnessKeyHashHexes),
      input.config.l1SlotConfig,
    );
    if (discovered.kind === "rejected") {
      return failed(`${discovered.code}: ${discovered.detail}`);
    }
    const cap = {
      memory: BigInt(input.limits.minSupportedTransactionExecutionMemoryUnits),
      cpu: BigInt(input.limits.minSupportedTransactionExecutionCpuUnits),
    };
    const redeemers: MidgardRedeemerBudget[] = [];
    for (const [index, execution] of discovered.executions.entries()) {
      if (execution.resolved.version === "NativeCardano") continue;
      if (
        execution.resolved.version === "PlutusV3" &&
        execution.purpose.kind === "receive"
      ) {
        return failed("ReceivingScript requires MidgardV1 context");
      }
      const redeemer = findRedeemerByPointer(
        discovered.contextView.redeemers.map((entry) => entry.redeemer),
        execution.pointer,
      )!;
      const { result } = yield* runScriptExecution(
        {
          scriptBytes: execution.resolved.source.scriptBytes,
          contextCbor: scriptExecutionContextCbor(
            discovered.contextView,
            execution,
            redeemer,
          ),
          executionBudget: cap,
          executionIndex: BigInt(index),
          maxSteps:
            input.config.maxScriptExecutionSteps ??
            MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
        },
        input.programMaterialSidecarCbor,
        material,
        input.config,
      );
      const pointer = `${execution.purpose.kind} redeemer ${redeemer.tag}:${redeemer.index}`;
      if (result.kind === "script_invalid") {
        return failed(`${pointer}: ${result.detail}`);
      }
      if (result.budget.memory > cap.memory || result.budget.cpu > cap.cpu) {
        return failed(`${pointer}: exceeds the per-transaction ex-units cap`);
      }
      redeemers.push({
        tag: redeemer.tag,
        index: redeemer.index,
        memory: result.budget.memory,
        steps: result.budget.cpu,
      });
    }
    return { kind: "evaluated", redeemers };
  });
