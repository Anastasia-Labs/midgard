import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseFaultProofBlueprint } from "./blueprint.js";
import { buildFaultProofSpendingStep } from "./shared.js";
export const DIRECT_SOURCE_TITLES = {
  start: "fraud_proofs/direct_script_derivation/source_start.main.spend",
  resolved: "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
  map: "fraud_proofs/direct_script_derivation/source_map.main.spend",
  authenticate:
    "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
  inputFinish:
    "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
  grammar: "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
  witnesses:
    "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
  finish: "fraud_proofs/direct_script_derivation/source_finish.main.spend",
} as const;
/** Authentication and mutation are separate bounded batches. Resume hashes
 * come from the actual caller and are committed with exact pending custody;
 * finish stages independently authenticate exhaustion, including empty fields. */
export const buildDirectSourceContracts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fieldPreimageCertificatePolicyId,
  nextStepScriptHash,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly nextStepScriptHash: string;
}) =>
  Effect.gen(function* () {
    const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
    const map = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.map,
      [computationThreadPolicyId],
      "Failed to build direct source map mutation",
    );
    const authenticate = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.authenticate,
      [map.spendingScriptHash, computationThreadPolicyId],
      "Failed to build direct resolved ledger authentication",
    );
    const finish = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.finish,
      [
        nextStepScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct source exhaustion",
    );
    const witnesses = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.witnesses,
      [
        map.spendingScriptHash,
        finish.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct witness source scan",
    );
    const grammar = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.grammar,
      [
        witnesses.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct source grammar",
    );
    const inputFinish = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.inputFinish,
      [
        grammar.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct input exhaustion",
    );
    const resolved = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.resolved,
      [
        authenticate.spendingScriptHash,
        inputFinish.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct resolved source scan",
    );
    const start = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_SOURCE_TITLES.start,
      [
        resolved.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct source pass initialization",
    );
    return {
      start,
      resolved,
      authenticate,
      map,
      inputFinish,
      grammar,
      witnesses,
      finish,
    };
  });
