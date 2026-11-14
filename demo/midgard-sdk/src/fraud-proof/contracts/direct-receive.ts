import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import { parseFaultProofBlueprint } from "./blueprint.js";
import { buildFaultProofSpendingStep } from "./shared.js";

export * from "../direct-receive-start.js";

export const DIRECT_RECEIVE_TITLES = {
  start: "fraud_proofs/direct_script_derivation/receive_start.main.spend",
  grammar: "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
  outputs: "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
  finish: "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
  declaration:
    "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
} as const;

export type DirectReceiveContracts = Readonly<{
  start: SpendingValidator;
  grammar: SpendingValidator;
  outputs: SpendingValidator;
  finish: SpendingValidator;
  declaration: SpendingValidator;
}>;

/** Physical stages separate grammar, map updates, and independently verified
 * exhaustion so every reference script remains publishable at the L1 limit. */
export const buildDirectReceiveContracts = ({
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
}): Effect.Effect<DirectReceiveContracts, Error> =>
  Effect.gen(function* () {
    const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
    const declaration = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_RECEIVE_TITLES.declaration,
      [nextStepScriptHash, computationThreadPolicyId],
      "Failed to build direct receive declaration",
    );
    const finish = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_RECEIVE_TITLES.finish,
      [
        declaration.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct receive exhaustion check",
    );
    const outputs = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_RECEIVE_TITLES.outputs,
      [
        finish.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct receive output scan",
    );
    const grammar = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_RECEIVE_TITLES.grammar,
      [
        outputs.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct receive grammar",
    );
    const start = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_RECEIVE_TITLES.start,
      [
        grammar.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct receive initialization",
    );
    return { start, grammar, outputs, finish, declaration };
  });
