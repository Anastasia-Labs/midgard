import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import { parseFaultProofBlueprint } from "./blueprint.js";
import { buildFaultProofSpendingStep } from "./shared.js";

export const DIRECT_BINDING_TITLES = {
  event: "fraud_proofs/direct_script_derivation/bind_event.main.spend",
  preState: "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
} as const;

export type DirectBindingContracts = Readonly<{
  event: SpendingValidator;
  preState: SpendingValidator;
}>;

/** Event selection and transition opening are separate physical steps because
 * each transaction may authenticate only one counted membership claim. */
export const buildDirectBindingContracts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  nextStepScriptHash,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly nextStepScriptHash: string;
}): Effect.Effect<DirectBindingContracts, Error> =>
  Effect.gen(function* () {
    const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
    const preState = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_BINDING_TITLES.preState,
      [nextStepScriptHash, computationThreadPolicyId],
      "Failed to build direct transition pre-state binding",
    );
    const event = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_BINDING_TITLES.event,
      [preState.spendingScriptHash, computationThreadPolicyId],
      "Failed to build direct event selection",
    );
    return { event, preState };
  });
