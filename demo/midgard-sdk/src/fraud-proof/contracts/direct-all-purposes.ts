import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import { parseFaultProofBlueprint } from "./blueprint.js";
import { buildFaultProofSpendingStep } from "./shared.js";

const title = (name: string) =>
  `fraud_proofs/direct_script_derivation/${name}.main.spend`;

export const DIRECT_ALL_PURPOSES_TITLES = {
  spend: title("all_purposes_spend"),
  spendResolve: title("all_purposes_spend_resolve"),
  mintGrammar: title("all_purposes_mint_grammar"),
  mint: title("all_purposes_mint"),
  observeOpen: title("all_purposes_observe_open"),
  observe: title("all_purposes_observe"),
  receiveGrammar: title("all_purposes_receive_grammar"),
  receive: title("all_purposes_receive"),
} as const;

export type DirectAllPurposesContracts = Readonly<
  Record<keyof typeof DIRECT_ALL_PURPOSES_TITLES, SpendingValidator>
>;

/** The all-purposes walk in purpose-kind order: spends, mint policies,
 * required observers, then receive outputs. The exhausted receive field hands
 * the thread to `nextStepScriptHash`. */
export const buildDirectAllPurposesContracts = ({
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
}): Effect.Effect<DirectAllPurposesContracts, Error> =>
  Effect.gen(function* () {
    const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ];
    const receive = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.receive,
      [nextStepScriptHash, ...certified],
      "Failed to build all-purposes receive walk",
    );
    const receiveGrammar = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.receiveGrammar,
      [receive.spendingScriptHash, nextStepScriptHash, ...certified],
      "Failed to build all-purposes receive grammar",
    );
    const observe = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.observe,
      [receiveGrammar.spendingScriptHash, ...certified],
      "Failed to build all-purposes observer walk",
    );
    const observeOpen = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.observeOpen,
      [
        observe.spendingScriptHash,
        receiveGrammar.spendingScriptHash,
        ...certified,
      ],
      "Failed to build all-purposes observer opening",
    );
    const mint = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.mint,
      [observeOpen.spendingScriptHash, ...certified],
      "Failed to build all-purposes mint walk",
    );
    const mintGrammar = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.mintGrammar,
      [mint.spendingScriptHash, observeOpen.spendingScriptHash, ...certified],
      "Failed to build all-purposes mint grammar",
    );
    const spendResolve = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.spendResolve,
      [mintGrammar.spendingScriptHash, computationThreadPolicyId],
      "Failed to build all-purposes spend resolution",
    );
    const spend = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_ALL_PURPOSES_TITLES.spend,
      [spendResolve.spendingScriptHash, ...certified],
      "Failed to build all-purposes spend walk",
    );
    return {
      spend,
      spendResolve,
      mintGrammar,
      mint,
      observeOpen,
      observe,
      receiveGrammar,
      receive,
    };
  });
