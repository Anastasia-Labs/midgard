import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import { parseFaultProofBlueprint } from "./blueprint.js";
import {
  buildDirectReceiveContracts,
  type DirectReceiveContracts,
} from "./direct-receive.js";
import { buildFaultProofSpendingStep } from "./shared.js";

export * from "../direct-purpose-workers.js";

export const DIRECT_PURPOSE_TITLES = {
  select: "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
  spendSelect: "fraud_proofs/direct_script_derivation/spend_select.main.spend",
  spendScan: "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
  mintGrammar: "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
  mintScan: "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
  lookup: "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
} as const;

export type DirectPurposeContracts = Readonly<{
  select: SpendingValidator;
  spendSelect: SpendingValidator;
  spendScan: SpendingValidator;
  mintGrammar: SpendingValidator;
  mintScan: SpendingValidator;
  lookup: SpendingValidator;
  receive: DirectReceiveContracts;
}>;

export const buildDirectPurposeContracts = ({
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
}): Effect.Effect<DirectPurposeContracts, Error> =>
  Effect.gen(function* () {
    const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
    const lookup = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_PURPOSE_TITLES.lookup,
      [nextStepScriptHash, computationThreadPolicyId],
      "Failed to build direct source lookup",
    );
    const receive = yield* buildDirectReceiveContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: lookup.spendingScriptHash,
    });
    const spendScan = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_PURPOSE_TITLES.spendScan,
      [
        lookup.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct spend pointer rank",
    );
    const spendSelect = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_PURPOSE_TITLES.spendSelect,
      [
        spendScan.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct spend selection",
    );
    const mintScan = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_PURPOSE_TITLES.mintScan,
      [
        lookup.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct mint selection",
    );
    const mintGrammar = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_PURPOSE_TITLES.mintGrammar,
      [
        mintScan.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct mint grammar",
    );
    const select = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_PURPOSE_TITLES.select,
      [
        lookup.spendingScriptHash,
        spendSelect.spendingScriptHash,
        mintGrammar.spendingScriptHash,
        receive.start.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build direct purpose selection",
    );
    return {
      select,
      spendSelect,
      spendScan,
      mintGrammar,
      mintScan,
      lookup,
      receive,
    };
  });
