import type { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import { parseFaultProofBlueprint } from "./blueprint.js";
import { buildFaultProofSpendingStep } from "./shared.js";

/** Claim custody roles shared by the direct purpose families. */
export const DIRECT_CLAIM_TITLES = {
  start: "fraud_proofs/direct_script_derivation/claim_start.main.spend",
  spendStart:
    "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
  spendScan:
    "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
  mintGrammar:
    "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
  mintScan: "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
  receiveGrammar:
    "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
  receiveOutputs:
    "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
  receiveFinish:
    "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
  receiveDeclaration:
    "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
  adjudicate:
    "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
  claimedGrammar:
    "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
  claimedScan:
    "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
  terminal: "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
} as const;

/** Claim start and the spend, mint and receive claim domains; every domain
 * ends at the given adjudication step. */
export type DirectClaimDomainContracts = Readonly<{
  start: SpendingValidator;
  spendStart: SpendingValidator;
  spendScan: SpendingValidator;
  mintGrammar: SpendingValidator;
  mintScan: SpendingValidator;
  receiveGrammar: SpendingValidator;
  receiveOutputs: SpendingValidator;
  receiveFinish: SpendingValidator;
  receiveDeclaration: SpendingValidator;
}>;

/** The shared adjudication and its claimed field-8 scan. */
export type DirectClaimAdjudicationContracts = Readonly<{
  adjudicate: SpendingValidator;
  claimedGrammar: SpendingValidator;
  claimedScan: SpendingValidator;
}>;

export type DirectClaimContracts = DirectClaimDomainContracts &
  DirectClaimAdjudicationContracts;

type CertifiedStepInputs = {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
};

const stepBuilder = ({
  blueprint,
  network,
}: Pick<CertifiedStepInputs, "blueprint" | "network">) => {
  const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
  return (title: string, params: readonly Data[], label: string) =>
    buildFaultProofSpendingStep(
      context,
      title,
      params,
      `Failed to build direct claim ${label}`,
    );
};

/** Builds claim start and its domains from the adjudication step backwards. */
export const buildDirectClaimDomainContracts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fieldPreimageCertificatePolicyId,
  adjudicateScriptHash,
}: CertifiedStepInputs & {
  readonly adjudicateScriptHash: string;
}): Effect.Effect<DirectClaimDomainContracts, Error> =>
  Effect.gen(function* () {
    const step = stepBuilder({ blueprint, network });
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ];
    const T = DIRECT_CLAIM_TITLES;
    const next = adjudicateScriptHash;
    const spendScan = yield* step(
      T.spendScan,
      [next, ...certified],
      "spend scan",
    );
    const spendStart = yield* step(
      T.spendStart,
      [spendScan.spendingScriptHash, ...certified],
      "spend start",
    );
    const mintScan = yield* step(T.mintScan, [next, ...certified], "mint scan");
    const mintGrammar = yield* step(
      T.mintGrammar,
      [mintScan.spendingScriptHash, ...certified],
      "mint grammar",
    );
    const receiveDeclaration = yield* step(
      T.receiveDeclaration,
      [next, computationThreadPolicyId],
      "receive declaration",
    );
    const receiveFinish = yield* step(
      T.receiveFinish,
      [receiveDeclaration.spendingScriptHash, ...certified],
      "receive finish",
    );
    const receiveOutputs = yield* step(
      T.receiveOutputs,
      [receiveFinish.spendingScriptHash, ...certified],
      "receive outputs",
    );
    const receiveGrammar = yield* step(
      T.receiveGrammar,
      [receiveOutputs.spendingScriptHash, ...certified],
      "receive grammar",
    );
    const start = yield* step(
      T.start,
      [
        next,
        spendStart.spendingScriptHash,
        mintGrammar.spendingScriptHash,
        receiveGrammar.spendingScriptHash,
        ...certified,
      ],
      "start",
    );
    return {
      start,
      spendStart,
      spendScan,
      mintGrammar,
      mintScan,
      receiveGrammar,
      receiveOutputs,
      receiveFinish,
      receiveDeclaration,
    };
  });

/** Builds the shared adjudication and its claimed field-8 scan from the
 * terminal backwards. */
export const buildDirectClaimAdjudicationContracts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fieldPreimageCertificatePolicyId,
  terminalScriptHash,
}: CertifiedStepInputs & {
  readonly terminalScriptHash: string;
}): Effect.Effect<DirectClaimAdjudicationContracts, Error> =>
  Effect.gen(function* () {
    const step = stepBuilder({ blueprint, network });
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ];
    const T = DIRECT_CLAIM_TITLES;
    const claimedScan = yield* step(
      T.claimedScan,
      [terminalScriptHash, ...certified],
      "claimed redeemer scan",
    );
    const claimedGrammar = yield* step(
      T.claimedGrammar,
      [claimedScan.spendingScriptHash, ...certified],
      "claimed redeemer grammar",
    );
    const adjudicate = yield* step(
      T.adjudicate,
      [terminalScriptHash, claimedGrammar.spendingScriptHash, ...certified],
      "adjudication",
    );
    return { adjudicate, claimedGrammar, claimedScan };
  });

/** Builds the shared claim suffix (domains, adjudication, claimed scan) from
 * the terminal backwards. */
export const buildDirectClaimContracts = (
  inputs: CertifiedStepInputs & { readonly terminalScriptHash: string },
): Effect.Effect<DirectClaimContracts, Error> =>
  Effect.gen(function* () {
    const adjudication = yield* buildDirectClaimAdjudicationContracts(inputs);
    const domains = yield* buildDirectClaimDomainContracts({
      ...inputs,
      adjudicateScriptHash: adjudication.adjudicate.spendingScriptHash,
    });
    return { ...domains, ...adjudication };
  });

/** The shared claim terminal: finalize, then convict on the adjudicated
 * claim. */
export const buildDirectClaimTerminal = ({
  blueprint,
  network,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  computationThreadPolicyId,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly computationThreadPolicyId: string;
}): Effect.Effect<SpendingValidator, Error> =>
  stepBuilder({ blueprint, network })(
    DIRECT_CLAIM_TITLES.terminal,
    [fraudProofPolicyId, fraudProofTokenAddressData, computationThreadPolicyId],
    "terminal",
  );
