import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import { parseFaultProofBlueprint } from "./blueprint.js";
import { buildFaultProofSpendingStep } from "./shared.js";

/** Family entry, claimed-purpose custody and adjudication roles. */
export const MISSING_REDEEMER_CLAIM_TITLES = {
  familyEntry: deployedTitle("fraudProofMissingRedeemerFamilyEntry"),
  start: deployedTitle("fraudProofMissingRedeemerClaimStart"),
  spendStart: deployedTitle("fraudProofMissingRedeemerClaimSpendStart"),
  spendScan: deployedTitle("fraudProofMissingRedeemerClaimSpendScan"),
  mintGrammar: deployedTitle("fraudProofMissingRedeemerClaimMintGrammar"),
  mintScan: deployedTitle("fraudProofMissingRedeemerClaimMintScan"),
  receiveGrammar: deployedTitle("fraudProofMissingRedeemerClaimReceiveGrammar"),
  receiveOutputs: deployedTitle("fraudProofMissingRedeemerClaimReceiveOutputs"),
  receiveFinish: deployedTitle("fraudProofMissingRedeemerClaimReceiveFinish"),
  receiveDeclaration: deployedTitle(
    "fraudProofMissingRedeemerClaimReceiveDeclaration",
  ),
  adjudicate: deployedTitle("fraudProofMissingRedeemerClaimAdjudicate"),
  claimedGrammar: deployedTitle(
    "fraudProofMissingRedeemerClaimedRedeemerGrammar",
  ),
  claimedScan: deployedTitle("fraudProofMissingRedeemerClaimedRedeemerScan"),
} as const;

export type MissingRedeemerClaimContracts = Readonly<
  Record<
    Exclude<keyof typeof MISSING_REDEEMER_CLAIM_TITLES, "familyEntry">,
    SpendingValidator
  >
>;

/** Builds the claim suffix from the terminal backwards. */
export const buildMissingRedeemerClaimContracts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fieldPreimageCertificatePolicyId,
  terminalScriptHash,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly terminalScriptHash: string;
}): Effect.Effect<MissingRedeemerClaimContracts, Error> =>
  Effect.gen(function* () {
    const context = { blueprint: parseFaultProofBlueprint(blueprint), network };
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ];
    const step = (title: string, params: readonly string[], label: string) =>
      buildFaultProofSpendingStep(
        context,
        title,
        params,
        `Failed to build missing-redeemer ${label}`,
      );
    const T = MISSING_REDEEMER_CLAIM_TITLES;
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
      "claim adjudication",
    );
    const next = adjudicate.spendingScriptHash;
    const spendScan = yield* step(
      T.spendScan,
      [next, ...certified],
      "claimed spend scan",
    );
    const spendStart = yield* step(
      T.spendStart,
      [spendScan.spendingScriptHash, ...certified],
      "claimed spend start",
    );
    const mintScan = yield* step(
      T.mintScan,
      [next, ...certified],
      "claimed mint scan",
    );
    const mintGrammar = yield* step(
      T.mintGrammar,
      [mintScan.spendingScriptHash, ...certified],
      "claimed mint grammar",
    );
    const receiveDeclaration = yield* step(
      T.receiveDeclaration,
      [next, computationThreadPolicyId],
      "claimed receive declaration",
    );
    const receiveFinish = yield* step(
      T.receiveFinish,
      [receiveDeclaration.spendingScriptHash, ...certified],
      "claimed receive finish",
    );
    const receiveOutputs = yield* step(
      T.receiveOutputs,
      [receiveFinish.spendingScriptHash, ...certified],
      "claimed receive outputs",
    );
    const receiveGrammar = yield* step(
      T.receiveGrammar,
      [receiveOutputs.spendingScriptHash, ...certified],
      "claimed receive grammar",
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
      "claim start",
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
      adjudicate,
      claimedGrammar,
      claimedScan,
    };
  });

/** The route choice between the observed purpose graph and claim custody. */
export const buildMissingRedeemerFamilyEntry = ({
  blueprint,
  network,
  computationThreadPolicyId,
  purposeSelectScriptHash,
  claimStartScriptHash,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly purposeSelectScriptHash: string;
  readonly claimStartScriptHash: string;
}): Effect.Effect<SpendingValidator, Error> =>
  buildFaultProofSpendingStep(
    { blueprint: parseFaultProofBlueprint(blueprint), network },
    MISSING_REDEEMER_CLAIM_TITLES.familyEntry,
    [purposeSelectScriptHash, claimStartScriptHash, computationThreadPolicyId],
    "Failed to build missing-redeemer family entry",
  );
