import { spend } from "./recipes.types.js";

export const missingRedeemerRecipes = {
  fraudProofMissingRedeemer: spend(
    (c) => c.fraudProofContracts.missingRedeemer.firstStep,
  ),
  fraudProofMissingRedeemerBindEvent: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[1],
  ),
  fraudProofMissingRedeemerBindPreState: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[2],
  ),
  fraudProofMissingRedeemerSourceStart: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[3],
  ),
  fraudProofMissingRedeemerSourceResolved: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[4],
  ),
  fraudProofMissingRedeemerSourceAuthenticate: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[5],
  ),
  fraudProofMissingRedeemerSourceMap: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[6],
  ),
  fraudProofMissingRedeemerSourceInputFinish: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[7],
  ),
  fraudProofMissingRedeemerSourceGrammar: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[8],
  ),
  fraudProofMissingRedeemerSourceWitnesses: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[9],
  ),
  fraudProofMissingRedeemerSourceFinish: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[10],
  ),
  fraudProofMissingRedeemerFamilyEntry: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[11],
  ),
  fraudProofMissingRedeemerPurposeSelect: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[12],
  ),
  fraudProofMissingRedeemerSpendSelect: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[13],
  ),
  fraudProofMissingRedeemerSpendScan: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[14],
  ),
  fraudProofMissingRedeemerMintGrammar: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[15],
  ),
  fraudProofMissingRedeemerMintScan: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[16],
  ),
  fraudProofMissingRedeemerReceiveStart: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[17],
  ),
  fraudProofMissingRedeemerReceiveGrammar: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[18],
  ),
  fraudProofMissingRedeemerReceiveOutputs: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[19],
  ),
  fraudProofMissingRedeemerReceiveFinish: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[20],
  ),
  fraudProofMissingRedeemerReceiveDeclaration: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[21],
  ),
  fraudProofMissingRedeemerSourceLookup: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[22],
  ),
  fraudProofMissingRedeemerStep02: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[23],
  ),
  fraudProofMissingRedeemerStep03: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[24],
  ),
  fraudProofMissingRedeemerStep04: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[25],
  ),
  fraudProofMissingRedeemerClaimStart: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[26],
  ),
  fraudProofMissingRedeemerClaimSpendScan: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[27],
  ),
  fraudProofMissingRedeemerClaimMintGrammar: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[28],
  ),
  fraudProofMissingRedeemerClaimMintScan: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[29],
  ),
  fraudProofMissingRedeemerClaimReceiveGrammar: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[30],
  ),
  fraudProofMissingRedeemerClaimReceiveOutputs: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[31],
  ),
  fraudProofMissingRedeemerClaimReceiveDeclaration: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[32],
  ),
  fraudProofMissingRedeemerClaimAdjudicate: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[33],
  ),
  fraudProofMissingRedeemerClaimedRedeemerGrammar: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[34],
  ),
  fraudProofMissingRedeemerClaimedRedeemerScan: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[35],
  ),
  fraudProofMissingRedeemerStep05: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[36],
  ),
} as const;
