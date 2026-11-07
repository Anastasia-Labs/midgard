import type { MidgardValidators } from "../common.js";
import { mint, spend, withdraw } from "./recipes.types.js";

const controlPresent = (contracts: MidgardValidators) =>
  contracts.fraudProofs.validationTraceDispute !== undefined;
const control = (select: Parameters<typeof spend>[0]) =>
  spend(select, controlPresent);

export const validationTraceControlRecipes = {
  validationTraceDispute: control((c) => c.fraudProofs.validationTraceDispute),
  validationTraceDisputeSource: control(
    (c) => c.fraudProofs.validationTraceDispute.source,
  ),
  validationTraceDisputeGame: control(
    (c) => c.fraudProofs.validationTraceDispute.game,
  ),
  validationTraceDisputeBoundary: control(
    (c) => c.fraudProofs.validationTraceDispute.boundary,
  ),
  validationTraceDisputeTimeout: control(
    (c) => c.fraudProofs.validationTraceDispute.timeout,
  ),
  validationTraceDisputeAward: control(
    (c) => c.fraudProofs.validationTraceDispute.award,
  ),
  validationTraceDisputeAwardMint: mint(
    (c) => c.fraudProofContracts.validationTraceDispute.award,
    controlPresent,
  ),
  validationTraceDisputeCase: control(
    (c) => c.fraudProofContracts.validationTraceDispute.case,
  ),
  validationTraceDisputeCaseMint: mint(
    (c) => c.fraudProofContracts.validationTraceDispute.case,
    controlPresent,
  ),
  // Minted once, by protocol initialization.
  validationTraceDisputeCaseConfigMint: mint(
    (c) => c.fraudProofContracts.validationTraceDispute.caseConfig,
  ),
  validationTraceDisputeThreadExpiryWithdraw: withdraw(
    (c) => c.fraudProofContracts.validationTraceDispute.threadExpiry,
    controlPresent,
  ),
  validationTraceDisputeBond: control(
    (c) => c.fraudProofContracts.validationTraceDispute.disputeBond,
  ),
} as const;
