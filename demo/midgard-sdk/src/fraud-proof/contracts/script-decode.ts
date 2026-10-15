/**
 * The script-decode family: a run walks at the validator's spending address
 * and its verdict is minted as a record token under the validator's policy.
 */

import { type Network } from "@lucid-evolution/lucid";

import { type AuthenticatedValidator } from "../../common.js";
import {
  type FaultProofBlueprint,
  getUnappliedScript,
  makeAuthenticatedValidator,
} from "./blueprint.js";
import { FAULT_PROOF_SHARED_TITLES } from "./titles.js";

/**
 * Builds the script-decode validator. It takes no parameters and its handlers
 * share one script, so its policy id is its script hash and a pure function of
 * the blueprint.
 */
export const buildScriptDecodeValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
): AuthenticatedValidator => {
  const validator = makeAuthenticatedValidator(
    network,
    getUnappliedScript(blueprint, FAULT_PROOF_SHARED_TITLES.scriptDecodeMint),
    getUnappliedScript(blueprint, FAULT_PROOF_SHARED_TITLES.scriptDecodeSpend),
  );
  if (validator.policyId !== validator.spendingScriptHash) {
    throw new Error(
      "Script-decode mint and spend handlers must share one script",
    );
  }
  return validator;
};
