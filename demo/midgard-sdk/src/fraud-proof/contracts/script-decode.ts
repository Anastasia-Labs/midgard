/**
 * The script-decode family: a run walks at the validator's spending address
 * and its verdict is minted as a record token under the validator's policy.
 * The decoding itself runs in six withdraw-zero stakers, one per class of
 * decoder controls, whose hashes the validator takes as its parameter.
 */

import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Data, type Network } from "@lucid-evolution/lucid";

import {
  type AuthenticatedValidator,
  type WithdrawalValidator,
} from "../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  getUnappliedScript,
  makeAuthenticatedValidator,
  makeWithdrawalValidator,
} from "./blueprint.js";
import { FAULT_PROOF_SHARED_TITLES } from "./titles.js";

/** The decoder control classes, in the order of the validator's parameter. */
export const SCRIPT_DECODE_STAKER_CLASS_NAMES = [
  "term",
  "value",
  "list",
  "fold",
  "bytes",
  "data",
] as const;

export type ScriptDecodeStakerClassName =
  (typeof SCRIPT_DECODE_STAKER_CLASS_NAMES)[number];

export type ScriptDecodeStakerValidators = Readonly<
  Record<ScriptDecodeStakerClassName, WithdrawalValidator>
>;

export type ScriptDecodeValidator = AuthenticatedValidator & {
  readonly stakers: ScriptDecodeStakerValidators;
};

/** The deployment member of each class's staker. */
export const SCRIPT_DECODE_STAKER_MEMBERS = {
  term: "scriptDecodeTermWithdraw",
  value: "scriptDecodeValueWithdraw",
  list: "scriptDecodeListWithdraw",
  fold: "scriptDecodeFoldWithdraw",
  bytes: "scriptDecodeBytesWithdraw",
  data: "scriptDecodeDataWithdraw",
} as const satisfies Record<ScriptDecodeStakerClassName, string>;

/**
 * Builds the script-decode validator and its stakers. The stakers take no
 * parameters; the validator takes their hashes, and its handlers share one
 * script, so its policy id is its script hash.
 */
export const buildScriptDecodeValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
): ScriptDecodeValidator => {
  const stakers = Object.fromEntries(
    SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name) => [
      name,
      makeWithdrawalValidator(
        getUnappliedScript(
          blueprint,
          deployedTitle(SCRIPT_DECODE_STAKER_MEMBERS[name]),
        ),
      ),
    ]),
  ) as Record<ScriptDecodeStakerClassName, WithdrawalValidator>;
  const params: Data[] = [
    SCRIPT_DECODE_STAKER_CLASS_NAMES.map(
      (name) => stakers[name].withdrawalScriptHash,
    ),
  ];
  const validator = makeAuthenticatedValidator(
    network,
    applyBlueprintParams(
      blueprint,
      FAULT_PROOF_SHARED_TITLES.scriptDecodeMint,
      params,
    ),
    applyBlueprintParams(
      blueprint,
      FAULT_PROOF_SHARED_TITLES.scriptDecodeSpend,
      params,
    ),
  );
  if (validator.policyId !== validator.spendingScriptHash) {
    throw new Error(
      "Script-decode mint and spend handlers must share one script",
    );
  }
  return { ...validator, stakers };
};
