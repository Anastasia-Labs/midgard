import { type AuthenticatedValidator } from "@al-ft/midgard-sdk";

import { applyCompiledScript, type Blueprint } from "./blueprints.js";
import { makeMintingValidator, makeSpendingValidator } from "./validators.js";

/** One operator-directory list: its minting policy, parameterized by
 * `mintParams`, and the spending validator bound to that policy (plus any
 * `spendParams` after it). */
const directoryList = (
  realBlueprint: Blueprint,
  list: string,
  mintParams: readonly string[],
  spendParams: readonly string[] = [],
): AuthenticatedValidator => {
  const minting = makeMintingValidator(
    applyCompiledScript(realBlueprint, `operator_directory/${list}.mint.mint`, [
      ...mintParams,
    ]),
  );
  return {
    ...minting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        `operator_directory/${list}.spend.spend`,
        [minting.policyId, ...spendParams],
      ),
    ),
  };
};

/** The retired, registered and active operator lists, which slash for a
 * lost validation-case defense on the award's receipt policy. */
export const buildOperatorDirectoryValidators = ({
  realBlueprint,
  hubOraclePolicyId,
  lostDefenseReceiptPolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly hubOraclePolicyId: string;
  readonly lostDefenseReceiptPolicyId: string;
}) => {
  const retiredOperators = directoryList(realBlueprint, "retired_operators", [
    hubOraclePolicyId,
    lostDefenseReceiptPolicyId,
  ]);
  const registeredOperators = directoryList(
    realBlueprint,
    "registered_operators",
    [retiredOperators.policyId, hubOraclePolicyId],
  );
  const activeOperators = directoryList(
    realBlueprint,
    "active_operators",
    [
      hubOraclePolicyId,
      registeredOperators.policyId,
      retiredOperators.policyId,
      lostDefenseReceiptPolicyId,
    ],
    [hubOraclePolicyId],
  );
  return { retiredOperators, registeredOperators, activeOperators };
};
