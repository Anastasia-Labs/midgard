/** Shared deployment recipes for the hub and authenticated user-event scripts. */
import { normalizeOutRef, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { Constr, type Network } from "@lucid-evolution/lucid";

import type {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  getUnappliedScript,
  makeAuthenticatedValidator,
  makeMintingPolicy,
  makeSpendingValidator,
} from "../fraud-proof/contracts/blueprint.js";
import { HUB_ORACLE_ASSET_NAME } from "../hub-oracle.js";

export const USER_EVENT_CONTRACT_TITLES = Object.freeze({
  hubOracle: Object.freeze({ mint: "hub_oracle.mint.mint" }),
  deposit: Object.freeze({
    mint: "user_events/deposit.mint.mint",
    spend: "user_events/deposit.spend.spend",
  }),
  withdrawal: Object.freeze({
    mint: "user_events/withdrawal.mint.mint",
    spend: "user_events/withdrawal.spend.spend",
  }),
  txOrder: Object.freeze({
    mint: "user_events/tx_order_v1.mint.mint",
    spend: "user_events/tx_order_v1.spend.spend",
    fieldPreimageCertificateSpend:
      "field_preimage_certificate.field_preimage_certificate.spend",
    fieldPreimageCertificateMint:
      "field_preimage_certificate.field_preimage_certificate.mint",
    cekProgramMaterialSpend: "user_events/cek_program_material_v1.spend.spend",
  }),
});

const assertRecipe = (
  blueprint: FaultProofBlueprint,
  title: string,
  parameterNames: readonly string[],
): void => {
  const matches = blueprint.validators.filter((entry) => entry.title === title);
  const entry = matches[0];
  if (matches.length !== 1 || entry === undefined) {
    throw new Error(`User-event blueprint must contain exactly one "${title}"`);
  }
  if (
    entry.parameters.length !== parameterNames.length ||
    entry.parameters.some(
      (parameter, index) => parameter.title !== parameterNames[index],
    )
  ) {
    throw new Error(
      `User-event blueprint "${title}" has unexpected declared parameters`,
    );
  }
  if (
    entry.compiledCode.length === 0 ||
    entry.compiledCode.length > 2 * 1024 * 1024 ||
    !/^(?:[0-9a-fA-F]{2})+$/u.test(entry.compiledCode)
  ) {
    throw new Error(
      `User-event blueprint "${title}" has invalid compiled code`,
    );
  }
};

const assertHubPolicyId = (hubOraclePolicyId: string): void => {
  if (!/^[0-9a-f]{56}$/u.test(hubOraclePolicyId)) {
    throw new Error(
      "Hub-oracle policy id must be exactly 28 bytes of hexadecimal",
    );
  }
};

export const buildHubOracleMintingValidator = (input: {
  readonly blueprint: FaultProofBlueprint;
  readonly oneShotOutRef: OutRefLike;
}): MintingValidator => {
  const title = USER_EVENT_CONTRACT_TITLES.hubOracle.mint;
  assertRecipe(input.blueprint, title, ["init_utxo", "hub_oracle_asset_name"]);
  const oneShot = normalizeOutRef(input.oneShotOutRef);
  return makeMintingPolicy(
    applyBlueprintParams(input.blueprint, title, [
      new Constr(0, [oneShot.txHash, BigInt(oneShot.outputIndex)]),
      HUB_ORACLE_ASSET_NAME,
    ]),
  );
};

type UserEventContractInput = Readonly<{
  blueprint: FaultProofBlueprint;
  network: Network;
  hubOraclePolicyId: string;
}>;

const buildAuthenticatedEvent = (
  input: UserEventContractInput,
  titles: Readonly<{ mint: string; spend: string }>,
): AuthenticatedValidator => {
  assertHubPolicyId(input.hubOraclePolicyId);
  assertRecipe(input.blueprint, titles.mint, ["hub_oracle"]);
  assertRecipe(input.blueprint, titles.spend, ["hub_oracle"]);
  return makeAuthenticatedValidator(
    input.network,
    applyBlueprintParams(input.blueprint, titles.mint, [
      input.hubOraclePolicyId,
    ]),
    applyBlueprintParams(input.blueprint, titles.spend, [
      input.hubOraclePolicyId,
    ]),
  );
};

export const buildDepositValidators = (
  input: UserEventContractInput,
): AuthenticatedValidator =>
  buildAuthenticatedEvent(input, USER_EVENT_CONTRACT_TITLES.deposit);

export const buildWithdrawalValidators = (
  input: UserEventContractInput,
): AuthenticatedValidator =>
  buildAuthenticatedEvent(input, USER_EVENT_CONTRACT_TITLES.withdrawal);

/** Derive the parameterless certificate before applying its policy to tx-order. */
export const buildTxOrderValidators = (
  input: UserEventContractInput,
): {
  readonly txOrder: AuthenticatedValidator;
  readonly fieldPreimageCertificate: SpendingValidator & MintingValidator;
  readonly cekProgramMaterial: SpendingValidator;
} => {
  const titles = USER_EVENT_CONTRACT_TITLES.txOrder;
  assertHubPolicyId(input.hubOraclePolicyId);
  assertRecipe(input.blueprint, titles.mint, [
    "hub_oracle",
    "field_preimage_certificate_policy_id",
  ]);
  assertRecipe(input.blueprint, titles.spend, ["hub_oracle"]);
  for (const title of [
    titles.fieldPreimageCertificateMint,
    titles.fieldPreimageCertificateSpend,
    titles.cekProgramMaterialSpend,
  ]) {
    assertRecipe(input.blueprint, title, []);
  }
  const fieldPreimageCertificate = makeAuthenticatedValidator(
    input.network,
    getUnappliedScript(input.blueprint, titles.fieldPreimageCertificateMint),
    getUnappliedScript(input.blueprint, titles.fieldPreimageCertificateSpend),
  );
  return {
    fieldPreimageCertificate,
    cekProgramMaterial: makeSpendingValidator(
      input.network,
      getUnappliedScript(input.blueprint, titles.cekProgramMaterialSpend),
    ),
    txOrder: makeAuthenticatedValidator(
      input.network,
      applyBlueprintParams(input.blueprint, titles.mint, [
        input.hubOraclePolicyId,
        fieldPreimageCertificate.policyId,
      ]),
      applyBlueprintParams(input.blueprint, titles.spend, [
        input.hubOraclePolicyId,
      ]),
    ),
  };
};
