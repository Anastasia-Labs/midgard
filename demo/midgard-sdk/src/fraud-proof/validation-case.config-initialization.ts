import {
  credentialToAddress,
  Data,
  fromText,
  type Network,
  scriptHashToCredential,
  toUnit,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type MintingValidator } from "../common.js";
import { CaseConfig } from "./validation-case.js";

/** `midgard/validation_case_config_v1.config_asset_name`. */
export const VALIDATION_CASE_CONFIG_ASSET_NAME = fromText(
  "MIDGARD_CASE_CONFIG",
);

type ScriptHashCarrier = { readonly spendingScriptHash: string };

/** The validation-trace members the case configuration names. */
export type ValidationCaseConfigSource = {
  readonly caseConfig: MintingValidator;
  readonly source: ScriptHashCarrier;
  readonly game: ScriptHashCarrier;
  readonly boundary: ScriptHashCarrier;
  readonly award: ScriptHashCarrier;
  readonly timeout: ScriptHashCarrier;
  readonly disputeBond: ScriptHashCarrier;
  readonly threadExpiry: { readonly withdrawalScriptHash: string };
};

/** The `CaseConfigV1` of a validation-trace deployment. */
export const validationCaseConfigFromContracts = (
  chain: ValidationCaseConfigSource,
): CaseConfig => ({
  source_v1: chain.source.spendingScriptHash,
  game_v1: chain.game.spendingScriptHash,
  boundary_v1: chain.boundary.spendingScriptHash,
  award_v1: chain.award.spendingScriptHash,
  timeout_v1: chain.timeout.spendingScriptHash,
  dispute_bond_v1: chain.disputeBond.spendingScriptHash,
  thread_expiry_v1: chain.threadExpiry.withdrawalScriptHash,
});

/**
 * The address that holds the configuration token for ever: the configuration
 * policy's own script address, whose spend always fails.
 */
export const validationCaseConfigAddress = (
  network: Network,
  caseConfig: MintingValidator,
): string =>
  credentialToAddress(network, scriptHashToCredential(caseConfig.policyId));

/**
 * Mints the one-shot case configuration token beside its `CaseConfigV1`
 * inline datum. The transaction must spend the configuration's
 * initialization nonce, the hub one-shot of protocol initialization. The
 * minting script is read from its published reference script, which is
 * required.
 */
export const appendValidationCaseConfigInitialization = (
  tx: TxBuilder,
  params: {
    readonly network: Network;
    readonly contracts: ValidationCaseConfigSource;
    readonly referenceScript: UTxO;
  },
): TxBuilder => {
  const { caseConfig } = params.contracts;
  const { referenceScript } = params;
  if (
    referenceScript.scriptRef == null ||
    validatorToScriptHash(referenceScript.scriptRef) !== caseConfig.policyId
  ) {
    throw new Error(
      `Case configuration reference script ${referenceScript.txHash}#${referenceScript.outputIndex.toString()} does not carry the case configuration minting script`,
    );
  }
  const unit = toUnit(caseConfig.policyId, VALIDATION_CASE_CONFIG_ASSET_NAME);
  tx.mintAssets({ [unit]: 1n }, Data.void()).pay.ToContract(
    validationCaseConfigAddress(params.network, caseConfig),
    {
      kind: "inline",
      value: Data.to(
        validationCaseConfigFromContracts(params.contracts),
        CaseConfig,
      ),
    },
    { [unit]: 1n },
  );
  return tx.readFrom([referenceScript]);
};
