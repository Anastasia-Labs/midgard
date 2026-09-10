import {
  type Emulator,
  Lucid,
  type LucidEvolution,
  type Network,
  type ProtocolParameters,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import snapshot from "../fixtures/mainnet-protocol-parameters.json" with { type: "json" };

/** Pinned mainnet epoch 654, protocol 11 (Van Rossem); no live network reads in tests. */
export const MAINNET_PROTOCOL_PARAMETERS_SOURCE = Object.freeze({
  url: snapshot.sourceUrl,
  retrievedAt: snapshot.retrievedAt,
  responseSha256: snapshot.sourceResponseSha256,
  epoch: snapshot.parameters.epoch_no,
  protocolMajor: snapshot.parameters.protocol_major,
  protocolMinor: snapshot.parameters.protocol_minor,
});

const parameters = snapshot.parameters;

/** All evaluator costs and transaction limits come from the same ledger snapshot. */
export const MAINNET_PROTOCOL_PARAMETERS: ProtocolParameters = {
  minFeeA: parameters.min_fee_a,
  minFeeB: parameters.min_fee_b,
  maxTxSize: parameters.max_tx_size,
  maxValSize: parameters.max_val_size,
  keyDeposit: BigInt(parameters.key_deposit),
  poolDeposit: BigInt(parameters.pool_deposit),
  drepDeposit: BigInt(parameters.drep_deposit),
  govActionDeposit: BigInt(parameters.gov_action_deposit),
  priceMem: parameters.price_mem,
  priceStep: parameters.price_step,
  maxTxExMem: BigInt(parameters.max_tx_ex_mem),
  maxTxExSteps: BigInt(parameters.max_tx_ex_steps),
  coinsPerUtxoByte: BigInt(parameters.coins_per_utxo_size),
  collateralPercentage: parameters.collateral_percent,
  maxCollateralInputs: parameters.max_collateral_inputs,
  minFeeRefScriptCostPerByte: parameters.min_fee_ref_script_cost_per_byte,
  costModels: {
    PlutusV1: [...parameters.cost_models.PlutusV1],
    PlutusV2: [...parameters.cost_models.PlutusV2],
    PlutusV3: [...parameters.cost_models.PlutusV3],
  },
};

/** Protocol 11 costing requires a version-aware evaluator for the complete cost model. */
export const createMainnetEmulatorLucid = (
  emulator: Emulator,
  network: Network = "Preprod",
): Promise<LucidEvolution> =>
  Lucid(emulator, network, {
    evaluator: createScalusEvaluator({
      protocolMajorVersion: MAINNET_PROTOCOL_PARAMETERS_SOURCE.protocolMajor,
    }),
  });
