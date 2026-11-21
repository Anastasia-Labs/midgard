import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

export const measureCollateralizedPlutusFeasibilityCandidate = (
  signedCardanoCborHex: string,
): {
  readonly signedBytes: number;
  readonly inputCount: number;
  readonly outputCount: number;
  readonly fee: bigint;
  readonly collateralInputOutRefs: readonly string[];
  readonly collateralReturnCborHex: string | undefined;
  readonly totalCollateral: bigint | undefined;
  readonly scriptDataHashHex: string | undefined;
  readonly vkeyWitnessCount: number;
  readonly plutusV3ScriptCount: number;
  readonly redeemerCount: number;
  readonly redeemersCborHex: string;
  readonly redeemerTags: readonly number[];
  readonly redeemerIndexes: readonly bigint[];
  readonly redeemerDataCborHexes: readonly string[];
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const collateralInputs = body.collateral_inputs();
  const collateralInputOutRefs: string[] = [];
  for (let index = 0; index < (collateralInputs?.len() ?? 0); index += 1) {
    const input = collateralInputs!.get(index);
    collateralInputOutRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  const redeemers = witnessSet.redeemers();
  if (redeemers === undefined) {
    throw new Error(
      "Collateralized Cardano feasibility candidate has no redeemers",
    );
  }
  const flatRedeemers = redeemers.to_flat_format();
  const redeemerTags: number[] = [];
  const redeemerIndexes: bigint[] = [];
  const redeemerDataCborHexes: string[] = [];
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let index = 0; index < flatRedeemers.len(); index += 1) {
    const redeemer = flatRedeemers.get(index);
    redeemerTags.push(redeemer.tag());
    redeemerIndexes.push(redeemer.index());
    redeemerDataCborHexes.push(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        redeemer.data().to_cbor_hex(),
      ),
    );
    executionMemory += redeemer.ex_units().mem();
    executionSteps += redeemer.ex_units().steps();
  }
  return {
    signedBytes: signedCardanoCborHex.length / 2,
    inputCount: body.inputs().len(),
    outputCount: body.outputs().len(),
    fee: body.fee(),
    collateralInputOutRefs,
    collateralReturnCborHex: body.collateral_return()?.to_cbor_hex(),
    totalCollateral: body.total_collateral(),
    scriptDataHashHex: body.script_data_hash()?.to_hex(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    plutusV3ScriptCount: witnessSet.plutus_v3_scripts()?.len() ?? 0,
    redeemerCount: flatRedeemers.len(),
    redeemersCborHex: Buffer.from(redeemers.to_cbor_bytes()).toString("hex"),
    redeemerTags,
    redeemerIndexes,
    redeemerDataCborHexes,
    executionMemory,
    executionSteps,
  };
};
