import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  Address,
  Assets,
  type BuildTxWithRedeemer,
  Data,
  fromUnit,
  LucidEvolution,
  PolicyId,
  Script,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData } from "effect";

import { GenericErrorFields } from "./common.js";
import {
  type CorrectionIdentity,
  CorrectionLockDatum,
  CorrectionLockRedeemer,
  type CorrectionLockUTxO,
} from "./correction-lock.js";
import {
  collectRemoveSlashingInputs,
  removeSlashingFraudProverReward,
  removeSlashingReferenceInputs,
} from "./state-queue.collect-remove-slashing-inputs.js";
import {
  type EmulatorStateQueueRemoveSlashingParams,
  type StateQueueRemoveReferenceScriptUTxOs,
} from "./state-queue.emulator-state-queue-commit-block-header-params.js";
import {
  applyStateQueueZeroYield,
  STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
  type StateQueueYieldWitness,
} from "./state-queue.state-queue-redeemer-schema.js";
import { requireReferenceInputIndex } from "./tx-context-redeemer.js";
import { dedupeAndSortUtxos } from "./tx-out-ref-order.js";

type StateQueueRemovalTxAssemblyParams = {
  readonly collectedStateQueueInputs: readonly UTxO[];
  readonly continuedOutput: {
    readonly datum: string;
    readonly assets: Assets;
  };
  readonly assetsToBurn: Assets;
  readonly stateQueueMintRedeemer: BuildTxWithRedeemer;
  readonly additionalInputs?: readonly UTxO[];
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
  readonly fraudProofRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly correctionLockInput: CorrectionLockUTxO;
  readonly correctionLockOutputDatum: CorrectionLockDatum;
  readonly correctionLockSpendingScript: Script;
  readonly additionalRefInputs?: readonly UTxO[];
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
  readonly slashing: EmulatorStateQueueRemoveSlashingParams;
  readonly yieldWitness: StateQueueYieldWitness;
};

export const buildStateQueueRemovalTx = (
  lucid: LucidEvolution,
  stateQueueAddress: Address,
  params: StateQueueRemovalTxAssemblyParams,
): TxBuilder => {
  const additionalInputs = params.additionalInputs ?? [];
  const referenceScriptInputs = Object.values(
    params.referenceScripts ?? {},
  ).filter((utxo): utxo is UTxO => utxo !== undefined);
  const referenceInputs = dedupeAndSortUtxos([
    params.fraudProofRefInput,
    params.hubOracleRefInput,
    ...(params.additionalRefInputs ?? []),
    ...removeSlashingReferenceInputs(params.slashing),
    ...referenceScriptInputs,
    params.yieldWitness.referenceInput,
  ]);
  let tx = lucid.newTx();
  if (params.validFrom !== undefined) {
    tx = tx.validFrom(Number(params.validFrom));
  }
  if (params.validTo !== undefined) {
    tx = tx.validTo(Number(params.validTo));
  }
  if (additionalInputs.length > 0) {
    tx = tx.collectFrom([...additionalInputs]);
  }
  tx = tx
    .collectFrom(
      [...params.collectedStateQueueInputs],
      STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
    )
    .collectFrom([params.correctionLockInput.utxo], ((ctx) =>
      Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              params.hubOracleRefInput,
              "state-queue correction lock hub oracle",
            ),
          },
        } satisfies CorrectionLockRedeemer,
        CorrectionLockRedeemer,
      )) satisfies BuildTxWithRedeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      stateQueueAddress,
      { kind: "inline", value: params.continuedOutput.datum },
      params.continuedOutput.assets,
    )
    .pay.ToContract(
      params.correctionLockInput.utxo.address,
      {
        kind: "inline",
        value: Data.to(params.correctionLockOutputDatum, CorrectionLockDatum),
      },
      params.correctionLockInput.utxo.assets,
    )
    .mintAssets(params.assetsToBurn, params.stateQueueMintRedeemer);

  // D3: the fraud prover's exact reward, ADA-only, at their enterprise
  // address. Nothing is paid while the compiled reward is zero, which is the
  // only case in which the slashing redeemer may carry a null reward index.
  //
  const rewardPlan = removeSlashingFraudProverReward(params.slashing);
  if (rewardPlan !== undefined) {
    tx = tx.pay.ToAddress(rewardPlan.proverEnterpriseAddress, {
      lovelace: rewardPlan.lovelace,
    });
  }

  if (params.referenceScripts?.stateQueueSpend === undefined) {
    tx = tx.attach.Script(params.stateQueueSpendingScript);
  }
  if (params.referenceScripts?.stateQueueMint === undefined) {
    tx = tx.attach.Script(params.stateQueueMintingScript);
  }
  if (params.referenceScripts?.correctionLockSpend === undefined) {
    tx = tx.attach.Script(params.correctionLockSpendingScript);
  }

  const withSlashing = collectRemoveSlashingInputs(
    tx,
    params.slashing,
    params.referenceScripts,
  );
  return applyStateQueueZeroYield(lucid, withSlashing, params.yieldWitness);
};

export const requireFraudProofCorrectionIdentity = (
  fraudProofRefInput: UTxO,
  fraudProofPolicyId: PolicyId,
): CorrectionIdentity => {
  const candidates = Object.entries(fraudProofRefInput.assets).flatMap(
    ([unit, quantity]) => {
      if (unit === "lovelace" || unit === "" || quantity !== 1n) {
        return [];
      }
      const asset = fromUnit(unit);
      return asset.policyId === fraudProofPolicyId && asset.assetName !== null
        ? [asset.assetName]
        : [];
    },
  );
  if (candidates.length !== 1) {
    throw new Error(
      `Expected exactly one permanent fraud-proof token under policy ${fraudProofPolicyId}; found ${candidates.length.toString()}`,
    );
  }
  return {
    FraudProof: { fraud_proof_asset_name: candidates[0] },
  };
};

export const requireCorrectionLockForTarget = (
  correctionLockInput: CorrectionLockUTxO,
  targetHeaderHash: string,
  correctionIdentity: CorrectionIdentity,
): void => {
  const expectedLocked: CorrectionLockDatum = {
    Locked: {
      target_header_hash: targetHeaderHash,
      correction_identity: correctionIdentity,
    },
  };
  if (
    correctionLockInput.datum !== "Idle" &&
    Data.to(correctionLockInput.datum, CorrectionLockDatum) !==
      Data.to(expectedLocked, CorrectionLockDatum)
  ) {
    throw new Error(
      "Correction lock is held by a different target or correction identity",
    );
  }
};

export const DA_ATTESTATION_TIMEOUT_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.da_attestation_timeout_ms,
);

export class StateQueueError extends EffectData.TaggedError(
  "StateQueueError",
)<GenericErrorFields> {}
