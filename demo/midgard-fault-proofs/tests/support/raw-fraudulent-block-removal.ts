/** A fraudulent-block removal written directly as a transaction, the way a
 * submitter who does not use the off-chain removal builder would write it.
 *
 * Every ingredient is read from the chain: the queue nodes, the correction
 * lock, the hub oracle, the deployed reference scripts and, when the
 * transaction touches the operator directory, the directory entries and
 * scheduler. The caller chooses only what a submitter is free to choose: the
 * record presented as the conviction, the slashing approach the state-queue
 * redeemer names, what the transaction does to the removed block's operator,
 * and any extra output. Nothing here consults the builder's own decision of
 * which arm applies. */
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  fromUnit,
  toUnit,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseContractDeploymentInfo } from "../../src/inspect-contracts.parse-contract-deployment-info.js";
import { buildRemovalContracts } from "../../src/remove-fraudulent-block.assemble-removal-contracts.js";
import {
  fraudSlashEconomicsFromDeploymentManifest,
  resolveFraudSlashEconomics,
} from "../../src/remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { resolveOperatorSlashingPlan } from "../../src/remove-fraudulent-block.build-active-slashing-inputs.js";
import {
  loadStateQueueTopology,
  resolveReferenceScripts,
} from "../../src/remove-fraudulent-block.load-state-queue-topology.js";
import { resolveRegisteredOperatorRemovalWitness } from "../../src/remove-fraudulent-block.resolve-registered-operator-removal-witness.js";
import { buildSlashingInputs } from "../../src/remove-fraudulent-block.resolve-state-queue-slashing-approach.js";
import { outRefLabel } from "../../src/runtime.js";
import { type ProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.js";
import { network } from "./submit-init-emulator-shared.js";
import { proverEnterpriseAddress } from "./validation-trace-removal.js";

export type RawRemoval = {
  /** The block removed: the convicted block's immediate successor (the
   * queue link is spliced past it), or the convicted block itself. */
  readonly removes: "successor" | "convicted";
  /** The reference input presented as the convicted block's proof record. */
  readonly record: UTxO;
  /** The slashing approach the state-queue burn redeemer names. */
  readonly approach:
    | "DescendantOfValidationTraceConviction"
    | "SlashActiveOperator"
    | "SlashRetiredOperator"
    | "OperatorAlreadySlashed";
  /** What the transaction does to the removed block's operator:
   * - `untouched`: nothing;
   * - `witnessed`: cite the directory entries proving it is in neither
   *   operator directory;
   * - `{ slashRewardTo }`: spend and burn its node in whichever directory
   *   holds it, as a bond-backed slash does, paying the reward tranche to
   *   `slashRewardTo`. */
  readonly operator:
    | "untouched"
    | "witnessed"
    | { readonly slashRewardTo: string };
  /** Add a wallet-funded output paying exactly the prover's reward to the
   * prover's enterprise address. */
  readonly payRewardShapedOutput?: boolean;
};

const deploymentDocument = (fixture: ProvedDoubleSpendFixture): unknown =>
  "manifestId" in fixture.deploymentInfo
    ? structuredClone(verifyFinalizedDeploymentManifest(fixture.deploymentInfo))
    : fixture.deploymentInfo;

/** The correction identity a record's single token names. */
const recordAssetName = (record: UTxO): string => {
  const units = Object.entries(record.assets).filter(
    ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
  );
  if (units.length !== 1) {
    throw new Error(
      `proof record ${outRefLabel(record)} carries ${units.length.toString()} tokens`,
    );
  }
  const { assetName } = fromUnit(units[0]![0]);
  if (assetName === null) throw new Error("proof record token has no name");
  return assetName;
};

const dedupe = (utxos: readonly UTxO[]): UTxO[] => [
  ...new Map(utxos.map((utxo) => [outRefLabel(utxo), utxo])).values(),
];

/** Build, evaluate, sign and submit one removal of a block of
 * `fixture`'s queue, signed by the prover's wallet. The returned promise
 * rejects with the evaluator's failure when a validator refuses it. */
export const submitRawFraudulentBlockRemoval = async (
  fixture: ProvedDoubleSpendFixture,
  removal: RawRemoval,
): Promise<TxSigned> => {
  const lucid = fixture.proverLucid;
  fixture.proverSigner.selectWallet(lucid);
  const document = deploymentDocument(fixture);
  // Category-independent: every address, script and policy a removal touches
  // comes from the deployment manifest; the category only names a token.
  const contracts = await buildRemovalContracts({
    blueprint: fixture.realBlueprint,
    deploymentInfo: document,
    network,
    fraudCategory: "doubleSpend",
  });
  const referenceScripts = await resolveReferenceScripts({
    lucid,
    deploymentInfo: parseContractDeploymentInfo(document),
    requireReferenceScripts: true,
  });
  const now = BigInt(fixture.emulator.now());
  const validFrom = now > 120_000n ? now - 120_000n : 0n;
  const validTo = now + 300_000n;
  const schedulerStartTime =
    BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(Number(validTo)))) - 1n;

  const topology = await loadStateQueueTopology({
    lucid,
    stateQueueAddress: contracts.stateQueueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  });
  const convicted = topology.nodeByHeaderHash.get(fixture.headerHash);
  const [anchor, removed] =
    removal.removes === "successor"
      ? [convicted, topology.successorByHeaderHash.get(fixture.headerHash)]
      : [topology.predecessorByHeaderHash.get(fixture.headerHash), convicted];
  if (anchor === undefined || removed === undefined) {
    throw new Error(`the queue has no ${removal.removes} block to remove`);
  }
  const operator = (
    await Effect.runPromise(SDK.getHeaderFromStateQueueDatum(removed.datum))
  ).operatorVkey;
  const correctionLock = await Effect.runPromise(
    SDK.fetchCorrectionLockUTxOProgram(lucid, {
      correctionLockAddress: contracts.correctionLockAddress,
      hubOraclePolicyId: contracts.hubOraclePolicyId,
    }),
  );
  const hubOracle = await lucid.utxoByUnit(
    toUnit(contracts.hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
  );
  const yieldReference = referenceScripts.stateQueueFraudRemovalWithdraw;

  // The removed operator's directory entries, read the way the honest slash
  // reads them.
  const directoryPlan =
    removal.operator === "untouched"
      ? undefined
      : await resolveOperatorSlashingPlan({
          lucid,
          contracts,
          operator,
          schedulerUtxo: await lucid.utxoByUnit(
            toUnit(contracts.schedulerPolicyId, SDK.SCHEDULER_ASSET_NAME),
          ),
          activeOperatorsRootUnit: toUnit(
            contracts.activeOperatorsPolicyId,
            SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
          ),
          retiredOperatorsRootUnit: toUnit(
            contracts.retiredOperatorsPolicyId,
            SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME,
          ),
        });
  const witnesses =
    directoryPlan?.approach === "OperatorAlreadySlashed"
      ? [directoryPlan.activeWitness.utxo, directoryPlan.retiredWitness.utxo]
      : undefined;
  if (removal.operator === "witnessed" && witnesses === undefined) {
    throw new Error("the removed operator is still in an operator directory");
  }

  let slash:
    | {
        readonly fee: bigint;
        readonly referenceInputs: readonly UTxO[];
        readonly component: Extract<
          ReturnType<ReturnType<typeof buildSlashingInputs>["buildSlashing"]>,
          { readonly kind: "slashActiveOperator" | "slashRetiredOperator" }
        >;
      }
    | undefined;
  if (typeof removal.operator === "object") {
    if (
      directoryPlan?.approach !== "SlashActiveOperator" &&
      directoryPlan?.approach !== "SlashRetiredOperator"
    ) {
      throw new Error("the removed operator has no directory node");
    }
    const economics = resolveFraudSlashEconomics(
      fraudSlashEconomicsFromDeploymentManifest(document),
      directoryPlan.removalPlan.node.utxo.assets.lovelace ?? 0n,
    );
    const registeredOperatorsElementUtxo =
      directoryPlan.approach === "SlashActiveOperator" &&
      directoryPlan.schedulerPlan.kind === "rewind"
        ? await resolveRegisteredOperatorRemovalWitness({
            utxos: await lucid.utxosAt(contracts.registeredOperatorsAddress),
            address: contracts.registeredOperatorsAddress,
            policyId: contracts.registeredOperatorsPolicyId,
            inclusiveValidityUpperBound: schedulerStartTime,
          })
        : undefined;
    const plan = buildSlashingInputs({
      plan: directoryPlan,
      operator,
      contracts,
      hubOracleUtxo: hubOracle,
      ...(registeredOperatorsElementUtxo === undefined
        ? {}
        : { registeredOperatorsElementUtxo }),
      fraudProverReward: {
        proverEnterpriseAddress: removal.operator.slashRewardTo,
        lovelace: economics.fraudProverRewardLovelace,
      },
      bondExcessLovelace: economics.excessLovelace,
    });
    const component = plan.buildSlashing(schedulerStartTime);
    if (
      component.kind !== "slashActiveOperator" &&
      component.kind !== "slashRetiredOperator"
    ) {
      throw new Error("the directory slash resolved to another arm");
    }
    slash = {
      fee: economics.exactFeeLovelace,
      referenceInputs: plan.additionalRefInputs,
      component,
    };
  }

  const continuedUnit = toUnit(contracts.stateQueuePolicyId, anchor.assetName);
  const continuedDatum = SDK.encodeLinkedListNodeView({
    ...anchor.datum,
    next: removal.removes === "successor" ? removed.datum.next : "Empty",
  });
  const lockDatum: SDK.CorrectionLockDatum =
    removal.removes === "successor"
      ? {
          Locked: {
            target_header_hash: fixture.headerHash,
            correction_identity: {
              FraudProof: {
                fraud_proof_asset_name: recordAssetName(removal.record),
              },
            },
          },
        }
      : "Idle";

  const slashingApproach = (
    ctx: Parameters<BuildTxWithRedeemer>[0],
  ): SDK.SlashingApproach => {
    switch (removal.approach) {
      case "DescendantOfValidationTraceConviction":
        return "DescendantOfValidationTraceConviction";
      case "SlashActiveOperator":
        return {
          SlashActiveOperator: {
            active_operators_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.activeOperatorsPolicyId,
              "raw removal active-operators burn",
            ),
          },
        };
      case "SlashRetiredOperator":
        return {
          SlashRetiredOperator: {
            retired_operators_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.retiredOperatorsPolicyId,
              "raw removal retired-operators burn",
            ),
          },
        };
      case "OperatorAlreadySlashed": {
        if (witnesses === undefined) {
          throw new Error(
            "OperatorAlreadySlashed needs the directory witnesses",
          );
        }
        return {
          OperatorAlreadySlashed: {
            active_operators_element_ref_input_index:
              SDK.requireReferenceInputIndex(
                ctx,
                witnesses[0]!,
                "raw removal active-operators witness",
              ),
            retired_operators_element_ref_input_index:
              SDK.requireReferenceInputIndex(
                ctx,
                witnesses[1]!,
                "raw removal retired-operators witness",
              ),
          },
        };
      }
    }
  };
  const stateQueueBurn = ((ctx) => {
    const continuedOutputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === contracts.stateQueueAddress &&
        (output.assets[continuedUnit] ?? 0n) === 1n,
      "raw removal continued queue node",
    );
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          yield_to_ref_input_index: SDK.requireReferenceInputIndex(
            ctx,
            yieldReference,
            "raw removal yield",
          ),
          fraudulent_operator: operator,
          fraudulent_blocks_header_hash: fixture.headerHash,
          slashing_approach: slashingApproach(ctx),
          fraud_proof_ref_input_index: SDK.requireReferenceInputIndex(
            ctx,
            removal.record,
            "raw removal proof record",
          ),
          block_removal_approach:
            removal.removes === "successor"
              ? {
                  RemoveFraudulentBlocksLink: {
                    fraudulent_node_input_outref: SDK.outputReferenceFromUTxO(
                      anchor.utxo,
                    ),
                    fraudulent_node_output_index: continuedOutputIndex,
                  },
                }
              : {
                  RemoveLastFraudulentBlock: {
                    anchor_element_input_outref: SDK.outputReferenceFromUTxO(
                      anchor.utxo,
                    ),
                    anchor_element_output_index: continuedOutputIndex,
                  },
                },
        },
      } satisfies SDK.StateQueueRedeemer,
      SDK.StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const correct = ((ctx) =>
    Data.to(
      {
        Correct: {
          hub_oracle_ref_input_index: SDK.requireReferenceInputIndex(
            ctx,
            hubOracle,
            "raw removal hub oracle",
          ),
        },
      } satisfies SDK.CorrectionLockRedeemer,
      SDK.CorrectionLockRedeemer,
    )) satisfies BuildTxWithRedeemer;

  let tx = lucid
    .newTx()
    .validFrom(Number(validFrom))
    .validTo(Number(validTo))
    .collectFrom(
      [anchor.utxo, removed.utxo],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .collectFrom([correctionLock.utxo], correct)
    .readFrom(
      dedupe([
        removal.record,
        hubOracle,
        ...(witnesses ?? []),
        ...(slash?.referenceInputs ?? []),
        ...Object.values(referenceScripts),
      ]),
    )
    .pay.ToContract(
      contracts.stateQueueAddress,
      { kind: "inline", value: continuedDatum },
      {
        ...anchor.utxo.assets,
        lovelace:
          (anchor.utxo.assets.lovelace ?? 0n) +
          (removed.utxo.assets.lovelace ?? 0n),
      },
    )
    .pay.ToContract(
      correctionLock.utxo.address,
      { kind: "inline", value: Data.to(lockDatum, SDK.CorrectionLockDatum) },
      correctionLock.utxo.assets,
    )
    .mintAssets(
      { [toUnit(contracts.stateQueuePolicyId, removed.assetName)]: -1n },
      stateQueueBurn,
    )
    .withdraw(
      SDK.scriptRewardAddress(
        network,
        contracts.stateQueueFraudRemovalWithdrawalScript,
      ),
      0n,
      SDK.encodeStateQueueYieldRedeemer(),
    );
  if (slash !== undefined) {
    const { component } = slash;
    const anchorOutput =
      component.kind === "slashActiveOperator"
        ? component.continuedActiveOperatorAnchorOutput
        : component.continuedRetiredOperatorAnchorOutput;
    tx =
      component.kind === "slashActiveOperator"
        ? tx
            .collectFrom(
              [...component.activeOperatorInputs],
              Data.to("ListStateTransition", SDK.ActiveOperatorSpendRedeemer),
            )
            .mintAssets(
              component.activeOperatorsAssetsToBurn,
              component.activeOperatorsMintRedeemer,
            )
        : tx
            .collectFrom([...component.retiredOperatorInputs], Data.void())
            .mintAssets(
              component.retiredOperatorsAssetsToBurn,
              component.retiredOperatorsMintRedeemer,
            );
    if (anchorOutput !== undefined) {
      tx = tx.pay.ToContract(
        anchorOutput.address,
        { kind: "inline", value: anchorOutput.datum },
        anchorOutput.assets,
      );
    }
    if (
      component.kind === "slashActiveOperator" &&
      component.schedulerSpend !== undefined
    ) {
      const scheduler = component.schedulerSpend;
      tx = tx.pay
        .ToContract(
          scheduler.continuedOutput.address,
          { kind: "inline", value: scheduler.continuedOutput.datum },
          scheduler.continuedOutput.assets,
        )
        .collectFrom([scheduler.input], scheduler.redeemer);
    }
    const reward = component.fraudProverReward;
    if (reward !== undefined) {
      tx = tx.pay.ToAddress(reward.proverEnterpriseAddress, {
        lovelace: reward.lovelace,
      });
    }
  }
  if (removal.payRewardShapedOutput === true) {
    tx = tx.pay.ToAddress(proverEnterpriseAddress(fixture), {
      lovelace: SDK.getProtocolParameters(network).fraud_prover_reward,
    });
  }
  // A slash is bond-backed: the bond pays the fee exactly and no wallet
  // input joins. A bondless removal is balanced by the wallet.
  const unsigned =
    slash === undefined
      ? await tx.complete({ localUPLCEval: true })
      : await tx
          .setMinFee(slash.fee)
          .complete({ coinSelection: false, localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
  return signed;
};
