import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { faultProofFieldOpening } from "../../src/field-opening.js";
import { requireLinearFaultThreadUtxo } from "../../src/linear-fault-family.js";
import {
  type NonExistentInputForcedContracts,
  nonExistentInputForcedFieldPlan,
  nonExistentInputForcedStates,
} from "../../src/non-existent-input/submit.js";
import type { PreparedNonExistentInputWrongfulRejection } from "../../src/non-existent-input/wrongful-rejection.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { selectFeeInput } from "../../src/submit-step-01.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";

/** Deliberately bypasses local evidence admission; validator refusals must
 * remain observable for a malicious challenger submitting raw redeemers. */
export const submitRawNonExistentInputForcedStep = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  stepIndex,
  references,
  mutateArgs,
}: {
  lucid: LucidEvolution;
  contracts: NonExistentInputForcedContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  prepared: PreparedNonExistentInputWrongfulRejection;
  stepIndex: 0 | 1 | 2 | 3;
  references: {
    steps: readonly UTxO[];
    computationThreadMint: UTxO;
    fraudProofMint: UTxO;
  };
  mutateArgs?: (args: unknown) => unknown;
}) => {
  signer.selectWallet(lucid);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "non-existent-input",
    stepIndex,
    threadOutRef,
  });
  const states = nonExistentInputForcedStates(prepared);
  const schemas = [
    SDK.NonExistentInputStep02ThreadDatum,
    SDK.NonExistentInputStep03ThreadDatum,
    SDK.NonExistentInputStep04ThreadDatum,
  ];
  const terminal = stepIndex === 3;
  const unit = terminal
    ? toUnit(contracts.fraudProof.policyId, threadToken.assetName)
    : threadToken.unit;
  const address = terminal
    ? contracts.fraudProof.spendingScriptAddress
    : contracts.steps[stepIndex + 1]!.spendingScriptAddress;
  const datum = terminal
    ? Data.to({ fraud_prover: signer.paymentKeyHash }, SDK.FraudProofTokenDatum)
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: states[stepIndex],
        } as never,
        schemas[stepIndex] as never,
      );
  const matches = computationThreadOutputPredicate({ address, datum, unit });
  let outputIndex = 0n;
  const redeemer = ((ctx) => {
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      "raw no-input",
    );
    const indices = {
      input_index: SDK.requireInputIndex(ctx, threadUtxo, "raw no-input"),
      output_index: outputIndex,
    };
    const args =
      stepIndex === 0
        ? { source: { ForcedSource: { ...prepared.forcedSource, ...indices } } }
        : stepIndex === 1
          ? {
              ForcedArgs: {
                ...indices,
                spend_inputs_opening: faultProofFieldOpening({
                  planned: nonExistentInputForcedFieldPlan(
                    prepared,
                    signer.paymentKeyHash,
                  ),
                  label: "raw no-input",
                }),
                event_membership: prepared.eventMembership,
              },
            }
          : stepIndex === 2
            ? {
                ForcedArgs: {
                  ...indices,
                  transition_membership: prepared.transitionMembership,
                },
              }
            : {
                ForcedArgs: {
                  ...indices,
                  fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
                    ctx,
                    contracts.fraudProof.policyId,
                    "raw no-input",
                  ),
                  membership:
                    prepared.ledgerMembership === null
                      ? null
                      : {
                          value_hash: computeHash32(
                            Buffer.from(prepared.ledgerMembership.value, "hex"),
                          ).toString("hex"),
                          proof: prepared.ledgerMembership.proof,
                        },
                },
              };
    const schema = [
      SDK.NonExistentInputStep01SpendRedeemer,
      SDK.NonExistentInputStep02ThreadSpendRedeemer,
      SDK.NonExistentInputStep03ThreadSpendRedeemer,
      SDK.NonExistentInputStep04ThreadSpendRedeemer,
    ][stepIndex];
    return Data.to(
      { Continue: [mutateArgs?.(args) ?? args] } as never,
      schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  let builder = lucid
    .newTx()
    .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      references.steps[stepIndex]!,
      ...(terminal
        ? [references.computationThreadMint, references.fraudProofMint]
        : []),
    ])
    .pay.ToContract(
      address,
      { kind: "inline", value: datum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [unit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  if (terminal)
    builder = builder
      .mintAssets(
        { [threadToken.unit]: -1n },
        Data.to(
          { Success: { burning_token_asset_name: threadToken.assetName } },
          SDK.FraudProofComputationThreadRedeemer,
        ),
      )
      .mintAssets({ [unit]: 1n }, ((ctx) =>
        Data.to(
          {
            computation_thread_token_asset_name: threadToken.assetName,
            computation_thread_mint_redeemer_index:
              SDK.requireMintRedeemerIndex(
                ctx,
                contracts.computationThread.policyId,
                "raw no-input",
              ),
          },
          SDK.FraudProofTokenMintRedeemer,
        )) satisfies BuildTxWithRedeemer);
  const unsigned = await builder.complete({ localUPLCEval: true });
  const txHash = await (await unsigned.sign.withWallet().complete()).submit();
  await lucid.awaitTx(txHash);
  return `${txHash}#${outputIndex}`;
};
