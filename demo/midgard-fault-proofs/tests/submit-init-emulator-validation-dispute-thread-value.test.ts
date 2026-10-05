import {
  FraudProofTokenDatum,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ValidationAwardSpendRedeemer,
  validationMachineStateDataFromCore,
  ValidationResolutionDatum,
  validationTraceProofDataFromCore,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import { Data, generateEmulatorAccount, Lucid } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  resolveValidationTraceDisputeDeploymentContracts,
  submitValidationDisputeAward,
} from "../src/index.js";
import { outputWithDatumAndUnitPredicate } from "../src/tx-layout.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
} from "../src/validation-dispute/submit/redeemers.js";
import { makeTerminalPaddingRedeemer } from "../src/validation-dispute/submit/resolution.submit-validation-dispute-award-terminal-padding.js";
import {
  makePrepareResolutionRedeemer,
  validationResolverIndex,
} from "../src/validation-dispute/submit/resolution.submit-validation-dispute-enter-resolution.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildTerminalPaddingFixture } from "./support/emulator/validation-dispute-fixtures.terminal-padding.js";
import {
  buildHonestAcceptedValidationDisputeFixture,
  network,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

for (const terminal of [false, true]) {
  it(`${terminal ? "terminal award" : "ordinary preparation"} preserves thread ADA against a permissionless spender and permits separately funded topups`, async () => {
    const result = await runForcedValidationDisputeScenario(
      terminal
        ? buildTerminalPaddingFixture
        : buildHonestAcceptedValidationDisputeFixture,
      { stopAfter: "enter-resolution" },
    );
    const context = result.resolutionContext!;
    const { contracts } =
      await resolveValidationTraceDisputeDeploymentContracts({
        blueprint: context.blueprint,
        deploymentInfo: context.deploymentInfo,
        network,
      });
    const [threadHash, threadIndex] =
      context.resolutionResult.nextThreadOutRef.split("#");
    const [thread] = await context.lucid.utxosByOutRef([
      { txHash: threadHash!, outputIndex: Number(threadIndex) },
    ]);
    if (thread === undefined)
      throw new Error("Authenticated boundary thread missing");
    const unit = Object.keys(thread.assets).find(
      (asset) => asset !== "lovelace",
    )!;
    const inputAda = thread.assets.lovelace!;
    // A genuine boundary datum is larger than the reduced successor datum.
    // Any remaining thread ADA is still the prover's funding, not a bounty for
    // whoever supplies the public continuation witness.
    expect(inputAda).toBeGreaterThan(3_000_000n);
    const attacker = generateEmulatorAccount({ lovelace: 0n });
    context.signer.selectWallet(context.lucid);
    const funding = await context.lucid
      .newTx()
      .pay.ToAddress(attacker.address, { lovelace: 20_000_000n })
      .complete();
    const fundHash = await (
      await funding.sign.withWallet().complete()
    ).submit();
    await context.lucid.awaitTx(fundHash);
    const attackerLucid = await Lucid(context.emulator, "Custom", {
      slotConfig: context.lucid.config().slotConfig!,
    });
    attackerLucid.selectWallet.fromSeed(attacker.seedPhrase);
    const low = context.fixture.evidence.finalDispute.lowIndex;
    const high = context.fixture.evidence.finalDispute.highIndex;
    const preState = validationMachineStateDataFromCore(
      context.fixture.operatorTrace.states[low]!,
    );
    const operatorPost = validationTraceProofDataFromCore(
      context.fixture.operatorTrace.tree.proofs[high]!,
    );
    const challengerPost = validationTraceProofDataFromCore(
      context.fixture.challengerTrace.tree.proofs[high]!,
    );
    const resolverIndex = terminal
      ? 0
      : validationResolverIndex(preState.phase);
    const destination = terminal
      ? contracts.validationTraceDispute.award
      : contracts.validationTraceDispute.resolvers[resolverIndex]!;
    const outputDatum = terminal
      ? Data.to(
          {
            fraud_prover: context.signer.paymentKeyHash,
            data: { version: 1n },
          },
          WinningValidationResolutionDatum,
        )
      : Data.to(
          {
            fraud_prover: context.signer.paymentKeyHash,
            data: {
              version: 1n,
              pre_state: preState,
              operator_successor_hash: operatorPost.state_hash,
              challenger_successor_hash: challengerPost.state_hash,
            },
          },
          ValidationResolutionDatum,
        );
    const spend = async (successorAda: bigint, submit: boolean) => {
      const makeRedeemer = terminal
        ? makeTerminalPaddingRedeemer({
            threadUtxo: thread,
            outputAddress: destination.spendingScriptAddress,
            outputDatum,
            threadUnit: unit,
            terminalState: preState,
            onLayout: () => {},
          })
        : makePrepareResolutionRedeemer({
            threadUtxo: thread,
            outputAddress: destination.spendingScriptAddress,
            outputDatum,
            threadUnit: unit,
            resolverIndex: BigInt(resolverIndex),
            preState,
            operatorPost,
            challengerPost,
            onLayout: () => {},
          });
      const feeInputs = await attackerLucid.wallet().getUtxos();
      const range = context.validityRange();
      const tx = await attackerLucid
        .newTx()
        .collectFrom([feeInputs[0]!])
        .collectFrom([thread], makeRedeemer)
        .readFrom([context.boundaryReferenceScriptUtxo])
        .pay.ToContract(
          destination.spendingScriptAddress,
          { kind: "inline", value: outputDatum },
          { lovelace: successorAda, [unit]: 1n },
        )
        .validFrom(range.validFrom)
        .validTo(range.validTo)
        .complete({ localUPLCEval: true });
      if (!submit) return undefined;
      const hash = await (await tx.sign.withWallet().complete()).submit();
      await attackerLucid.awaitTx(hash);
      const [next] = await attackerLucid.utxosAtWithUnit(
        destination.spendingScriptAddress,
        unit,
      );
      expect(next?.assets.lovelace).toBe(successorAda);
      return next!;
    };
    await expectOnchainRefusal(() => spend(2_000_000n, true));
    // No prover signature is present. The outsider can continue only when its
    // own wallet funds fees and the successor preserves the full thread value.
    const next = await spend(inputAda + 2_000_000n, true);
    if (terminal) {
      const proofUnit = contracts.fraudProof.policyId + unit.slice(56);
      const proofDatum = Data.to(
        { fraud_prover: context.signer.paymentKeyHash },
        FraudProofTokenDatum,
      );
      const feeInputs = await attackerLucid.wallet().getUtxos();
      const range = context.validityRange();
      await expectOnchainRefusal(async () =>
        attackerLucid
          .newTx()
          .collectFrom([feeInputs[0]!])
          .collectFrom([next!], (ctx) => {
            requireOwnSpendPurpose(ctx, next!, "adversarial award");
            return Data.to(
              {
                Continue: [
                  {
                    input_index: requireInputIndex(
                      ctx,
                      next!,
                      "adversarial award",
                    ),
                    output_index: requireUniqueOutputIndex(
                      ctx.outputs,
                      outputWithDatumAndUnitPredicate({
                        address: contracts.fraudProof.spendingScriptAddress,
                        datum: proofDatum,
                        unit: proofUnit,
                      }),
                      "adversarial permanent record",
                    ),
                    fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
                      ctx,
                      contracts.fraudProof.policyId,
                      "adversarial proof mint",
                    ),
                  },
                ],
              },
              ValidationAwardSpendRedeemer,
            );
          })
          .readFrom([
            context.awardReferenceScriptUtxo,
            context.witnessReferenceScripts.computationThreadMint!,
            context.witnessReferenceScripts.fraudProofMint!,
          ])
          .mintAssets(
            { [unit]: -1n },
            makeComputationThreadSuccessRedeemer({
              computationThreadPolicyId: contracts.computationThread.policyId,
              computationThreadAssetName: unit.slice(56),
            }),
          )
          .mintAssets(
            { [proofUnit]: 1n },
            makeFraudProofMintRedeemer({
              fraudProofPolicyId: contracts.fraudProof.policyId,
              computationThreadPolicyId: contracts.computationThread.policyId,
              computationThreadAssetName: unit.slice(56),
              onComputationThreadMintRedeemerIndex: () => {},
            }),
          )
          .pay.ToContract(
            contracts.fraudProof.spendingScriptAddress,
            { kind: "inline", value: proofDatum },
            { lovelace: 2_000_000n, [proofUnit]: 1n },
          )
          .validFrom(range.validFrom)
          .validTo(range.validTo)
          .complete({ localUPLCEval: true }),
      );
      const award = await submitValidationDisputeAward({
        ...context,
        network,
        threadOutRef: `${next!.txHash}#${next!.outputIndex}`,
        validityRange: context.validityRange(),
      });
      const [record] = await context.lucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        award.fraudProofUnit,
      );
      expect(record?.assets.lovelace).toBe(inputAda + 2_000_000n);
    }
  }, 600_000);
}
