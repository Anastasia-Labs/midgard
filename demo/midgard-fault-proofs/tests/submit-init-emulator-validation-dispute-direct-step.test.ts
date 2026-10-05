import {
  validationMachineStateDataFromCore,
  ValidationResolutionDatum,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { selectFeeInput } from "../src/step-support.js";
import { makeGameHandoffRedeemer } from "../src/validation-dispute/submit/redeemers.js";
import {
  expectOnchainRefusal,
  type RefusalPin,
} from "./support/emulator/expect-onchain-refusal.js";
import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

const phases = [
  "canonicalDecode",
  "compactBinding",
  "staticLedgerRules",
  "inputSets",
  // Signatures remains unresolved until address-item successor uniqueness is proved.
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
  "resolveInputs",
  "scriptSources",
  "nativeScripts",
  "scriptIntegrity",
  "valueAndMint",
  "ledgerDelta",
] as const;

it.each(phases)(
  "proves an incorrect committed %s successor without bisection",
  async (phase) => {
    const result = await runForcedValidationDisputeScenario(
      (input) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          ...input,
          disputedPhase: phase,
        }),
      {
        directCommittedStep: true,
        ...(phase === "phaseANativeScripts"
          ? { phaseANativeItemYieldKind: "native" as const }
          : {}),
        onSubmittedTransaction: (measurement) => {
          expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
          expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
          expect(measurement.executionSteps).toBeLessThanOrEqual(
            8_000_000_000n,
          );
        },
      },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  600_000,
);

it.each(phases)(
  "refuses a forged direct successor against an honest %s trace",
  async (phase) => {
    const failure = await expectOnchainRefusal(() =>
      runForcedValidationDisputeScenario(
        (input) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            ...input,
            disputedPhase: phase,
            dishonestChallenger: true,
          }),
        {
          directCommittedStep: true,
          ...(phase === "phaseANativeScripts"
            ? { phaseANativeItemYieldKind: "native" as const }
            : {}),
        },
      ),
    );
    expect(failure).toContain("semantic-resolution");
  },
  600_000,
);

const expectDirectGameClaimRefusal = async (
  {
    phase,
    resolverIndex,
    authorizeProver,
  }: {
    readonly phase: "valueAndMint" | "signatures";
    readonly resolverIndex: number;
    readonly authorizeProver: boolean;
  },
  pin: RefusalPin,
) => {
  const staged = await runForcedValidationDisputeScenario(
    (input) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        ...input,
        disputedPhase: phase,
      }),
    { stopAfter: "source" },
  );
  const context = staged.gameContext!;
  const [txHash, outputIndex] =
    context.sourceResult.nextThreadOutRef.split("#");
  const [thread] = await context.lucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(outputIndex) },
  ]);
  if (thread === undefined) throw new Error("staged game thread disappeared");
  const { lowIndex, highIndex } = context.fixture.evidence.finalDispute;
  const evidence = {
    pre_state: validationMachineStateDataFromCore(
      context.fixture.operatorTrace.states[lowIndex]!,
    ),
    pre_proof: validationTraceProofDataFromCore(
      context.fixture.operatorTrace.tree.proofs[lowIndex]!,
    ),
    post_proof: validationTraceProofDataFromCore(
      context.fixture.operatorTrace.tree.proofs[highIndex]!,
    ),
    challenger_successor_hash: "ef".repeat(32),
  };
  const target =
    context.contracts.fraudProofContracts.validationTraceDispute
      .prepareResolvers[resolverIndex]!;
  const datum = Data.to(
    {
      fraud_prover: context.signer.paymentKeyHash,
      data: {
        version: 1n,
        pre_state: evidence.pre_state,
        operator_successor_hash: evidence.post_proof.state_hash,
        challenger_successor_hash: evidence.challenger_successor_hash,
      },
    },
    ValidationResolutionDatum,
  );
  context.signer.selectWallet(context.lucid);
  const feeInput = selectFeeInput(await context.lucid.wallet().getUtxos());
  const unit = context.initResult.computationThreadUnit;
  const range = context.validityRange();
  await expectOnchainRefusal(
    () =>
      context.lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom(
          [thread],
          makeGameHandoffRedeemer({
            threadUtxo: thread,
            outputAddress: target.spendingScriptAddress,
            outputDatum: datum,
            threadUnit: unit,
            destination: "resolution",
            committedStep: { resolverIndex, evidence },
            onLayout: () => {},
          }),
        )
        .readFrom([context.gameReferenceScriptUtxo])
        .pay.ToContract(
          target.spendingScriptAddress,
          { kind: "inline", value: datum },
          { lovelace: thread.assets.lovelace!, [unit]: 1n },
        )
        .validFrom(range.validFrom)
        .validTo(range.validTo)
        .compose(
          authorizeProver
            ? context.lucid.newTx().addSignerKey(context.signer.paymentKeyHash)
            : context.lucid.newTx(),
        )
        .complete({ localUPLCEval: true }),
    pin,
  );
};

it("refuses an unauthorized direct claim that would strand an active dispute", async () => {
  await expectDirectGameClaimRefusal(
    { phase: "valueAndMint", resolverIndex: 12, authorizeProver: false },
    {
      refusedBy: "fraud_proofs/validation_trace/game_v1",
      check: /expect fraud_prover.*utils.has_signed/su,
    },
  );
}, 600_000);

it("refuses a signed direct Signatures claim while successor uniqueness is unresolved", async () => {
  await expectDirectGameClaimRefusal(
    { phase: "signatures", resolverIndex: 4, authorizeProver: true },
    {
      refusedBy: "fraud_proofs/validation_trace/game_v1",
      check:
        /expect evidence.pre_state.phase != validation_trace_v1.Signatures/su,
    },
  );
}, 600_000);

it("refuses Signatures at the direct submitter while successor uniqueness is unresolved", async () => {
  await expect(
    runForcedValidationDisputeScenario(
      (input) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          ...input,
          disputedPhase: "signatures",
        }),
      { directCommittedStep: true },
    ),
  ).rejects.toThrow(
    /Direct committed-step resolution is unavailable for validation phase Signatures/u,
  );
}, 600_000);
