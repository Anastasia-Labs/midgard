/**
 * `executionSourceScriptDecoding` custody refuses a forged successor at every
 * role: from a genuine Init, step-01 is first built paying the event
 * selection the state the terminal reads, and refused; the event selection
 * is built paying the terminal the state step-01 pays, and refused before
 * the honest transition advances the thread; the terminal is built paying
 * the fraud-proof token a datum naming another prover and refused before the
 * honest terminal convicts. Every forgery is a raw transaction built against
 * the real role scripts.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildDirectPurposeTransition,
  type DirectPurposeContracts,
  type DirectPurposeRole,
  submitDirectPurposeTransition,
} from "../src/direct-purpose-family/index.js";
import { submitNativeTxBinding } from "../src/native-tx-binding.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.build-source-membership-proof.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../src/witness-reference-scripts.js";
import type { DirectPurposeHarness } from "./support/direct-purpose-emulator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionSourceScriptDecodingThread,
  makeExecutionSourceScriptDecodingHarness,
  publishExecutionSourceScriptDecodingRoles,
} from "./support/execution-source-script-decoding-emulator.js";
import {
  honestForcedObserverShape,
  type MissingRedeemerScenario,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { network } from "./support/submit-init-emulator-shared.js";

/** Step-01 from the Init `thread`, paying `bindEvent` the `forged` datum. */
const forgedStep01 = async (
  harness: DirectPurposeHarness,
  scenario: MissingRedeemerScenario,
  reference: UTxO,
  thread: UTxO,
  unit: string,
  forged: string,
) => {
  const contracts: DirectPurposeContracts = harness.direct;
  const signer = harness.proverSigner;
  if (scenario.block.txInclusion !== null)
    return await submitNativeTxBinding({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts,
      signer,
      stepIndex: 0,
      threadUtxo: thread,
      threadToken: { unit, fraudulentHeaderHash: scenario.setup.headerHash },
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.block.txInclusion,
      nextDatum: forged,
      spendRedeemerSchema: SDK.DirectStep01Redeemer,
      wrapInclusionCarriage: (inclusion) => ({
        source: { AcceptedSource: { inclusion } },
      }),
      referenceScriptUtxo: reference,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  const next = contracts.steps[1]!;
  const { eventKey } = scenario.material;
  if (!("ForcedTransactionEventKey" in eventKey))
    throw new Error("forged forced step-01: subject is not forced");
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: scenario.block.reconstruction,
    eventKey,
  });
  const label = "forged forced step-01";
  const redeemer = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: SDK.requireInputIndex(ctx, thread, label),
                output_index: SDK.requireUniqueOutputIndex(
                  ctx.outputs,
                  computationThreadOutputPredicate({
                    address: next.spendingScriptAddress,
                    datum: forged,
                    unit,
                  }),
                  label,
                ),
                header: scenario.material.sourceBound.header,
                membership: SDK.forcedLeafMembershipV1(membership),
              },
            },
          },
        ],
      },
      SDK.DirectStep01Redeemer,
    )) satisfies BuildTxWithRedeemer;
  signer.selectWallet(harness.proverLucid);
  return await harness.proverLucid
    .newTx()
    .collectFrom([thread], redeemer)
    .readFrom([reference])
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: forged },
      thread.assets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
};

/** The terminal from `thread`, minting the fraud-proof token to an output
 * whose datum names another prover. */
const forgedTerminal = async (
  harness: DirectPurposeHarness,
  reference: UTxO,
  thread: UTxO,
  unit: string,
) => {
  const contracts: DirectPurposeContracts = harness.direct;
  const signer = harness.proverSigner;
  const assetName = unit.slice(56);
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, assetName);
  const forged = Data.to(
    { fraud_prover: "ee".repeat(28) },
    SDK.FraudProofTokenDatum,
  );
  expect(forged).not.toBe(
    Data.to({ fraud_prover: signer.paymentKeyHash }, SDK.FraudProofTokenDatum),
  );
  const label = "forged terminal";
  const spend = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(ctx, thread, label),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === contracts.fraudProof.spendingScriptAddress,
              label,
            ),
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              label,
            ),
          },
        ],
      },
      SDK.DirectClaimFinalizeRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = (() =>
    Data.to(
      { Success: { burning_token_asset_name: assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) =>
    Data.to(
      {
        computation_thread_token_asset_name: assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          label,
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burnCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: `${label} burn`,
  });
  const mintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: `${label} mint`,
  });
  signer.selectWallet(harness.proverLucid);
  return await harness.proverLucid
    .newTx()
    .collectFrom([thread], spend)
    .mintAssets({ [unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: forged },
      { lovelace: thread.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([
      reference,
      ...burnCarriage.referenceInputs,
      ...mintCarriage.referenceInputs,
    ])
    .complete({ localUPLCEval: true });
};

describe("execution-source-script-decoding forged successor refusal", () => {
  it.each([
    "ExecutionNativeScriptMalformed",
    "ExecutionNativeScriptNodeLimit",
    "ExecutionNativeScriptDepthLimit",
  ] as const)(
    "refuses a forged successor at every role of a rejection recording %s",
    async (arm) => {
      const harness = await makeExecutionSourceScriptDecodingHarness();
      const references =
        await publishExecutionSourceScriptDecodingRoles(harness);
      const reference = (role: DirectPurposeRole) => references.get(role)!.utxo;
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape: honestForcedObserverShape(5),
        source: {
          kind: "forced",
          verdict: {
            ForcedTxInvalid: {
              reason: {
                [arm]: { purpose_kind: 2n, purpose_index: 0n },
              } as SDK.RejectionReason,
            },
          },
        },
      });
      const refused: DirectPurposeRole[] = [];
      const run = await driveExecutionSourceScriptDecodingThread({
        harness,
        scenario,
        references,
        transitions: () => [],
        beforeStep01: async ({ thread, unit, prepared }) => {
          // The state the terminal reads, one role early.
          await expectOnchainRefusal(() =>
            forgedStep01(
              harness,
              scenario,
              reference("step01"),
              thread,
              unit,
              prepared.transitions[0]!.datum,
            ),
          );
          refused.push("step01");
        },
      });
      const [transition] = run.prepared.transitions;
      expect(run.prepared.transitions).toHaveLength(1);
      // The state step-01 paid, carried on to the terminal.
      const forged = run.thread.datum!;
      expect(forged).not.toBe(transition!.datum);
      const built = await buildDirectPurposeTransition(
        run.custody,
        run.thread,
        transition!,
        {
          datum: forged,
          redeemer: (args) =>
            transition!.redeemer({
              ...args,
              ctx: {
                ...args.ctx,
                outputs: args.ctx.outputs.map((output) =>
                  output.datum === forged
                    ? { ...output, datum: transition!.datum }
                    : output,
                ),
              },
            }),
        },
      );
      await expectOnchainRefusal(() => built.complete());
      const { thread } = await submitDirectPurposeTransition(
        run.custody,
        run.thread,
        transition!,
      );
      refused.push(transition!.role);
      await expectOnchainRefusal(() =>
        forgedTerminal(harness, reference("terminal"), thread, run.unit),
      );
      refused.push("terminal");
      const outcome = await run.terminal(true, thread);
      expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
      expect(new Set(refused)).toStrictEqual(
        new Set(SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER),
      );
    },
    2_400_000,
  );
});
