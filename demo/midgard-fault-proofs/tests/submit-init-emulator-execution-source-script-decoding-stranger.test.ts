/**
 * `executionSourceScriptDecoding` steps against a party that is not the
 * thread's prover. An honest prover opens the thread from a genuine Init and
 * step-01; a funded stranger then builds the event selection directly
 * against the chain, keeping the honest prover as the thread's
 * `fraud_prover` and signing only with its own key. The validator refuses
 * the stranger, and the honest prover's own transition then lands on the
 * untouched thread and the thread convicts.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  generateEmulatorAccount,
  Lucid,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  directPurposeRoleValidator,
  type DirectPurposeTransition,
  submitDirectPurposeTransition,
} from "../src/direct-purpose-family/index.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY } from "../src/execution-source-script-decoding/index.js";
import { faultProofFieldOpening } from "../src/field-opening.js";
import {
  getCompiledScript,
  phasMembershipRewardAddress,
  resolveProverSigner,
} from "../src/runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../src/step-support.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionSourceScriptDecodingThread,
  makeExecutionSourceScriptDecodingHarness,
  publishExecutionSourceScriptDecodingRoles,
} from "./support/execution-source-script-decoding-emulator.js";
import {
  honestForcedObserverShape,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { network } from "./support/submit-init-emulator-shared.js";

const STRANGER_FUNDING_LOVELACE = 1_000_000_000n;

describe("execution-source-script-decoding steps against a stranger key", () => {
  it("refuses a stranger at the event selection, and the prover's own step lands", async () => {
    const harness = await makeExecutionSourceScriptDecodingHarness();
    const references = await publishExecutionSourceScriptDecodingRoles(harness);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: honestForcedObserverShape(5),
      source: {
        kind: "forced",
        verdict: {
          ForcedTxInvalid: {
            reason: {
              ExecutionNativeScriptMalformed: {
                purpose_kind: 2n,
                purpose_index: 0n,
              },
            },
          },
        },
      },
    });
    const run = await driveExecutionSourceScriptDecodingThread({
      harness,
      scenario,
      references,
      transitions: () => [],
    });

    const stranger = generateEmulatorAccount({ lovelace: 0n });
    const strangerLucid = await Lucid(harness.emulator, "Custom");
    const strangerSigner = resolveProverSigner({
      network,
      walletSeedPhrase: stranger.seedPhrase,
    });
    strangerSigner.selectWallet(strangerLucid);
    const funding = await harness.funderLucid
      .newTx()
      .pay.ToAddress(strangerSigner.address, {
        lovelace: STRANGER_FUNDING_LOVELACE,
      })
      .complete();
    await harness.funderLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    expect(strangerSigner.paymentKeyHash).not.toBe(
      harness.proverSigner.paymentKeyHash,
    );
    const phasReward = phasMembershipRewardAddress(network, {
      type: "PlutusV3",
      script: getCompiledScript(
        harness.realBlueprint,
        PHAS_MEMBERSHIP_WITHDRAW_TITLE,
      ),
    } satisfies Script);

    /** The planned transition, built and signed by the stranger alone. */
    const strangerTransition = async (
      thread: UTxO,
      transition: DirectPurposeTransition,
    ) => {
      const next = directPurposeRoleValidator(
        EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY,
        harness.essd,
        transition.next,
      );
      const label = `stranger ${transition.label}`;
      const redeemer = ((ctx) =>
        transition.redeemer({
          indices: {
            input_index: SDK.requireInputIndex(ctx, thread, label),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              computationThreadOutputPredicate({
                address: next.spendingScriptAddress,
                datum: transition.datum,
                unit: run.unit,
              }),
              label,
            ),
          },
          open: (field) =>
            faultProofFieldOpening({
              planned: run.custody.plan(field).openingPlan,
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId:
                harness.essd.fieldPreimageCertificatePolicyId,
              label: `${label} field ${field.toString()}`,
            }),
          ctx,
          thread,
          unit: run.unit,
          nextAddress: next.spendingScriptAddress,
          membershipRewardAddress: phasReward,
        })) satisfies BuildTxWithRedeemer;
      let tx = strangerLucid
        .newTx()
        .collectFrom([thread], redeemer)
        .readFrom([
          references.get(transition.role)!.utxo,
          ...transition.fields.flatMap((field) => run.custody.carriage(field)),
          ...(transition.membership === undefined
            ? []
            : [harness.witnessReferenceScripts.phasMembershipWithdraw!]),
        ])
        .pay.ToContract(
          next.spendingScriptAddress,
          { kind: "inline", value: transition.datum },
          thread.assets,
        )
        .addSignerKey(strangerSigner.paymentKeyHash);
      if (transition.membership !== undefined)
        tx = tx.withdraw(phasReward, 0n, transition.membership.redeemer);
      const signed = await (await tx.complete({ localUPLCEval: true })).sign
        .withWallet()
        .complete();
      await strangerLucid.awaitTx(await signed.submit());
    };

    const [transition] = run.prepared.transitions;
    expect(transition!.role).toBe("bindEvent");
    await expectOnchainRefusal(() =>
      strangerTransition(run.thread, transition!),
    );
    const { thread } = await submitDirectPurposeTransition(
      run.custody,
      run.thread,
      transition!,
    );
    const outcome = await run.terminal(true, thread);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  }, 2_400_000);
});
