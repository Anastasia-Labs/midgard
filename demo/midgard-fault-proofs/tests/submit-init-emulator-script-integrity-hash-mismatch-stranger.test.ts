/**
 * `scriptIntegrityHashMismatch` steps against a party that is not the
 * thread's prover. An honest prover opens the thread from a genuine Init and
 * step-01; at every later role a funded stranger builds that role's planned
 * transition directly against the chain, keeping the honest prover as the
 * thread's `fraud_prover` and signing only with its own key. The role's
 * validator refuses the stranger, and the honest prover's own transition then
 * lands on the untouched thread.
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

import { faultProofFieldOpening } from "../src/field-opening.js";
import {
  getCompiledScript,
  phasMembershipRewardAddress,
  resolveProverSigner,
} from "../src/runtime.js";
import {
  scriptIntegrityHashMismatchContractRoles,
  type ScriptIntegrityHashMismatchRole,
  type ScriptIntegrityHashMismatchTransition,
  submitScriptIntegrityHashMismatchTransition,
} from "../src/script-integrity-hash-mismatch/index.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../src/step-support.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveScriptIntegrityHashMismatchThread,
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
} from "./support/script-integrity-hash-mismatch-emulator.js";
import {
  nativeScript,
  plutusScript,
  scriptLockedEntries,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { network } from "./support/submit-init-emulator-shared.js";

const STRANGER_FUNDING_LOVELACE = 1_000_000_000n;

describe("script-integrity-hash-mismatch steps against a stranger key", () => {
  it("refuses a stranger at every role after step-01, and the prover's own step lands", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    const spent = plutusScript(31);
    const scenario = await setupIntegrityScenario({
      harness,
      shape: {
        spend: scriptLockedEntries(0x61, [spent]),
        witnesses: [spent],
        spendRedeemers: [0n],
        mints: [{ script: plutusScript(32), redeemer: true }],
        observers: [{ script: nativeScript(4n), redeemer: false }],
        receives: [{ script: plutusScript(33), redeemer: true }],
        integrity: WRONG_HASH,
      },
      source: { kind: "normal" },
    });
    const run = await driveScriptIntegrityHashMismatchThread({
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

    const roles = scriptIntegrityHashMismatchContractRoles(
      harness.scriptIntegrityHashMismatch,
    );
    const phasScript: Script = {
      type: "PlutusV3",
      script: getCompiledScript(
        harness.realBlueprint,
        PHAS_MEMBERSHIP_WITHDRAW_TITLE,
      ),
    };
    const phasReward = phasMembershipRewardAddress(network, phasScript);

    /** The planned transition, built and signed by the stranger alone. */
    const strangerTransition = async (
      thread: UTxO,
      transition: ScriptIntegrityHashMismatchTransition,
    ) => {
      const next = roles[transition.next];
      const label = `stranger ${transition.label}`;
      const carriage = transition.fields.flatMap((field) =>
        run.custody.carriage(field),
      );
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
              planned: run.prepared.plan(field).openingPlan,
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId:
                harness.scriptIntegrityHashMismatch
                  .fieldPreimageCertificatePolicyId,
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
          ...carriage,
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

    let thread = run.thread;
    const refused: ScriptIntegrityHashMismatchRole[] = [];
    for (const transition of run.prepared.transitions) {
      await expectOnchainRefusal(() => strangerTransition(thread, transition));
      refused.push(transition.role);
      thread = (
        await submitScriptIntegrityHashMismatchTransition(
          run.custody,
          thread,
          transition,
        )
      ).thread;
    }
    expect(new Set(refused)).toStrictEqual(
      new Set(
        SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.filter(
          (role) => role !== "step01" && role !== "terminal",
        ),
      ),
    );
    const outcome = await run.terminal(true, thread);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  }, 2_400_000);
});
