/**
 * `distinctAssetAccumulationLimit` steps against a party that is not the
 * thread's prover. An honest prover opens the thread from a genuine Init and
 * step-01; at every later role a funded stranger builds that role's planned
 * transition directly against the chain, keeping the honest prover as the
 * thread's `fraud_prover` and signing only with its own key. The role's
 * validator refuses the stranger, and the honest prover's own transition then
 * lands on the untouched thread. An input crossing ranks its input; a
 * crossing in the mint walks the outputs and the mint, so the two reach every
 * role.
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
  distinctAssetAccumulationLimitContractRoles,
  type DistinctAssetAccumulationLimitRole,
  type DistinctAssetAccumulationLimitTransition,
  submitDistinctAssetAccumulationLimitTransition,
} from "../src/distinct-asset-accumulation-limit/index.js";
import { faultProofFieldOpening } from "../src/field-opening.js";
import {
  getCompiledScript,
  phasMembershipRewardAddress,
  resolveProverSigner,
} from "../src/runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../src/step-support.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  driveDistinctAssetAccumulationLimitThread,
  makeDistinctAssetAccumulationLimitHarness,
  publishDistinctAssetAccumulationLimitRoles,
} from "./support/distinct-asset-accumulation-limit-emulator.js";
import {
  type DistinctAssetSubjectShape,
  setupDistinctAssetScenario,
} from "./support/distinct-asset-accumulation-limit-emulator.subject.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { network } from "./support/submit-init-emulator-shared.js";

const STRANGER_FUNDING_LOVELACE = 1_000_000_000n;

const SPENT = Array.from({ length: 10 }, () => 1_536);

/** The eleventh spend crosses, so the crossing input is ranked. */
const INPUT_CROSSING: DistinctAssetSubjectShape = {
  inputs: [...SPENT, 1_536],
  references: 1,
  outputs: [[1]],
  mint: [],
};

/** The mint's third policy crosses after both outputs are walked. */
const MINT_CROSSING: DistinctAssetSubjectShape = {
  inputs: SPENT,
  outputs: [[256], [256]],
  mint: [256, 256, 10],
};

/** Every role of the shape's thread after step-01 refuses a stranger and
 * then takes the prover's own step; returns the roles that refused. */
const refuseStrangerAtEveryRole = async (
  shape: DistinctAssetSubjectShape,
): Promise<readonly DistinctAssetAccumulationLimitRole[]> => {
  const harness = await makeDistinctAssetAccumulationLimitHarness();
  const references = await publishDistinctAssetAccumulationLimitRoles(harness);
  const scenario = await setupDistinctAssetScenario({
    harness,
    shape,
    source: { kind: "normal" },
  });
  const run = await driveDistinctAssetAccumulationLimitThread({
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

  const roles = distinctAssetAccumulationLimitContractRoles(
    harness.distinctAssetAccumulationLimit,
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
    transition: DistinctAssetAccumulationLimitTransition,
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
              harness.distinctAssetAccumulationLimit
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
  const refused: DistinctAssetAccumulationLimitRole[] = [];
  for (const transition of run.prepared.transitions) {
    await expectOnchainRefusal(() => strangerTransition(thread, transition));
    refused.push(transition.role);
    thread = (
      await submitDistinctAssetAccumulationLimitTransition(
        run.custody,
        thread,
        transition,
      )
    ).thread;
  }
  const outcome = await run.terminal(true, thread);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  return refused;
};

describe("distinct-asset-accumulation-limit steps against a stranger key", () => {
  it("refuses a stranger at every role after step-01, and the prover's own step lands", async () => {
    const mint = await refuseStrangerAtEveryRole(MINT_CROSSING);
    const input = await refuseStrangerAtEveryRole(INPUT_CROSSING);
    expect(new Set([...mint, ...input])).toStrictEqual(
      new Set(
        SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.filter(
          (role) => role !== "step01" && role !== "terminal",
        ),
      ),
    );
  }, 2_400_000);
});
