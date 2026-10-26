/**
 * `unusedScriptWitness` custody refuses a forged successor at every
 * role: from a genuine Init, step-01 is first built paying the next role
 * another planned state of the same thread, and its validator must refuse it;
 * each planned transition from the binding prefix through the walk is then
 * built the same way and refused before the honest transition advances the
 * thread; the terminal, which has no successor role, is built paying the
 * fraud-proof token a forged finalize datum and refused before the honest
 * terminal convicts.
 */
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitNativeTxBinding } from "../src/native-tx-binding.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.build-source-membership-proof.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  buildUnusedScriptWitnessTransition,
  submitUnusedScriptWitnessTransition,
  unusedScriptWitnessContractRoles,
  type UnusedScriptWitnessRole,
} from "../src/unused-script-witness/index.js";
import { witnessMintingPolicyCarriage } from "../src/witness-reference-scripts.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildIntegritySubject,
  expectedHash,
  type IntegrityScenario,
  nativeScript,
  plutusScript,
  scriptLockedEntries,
  setupIntegrityScenario,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  driveUnusedScriptWitnessThread,
  makeUnusedScriptWitnessHarness,
  publishUnusedScriptWitnessRoles,
  type UnusedScriptWitnessHarness,
} from "./support/unused-script-witness-emulator.js";
import {
  admissibleForcedWith,
  holdingNativeScript,
} from "./support/unused-script-witness-emulator.subject.js";

type Scenario = Parameters<typeof setupIntegrityScenario>[0];

/** Step-01 from the Init `thread`, paying `bindEvent` the `forged` datum. */
const forgedStep01 = async (
  harness: UnusedScriptWitnessHarness,
  scenario: IntegrityScenario,
  reference: UTxO,
  thread: UTxO,
  unit: string,
  forged: string,
) => {
  const contracts = harness.unusedScriptWitness;
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
  const next = unusedScriptWitnessContractRoles(contracts).bindEvent;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: scenario.block.reconstruction,
    eventKey: scenario.material.eventKey,
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
  harness: UnusedScriptWitnessHarness,
  selection: SDK.UnusedScriptWitnessSelection | null,
  reference: UTxO,
  thread: UTxO,
  unit: string,
) => {
  const contracts = harness.unusedScriptWitness;
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
            selection,
          },
        ],
      },
      SDK.UnusedScriptWitnessFinalizeRedeemer,
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

/** Forges then advances every role of the thread, from step-01 to the
 * terminal; returns the roles that refused a forgery. */
const forgeEveryRole = async (
  shape: Scenario["shape"],
  source: Scenario["source"],
): Promise<readonly UnusedScriptWitnessRole[]> => {
  const harness = await makeUnusedScriptWitnessHarness();
  const references = await publishUnusedScriptWitnessRoles(harness);
  const reference = (role: UnusedScriptWitnessRole) =>
    references.get(role)!.utxo;
  const scenario = await setupIntegrityScenario({
    harness,
    shape,
    source,
  });
  const refused: UnusedScriptWitnessRole[] = [];
  const run = await driveUnusedScriptWitnessThread({
    harness,
    scenario,
    references,
    transitions: () => [],
    beforeStep01: async ({ thread, unit, prepared }) => {
      // A later planned state: the one `bindEvent` pays.
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
  const planned = run.prepared.transitions;
  let thread: UTxO = run.thread;
  for (const [index, transition] of planned.entries()) {
    // The nearest later planned state that differs from the honest one.
    const forged = [...planned.slice(index + 1), ...planned.slice(0, index)]
      .map(({ datum }) => datum)
      .find((datum) => datum !== transition.datum);
    if (forged === undefined)
      throw new Error(`${transition.label}: no distinct planned state`);
    const built = await buildUnusedScriptWitnessTransition(
      run.custody,
      thread,
      transition,
      {
        datum: forged,
        // The honest redeemer, read as if the forged output carried the
        // honest state, so it names the forged output's position.
        redeemer: (args) =>
          transition.redeemer({
            ...args,
            ctx: {
              ...args.ctx,
              outputs: args.ctx.outputs.map((output) =>
                output.datum === forged
                  ? { ...output, datum: transition.datum }
                  : output,
              ),
            },
          }),
      },
    );
    await expectOnchainRefusal(() => built.complete());
    thread = (
      await submitUnusedScriptWitnessTransition(run.custody, thread, transition)
    ).thread;
    refused.push(transition.role);
  }
  await expectOnchainRefusal(() =>
    forgedTerminal(
      harness,
      run.prepared.selection,
      reference("terminal"),
      thread,
      run.unit,
    ),
  );
  refused.push("terminal");
  const outcome = await run.terminal(true, thread);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  return refused;
};

const EVERY_ROLE = new Set<UnusedScriptWitnessRole>(
  SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER,
);

describe("unused-script-witness forged successor refusal", () => {
  it("refuses a forged successor at every role of an accepted transaction", async () => {
    const spent = plutusScript(21);
    const refused = await forgeEveryRole(
      {
        spend: scriptLockedEntries(0x51, [spent]),
        witnesses: [spent, plutusScript(24)],
        spendRedeemers: [0n],
        mints: [{ script: plutusScript(22), redeemer: true }],
        observers: [{ script: nativeScript(3n), redeemer: false }],
        receives: [{ script: plutusScript(23), redeemer: true }],
        integrity: expectedHash("PlutusV3"),
      },
      { kind: "normal" },
    );
    expect(EVERY_ROLE.size).toBe(24);
    expect(new Set(refused)).toStrictEqual(EVERY_ROLE);
  }, 2_400_000);

  it("refuses a forged successor at every role of a forced acceptance", async () => {
    // An admissible forced subject (a native-only key spend) with one native
    // observer and an unused native witness; it mints nothing and has no
    // outputs, so the mint and receive walks are never entered.
    const shape = {
      ...admissibleForcedWith([holdingNativeScript(2)]),
      observers: [{ script: nativeScript(5n), redeemer: false }],
    };
    expect(screenMidgardForcedTxAdmission(buildIntegritySubject(shape))).toBe(
      null,
    );
    const refused = await forgeEveryRole(shape, {
      kind: "forced",
      verdict: "ForcedTxValid",
    });
    expect(new Set(refused)).toStrictEqual(
      new Set(
        [...EVERY_ROLE].filter(
          (role) => role !== "allPurposesMint" && role !== "allPurposesReceive",
        ),
      ),
    );
  }, 2_400_000);
});
