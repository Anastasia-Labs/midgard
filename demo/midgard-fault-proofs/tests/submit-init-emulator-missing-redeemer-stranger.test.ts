/**
 * `missingRedeemer` steps against a party that is not the thread's prover.
 *
 * Continuing a fault-proof thread requires its prover. Each row lets an
 * honest prover open a thread over an accepted forced transaction whose
 * script spend has no redeemer. A funded stranger then builds the thread's
 * next step directly against the chain: the same successor state, with the
 * honest prover still named as the thread's `fraud_prover`, signed only with
 * the stranger's key. The step validator refuses it, and the honest prover's
 * own step still lands on the untouched thread.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  generateEmulatorAccount,
  Lucid,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type DirectPurposeRole,
  directPurposeRoleValidator,
  type DirectPurposeTransition,
  submitDirectPurposeInit,
  submitDirectPurposeStep01Forced,
  submitDirectPurposeTransition,
} from "../src/direct-purpose-family/index.js";
import { faultProofFieldOpening } from "../src/field-opening.js";
import { MISSING_REDEEMER_FAMILY } from "../src/missing-redeemer/index.js";
import {
  getCompiledScript,
  phasMembershipRewardAddress,
  resolveProverSigner,
} from "../src/runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../src/step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  publishMissingRedeemerRoles,
} from "./support/missing-redeemer-emulator.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  missingRedeemerLedgerEntries,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

const STRANGER_FUNDING_LOVELACE = 1_000_000_000n;

/** An honest prover's harness and committed subject, and a funded stranger. */
const scene = async () => {
  const harness = await makeMissingRedeemerHarness();
  const references = await publishMissingRedeemerRoles(harness);
  // A spend of an output locked by a Plutus script sourced only from a
  // reference input, with no redeemer, committed as an accepted forced leaf.
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape: {
      observers: [],
      spend: missingRedeemerLedgerEntries(
        0x41,
        [null],
        scriptAddress(plutusScript(9)),
      ),
      reference: missingRedeemerLedgerEntries(0x31, [plutusScript(9)]),
    },
    source: { kind: "forced", verdict: "ForcedTxValid" },
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
  const reference = (role: DirectPurposeRole) => {
    const found = references.get(role);
    if (found === undefined) throw new Error(`${role} reference absent`);
    return found.utxo;
  };
  return {
    harness,
    references,
    reference,
    scenario,
    strangerLucid,
    strangerSigner,
  };
};

type Scene = Awaited<ReturnType<typeof scene>>;

const threadAt = (s: Scene, address: string, unit: string) =>
  expectSingleUtxoWithUnit(s.harness.proverLucid, address, unit);

/**
 * Forced step-01, built by the stranger: it opens the forced leaf under the
 * challenged header and pays the bound source to the next role, naming the
 * honest prover as the thread's `fraud_prover`.
 */
const strangerStep01 = async (s: Scene, thread: UTxO, unit: string) => {
  const { harness, scenario, strangerLucid, strangerSigner } = s;
  const { material } = scenario;
  const eventKey = material.eventKey;
  if (!("ForcedTransactionEventKey" in eventKey))
    throw new Error("stranger step-01: subject is not forced");
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: scenario.block.reconstruction,
    eventKey,
  });
  const next = harness.missingRedeemer.steps[1]!;
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: material.sourceBound,
    },
    SDK.DirectBindEventDatum,
  );
  const redeemer = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: SDK.requireInputIndex(ctx, thread, "step 01"),
                output_index: SDK.requireUniqueOutputIndex(
                  ctx.outputs,
                  computationThreadOutputPredicate({
                    address: next.spendingScriptAddress,
                    datum: nextDatum,
                    unit,
                  }),
                  "step 01 output",
                ),
                header: material.sourceBound.header,
                membership: SDK.forcedLeafMembershipV1(membership),
              },
            },
          },
        ],
      },
      SDK.DirectStep01Redeemer,
    )) satisfies BuildTxWithRedeemer;
  const tx = await strangerLucid
    .newTx()
    .collectFrom([thread], redeemer)
    .readFrom([s.reference("step01")])
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      thread.assets,
    )
    .addSignerKey(strangerSigner.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  await strangerLucid.awaitTx(await signed.submit());
};

type Run = Awaited<ReturnType<typeof driveMissingRedeemerThread>>;

/**
 * A planned custody transition, built by the stranger: the honest successor
 * datum and redeemer, the same opened fields read by reference, signed only
 * with the stranger's key.
 */
const strangerTransition = async (
  s: Scene,
  run: Run,
  thread: UTxO,
  transition: DirectPurposeTransition,
) => {
  const { harness, strangerLucid, strangerSigner } = s;
  const contracts = harness.missingRedeemer;
  const next = directPurposeRoleValidator(
    MISSING_REDEEMER_FAMILY,
    contracts,
    transition.next,
  );
  const membershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      harness.realBlueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const membershipRewardAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const carriage = [
    ...new Map(
      transition.fields
        .flatMap((field) => run.custody.carriage(field))
        .map((utxo) => [outRefLabel(utxo), utxo]),
    ).values(),
  ];
  const redeemer = ((ctx) =>
    transition.redeemer({
      indices: {
        input_index: SDK.requireInputIndex(ctx, thread, transition.label),
        output_index: SDK.requireUniqueOutputIndex(
          ctx.outputs,
          computationThreadOutputPredicate({
            address: next.spendingScriptAddress,
            datum: transition.datum,
            unit: run.unit,
          }),
          transition.label,
        ),
      },
      open: (field) =>
        faultProofFieldOpening({
          planned: run.custody.plan(field).openingPlan,
          referenceInputs: ctx.referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: `stranger ${transition.label} field ${field.toString()}`,
        }),
      ctx,
      thread,
      unit: run.unit,
      nextAddress: next.spendingScriptAddress,
      membershipRewardAddress,
    })) satisfies BuildTxWithRedeemer;
  let tx = strangerLucid
    .newTx()
    .collectFrom([thread], redeemer)
    .readFrom([
      s.reference(transition.role),
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
    tx = tx.withdraw(
      membershipRewardAddress,
      0n,
      transition.membership.redeemer,
    );
  const completed = await tx.complete({ localUPLCEval: true });
  const signed = await completed.sign.withWallet().complete();
  await strangerLucid.awaitTx(await signed.submit());
};

describe("missing-redeemer steps refuse a party other than the thread's prover", () => {
  it("refuses a stranger's forced step-01, then takes the prover's", async () => {
    const s = await scene();
    const { harness, scenario } = s;
    const init = await submitDirectPurposeInit({
      family: MISSING_REDEEMER_FAMILY,
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.missingRedeemer,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const unit = init.computationThreadUnit;
    const firstStep = harness.missingRedeemer.steps[0]!.spendingScriptAddress;
    const thread = await threadAt(s, firstStep, unit);
    expect(outRefLabel(thread)).toBe(init.nextThreadOutRef);

    await expectOnchainRefusal(() => strangerStep01(s, thread, unit), {
      refusedBy: "fraud_proofs/direct_script_derivation/step_01",
      check: /fraud_prover \|> has_signed\(extra_signatories\)/u,
    });

    // The thread is where the prover left it, and the prover's own step lands.
    expect(outRefLabel(await threadAt(s, firstStep, unit))).toBe(
      outRefLabel(thread),
    );
    const step01 = await submitDirectPurposeStep01Forced({
      family: MISSING_REDEEMER_FAMILY,
      lucid: harness.proverLucid,
      contracts: harness.missingRedeemer,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(thread),
      material: scenario.material,
      referenceScriptUtxo: s.reference("step01"),
      reconstruction: scenario.block.reconstruction,
    });
    const advanced = await threadAt(
      s,
      harness.missingRedeemer.steps[1]!.spendingScriptAddress,
      unit,
    );
    expect(outRefLabel(advanced)).toBe(step01.nextThreadOutRef);
  }, 1_200_000);

  it("refuses a stranger's family-entry and claim-adjudicate steps, then convicts with the prover's", async () => {
    const s = await scene();
    const { harness } = s;
    const contested = new Set<DirectPurposeRole>([
      "familyEntry",
      "claimAdjudicate",
    ]);
    const run = await driveMissingRedeemerThread({
      harness,
      scenario: s.scenario,
      references: s.references,
      transitions: () => [],
    });
    expect(run.prepared.convicts).toBe(true);
    const planned = run.prepared.transitions;
    expect(
      planned.filter((transition) => contested.has(transition.role)),
    ).toHaveLength(contested.size);

    let thread: UTxO = run.thread;
    const refused: DirectPurposeRole[] = [];
    for (const transition of planned) {
      if (contested.has(transition.role)) {
        const before = outRefLabel(thread);
        await expectOnchainRefusal(
          () => strangerTransition(s, run, thread, transition),
          transition.role === "familyEntry"
            ? {
                refusedBy: "fraud_proofs/missing_redeemer/family_entry",
                check: /fraud_prover \|> has_signed\(extra_signatories\)/u,
              }
            : {
                refusedBy:
                  "fraud_proofs/direct_script_derivation/claim_adjudicate",
                check: /fraud_prover \|> has_signed\(extra_signatories\)/u,
              },
        );
        expect(outRefLabel(await threadAt(s, thread.address, run.unit))).toBe(
          before,
        );
        refused.push(transition.role);
      }
      thread = (
        await submitDirectPurposeTransition(run.custody, thread, transition)
      ).thread;
    }
    expect(refused).toStrictEqual(["familyEntry", "claimAdjudicate"]);

    const outcome = await run.terminal(true, thread);
    expect(outcome.fraudulentHeaderHash).toBe(s.scenario.setup.headerHash);
    const token = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });
  }, 1_200_000);
});
