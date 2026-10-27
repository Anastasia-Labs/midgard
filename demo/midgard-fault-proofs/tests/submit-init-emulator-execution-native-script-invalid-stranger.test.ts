/**
 * `executionNativeScriptInvalid` steps against a party that is not the
 * thread's prover. An honest prover opens the thread from a genuine Init and
 * step-01; at every later role a funded stranger builds that role's planned
 * transition directly against the chain, keeping the honest prover as the
 * thread's `fraud_prover` and signing only with its own key. The role's
 * validator refuses the stranger, and the honest prover's own transition
 * then lands on the untouched thread. The scenarios together visit every
 * role between step-01 and the terminal.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  generateEmulatorAccount,
  Lucid,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  executionNativeScriptInvalidContractRoles,
  type ExecutionNativeScriptInvalidRole,
  type ExecutionNativeScriptInvalidTransition,
  submitExecutionNativeScriptInvalidTransition,
} from "../src/execution-native-script-invalid/index.js";
import { faultProofFieldOpening } from "../src/field-opening.js";
import type { LedgerEntry } from "../src/ledger-at.js";
import {
  getCompiledScript,
  phasMembershipRewardAddress,
  resolveProverSigner,
} from "../src/runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../src/step-support.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionNativeScriptInvalidThread,
  makeExecutionNativeScriptInvalidHarness,
  publishExecutionNativeScriptInvalidRoles,
} from "./support/execution-native-script-invalid-emulator.js";
import {
  admissibleNativeObservers,
  keyLockedEntries,
  native,
  nativeFalseAt,
  type NativeSource,
  type NativeSubjectShape,
  PAYER,
  setupNativeScenario,
  sig,
  signingKey,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import { rejectedFor } from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { network } from "./support/submit-init-emulator-shared.js";

const STRANGER_FUNDING_LOVELACE = 1_000_000_000n;

/** Every role a scenario of this file refused a stranger at. */
const covered = new Set<ExecutionNativeScriptInvalidRole>();

/** Runs the thread with a stranger attempting every planned transition
 * first; returns the roles that refused the stranger. */
const strangerAtEveryRole = async (
  shape: NativeSubjectShape,
  source: NativeSource,
) => {
  const harness = await makeExecutionNativeScriptInvalidHarness();
  const references = await publishExecutionNativeScriptInvalidRoles(harness);
  const scenario = await setupNativeScenario({ harness, shape, source });
  const run = await driveExecutionNativeScriptInvalidThread({
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

  const roles = executionNativeScriptInvalidContractRoles(
    harness.executionNativeScriptInvalid,
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
    transition: ExecutionNativeScriptInvalidTransition,
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
              harness.executionNativeScriptInvalid
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
  const refused: ExecutionNativeScriptInvalidRole[] = [];
  for (const transition of run.prepared.transitions) {
    await expectOnchainRefusal(() => strangerTransition(thread, transition));
    refused.push(transition.role);
    thread = (
      await submitExecutionNativeScriptInvalidTransition(
        run.custody,
        thread,
        transition,
      )
    ).thread;
  }
  const outcome = await run.terminal(true, thread);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  for (const role of refused) covered.add(role);
  return new Set(refused);
};

/** A native script that never holds. */
const never = (seed: number): MidgardVersionedScript =>
  native({ type: "any", scripts: [sig(signingKey(200 + seed))] });

/** A committed output locked by `script`'s enterprise script address. */
const scriptLocked = (script: MidgardVersionedScript): LedgerEntry[] => [
  {
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x61),
      outputIndex: 0,
    }),
    output: encodeMidgardTxOutput({
      address: scriptAddress(script),
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  },
];

describe("execution-native-script-invalid steps against a stranger key", () => {
  afterAll(() => {
    expect(covered).toStrictEqual(
      new Set(
        SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.filter(
          (role) => role !== "step01" && role !== "terminal",
        ),
      ),
    );
  });

  it.each([
    {
      name: "a forced acceptance whose observer's native script is false",
      shape: () => admissibleNativeObservers([never(1)]),
      source: { kind: "forced", verdict: "ForcedTxValid" } as NativeSource,
      reaches: "evaluate",
    },
    {
      name: "an accepted spend whose native script is false",
      shape: (): NativeSubjectShape => ({
        spend: scriptLocked(never(2)),
        reference: keyLockedEntries(0x31, PAYER),
        witnesses: [never(2)],
        signers: [PAYER],
      }),
      source: { kind: "normal" } as NativeSource,
      reaches: "spendScan",
    },
    {
      name: "an accepted native mint policy that is false",
      shape: (): NativeSubjectShape => ({
        spend: keyLockedEntries(0x21, PAYER),
        mints: [{ script: never(3), redeemer: false }],
        signers: [PAYER],
      }),
      source: { kind: "normal" } as NativeSource,
      reaches: "mintScan",
    },
    {
      name: "an accepted native-protected output that is false",
      shape: (): NativeSubjectShape => ({
        spend: keyLockedEntries(0x21, PAYER),
        receives: [{ script: never(4), redeemer: false }],
        signers: [PAYER],
      }),
      source: { kind: "normal" } as NativeSource,
      reaches: "receiveDeclaration",
    },
    {
      name: "a rejection recorded at a key-locked spend",
      shape: () => admissibleNativeObservers([native(sig(PAYER))]),
      source: rejectedFor(nativeFalseAt(0n, 0n)),
      reaches: "spendRank",
    },
  ] as const)(
    "refuses a stranger at every role of $name, and the prover's own step lands",
    async (row) => {
      const refused = await strangerAtEveryRole(row.shape(), row.source);
      expect(refused.has(row.reaches)).toBe(true);
    },
    2_400_000,
  );
});
