/**
 * `executionNativeScriptInvalid` custody refuses a forged successor at every
 * role: from a genuine Init, step-01 is first built paying the next role
 * another planned state of the same thread, and its validator must refuse it;
 * each planned transition from the binding prefix through the evaluation is
 * then built the same way and refused before the honest transition advances
 * the thread; the terminal, which has no successor role, is built paying the
 * fraud-proof token a datum naming another prover and refused before the
 * honest terminal convicts. The scenarios together visit every role: a
 * native observer, a native-locked spend, a native mint policy and a
 * native-protected output whose scripts are false, and a recorded rejection
 * at a key-locked spend.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  buildExecutionNativeScriptInvalidTransition,
  type ExecutionNativeScriptInvalidRole,
  submitExecutionNativeScriptInvalidTransition,
} from "../src/execution-native-script-invalid/index.js";
import type { LedgerEntry } from "../src/ledger-at.js";
import { submitNativeTxBinding } from "../src/native-tx-binding.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.build-source-membership-proof.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../src/witness-reference-scripts.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionNativeScriptInvalidThread,
  type ExecutionNativeScriptInvalidHarness,
  makeExecutionNativeScriptInvalidHarness,
  publishExecutionNativeScriptInvalidRoles,
} from "./support/execution-native-script-invalid-emulator.js";
import {
  admissibleNativeObservers,
  keyLockedEntries,
  native,
  nativeFalseAt,
  type NativeScenario,
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

/** Step-01 from the Init `thread`, paying `bindEvent` the `forged` datum. */
const forgedStep01 = async (
  harness: ExecutionNativeScriptInvalidHarness,
  scenario: NativeScenario,
  reference: UTxO,
  thread: UTxO,
  unit: string,
  forged: string,
) => {
  const contracts = harness.executionNativeScriptInvalid;
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
  harness: ExecutionNativeScriptInvalidHarness,
  reference: UTxO,
  thread: UTxO,
  unit: string,
) => {
  const contracts = harness.executionNativeScriptInvalid;
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

/** Every role a scenario of this file refused a forgery at. */
const covered = new Set<ExecutionNativeScriptInvalidRole>();

/** Forges then advances every role of the scenario's thread and convicts;
 * returns the roles that refused a forgery. */
const forgeEveryRole = async (
  shape: NativeSubjectShape,
  source: NativeSource,
): Promise<ReadonlySet<ExecutionNativeScriptInvalidRole>> => {
  const harness = await makeExecutionNativeScriptInvalidHarness();
  const references = await publishExecutionNativeScriptInvalidRoles(harness);
  const reference = (role: ExecutionNativeScriptInvalidRole) =>
    references.get(role)!.utxo;
  const scenario = await setupNativeScenario({ harness, shape, source });
  const refused: ExecutionNativeScriptInvalidRole[] = [];
  const run = await driveExecutionNativeScriptInvalidThread({
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
  expect(run.prepared.convicts).toBe(true);
  const planned = run.prepared.transitions;
  let thread: UTxO = run.thread;
  for (const [index, transition] of planned.entries()) {
    // The nearest later planned state that differs from the honest one.
    const forged = [...planned.slice(index + 1), ...planned.slice(0, index)]
      .map(({ datum }) => datum)
      .find((datum) => datum !== transition.datum);
    if (forged === undefined)
      throw new Error(`${transition.label}: no distinct planned state`);
    const built = await buildExecutionNativeScriptInvalidTransition(
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
      await submitExecutionNativeScriptInvalidTransition(
        run.custody,
        thread,
        transition,
      )
    ).thread;
    refused.push(transition.role);
  }
  await expectOnchainRefusal(() =>
    forgedTerminal(harness, reference("terminal"), thread, run.unit),
  );
  refused.push("terminal");
  const outcome = await run.terminal(true, thread);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  for (const role of refused) covered.add(role);
  console.log(`ENSI forged-successor roles`, JSON.stringify(refused));
  return new Set(refused);
};

/** A native script that never holds. */
const never = (seed: number): MidgardVersionedScript =>
  native({
    type: "any",
    scripts: [sig(signingKey(100 + seed))],
  });

/** A committed output locked by `script`'s enterprise script address. */
const scriptLocked = (script: MidgardVersionedScript): LedgerEntry[] => [
  {
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x41),
      outputIndex: 0,
    }),
    output: encodeMidgardTxOutput({
      address: scriptAddress(script),
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  },
];

const ROLES = (...roles: readonly ExecutionNativeScriptInvalidRole[]) =>
  new Set(roles);

describe("execution-native-script-invalid forged successor refusal", () => {
  afterAll(() => {
    expect(covered).toStrictEqual(
      ROLES(...SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER),
    );
  });

  it("refuses a forged successor at every role of a forced acceptance whose observer's native script is false", async () => {
    const refused = await forgeEveryRole(
      admissibleNativeObservers([never(1)]),
      { kind: "forced", verdict: "ForcedTxValid" },
    );
    expect(refused.has("evaluate")).toBe(true);
  }, 2_400_000);

  it("refuses a forged successor at every role of an accepted spend whose native script is false", async () => {
    const refused = await forgeEveryRole(
      {
        spend: scriptLocked(never(2)),
        reference: keyLockedEntries(0x31, PAYER),
        witnesses: [never(2)],
        signers: [PAYER],
      },
      { kind: "normal" },
    );
    expect(refused.has("spendScan")).toBe(true);
  }, 2_400_000);

  it("refuses a forged successor at every role of an accepted native mint policy that is false", async () => {
    const refused = await forgeEveryRole(
      {
        spend: keyLockedEntries(0x21, PAYER),
        mints: [{ script: never(3), redeemer: false }],
        signers: [PAYER],
      },
      { kind: "normal" },
    );
    expect(refused.has("mintScan")).toBe(true);
  }, 2_400_000);

  it("refuses a forged successor at every role of an accepted native-protected output that is false", async () => {
    const refused = await forgeEveryRole(
      {
        spend: keyLockedEntries(0x21, PAYER),
        receives: [{ script: never(4), redeemer: false }],
        signers: [PAYER],
      },
      { kind: "normal" },
    );
    expect(refused.has("receiveDeclaration")).toBe(true);
  }, 2_400_000);

  it("refuses a forged successor at every role of a rejection recorded at a key-locked spend", async () => {
    const refused = await forgeEveryRole(
      admissibleNativeObservers([native(sig(PAYER))]),
      rejectedFor(nativeFalseAt(0n, 0n)),
    );
    expect(refused.has("spendKey")).toBe(true);
  }, 2_400_000);
});
