/** Submits one direct purpose custody transition: the thread moves from
 * its role to the next role with the exact next datum, the fields it opens
 * carried by reference, and a PHAS membership withdrawal where it binds. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  type LucidEvolution,
  type Network,
  type Script,
  type TxBuilder,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import { faultProofFieldOpening } from "../field-opening.js";
import {
  getCompiledScript,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { completeStep, fundStep, submitStep } from "../step-completion.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type {
  DirectPurposeFieldPlan,
  DirectPurposeTransition,
} from "./custody.js";
import {
  type DirectPurposeContracts,
  type DirectPurposeCustodyFamily,
  type DirectPurposeRole,
  directPurposeRoleValidator,
} from "./family.js";

export type DirectPurposeCustody = {
  readonly family: DirectPurposeCustodyFamily;
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: DirectPurposeContracts;
  readonly signer: ResolvedProverSigner;
  /** The thread's computation-thread unit. */
  readonly unit: string;
  /** Published reference script of each role the thread spends. */
  readonly references: (role: DirectPurposeRole) => UTxO;
  /** Field-preimage carriage of each opened field; empty for inline tiers. */
  readonly carriage: (field: number) => readonly UTxO[];
  readonly plan: (field: number) => DirectPurposeFieldPlan;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Observes every completed signed transaction before submission. */
  readonly onSigned?: (label: string, signed: TxSigned) => void;
};

/** A deliberately different datum, redeemer or destination, for refusal rows. */
export type DirectPurposeTransitionOverride = {
  readonly datum?: string;
  readonly redeemer?: DirectPurposeTransition["redeemer"];
  readonly next?: DirectPurposeRole;
};

const phasMembership = (custody: DirectPurposeCustody) => {
  const script: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      custody.blueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  return {
    script,
    rewardAddress: phasMembershipRewardAddress(custody.network, script),
  };
};

/** The unbuilt transaction; `layout()` reads the completed output index. */
export const buildDirectPurposeTransition = async (
  custody: DirectPurposeCustody,
  thread: UTxO,
  transition: DirectPurposeTransition,
  override: DirectPurposeTransitionOverride = {},
) => {
  const { family, contracts } = custody;
  const role = directPurposeRoleValidator(family, contracts, transition.role);
  const next = directPurposeRoleValidator(
    family,
    contracts,
    override.next ?? transition.next,
  );
  const datum = override.datum ?? transition.datum;
  const redeemer = override.redeemer ?? transition.redeemer;
  const label = `${family.label} ${transition.label}`;
  if (thread.address !== role.spendingScriptAddress)
    throw new Error(`${label}: thread is not at the ${transition.role} role`);
  const membership = phasMembership(custody);
  const membershipCarriage =
    transition.membership === undefined
      ? undefined
      : witnessWithdrawalValidatorCarriage({
          script: membership.script,
          referenceUtxo:
            custody.witnessReferenceScripts?.phasMembershipWithdraw,
          label: `${label} membership`,
        });
  const carriage = [
    ...new Map(
      transition.fields
        .flatMap((field) => custody.carriage(field))
        .map((utxo) => [`${utxo.txHash}#${utxo.outputIndex.toString()}`, utxo]),
    ).values(),
  ];
  let outputIndex: bigint | undefined;
  const spend = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, thread, label);
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      computationThreadOutputPredicate({
        address: next.spendingScriptAddress,
        datum,
        unit: custody.unit,
      }),
      label,
    );
    return redeemer({
      indices: {
        input_index: SDK.requireInputIndex(ctx, thread, label),
        output_index: outputIndex,
      },
      open: (field) =>
        faultProofFieldOpening({
          planned: custody.plan(field).openingPlan,
          referenceInputs: ctx.referenceInputs,
          certificatePolicyId:
            custody.contracts.fieldPreimageCertificatePolicyId,
          label: `${label} field ${field.toString()}`,
        }),
      ctx,
      thread,
      unit: custody.unit,
      nextAddress: next.spendingScriptAddress,
      membershipRewardAddress: membership.rewardAddress,
    });
  }) satisfies BuildTxWithRedeemer;
  const funding = await fundStep(custody.lucid, {
    signer: custody.signer,
    excluding: carriage,
  });
  let tx: TxBuilder = funding.tx
    .collectFrom([thread], spend)
    .readFrom([
      custody.references(transition.role),
      ...carriage,
      ...(membershipCarriage?.referenceInputs ?? []),
    ])
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: datum },
      thread.assets,
    );
  if (transition.membership !== undefined)
    tx = tx.withdraw(
      membership.rewardAddress,
      0n,
      transition.membership.redeemer,
    );
  return {
    tx,
    complete: () => completeStep(tx, funding),
    outputIndex: () => outputIndex,
    referenceCandidates: [
      {
        role: `${label} role`,
        utxo: custody.references(transition.role),
        expectedScript: role.spendingScript,
      },
      {
        role: `${label} membership`,
        utxo: custody.witnessReferenceScripts?.phasMembershipWithdraw,
        expectedScript: membership.script,
      },
    ],
  };
};

/** Submits one honest transition and returns the thread's next UTxO. */
export const submitDirectPurposeTransition = async (
  custody: DirectPurposeCustody,
  thread: UTxO,
  transition: DirectPurposeTransition,
): Promise<{ readonly txHash: string; readonly thread: UTxO }> => {
  const built = await buildDirectPurposeTransition(custody, thread, transition);
  const signed = await built.complete();
  const outputIndex = built.outputIndex();
  if (outputIndex === undefined)
    throw new Error(`${transition.label}: completed output index absent`);
  custody.onSigned?.(transition.label, signed);
  const txHash = await submitStep(custody.lucid, signed, {
    label: `${custody.family.label} ${transition.label}`,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: built.referenceCandidates,
    }),
    boundary: custody.preSubmitBoundary,
    awaitConfirmation: custody.awaitConfirmation ?? true,
  });
  const [next] = await custody.lucid.utxosByOutRef([
    { txHash, outputIndex: Number(outputIndex) },
  ]);
  if (next === undefined)
    throw new Error(`${transition.label}: next thread output absent`);
  return { txHash, thread: next };
};

/** Submits transitions in order; returns the thread at the last role. */
export const submitDirectPurposeTransitions = async (
  custody: DirectPurposeCustody,
  thread: UTxO,
  transitions: readonly DirectPurposeTransition[],
): Promise<UTxO> => {
  let current = thread;
  for (const transition of transitions)
    current = (
      await submitDirectPurposeTransition(custody, current, transition)
    ).thread;
  return current;
};
