/** Submits one `distinctAssetAccumulationLimit` custody transition: the thread
 * moves from its role to the next role with the exact next datum, the fields
 * it opens carried by reference, and a PHAS membership withdrawal where it
 * binds. */
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
  DEFAULT_CONFIRMATION_POLL_MS,
  getCompiledScript,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
} from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL,
  distinctAssetAccumulationLimitContractRoles,
  type DistinctAssetAccumulationLimitContracts,
  type DistinctAssetAccumulationLimitRole,
} from "./contracts.js";
import type {
  DistinctAssetAccumulationLimitFieldPlan,
  DistinctAssetAccumulationLimitTransition,
} from "./custody.js";

export type DistinctAssetAccumulationLimitCustody = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: DistinctAssetAccumulationLimitContracts;
  readonly signer: ResolvedProverSigner;
  /** The thread's computation-thread unit. */
  readonly unit: string;
  /** Published reference script of each role the thread spends. */
  readonly references: (role: DistinctAssetAccumulationLimitRole) => UTxO;
  /** Field-preimage carriage of each opened field; empty for inline tiers. */
  readonly carriage: (field: number) => readonly UTxO[];
  readonly plan: (field: number) => DistinctAssetAccumulationLimitFieldPlan;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Observes every completed signed transaction before submission. */
  readonly onSigned?: (label: string, signed: TxSigned) => void;
};

/** A deliberately different datum, redeemer or destination, for refusal rows. */
export type DistinctAssetAccumulationLimitTransitionOverride = {
  readonly datum?: string;
  readonly redeemer?: DistinctAssetAccumulationLimitTransition["redeemer"];
  readonly next?: DistinctAssetAccumulationLimitRole;
};

const phasMembership = (custody: DistinctAssetAccumulationLimitCustody) => {
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
export const buildDistinctAssetAccumulationLimitTransition = async (
  custody: DistinctAssetAccumulationLimitCustody,
  thread: UTxO,
  transition: DistinctAssetAccumulationLimitTransition,
  override: DistinctAssetAccumulationLimitTransitionOverride = {},
) => {
  const roles = distinctAssetAccumulationLimitContractRoles(custody.contracts);
  const role = roles[transition.role];
  const next = roles[override.next ?? transition.next];
  const datum = override.datum ?? transition.datum;
  const redeemer = override.redeemer ?? transition.redeemer;
  const label = `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL} ${transition.label}`;
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
  custody.signer.selectWallet(custody.lucid);
  const wallet = carriage.reduce<readonly UTxO[]>(
    (utxos, item) => excludeUtxo(utxos, item),
    await custody.lucid.wallet().getUtxos(),
  );
  let tx: TxBuilder = custody.lucid
    .newTx()
    .collectFrom([selectFeeInput(wallet)])
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
    )
    .addSignerKey(custody.signer.paymentKeyHash);
  if (transition.membership !== undefined)
    tx = tx.withdraw(
      membership.rewardAddress,
      0n,
      transition.membership.redeemer,
    );
  return {
    tx,
    complete: () =>
      tx.complete({
        localUPLCEval: true,
        ...(carriage.length === 0
          ? {}
          : { presetWalletInputs: wallet as UTxO[] }),
      }),
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
export const submitDistinctAssetAccumulationLimitTransition = async (
  custody: DistinctAssetAccumulationLimitCustody,
  thread: UTxO,
  transition: DistinctAssetAccumulationLimitTransition,
): Promise<{ readonly txHash: string; readonly thread: UTxO }> => {
  const built = await buildDistinctAssetAccumulationLimitTransition(
    custody,
    thread,
    transition,
  );
  const signed = await (await built.complete()).sign.withWallet().complete();
  const outputIndex = built.outputIndex();
  if (outputIndex === undefined)
    throw new Error(`${transition.label}: completed output index absent`);
  custody.onSigned?.(transition.label, signed);
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: built.referenceCandidates,
    }),
    boundary: custody.preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error(
      `${transition.label}: provider returned ${txHash}, expected ${expected}`,
    );
  if (custody.awaitConfirmation ?? true)
    await custody.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  const [next] = await custody.lucid.utxosByOutRef([
    { txHash, outputIndex: Number(outputIndex) },
  ]);
  if (next === undefined)
    throw new Error(`${transition.label}: next thread output absent`);
  return { txHash, thread: next };
};

/** Submits transitions in order; returns the thread at the last role. */
export const submitDistinctAssetAccumulationLimitTransitions = async (
  custody: DistinctAssetAccumulationLimitCustody,
  thread: UTxO,
  transitions: readonly DistinctAssetAccumulationLimitTransition[],
): Promise<UTxO> => {
  let current = thread;
  for (const transition of transitions)
    current = (
      await submitDistinctAssetAccumulationLimitTransition(
        custody,
        current,
        transition,
      )
    ).thread;
  return current;
};
