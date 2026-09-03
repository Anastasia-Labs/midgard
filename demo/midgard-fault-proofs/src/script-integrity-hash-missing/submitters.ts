import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts.js";
import {
  ScriptIntegritySpendRedeemers,
  ScriptIntegrityStepDatums,
} from "./schemas.js";

const FAMILY = "script-integrity-hash-missing";

export type ScriptIntegrityHashMissingContinueArgs = {
  readonly lucid: LucidEvolution;
  readonly contracts: ScriptIntegrityHashMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nextDatum: string;
  readonly buildArgs: (layout: {
    readonly input_index: bigint;
    readonly output_index: bigint;
  }) => unknown;
  readonly referenceScriptUtxo: UTxO;
  /** Raw chunks and field certificates are reference inputs, never wallet inputs. */
  readonly authenticatedCarriageUtxos?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

const requireCertifiedCarriage = (
  contracts: ScriptIntegrityHashMissingContracts,
  utxos: readonly UTxO[],
): void => {
  if (utxos.length === 0)
    throw new Error(
      `${FAMILY}: staged transition requires authenticated carriage`,
    );
  const prefix = contracts.fieldPreimageCertificatePolicyId;
  if (
    !utxos.some(({ assets }) =>
      Object.keys(assets).some(
        (unit) => unit.startsWith(prefix) && assets[unit] === 1n,
      ),
    )
  ) {
    throw new Error(
      `${FAMILY}: staged carriage has no field-preimage certificate token`,
    );
  }
};

const submitContinue = async (
  physicalStep: 0 | 1 | 2 | 3 | 4 | 5,
  nextPhysicalStep: 0 | 1 | 2 | 3 | 4 | 5 | 6,
  args: ScriptIntegrityHashMissingContinueArgs,
) => {
  const {
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    nextDatum,
    buildArgs,
    referenceScriptUtxo,
    authenticatedCarriageUtxos = [],
    preSubmitBoundary,
    awaitConfirmation = true,
  } = args;
  if (physicalStep >= 3)
    requireCertifiedCarriage(contracts, authenticatedCarriageUtxos);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: physicalStep,
    threadOutRef,
  });
  if (physicalStep === 0) {
    requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  } else {
    requireLinearFaultStepState({
      threadUtxo,
      signer,
      schema: ScriptIntegrityStepDatums[physicalStep] as never,
      family: FAMILY,
      stepIndex: physicalStep,
    });
  }
  const reference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[physicalStep].spendingScriptHash,
    family: FAMILY,
    stepIndex: physicalStep,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextPhysicalStep].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let layout: { input_index: bigint; output_index: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `${FAMILY} physical step ${physicalStep + 1}`,
    );
    layout = {
      input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
      output_index: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        FAMILY,
      ),
    };
    return Data.to(
      { Continue: [buildArgs(layout)] } as never,
      ScriptIntegritySpendRedeemers[physicalStep] as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: reference,
    stepScript: contracts.steps[physicalStep].spendingScript,
    stepRole: `${FAMILY} physical step ${physicalStep + 1}`,
    nextAddress: contracts.steps[nextPhysicalStep].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: authenticatedCarriageUtxos,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (layout === undefined)
    throw new Error(`${FAMILY}: unresolved transaction layout`);
  return {
    txHash,
    outputIndex: layout.output_index,
    nextThreadOutRef: `${txHash}#${layout.output_index.toString()}`,
  };
};

export const submitScriptIntegrityHashMissingStep01 = (
  args: ScriptIntegrityHashMissingContinueArgs,
) => submitContinue(0, 1, args);
export const submitScriptIntegrityHashMissingStep02 = (
  args: ScriptIntegrityHashMissingContinueArgs,
) => submitContinue(1, 2, args);
export const submitScriptIntegrityHashMissingStep03 = (
  args: ScriptIntegrityHashMissingContinueArgs & {
    readonly staged?: boolean;
  },
) => submitContinue(2, args.staged === true ? 3 : 6, args);
/** Self-loops while grammar remains pending; set `closes` to enter semantic scan. */
export const submitScriptIntegrityHashMissingScriptGrammar = (
  args: ScriptIntegrityHashMissingContinueArgs & { readonly closes: boolean },
) => submitContinue(3, args.closes ? 4 : 3, args);
/** Self-loops until all authenticated script items have been folded. */
export const submitScriptIntegrityHashMissingScriptScan = (
  args: ScriptIntegrityHashMissingContinueArgs & { readonly closes: boolean },
) => submitContinue(4, args.closes ? 5 : 4, args);
/** Self-loops while field 8 grammar certification remains pending. */
export const submitScriptIntegrityHashMissingRedeemerGrammar = (
  args: ScriptIntegrityHashMissingContinueArgs & { readonly closes: boolean },
) => submitContinue(5, args.closes ? 6 : 5, args);

export const submitScriptIntegrityHashMissingStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: Omit<
  ScriptIntegrityHashMissingContinueArgs,
  "nextDatum" | "buildArgs" | "authenticatedCarriageUtxos"
> & {
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const physicalStep = 6;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: physicalStep,
    threadOutRef,
  });
  requireLinearFaultStepState({
    threadUtxo,
    signer,
    schema: ScriptIntegrityStepDatums[physicalStep] as never,
    family: FAMILY,
    stepIndex: physicalStep,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex: physicalStep,
    step: contracts.steps[physicalStep],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ScriptIntegritySpendRedeemers[physicalStep],
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

/** The generic cancel resolves the current one of all seven reachable addresses. */
export const submitScriptIntegrityHashMissingCancel = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: Omit<
  ScriptIntegrityHashMissingContinueArgs,
  "nextDatum" | "buildArgs" | "authenticatedCarriageUtxos"
> & {
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) =>
  await submitLinearFaultCancel({
    lucid,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
    categoryId,
    signer,
    threadOutRef,
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
