import { type MidgardNativeTxFull } from "@al-ft/midgard-core";
import { type MidgardForcedTxFull } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

import { buildCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import type { FaultProofFieldGrammarDeployment } from "../src/field-grammar-certificate.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { buildLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import {
  buildProtectedOutputSignerMissingCancel,
  buildProtectedOutputSignerMissingStep01Accepted,
  buildProtectedOutputSignerMissingStep01Forced,
  buildProtectedOutputSignerMissingStep02,
  type ProtectedOutputSignerMissingEvidence,
} from "../src/protected-output-signer-missing/index.js";
import { buildProtectedOutputSignerOpeningTransition } from "../src/protected-output-signer-missing/submit-opening-transition.js";
import {
  buildSignerWitnessScan,
  buildSignerWitnessTerminal,
  SIGNER_WITNESS_TERMINAL_STEP_INDEX,
  SignerWitnessTerminalRedeemerSchema,
} from "../src/signer-witness-scan/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  compactHex,
  FORCED_ORDER_KEY,
  publishFamilyReferences,
  registeredContracts,
  witnessSetCompactHex,
} from "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { submitRawSignerWitnessScanTurn } from "./support/emulator/signer-witness-scan-raw-turn.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  network,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  buildAndSubmitUnjournaled,
  type SubmittedStep,
  submitTurnsUnjournaled,
} from "./support/submit-unjournaled.js";

/**
 * One committed block on the registered chain (a normal subject or a forced
 * leaf, plus any extra normal transactions), the family's four reference
 * scripts, and submitters that hand the caller's exact datum, opening and
 * checkpoint to the chain, so every negative below is a validator refusal
 * rather than an off-chain guard.
 */
export const makeScenario = async ({
  nativeTx,
  forcedReason,
  additionalTransactions = [],
}: {
  readonly nativeTx: MidgardNativeTxFull;
  readonly forcedReason?: SDK.RejectionReason;
  readonly additionalTransactions?: readonly MidgardNativeTxFull[];
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realProtectedOutputSignerMissing: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const { steps, contracts, category } = await registeredContracts(harness);
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject:
      forcedReason === undefined
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: FORCED_ORDER_KEY,
            verdict: { ForcedTxInvalid: { reason: forcedReason } },
          },
    additionalTransactions: [...additionalTransactions],
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  const { refs, certificateRef } = await publishFamilyReferences(
    harness,
    steps,
    "protected-output scenario",
  );
  const common = (index: number) => ({
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
    referenceScriptUtxo: refs[index]!,
  });
  const init = async () => {
    const result = await buildAndSubmitUnjournaled(
      buildCommittedFieldShapeInit,
      {
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      },
    );
    return result.nextThreadOutRef;
  };
  const threadAt = async (threadOutRef: string) => {
    const [txHash, outputIndex] = threadOutRef.split("#");
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (threadUtxo === undefined) throw new Error("thread absent");
    return threadUtxo;
  };
  const accepted01 = async (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    txInclusion = block.txInclusion!,
  ) => {
    const threadUtxo = await threadAt(threadOutRef);
    const { threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: "protected-output-signer-missing",
      stepIndex: 0,
      threadOutRef,
    });
    return (
      await buildAndSubmitUnjournaled(
        buildProtectedOutputSignerMissingStep01Accepted,
        {
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer: harness.proverSigner,
          evidence,
          threadUtxo,
          threadToken,
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion,
          referenceScriptUtxo: refs[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        },
      )
    ).nextThreadOutRef;
  };
  const forcedMembership = () =>
    buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: { tx_order_id: FORCED_ORDER_KEY },
      },
    });
  const forced01 = async (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    {
      header = block.header,
      membership,
    }: {
      readonly header?: SDK.Header;
      readonly membership?: SDK.RootMembershipProof<
        SDK.OutputReference,
        SDK.ForcedInclusionTxV1
      >;
    } = {},
  ) =>
    buildAndSubmitUnjournaled(buildProtectedOutputSignerMissingStep01Forced, {
      ...common(0),
      threadOutRef,
      evidence,
      forcedSource: {
        header,
        membership: membership ?? (await forcedMembership()),
      },
    });
  const step02 = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    subject: MidgardNativeTxFull | MidgardForcedTxFull,
    fieldGrammar?: FaultProofFieldGrammarDeployment,
  ) =>
    submitTurnsUnjournaled(buildProtectedOutputSignerMissingStep02, {
      ...common(1),
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactHex(subject),
      witnessSetCompactCbor: witnessSetCompactHex(subject),
      certificateReferenceScriptUtxo: certificateRef,
      ...(fieldGrammar === undefined ? {} : { fieldGrammar }),
    });
  /** One scan turn; a resumed turn reuses the opening turn's carriage. */
  const scan = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    subject: MidgardNativeTxFull | MidgardForcedTxFull,
    carriage?: Pick<
      SubmittedStep<typeof buildSignerWitnessScan>,
      "carriageUtxos" | "certificateUtxo"
    >,
  ) =>
    submitTurnsUnjournaled(buildSignerWitnessScan, {
      ...common(2),
      family: "protected-output-signer-missing",
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactHex(subject),
      witnessSetCompactCbor: witnessSetCompactHex(subject),
      certificateReferenceScriptUtxo: certificateRef,
      ...(carriage === undefined
        ? {}
        : {
            publishedCarriageUtxos: carriage.carriageUtxos,
            ...(carriage.certificateUtxo === undefined
              ? {}
              : { certificateUtxo: carriage.certificateUtxo }),
          }),
    });
  const terminal = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
  ) =>
    buildAndSubmitUnjournaled(buildSignerWitnessTerminal, {
      ...common(3),
      family: "protected-output-signer-missing",
      threadOutRef,
      evidence,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  /** The terminal without the off-chain polarity guard: the validator decides. */
  const rawTerminal = async (threadOutRef: string) => {
    const stepIndex = SIGNER_WITNESS_TERMINAL_STEP_INDEX;
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: "protected-output-signer-missing",
      stepIndex,
      threadOutRef,
    });
    return await buildAndSubmitUnjournaled(buildLinearFaultFinalize, {
      lucid: harness.proverLucid,
      family: "protected-output-signer-missing",
      stepIndex,
      step: contracts.steps[stepIndex],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: SignerWitnessTerminalRedeemerSchema,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: refs[stepIndex]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  const cancel = (threadOutRef: string, index: number) =>
    buildAndSubmitUnjournaled(buildProtectedOutputSignerMissingCancel, {
      ...common(index),
      threadOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const datum = (schema: unknown, data: unknown) =>
    Data.to(
      { fraud_prover: harness.proverSigner.paymentKeyHash, data } as never,
      schema as never,
    );
  /** Hands an arbitrary datum and opening to step 02. */
  const rawTransition = (input: {
    readonly threadOutRef: string;
    readonly stepIndex: 1;
    readonly nextStepIndex: 2 | 3;
    readonly nextDatum: string;
    readonly opening: SDK.FieldOpening;
    readonly carriageReferenceInputs: readonly UTxO[];
    readonly redeemerSchema: unknown;
  }) =>
    buildAndSubmitUnjournaled(buildProtectedOutputSignerOpeningTransition, {
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: input.threadOutRef,
      stepIndex: input.stepIndex,
      nextStepIndex: input.nextStepIndex,
      nextDatum: input.nextDatum,
      openingFor: () => input.opening,
      referenceScriptUtxo: refs[input.stepIndex]!,
      carriageReferenceInputs: input.carriageReferenceInputs,
      redeemerSchema: input.redeemerSchema as never,
    });
  /** Hands an arbitrary datum, opening and checkpoint to the shared scan. */
  const rawScan = (input: {
    readonly threadOutRef: string;
    readonly terminal?: boolean;
    readonly nextDatum: string;
    readonly opening:
      | SDK.FieldOpening
      | ((ctx: RedeemerContext) => SDK.FieldOpening);
    readonly checkpointCbor: string | null;
    readonly carriageReferenceInputs: readonly UTxO[];
  }) =>
    submitRawSignerWitnessScanTurn({
      lucid: harness.proverLucid,
      family: "protected-output-signer-missing",
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: input.threadOutRef,
      terminal: input.terminal ?? false,
      nextDatum: input.nextDatum,
      openingFor:
        typeof input.opening === "function"
          ? input.opening
          : () => input.opening as SDK.FieldOpening,
      checkpointCbor: input.checkpointCbor,
      referenceScriptUtxo: refs[2]!,
      carriageReferenceInputs: input.carriageReferenceInputs,
    });
  return {
    harness,
    contracts,
    category,
    block,
    setup,
    refs,
    certificateRef,
    init,
    accepted01,
    forced01,
    forcedMembership,
    step02,
    scan,
    terminal,
    rawTerminal,
    cancel,
    datum,
    rawTransition,
    rawScan,
  };
};
