import { type MidgardNativeTxFull } from "@al-ft/midgard-core";
import { type MidgardForcedTxFull } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import {
  type ProtectedOutputSignerMissingEvidence,
  ProtectedOutputSignerStep05RedeemerSchema,
  submitProtectedOutputSignerMissingCancel,
  submitProtectedOutputSignerMissingStep01Accepted,
  submitProtectedOutputSignerMissingStep01Forced,
  submitProtectedOutputSignerMissingStep02,
  submitProtectedOutputSignerMissingStep03,
  submitProtectedOutputSignerMissingStep04,
  submitProtectedOutputSignerMissingStep05,
} from "../src/protected-output-signer-missing/index.js";
import { submitProtectedOutputSignerOpeningTransition } from "../src/protected-output-signer-missing/submit-opening-transition.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  compactHex,
  FORCED_ORDER_KEY,
  publishFamilyReferences,
  registeredContracts,
  witnessSetCompactHex,
} from "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  network,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * One committed block on the registered chain (a normal subject or a forced
 * leaf, plus any extra normal transactions), the family's five reference
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
    const result = await submitCommittedFieldShapeInit({
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
    });
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
      await submitProtectedOutputSignerMissingStep01Accepted({
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
      })
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
      direction = 1n,
    }: {
      readonly header?: SDK.Header;
      readonly membership?: SDK.RootMembershipProof<
        SDK.OutputReference,
        SDK.ForcedInclusionTxV1
      >;
      readonly direction?: bigint;
    } = {},
  ) =>
    submitProtectedOutputSignerMissingStep01Forced({
      ...common(0),
      threadOutRef,
      evidence,
      forcedSource: {
        header,
        membership: membership ?? (await forcedMembership()),
        direction,
      },
    });
  const step02 = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    subject: MidgardNativeTxFull | MidgardForcedTxFull,
  ) =>
    submitProtectedOutputSignerMissingStep02({
      ...common(1),
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactHex(subject),
      witnessSetCompactCbor: witnessSetCompactHex(subject),
      certificateReferenceScriptUtxo: certificateRef,
    });
  const step03 = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    subject: MidgardNativeTxFull | MidgardForcedTxFull,
  ) =>
    submitProtectedOutputSignerMissingStep03({
      ...common(2),
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactHex(subject),
      witnessSetCompactCbor: witnessSetCompactHex(subject),
      certificateReferenceScriptUtxo: certificateRef,
    });
  const step04 = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
    subject: MidgardNativeTxFull | MidgardForcedTxFull,
    carriage: Awaited<
      ReturnType<typeof submitProtectedOutputSignerMissingStep03>
    >,
  ) =>
    submitProtectedOutputSignerMissingStep04({
      ...common(3),
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactHex(subject),
      witnessSetCompactCbor: witnessSetCompactHex(subject),
      certificateReferenceScriptUtxo: certificateRef,
      publishedCarriageUtxos: carriage.carriageUtxos,
      ...(carriage.certificateUtxo === undefined
        ? {}
        : { certificateUtxo: carriage.certificateUtxo }),
    });
  const step05 = (
    threadOutRef: string,
    evidence: ProtectedOutputSignerMissingEvidence,
  ) =>
    submitProtectedOutputSignerMissingStep05({
      ...common(4),
      threadOutRef,
      evidence,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  /** Step 05 without the off-chain polarity guard: the validator decides. */
  const rawStep05 = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: "protected-output-signer-missing",
      stepIndex: 4,
      threadOutRef,
    });
    return await submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family: "protected-output-signer-missing",
      stepIndex: 4,
      step: contracts.steps[4],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: ProtectedOutputSignerStep05RedeemerSchema,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: refs[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  const cancel = (threadOutRef: string, index: number) =>
    submitProtectedOutputSignerMissingCancel({
      ...common(index),
      threadOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const datum = (schema: unknown, data: unknown) =>
    Data.to(
      { fraud_prover: harness.proverSigner.paymentKeyHash, data } as never,
      schema as never,
    );
  /** Hands an arbitrary datum, opening and checkpoint to a step validator. */
  const rawTransition = (input: {
    readonly threadOutRef: string;
    readonly stepIndex: 1 | 2 | 3;
    readonly nextStepIndex: 2 | 3 | 4;
    readonly nextDatum: string;
    readonly opening: SDK.FieldOpening;
    readonly checkpointCbor?: string;
    readonly carriageReferenceInputs: readonly UTxO[];
    readonly redeemerSchema: unknown;
  }) =>
    submitProtectedOutputSignerOpeningTransition({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: input.threadOutRef,
      stepIndex: input.stepIndex,
      nextStepIndex: input.nextStepIndex,
      nextDatum: input.nextDatum,
      opening: input.opening,
      ...(input.checkpointCbor === undefined
        ? {}
        : { checkpointCbor: input.checkpointCbor }),
      referenceScriptUtxo: refs[input.stepIndex]!,
      carriageReferenceInputs: input.carriageReferenceInputs,
      redeemerSchema: input.redeemerSchema as never,
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
    step03,
    step04,
    step05,
    rawStep05,
    cancel,
    datum,
    rawTransition,
  };
};
