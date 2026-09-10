import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardTxOutput,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  type MidgardFieldCarriagePlan,
  midgardFieldCommitment,
  type MidgardFieldPreimageCertificate,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  forcedVerdictSubject,
  missingSignatureVkeyHash,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import {
  applyProtectedOutputSignerMissingScripts,
  planProtectedOutputSignerOutputOpening,
  planProtectedOutputSignerWitnessOpening,
  prepareProtectedOutputSignerMissingEvidence,
  PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES,
  PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  PROTECTED_OUTPUT_SIGNER_SCAN_BATCH,
  type ProtectedOutputSignerMissingContracts,
  type ProtectedOutputSignerMissingEvidence,
  ProtectedOutputSignerStep02RedeemerSchema,
  ProtectedOutputSignerStep03DatumSchema,
  ProtectedOutputSignerStep03RedeemerSchema,
  ProtectedOutputSignerStep04DatumSchema,
  ProtectedOutputSignerStep04RedeemerSchema,
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
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { makeProtectedOutputSignerIsolatedEvaluator } from "./protected-output-signer-missing-isolated-evaluator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCbor } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeHeader,
  makeNativeTx,
  network,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const REASON = "ProtectedOutputSignerMissing";
/** Every seam a step authenticates before it reads or commits anything. */
const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf",
  "compact_tx",
  "witness_set_anchor",
  "field_preimage",
  "field_certificate",
  "checkpoint",
  "credential",
] as const;
/** The five physical scripts, in chain order; every one carries a cancel arm. */
const PHYSICAL_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;
const FORCED_ORDER_KEY = { transactionId: "ab".repeat(32), outputIndex: 0n };
const coverage = createLifecycleCoverageRecorder();

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

// ---------------------------------------------------------------------------
// A real Ed25519 signer, so the frontier admits a genuine signature over the
// transaction id and refuses a forged one.
// ---------------------------------------------------------------------------

const signerSeed = Buffer.alloc(32, 0x2b);
const signerPrivateKey = createPrivateKey({
  key: Buffer.concat([
    Buffer.from("302e020100300506032b657004220420", "hex"),
    signerSeed,
  ]),
  format: "der",
  type: "pkcs8",
});
const signerVerificationKey = createPublicKey(signerPrivateKey)
  .export({ format: "der", type: "spki" })
  .subarray(-32);
const signerCredentialHex = missingSignatureVkeyHash(
  signerVerificationKey.toString("hex"),
);

/**
 * Address header `0x68` is a protected pub-key enterprise address; `0x60` is
 * its unprotected form and `0x78` a protected script enterprise address.
 */
const protectedOutputCbor = (
  credentialHex: string,
  addressHeader = 0x68,
): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([addressHeader]),
      Buffer.from(credentialHex, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

const validWitness = (txId: Buffer): Buffer =>
  encodeMidgardAddressWitnessItem({
    verificationKey: signerVerificationKey,
    signature: sign(null, txId, signerPrivateKey),
  });

/** The right key with a signature that does not verify. */
const forgedWitness = (): Buffer =>
  encodeMidgardAddressWitnessItem({
    verificationKey: signerVerificationKey,
    signature: Buffer.alloc(64, 0xff),
  });

/** A decoy key nobody holds, with an unverifiable signature. */
const decoyWitness = (index: number): Buffer => {
  const key = Buffer.alloc(32);
  key.writeUInt32BE(index + 1, 28);
  return encodeMidgardAddressWitnessItem({
    verificationKey: key,
    signature: Buffer.alloc(64, 0xff),
  });
};

/**
 * One protected pub-key output and a field-7 witness collection built after
 * the body is fixed: the transaction id commits the body alone, so witnesses
 * can sign it.
 */
const nativeTxWith = ({
  fee,
  credentialHex = signerCredentialHex,
  addressHeader = 0x68,
  witnesses,
}: {
  readonly fee: bigint;
  readonly credentialHex?: string;
  readonly addressHeader?: number;
  readonly witnesses: (txId: Buffer) => readonly Buffer[];
}): MidgardNativeTxFull => {
  const unsigned = makeNativeTx({
    spendInputCbors: [],
    fee,
    outputCbor: protectedOutputCbor(credentialHex, addressHeader),
  });
  const txId = computeMidgardNativeTxId(unsigned);
  return makeNativeTx({
    spendInputCbors: [],
    fee,
    outputCbor: protectedOutputCbor(credentialHex, addressHeader),
    addrTxWitsPreimageCbor: encodeCbor([...witnesses(txId)]),
  });
};

const compactHex = (nativeTx: MidgardNativeTxFull): string =>
  encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex");

const witnessSetCompactHex = (nativeTx: MidgardNativeTxFull): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

const witnessSetOf = (
  nativeTx: MidgardNativeTxFull,
): SDK.NativeTxWitnessSetCompact => {
  const derived = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    addr_tx_wits_hash: Buffer.from(derived.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(derived.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(derived.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

const evidenceFor = (
  subject: SDK.VerdictSubject,
  nativeTx: MidgardNativeTxFull,
): ProtectedOutputSignerMissingEvidence =>
  prepareProtectedOutputSignerMissingEvidence({
    subject,
    outputIndex: 0,
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
  });

/**
 * Evidence for an honest block: the production preparer refuses evidence
 * that agrees with the operator, so the honest suites prepare the closing
 * polarity and carry the honest subject instead. Every later step derives
 * its datum from the subject, so the chain sees exactly the honest claim.
 */
const honestEvidenceFor = (
  honestSubject: SDK.VerdictSubject,
  closingSubject: SDK.VerdictSubject,
  nativeTx: MidgardNativeTxFull,
): ProtectedOutputSignerMissingEvidence =>
  Object.freeze({
    ...evidenceFor(closingSubject, nativeTx),
    subject: honestSubject,
  });

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.protectedOutputSignerMissing;
  const category = harness.catalogue.categories.protectedOutputSignerMissing;
  expectRegisteredChainParity({
    registered,
    applied: applyProtectedOutputSignerMissingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  );
  const contracts: ProtectedOutputSignerMissingContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return { steps, contracts, catalogue: harness.catalogue, category };
};

const publishFamilyReferences = async (
  harness: Harness,
  steps: ProtectedOutputSignerMissingContracts["steps"],
  label: string,
) => {
  const refs: UTxO[] = [];
  for (const [index, step] of steps.entries())
    refs.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `${label} step ${(index + 1).toString()}`,
        })
      ).utxo,
    );
  const certificateRef = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${label} certificate`,
    })
  ).utxo;
  return { refs, certificateRef };
};

/**
 * One committed block on the registered chain (a normal subject or a forced
 * leaf, plus any extra normal transactions), the family's five reference
 * scripts, and submitters that hand the caller's exact datum, opening and
 * checkpoint to the chain, so every negative below is a validator refusal
 * rather than an off-chain guard.
 */
const makeScenario = async ({
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
    subject: MidgardNativeTxFull,
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
    subject: MidgardNativeTxFull,
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
    subject: MidgardNativeTxFull,
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

type Scenario = Awaited<ReturnType<typeof makeScenario>>;

const scanToTerminal = async (
  s: Scenario,
  threadOutRef: string,
  evidence: ProtectedOutputSignerMissingEvidence,
  subject: MidgardNativeTxFull,
  carriage: Awaited<
    ReturnType<typeof submitProtectedOutputSignerMissingStep03>
  >,
) => {
  let cursor = threadOutRef;
  for (;;) {
    const result = await s.step04(cursor, evidence, subject, carriage);
    cursor = result.nextThreadOutRef;
    if (result.terminal) return cursor;
  }
};

/** Swaps the certificate slot with a chunk slot in a tier-3 opening. */
const withSwappedCertificateSlot = (
  opening: SDK.FieldOpening,
): SDK.FieldOpening => {
  const copy = structuredClone(opening) as Record<string, unknown>;
  const variant = Object.values(copy)[0] as Record<string, unknown>;
  const carriage = variant.carriage as Record<string, unknown>;
  const certified = carriage.Certified as
    | { cert_ref_input_index: bigint; chunk_ref_input_indices: bigint[] }
    | undefined;
  if (certified === undefined)
    throw new Error("expected a Certified carriage to mutate");
  const [firstChunk, ...rest] = certified.chunk_ref_input_indices;
  if (firstChunk === undefined)
    throw new Error("certified carriage has no chunk");
  certified.chunk_ref_input_indices = [certified.cert_ref_input_index, ...rest];
  certified.cert_ref_input_index = firstChunk;
  return copy as SDK.FieldOpening;
};

const measuredFit = createMeasuredFitRecorder(
  "protected-output-signer-missing",
  "lifecycle",
  "318 address witnesses in a three-chunk certified field 7; forced exact-reason and direct-terminal paths",
);

describe("protectedOutputSignerMissing registered-chain lifecycle", () => {
  it("runs maximum-carriage evidence through cancel, restartable scan, mint and leased removal", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realProtectedOutputSignerMissing: true,
        alwaysFraudProofCatalogue: true,
      },
      lucidOptions: {
        evaluator: makeProtectedOutputSignerIsolatedEvaluator(),
      },
    });
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:harness-ready");
    const { steps, contracts, catalogue, category } =
      await registeredContracts(harness);
    const credential = Buffer.alloc(28, 0xa7);
    const output = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x68]), credential]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const certifiedWitnessCount = PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES;
    const witnesses = Array.from(
      { length: certifiedWitnessCount },
      (_unused, index) => decoyWitness(index),
    );
    const nativeTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbor: output,
      addrTxWitsPreimageCbor: encodeCbor(witnesses),
    });
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
    const witnessSetCompactCbor = witnessSetCompactHex(nativeTx);
    const sourceCbor = l2TransactionSourceCbor(nativeTx);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(
      Buffer.from(nativeTxId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
    const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
    const transactionsRoot = Buffer.from(trie.hash).toString("hex");
    const txInclusion = {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    };
    const predecessor = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: {
        transactionsRoot,
        l2TransactionCount: 1n,
        headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
      },
    });
    const targetStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: predecessor.header.endTime,
      emulator: harness.emulator,
    });
    const targetHeader = {
      ...makeHeader(
        predecessor.header.operatorVkey,
        targetStart,
        await countedTransactionsRoot(transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: predecessor.headerHash,
    };
    const target = await submitSuccessorBlockTx({
      lucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      anchorBlockUnit: predecessor.stateQueueBlockUnit,
      header: targetHeader,
      hubOracle: predecessor.hubOracle,
      scheduler: predecessor.scheduler,
      activeOperatorNode: predecessor.activeOperatorNode,
      activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
    });
    const setup = {
      fraudulentBlockOutRef: target.successorOutRef,
      headerHash: target.successorHeaderHash,
    };
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:block-ready");
    const evidence = prepareProtectedOutputSignerMissingEvidence({
      subject: acceptedVerdictSubject(nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
    });
    expect(evidence.witnessCarriage).toBe("Certified");
    expect(evidence.validSignerHashes).toEqual([]);
    coverage.scenario("maximum_supported_evidence");
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:evidence-ready");

    const references: UTxO[] = [];
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `protected-output-signer-missing-${index.toString()}`,
          })
        ).utxo,
      );
    }
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "protected-output-signer-missing-certificate",
      })
    ).utxo;
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:references-ready");
    const initThread = async () => {
      const init = await captureEmulatorSubmission(harness.emulator, () =>
        submitCommittedFieldShapeInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: contracts as never,
          category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      return init;
    };
    const cancelInit = await initThread();
    const cancellation = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${cancelInit.result.txHash}#${cancelInit.result.firstStepOutputIndex.toString()}`,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(cancellation.measurement.l1ByteMargin).toBeGreaterThan(0);
    coverage.cancelled("step-01");

    const init = await initThread();
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: init.result.txHash,
        outputIndex: init.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    const step01Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        evidence,
        threadUtxo,
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:step01-ready");
    const step02Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:step02-ready");
    const step03Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        referenceScriptUtxo: references[2]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:certificate-ready");

    // Field certificate: the tier-3 opening names its certificate by slot. A
    // redeemer pointing the certificate slot at a chunk (and the chunk slot at
    // the certificate) is refused by the door before any item is read.
    const witnessPlan = planProtectedOutputSignerWitnessOpening({
      evidence,
      nativeTxCompactCbor: compactCbor.toString("hex"),
      witnessSetCompactCbor,
      owner: harness.proverSigner.paymentKeyHash,
    });
    const checkpointAt = (nextItemIndex: number) =>
      SDK.missingSignatureFieldWalkCheckpoint({
        txId: nativeTxId,
        itemCount: witnessPlan.itemCount,
        totalLength: witnessPlan.preimage.length,
        nextItemIndex,
      });
    const scanState = {
      protected: {
        subject: evidence.subject,
        transaction_id: evidence.subject.transaction_id,
        witness_set_hash: evidence.witnessSetHashHex,
        output_index: 0n,
        payment_credential: evidence.paymentCredentialHex,
      },
      checkpoint_hash: checkpointAt(PROTECTED_OUTPUT_SIGNER_SCAN_BATCH)
        .checkpointHash,
      signer_present: false,
    };
    const honestReferenceInputs = [
      ...step03Result.result.carriageUtxos,
      references[3]!,
      step03Result.result.certificateUtxo!,
    ];
    const honestOpening = faultProofFieldOpening({
      planned: witnessPlan,
      referenceInputs: honestReferenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: "protected-output max scan opening",
    });
    const rawScan = (opening: SDK.FieldOpening, checkpointCbor: string) =>
      submitProtectedOutputSignerOpeningTransition({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step03Result.result.nextThreadOutRef,
        stepIndex: 3,
        nextStepIndex: 3,
        nextDatum: Data.to(
          {
            fraud_prover: harness.proverSigner.paymentKeyHash,
            data: scanState,
          } as never,
          ProtectedOutputSignerStep04DatumSchema as never,
        ),
        opening,
        checkpointCbor,
        referenceScriptUtxo: references[3]!,
        carriageReferenceInputs: honestReferenceInputs,
        redeemerSchema: ProtectedOutputSignerStep04RedeemerSchema as never,
      });
    await expectOnchainRefusal(() =>
      rawScan(
        withSwappedCertificateSlot(honestOpening),
        checkpointAt(0).checkpointCbor,
      ),
    );
    coverage.seamMutated("field_certificate");
    // Checkpoint: a position the thread never committed (one batch ahead)
    // under the committed digest, then malformed bytes.
    await expectOnchainRefusal(() =>
      rawScan(
        honestOpening,
        checkpointAt(PROTECTED_OUTPUT_SIGNER_SCAN_BATCH).checkpointCbor,
      ),
    );
    await expectOnchainRefusal(() =>
      rawScan(honestOpening, checkpointAt(0).checkpointCbor.slice(0, 80)),
    );
    coverage.seamMutated("checkpoint");

    const scanResults: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] =
      [];
    let scanThreadOutRef = step03Result.result.nextThreadOutRef;
    for (;;) {
      const result = await captureEmulatorSubmission(harness.emulator, () =>
        submitProtectedOutputSignerMissingStep04({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: scanThreadOutRef,
          evidence,
          nativeTxCompactCbor: compactCbor.toString("hex"),
          witnessSetCompactCbor,
          referenceScriptUtxo: references[3]!,
          certificateReferenceScriptUtxo: certificateReference,
          publishedCarriageUtxos: step03Result.result.carriageUtxos,
          certificateUtxo: step03Result.result.certificateUtxo,
        }),
      );
      scanResults.push(result);
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          `protected-output-signer-missing:max:scan-${scanResults.length.toString()}`,
        );
      scanThreadOutRef = result.result.nextThreadOutRef;
      if (result.result.terminal) break;
    }
    expect(scanResults).toHaveLength(10);
    // Every resume above started from nothing but the chain's committed digest
    // and the deterministic frontier: a real interruption at every batch.
    coverage.resumed();
    const step05Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: scanThreadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(step05Result.result.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    for (const capture of [
      init,
      step01Result,
      step02Result,
      step03Result,
      ...scanResults,
      step05Result,
    ]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
    measuredFit.record("accepted-init", init.measurement);
    measuredFit.record("accepted-step01", step01Result.measurement);
    measuredFit.record("accepted-step02", step02Result.measurement);
    step03Result.measurements.forEach((measurement, index) =>
      measuredFit.record(
        `accepted-carriage-${index}`,
        measurement,
        measurement.executionMemory === 0n ? "publication" : "lifecycle",
      ),
    );
    scanResults.forEach((capture, index) =>
      measuredFit.record(`accepted-scan-${index}`, capture.measurement),
    );
    measuredFit.record("accepted-step05", step05Result.measurement);
    measuredFit.record("accepted-cancel-init", cancelInit.measurement);
    measuredFit.record("accepted-cancel", cancellation.measurement);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        JSON.stringify(
          {
            lifecycle: [
              ["init", init.measurement],
              ["step01", step01Result.measurement],
              ["step02", step02Result.measurement],
              ["step03", step03Result.measurement],
              ...scanResults.map((result, index) => [
                `step04-${index.toString()}`,
                result.measurement,
              ]),
              ["step05", step05Result.measurement],
              ["cancel-init", cancelInit.measurement],
              ["cancel", cancellation.measurement],
            ],
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofProtectedOutputSignerMissing entries carry the
    // registered chain the harness built.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "protectedOutputSignerMissing",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "protected-output-signer-missing-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("0000002b");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
    measuredFit.record("accepted-removal", removal.measurement);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        JSON.stringify(
          {
            auxiliary: step03Result.measurements,
            removal: removal.measurement,
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
  }, 900_000);

  it("proves an exact forced ProtectedOutputSignerMissing rejection and refuses its mutations", async () => {
    // The operator rejected a transaction whose protected output IS signed:
    // the valid witness sits behind four unverifiable decoys, so the complete
    // scan has to reach the last position to find it.
    const nativeTx = nativeTxWith({
      fee: 11n,
      witnesses: (txId) => [
        decoyWitness(0),
        decoyWitness(1),
        decoyWitness(2),
        decoyWitness(3),
        validWitness(txId),
      ],
    });
    const s = await makeScenario({
      nativeTx,
      forcedReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const adjudicated = adjudicateMidgardNativeTxFullValidity(
      nativeTx,
      "TxIsInvalid",
    );
    const subject = forcedVerdictSubject({
      transactionId: s.block.nativeTxId,
      sourceKey: FORCED_ORDER_KEY,
      rejectionReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const evidence = evidenceFor(subject, adjudicated);
    expect(evidence.signerPresent).toBe(true);
    expect(evidence.validSignerHashes).toEqual([signerCredentialHex]);
    const membership = await s.forcedMembership();
    const thread = await s.init();
    // Reason coordinate: the leaf rejects output 0; a claim bound to output 1
    // passes every off-chain shape check and fails the exact-reason bind.
    await expectOnchainRefusal(() =>
      s.forced01(
        thread,
        Object.freeze({
          ...evidence,
          outputIndex: 1,
          subject: forcedVerdictSubject({
            transactionId: s.block.nativeTxId,
            sourceKey: FORCED_ORDER_KEY,
            rejectionReason: {
              ProtectedOutputSignerMissing: { output_index: 1n },
            },
          }),
        }),
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    // Direction: a rejecting leaf cannot open a wrongful-acceptance thread.
    await expectOnchainRefusal(() =>
      s.forced01(thread, evidence, { direction: 0n }),
    );
    // Forced leaf: a leaf carrying another verdict is not in the forced root.
    await expectOnchainRefusal(() =>
      s.forced01(thread, evidence, {
        membership: {
          ...membership,
          value: { ...membership.value, verdict: "ForcedTxValid" },
        },
      }),
    );
    // Header: a header committing another forced count is not the thread's.
    await expectOnchainRefusal(() =>
      s.forced01(thread, evidence, {
        header: {
          ...s.block.header,
          forcedTransactionCount: s.block.header.forcedTransactionCount + 1n,
        },
      }),
    );
    coverage.seamMutated("forced_leaf");
    const bound = await captureEmulatorSubmission(s.harness.emulator, () =>
      s.forced01(thread, evidence),
    );
    expect(bound.measurement.l1ByteMargin).toBeGreaterThan(0);
    measuredFit.record("forced-bind", bound.measurement);
    console.info(
      `[protected-output-signer-missing-forced-step01] ${JSON.stringify({ bytes: bound.measurement.completeSignedBytes, memory: bound.measurement.executionMemory.toString(), cpu: bound.measurement.executionSteps.toString() })}`,
    );
    const completed = await captureEmulatorSubmission(
      s.harness.emulator,
      async () => {
        const credential = await s.step02(
          bound.result.nextThreadOutRef,
          evidence,
          adjudicated,
        );
        const carriage = await s.step03(
          credential.nextThreadOutRef,
          evidence,
          adjudicated,
        );
        const terminal = await scanToTerminal(
          s,
          carriage.nextThreadOutRef,
          evidence,
          adjudicated,
          carriage,
        );
        const minted = await s.step05(terminal, evidence);
        expect(minted.fraudProofUnit).toContain(s.category.categoryId);
      },
    );
    completed.measurements.forEach((measurement, index) =>
      measuredFit.record(
        `forced-completion-${index}`,
        measurement,
        measurement.executionMemory === 0n ? "publication" : "lifecycle",
      ),
    );
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
  }, 900_000);

  it("refuses an honest accepted block, every substituted accepted seam, the adjacent over-bound field, and cancels every step", async () => {
    // Honest: the protected output's signer is present behind one decoy, so
    // the accepted block is correct and no acceptance thread may close.
    const honestTx = nativeTxWith({
      fee: 13n,
      witnesses: (txId) => [decoyWitness(0), validWitness(txId)],
    });
    // One witness past the canonical 318 maximum: 33,000 field-7 bytes, past
    // the 32,768-byte aggregate bound every carriage tier enforces on chain.
    const overBoundCredentialHex = "a7".repeat(28);
    const overBoundTx = nativeTxWith({
      fee: 17n,
      credentialHex: overBoundCredentialHex,
      witnesses: () =>
        Array.from(
          { length: PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES + 1 },
          (_unused, index) => decoyWitness(index),
        ),
    });
    const s = await makeScenario({
      nativeTx: honestTx,
      additionalTransactions: [overBoundTx],
    });
    const subject = acceptedVerdictSubject(s.block.nativeTxId);
    const evidence = honestEvidenceFor(
      subject,
      forcedVerdictSubject({
        transactionId: s.block.nativeTxId,
        sourceKey: FORCED_ORDER_KEY,
        rejectionReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
      }),
      honestTx,
    );
    expect(evidence.signerPresent).toBe(true);
    const thread = await s.init();
    // Transaction membership: a foreign transactions root cannot bind the
    // header's counted root, whatever proof rides with it.
    await expectOnchainRefusal(() =>
      s.accepted01(thread, evidence, {
        ...s.block.txInclusion!,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const bound = await s.accepted01(thread, evidence);
    const owner = s.harness.proverSigner.paymentKeyHash;
    const outputPlan = planProtectedOutputSignerOutputOpening({
      evidence,
      nativeTxCompactCbor: compactHex(honestTx),
      owner,
    });
    const outputOpening = faultProofFieldOpening({
      planned: outputPlan,
      referenceInputs: [s.refs[1]!],
      certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
      label: "honest output opening",
    });
    const credentialDatum = (data: Record<string, unknown>) =>
      s.datum(ProtectedOutputSignerStep03DatumSchema, {
        subject,
        transaction_id: subject.transaction_id,
        witness_set_hash: evidence.witnessSetHashHex,
        output_index: 0n,
        payment_credential: evidence.paymentCredentialHex,
        ...data,
      });
    // Subject coordinate: step 02 re-derives the credential state from the
    // bound subject and refuses a datum naming another transaction or output.
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: bound,
        stepIndex: 1,
        nextStepIndex: 2,
        nextDatum: credentialDatum({
          subject: acceptedVerdictSubject("99".repeat(32)),
          transaction_id: "99".repeat(32),
        }),
        opening: outputOpening,
        carriageReferenceInputs: [s.refs[1]!],
        redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema,
      }),
    );
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: bound,
        stepIndex: 1,
        nextStepIndex: 2,
        nextDatum: credentialDatum({ output_index: 1n }),
        opening: outputOpening,
        carriageReferenceInputs: [s.refs[1]!],
        redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema,
      }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    // Compact transaction: another transaction's bytes under the anchored id.
    const foreignPlan = planFaultProofFieldOpening({
      fieldIndex: 2,
      anchorTxId: computeMidgardNativeTxId(overBoundTx).toString("hex"),
      nativeTxCompactCbor: compactHex(overBoundTx),
      itemCbors: [protectedOutputCbor(overBoundCredentialHex)],
      owner,
      label: "foreign output opening",
    });
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: bound,
        stepIndex: 1,
        nextStepIndex: 2,
        nextDatum: credentialDatum({}),
        opening: faultProofFieldOpening({
          planned: foreignPlan,
          referenceInputs: [s.refs[1]!],
          certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
          label: "foreign output opening",
        }),
        carriageReferenceInputs: [s.refs[1]!],
        redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema,
      }),
    );
    coverage.seamMutated("compact_tx");
    const credential = (await s.step02(bound, evidence, honestTx))
      .nextThreadOutRef;
    const witnessPlan = planProtectedOutputSignerWitnessOpening({
      evidence,
      nativeTxCompactCbor: compactHex(honestTx),
      witnessSetCompactCbor: witnessSetCompactHex(honestTx),
      owner,
    });
    expect(witnessPlan.plan.tier).toBe("Inline");
    const scanDatum = (checkpointHash: string) =>
      s.datum(ProtectedOutputSignerStep04DatumSchema, {
        protected: {
          subject,
          transaction_id: subject.transaction_id,
          witness_set_hash: evidence.witnessSetHashHex,
          output_index: 0n,
          payment_credential: evidence.paymentCredentialHex,
        },
        checkpoint_hash: checkpointHash,
        signer_present: false,
      });
    const startHash = SDK.missingSignatureFieldWalkCheckpoint({
      txId: s.block.nativeTxId,
      itemCount: witnessPlan.itemCount,
      totalLength: witnessPlan.preimage.length,
      nextItemIndex: 0,
    }).checkpointHash;
    // Witness-set anchor: the genuine compact bytes paired with another
    // transaction's witness set fail the anchored `witness_set_hash`.
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: credential,
        stepIndex: 2,
        nextStepIndex: 3,
        nextDatum: scanDatum(startHash),
        opening: {
          WitnessFieldOpening: {
            native_tx_compact_cbor: compactHex(honestTx),
            witness_set: witnessSetOf(overBoundTx),
            carriage: {
              Inline: { preimage: witnessPlan.preimage.toString("hex") },
            },
          },
        },
        carriageReferenceInputs: [s.refs[2]!],
        redeemerSchema: ProtectedOutputSignerStep03RedeemerSchema,
      }),
    );
    coverage.seamMutated("witness_set_anchor");
    // Field preimage: a witness collection the transaction did not commit
    // fails the flat commitment under the anchored witness set.
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: credential,
        stepIndex: 2,
        nextStepIndex: 3,
        nextDatum: scanDatum(startHash),
        opening: {
          WitnessFieldOpening: {
            native_tx_compact_cbor: compactHex(honestTx),
            witness_set: witnessSetOf(honestTx),
            carriage: {
              Inline: {
                preimage: encodeCbor([forgedWitness()]).toString("hex"),
              },
            },
          },
        },
        carriageReferenceInputs: [s.refs[2]!],
        redeemerSchema: ProtectedOutputSignerStep03RedeemerSchema,
      }),
    );
    coverage.seamMutated("field_preimage");
    const carriage = await s.step03(credential, evidence, honestTx);
    const terminal = await scanToTerminal(
      s,
      carriage.nextThreadOutRef,
      evidence,
      honestTx,
      carriage,
    );
    // The complete scan found the valid signer; the honest block stands.
    await expectOnchainRefusal(() => s.rawStep05(terminal));
    coverage.scenario("honest_accepted_block_refusal");
    coverage.reason(REASON);

    // Cancel from every physical step after the first.
    const boundThread = async () =>
      await s.accepted01(await s.init(), evidence);
    const credentialThread = async () =>
      (await s.step02(await boundThread(), evidence, honestTx))
        .nextThreadOutRef;
    const scanThread = async () =>
      await s.step03(await credentialThread(), evidence, honestTx);
    await s.cancel(await boundThread(), 1);
    coverage.cancelled("step-02");
    await s.cancel(await credentialThread(), 2);
    coverage.cancelled("step-03");
    await s.cancel((await scanThread()).nextThreadOutRef, 3);
    coverage.cancelled("step-04");
    const terminalCarriage = await scanThread();
    await s.cancel(
      await scanToTerminal(
        s,
        terminalCarriage.nextThreadOutRef,
        evidence,
        honestTx,
        terminalCarriage,
      ),
      4,
    );
    coverage.cancelled("step-05");

    // Adjacent consensus bound: the 319-witness transaction is committed and
    // binds through step 02, but its field 7 has no admissible carriage. The
    // production planner refuses it before any transaction; a carriage
    // assembled below the planner is refused on chain by the certificate mint,
    // so no frontier over that field can ever be initialized.
    const overBoundId = computeMidgardNativeTxId(overBoundTx).toString("hex");
    const overBoundMaterial = deriveMidgardNativeTxFaultEvidenceMaterial(
      encodeMidgardNativeTxCanonical(overBoundTx),
    );
    const overBoundPreimage = overBoundMaterial.fieldPreimages[7]!;
    expect(overBoundPreimage.length).toBeGreaterThan(32_768);
    const overBoundEvidence: ProtectedOutputSignerMissingEvidence =
      Object.freeze({
        subject: acceptedVerdictSubject(overBoundId),
        outputIndex: 0,
        route: "witness_scan" as const,
        signerRequired: true,
        canonicalTransactionCborHex:
          encodeMidgardNativeTxCanonical(overBoundTx).toString("hex"),
        outputCborHex: protectedOutputCbor(overBoundCredentialHex).toString(
          "hex",
        ),
        paymentCredentialHex: overBoundCredentialHex,
        witnessSetHashHex: Buffer.from(
          overBoundMaterial.compact.transactionWitnessSetHash,
        ).toString("hex"),
        addressWitnessFieldPreimageHex: overBoundPreimage.toString("hex"),
        validSignerHashes: [],
        signerPresent: false,
        outputCarriage: "Inline",
        witnessCarriage: "Certified",
        checkpoints: [],
      });
    expect(() =>
      prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(overBoundId),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(overBoundTx),
      }),
    ).toThrow(/frontier exceeds the canonical maximum/u);
    const overBoundInclusion = s.block.txInclusions.get(overBoundId);
    if (overBoundInclusion === undefined)
      throw new Error("over-bound inclusion missing");
    const overBoundCredential = await s.step02(
      await s.accepted01(await s.init(), overBoundEvidence, overBoundInclusion),
      overBoundEvidence,
      overBoundTx,
    );
    expect(overBoundCredential.nextThreadOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
    expect(() =>
      planProtectedOutputSignerWitnessOpening({
        evidence: overBoundEvidence,
        nativeTxCompactCbor: compactHex(overBoundTx),
        witnessSetCompactCbor: witnessSetCompactHex(overBoundTx),
        owner,
      }),
    ).toThrow(/aggregate bound/u);
    const chunks: Buffer[] = [];
    for (
      let start = 0;
      start < overBoundPreimage.length;
      start += MIDGARD_CHUNK_BYTES_K
    )
      chunks.push(
        overBoundPreimage.subarray(
          start,
          Math.min(start + MIDGARD_CHUNK_BYTES_K, overBoundPreimage.length),
        ),
      );
    // The §8.6 certificate exactly as the core derives it for an admissible
    // field (`deriveMidgardFieldPreimageCertificate`), assembled by hand only
    // because the core refuses to split an over-bound preimage at all.
    const certificate: MidgardFieldPreimageCertificate = {
      owner: Buffer.from(owner, "hex"),
      txId: Buffer.from(overBoundId, "hex"),
      fieldIndex: 7,
      fieldHash: midgardFieldCommitment(overBoundPreimage),
      totalLength: overBoundPreimage.length,
      chunkDigests: chunks.map((chunk) => midgardFieldCommitment(chunk)),
    };
    const overBoundPlan: MidgardFieldCarriagePlan = {
      tier: "Certified",
      fieldIndex: 7,
      txId: Buffer.from(overBoundId, "hex"),
      totalLength: overBoundPreimage.length,
      commitment: midgardFieldCommitment(overBoundPreimage),
      inlinePreimage: null,
      publications: chunks.map((bytes, chunkIndex) => ({
        chunkIndex,
        bytes,
        digest: certificate.chunkDigests[chunkIndex]!,
      })),
      certificate,
      certificateAssetName: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    };
    const overBoundPlanned: FaultProofFieldOpeningPlan = {
      fieldIndex: 7,
      nativeTxId: overBoundId,
      nativeTxCompactCbor: compactHex(overBoundTx),
      preimage: overBoundPreimage,
      itemCount: PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES + 1,
      commitment: midgardFieldCommitment(overBoundPreimage).toString("hex"),
      plan: overBoundPlan,
      witnessSet: witnessSetOf(overBoundTx),
      witnessSetHash: overBoundEvidence.witnessSetHashHex,
    };
    const overBoundChunks = await publishFaultProofFieldCarriage({
      lucid: s.harness.proverLucid,
      signer: s.harness.proverSigner,
      planned: overBoundPlanned,
      publisherAddress: s.harness.proverSigner.address,
      label: "over-bound witness carriage",
    });
    expect(overBoundChunks).toHaveLength(3);
    await expectOnchainRefusal(() =>
      certifyFaultProofFieldCarriage({
        lucid: s.harness.proverLucid,
        network,
        signer: s.harness.proverSigner,
        planned: overBoundPlanned,
        certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
        certificateMintingScript:
          s.contracts.fieldPreimageCertificateMintingScript,
        certificateReferenceScriptUtxo: s.certificateRef,
        chunkUtxos: overBoundChunks,
        compactCbor: compactHex(overBoundTx),
        witnessSetCompactCbor: witnessSetCompactHex(overBoundTx),
      }),
    );
    coverage.adjacentOverBoundRefused();
  }, 1_200_000);

  it("refuses an honest forced rejection whose signer really is missing", async () => {
    // The right key with a forged signature: canonical validation admits no
    // signer, so the operator's rejection is exactly right and the
    // wrongful-rejection thread walks the field to `signer_present = False`
    // and cannot close.
    const nativeTx = nativeTxWith({
      fee: 19n,
      witnesses: () => [forgedWitness(), decoyWitness(0)],
    });
    const s = await makeScenario({
      nativeTx,
      forcedReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const adjudicated = adjudicateMidgardNativeTxFullValidity(
      nativeTx,
      "TxIsInvalid",
    );
    const subject = forcedVerdictSubject({
      transactionId: s.block.nativeTxId,
      sourceKey: FORCED_ORDER_KEY,
      rejectionReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const evidence = honestEvidenceFor(
      subject,
      acceptedVerdictSubject(s.block.nativeTxId),
      adjudicated,
    );
    expect(evidence.signerPresent).toBe(false);
    expect(evidence.validSignerHashes).toEqual([]);
    const bound = await s.forced01(await s.init(), evidence);
    const credential = await s.step02(
      bound.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const carriage = await s.step03(
      credential.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const terminal = await scanToTerminal(
      s,
      carriage.nextThreadOutRef,
      evidence,
      adjudicated,
      carriage,
    );
    await expectOnchainRefusal(() => s.rawStep05(terminal));
    coverage.scenario("honest_forced_rejection_refusal");
  }, 900_000);

  it("proves forced rejections over an unprotected and a script-locked output through the direct terminal route and refuses the direct exit for a protected key", async () => {
    // Canonical validation authorizes an unprotected output, and a protected
    // script output, with no signer at all: the operator's rejection is
    // wrong without any witness, and step 02 closes at step 05 directly.
    const forcedReason = {
      ProtectedOutputSignerMissing: { output_index: 0n },
    } as const;
    for (const [label, addressHeader, route] of [
      ["unprotected", 0x60, "unprotected_output"],
      ["script-locked", 0x78, "script_credential"],
    ] as const) {
      const nativeTx = nativeTxWith({
        fee: 23n,
        addressHeader,
        witnesses: () => [],
      });
      const s = await makeScenario({ nativeTx, forcedReason });
      const adjudicated = adjudicateMidgardNativeTxFullValidity(
        nativeTx,
        "TxIsInvalid",
      );
      const evidence = evidenceFor(
        forcedVerdictSubject({
          transactionId: s.block.nativeTxId,
          sourceKey: FORCED_ORDER_KEY,
          rejectionReason: forcedReason,
        }),
        adjudicated,
      );
      expect(evidence.route).toBe(route);
      expect(evidence.signerRequired).toBe(false);
      expect(evidence.paymentCredentialHex).toBeUndefined();
      const bound = await s.forced01(await s.init(), evidence);
      // Credential seam: the same forced claim cannot take the scan door,
      // and an in-range coordinate cannot claim the out-of-range arm.
      await expectOnchainRefusal(() =>
        s.step02(
          bound.nextThreadOutRef,
          Object.freeze({
            ...evidence,
            route: "witness_scan",
            signerRequired: true,
            paymentCredentialHex: signerCredentialHex,
          }),
          adjudicated,
        ),
      );
      await expectOnchainRefusal(() =>
        s.step02(
          bound.nextThreadOutRef,
          Object.freeze({ ...evidence, route: "coordinate_out_of_range" }),
          adjudicated,
        ),
      );
      const direct = await captureEmulatorSubmission(s.harness.emulator, () =>
        s.step02(bound.nextThreadOutRef, evidence, adjudicated),
      );
      expect(direct.result.stage).toBe("step05");
      expect(direct.measurement.l1ByteMargin).toBeGreaterThan(0);
      measuredFit.record(`forced-direct-${label}`, direct.measurement);
      console.info(
        `[protected-output-signer-missing-forced-direct-${label}] ${JSON.stringify({ bytes: direct.measurement.completeSignedBytes, memory: direct.measurement.executionMemory.toString(), cpu: direct.measurement.executionSteps.toString() })}`,
      );
      const minted = await s.step05(direct.result.nextThreadOutRef, evidence);
      expect(minted.fraudProofUnit).toContain(s.category.categoryId);
    }
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");

    // Credential seam, the other way: a protected pub-key output whose
    // signer really is missing cannot skip the scan through the direct exit.
    const unsignedTx = nativeTxWith({
      fee: 29n,
      witnesses: () => [forgedWitness()],
    });
    const s = await makeScenario({ nativeTx: unsignedTx, forcedReason });
    const adjudicated = adjudicateMidgardNativeTxFullValidity(
      unsignedTx,
      "TxIsInvalid",
    );
    const honest = honestEvidenceFor(
      forcedVerdictSubject({
        transactionId: s.block.nativeTxId,
        sourceKey: FORCED_ORDER_KEY,
        rejectionReason: forcedReason,
      }),
      acceptedVerdictSubject(s.block.nativeTxId),
      adjudicated,
    );
    expect(honest.route).toBe("witness_scan");
    const bound = await s.forced01(await s.init(), honest);
    await expectOnchainRefusal(() =>
      s.step02(
        bound.nextThreadOutRef,
        Object.freeze({
          ...honest,
          route: "unprotected_output",
          signerRequired: false,
          signerPresent: false,
        }),
        adjudicated,
      ),
    );
    coverage.seamMutated("credential");
  }, 900_000);

  it("declares the complete lifecycle coverage it exercised", () => {
    // Recorded while the suites above ran, never pre-filled.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
  });
});
