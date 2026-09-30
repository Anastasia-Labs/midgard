import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  MissingSignatureForcedWitnessSpendRedeemer,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import {
  planMissingSignatureAddressWitnessesOpening,
  planMissingSignatureRequiredSignersOpening,
} from "../../src/missing-signature/evidence.js";
import { submitMissingSignatureForcedAction } from "../../src/missing-signature/submit-forced.js";
import { submitMissingSignatureInit } from "../../src/missing-signature/submit-missing-signature-init.js";
import type { PreparedMissingSignatureWrongfulRejection } from "../../src/missing-signature/wrongful-rejection.js";
import type { VanRossemFitMeasurement } from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { requireComputationThreadToken } from "../../src/step-support.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { expectProofFit } from "./emulator/proof-fit.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { submitSetupTx } from "./emulator/setup-tx.js";
import { makeMissingSignatureEmulatorHarness } from "./missing-signature-emulator.js";
import { buildMissingSignatureForcedTransaction as transactionFor } from "./missing-signature-forced-shapes.js";
import { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./submit-init-emulator-shared.js";

export const network = "Custom" as const;
type Harness = Awaited<ReturnType<typeof makeMissingSignatureEmulatorHarness>>;
type ForcedLeaf = {
  readonly outputIndex: bigint;
  readonly value: PreparedMissingSignatureWrongfulRejection["forcedSource"]["membership"]["value"];
};
const commitForcedBlock = async (
  harness: Harness,
  leaves: readonly ForcedLeaf[],
) => {
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("funder key absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const keyed = leaves.map((leaf) => ({
    ...leaf,
    key: {
      ...base.eventKey.ForcedTransactionEventKey.tx_order_id,
      outputIndex: leaf.outputIndex,
    },
  }));
  const encoded = keyed.map((leaf) => ({
    key: Buffer.from(Data.to(leaf.key, OutputReference), "hex"),
    value: Buffer.from(
      Data.to(leaf.value as never, ForcedInclusionTxV1Schema as never),
      "hex",
    ),
  }));
  const root = await buildCountedRoot(
    ROOT_DOMAINS.forcedTransactionsV1,
    encoded,
  );
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const { key, value } of encoded) await trie.insert(key, value);
  const memberships: PreparedMissingSignatureWrongfulRejection["forcedSource"]["membership"][] =
    await Promise.all(
      keyed.map(async (leaf, index) => ({
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key: leaf.key,
        value: leaf.value,
        proof: Data.from(
          (await trie.prove(encoded[index]!.key)).toCBOR().toString("hex"),
          Proof,
        ),
      })) as never,
    );
  const header = {
    ...base.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: BigInt(leaves.length),
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  return { base, header, setup, root, memberships };
};

/**
 * One block whose single forced leaf rejects `transactionFor(shape)` with
 * `RequiredSignerUnsigned { signerIndex }`, the published step scripts, and
 * the stage runners every missing-signature forced lifecycle drives. Each
 * submission's measurement is appended to `fitMeasurements`.
 */
export const setupMissingSignatureForcedScenario = async (
  fitMeasurements: VanRossemFitMeasurement[],
  shape: Parameters<typeof transactionFor>[0] = {},
  signerIndex = 0n,
) => {
  const harness = await makeMissingSignatureEmulatorHarness();
  const transaction = transactionFor(shape);
  const reason = { RequiredSignerUnsigned: { signer_index: signerIndex } };
  const block = await commitForcedBlock(harness, [
    {
      outputIndex: 0n,
      value: {
        tx_id: transaction.transactionId,
        submitted_source: transaction.source,
        verdict: { ForcedTxInvalid: { reason } },
      },
    },
  ]);
  const membership = block.memberships[0]!;
  const subject = forcedVerdictSubject({
    transactionId: transaction.transactionId,
    sourceKey: membership.key,
    rejectionReason: reason,
  });
  const prepared: PreparedMissingSignatureWrongfulRejection = {
    detectionId: "test",
    violationId: "missing-signature-wrongful-rejection",
    position: 0n,
    forcedIndex: 0,
    headerHash: block.setup.headerHash,
    transactionId: transaction.transactionId,
    nativeTxCompactCbor: transaction.source.compact_cbor,
    witnessSetCompact: transaction.witnessSetCompact,
    verifiedWitnessSetHash:
      transaction.transaction.compact.transactionWitnessSetHash.toString("hex"),
    forcedSource: { header: block.header, membership, direction: 1n },
    evidence: {
      subject,
      signerIndex,
      requiredSignerHashes: transaction.requiredSignerHashes,
      addrTxWits: transaction.addrTxWits,
    },
    witnessIndex: 0n,
  };
  const physical = [
    harness.missingSignature.steps[0],
    harness.missingSignature.forcedStep,
    harness.missingSignature.forcedSigner,
    harness.missingSignature.forcedWitness,
  ];
  const refs: UTxO[] = [];
  const record = async <T>(
    stage: string,
    submit: () => Promise<T>,
  ): Promise<T> => {
    const { result, measurements } = await captureEmulatorSubmission(
      harness.emulator,
      submit,
    );
    for (const measurement of measurements) {
      fitMeasurements.push({
        name: `${expect.getState().currentTestName}:${stage}:${fitMeasurements.length}`,
        kind: measurement.executionMemory === 0n ? "publication" : "lifecycle",
        maximumShape: expect.getState().currentTestName!,
        signedBytes: measurement.completeSignedBytes,
        memoryUnits: measurement.executionMemory,
        cpuUnits: measurement.executionSteps,
      });
      expectProofFit({
        stage,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
      console.info(
        `[missing-signature-forced-fit] ${JSON.stringify({ stage, bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })}`,
      );
    }
    return result;
  };
  for (const [index, step] of physical.entries())
    refs.push(
      (
        await record(`publish-${index}`, () =>
          publishPlainReferenceScriptUtxo({
            lucid: harness.proverLucid,
            script: step.spendingScript,
            label: `missing-signature-forced-${index}`,
          }),
        )
      ).utxo,
    );
  const init = async () =>
    (
      await record("init", () =>
        submitMissingSignatureInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: harness.missingSignature,
          category: harness.category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: harness.catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).nextThreadOutRef;
  const certificates = new Map<number, UTxO>();
  const action = (threadOutRef: string, index: number, evidence = prepared) =>
    record(`step-${index}`, () =>
      submitMissingSignatureForcedAction({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        prepared: evidence,
        referenceScriptUtxo: refs[index]!,
        certificateUtxo: certificates.get(index),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const advance = async (
    thread: string,
    index: number,
    evidence = prepared,
  ) => {
    const result = await action(thread, index, evidence);
    if (result.kind !== "advanced") throw new Error("expected advance");
    return result.nextThreadOutRef;
  };
  const remove = async () => {
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const now = BigInt(harness.emulator.now());
    return record("remove", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "missingSignature",
        fraudulentHeaderHash: block.setup.headerHash,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
  };
  const certify = async (index: 2 | 3) => {
    const planned =
      index === 2
        ? planMissingSignatureRequiredSignersOpening({
            anchorSourceKind: 1n,
            anchorTxId: prepared.transactionId,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            requiredSignerHashes: prepared.evidence.requiredSignerHashes,
            owner: harness.proverSigner.paymentKeyHash,
          })
        : planMissingSignatureAddressWitnessesOpening({
            anchorSourceKind: 1n,
            anchorTxId: prepared.transactionId,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            addrTxWits: prepared.evidence.addrTxWits,
            witnessSet: prepared.witnessSetCompact,
            anchorWitnessSetHash: prepared.verifiedWitnessSetHash,
            owner: harness.proverSigner.paymentKeyHash,
          });
    const chunks = await record("field-publication", () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: "missing-signature max field",
      }),
    );
    if (planned.plan.tier !== "Certified") return planned;
    const certificateReference = await record("certificate-publication", () =>
      publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "missing-signature certificate",
      }),
    );
    const certificate = await record("certify", () =>
      certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: certificateReference.utxo,
        chunkUtxos: chunks,
        compactCbor: prepared.nativeTxCompactCbor,
        witnessSetCompactCbor: transaction.source.witness_set_compact_cbor,
      }),
    );
    certificates.set(index, certificate.certificateUtxo);
    return planned;
  };
  /**
   * Submits the terminal step directly, past the builder's local admission,
   * so the validator's own verdict on `witnessIndex` is observable.
   */
  const finalizeRaw = async (thread: string, witnessIndex = 0n) => {
    const [txHash, index] = thread.split("#");
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(index) },
    ]);
    if (threadUtxo === undefined) throw new Error("thread absent");
    const planned = planMissingSignatureAddressWitnessesOpening({
      anchorSourceKind: 1n,
      anchorTxId: prepared.transactionId,
      nativeTxCompactCbor: prepared.nativeTxCompactCbor,
      addrTxWits: prepared.evidence.addrTxWits,
      witnessSet: prepared.witnessSetCompact,
      anchorWitnessSetHash: prepared.verifiedWitnessSetHash,
      owner: harness.proverSigner.paymentKeyHash,
    });
    return submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family: "missing-signature forced",
      stepIndex: 3,
      step: harness.missingSignature.forcedWitness,
      computationThread: harness.missingSignature.computationThread,
      fraudProof: harness.missingSignature.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken: requireComputationThreadToken({
        utxo: threadUtxo,
        computationThreadPolicyId:
          harness.missingSignature.computationThread.policyId,
        categoryId: harness.category.categoryId,
        categoryLabel: "missing-signature",
      }),
      spendRedeemerSchema: MissingSignatureForcedWitnessSpendRedeemer,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
        addr_tx_wits_opening: faultProofFieldOpening({
          planned,
          label: "claimed signature",
        }),
        witness_index: witnessIndex,
      }),
      referenceScriptUtxo: refs[3]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  return {
    harness,
    block,
    prepared,
    transaction,
    refs,
    init,
    action,
    advance,
    remove,
    record,
    certify,
    finalizeRaw,
    transactionCbor: encodeMidgardForcedTxCanonical(
      transaction.transaction,
    ).toString("hex"),
  };
};
