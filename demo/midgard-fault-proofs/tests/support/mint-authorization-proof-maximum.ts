import { inspect } from "node:util";

import {
  computeHash28,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { buildMintAuthorizationStep02Evidence } from "../../src/mint-authorization/evidence.js";
import {
  submitMintAuthorizationInit,
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03WitnessAbsence,
  submitMintAuthorizationStep04AdvanceComplete,
  submitMintAuthorizationStep04ResolveNext,
  submitMintAuthorizationStep05,
} from "../../src/mint-authorization/index.js";
import type { VanRossemFitMeasurement } from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { publishProofChunks } from "../../src/publish-proof-chunks.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { commitCountedRoot } from "../../src/transition-trace/phas.js";
import { createStructuredDataPreimageRequirement } from "../../src/workflow/raw-datum-preimage.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { publishFaultProofWitnessReferenceScripts } from "./emulator/reference-scripts.js";
import {
  buildMintAuthorizationLedgerFixture,
  buildMintAuthorizationSubject,
  makeMintAuthorizationEmulatorHarness,
  publishMintAuthorizationReferenceScripts,
  referenceInputItemCbor,
  setupMintAuthorizationScenario,
} from "./mint-authorization-emulator.js";
import { mintAuthorizationMaximumMintField } from "./mint-authorization-maxima.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

/** Synthetic sibling frontiers measure physical proof ceilings separately from retained-DA workflow cases. */
export const runMintAuthorizationProofMaximum = async (
  selectedMint?: Pick<
    ReturnType<typeof mintAuthorizationMaximumMintField>,
    "mintItemCbors" | "targetPolicyIndex"
  >,
  scenario = "maximum-proofs",
): Promise<VanRossemFitMeasurement[]> => {
  const h = await makeMintAuthorizationEmulatorHarness();
  const measurements: VanRossemFitMeasurement[] = [];
  const capture = async <T>(
    stage: string,
    kind: "publication" | "lifecycle",
    operation: () => Promise<T>,
  ): Promise<T> => {
    const result = await captureEmulatorSubmission(h.emulator, operation).catch(
      (cause: unknown) => {
        throw new Error(`${stage}: ${inspect(cause, { depth: 20 })}`);
      },
    );
    result.measurements.forEach((m, index) =>
      measurements.push({
        name: `${scenario}/${stage}/${index}`,
        kind,
        maximumShape: `64-branch source/event/transition/ledger openings; ${selectedMint === undefined ? "maximum mint-field tail" : "32768-byte selected asset map"}`,
        signedBytes: m.completeSignedBytes,
        memoryUnits: m.executionMemory,
        cpuUnits: m.executionSteps,
      }),
    );
    return result.result;
  };
  const reference = referenceInputItemCbor({
    txIdHex: "cd".repeat(32),
    outputIndex: 2,
  });
  const ledger = await buildMintAuthorizationLedgerFixture({
    txIdHex: "cd".repeat(32),
    outputIndex: 2,
  });
  const ledgerProof = syntheticDeepMembershipProof({
    key: Buffer.from(reference, "hex"),
    value: Buffer.from(ledger.descriptorCbor, "hex"),
    branchLevels: 64,
  });
  const mint = selectedMint ?? mintAuthorizationMaximumMintField();
  const subject = buildMintAuthorizationSubject({
    mintItemCbors: mint.mintItemCbors,
    referenceInputItemCbors: [reference],
  });
  let claimEvidence:
    | Awaited<ReturnType<typeof buildMintAuthorizationStep02Evidence>>
    | undefined;
  const { block, setup } = await setupMintAuthorizationScenario({
    harness: h,
    subject,
    priorLedgerRoot: ledgerProof.transactionsPhasRoot,
    transformBlock: async (base) => {
      if (base.txInclusion === null)
        throw new Error("missing source inclusion");
      const evidence = await buildMintAuthorizationStep02Evidence({
        reconstruction: base.reconstruction,
        eventKey: { L2TransactionEventKey: { tx_id: base.nativeTxId } },
      });
      const event = syntheticDeepMembershipProof({
        key: Buffer.from(
          Data.to(evidence.eventToStepMembership.key, SDK.EventKey),
          "hex",
        ),
        value: Buffer.from(
          Data.to(evidence.eventToStepMembership.value, SDK.EventToStepValue),
          "hex",
        ),
        branchLevels: 64,
      });
      const trace = syntheticDeepMembershipProof({
        key: Buffer.from(Data.to(evidence.transitionStepMembership.key), "hex"),
        value: Buffer.from(
          Data.to(evidence.transitionStepMembership.value, SDK.TransitionStep),
          "hex",
        ),
        branchLevels: 64,
      });
      const source = syntheticDeepMembershipProof({
        key: Buffer.from(base.nativeTxId, "hex"),
        value: Buffer.from(base.txInclusion.l2TransactionSourceCbor!, "hex"),
        branchLevels: 64,
      });
      const eventRoot = await commitCountedRoot({
        domain: evidence.eventToStepMembership.domain,
        phasRoot: event.transactionsPhasRoot,
        count: evidence.eventToStepMembership.count,
      });
      const traceRoot = await commitCountedRoot({
        domain: evidence.transitionStepMembership.domain,
        phasRoot: trace.transactionsPhasRoot,
        count: evidence.transitionStepMembership.count,
      });
      const header = {
        ...base.header,
        eventToStepRoot: eventRoot,
        transitionTraceRoot: traceRoot,
        transactionsRoot: await commitCountedRoot({
          domain: SDK.ROOT_DOMAINS.transactionsV1,
          phasRoot: source.transactionsPhasRoot,
          count: base.header.l2TransactionCount,
        }),
      };
      const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
        "hex",
      );
      claimEvidence = {
        header,
        eventToStepMembership: {
          ...evidence.eventToStepMembership,
          root: eventRoot,
          phas_root: event.transactionsPhasRoot,
          proof: Data.from(event.proofCbor, SDK.Proof),
        },
        transitionStepMembership: {
          ...evidence.transitionStepMembership,
          root: traceRoot,
          phas_root: trace.transactionsPhasRoot,
          proof: Data.from(trace.proofCbor, SDK.Proof),
        },
      };
      return {
        ...base,
        header,
        headerHash,
        reconstruction: { ...base.reconstruction, header, headerHash },
        txInclusion: {
          ...base.txInclusion,
          transactionsPhasRoot: source.transactionsPhasRoot,
          txMembershipProofCbor: source.proofCbor,
          txMembershipProof: Data.from(source.proofCbor, SDK.Proof),
        },
      };
    },
  });
  if (claimEvidence === undefined || block.txInclusion === null)
    throw new Error("missing maximum openings");
  const refs = await capture("publish", "publication", async () => {
    const witnesses = await publishFaultProofWitnessReferenceScripts({
      lucid: h.proverLucid,
      realBlueprint: h.realBlueprint,
      includeChunkedVerify: true,
      includePexcludes: true,
      computationThreadMintingScript: h.family.computationThread.mintingScript,
      fraudProofMintingScript: h.family.fraudProof.mintingScript,
    });
    const steps = await publishMintAuthorizationReferenceScripts({
      lucid: h.proverLucid,
      contracts: h.family,
    });
    const certificate = (
      await publishPlainReferenceScriptUtxo({
        lucid: h.proverLucid,
        script: h.contracts.fieldPreimageCertificate.mintingScript,
        label: "mint maximum certificate",
      })
    ).utxo;
    return { witnesses, steps, certificate };
  });
  const chunks = await capture("source-publications", "publication", () =>
    publishProofChunks({
      lucid: h.proverLucid,
      network,
      signer: h.proverSigner,
      proofCbor: block.txInclusion!.txMembershipProofCbor!,
    }),
  );
  const common = {
    lucid: h.proverLucid,
    contracts: h.family,
    categoryId: h.category.categoryId,
    signer: h.proverSigner,
    nativeTxCompactCbor: block.nativeTxCompactCbor,
    witnessSet: subject.witnessSetCompact,
  };
  const init = await capture("init", "lifecycle", () =>
    submitMintAuthorizationInit({
      ...common,
      blueprint: h.realBlueprint,
      network,
      category: h.category,
      catalogue: {
        policyId: h.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          h.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: h.catalogue.root,
      },
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: refs.witnesses,
    }),
  );
  const one = await capture("step_01", "lifecycle", () =>
    submitMintAuthorizationStep01({
      ...common,
      blueprint: h.realBlueprint,
      network,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion!,
      publishedProofChunks: chunks.chunks,
      referenceScriptUtxo: refs.steps[0],
      witnessReferenceScripts: refs.witnesses,
    }),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: SDK.MIDGARD_FIELD_INDEX.mint,
    anchorTxId: block.nativeTxId,
    nativeTxCompactCbor: block.nativeTxCompactCbor,
    itemCbors: subject.mintItemCbors.map((item) => Buffer.from(item, "hex")),
    owner: h.proverSigner.paymentKeyHash,
    label: "maximum mint field with maximum proofs",
  });
  const field = await capture("field-publications", "publication", () =>
    publishFaultProofFieldCarriage({
      lucid: h.proverLucid,
      signer: h.proverSigner,
      planned,
      publisherAddress: h.proverSigner.address,
      label: "maximum mint field",
    }),
  );
  const certified = await capture("field-certificate", "lifecycle", () =>
    certifyFaultProofFieldCarriage({
      lucid: h.proverLucid,
      network,
      signer: h.proverSigner,
      planned,
      certificatePolicyId: h.contracts.fieldPreimageCertificate.policyId,
      certificateMintingScript:
        h.contracts.fieldPreimageCertificate.mintingScript,
      certificateReferenceScriptUtxo: refs.certificate,
      chunkUtxos: field,
      compactCbor: block.nativeTxCompactCbor,
      witnessSetCompactCbor: deriveMidgardNativeTxProofSource(
        subject.nativeTx,
      ).witnessSetCompactCbor.toString("hex"),
    }),
  );
  const policyIndex = BigInt(subject.mintItemCbors.length - 1);
  const preimageHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(
      {
        header: block.header,
        event_to_step_membership: claimEvidence.eventToStepMembership,
        transition_step_membership: claimEvidence.transitionStepMembership,
        policy_index: policyIndex,
        direction: 0n,
      },
      SDK.MintAuthorizationClaimEvidence,
    ),
  );
  const requirement = createStructuredDataPreimageRequirement({ preimageHex });
  const evidenceReferences: UTxO[] = [];
  await capture("claim-publications", "publication", async () => {
    for (const datum of requirement.publicationDatums) {
      const signed = await (
        await h.proverLucid
          .newTx()
          .pay.ToAddressWithData(
            h.proverSigner.address,
            { kind: "inline", value: datum },
            { lovelace: 80_000_000n },
          )
          .complete()
      ).sign
        .withWallet()
        .complete();
      const txHash = await signed.submit();
      await h.proverLucid.awaitTx(txHash);
      const found = (await h.proverLucid.utxosAt(h.proverSigner.address)).find(
        (utxo) => utxo.txHash === txHash && utxo.datum === datum,
      );
      if (found === undefined)
        throw new Error("missing structured claim publication");
      evidenceReferences.push(found);
    }
  });
  const two = await capture("step_02", "lifecycle", () =>
    submitMintAuthorizationStep02({
      ...common,
      threadOutRef: one.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex,
      direction: 0n,
      mintItemCbors: subject.mintItemCbors,
      claimEvidence,
      evidenceReferences,
      publishedCarriageUtxos: field,
      certificateUtxo: certified.certificateUtxo,
      referenceScriptUtxo: refs.steps[1],
    }),
  );
  const three = await capture("step_03", "lifecycle", () =>
    submitMintAuthorizationStep03WitnessAbsence({
      ...common,
      threadOutRef: two.nextThreadOutRef,
      scriptTxWitsItemCbors: [],
      referenceScriptUtxo: refs.steps[2],
    }),
  );
  const four = await capture("step_04", "lifecycle", () =>
    submitMintAuthorizationStep04ResolveNext({
      ...common,
      threadOutRef: three.nextThreadOutRef,
      referenceInputsItemCbors: subject.referenceInputItemCbors,
      trie: {
        rootHex: ledgerProof.transactionsPhasRoot,
        prove: async () => Buffer.from(ledgerProof.proofCbor, "hex"),
      },
      descriptorCborHex: ledger.descriptorCbor,
      referenceScriptUtxo: refs.steps[3],
    }),
  );
  const final = await capture("step_04-complete", "lifecycle", () =>
    submitMintAuthorizationStep04AdvanceComplete({
      ...common,
      threadOutRef: four.nextThreadOutRef,
      referenceInputsItemCbors: subject.referenceInputItemCbors,
      referenceScriptUtxo: refs.steps[3],
    }),
  );
  const proof = await capture("step_05", "lifecycle", () =>
    submitMintAuthorizationStep05({
      ...common,
      threadOutRef: final.nextThreadOutRef,
      referenceScriptUtxo: refs.steps[4],
      witnessReferenceScripts: refs.witnesses,
    }),
  );
  expect(
    await h.proverLucid.utxosAtWithUnit(
      proof.fraudProofAddress,
      proof.fraudProofUnit,
    ),
  ).toHaveLength(1);
  const removal = await capture("publish-removal", "publication", () =>
    publishRemovalReferenceScripts({
      lucid: h.proverLucid,
      contracts: h.contracts,
    }),
  );
  const now = BigInt(h.emulator.now());
  await capture("remove", "lifecycle", () =>
    submitRemoveFraudulentBlock({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
        removalReferenceScripts: removal.published,
      }),
      network,
      signer: h.proverSigner,
      fraudCategory: "mintAuthorization",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    }),
  );
  expect(
    await h.proverLucid.utxosAtWithUnit(
      h.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueBlockUnit,
    ),
  ).toHaveLength(0);
  return measurements;
};
