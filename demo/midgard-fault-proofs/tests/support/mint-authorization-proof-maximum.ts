import { inspect } from "node:util";

import {
  computeHash28,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { planFaultProofFieldOpening } from "../../src/field-opening.js";
import { MINT_AUTHORIZATION_CATEGORY_LABEL } from "../../src/mint-authorization/contracts.js";
import { buildMintAuthorizationStep02Evidence } from "../../src/mint-authorization/evidence.js";
import { buildMintAuthorizationStep01 } from "../../src/mint-authorization/submit-mint-authorization-step-01.js";
import { buildMintAuthorizationStep02 } from "../../src/mint-authorization/submit-mint-authorization-step-02.submit-mint-authorization-step02.js";
import { buildMintAuthorizationStep03WitnessAbsence } from "../../src/mint-authorization/submit-mint-authorization-step-03.submit-mint-authorization-step03-witness-absence.js";
import {
  buildMintAuthorizationStep04AdvanceComplete,
  buildMintAuthorizationStep04ResolveNext,
} from "../../src/mint-authorization/submit-mint-authorization-step-04.js";
import { buildMintAuthorizationStep05 } from "../../src/mint-authorization/submit-mint-authorization-step-05.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { buildResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import { commitCountedRoot } from "../../src/transition-trace/phas.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import { createStructuredDataPreimageRequirement } from "../../src/workflow/raw-datum-preimage.js";
import { network } from "./emulator/blueprints.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { publishFaultProofWitnessReferenceScripts } from "./emulator/reference-scripts.js";
import { publishRemovalReferenceScripts } from "./emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.build-removal-deployment-info.js";
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
  buildAndSubmitUnjournaled,
  certifyCarriageUnjournaled,
  publishCarriageUnjournaled,
  publishProofChunks,
  submitTurnsUnjournaled,
} from "./submit-unjournaled.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

/** Synthetic sibling frontiers measure physical proof ceilings separately from retained-DA workflow cases. */
export const runMintAuthorizationProofMaximum = async (
  selectedMint?: Pick<
    ReturnType<typeof mintAuthorizationMaximumMintField>,
    "mintItemCbors" | "targetPolicyIndex"
  >,
): Promise<void> => {
  const ownOutputs = createOwnOutputJournal();
  const h = await makeMintAuthorizationEmulatorHarness();
  const capture = async <T>(
    stage: string,
    operation: () => Promise<T>,
  ): Promise<T> => {
    const result = await captureEmulatorSubmission(h.emulator, operation).catch(
      (cause: unknown) => {
        throw new Error(`${stage}: ${inspect(cause, { depth: 20 })}`);
      },
    );
    return result.result;
  };
  // The deep ledger opening is a spent input: step-04 closes the empty
  // reference-input field and then resolves it in field 0.
  const spent = referenceInputItemCbor({
    txIdHex: "cd".repeat(32),
    outputIndex: 2,
  });
  const ledger = await buildMintAuthorizationLedgerFixture({
    txIdHex: "cd".repeat(32),
    outputIndex: 2,
  });
  const ledgerProof = syntheticDeepMembershipProof({
    key: Buffer.from(spent, "hex"),
    value: Buffer.from(ledger.descriptorCbor, "hex"),
    branchLevels: 64,
  });
  const mint = selectedMint ?? mintAuthorizationMaximumMintField();
  const subject = buildMintAuthorizationSubject({
    mintItemCbors: mint.mintItemCbors,
    spendInputItemCbors: [spent],
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
  const refs = await capture("publish", async () => {
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
  const chunks = await capture("source-publications", () =>
    publishProofChunks({
      lucid: h.proverLucid,
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
  const init = await capture("init", () =>
    buildAndSubmitUnjournaled(buildResolvedInit, {
      label: MINT_AUTHORIZATION_CATEGORY_LABEL,
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
  const one = await capture("step_01", () =>
    buildAndSubmitUnjournaled(buildMintAuthorizationStep01, {
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
    anchorSourceKind: 0n,
    fieldIndex: SDKFraudProof.MIDGARD_FIELD_INDEX.mint,
    anchorTxId: block.nativeTxId,
    nativeTxCompactCbor: block.nativeTxCompactCbor,
    itemCbors: subject.mintItemCbors.map((item) => Buffer.from(item, "hex")),
    owner: h.proverSigner.paymentKeyHash,
    label: "maximum mint field with maximum proofs",
  });
  const field = await capture("field-publications", () =>
    publishCarriageUnjournaled({
      ownOutputs,
      lucid: h.proverLucid,
      signer: h.proverSigner,
      planned,
      publisherAddress: h.proverSigner.proofMaterialsAddress,
      label: "maximum mint field",
    }),
  );
  const certified = await capture("field-certificate", () =>
    certifyCarriageUnjournaled({
      ownOutputs,
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
      SDKFraudProof.MintAuthorizationClaimEvidence,
    ),
  );
  const requirement = createStructuredDataPreimageRequirement({ preimageHex });
  const evidenceReferences: UTxO[] = [];
  await capture("claim-publications", async () => {
    for (const datum of requirement.publicationDatums) {
      const signed = await (
        await h.proverLucid
          .newTx()
          .pay.ToAddressWithData(
            h.proverSigner.proofMaterialsAddress,
            { kind: "inline", value: datum },
            { lovelace: 80_000_000n },
          )
          .complete()
      ).sign
        .withWallet()
        .complete();
      const txHash = await signed.submit();
      await h.proverLucid.awaitTx(txHash);
      const found = (
        await h.proverLucid.utxosAt(h.proverSigner.proofMaterialsAddress)
      ).find((utxo) => utxo.txHash === txHash && utxo.datum === datum);
      if (found === undefined)
        throw new Error("missing structured claim publication");
      evidenceReferences.push(found);
    }
  });
  const two = await capture("step_02", () =>
    submitTurnsUnjournaled(buildMintAuthorizationStep02, {
      ownOutputs,
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
  const three = await capture("step_03", () =>
    submitTurnsUnjournaled(buildMintAuthorizationStep03WitnessAbsence, {
      ...common,
      threadOutRef: two.nextThreadOutRef,
      scriptTxWitsItemCbors: [],
      referenceScriptUtxo: refs.steps[2],
    }),
  );
  const inputFields = {
    referenceInputsItemCbors: subject.referenceInputItemCbors,
    spendInputsItemCbors: subject.spendInputItemCbors,
  };
  const references = await capture("step_04-complete-references", () =>
    submitTurnsUnjournaled(buildMintAuthorizationStep04AdvanceComplete, {
      ...common,
      ...inputFields,
      threadOutRef: three.nextThreadOutRef,
      referenceScriptUtxo: refs.steps[3],
    }),
  );
  const four = await capture("step_04", () =>
    submitTurnsUnjournaled(buildMintAuthorizationStep04ResolveNext, {
      ...common,
      ...inputFields,
      threadOutRef: references.nextThreadOutRef,
      trie: {
        rootHex: ledgerProof.transactionsPhasRoot,
        prove: async () => Buffer.from(ledgerProof.proofCbor, "hex"),
      },
      descriptorCborHex: ledger.descriptorCbor,
      referenceScriptUtxo: refs.steps[3],
    }),
  );
  const final = await capture("step_04-complete", () =>
    submitTurnsUnjournaled(buildMintAuthorizationStep04AdvanceComplete, {
      ...common,
      ...inputFields,
      threadOutRef: four.nextThreadOutRef,
      referenceScriptUtxo: refs.steps[3],
    }),
  );
  const proof = await capture("step_05", () =>
    buildAndSubmitUnjournaled(buildMintAuthorizationStep05, {
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
  const removal = await capture("publish-removal", () =>
    publishRemovalReferenceScripts({
      lucid: h.proverLucid,
      contracts: h.contracts,
    }),
  );
  const now = BigInt(h.emulator.now());
  await capture("remove", () =>
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
};
