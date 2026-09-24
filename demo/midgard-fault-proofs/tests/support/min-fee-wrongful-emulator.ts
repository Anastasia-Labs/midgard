import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  getAddressDetails,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { submitLinearFaultCancel } from "../../src/linear-fault-cancel.js";
import { MIN_FEE_CATEGORY_LABEL } from "../../src/min-fee-contracts.js";
import {
  MIN_FEE_FORCED_ARTIFACT,
  prepareMinFeeForcedArtifact,
} from "../../src/min-fee-forced-artifact.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { submitMinFeeStep01Forced } from "../../src/submit-min-fee-forced-step-01.js";
import { submitMinFeeInit } from "../../src/submit-min-fee-init.js";
import {
  type MinFeeFieldItemCbors,
  submitMinFeeStep02,
} from "../../src/submit-min-fee-step-02.js";
import { forcedTxFromCoreCompact } from "../../src/submit-step-01.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../../src/transition-trace/reconstruct.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

export const makeMinFeeWrongfulRejectionScenario = async (
  minimum: bigint,
  inputCount = 1,
  allFields = false,
  depth = 0,
  scaledMinimum = false,
  claimedVerdict: SDK.OperatorVerdict = {
    ForcedTxInvalid: { reason: "FeeBelowMinimum" },
  },
) => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: { realMinFee: true },
  });
  const contracts = h.contracts.minFee!;
  const category = h.catalogue.categories.minFee!;
  expect(category.scriptHash).toBe(contracts.steps[0].spendingScriptHash);
  const credential = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const fixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1,
  });
  const inputItems = Array.from({ length: inputCount }, (_, i) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.from("77".repeat(32), "hex"),
      outputIndex: i,
    }),
  );
  const base = makeNativeTx({ spendInputCbors: inputItems, fee: 1_000n });
  const items = (count: number, width: number, seed: number) =>
    encodeMidgardFieldPreimage(
      Array.from({ length: count }, (_, i) =>
        Buffer.alloc(width, (i + seed) % 256),
      ),
    );
  const submitted = allFields
    ? materializeMidgardNativeTxFromCanonical({
        ...base,
        body: {
          ...base.body,
          referenceInputsPreimageCbor: encodeMidgardFieldPreimage(inputItems),
          outputsPreimageCbor: items(100, 32, 1),
          requiredObserversPreimageCbor: items(110, 28, 2),
          requiredSignersPreimageCbor: items(110, 28, 3),
          mintPreimageCbor: items(100, 32, 4),
        },
        witnessSet: {
          ...base.witnessSet,
          scriptTxWitsPreimageCbor: items(100, 32, 5),
          addrTxWitsPreimageCbor: items(33, 101, 6),
          redeemerTxWitsPreimageCbor: items(100, 32, 7),
        },
      })
    : base;
  const tx = materializeMidgardForcedTxFromCanonical(submitted);
  const transactionId = computeMidgardNativeTxId(tx).toString("hex");
  const proofSource = deriveMidgardForcedTxProofSource(tx);
  const sourceKey = fixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const leaf: SDK.ForcedInclusionTxV1 = {
    tx_id: transactionId,
    submitted_source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: claimedVerdict,
  };
  const key = Buffer.from(Data.to(sourceKey, SDK.OutputReference), "hex");
  const value = Buffer.from(
    Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  let root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key, value },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(key, value);
  const proof = await trie.prove(key);
  const deep =
    depth > 0
      ? syntheticDeepMembershipProof({ key, value, branchLevels: depth })
      : null;
  if (deep !== null)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
      count: 1n,
      root: await commitCountedRoot({
        domain: root.domain,
        phasRoot: deep.transactionsPhasRoot,
        count: 1n,
      }),
    };
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key: sourceKey,
    value: leaf,
    proof: Data.from(
      deep?.proofCbor ?? proof.toCBOR().toString("hex"),
      SDK.Proof,
    ),
  };
  const minFeeA = scaledMinimum ? 1n : 0n;
  const minFeeB = scaledMinimum
    ? 1_000n -
      SDK.minimumFeeFromProofSource({
        sourceKind: "forced",
        source: proofSource,
        minFeeA: 1n,
        minFeeB: 0n,
      }).canonicalTxSize
    : minimum;
  const header = {
    ...fixture.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
    minFeeA,
    minFeeB,
  };
  const seeded = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  const forcedSource = { header: header, membership, direction: 1n };
  const state: SDK.MinFeeStep02State = {
    subject: SDK.forcedVerdictSubject({
      transactionId,
      sourceKey,
      rejectionReason: "FeeBelowMinimum",
    }),
    bad_tx: forcedTxFromCoreCompact(tx.compact),
    bad_tx_body_fee: 1_000n,
    bad_tx_id: transactionId,
    min_fee_a: minFeeA,
    min_fee_b: minFeeB,
  };
  const witness = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  const witnessSet = {
    addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
  };
  const fields = [
    tx.body.spendInputsPreimageCbor,
    tx.body.referenceInputsPreimageCbor,
    tx.body.outputsPreimageCbor,
    tx.body.requiredObserversPreimageCbor,
    tx.body.requiredSignersPreimageCbor,
    tx.body.mintPreimageCbor,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    tx.witnessSet.addrTxWitsPreimageCbor,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  ].map((field) =>
    decodeMidgardFieldPreimage(field),
  ) as unknown as MinFeeFieldItemCbors;
  const refs: UTxO[] = [];
  const scriptPublications = [];
  for (const step of contracts.steps) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: step.spendingScript,
      label: "min-fee forced step",
    });
    refs.push(publication.utxo);
    scriptPublications.push(publication.publicationMeasurement);
  }
  const init = () =>
    submitMinFeeInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      network,
      contracts,
      category,
      catalogue: {
        policyId: h.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          h.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: h.catalogue.root,
      },
      signer: h.proverSigner,
      fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const bind = (threadOutRef: string, overrides = {}) =>
    submitMinFeeStep01Forced({
      lucid: h.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: h.proverSigner,
      threadOutRef,
      state,
      forcedSource,
      referenceScriptUtxo: refs[0]!,
      ...overrides,
    });
  const certificates: UTxO[] = [];
  const chunks: UTxO[] = [];
  const prepareCarriages = async () => {
    for (const [fieldIndex, items] of fields.entries()) {
      const planned = planFaultProofFieldOpening({
        anchorSourceKind: 1n,
        fieldIndex,
        anchorTxId: transactionId,
        nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
        itemCbors: items,
        owner: h.proverSigner.paymentKeyHash,
        publish: true,
        ...(fieldIndex < 6
          ? {}
          : {
              witnessSet,
              anchorWitnessSetHash: state.bad_tx.witness_set_hash,
            }),
        label: "min-fee maximum field",
      });
      const published = await publishFaultProofFieldCarriage({
        lucid: h.proverLucid,
        signer: h.proverSigner,
        planned,
        publisherAddress: h.proverSigner.address,
        label: "min-fee maximum field",
      });
      chunks.push(...published);
      if (planned.plan.tier === "Certified") {
        const reference = await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: h.contracts.fieldPreimageCertificate.mintingScript,
          label: "min-fee field certificate",
        });
        const certificate = await certifyFaultProofFieldCarriage({
          lucid: h.proverLucid,
          network,
          signer: h.proverSigner,
          planned,
          certificatePolicyId: h.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            h.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: reference.utxo,
          chunkUtxos: published,
          compactCbor: proofSource.compactCbor.toString("hex"),
          witnessSetCompactCbor:
            proofSource.witnessSetCompactCbor.toString("hex"),
        });
        certificates.push(certificate.certificateUtxo);
      }
    }
  };
  const finish = (
    threadOutRef: string,
    unsafeSkipLocalViolationCheckForTest = false,
  ) =>
    submitMinFeeStep02({
      lucid: h.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: h.proverSigner,
      threadOutRef,
      nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
      witnessSet,
      fieldItemCbors: fields,
      certificateUtxos: certificates,
      existingPublicationUtxos: chunks,
      publishCarriages: chunks.length > 0,
      publishMissingCarriages: chunks.length === 0,
      referenceScriptUtxo: refs[1]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      unsafeSkipLocalViolationCheckForTest,
    });
  const cancel = (threadOutRef: string, step: number) =>
    submitLinearFaultCancel({
      lucid: h.proverLucid,
      family: MIN_FEE_CATEGORY_LABEL,
      steps: contracts.steps,
      computationThread: contracts.computationThread,
      categoryId: category.categoryId,
      signer: h.proverSigner,
      threadOutRef,
      referenceScriptUtxo: refs[step]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const remove = async () => {
    const refs = await captureEmulatorSubmission(h.emulator, () =>
      publishRemovalReferenceScripts({
        lucid: h.proverLucid,
        contracts: h.contracts,
      }),
    );
    const now = BigInt(h.emulator.now());
    const removal = await captureEmulatorSubmission(h.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
          removalReferenceScripts: refs.result.published,
        }),
        network,
        signer: h.proverSigner,
        fraudCategory: "minFee",
        fraudulentHeaderHash: seeded.headerHash,
        requireReferenceScripts: true,
        validFrom: now - 120_000n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.transactions).toHaveLength(1);
    expect(removal.result.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );
    const removalBody = CML.Transaction.from_cbor_hex(
      removal.transactionCbors.at(-1)!,
    ).body();
    expect(removalBody.fee()).toBe(500_000_000n);
    const outputs = removalBody.outputs();
    let reward = 0n;
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = outputs.get(index);
      if (
        getAddressDetails(output.address().to_bech32()).paymentCredential
          ?.hash === h.proverSigner.paymentKeyHash
      )
        reward += output.amount().coin();
    }
    expect(reward).toBe(400_000_000n);
    expect(
      await h.proverLucid.utxosAtWithUnit(
        h.contracts.activeOperators.spendingScriptAddress,
        seeded.activeOperatorNodeUnit,
      ),
    ).toHaveLength(0);
    expect(
      await h.proverLucid.utxosAtWithUnit(
        h.contracts.stateQueue.spendingScriptAddress,
        seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(0);
    return [...refs.measurements, ...removal.measurements];
  };
  const artifact = {
    schemaVersion: MIN_FEE_FORCED_ARTIFACT,
    headerHash: seeded.headerHash,
    detectionId: `min-fee:forced:0:${transactionId}`,
    forcedIndex: 0,
    forcedSourceCbor: Data.to(
      forcedSource as never,
      SDK.MinFeeForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor: encodeMidgardForcedTxCanonical(tx).toString("hex"),
  };
  const prepareArtifact = () => {
    const entry = {
      key: sourceKey,
      value: leaf,
      keyBytes: key,
      valueBytes: value,
      fullTransactionCbor: encodeMidgardForcedTxCanonical(tx),
    };
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: sourceKey } };
    const fingerprint = eventKeyFingerprint(eventKey);
    const block = {
      headerHash: seeded.headerHash,
      header,
      reconstruction: {
        forcedTransactions: [entry],
        rootData: { forcedTransactions: root },
        sourceEventsByFingerprint: new Map([
          [
            fingerprint,
            { phase: "ForcedTransaction", eventKey, fingerprint, entry },
          ],
        ]),
      },
    } as unknown as CanonicalBlockEvidence;
    return prepareMinFeeForcedArtifact({
      block,
      detectionId: artifact.detectionId,
    });
  };
  return {
    h,
    tx,
    leaf,
    contracts,
    category,
    seeded,
    forcedSource,
    state,
    refs,
    init,
    bind,
    finish,
    cancel,
    prepareCarriages,
    scriptPublications,
    remove,
    artifact,
    prepareArtifact,
  };
};
