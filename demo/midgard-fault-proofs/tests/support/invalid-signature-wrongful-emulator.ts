import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  admitInvalidSignatureForcedArtifact,
  INVALID_SIGNATURE_FORCED_ARTIFACT,
} from "../../src/invalid-signature/artifact.js";
import type { InvalidSignatureContracts } from "../../src/invalid-signature/contracts.js";
import { submitInvalidSignatureStep01Forced } from "../../src/invalid-signature/submit.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { submitInit } from "../../src/submit-init.js";
import { submitInvalidSignatureStep02 } from "../../src/submit-invalid-signature-step-02.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../../src/transition-trace/phas.js";
import { submitZeroInputCancel } from "../../src/zero-input/submit-cancel.js";
import { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { submitSetupTx } from "./emulator/setup-tx.js";
import {
  buildInvalidSignatureSubject,
  submitRawInvalidSignatureStep02,
} from "./invalid-signature-emulator.js";
import { createMeasuredFitRecorder } from "./measured-fit-ledger.js";
import { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.js";
import { publishPlainReferenceScriptUtxo } from "./submit-init-emulator-shared.js";
import { publishRemovalReferenceScripts } from "./submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

const network = "Custom" as const;

/** Complete registered invalid-signature lifecycle, including raw on-chain
 * mutations of the exact rejected witness coordinate when deepMembership is set. */
export const runInvalidSignatureWrongfulRejectionScenario = async (
  scenario: {
    decoyWitnessCount: number;
    accused: "honest" | "invalid";
    rejectedIndex: bigint | null;
    deepMembership?: boolean;
  },
  measuredFit?: ReturnType<typeof createMeasuredFitRecorder>,
) => {
  const { decoyWitnessCount, accused, rejectedIndex } = scenario;
  const deepMembership = scenario.deepMembership === true;
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realInvalidSignature: true,
    },
  });
  const chain = harness.contracts.fraudProofContracts.invalidSignature;
  const references = [
    harness.faultProofReferenceScripts.fraudProofInvalidSignature!.utxo,
    harness.faultProofReferenceScripts.fraudProofInvalidSignatureStep02!.utxo,
  ] as const;
  const contracts: InvalidSignatureContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle:
        index === 0
          ? "fraud_proofs/invalid_range/step_01.main.spend"
          : "fraud_proofs/invalid_range/step_02.main.spend",
      referenceOutRef: `${references[index].txHash}#${references[index].outputIndex}`,
    })) as unknown as InvalidSignatureContracts["steps"],
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
  };
  const catalogue = await buildCatalogueDeploymentInfo(
    harness.contracts.fraudProofs,
  );
  const category = catalogue.categories.invalidSignature;
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("operator key absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const signedSubject = await buildInvalidSignatureSubject({
    accused,
    decoyWitnessCount,
    spendInputByte: decoyWitnessCount === 317 ? null : "55",
  });
  if (decoyWitnessCount === 317) {
    expect(signedSubject.addrTxWits).toHaveLength(318);
    expect(
      SDK.encodeAddressWitnessPreimage(signedSubject.addrTxWits).length,
    ).toBe(32_757);
    expect(
      SDK.encodeAddressWitnessPreimage([
        ...signedSubject.addrTxWits,
        signedSubject.addrTxWits[0]!,
      ]).length,
    ).toBeGreaterThan(32_768);
  }
  const invalid = materializeMidgardForcedTxFromCanonical(
    signedSubject.nativeTx,
  );
  const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
  const source = deriveMidgardForcedTxProofSource(invalid);
  const leaf = {
    tx_id: transactionId,
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: {
        reason: {
          AddressWitnessSignatureInvalid: {
            witness_index: rejectedIndex ?? signedSubject.badAddrTxWitIndex,
          },
        },
      },
    },
  } as const;
  const key = base.eventKey.ForcedTransactionEventKey.tx_order_id;
  const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(leaf as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  let root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const membershipProof = await trie.prove(keyBytes);
  const deep = deepMembership
    ? syntheticDeepMembershipProof({
        key: keyBytes,
        value: valueBytes,
        branchLevels: 64,
      })
    : null;
  if (deep !== null)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
      root: await commitCountedRoot({
        domain: root.domain,
        count: 1n,
        phasRoot: deep.transactionsPhasRoot,
      }),
    };
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key,
    value: leaf,
    proof: Data.from(
      deep?.proofCbor ?? membershipProof.toCBOR().toString("hex"),
      Proof,
    ),
  };
  const header = {
    ...base.header,
    blockSlot: 10n,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: 1n,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const evidence = {
    subject: forcedVerdictSubject({
      transactionId,
      sourceKey: key,
      rejectionReason: {
        AddressWitnessSignatureInvalid: {
          witness_index: rejectedIndex ?? signedSubject.badAddrTxWitIndex,
        },
      },
    }),
    witnessIndex: rejectedIndex ?? signedSubject.badAddrTxWitIndex,
    witnessSetHash: invalid.compact.transactionWitnessSetHash.toString("hex"),
    witnessSet: signedSubject.witnessSetCompact,
    addressWitnesses: signedSubject.addrTxWits,
    nativeTxCompactCbor: leaf.submitted_source.compact_cbor,
  };
  const artifact = {
    schemaVersion: INVALID_SIGNATURE_FORCED_ARTIFACT,
    headerHash: setup.headerHash,
    forcedSourceCbor: Data.to(
      { header, membership, direction: 1n } as never,
      SDK.InvalidSignatureForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor: encodeMidgardForcedTxCanonical(
      signedSubject.nativeTx,
    ).toString("hex"),
  };
  if (accused === "honest")
    expect(
      (
        await admitInvalidSignatureForcedArtifact(
          JSON.parse(JSON.stringify(artifact)),
        )
      ).evidence,
    ).toEqual(evidence);
  else
    await expect(admitInvalidSignatureForcedArtifact(artifact)).rejects.toThrow(
      /contradiction/,
    );
  await expect(
    admitInvalidSignatureForcedArtifact({
      ...artifact,
      headerHash: "00".repeat(28),
    }),
  ).rejects.toThrow(/header/);
  await expect(
    admitInvalidSignatureForcedArtifact({
      ...artifact,
      fullTransactionCbor: "00",
    }),
  ).rejects.toThrow();
  for (const changedMembership of [
    {
      ...membership,
      key: {
        ...membership.key,
        outputIndex: membership.key.outputIndex + 1n,
      },
    },
    {
      ...membership,
      value: {
        ...membership.value,
        verdict: { ForcedTxInvalid: { reason: "EmptyInputs" as const } },
      },
    },
    {
      ...membership,
      value: {
        ...membership.value,
        verdict: {
          ForcedTxInvalid: {
            reason: {
              AddressWitnessSignatureInvalid: {
                witness_index: evidence.witnessIndex + 1n,
              },
            },
          },
        },
      },
    },
    {
      ...membership,
      value: {
        ...membership.value,
        submitted_source: {
          ...membership.value.submitted_source,
          witness_set_compact_cbor: "00",
        },
      },
    },
  ]) {
    await expect(
      admitInvalidSignatureForcedArtifact({
        ...artifact,
        forcedSourceCbor: Data.to(
          { header, membership: changedMembership, direction: 1n } as never,
          SDK.InvalidSignatureForcedSourcePayloadSchema as never,
        ),
      }),
    ).rejects.toThrow();
  }
  const captures: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] = [];
  const initialize = async () => {
    const c = await captureEmulatorSubmission(harness.emulator, () =>
      submitInit({
        lucid: harness.proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          catalogue,
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "invalidSignature",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    captures.push(c);
    return `${c.result.txHash}#${c.result.firstStepOutputIndex}`;
  };
  const bind = async (threadOutRef: string) => {
    const c = await captureEmulatorSubmission(harness.emulator, () =>
      submitInvalidSignatureStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        forcedSource: { header, membership, direction: 1n },
        referenceScriptUtxo: references[0],
      }),
    );
    captures.push(c);
    return c.result.nextThreadOutRef;
  };
  const cancel = async (threadOutRef: string, step: 0 | 1) =>
    captures.push(
      await captureEmulatorSubmission(harness.emulator, () =>
        submitZeroInputCancel({
          lucid: harness.proverLucid,
          contracts: contracts as never,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: references[step],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      ),
    );
  await cancel(await initialize(), 0);
  await cancel(await bind(await initialize()), 1);
  const initialOutRef = await initialize();
  if (deepMembership) {
    for (const changedMembership of [
      {
        ...membership,
        key: {
          ...membership.key,
          outputIndex: membership.key.outputIndex + 1n,
        },
      },
      {
        ...membership,
        value: {
          ...membership.value,
          verdict: {
            ForcedTxInvalid: {
              reason: {
                AddressWitnessSignatureInvalid: {
                  witness_index: evidence.witnessIndex + 1n,
                },
              },
            },
          },
        },
      },
      {
        ...membership,
        value: {
          ...membership.value,
          submitted_source: {
            ...membership.value.submitted_source,
            witness_set_compact_cbor: "00",
          },
        },
      },
    ])
      await expect(
        submitInvalidSignatureStep01Forced({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: initialOutRef,
          evidence,
          forcedSource: {
            header,
            membership: changedMembership,
            direction: 1n,
          },
          referenceScriptUtxo: references[0],
        }),
      ).rejects.toThrow();
  }
  const terminalOutRef = await bind(initialOutRef);
  if (accused === "invalid") {
    await expect(
      submitRawInvalidSignatureStep02({
        harness,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          catalogue,
        ),
        threadOutRef: terminalOutRef,
        subject: {
          ...signedSubject,
          nativeTxCompactCbor: evidence.nativeTxCompactCbor,
        },
        referenceScriptUtxo: references[1],
      }),
    ).rejects.toThrow();
    await cancel(terminalOutRef, 1);
    return;
  }
  if (decoyWitnessCount === 0) {
    await expect(
      submitRawInvalidSignatureStep02({
        harness,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          catalogue,
        ),
        threadOutRef: terminalOutRef,
        subject: {
          ...signedSubject,
          nativeTxCompactCbor: evidence.nativeTxCompactCbor,
        },
        referenceScriptUtxo: references[1],
        badAddrTxWitIndex: evidence.witnessIndex + 1n,
      }),
    ).rejects.toThrow();
  }
  const opening = planFaultProofFieldOpening({
    anchorSourceKind: 1n,
    fieldIndex: 7,
    anchorTxId: transactionId,
    nativeTxCompactCbor: evidence.nativeTxCompactCbor,
    itemCbors: evidence.addressWitnesses.map(
      SDK.encodeMidgardAddressWitnessCanonical,
    ),
    owner: harness.proverSigner.paymentKeyHash,
    witnessSet: evidence.witnessSet,
    anchorWitnessSetHash: evidence.witnessSetHash,
    label: "invalid signature maximum field",
  });
  const chunksCapture =
    opening.plan.tier === "Inline"
      ? null
      : await captureEmulatorSubmission(harness.emulator, () =>
          publishFaultProofFieldCarriage({
            lucid: harness.proverLucid,
            signer: harness.proverSigner,
            planned: opening,
            publisherAddress: harness.proverSigner.address,
            label: "invalid signature witnesses",
          }),
        );
  if (chunksCapture) captures.push(chunksCapture);
  const chunks = chunksCapture?.result ?? [];
  let certificateUtxos = [] as import("@lucid-evolution/lucid").UTxO[];
  if (opening.plan.tier === "Certified") {
    const reference = await publishPlainReferenceScriptUtxo({
      lucid: harness.proverLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: "invalid signature field certificate",
    });
    const certified = await captureEmulatorSubmission(harness.emulator, () =>
      certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned: opening,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: reference.utxo,
        chunkUtxos: chunks,
        compactCbor: evidence.nativeTxCompactCbor,
        witnessSetCompactCbor: leaf.submitted_source.witness_set_compact_cbor,
      }),
    );
    captures.push(certified);
    certificateUtxos = [certified.result.certificateUtxo];
  }
  const final = await captureEmulatorSubmission(harness.emulator, () =>
    submitInvalidSignatureStep02({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(harness.contracts, catalogue),
      network,
      signer: harness.proverSigner,
      threadOutRef: terminalOutRef,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      witnessSetCompact: evidence.witnessSet,
      addrTxWitsPreimage: evidence.addressWitnesses,
      badAddrTxWitIndex: evidence.witnessIndex,
      certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
      certificateUtxos,
      existingPublicationUtxos: chunks,
      publishMissingCarriage: false,
      referenceScriptUtxo: references[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  captures.push(final);
  expect(final.result.fraudProofUnit).toBeTruthy();
  const removalRefs = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const now = BigInt(harness.emulator.now());
  captures.push(
    await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          catalogue,
          { removalReferenceScripts: removalRefs.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "invalidSignature",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "invalid-signature-lease",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    ),
  );
  captures
    .flatMap((capture) => capture.measurements)
    .forEach((measurement, index) =>
      measuredFit?.record(
        `${decoyWitnessCount}-${rejectedIndex ?? "selected"}-${deepMembership ? "deep64" : "single"}/${index}`,
        measurement,
        measurement.executionMemory === 0n ? "publication" : "lifecycle",
      ),
    );
  for (const measurement of captures.flatMap(
    (capture) => capture.measurements,
  )) {
    expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
    expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
  }
  console.info(
    `[invalid-signature-forced-lifecycle-${decoyWitnessCount}-${rejectedIndex ?? "selected"}-${deepMembership ? "deep64" : "single"}] ${JSON.stringify(captures.flatMap((capture) => capture.measurements).map((measurement) => ({ bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })))}`,
  );
};
