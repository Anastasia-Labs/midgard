import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  ForcedInclusionTxV1Schema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriageV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../src/field-opening-v1.js";
import {
  submitInputSetUniquenessCancelV1,
  submitInputSetUniquenessForcedStep01V1,
  submitInputSetUniquenessInit,
  submitInputSetUniquenessStep03V1,
  submitInputSetUniquenessStep04AdvanceV1,
  submitInputSetUniquenessStep04FinalizeV1,
} from "../src/input-set-uniqueness/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInputSetUniquenessFixtureV1,
  makeInputSetUniquenessEmulatorHarnessV1,
  publishInputSetUniquenessReferenceScriptsV1,
} from "./support/input-set-uniqueness-emulator-v1.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  expectProofFitV1,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const exerciseForcedLifecycle = async ({
  referenceCount,
  complete,
}: {
  readonly referenceCount: 400 | 819;
  readonly complete: boolean;
}) => {
  const harness = await makeInputSetUniquenessEmulatorHarnessV1();
  // Field 1 reaches the 32,768-byte aggregate-field bound at 819 items
  // (32,763 bytes, all three certified chunk positions). The one spend item
  // is the minimum needed to bind the cross-field accused coordinates, so
  // this is the largest complete input-set frontier the proof can traverse
  // without weakening either reason-coordinate check.
  const frontier = Array.from({ length: referenceCount + 1 }, (_, index) => ({
    tx_id: index.toString(16).padStart(64, "0"),
    output_index: 0n,
  }));
  const fixture = await buildInputSetUniquenessFixtureV1({
    spendInputs: frontier.slice(0, 1),
    referenceInputs: frontier.slice(1),
    validity: "TxIsInvalid",
  });
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("funder key is absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const sourceKey = base.eventKey.ForcedTransactionEventKey.tx_order_id;
  const forcedTransaction = {
    tx_id: fixture.nativeTxId,
    source: fixture.forcedSource,
    verdict: {
      ForcedTxInvalid: {
        reason: {
          DuplicateInput: {
            first_field_index: 0n,
            first_item_index: 0n,
            second_field_index: 1n,
            second_item_index: 0n,
          },
        },
      },
    },
  } as const;
  const keyBytes = Buffer.from(Data.to(sourceKey, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(forcedTransaction as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const forcedRoot = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const membershipProof = await trie.prove(keyBytes);
  const membership = {
    domain: forcedRoot.domain,
    root: forcedRoot.root,
    phas_root: forcedRoot.phasRoot,
    count: forcedRoot.count,
    key: sourceKey,
    value: forcedTransaction,
    proof: Data.from(membershipProof.toCBOR().toString("hex"), Proof),
  };
  const header = {
    ...base.header,
    forcedTransactionsRoot: forcedRoot.root,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  console.info("[isu-frontier] setup");
  const references = await publishInputSetUniquenessReferenceScriptsV1({
    lucid: harness.funderLucid,
    contracts: harness.family,
  });
  expect(references).toHaveLength(4);
  console.info("[isu-frontier] references");
  const plan = (fieldIndex: number, itemCbors: readonly string[]) =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: fixture.nativeTxId,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      itemCbors: itemCbors.map((item) => Buffer.from(item, "hex")),
      owner: harness.proverSigner.paymentKeyHash,
      label: `input-set-uniqueness frontier field ${fieldIndex.toString()}`,
    });
  const spendPlan = plan(0, fixture.spendInputItemCbors);
  const referencePlan = plan(1, fixture.referenceInputItemCbors);
  expect(spendPlan.plan.tier).toBe("Inline");
  expect(referencePlan.plan.tier).toBe("Certified");
  const spendPublication = {
    result: [] as Awaited<ReturnType<typeof publishFaultProofFieldCarriageV1>>,
    measurements: [] as CompleteSignedTransactionMeasurement[],
  };
  const referencePublication = await captureEmulatorSubmission(
    harness.emulator,
    () =>
      publishFaultProofFieldCarriageV1({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: referencePlan,
        publisherAddress: harness.proverSigner.address,
        label: "input-set-uniqueness frontier reference",
      }),
  );
  console.info("[isu-frontier] carriage");
  expect(spendPublication.result).toHaveLength(0);
  expect(referencePublication.result).toHaveLength(
    referencePlan.plan.publications.length,
  );
  const certificateReferencePublication = await captureEmulatorSubmission(
    harness.emulator,
    () =>
      publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "input-set-uniqueness field certificate mint",
      }),
  );
  const certificateReference = certificateReferencePublication.result.utxo;
  const certify = async (
    planned: typeof spendPlan,
    chunks: typeof spendPublication.result,
  ) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      certifyFaultProofFieldCarriageV1({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: certificateReference,
        chunkUtxos: chunks,
        compactCbor: fixture.nativeTxCompactCbor,
        witnessSetCompactCbor: fixture.forcedSource.witness_set_compact_cbor,
      }),
    );
  const referenceCertificate = await certify(
    referencePlan,
    referencePublication.result,
  );
  console.info("[isu-frontier] certificate");
  const measurements: CompleteSignedTransactionMeasurement[] = [];
  const observedCursors: bigint[] = [];
  const initialize = async () => {
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      submitInputSetUniquenessInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        category: harness.category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    measurements.push(captured.measurement);
    return captured.result.nextThreadOutRef;
  };
  const bind = async (threadOutRef: string) => {
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      submitInputSetUniquenessForcedStep01V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        header,
        membership,
        referenceScriptUtxo: references[0],
      }),
    );
    measurements.push(captured.measurement);
    return captured.result.nextThreadOutRef;
  };
  const seed = async (threadOutRef: string) => {
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      submitInputSetUniquenessStep03V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
        publishedSpendCarriageUtxos: spendPublication.result,
        publishedReferenceCarriageUtxos: referencePublication.result,
        referenceCertificateUtxo: referenceCertificate.result.certificateUtxo,
        referenceScriptUtxo: references[2],
      }),
    );
    measurements.push(captured.measurement);
    return captured.result.nextThreadOutRef;
  };
  const advance = async (threadOutRef: string, cursor: number) => {
    const readingSpend = cursor < fixture.spendInputItemCbors.length;
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      submitInputSetUniquenessStep04AdvanceV1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
        publishedCarriageUtxos: readingSpend
          ? spendPublication.result
          : referencePublication.result,
        certificateUtxo: readingSpend
          ? undefined
          : referenceCertificate.result.certificateUtxo,
        referenceScriptUtxo: references[3],
      }),
    );
    measurements.push(captured.measurement);
    observedCursors.push(captured.result.state.cursor);
    return captured.result.nextThreadOutRef;
  };
  const cancel = async (threadOutRef: string, stepIndex: 0 | 2 | 3) => {
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      submitInputSetUniquenessCancelV1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    measurements.push(captured.measurement);
  };

  if (!complete) {
    await cancel(await initialize(), 0);
    await cancel(await bind(await initialize()), 2);
    const substitutionThread = await bind(await initialize());
    await expect(
      submitInputSetUniquenessStep03V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: substitutionThread,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
        referenceScriptUtxo: references[0],
      }),
    ).rejects.toThrow(/reference script/i);
    await cancel(substitutionThread, 2);
    await cancel(await seed(await bind(await initialize())), 3);
    await cancel(
      await advance(await seed(await bind(await initialize())), 0),
      3,
    );
  }

  const initializedThread = await initialize();
  console.info("[isu-frontier] init");
  const boundThread = await bind(initializedThread);
  console.info("[isu-frontier] bind");
  let threadOutRef = await seed(boundThread);
  console.info("[isu-frontier] seed");
  const visitedOutRefs = new Set([threadOutRef]);
  const expectedCursors: bigint[] = [];
  let cursor = 0;
  const terminalCursor = complete ? frontier.length : 1;
  while (cursor < terminalCursor) {
    console.info(`[isu-frontier] advance-start ${cursor.toString()}`);
    threadOutRef = await advance(threadOutRef, cursor);
    expect(visitedOutRefs.has(threadOutRef)).toBe(false);
    visitedOutRefs.add(threadOutRef);
    cursor = Number(observedCursors.at(-1));
    console.info(`[isu-frontier] advance-end ${cursor.toString()}`);
    expectedCursors.push(BigInt(cursor));
  }
  expect(observedCursors.slice(-expectedCursors.length)).toStrictEqual(
    expectedCursors,
  );
  if (!complete) {
    await cancel(threadOutRef, 3);
    for (const [index, measurement] of measurements.entries()) {
      expectProofFitV1({
        stage: `input-set-uniqueness-maximum-${index.toString()}`,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
    }
    return;
  }
  const finalized = await captureEmulatorSubmission(harness.emulator, () =>
    submitInputSetUniquenessStep04FinalizeV1({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
      referenceScriptUtxo: references[3],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  measurements.push(finalized.measurement);
  expect(finalized.result.fraudProofUnit).toBeTruthy();

  const removalReferences = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const now = BigInt(harness.emulator.now());
  const removal = await captureEmulatorSubmission(harness.emulator, () =>
    submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalReferences.published },
      ),
      network,
      signer: harness.proverSigner,
      fraudCategory: "inputSetUniqueness",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      awaitConfirmation: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "input-set-uniqueness-emulator-lease",
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
  measurements.push(removal.measurement);
  for (const [index, measurement] of measurements.entries()) {
    expectProofFitV1({
      stage: `input-set-uniqueness-${index.toString()}`,
      measurement,
      maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
      maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
    });
  }
  const supportMeasurements = [
    ...spendPublication.measurements,
    ...referencePublication.measurements,
    referenceCertificate.measurement,
    certificateReferencePublication.measurement,
  ];
  for (const [index, measurement] of supportMeasurements.entries()) {
    expectProofFitV1({
      stage: `input-set-uniqueness-carriage-${index.toString()}`,
      measurement,
      maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
      maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
    });
  }
  console.info(
    `[input-set-uniqueness-wrongful-rejection] ${JSON.stringify({
      transactions: measurements.length,
      maxBytes: Math.max(
        ...measurements.map((measurement) =>
          Number(measurement.completeSignedBytes),
        ),
      ),
      maxMemory: measurements
        .reduce(
          (maximum, measurement) =>
            measurement.executionMemory > maximum
              ? measurement.executionMemory
              : maximum,
          0n,
        )
        .toString(),
      maxCpu: measurements
        .reduce(
          (maximum, measurement) =>
            measurement.executionSteps > maximum
              ? measurement.executionSteps
              : maximum,
          0n,
        )
        .toString(),
    })}`,
  );
};

describe("input-set-uniqueness wrongful-rejection lifecycle", () => {
  it("cancels at every forced boundary, restarts, resumes exactly, and rejects reference-script substitution", async () => {
    await exerciseForcedLifecycle({ referenceCount: 400, complete: false });
  }, 600_000);

  it("authenticates the maximum 819-reference field through every batch, permanent mint, and leased removal", async () => {
    await exerciseForcedLifecycle({ referenceCount: 819, complete: true });
  }, 600_000);
});
