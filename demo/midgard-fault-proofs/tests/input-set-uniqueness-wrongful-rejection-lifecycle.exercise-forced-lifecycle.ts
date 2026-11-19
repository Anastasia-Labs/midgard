import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  ForcedInclusionTxV1Schema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.plan-fault-proof-field-opening.js";
import { type BoundDuplicateInput } from "../src/input-set-uniqueness/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { expectProofFit } from "./support/emulator/proof-fit.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInputSetUniquenessFixture,
  makeInputSetUniquenessEmulatorHarness,
  publishInputSetUniquenessReferenceScripts,
} from "./support/input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";
import { expectRawScanFinalizeRefused } from "./support/input-set-uniqueness-raw.submit-scan-finalize-raw.js";
import { submitFamilyCancel } from "./support/linear-fault-cancel.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init/forced-transitions.js";
import {
  submitInputSetUniquenessForcedStep01,
  submitInputSetUniquenessInit,
  submitInputSetUniquenessStep03,
  submitInputSetUniquenessStep04Advance,
  submitInputSetUniquenessStep04Finalize,
} from "./support/submit-unjournaled.input-set-uniqueness.js";
import {
  certifyCarriageUnjournaled,
  publishCarriageUnjournaled,
} from "./support/submit-unjournaled.js";

/** The accused pair of a forced `DuplicateInput` rejection. */
export type AccusedInputPair = Omit<BoundDuplicateInput, "subject">;

/** A forced `DuplicateInput` thread over one spend and `referenceCount`
 * reference inputs; `overlap` repeats the spend input first among the
 * references. `finalizeAtSeed` finalizes at cursor 0; `refuseFinalize` checks
 * the on-chain refusal of that finalize built by hand. */
export const exerciseForcedLifecycle = async ({
  referenceCount,
  complete,
  accused = {
    first_field_index: 0n,
    first_item_index: 0n,
    second_field_index: 1n,
    second_item_index: 0n,
  },
  overlap = false,
  finalizeAtSeed = false,
  refuseFinalize,
}: {
  readonly referenceCount: 400 | 819;
  readonly complete: boolean;
  readonly accused?: AccusedInputPair;
  readonly overlap?: boolean;
  readonly finalizeAtSeed?: boolean;
  readonly refuseFinalize?: (
    finalize: () => Promise<unknown>,
  ) => Promise<unknown>;
}) => {
  const ownOutputs = createOwnOutputJournal();
  const harness = await makeInputSetUniquenessEmulatorHarness();
  // Field 1 reaches the 32,768-byte aggregate-field bound at 819 items
  // (32,763 bytes, all three certified chunk positions). The one spend item
  // is the minimum needed to bind the cross-field accused coordinates, so
  // this is the largest complete input-set frontier the proof can traverse
  // without weakening either reason-coordinate check.
  const frontier = Array.from({ length: referenceCount + 1 }, (_, index) => ({
    tx_id: index.toString(16).padStart(64, "0"),
    output_index: 0n,
  }));
  const fixture = await buildInputSetUniquenessFixture({
    spendInputs: frontier.slice(0, 1),
    referenceInputs: overlap
      ? [frontier[0]!, ...frontier.slice(2)]
      : frontier.slice(1),
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
    submitted_source: fixture.forcedSource,
    verdict: {
      ForcedTxInvalid: {
        reason: { DuplicateInput: accused },
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
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: { ...base.header, forcedTransactionsRoot: forcedRoot.root },
  });
  const header = setup.header;
  console.info("[isu-frontier] setup");
  const references = await publishInputSetUniquenessReferenceScripts({
    lucid: harness.funderLucid,
    contracts: harness.family,
  });
  expect(references).toHaveLength(4);
  console.info("[isu-frontier] references");
  const plan = (fieldIndex: number, itemCbors: readonly string[]) =>
    planFaultProofFieldOpening({
      anchorSourceKind: 1n,
      fieldIndex,
      anchorTxId: fixture.nativeTxId,
      nativeTxCompactCbor: fixture.forcedSource.compact_cbor,
      itemCbors: itemCbors.map((item) => Buffer.from(item, "hex")),
      owner: harness.proverSigner.paymentKeyHash,
      label: `input-set-uniqueness frontier field ${fieldIndex.toString()}`,
    });
  const spendPlan = plan(0, fixture.spendInputItemCbors);
  const referencePlan = plan(1, fixture.referenceInputItemCbors);
  expect(spendPlan.plan.tier).toBe("Inline");
  expect(referencePlan.plan.tier).toBe("Certified");
  const spendPublication = {
    result: [] as Awaited<ReturnType<typeof publishCarriageUnjournaled>>,
    measurements: [] as CompleteSignedTransactionMeasurement[],
  };
  const referencePublication = await captureEmulatorSubmission(
    harness.emulator,
    () =>
      publishCarriageUnjournaled({
        ownOutputs,
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: referencePlan,
        publisherAddress: harness.proverSigner.proofMaterialsAddress,
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
      certifyCarriageUnjournaled({
        ownOutputs,
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
        compactCbor: fixture.forcedSource.compact_cbor,
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
      submitInputSetUniquenessForcedStep01({
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
      submitInputSetUniquenessStep03({
        ownOutputs,
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        nativeTxCompactCbor: fixture.forcedSource.compact_cbor,
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
      submitInputSetUniquenessStep04Advance({
        ownOutputs,
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        nativeTxCompactCbor: fixture.forcedSource.compact_cbor,
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
      submitFamilyCancel("input-set-uniqueness", {
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
      submitInputSetUniquenessStep03({
        ownOutputs,
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: substitutionThread,
        nativeTxCompactCbor: fixture.forcedSource.compact_cbor,
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
  const terminalCursor = finalizeAtSeed ? 0 : complete ? frontier.length : 1;
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
      expectProofFit({
        stage: `input-set-uniqueness-maximum-${index.toString()}`,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
    }
    return measurements;
  }
  if (refuseFinalize !== undefined) {
    await expectRawScanFinalizeRefused({
      harness,
      threadOutRef,
      referenceScriptUtxo: references[3],
      refuse: refuseFinalize,
    });
    return measurements;
  }
  const finalized = await captureEmulatorSubmission(harness.emulator, () =>
    submitInputSetUniquenessStep04Finalize({
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
    expectProofFit({
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
    expectProofFit({
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
  return measurements;
};
