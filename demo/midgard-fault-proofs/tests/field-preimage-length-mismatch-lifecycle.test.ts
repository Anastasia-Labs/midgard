import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  midgardNativeTxProofFieldPreimageLengths,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import {
  type FieldPreimageLengthMismatchFaultProofContracts,
  type Header,
  L2TransactionSourceSchema,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  faultProofRawFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import type { ManifestBoundFieldPreimageLengthConfig } from "../src/field-preimage-length-mismatch/config.js";
import { prepareAcceptedFieldPreimageLengthMismatch } from "../src/field-preimage-length-mismatch/prepare-accepted.js";
import {
  submitFieldPreimageLengthAcceptedAuthentication,
  submitFieldPreimageLengthAcceptedDispatch,
  submitFieldPreimageLengthCancel,
  submitFieldPreimageLengthForcedAuthentication,
  submitFieldPreimageLengthForcedDispatch,
  submitFieldPreimageLengthInit,
  submitFieldPreimageLengthTerminal,
} from "../src/field-preimage-length-mismatch/submit-lucid.js";
import { prepareFieldPreimageLengthWorkflow } from "../src/field-preimage-length-mismatch/workflow.js";
import { encodeL2TransactionSourceValue } from "../src/prepare-double-spend.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../src/submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  countedTransactionsRoot,
  createRecordingLeaseCoordinator,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const emitFit = (
  stage: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  console.info(
    `[field-preimage-length-fit] ${JSON.stringify({
      stage,
      signedBytes: measurement.completeSignedBytes,
      byteMargin: measurement.l1ByteMargin,
      memory: measurement.executionMemory.toString(),
      memoryMargin: (16_500_000n - measurement.executionMemory).toString(),
      cpu: measurement.executionSteps.toString(),
      cpuMargin: (10_000_000_000n - measurement.executionSteps).toString(),
    })}`,
  );
};

const setup = async (forced = false, acceptedPreimageBytes?: number) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realFieldPreimageLengthMismatch: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const chain =
    harness.contracts.fraudProofContracts.fieldPreimageLengthMismatch;
  const category = harness.catalogue.categories.fieldPreimageLengthMismatch;
  if (chain === undefined || category === undefined) {
    throw new Error("field-preimage-length deployment is absent");
  }
  const forcedFixture = forced
    ? await (async () => {
        const credential = getAddressDetails(
          await harness.funderLucid.wallet().address(),
        ).paymentCredential;
        if (credential?.type !== "Key") throw new Error("missing funder key");
        return await buildInvalidForcedTransitionTraceFixture({
          operatorVkey: credential.hash,
          now:
            alignUnixTimeToEmulatorSlotBoundary(
              harness.funderLucid,
              harness.emulator.now() + 120_000,
            ) - 1,
          fieldPreimageLengthMismatchIndex: 0,
        });
      })()
    : undefined;
  const baseMaterial = committedFieldShapeScenarioMaterial("honest");
  if (baseMaterial.fullTx === null || baseMaterial.canonicalTx === null)
    throw new Error("missing canonical tx");
  const material =
    acceptedPreimageBytes === undefined
      ? baseMaterial
      : (() => {
          const canonical = {
            ...baseMaterial.canonicalTx,
            body: {
              ...baseMaterial.canonicalTx.body,
              spendInputsPreimageCbor:
                acceptedPreimageBytes === 32_768
                  ? encodeMidgardFieldPreimage([
                      encodeCbor(Buffer.alloc(32_761, 0xa5)),
                    ])
                  : Buffer.alloc(acceptedPreimageBytes, 0xa5),
            },
          };
          const fullTx = materializeMidgardNativeTxFromCanonical(canonical);
          return {
            ...baseMaterial,
            canonicalTx: canonical,
            fullTx,
            compact: fullTx.compact,
            committedPreimage: Buffer.from(fullTx.body.spendInputsPreimageCbor),
          };
        })();
  const materialFullTx = material.fullTx;
  if (materialFullTx === null) throw new Error("missing material full tx");
  const nativeTxId = computeMidgardNativeTxId(material.compact).toString("hex");
  const lengths = [
    ...midgardNativeTxProofFieldPreimageLengths({
      body: materialFullTx.body,
      witnessSet: materialFullTx.witnessSet,
    }),
  ];
  lengths[material.fieldIndex] = lengths[material.fieldIndex]! + 1;
  const sourceCbor = encodeL2TransactionSourceValue({
    txId: nativeTxId,
    proofSource: {
      compactCbor: encodeMidgardNativeTxCompact(material.compact),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(materialFullTx.witnessSet),
      ),
      fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(lengths),
    },
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(sourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const fraudulent =
    forcedFixture === undefined
      ? await setupFraudulentBlock({
          funderLucid: harness.funderLucid,
          emulator: harness.emulator,
          contracts: harness.contracts,
          catalogue: harness.catalogue,
          fixture: {
            transactionsRoot,
            l2TransactionCount: 1n,
            headerDurationMs: 300_000,
          },
        })
      : await submitSetupTx({
          lucid: harness.funderLucid,
          contracts: harness.contracts,
          nonceUtxo: harness.nonceUtxo,
          catalogue: harness.catalogue,
          header: forcedFixture.header,
        });
  const references = [];
  for (const [index, step] of chain.steps.entries()) {
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `field-preimage-length-step-${index.toString()}`,
        })
      ).utxo,
    );
  }
  const acceptedPrepared =
    forcedFixture === undefined && acceptedPreimageBytes === undefined
      ? await prepareAcceptedFieldPreimageLengthMismatch({
          headerHash: fraudulent.headerHash,
          committedTransactionsRoot: await countedTransactionsRoot(
            transactionsRoot,
            1n,
          ),
          l2TransactionCount: 1n,
          entries: [[nativeTxId, sourceCbor]],
          transactionId: nativeTxId,
          canonicalTransactionCbor:
            encodeMidgardNativeTxCanonical(materialFullTx),
          fieldIndex: material.fieldIndex,
        })
      : undefined;
  const scenario = {
    canonicalTx: material.canonicalTx,
    fullTx: material.fullTx,
    nativeTxId,
    fieldIndex: material.fieldIndex,
    committedPreimage: material.committedPreimage,
    inclusion: {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(material.compact),
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        material.compact,
      ).toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    },
  };
  const contracts: FieldPreimageLengthMismatchFaultProofContracts = {
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    fieldPreimageCertificate: harness.contracts.fieldPreimageCertificate,
    fieldPreimageLengthMismatch: {
      ...chain,
      acceptedStep02: chain.steps[1],
      forcedStep02: chain.steps[2],
    },
  };
  const config = {
    schemaVersion:
      "midgard-field-preimage-length-mismatch-production-config-v1",
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    binding: {
      blueprint: harness.realBlueprint,
      network,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      definition: {
        headerHash: fraudulent.headerHash,
        stateQueue: { policyId: harness.contracts.stateQueue.policyId },
      },
      resolvedContracts: {
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        category,
      },
    },
    contracts,
    referenceScripts: {
      step01: references[0],
      step02Accepted: references[1],
      step02Forced: references[2],
      step03: references[3],
      witnesses: harness.witnessReferenceScripts,
    },
  } as unknown as ManifestBoundFieldPreimageLengthConfig;
  return {
    harness,
    config,
    fraudulent,
    scenario,
    forcedFixture,
    acceptedPrepared,
    sourceCbor,
    transactionsRoot,
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(materialFullTx),
    fraudulentHeader:
      forcedFixture === undefined
        ? (fraudulent as unknown as { readonly header: Header }).header
        : forcedFixture.header,
  };
};

describe("field-preimage-length forced dispatch lifecycle", () => {
  it.each([0, 1, 3] as const)(
    "cancels the accepted path from physical step %s",
    async (stepIndex) => {
      const fixture = await setup();
      if (fixture.acceptedPrepared === undefined)
        throw new Error("missing directly prepared accepted evidence");
      const init = await submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      });
      let threadOutRef = init.nextThreadOutRef;
      if (stepIndex >= 1) {
        const dispatch = await submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef,
          stateQueueBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
          inclusion: parseSubmitStep01TxInclusion(
            fixture.acceptedPrepared.inclusion,
          ),
          claim: fixture.acceptedPrepared.claim,
        });
        threadOutRef = dispatch.nextThreadOutRef;
      }
      if (stepIndex === 3) {
        const authentication =
          await submitFieldPreimageLengthAcceptedAuthentication({
            config: fixture.config,
            threadOutRef,
            claim: fixture.acceptedPrepared.claim,
            prepared: fixture.acceptedPrepared.prepared,
          });
        threadOutRef = authentication.nextThreadOutRef;
      }
      const cancel = await captureEmulatorSubmission(
        fixture.harness.emulator,
        () =>
          submitFieldPreimageLengthCancel({
            config: fixture.config,
            threadOutRef,
            stepIndex,
          }),
      );
      emitFit(
        `accepted-step-${stepIndex.toString()}-cancel`,
        cancel.measurement,
      );
      expect(cancel.measurement.l1ByteMargin).toBeGreaterThan(0);
    },
    120_000,
  );

  it("executes certified maximum evidence and refuses the adjacent actual length", async () => {
    const fixture = await setup(false, 32_768);
    const plan = planMidgardFieldCarriage({
      owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
      txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
      fieldIndex: fixture.scenario.fieldIndex,
      preimage: fixture.scenario.committedPreimage,
      publish: false,
    });
    expect(plan.tier).toBe("Certified");
    const planned = {
      fieldIndex: fixture.scenario.fieldIndex,
      nativeTxId: fixture.scenario.nativeTxId,
      nativeTxCompactCbor: fixture.scenario.inclusion.nativeTxCompactCbor,
      preimage: fixture.scenario.committedPreimage,
      itemCount: 0,
      commitment: Buffer.from(plan.commitment).toString("hex"),
      plan,
    } as FaultProofFieldOpeningPlan;
    const rawPlan = planMidgardFieldCarriage({
      owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
      txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
      fieldIndex: fixture.scenario.fieldIndex,
      preimage: Buffer.alloc(14_337, 0xa4),
      publish: false,
    });
    expect(rawPlan.tier).toBe("RawUtxo");
    const rawPublication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        publishFaultProofFieldCarriage({
          lucid: fixture.harness.proverLucid,
          signer: fixture.harness.proverSigner,
          planned: {
            ...planned,
            preimage: Buffer.alloc(14_337, 0xa4),
            commitment: Buffer.from(rawPlan.commitment).toString("hex"),
            plan: rawPlan,
          },
          publisherAddress: fixture.harness.proverSigner.address,
          label: "field-preimage-length raw tier boundary",
        }),
    );
    expect(rawPublication.measurements).toHaveLength(1);
    emitFit("accepted-raw-utxo-14337-publication", rawPublication.measurement);
    const chunkPublication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        publishFaultProofFieldCarriage({
          lucid: fixture.harness.proverLucid,
          signer: fixture.harness.proverSigner,
          planned,
          publisherAddress: fixture.harness.proverSigner.address,
          label: "field-preimage-length maximum",
        }),
    );
    const chunks = chunkPublication.result;
    chunkPublication.measurements.forEach((measurement, index) =>
      emitFit(
        `accepted-certified-32768-chunk-${(index + 1).toString()}`,
        measurement,
      ),
    );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: fixture.harness.proverLucid,
        script: fixture.config.contracts.fieldPreimageCertificate.mintingScript,
        label: "field-preimage-length certificate mint",
      })
    ).utxo;
    const certificateMint = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        certifyFaultProofFieldCarriage({
          lucid: fixture.harness.proverLucid,
          network,
          signer: fixture.harness.proverSigner,
          planned,
          certificatePolicyId:
            fixture.config.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            fixture.config.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference,
          chunkUtxos: chunks,
          compactCbor: fixture.scenario.inclusion.nativeTxCompactCbor,
          witnessSetCompactCbor: (
            Data.from(
              fixture.sourceCbor,
              L2TransactionSourceSchema as never,
            ) as {
              source: { witness_set_compact_cbor: string };
            }
          ).source.witness_set_compact_cbor,
        }),
    );
    const certificate = certificateMint.result;
    emitFit(
      "accepted-certified-32768-certificate",
      certificateMint.measurement,
    );
    const allAuthenticationReferences = [
      fixture.config.referenceScripts.step02Accepted,
      certificate.certificateUtxo,
      ...chunks,
    ];
    const carriage = faultProofRawFieldCarriage({
      plan,
      referenceInputs: allAuthenticationReferences,
      certificatePolicyId:
        fixture.config.contracts.fieldPreimageCertificate.policyId,
      label: "field-preimage-length maximum",
    });
    const direct = await prepareAcceptedFieldPreimageLengthMismatch({
      headerHash: fixture.fraudulent.headerHash,
      committedTransactionsRoot: await countedTransactionsRoot(
        fixture.transactionsRoot,
        1n,
      ),
      l2TransactionCount: 1n,
      entries: [[fixture.scenario.nativeTxId, fixture.sourceCbor]],
      transactionId: fixture.scenario.nativeTxId,
      canonicalTransactionCbor: fixture.canonicalTransactionCbor,
      fieldIndex: fixture.scenario.fieldIndex,
      carriage,
    });
    expect(direct.prepared.actualLength).toBe(32_768);
    expect(direct.prepared.carriage).toBe("Certified");
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      }),
    );
    emitFit("accepted-certified-32768-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
          inclusion: parseSubmitStep01TxInclusion(direct.inclusion),
          claim: direct.claim,
        }),
    );
    emitFit("accepted-certified-32768-dispatch", dispatch.measurement);
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: direct.claim,
          prepared: direct.prepared,
          carriageReferenceInputs: [certificate.certificateUtxo, ...chunks],
        }),
    );
    emitFit(
      "accepted-certified-32768-authenticate",
      authentication.measurement,
    );
    expect(authentication.measurement.l1ByteMargin).toBeGreaterThan(0);
    expect(authentication.measurement.executionMemory).toBeLessThanOrEqual(
      16_500_000n,
    );
    expect(authentication.measurement.executionSteps).toBeLessThanOrEqual(
      10_000_000_000n,
    );
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("accepted-certified-32768-final-mint", terminal.measurement);
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: fixture.harness.proverLucid,
      contracts: fixture.harness.contracts,
    });
    const removal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitRemoveFraudulentBlock({
          lucid: fixture.harness.proverLucid,
          blueprint: fixture.harness.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            fixture.harness.contracts,
            fixture.harness.catalogue,
            { removalReferenceScripts: removalReferences.published },
          ),
          network,
          signer: fixture.harness.proverSigner,
          fraudCategory: "fieldPreimageLengthMismatch",
          fraudulentHeaderHash: fixture.fraudulent.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: BigInt(
            Math.max(0, fixture.harness.emulator.now() - 120_000),
          ),
          validTo: BigInt(fixture.harness.emulator.now() + 300_000),
        }),
    );
    emitFit("accepted-certified-32768-remove", removal.measurement);
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        headerHash: fixture.fraudulent.headerHash,
        transactionId: fixture.scenario.nativeTxId,
        direction: "wrongfulAcceptance",
        fieldIndex: fixture.scenario.fieldIndex,
        fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
          32_768, 0, 0, 0, 0, 0, 0, 0, 0,
        ]),
        fieldPreimage: Buffer.alloc(32_769),
      }),
    ).toThrow(/consensus bound/u);
  }, 120_000);

  it("starts at generic Init, convicts accepted source, mints proof, and removes descendant chain", async () => {
    const fixture = await setup();
    if (fixture.scenario.canonicalTx === null) {
      throw new Error("accepted fixture is not canonical");
    }
    if (fixture.acceptedPrepared === undefined)
      throw new Error("missing directly prepared accepted evidence");
    const claim = fixture.acceptedPrepared.claim;
    const successorValidFrom = Number(
      fixture.fraudulentHeader.endTime - 60_000n,
    );
    const millisecondsToAdvance =
      successorValidFrom - fixture.harness.emulator.now() + 1_000;
    if (millisecondsToAdvance > 0) {
      fixture.harness.emulator.awaitSlot(
        Math.ceil(millisecondsToAdvance / 1_000),
      );
    }
    const successorStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: fixture.fraudulentHeader.endTime,
      emulator: fixture.harness.emulator,
    });
    const successorHeader = {
      ...makeHeader(
        fixture.fraudulentHeader.operatorVkey,
        successorStart,
        await countedTransactionsRoot(fixture.transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: fixture.fraudulent.headerHash,
      prevUtxosRoot: fixture.fraudulentHeader.utxosRoot,
      utxosRoot: fixture.fraudulentHeader.utxosRoot,
    };
    const successor = await submitSuccessorBlockTx({
      lucid: fixture.harness.funderLucid,
      emulator: fixture.harness.emulator,
      contracts: fixture.harness.contracts,
      anchorBlockUnit: fixture.fraudulent.stateQueueBlockUnit,
      header: successorHeader,
      hubOracle: fixture.fraudulent.hubOracle,
      scheduler: fixture.fraudulent.scheduler,
      activeOperatorNode: fixture.fraudulent.activeOperatorNode,
      activeOperatorNodeUnit: fixture.fraudulent.activeOperatorNodeUnit,
    });
    const targetOutRef = successor.continuedAnchorOutRef;
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: targetOutRef,
      }),
    );
    emitFit("accepted-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion: parseSubmitStep01TxInclusion(
            fixture.acceptedPrepared!.inclusion,
          ),
          claim,
        }),
    );
    expect(dispatch.measurement.executionMemory).toBeGreaterThan(0n);
    emitFit("accepted-dispatch", dispatch.measurement);
    expect(dispatch.measurement.l1ByteMargin).toBeGreaterThan(0);
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim,
          prepared: fixture.acceptedPrepared!.prepared,
        }),
    );
    expect(authentication.measurement.executionMemory).toBeGreaterThan(0n);
    emitFit("accepted-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    expect(terminal.measurement.executionMemory).toBeGreaterThan(0n);
    emitFit("accepted-terminal", terminal.measurement);
    expect(terminal.measurement.l1ByteMargin).toBeGreaterThan(0);
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: fixture.harness.proverLucid,
      contracts: fixture.harness.contracts,
    });
    const removal = await submitRemoveFraudulentBlock({
      lucid: fixture.harness.proverLucid,
      blueprint: fixture.harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        fixture.harness.contracts,
        fixture.harness.catalogue,
        { removalReferenceScripts: removalReferences.published },
      ),
      network,
      signer: fixture.harness.proverSigner,
      fraudCategory: "fieldPreimageLengthMismatch",
      fraudulentHeaderHash: fixture.fraudulent.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: createRecordingLeaseCoordinator([]),
      validFrom: BigInt(Math.max(0, fixture.harness.emulator.now() - 120_000)),
      validTo: BigInt(fixture.harness.emulator.now() + 300_000),
    });
    expect(removal.transactions.map(({ kind }) => kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
  }, 120_000);

  it.each([0n, 1n] as const)(
    "starts at generic Init and resolves forced direction %s",
    async (direction) => {
      const fixture = await setup(true);
      const init = await captureEmulatorSubmission(
        fixture.harness.emulator,
        () =>
          submitFieldPreimageLengthInit({
            config: fixture.config,
            fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
          }),
      );
      expect(init.measurement.l1ByteMargin).toBeGreaterThan(0);
      emitFit(`forced-${direction.toString()}-init`, init.measurement);
      const dispatch = await captureEmulatorSubmission(
        fixture.harness.emulator,
        () =>
          submitFieldPreimageLengthForcedDispatch({
            config: fixture.config,
            threadOutRef: init.result.nextThreadOutRef,
            direction,
          }),
      );
      expect(dispatch.measurement.executionMemory).toBeGreaterThan(0n);
      emitFit(`forced-${direction.toString()}-dispatch`, dispatch.measurement);
      expect(dispatch.measurement.executionSteps).toBeGreaterThan(0n);
      expect(dispatch.measurement.l1ByteMargin).toBeGreaterThan(0);
      if (direction === 1n) {
        if (fixture.forcedFixture === undefined)
          throw new Error("missing forced fixture");
        const forcedFixture = fixture.forcedFixture;
        const membership = await buildForcedTransactionLeafMembershipProof({
          reconstruction: forcedFixture.reconstruction,
          eventKey: forcedFixture.eventKey,
        });
        const preimage =
          forcedFixture.forcedNativeTx.body.spendInputsPreimageCbor;
        const authentication = await captureEmulatorSubmission(
          fixture.harness.emulator,
          () =>
            submitFieldPreimageLengthForcedAuthentication({
              config: fixture.config,
              threadOutRef: dispatch.result.nextThreadOutRef,
              header: forcedFixture.header,
              membership,
              claim: {
                BodyFieldClaim: {
                  field_index: 0n,
                  carriage: {
                    Inline: { preimage: Buffer.from(preimage).toString("hex") },
                  },
                },
              },
              prepared: {
                schemaVersion:
                  "midgard-field-preimage-length-mismatch-workflow-v1",
                headerHash: fixture.fraudulent.headerHash,
                transactionId: forcedFixture.forcedTransaction.tx_id,
                direction: "wrongfulRejection",
                fieldIndex: 0,
                declaredLength: preimage.length,
                actualLength: preimage.length,
                preimageHex: Buffer.from(preimage).toString("hex"),
                carriage: "Inline",
                evidenceDigest: "00".repeat(32),
              },
            }),
        );
        expect(authentication.measurement.executionMemory).toBeGreaterThan(0n);
        emitFit("forced-1-authenticate", authentication.measurement);
        const terminal = await captureEmulatorSubmission(
          fixture.harness.emulator,
          () =>
            submitFieldPreimageLengthTerminal({
              config: fixture.config,
              threadOutRef: authentication.result.nextThreadOutRef,
            }),
        );
        expect(terminal.measurement.executionMemory).toBeGreaterThan(0n);
        emitFit("forced-1-terminal", terminal.measurement);
        const removalReferences = await publishRemovalReferenceScripts({
          lucid: fixture.harness.proverLucid,
          contracts: fixture.harness.contracts,
        });
        const removal = await submitRemoveFraudulentBlock({
          lucid: fixture.harness.proverLucid,
          blueprint: fixture.harness.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            fixture.harness.contracts,
            fixture.harness.catalogue,
            { removalReferenceScripts: removalReferences.published },
          ),
          network,
          signer: fixture.harness.proverSigner,
          fraudCategory: "fieldPreimageLengthMismatch",
          fraudulentHeaderHash: fixture.fraudulent.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: BigInt(
            Math.max(0, fixture.harness.emulator.now() - 120_000),
          ),
          validTo: BigInt(fixture.harness.emulator.now() + 300_000),
        });
        expect(removal.transactions.map(({ kind }) => kind)).toEqual([
          "remove-target",
        ]);
        return;
      }
      if (fixture.forcedFixture === undefined)
        throw new Error("missing forced fixture");
      const honestMembership = await buildForcedTransactionLeafMembershipProof({
        reconstruction: fixture.forcedFixture.reconstruction,
        eventKey: fixture.forcedFixture.eventKey,
      });
      const honestPreimage =
        fixture.forcedFixture.forcedNativeTx.body.spendInputsPreimageCbor;
      await expect(
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: fixture.forcedFixture.header,
          membership: honestMembership,
          claim: {
            BodyFieldClaim: {
              field_index: 0n,
              carriage: {
                Inline: {
                  preimage: Buffer.from(honestPreimage).toString("hex"),
                },
              },
            },
          },
          prepared: {
            schemaVersion: "midgard-field-preimage-length-mismatch-workflow-v1",
            headerHash: fixture.fraudulent.headerHash,
            transactionId: fixture.forcedFixture.forcedTransaction.tx_id,
            direction: "wrongfulAcceptance",
            fieldIndex: 0,
            declaredLength: honestPreimage.length,
            actualLength: honestPreimage.length,
            preimageHex: Buffer.from(honestPreimage).toString("hex"),
            carriage: "Inline",
            evidenceDigest: "00".repeat(32),
          },
        }),
      ).rejects.toThrow(/forced leaf differs/u);
      const cancel = await captureEmulatorSubmission(
        fixture.harness.emulator,
        () =>
          submitFieldPreimageLengthCancel({
            config: fixture.config,
            threadOutRef: dispatch.result.nextThreadOutRef,
            stepIndex: 2,
          }),
      );
      expect(cancel.measurement.executionMemory).toBeGreaterThan(0n);
      emitFit("forced-0-cancel", cancel.measurement);
      expect(cancel.measurement.l1ByteMargin).toBeGreaterThan(0);
    },
    120_000,
  );
});
