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
  buildFaultProofContracts,
  buildFieldPreimageLengthMismatchFaultProofContracts,
  type CommittedFieldClaim,
  type FieldPreimageLengthMismatchFaultProofContracts,
  type Header,
  L2TransactionSourceSchema,
  parseFaultProofBlueprint,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  faultProofRawFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import type { ManifestBoundFieldPreimageLengthConfig } from "../src/field-preimage-length-mismatch/config.js";
import {
  fieldPreimageLengthCommittedClaim,
  prepareAcceptedFieldPreimageLengthMismatch,
} from "../src/field-preimage-length-mismatch/prepare-accepted.js";
import {
  submitFieldPreimageLengthAcceptedAuthentication,
  submitFieldPreimageLengthAcceptedDispatch,
  submitFieldPreimageLengthCancel,
  submitFieldPreimageLengthForcedAuthentication,
  submitFieldPreimageLengthForcedDispatch,
  submitFieldPreimageLengthInit,
  submitFieldPreimageLengthTerminal,
} from "../src/field-preimage-length-mismatch/submit-lucid.js";
import {
  type PreparedFieldPreimageLengthWorkflow,
  prepareFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/workflow.js";
import { encodeL2TransactionSourceValue } from "../src/prepare-double-spend.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { expectRegisteredChainParity } from "./support/emulator/registered-chain.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildFieldPreimageLengthForcedFixture,
  FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
} from "./support/field-preimage-length-mismatch-forced-fixture.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
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

const measuredFit = createMeasuredFitRecorder(
  "field-preimage-length-mismatch",
  "lifecycle",
  "32,768-byte certified field preimage, inline carriage, both proof directions and cancellation",
);

const REASON = "FieldPreimageLengthMismatch";
const WORKFLOW = "midgard-field-preimage-length-mismatch-workflow-v1" as const;
const coverage = createLifecycleCoverageRecorder();
const fitRows: {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
}[] = [];

const emitFit = (
  stage: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  fitRows.push({ stage, measurement });
  measuredFit.record(
    stage,
    measurement,
    measurement.executionMemory === 0n ? "publication" : "lifecycle",
  );
  expect(measurement.l1ByteMargin).toBeGreaterThan(0);
  expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
  expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
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

/**
 * A refusal raised by local UPLC evaluation of the real applied script, as
 * opposed to one of the family's own pre-construction guards. The submitters
 * prefix every off-chain refusal with the family label; a script failure
 * surfaces from Lucid's evaluator without it.
 */
const expectOnChainRefusal = async (
  attempt: () => Promise<unknown>,
  label: string,
): Promise<void> => {
  let message: string | undefined;
  try {
    await attempt();
  } catch (error) {
    message = error instanceof Error ? error.message : String(error);
  }
  if (message === undefined) {
    throw new Error(
      `${label}: the applied script accepted a mutated transaction`,
    );
  }
  expect(message).not.toMatch(/field-preimage-length-mismatch:/u);
  expect(message).not.toMatch(/forced leaf differs/u);
  expect(message).toMatch(/eval|script|uplc|redeemer|budget|fail/iu);
  console.info(
    `[field-preimage-length-refusal] ${label}: ${message.slice(0, 200)}`,
  );
};

const successorSwappedConfig = (
  config: ManifestBoundFieldPreimageLengthConfig,
): ManifestBoundFieldPreimageLengthConfig => {
  const chain = config.contracts.fieldPreimageLengthMismatch;
  return {
    ...config,
    contracts: {
      ...config.contracts,
      fieldPreimageLengthMismatch: {
        ...chain,
        acceptedStep02: chain.forcedStep02,
      },
    },
  };
};

const flipFirstByte = (bytes: Uint8Array): Buffer => {
  const flipped = Buffer.from(bytes);
  flipped[0] = (flipped[0]! ^ 0x01) & 0xff;
  return flipped;
};

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family SDK builder and the central SDK
 * chain builder are the two application paths that must reproduce it step
 * for step before the suite drives it.
 */
const registeredContracts = async (harness: Harness) => {
  const registered =
    harness.contracts.fraudProofContracts.fieldPreimageLengthMismatch;
  const category = harness.catalogue.categories.fieldPreimageLengthMismatch;
  if (registered === undefined || category === undefined) {
    throw new Error("field-preimage-length deployment is absent");
  }
  const params = () => ({
    eventHistoryBounds: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
    },
    blueprint: parseFaultProofBlueprint(structuredClone(harness.realBlueprint)),
    network,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    fraudProofCataloguePolicyId: harness.contracts.fraudProofCatalogue.policyId,
    referenceScriptAuthPolicyId: harness.contracts.referenceScriptAuth.policyId,
  });
  const family = await Effect.runPromise(
    buildFieldPreimageLengthMismatchFaultProofContracts(params()),
  );
  expectRegisteredChainParity({
    registered,
    applied: family.fieldPreimageLengthMismatch.steps,
    category,
  });
  expect(
    family.fieldPreimageLengthMismatch.acceptedStep02.spendingScriptHash,
  ).toBe(registered.steps[1].spendingScriptHash);
  expect(
    family.fieldPreimageLengthMismatch.forcedStep02.spendingScriptHash,
  ).toBe(registered.steps[2].spendingScriptHash);
  expect(family.fieldPreimageCertificate.policyId).toBe(
    harness.contracts.fieldPreimageCertificate.policyId,
  );
  const central = await Effect.runPromise(buildFaultProofContracts(params()));
  expectRegisteredChainParity({
    registered,
    applied: central.fieldPreimageLengthMismatch.steps,
    category,
  });
  expect(category.categoryId).toBe("00000020");
  return { chain: registered, category };
};

type SetupOptions = Readonly<{
  forced?: boolean;
  acceptedPreimageBytes?: number;
  /** Commit the honest length vector: the accepted block is not at fault. */
  honestAccepted?: boolean;
  /**
   * A forced leaf the family opens straight from the retained root entries:
   * `verdict` is the operator's, and `mismatch` overstates field 0 by one
   * byte in the committed length vector. Reconstruction refuses such a
   * payload, so these leaves never come out of `reconstructDaPayload`.
   */
  forcedLeaf?: {
    readonly verdict: "rejected" | "valid";
    readonly mismatch: boolean;
  };
}>;

const setup = async ({
  forced = false,
  acceptedPreimageBytes,
  honestAccepted = false,
  forcedLeaf,
}: SetupOptions = {}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realFieldPreimageLengthMismatch: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const { chain, category } = await registeredContracts(harness);
  const operator = async () => {
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("missing funder key");
    return {
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
    };
  };
  const forcedFixture = forced
    ? await buildInvalidForcedTransitionTraceFixture({
        ...(await operator()),
        fieldPreimageLengthMismatchIndex: 0,
      })
    : undefined;
  const familyForced =
    forcedLeaf === undefined
      ? undefined
      : await buildFieldPreimageLengthForcedFixture({
          ...(await operator()),
          verdict: forcedLeaf.verdict,
          ...(forcedLeaf.mismatch
            ? {
                lengthsMutation: (lengths: number[]) => [
                  lengths[0]! + 1,
                  ...lengths.slice(1),
                ],
              }
            : {}),
        });
  const forcedHeader = forcedFixture?.header ?? familyForced?.header;
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
  expect(material.fieldIndex).toBe(0);
  const nativeTxId = computeMidgardNativeTxId(material.compact).toString("hex");
  const honestLengths = [
    ...midgardNativeTxProofFieldPreimageLengths({
      body: materialFullTx.body,
      witnessSet: materialFullTx.witnessSet,
    }),
  ];
  const lengths = [...honestLengths];
  if (!honestAccepted) {
    lengths[material.fieldIndex] = lengths[material.fieldIndex]! + 1;
  }
  const proofSource = (fieldLengths: readonly number[]) => ({
    compactCbor: encodeMidgardNativeTxCompact(material.compact),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(materialFullTx.witnessSet),
    ),
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
      ...fieldLengths,
    ]),
  });
  const sourceCbor = encodeL2TransactionSourceValue({
    txId: nativeTxId,
    proofSource: proofSource(lengths),
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
    forcedHeader === undefined
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
          header: forcedHeader,
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
    forcedFixture === undefined &&
    acceptedPreimageBytes === undefined &&
    !honestAccepted
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
    referenceInputsPreimage: Buffer.from(
      materialFullTx.body.referenceInputsPreimageCbor,
    ),
    honestLengths,
    lengths,
    witnessSetCompactCbor: proofSource(lengths).witnessSetCompactCbor,
    /** The same transaction re-keyed under the honest vector: not in the PHAS. */
    substitutedSourceCbor: encodeL2TransactionSourceValue({
      txId: nativeTxId,
      proofSource: proofSource(honestLengths),
    }),
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
    familyForced,
    acceptedPrepared,
    sourceCbor,
    transactionsRoot,
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(materialFullTx),
    fraudulentHeader:
      forcedHeader ??
      (fraudulent as unknown as { readonly header: Header }).header,
  };
};

type Fixture = Awaited<ReturnType<typeof setup>>;

const removeFraudulentBlock = async (
  fixture: Pick<Fixture, "harness" | "fraudulent">,
  { leased = false }: { readonly leased?: boolean } = {},
) => {
  const removalReferences = await publishRemovalReferenceScripts({
    lucid: fixture.harness.proverLucid,
    contracts: fixture.harness.contracts,
  });
  // A registered family resolves removal through the canonical catalogue:
  // the manifest's fraudProofFieldPreimageLengthMismatch entries carry the
  // registered chain the harness folded into the catalogue root.
  return await captureEmulatorSubmission(fixture.harness.emulator, () =>
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
      ...(leased
        ? {
            stateQueueMutationLeaseCoordinator: createRecordingLeaseCoordinator(
              [],
            ),
          }
        : {}),
      validFrom: BigInt(Math.max(0, fixture.harness.emulator.now() - 120_000)),
      validTo: BigInt(fixture.harness.emulator.now() + 300_000),
    }),
  );
};

const forcedPrepared = ({
  headerHash,
  transactionId,
  direction,
  declaredLength,
  preimage,
}: {
  readonly headerHash: string;
  readonly transactionId: string;
  readonly direction: "wrongfulAcceptance" | "wrongfulRejection";
  readonly declaredLength: number;
  readonly preimage: Uint8Array;
}): PreparedFieldPreimageLengthWorkflow => ({
  schemaVersion: WORKFLOW,
  headerHash,
  transactionId,
  direction,
  fieldIndex: FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
  declaredLength,
  actualLength: preimage.length,
  preimageHex: Buffer.from(preimage).toString("hex"),
  carriage: "Inline",
  evidenceDigest: "00".repeat(32),
});

const inlineBodyClaim = (
  fieldIndex: number,
  preimage: Uint8Array,
): CommittedFieldClaim => ({
  BodyFieldClaim: {
    field_index: BigInt(fieldIndex),
    carriage: { Inline: { preimage: Buffer.from(preimage).toString("hex") } },
  },
});

/** `setup` over a retained-root forced leaf, exposing that leaf directly. */
const forcedSetup = async (
  verdict: "rejected" | "valid",
  mismatch: boolean,
) => {
  const fixture = await setup({
    honestAccepted: true,
    forcedLeaf: { verdict, mismatch },
  });
  if (fixture.familyForced === undefined)
    throw new Error("missing family forced leaf");
  return { ...fixture, forced: fixture.familyForced };
};

describe("field-preimage-length-mismatch registered-chain lifecycle", () => {
  it.each([
    [0, "step-01"],
    [1, "step-02-accepted"],
    [3, "step-03"],
  ] as const)(
    "cancels the accepted path from physical step %s",
    async (stepIndex, physicalStep) => {
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
      emitFit(`accepted-cancel-${physicalStep}`, cancel.measurement);
      coverage.cancelled(physicalStep);
    },
    120_000,
  );

  it("executes certified maximum evidence, refuses a substituted chunk order, and refuses the adjacent actual length", async () => {
    const fixture = await setup({ acceptedPreimageBytes: 32_768 });
    const plan = planMidgardFieldCarriage({
      owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
      txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
      fieldIndex: fixture.scenario.fieldIndex,
      preimage: fixture.scenario.committedPreimage,
      publish: false,
    });
    expect(plan.tier).toBe("Certified");
    const planned = {
      sourceKind: 0n,
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
    emitFit("raw-utxo-14337-publication", rawPublication.measurement);
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
        `certified-32768-chunk${(index + 1).toString().padStart(2, "0")}`,
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
    emitFit("accepted-certified-certificate", certificateMint.measurement);
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
    emitFit("accepted-certified-init", init.measurement);
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
    emitFit("accepted-certified-dispatch", dispatch.measurement);
    // The certificate seam: the same certificate and chunks, concatenated in
    // a substituted order, no longer hash to the committed field.
    if (!("Certified" in carriage))
      throw new Error("maximum carriage is not certified");
    const reordered = [...carriage.Certified.chunk_ref_input_indices];
    [reordered[0], reordered[1]] = [reordered[1]!, reordered[0]!];
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: {
            BodyFieldClaim: {
              field_index: BigInt(fixture.scenario.fieldIndex),
              carriage: {
                Certified: {
                  ...carriage.Certified,
                  chunk_ref_input_indices: reordered,
                },
              },
            },
          },
          prepared: direct.prepared,
          carriageReferenceInputs: [certificate.certificateUtxo, ...chunks],
        }),
      "reordered certified chunks",
    );
    coverage.seamMutated("field_certificate");
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
    emitFit("accepted-certified-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("accepted-certified-final-mint", terminal.measurement);
    const removal = await removeFraudulentBlock(fixture);
    expect(removal.result.fraudCategoryId).toBe("00000020");
    emitFit("accepted-certified-remove", removal.measurement);
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("maximum_supported_evidence");
    // The adjacent shape has no admissible carriage: the family refuses it
    // before any transaction exists, and the applied reducer's own bound is
    // exercised by the `authenticated.{..}` Aiken selectors.
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
    expect(() =>
      planMidgardFieldCarriage({
        owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
        txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
        fieldIndex: fixture.scenario.fieldIndex,
        preimage: Buffer.alloc(32_769, 0xa5),
        publish: false,
      }),
    ).toThrow();
    coverage.adjacentOverBoundRefused();
  }, 180_000);

  it("starts at generic Init, refuses every mutated accepted seam on chain, convicts the accepted source, mints proof, and removes the descendant chain", async () => {
    const fixture = await setup();
    if (fixture.scenario.canonicalTx === null) {
      throw new Error("accepted fixture is not canonical");
    }
    if (fixture.acceptedPrepared === undefined)
      throw new Error("missing directly prepared accepted evidence");
    const prepared = fixture.acceptedPrepared;
    const claim = prepared.claim;
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
    const inclusion = parseSubmitStep01TxInclusion(prepared.inclusion);
    // Transaction-membership seam: the same transaction re-keyed under the
    // honest length vector is not the leaf the header's PHAS commits.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion: {
            ...inclusion,
            l2TransactionSourceCbor: fixture.scenario.substitutedSourceCbor,
          },
          claim,
        }),
      "substituted source leaf",
    );
    coverage.seamMutated("tx_membership");
    // Successor seam: step 01 fixes the accepted authenticator's applied hash.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: successorSwappedConfig(fixture.config),
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion,
          claim,
        }),
      "forced authenticator as the accepted successor",
    );
    coverage.seamMutated("successor_script");
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion,
          claim,
        }),
    );
    emitFit("accepted-dispatch", dispatch.measurement);
    // Field-preimage seam: one flipped byte no longer hashes to the field the
    // committed body carries, even though its length would match the claim.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: inlineBodyClaim(
            fixture.scenario.fieldIndex,
            flipFirstByte(fixture.scenario.committedPreimage),
          ),
          prepared: prepared.prepared,
        }),
      "flipped preimage byte",
    );
    coverage.seamMutated("field_preimage");
    // Coordinate mutation: a claim naming field 1 while carrying field 0's
    // bytes opens nothing the body committed at that position.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: inlineBodyClaim(1, fixture.scenario.committedPreimage),
          prepared: { ...prepared.prepared, fieldIndex: 1 },
        }),
      "mutated field coordinate",
    );
    // Subject mutation: the terminal state must carry the subject step 01
    // bound, not one the prover names.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim,
          prepared: { ...prepared.prepared, transactionId: "ff".repeat(32) },
        }),
      "mutated subject transaction id",
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim,
          prepared: prepared.prepared,
        }),
    );
    emitFit("accepted-inline-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("accepted-inline-final-mint", terminal.measurement);
    const removal = await removeFraudulentBlock(fixture, { leased: true });
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removal.result.fraudCategoryId).toBe("00000020");
    emitFit("accepted-inline-remove", removal.measurement);
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 180_000);

  it("refuses an honest accepted block at the terminal after authenticating its field on chain", async () => {
    const fixture = await setup({ honestAccepted: true });
    const preimage = fixture.scenario.committedPreimage;
    const declaredLength =
      fixture.scenario.lengths[fixture.scenario.fieldIndex]!;
    expect(declaredLength).toBe(preimage.length);
    const claim = fieldPreimageLengthCommittedClaim({
      fieldIndex: fixture.scenario.fieldIndex,
      witnessSetCompactCbor: fixture.scenario.witnessSetCompactCbor,
      carriage: { Inline: { preimage: preimage.toString("hex") } },
    });
    // The family's own preparer already refuses to build this evidence.
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        headerHash: fixture.fraudulent.headerHash,
        transactionId: fixture.scenario.nativeTxId,
        direction: "wrongfulAcceptance",
        fieldIndex: fixture.scenario.fieldIndex,
        fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
          fixture.scenario.lengths,
        ),
        fieldPreimage: preimage,
      }),
    ).toThrow(/does not contradict/u);
    const prepared: PreparedFieldPreimageLengthWorkflow = {
      schemaVersion: WORKFLOW,
      headerHash: fixture.fraudulent.headerHash,
      transactionId: fixture.scenario.nativeTxId,
      direction: "wrongfulAcceptance",
      fieldIndex: fixture.scenario.fieldIndex,
      declaredLength,
      actualLength: preimage.length,
      preimageHex: preimage.toString("hex"),
      carriage: "Inline",
      evidenceDigest: "00".repeat(32),
    };
    const init = await submitFieldPreimageLengthInit({
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    });
    const dispatch = await submitFieldPreimageLengthAcceptedDispatch({
      config: fixture.config,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      inclusion: parseSubmitStep01TxInclusion(fixture.scenario.inclusion),
      claim,
    });
    // The authenticator has no opinion on polarity: it binds the honest
    // lengths into terminal state, and the terminal rule refuses to close a
    // wrongful-acceptance thread over equal lengths.
    const authentication =
      await submitFieldPreimageLengthAcceptedAuthentication({
        config: fixture.config,
        threadOutRef: dispatch.nextThreadOutRef,
        claim,
        prepared,
      });
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
        }),
      "honest accepted block at the terminal",
    );
    coverage.scenario("honest_accepted_block_refusal");
    const cancel = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthCancel({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
          stepIndex: 3,
        }),
    );
    emitFit("honest-accepted-cancel-step-03", cancel.measurement);
    coverage.cancelled("step-03");
  }, 120_000);

  it("starts at generic Init, refuses the mutated forced seams on chain, and resolves wrongful forced rejection", async () => {
    const fixture = await setup({ forced: true });
    if (fixture.forcedFixture === undefined)
      throw new Error("missing forced fixture");
    const forcedFixture = fixture.forcedFixture;
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      }),
    );
    emitFit("forced-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          direction: 1n,
        }),
    );
    emitFit("forced-dispatch", dispatch.measurement);
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forcedFixture.reconstruction,
      eventKey: forcedFixture.eventKey,
    });
    const preimage = Buffer.from(
      forcedFixture.forcedNativeTx.body.spendInputsPreimageCbor,
    );
    const referencePreimage = Buffer.from(
      forcedFixture.forcedNativeTx.body.referenceInputsPreimageCbor,
    );
    const prepared = forcedPrepared({
      headerHash: fixture.fraudulent.headerHash,
      transactionId: forcedFixture.forcedTransaction.tx_id,
      direction: "wrongfulRejection",
      declaredLength: preimage.length,
      preimage,
    });
    const claim = inlineBodyClaim(
      FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
      preimage,
    );
    // Forced-leaf seam: a leaf whose committed length vector differs from the
    // one the header's forced-transactions root commits does not open.
    const substitutedLeaf = {
      ...membership,
      value: {
        ...membership.value,
        submitted_source: {
          ...membership.value.submitted_source,
          field_preimage_lengths_cbor: encodeMidgardNativeTxProofFieldLengths([
            preimage.length + 1,
            ...fixture.scenario.honestLengths.slice(1),
          ]).toString("hex"),
        },
      },
    };
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: forcedFixture.header,
          membership: substitutedLeaf,
          claim,
          prepared: { ...prepared, declaredLength: preimage.length + 1 },
        }),
      "substituted forced leaf",
    );
    coverage.seamMutated("forced_leaf");
    // Reason-coordinate mutation: the leaf rejects field 0; authenticating
    // field 1, with field 1's real bytes, is refused by the exact bind.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: forcedFixture.header,
          membership,
          claim: inlineBodyClaim(1, referencePreimage),
          prepared: {
            ...prepared,
            fieldIndex: 1,
            declaredLength: referencePreimage.length,
            actualLength: referencePreimage.length,
            preimageHex: referencePreimage.toString("hex"),
          },
        }),
      "mutated forced reason coordinate",
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: forcedFixture.header,
          membership,
          claim,
          prepared,
        }),
    );
    emitFit("forced-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("forced-final-mint", terminal.measurement);
    const removal = await removeFraudulentBlock(fixture);
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
    expect(removal.result.fraudCategoryId).toBe("00000020");
    emitFit("forced-remove", removal.measurement);
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 180_000);

  it("refuses direction 0 against a rejected leaf before construction and cancels the forced authenticator", async () => {
    const fixture = await setup({ forced: true });
    if (fixture.forcedFixture === undefined)
      throw new Error("missing forced fixture");
    const init = await submitFieldPreimageLengthInit({
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    });
    const dispatch = await submitFieldPreimageLengthForcedDispatch({
      config: fixture.config,
      threadOutRef: init.nextThreadOutRef,
      direction: 0n,
    });
    const honestMembership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: fixture.forcedFixture.reconstruction,
      eventKey: fixture.forcedFixture.eventKey,
    });
    const honestPreimage = Buffer.from(
      fixture.forcedFixture.forcedNativeTx.body.spendInputsPreimageCbor,
    );
    await expect(
      submitFieldPreimageLengthForcedAuthentication({
        config: fixture.config,
        threadOutRef: dispatch.nextThreadOutRef,
        header: fixture.forcedFixture.header,
        membership: honestMembership,
        claim: inlineBodyClaim(
          FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
          honestPreimage,
        ),
        prepared: forcedPrepared({
          headerHash: fixture.fraudulent.headerHash,
          transactionId: fixture.forcedFixture.forcedTransaction.tx_id,
          direction: "wrongfulAcceptance",
          declaredLength: honestPreimage.length,
          preimage: honestPreimage,
        }),
      }),
    ).rejects.toThrow(/forced leaf differs/u);
    const cancel = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthCancel({
          config: fixture.config,
          threadOutRef: dispatch.nextThreadOutRef,
          stepIndex: 2,
        }),
    );
    emitFit("forced-cancel-step-02-forced", cancel.measurement);
    coverage.cancelled("step-02-forced");
  }, 120_000);

  it("refuses an honest forced rejection at the terminal after authenticating the mismatched leaf on chain", async () => {
    // The operator rightly rejected: the leaf's committed vector overstates
    // field 0 by one byte.
    const fixture = await forcedSetup("rejected", true);
    expect(fixture.forced.declaredLength).toBe(
      fixture.forced.preimage.length + 1,
    );
    const init = await submitFieldPreimageLengthInit({
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    });
    const dispatch = await submitFieldPreimageLengthForcedDispatch({
      config: fixture.config,
      threadOutRef: init.nextThreadOutRef,
      direction: 1n,
    });
    const authentication = await submitFieldPreimageLengthForcedAuthentication({
      config: fixture.config,
      threadOutRef: dispatch.nextThreadOutRef,
      header: fixture.forced.header,
      membership: fixture.forced.membership,
      claim: inlineBodyClaim(
        FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
        fixture.forced.preimage,
      ),
      prepared: forcedPrepared({
        headerHash: fixture.fraudulent.headerHash,
        transactionId: fixture.forced.forcedTransaction.tx_id,
        direction: "wrongfulRejection",
        declaredLength: fixture.forced.declaredLength,
        preimage: fixture.forced.preimage,
      }),
    });
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
        }),
      "honest forced rejection at the terminal",
    );
    coverage.scenario("honest_forced_rejection_refusal");
    const cancel = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthCancel({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
          stepIndex: 3,
        }),
    );
    emitFit("honest-forced-cancel-step-03", cancel.measurement);
    coverage.cancelled("step-03");
  }, 120_000);

  it("convicts a wrongfully accepted forced transaction through the forced authenticator", async () => {
    const fixture = await forcedSetup("valid", true);
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      }),
    );
    emitFit("forced-accepted-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          direction: 0n,
        }),
    );
    emitFit("forced-accepted-dispatch", dispatch.measurement);
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: fixture.forced.header,
          membership: fixture.forced.membership,
          claim: inlineBodyClaim(
            FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
            fixture.forced.preimage,
          ),
          prepared: forcedPrepared({
            headerHash: fixture.fraudulent.headerHash,
            transactionId: fixture.forced.forcedTransaction.tx_id,
            direction: "wrongfulAcceptance",
            declaredLength: fixture.forced.declaredLength,
            preimage: fixture.forced.preimage,
          }),
        }),
    );
    emitFit("forced-accepted-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("forced-accepted-final-mint", terminal.measurement);
    const removal = await removeFraudulentBlock(fixture);
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
    emitFit("forced-accepted-remove", removal.measurement);
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
  }, 120_000);

  it("declares the complete lifecycle coverage it exercised", () => {
    console.info(
      `[field-preimage-length-fit-rows] ${JSON.stringify(
        fitRows.map(({ stage, measurement }) => ({
          stage,
          signedBytes: measurement.completeSignedBytes,
          memory: measurement.executionMemory.toString(),
          cpu: measurement.executionSteps.toString(),
        })),
      )}`,
    );
    // Recorded while the suites above ran, never pre-filled. The family has
    // no resumable scan: one bounded whole-field opening authenticates the
    // maximum preimage, so no checkpoint exists to resume from.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [
        "tx_membership",
        "successor_script",
        "field_preimage",
        "field_certificate",
        "forced_leaf",
      ],
      cancellablePhysicalSteps: [
        "step-01",
        "step-02-accepted",
        "step-02-forced",
        "step-03",
      ],
      resumable: false,
      hasAdjacentConsensusBound: true,
    });
  });
});
