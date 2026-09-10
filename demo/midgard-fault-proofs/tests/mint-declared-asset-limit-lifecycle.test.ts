import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardMintPolicyItem,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  fieldOpeningForField,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  hashBlockHeader,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { requireLinearFaultReferenceScript } from "../src/linear-fault-family.js";
import { createMintDeclaredAssetLimitActuator } from "../src/mint-declared-asset-limit/actuator.js";
import { buildMintDeclaredAssetLimitArtifact } from "../src/mint-declared-asset-limit/artifact.js";
import {
  applyMintDeclaredAssetLimitScripts,
  MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
  type MintDeclaredAssetLimitContracts,
} from "../src/mint-declared-asset-limit/contracts.js";
import {
  MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
  MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS,
  type MintDeclaredAssetLimitEvidence,
  prepareMintDeclaredAssetLimitEvidence,
} from "../src/mint-declared-asset-limit/family.js";
import { MintDeclaredAssetLimitStep03DatumSchema } from "../src/mint-declared-asset-limit/schemas.js";
import {
  advanceMintDeclaredFoldSnapshot,
  encodeMintDeclaredGrammarCheckpoint,
  encodeMintDeclaredWalkCheckpoint,
  hashMintDeclaredGrammarCheckpoint,
  hashMintDeclaredWalkCheckpoint,
  initialMintDeclaredFoldSnapshot,
  planMintDeclaredAssetLimitField,
  planMintDeclaredAssetLimitStagedWalk,
} from "../src/mint-declared-asset-limit/staged-plan.js";
import { submitMintDeclaredAssetLimitCancel } from "../src/mint-declared-asset-limit/submit-cancel.js";
import {
  submitMintDeclaredAssetLimitStep01Accepted,
  submitMintDeclaredAssetLimitStep01Forced,
  submitMintDeclaredAssetLimitStep01ForcedRaw,
} from "../src/mint-declared-asset-limit/submit-step-01.js";
import {
  mintDeclaredStep02WirePlan,
  planMintDeclaredFieldOpening,
  resolveMintDeclaredFieldCarriage,
  submitMintDeclaredAssetLimitStep02,
  submitMintDeclaredAssetLimitStep02Raw,
} from "../src/mint-declared-asset-limit/submit-step-02.js";
import {
  mintDeclaredFoldSnapshotData,
  submitMintDeclaredAssetLimitStep03,
  submitMintDeclaredAssetLimitStep03Raw,
} from "../src/mint-declared-asset-limit/submit-step-03.js";
import {
  submitMintDeclaredAssetLimitStep04,
  submitMintDeclaredAssetLimitStep04Raw,
} from "../src/mint-declared-asset-limit/submit-step-04.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
} from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const firstStepDeploymentEntry = "fraudProofMintDeclaredAssetLimit";
const REASON_ARM = "MintDeclaredAssetLimit";
const coverage = createLifecycleCoverageRecorder();

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type Capture = Awaited<ReturnType<typeof captureEmulatorSubmission>>;
type Measurement = Capture["measurement"];

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
    harness.contracts.fraudProofContracts.mintDeclaredAssetLimit;
  const category = harness.catalogue.categories.mintDeclaredAssetLimit;
  expectRegisteredChainParity({
    registered,
    applied: applyMintDeclaredAssetLimitScripts({
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
  const applied = familyStepsFromRegisteredChain(
    registered.steps,
    MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
  );
  const contracts: MintDeclaredAssetLimitContracts = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  // Reference scripts are published only after the block under dispute is
  // set up: the funder's publications must not consume the harness nonce.
  return {
    applied,
    contracts,
    catalogue: harness.catalogue,
    category,
    references: undefined as unknown as readonly [UTxO, UTxO, UTxO, UTxO],
    certificateReference: undefined as unknown as UTxO,
  };
};
type Registered = Awaited<ReturnType<typeof registeredContracts>>;

/** Publishes the four applied steps and the certificate mint by reference. */
const publishFamilyReferences = async (
  harness: Harness,
  registered: Registered,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of registered.applied.entries())
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `mint-declared-lifecycle-${index.toString()}`,
        })
      ).utxo,
    );
  registered.references = references as unknown as readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
  registered.certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: "mint-declared-lifecycle-certificate",
    })
  ).utxo;
  return registered.references;
};

const policy = (byte: number) => Buffer.alloc(28, byte);

const singleton = (byte: number) =>
  encodeMidgardMintPolicyItem({
    policyId: policy(byte),
    assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
  });

/** `count` canonical two-byte asset names in ascending order. */
const wide = (byte: number, count: number) =>
  encodeMidgardMintPolicyItem({
    policyId: policy(byte),
    assets: Array.from({ length: count }, (_, index) => ({
      assetName: Buffer.from([index >> 8, index & 255]),
      quantity: 1n,
    })),
  });

/**
 * A policy item whose canonical map header declares `count` (>= 256) assets
 * over `padding` body bytes the machine never reads when the header decides.
 */
const declaring = (byte: number, count: number, padding: number) =>
  Buffer.concat([
    Buffer.from([0x82, 0x58, 0x1c]),
    policy(byte),
    Buffer.from([0xb9, count >> 8, count & 255]),
    Buffer.alloc(Math.max(1, padding), 0),
  ]);

/** Pads the declaring target so the field-5 preimage is exactly `total` bytes. */
const fieldOfExactly = (
  prefix: readonly Buffer[],
  targetByte: number,
  declaredCount: number,
  total: number,
) => {
  let padding = 1;
  let field = encodeMidgardFieldPreimage([
    ...prefix,
    declaring(targetByte, declaredCount, padding),
  ]);
  for (let attempt = 0; attempt < 3 && field.length !== total; attempt += 1) {
    padding += total - field.length;
    field = encodeMidgardFieldPreimage([
      ...prefix,
      declaring(targetByte, declaredCount, padding),
    ]);
  }
  expect(field).toHaveLength(total);
  return field;
};

const mintTx = (mintField: Buffer, fee: bigint) => {
  const base = makeNativeTx({ spendInputCbors: [], fee });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: { ...base.body, mintPreimageCbor: mintField },
    witnessSet: base.witnessSet,
  });
  return {
    nativeTx,
    id: computeMidgardNativeTxId(nativeTx).toString("hex"),
    compactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex"),
    sourceCbor: l2TransactionSourceCborV1(nativeTx),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
    ).toString("hex"),
    mintField,
    commitmentHex: midgardFieldCommitment(mintField).toString("hex"),
  };
};
type MintTx = ReturnType<typeof mintTx>;

/** One accepted block over several L2 transactions, with a proof for each. */
const acceptedBlock = async (txs: readonly MintTx[]) => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const tx of txs)
    await trie.insert(
      Buffer.from(tx.id, "hex"),
      Buffer.from(tx.sourceCbor, "hex"),
    );
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusions = [];
  for (const tx of txs) {
    const proof = await trie.prove(Buffer.from(tx.id, "hex"));
    const proofCbor = proof.toCBOR().toString("hex");
    inclusions.push({
      nativeTxId: tx.id,
      nativeTx: nativeTxFromCoreCompact(tx.nativeTx.compact),
      nativeTxCompactCbor: tx.compactCbor,
      l2TransactionSourceCbor: tx.sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proofCbor, Proof),
      txMembershipProofCbor: proofCbor,
    });
  }
  return { transactionsRoot, inclusions };
};

const acceptedEvidence = (tx: MintTx, policyIndex: number) =>
  prepareMintDeclaredAssetLimitEvidence({
    finding: { subject: acceptedVerdictSubject(tx.id), policyIndex },
    fieldPreimage: tx.mintField,
    committedFieldHashHex: tx.commitmentHex,
  });

/** Publishes a field's carriage (and, when certified, its certificate). */
const publishCarriage = async (
  harness: Harness,
  registered: Registered,
  tx: Pick<MintTx, "id" | "compactCbor"> &
    Partial<Pick<MintTx, "witnessSetCompactCbor">>,
  items: readonly Buffer[],
  label: string,
) => {
  const planned = planFaultProofFieldOpening({
    fieldIndex: 5,
    anchorTxId: tx.id,
    nativeTxCompactCbor: tx.compactCbor,
    itemCbors: items,
    owner: harness.proverSigner.paymentKeyHash,
    publish: true,
    label,
  });
  const carriage = await captureEmulatorSubmission(harness.emulator, () =>
    publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label,
    }),
  );
  const certificate =
    planned.plan.tier === "Certified"
      ? await captureEmulatorSubmission(harness.emulator, () =>
          certifyFaultProofFieldCarriage({
            lucid: harness.proverLucid,
            network,
            signer: harness.proverSigner,
            planned,
            certificatePolicyId:
              harness.contracts.fieldPreimageCertificate.policyId,
            certificateMintingScript:
              harness.contracts.fieldPreimageCertificate.mintingScript,
            certificateReferenceScriptUtxo: registered.certificateReference,
            chunkUtxos: carriage.result,
            compactCbor: tx.compactCbor,
            witnessSetCompactCbor: tx.witnessSetCompactCbor!,
          }),
        )
      : undefined;
  return { planned, carriage, certificate };
};

const progress = (message: string) => {
  if (process.env.MIDGARD_PRINT_FIT === "1")
    console.info(`[mint-declared-lifecycle] ${message}`);
};

const measuredFit = createMeasuredFitRecorder(
  "mint-declared-asset-limit",
  "lifecycle",
  "62 policies with 1000-asset first policy, exact 32768-byte certified field and 192-unit fold; both directions",
);

const printLedger = (
  label: string,
  rows: readonly (readonly [string, Measurement])[],
) => {
  rows.forEach(([name, measurement], index) =>
    measuredFit.record(
      `${label}/${index}-${name}`,
      measurement,
      measurement.executionMemory === 0n ? "publication" : "lifecycle",
    ),
  );
  if (process.env.MIDGARD_PRINT_FIT === "1")
    console.info(
      `[${label}] ${JSON.stringify(rows, (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      )}`,
    );
};

const expectPositiveMargins = (
  rows: readonly (readonly [string, Measurement])[],
  publicationOnly: readonly string[] = [],
) => {
  for (const [label, measurement] of rows) {
    expect(measurement.l1ByteMargin, label).toBeGreaterThan(0);
    if (!publicationOnly.includes(label)) {
      expect(measurement.executionMemory, label).toBeGreaterThan(0n);
      expect(measurement.executionSteps, label).toBeGreaterThan(0n);
    }
  }
};

describe("mintDeclaredAssetLimit registered-chain lifecycle", () => {
  it("proves the maximum accepted crossing, refuses every honest and substituted accepted shape, and removes the block", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realMintDeclaredAssetLimit: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { applied, contracts, catalogue, category } = registered;
    const rows: (readonly [string, Measurement])[] = [];
    const measured = async <Result>(
      label: string,
      action: () => Promise<Result>,
    ): Promise<Result> => {
      progress(label);
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        action,
      );
      rows.push([label, captured.measurement]);
      return captured.result;
    };
    const refused = async (label: string, action: () => Promise<unknown>) => {
      progress(`refusal: ${label}`);
      await expectOnchainRefusal(action);
    };

    // --- The three accepted transactions of one fraudulent block ---------
    // X: the maximum frontier. A 1,000-asset first policy (consumed across
    // several fold transactions), 60 singleton policies, then a target whose
    // header declares exactly one asset past the bound: the smallest crossing.
    const widePrefix = [
      wide(0, 1000),
      ...Array.from({ length: 60 }, (_, index) => singleton(index + 1)),
    ];
    const priorAssets = 1000 + 60;
    const crossingDeclared =
      MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS - priorAssets + 1;
    const xField = fieldOfExactly(widePrefix, 61, crossingDeclared, 32_768);
    const x = mintTx(xField, 7n);
    const xIndex = 61;
    // Y: an honest transaction (two singletons) the accepted direction must
    // never convict.
    const y = mintTx(
      encodeMidgardFieldPreimage([singleton(1), singleton(2)]),
      8n,
    );
    // Z: the exact boundary. One singleton, then a target declaring 16,383:
    // 1 + 16,383 == 16,384 is not a crossing, so the header opens the item.
    const zField = encodeMidgardFieldPreimage([
      singleton(1),
      declaring(2, MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS - 1, 4),
    ]);
    const z = mintTx(zField, 9n);
    const block = await acceptedBlock([x, y, z]);
    const [xInclusion, yInclusion, zInclusion] = block.inclusions;
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: {
        transactionsRoot: block.transactionsRoot,
        l2TransactionCount: 3n,
      },
    });

    const references = await publishFamilyReferences(harness, registered);

    const xEvidence = acceptedEvidence(x, xIndex);
    expect(xEvidence.crossing).toBe(true);
    expect(xEvidence.carriage).toBe("Certified");
    expect(xEvidence.accumulatedCount + xEvidence.targetDeclaredCount).toBe(
      MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS + 1,
    );
    const xStaged = planMintDeclaredAssetLimitStagedWalk({
      transactionId: x.id,
      fieldPreimageCbor: xField.toString("hex"),
      policyIndex: xIndex,
    });
    expect(xStaged.grammar).toHaveLength(3);
    expect(xStaged.walk.length).toBeGreaterThanOrEqual(6);
    // At least one transaction consumes a full asset budget inside the wide
    // policy and at least one keeps that policy open across the boundary.
    expect(
      xStaged.walk.some(
        (snapshot, index) =>
          index > 0 &&
          snapshot.cursor.activePolicy === "00".repeat(28) &&
          snapshot.cursor.accumulatedCount -
            xStaged.walk[index - 1]!.cursor.accumulatedCount ===
            MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
      ),
    ).toBe(true);
    coverage.scenario("maximum_supported_evidence");

    const yEvidence = acceptedEvidence(y, 1);
    expect(yEvidence.crossing).toBe(false);
    const yStaged = planMintDeclaredAssetLimitStagedWalk({
      transactionId: y.id,
      fieldPreimageCbor: y.mintField.toString("hex"),
      policyIndex: 1,
    });
    expect(yStaged.walk).toHaveLength(1);
    const zPlan = planMintDeclaredAssetLimitField({
      transactionId: z.id,
      fieldPreimageCbor: zField.toString("hex"),
      policyIndex: 1,
    });
    const zItems = zPlan.items;
    const zEvidence: MintDeclaredAssetLimitEvidence = {
      subject: acceptedVerdictSubject(z.id),
      policyIndex: 1,
      crossing: false,
      accumulatedCount: 1,
      targetPolicyId: zPlan.target.targetPolicyId,
      targetDeclaredCount: zPlan.target.targetDeclaredCount,
      fieldPreimageHex: zField.toString("hex"),
      fieldCommitmentHex: z.commitmentHex,
      targetItemHex: zItems[1]!.toString("hex"),
      carriage: selectMidgardFieldCarriageTier(zField.length),
    };

    // --- Carriage ---------------------------------------------------------
    const xCarriage = await publishCarriage(
      harness,
      registered,
      x,
      xStaged.items,
      "mint declared maximum",
    );
    expect(xCarriage.carriage.measurements).toHaveLength(3);
    for (const [
      index,
      measurement,
    ] of xCarriage.carriage.measurements.entries())
      rows.push([`certified-carriage-chunk-0${index.toString()}`, measurement]);
    rows.push([
      "certified-carriage-certificate",
      xCarriage.certificate!.measurement,
    ]);
    const yCarriage = await publishCarriage(
      harness,
      registered,
      y,
      yStaged.items,
      "mint declared honest",
    );
    rows.push(["raw-carriage-honest", yCarriage.carriage.measurement]);
    const zCarriage = await publishCarriage(
      harness,
      registered,
      z,
      zItems,
      "mint declared boundary",
    );
    rows.push(["raw-carriage-boundary", zCarriage.carriage.measurement]);

    // --- Thread helpers ---------------------------------------------------
    const initThread = async (label: string) => {
      const init = await measured(label, () =>
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
          fraudulentHeaderHash: setup.headerHash,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
        { txHash: init.txHash, outputIndex: init.firstStepOutputIndex },
      ]);
      if (threadUtxo === undefined)
        throw new Error("mint declared init thread absent");
      return {
        threadUtxo,
        threadToken: {
          unit: init.computationThreadUnit,
          fraudulentHeaderHash: init.fraudulentHeaderHash,
        },
      };
    };
    const bindAccepted = (
      thread: Awaited<ReturnType<typeof initThread>>,
      evidence: MintDeclaredAssetLimitEvidence,
      txInclusion: (typeof block.inclusions)[number],
    ) =>
      submitMintDeclaredAssetLimitStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo: thread.threadUtxo,
        threadToken: thread.threadToken,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const common = {
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
    };
    const step02Raw = async (
      threadOutRef: string,
      evidence: MintDeclaredAssetLimitEvidence,
      compactCbor: string,
      input: Partial<
        Parameters<typeof submitMintDeclaredAssetLimitStep02Raw>[0]
      > &
        Pick<
          Parameters<typeof submitMintDeclaredAssetLimitStep02Raw>[0],
          "action" | "next"
        > & {
          /** Compact bytes the opening names, when not the planned ones. */
          readonly openingCompactCbor?: string;
        },
    ) => {
      const planned = planMintDeclaredFieldOpening({
        evidence,
        nativeTxCompactCbor: compactCbor,
        signer: harness.proverSigner,
        label: "mint declared step-02",
      });
      const carriage = await resolveMintDeclaredFieldCarriage({
        ...common,
        planned,
      });
      const stepReference = requireLinearFaultReferenceScript({
        utxo: references[1],
        expectedScriptHash: contracts.steps[1].spendingScriptHash,
        family: "mint-declared-asset-limit",
        stepIndex: 1,
      });
      const opening = fieldOpeningForField({
        fieldIndex: 5,
        nativeTxCompactCbor: input.openingCompactCbor ?? compactCbor,
        carriage: faultProofFieldCarriage({
          planned,
          referenceInputs: [
            ...carriage.carriageUtxos,
            stepReference,
            ...(carriage.certificateUtxo === undefined
              ? []
              : [carriage.certificateUtxo]),
          ],
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: "mint declared step-02",
        }),
      });
      const { openingCompactCbor: _substituted, ...wire } = input;
      return await submitMintDeclaredAssetLimitStep02Raw({
        ...common,
        threadOutRef,
        evidence,
        opening,
        ...carriage,
        referenceScriptUtxo: references[1],
        ...wire,
      });
    };
    const step03Raw = (
      threadOutRef: string,
      evidence: MintDeclaredAssetLimitEvidence,
      compactCbor: string,
      input: Pick<
        Parameters<typeof submitMintDeclaredAssetLimitStep03Raw>[0],
        "priorData" | "checkpointBytesHex" | "budget" | "next" | "nextStepIndex"
      >,
    ) =>
      submitMintDeclaredAssetLimitStep03Raw({
        ...common,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor,
        referenceScriptUtxo: references[2],
        stepRole: "mint declared step-03 explicit",
        ...input,
      });

    // --- X: the maximum accepted lifecycle through the production actuator -
    const xThread = await initThread("init");
    await refused("tx_membership: another transaction's inclusion proof", () =>
      bindAccepted(xThread, xEvidence, {
        ...xInclusion!,
        txMembershipProof: yInclusion!.txMembershipProof,
        txMembershipProofCbor: yInclusion!.txMembershipProofCbor,
      }),
    );
    coverage.seamMutated("tx_membership");
    const xStep01 = await measured("step-01-accepted", () =>
      bindAccepted(xThread, xEvidence, xInclusion!),
    );
    let xOutRef = xStep01.nextThreadOutRef;
    const artifact = buildMintDeclaredAssetLimitArtifact({
      headerHash: setup.headerHash,
      detectionId: `${x.id}:accepted:${xIndex.toString()}`,
      position: 0n,
      evidence: xEvidence,
      nativeTxCompactCbor: x.compactCbor,
      witnessSetCompactCbor: x.witnessSetCompactCbor,
      l2TransactionSourceCbor: x.sourceCbor,
      transactionsPhasRoot: block.transactionsRoot,
      transactionMembershipCbor: xInclusion!.txMembershipProofCbor,
    });
    const binding = {
      definition: { headerHash: setup.headerHash },
      resolvedContracts: {
        category: { categoryId: category.categoryId },
        contracts: {
          fraudProof: {
            spendingScriptHash: harness.contracts.fraudProof.spendingScriptHash,
          },
        },
      },
      network,
      blueprint: harness.realBlueprint,
    };
    const actuatorReferences = {
      steps: references,
      witnesses: harness.witnessReferenceScripts as never,
      fieldPreimageCertificateMint: registered.certificateReference,
    };
    const actuator = createMintDeclaredAssetLimitActuator({
      binding: binding as never,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts,
      references: actuatorReferences,
      stateQueueMutationLeaseCoordinator: {} as never,
    });
    const actuate = async (
      label: string,
      action: Parameters<typeof actuator.capture>[0]["action"],
      nextAddress: string,
    ) =>
      await measured(label, async () => {
        const captured = await actuator.capture({ action, artifact });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await harness.proverLucid.awaitTx(txHash);
        const next = (await harness.proverLucid.utxosAt(nextAddress)).find(
          (utxo) => utxo.txHash === txHash,
        );
        if (next === undefined)
          throw new Error("mint declared actuator omitted its next output");
        return `${next.txHash}#${next.outputIndex.toString()}`;
      });

    // Grammar: a substituted successor is refused before the first
    // certification; a substituted grammar checkpoint is refused at resume.
    await refused("wrong_successor: grammar start to the fold script", () =>
      step02Raw(xOutRef, xEvidence, x.compactCbor, {
        ...mintDeclaredStep02WirePlan({
          staged: xStaged,
          action: { kind: "grammar_start" },
        }),
        nextStepIndex: 2,
      }),
    );
    coverage.seamMutated("wrong_successor");
    xOutRef = await actuate(
      "grammar-start",
      {
        stage: "step_02",
        threadOutRef: xOutRef,
        action: { kind: "grammar_start" },
      },
      applied[1].spendingScriptAddress,
    );
    await refused(
      "grammar_checkpoint: resume from a later ordinal's bytes",
      () =>
        step02Raw(xOutRef, xEvidence, x.compactCbor, {
          action: {
            kind: "grammar_resume",
            checkpointBytesHex: encodeMintDeclaredGrammarCheckpoint(
              xStaged.grammar[1]!,
            ).toString("hex"),
            itemBudget: 24n,
          },
          next: {
            kind: "grammar",
            checkpointHash: hashMintDeclaredGrammarCheckpoint(
              xStaged.grammar[1]!,
            ),
          },
        }),
    );
    coverage.seamMutated("grammar_checkpoint");
    await refused(
      "field_transaction: the honest transaction's compact bytes over the maximum carriage",
      () =>
        step02Raw(xOutRef, xEvidence, x.compactCbor, {
          openingCompactCbor: y.compactCbor,
          ...mintDeclaredStep02WirePlan({
            staged: xStaged,
            action: { kind: "grammar_resume", nextOrdinal: 1 },
          }),
        }),
    );
    coverage.seamMutated("field_transaction");
    for (const ordinal of [1, 2]) {
      xOutRef = await actuate(
        `grammar-resume-0${ordinal.toString()}`,
        {
          stage: "step_02",
          threadOutRef: xOutRef,
          action: { kind: "grammar_resume", nextOrdinal: ordinal },
        },
        applied[1].spendingScriptAddress,
      );
      coverage.resumed();
    }
    xOutRef = await actuate(
      "grammar-finish",
      {
        stage: "step_02",
        threadOutRef: xOutRef,
        action: { kind: "grammar_finish" },
      },
      applied[2].spendingScriptAddress,
    );

    // Fold: substituted checkpoint bytes, an over-bound budget and a budget
    // too small to open a policy are refused; then every planned transaction.
    const xInitialData = mintDeclaredFoldSnapshotData({
      evidence: xEvidence,
      staged: xStaged,
      snapshot: initialMintDeclaredFoldSnapshot(xStaged),
    });
    const xFirstData = mintDeclaredFoldSnapshotData({
      evidence: xEvidence,
      staged: xStaged,
      snapshot: xStaged.walk[0]!,
    });
    // The first fold transactions stay inside the wide policy, so their walk
    // position is still the initial one; a substituted checkpoint must name a
    // position the walk actually left.
    const laterWalkCheckpoint = xStaged.walk.find(
      (snapshot) => snapshot.checkpoint.nextItemIndex > 0,
    )!.checkpoint;
    expect(laterWalkCheckpoint.nextItemIndex).toBeGreaterThan(0);
    await refused("walk_checkpoint: bytes of a later walk position", () =>
      step03Raw(xOutRef, xEvidence, x.compactCbor, {
        priorData: xInitialData,
        checkpointBytesHex:
          encodeMintDeclaredWalkCheckpoint(laterWalkCheckpoint).toString("hex"),
        budget: BigInt(xStaged.foldBudget),
        next: { kind: "fold", data: xFirstData },
      }),
    );
    coverage.seamMutated("walk_checkpoint");
    await refused("fold_budget: one unit past staged_fold_budget", () =>
      step03Raw(xOutRef, xEvidence, x.compactCbor, {
        priorData: xInitialData,
        checkpointBytesHex: encodeMintDeclaredWalkCheckpoint(
          xStaged.initialWalk,
        ).toString("hex"),
        budget: BigInt(MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET + 1),
        next: { kind: "fold", data: xFirstData },
      }),
    );
    await refused(
      "fold_budget: a budget that cannot open a policy re-commits nothing",
      () =>
        step03Raw(xOutRef, xEvidence, x.compactCbor, {
          priorData: xInitialData,
          checkpointBytesHex: encodeMintDeclaredWalkCheckpoint(
            xStaged.initialWalk,
          ).toString("hex"),
          budget: 7n,
          next: { kind: "fold", data: xInitialData },
        }),
    );
    coverage.seamMutated("fold_budget");
    for (const [ordinal, snapshot] of xStaged.walk.entries()) {
      const terminal = ordinal === xStaged.walk.length - 1;
      const consumed =
        snapshot.cursor.accumulatedCount -
        (ordinal === 0
          ? 0
          : xStaged.walk[ordinal - 1]!.cursor.accumulatedCount);
      const shape = terminal
        ? "target header crosses at 16,385"
        : `${consumed.toString()} assets; ${snapshot.cursor.activePolicy === "" ? "policy boundary" : "policy open"}`;
      xOutRef = await actuate(
        `fold-${ordinal.toString().padStart(2, "0")} (${shape})`,
        { stage: "step_03", threadOutRef: xOutRef, walkOrdinal: ordinal },
        applied[terminal ? 3 : 2].spendingScriptAddress,
      );
      if (ordinal > 0) coverage.resumed();
    }
    const xProof = await measured("permanent-proof-mint", async () => {
      const captured = await actuator.capture({
        action: { stage: "step_04", threadOutRef: xOutRef },
        artifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      expect(txHash).toBe(captured.transaction.txHash);
      await harness.proverLucid.awaitTx(txHash);
      const proof = (
        await harness.proverLucid.utxosAt(
          harness.contracts.fraudProof.spendingScriptAddress,
        )
      ).find((utxo) => utxo.txHash === txHash);
      if (proof === undefined)
        throw new Error("mint declared actuator omitted permanent proof token");
      return `${proof.txHash}#${proof.outputIndex.toString()}`;
    });
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");

    // --- Y: the honest transaction is refused at every accepted door -------
    const yThread = await initThread("init-honest");
    const yStep01 = await measured("step-01-honest", () =>
      bindAccepted(yThread, yEvidence, yInclusion!),
    );
    const yWire = mintDeclaredStep02WirePlan({
      staged: yStaged,
      action: { kind: "direct" },
    });
    await refused(
      "field_carriage: the boundary transaction's carriage under the honest compact bytes",
      async () => {
        const zPlanned = planMintDeclaredFieldOpening({
          evidence: zEvidence,
          nativeTxCompactCbor: z.compactCbor,
          signer: harness.proverSigner,
          label: "mint declared substituted carriage",
        });
        const zResolved = await resolveMintDeclaredFieldCarriage({
          ...common,
          planned: zPlanned,
        });
        const stepReference = requireLinearFaultReferenceScript({
          utxo: references[1],
          expectedScriptHash: contracts.steps[1].spendingScriptHash,
          family: "mint-declared-asset-limit",
          stepIndex: 1,
        });
        return await submitMintDeclaredAssetLimitStep02Raw({
          ...common,
          threadOutRef: yStep01.nextThreadOutRef,
          evidence: yEvidence,
          opening: fieldOpeningForField({
            fieldIndex: 5,
            nativeTxCompactCbor: y.compactCbor,
            carriage: faultProofFieldCarriage({
              planned: zPlanned,
              referenceInputs: [...zResolved.carriageUtxos, stepReference],
              label: "mint declared substituted carriage",
            }),
          }),
          ...zResolved,
          ...yWire,
          referenceScriptUtxo: references[1],
        });
      },
    );
    coverage.seamMutated("field_carriage");
    await refused(
      "wrong_successor: direct authentication to the decision script",
      () =>
        step02Raw(yStep01.nextThreadOutRef, yEvidence, y.compactCbor, {
          ...yWire,
          nextStepIndex: 3,
        }),
    );
    const yStep02 = await measured("honest-direct-field", () =>
      submitMintDeclaredAssetLimitStep02({
        ...common,
        threadOutRef: yStep01.nextThreadOutRef,
        evidence: yEvidence,
        nativeTxCompactCbor: y.compactCbor,
        staged: yStaged,
        action: { kind: "direct" },
        referenceScriptUtxo: references[1],
      }),
    );
    const yInitialData = mintDeclaredFoldSnapshotData({
      evidence: yEvidence,
      staged: yStaged,
      snapshot: initialMintDeclaredFoldSnapshot(yStaged),
    });
    const yInitialBytes = encodeMintDeclaredWalkCheckpoint(
      yStaged.initialWalk,
    ).toString("hex");
    await refused(
      "honest accepted block: a crossing decision the fold does not reach",
      () =>
        step03Raw(yStep02.nextThreadOutRef, yEvidence, y.compactCbor, {
          priorData: yInitialData,
          checkpointBytesHex: yInitialBytes,
          budget: BigInt(yStaged.foldBudget),
          next: { kind: "decision", crossing: true },
        }),
    );
    await refused(
      "wrong_successor: the non-crossing decision to the fold script",
      () =>
        step03Raw(yStep02.nextThreadOutRef, yEvidence, y.compactCbor, {
          priorData: yInitialData,
          checkpointBytesHex: yInitialBytes,
          budget: BigInt(yStaged.foldBudget),
          next: { kind: "decision", crossing: false },
          nextStepIndex: 2,
        }),
    );
    const yStep03 = await measured("honest-complete-fold", () =>
      submitMintDeclaredAssetLimitStep03({
        ...common,
        threadOutRef: yStep02.nextThreadOutRef,
        evidence: yEvidence,
        nativeTxCompactCbor: y.compactCbor,
        staged: yStaged,
        walkOrdinal: 0,
        referenceScriptUtxo: references[2],
      }),
    );
    await refused(
      "honest accepted block: the terminal step refuses a non-crossing accepted decision",
      () =>
        submitMintDeclaredAssetLimitStep04Raw({
          ...common,
          threadOutRef: yStep03.nextThreadOutRef,
          policyIndex: 1,
          crossing: false,
          referenceScriptUtxo: references[3],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    coverage.scenario("honest_accepted_block_refusal");

    // A coordinate outside field 5 binds (step 01 checks only its sign) and
    // is refused where the item is authenticated.
    const outsideThread = await initThread("init-outside-coordinate");
    const outsideEvidence = { ...yEvidence, policyIndex: 5 };
    const outsideStep01 = await measured("step-01-outside-coordinate", () =>
      bindAccepted(outsideThread, outsideEvidence, yInclusion!),
    );
    await refused("coordinate: policy index past the field's item count", () =>
      step02Raw(
        outsideStep01.nextThreadOutRef,
        outsideEvidence,
        y.compactCbor,
        yWire,
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");

    // --- Z: the exact boundary is not a crossing -------------------------
    const zThread = await initThread("init-boundary");
    const zStep01 = await measured("step-01-boundary", () =>
      bindAccepted(zThread, zEvidence, zInclusion!),
    );
    const zStep02 = await measured("boundary-direct-field", () =>
      step02Raw(zStep01.nextThreadOutRef, zEvidence, z.compactCbor, {
        action: { kind: "direct" },
        next: {
          kind: "fold",
          checkpointHash: hashMintDeclaredWalkCheckpoint(zPlan.initialWalk),
        },
      }),
    );
    const zInitial = initialMintDeclaredFoldSnapshot(zPlan);
    const zInitialData = mintDeclaredFoldSnapshotData({
      evidence: zEvidence,
      staged: zPlan as never,
      snapshot: zInitial,
    });
    // 9 units close the singleton, 8 open the target: exactly at the bound.
    const zOpened = advanceMintDeclaredFoldSnapshot({
      snapshot: zInitial,
      transactionId: z.id,
      items: zItems,
      target: zPlan.target,
      budget: 17,
    });
    expect(zOpened.cursor.outcome).toBe(0);
    expect(zOpened.cursor.accumulatedCount).toBe(1);
    expect(zOpened.cursor.assetsRemaining).toBe(
      MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS - 1,
    );
    const zInitialBytes = encodeMintDeclaredWalkCheckpoint(
      zPlan.initialWalk,
    ).toString("hex");
    await refused(
      "adjacent bound: 16,384 declared in total is claimed as a crossing",
      () =>
        step03Raw(zStep02.nextThreadOutRef, zEvidence, z.compactCbor, {
          priorData: zInitialData,
          checkpointBytesHex: zInitialBytes,
          budget: 17n,
          next: { kind: "decision", crossing: true },
        }),
    );
    coverage.adjacentOverBoundRefused();
    const zStep03 = await measured("boundary-open-target", () =>
      step03Raw(zStep02.nextThreadOutRef, zEvidence, z.compactCbor, {
        priorData: zInitialData,
        checkpointBytesHex: zInitialBytes,
        budget: 17n,
        next: {
          kind: "fold",
          data: mintDeclaredFoldSnapshotData({
            evidence: zEvidence,
            staged: zPlan as never,
            snapshot: zOpened,
          }),
        },
      }),
    );
    const [zOpenedUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: zStep03.nextThreadOutRef.split("#")[0]!,
        outputIndex: Number(zStep03.nextThreadOutRef.split("#")[1]),
      },
    ]);
    const zOpenedDatum = Data.from(
      zOpenedUtxo!.datum!,
      MintDeclaredAssetLimitStep03DatumSchema as never,
    ) as {
      data: {
        assets_remaining: bigint;
        accumulated_count: bigint;
        outcome: bigint;
      };
    };
    expect(zOpenedDatum.data.outcome).toBe(0n);
    expect(zOpenedDatum.data.accumulated_count).toBe(1n);
    expect(zOpenedDatum.data.assets_remaining).toBe(
      BigInt(MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS - 1),
    );

    // --- Removal of the fraudulent block through X's permanent proof -----
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      {
        removalReferenceScripts: removalReferences.published,
      },
    );
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        [firstStepDeploymentEntry]: {
          scriptHash: applied[0].spendingScriptHash,
          contract: {
            type: applied[0].spendingScript.type,
            cborHex: applied[0].spendingScript.script,
          },
        },
      },
    };
    const removalActuator = createMintDeclaredAssetLimitActuator({
      binding: {
        ...binding,
        deploymentInfo,
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as never,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts,
      references: actuatorReferences,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "mint-declared-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    vi.setSystemTime(harness.emulator.now());
    await measured("mutation-leased-removal", async () => {
      const captured = await removalActuator.capture({
        action: {
          stage: "remove",
          nextRemovalOutRef: setup.fraudulentBlockOutRef,
          fraudProofOutRef: xProof,
        },
        artifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      expect(txHash).toBe(captured.transaction.txHash);
      await harness.proverLucid.awaitTx(txHash);
      return txHash;
    });
    coverage.scenario("permanent_proof_token_and_descendant_removal");
    expectPositiveMargins(rows, [
      "certified-carriage-chunk-00",
      "certified-carriage-chunk-01",
      "certified-carriage-chunk-02",
      "raw-carriage-honest",
      "raw-carriage-boundary",
    ]);
    printLedger("mint-declared-asset-limit-accepted-ledger", rows);
  }, 1_200_000);

  it("proves the exact forced wrongful rejection across a policy item, refuses an honest rejection and every mutated leaf, and cancels from every step", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realMintDeclaredAssetLimit: true,
        alwaysFraudProofCatalogue: true,
        alwaysStateQueue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, catalogue, category } = registered;
    const rows: (readonly [string, Measurement])[] = [];
    const measured = async <Result>(
      label: string,
      action: () => Promise<Result>,
    ): Promise<Result> => {
      progress(label);
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        action,
      );
      rows.push([label, captured.measurement]);
      return captured.result;
    };
    const refused = async (label: string, action: () => Promise<unknown>) => {
      progress(`refusal: ${label}`);
      await expectOnchainRefusal(action);
    };
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("forced funder key absent");
    const baseFixture = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
    });

    // A: wrongly rejected at policy 0 — a 300-asset policy that closes in a
    // second fold transaction. B: rightly rejected — the target declares
    // 16,385 at policy 0.
    const forcedTx = (mintField: Buffer) => {
      const valid = makeNativeTx({ spendInputCbors: [], fee: 0n });
      const nativeTx = materializeMidgardNativeTxFromCanonical({
        version: valid.version,
        validity: valid.validity,
        body: { ...valid.body, mintPreimageCbor: mintField },
        witnessSet: valid.witnessSet,
      });
      const id = computeMidgardNativeTxId(nativeTx).toString("hex");
      const adjudicated = adjudicateMidgardNativeTxFullValidity(
        nativeTx,
        "TxIsInvalid",
      );
      const source = deriveMidgardNativeTxProofSource(adjudicated);
      expect(computeMidgardNativeTxId(adjudicated).toString("hex")).toBe(id);
      const reason = { MintDeclaredAssetLimit: { policy_index: 0n } } as const;
      return {
        mintField,
        id,
        compactCbor: source.compactCbor.toString("hex"),
        reason,
        leaf: {
          tx_id: id,
          source: {
            compact_cbor: source.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              source.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              source.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: { ForcedTxInvalid: { reason } },
        } as const,
      };
    };
    const a = forcedTx(encodeMidgardFieldPreimage([wide(1, 300)]));
    const b = forcedTx(
      encodeMidgardFieldPreimage([
        declaring(1, MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS + 1, 4),
      ]),
    );
    const keyA = baseFixture.eventKey.ForcedTransactionEventKey.tx_order_id;
    const keyB = { ...keyA, outputIndex: keyA.outputIndex + 1n };
    const leafBytes = (key: typeof keyA, leaf: typeof a.leaf) => ({
      key: Buffer.from(Data.to(key, OutputReference), "hex"),
      value: Buffer.from(
        Data.to(leaf as never, ForcedInclusionTxV1Schema as never),
        "hex",
      ),
    });
    const leaves = [leafBytes(keyA, a.leaf), leafBytes(keyB, b.leaf)];
    const root = await buildCountedRoot(
      ROOT_DOMAINS.forcedTransactionsV1,
      leaves,
    );
    const proofStore = new Store(undefined);
    await proofStore.ready();
    const proofTrie = new Trie(proofStore);
    for (const leaf of leaves) await proofTrie.insert(leaf.key, leaf.value);
    const membershipOf = async (
      key: typeof keyA,
      leaf: typeof a.leaf,
      bytes: (typeof leaves)[number],
    ) => ({
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key,
      value: leaf,
      proof: Data.from(
        (await proofTrie.prove(bytes.key)).toCBOR().toString("hex"),
        Proof,
      ),
    });
    const membershipA = await membershipOf(keyA, a.leaf, leaves[0]!);
    const membershipB = await membershipOf(keyB, b.leaf, leaves[1]!);
    const header = {
      ...baseFixture.header,
      forcedTransactionsRoot: root.root,
      forcedTransactionCount: root.count,
    };
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header,
    });
    expect(setup.headerHash).toBe(
      await Effect.runPromise(hashBlockHeader(header)),
    );

    const references = await publishFamilyReferences(harness, registered);

    const evidenceOf = (tx: typeof a, key: typeof keyA) =>
      prepareMintDeclaredAssetLimitEvidence({
        finding: {
          subject: forcedVerdictSubject({
            transactionId: tx.id,
            sourceKey: key,
            rejectionReason: tx.reason,
          }),
          policyIndex: 0,
        },
        fieldPreimage: tx.mintField,
        committedFieldHashHex: midgardFieldCommitment(tx.mintField).toString(
          "hex",
        ),
      });
    const aEvidence = evidenceOf(a, keyA);
    expect(aEvidence.crossing).toBe(false);
    expect(aEvidence.accumulatedCount).toBe(300);
    const aStaged = planMintDeclaredAssetLimitStagedWalk({
      transactionId: a.id,
      fieldPreimageCbor: a.mintField.toString("hex"),
      policyIndex: 0,
    });
    expect(aStaged.walk).toHaveLength(2);
    expect(aStaged.walk[0]!.cursor.assetsRemaining).toBe(116);
    expect(aStaged.walk[1]!.cursor.outcome).toBe(2);
    const bEvidence = evidenceOf(b, keyB);
    expect(bEvidence.crossing).toBe(true);
    const bStaged = planMintDeclaredAssetLimitStagedWalk({
      transactionId: b.id,
      fieldPreimageCbor: b.mintField.toString("hex"),
      policyIndex: 0,
    });
    expect(bStaged.walk).toHaveLength(1);

    const aCarriage = await publishCarriage(
      harness,
      registered,
      a,
      aStaged.items,
      "mint declared forced",
    );
    rows.push(["raw-carriage-publication", aCarriage.carriage.measurement]);
    const bCarriage = await publishCarriage(
      harness,
      registered,
      b,
      bStaged.items,
      "mint declared honest rejection",
    );
    rows.push([
      "raw-carriage-honest-rejection",
      bCarriage.carriage.measurement,
    ]);

    const common = {
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
    };
    const initialize = async () => {
      const initialized = await submitCommittedFieldShapeInit({
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
        fraudulentHeaderHash: setup.headerHash,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      return `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
    };
    const bind = (
      threadOutRef: string,
      evidence: MintDeclaredAssetLimitEvidence,
      membership: typeof membershipA,
    ) =>
      submitMintDeclaredAssetLimitStep01Forced({
        ...common,
        threadOutRef,
        finding: evidence,
        forcedSource: { header, membership, direction: 1n },
        referenceScriptUtxo: references[0],
      });
    const decode = (
      threadOutRef: string,
      evidence: MintDeclaredAssetLimitEvidence,
      tx: typeof a,
      staged: typeof aStaged,
    ) =>
      submitMintDeclaredAssetLimitStep02({
        ...common,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: tx.compactCbor,
        staged,
        action: { kind: "direct" },
        referenceScriptUtxo: references[1],
      });
    const fold = (
      threadOutRef: string,
      evidence: MintDeclaredAssetLimitEvidence,
      tx: typeof a,
      staged: typeof aStaged,
      walkOrdinal: number,
    ) =>
      submitMintDeclaredAssetLimitStep03({
        ...common,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: tx.compactCbor,
        staged,
        walkOrdinal,
        referenceScriptUtxo: references[2],
      });
    const cancel = (threadOutRef: string, referenceScriptUtxo: UTxO) =>
      submitMintDeclaredAssetLimitCancel({
        ...common,
        threadOutRef,
        referenceScriptUtxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });

    // --- Cancel from every physical step ----------------------------------
    await measured("cancel-step-01", async () =>
      cancel(await initialize(), references[0]),
    );
    coverage.cancelled("step-01");
    const cancel02 = await bind(await initialize(), aEvidence, membershipA);
    await measured("cancel-step-02", () =>
      cancel(cancel02.nextThreadOutRef, references[1]),
    );
    coverage.cancelled("step-02");
    const cancel03 = await decode(
      (await bind(await initialize(), aEvidence, membershipA)).nextThreadOutRef,
      aEvidence,
      a,
      aStaged,
    );
    await measured("cancel-step-03", () =>
      cancel(cancel03.nextThreadOutRef, references[2]),
    );
    coverage.cancelled("step-03");
    const cancel04Fold0 = await fold(
      (
        await decode(
          (await bind(await initialize(), aEvidence, membershipA))
            .nextThreadOutRef,
          aEvidence,
          a,
          aStaged,
        )
      ).nextThreadOutRef,
      aEvidence,
      a,
      aStaged,
      0,
    );
    const cancel04 = await fold(
      cancel04Fold0.nextThreadOutRef,
      aEvidence,
      a,
      aStaged,
      1,
    );
    await measured("cancel-step-04", () =>
      cancel(cancel04.nextThreadOutRef, references[3]),
    );
    coverage.cancelled("step-04");

    // --- Mutated leaves and coordinates at the forced door ----------------
    const mutatedThread = await initialize();
    const rawBind = (
      input: Partial<
        Parameters<typeof submitMintDeclaredAssetLimitStep01ForcedRaw>[0]
      >,
    ) =>
      submitMintDeclaredAssetLimitStep01ForcedRaw({
        ...common,
        threadOutRef: mutatedThread,
        subject: aEvidence.subject,
        datumPolicyIndex: 0,
        redeemerPolicyIndex: 0,
        forcedSource: { header, membership: membershipA, direction: 1n },
        referenceScriptUtxo: references[0],
        ...input,
      });
    const subjectAt = (policyIndex: number) =>
      forcedVerdictSubject({
        transactionId: a.id,
        sourceKey: keyA,
        rejectionReason: {
          MintDeclaredAssetLimit: { policy_index: BigInt(policyIndex) },
        },
      });
    await refused(
      "reason coordinate: the leaf rejects policy 0, the thread claims policy 1",
      () =>
        rawBind({
          subject: subjectAt(1),
          datumPolicyIndex: 1,
          redeemerPolicyIndex: 1,
        }),
    );
    await refused(
      "subject coordinate: datum policy 1 against redeemer policy 0",
      () => rawBind({ datumPolicyIndex: 1 }),
    );
    await refused(
      "direction: a rejection leaf bound as wrongful acceptance",
      () =>
        rawBind({
          forcedSource: { header, membership: membershipA, direction: 0n },
        }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await refused(
      "forced_leaf: the leaf's reason re-spelled under the same proof",
      () =>
        rawBind({
          subject: subjectAt(1),
          datumPolicyIndex: 1,
          redeemerPolicyIndex: 1,
          forcedSource: {
            header,
            membership: {
              ...membershipA,
              value: {
                ...a.leaf,
                verdict: {
                  ForcedTxInvalid: {
                    reason: { MintDeclaredAssetLimit: { policy_index: 1n } },
                  },
                },
              },
            },
            direction: 1n,
          },
        }),
    );
    await refused("forced_leaf: another leaf's membership proof", () =>
      rawBind({
        forcedSource: {
          header,
          membership: { ...membershipA, proof: membershipB.proof },
          direction: 1n,
        },
      }),
    );
    coverage.seamMutated("forced_leaf");

    // --- A: the wrongful rejection, resumed inside the policy item --------
    const aStep01 = await measured("step-01-forced", async () =>
      bind(await initialize(), aEvidence, membershipA),
    );
    const aStep02 = await measured("forced-direct-field", () =>
      decode(aStep01.nextThreadOutRef, aEvidence, a, aStaged),
    );
    const aFold0 = await measured(
      "forced-fold-00 (8 + 184 assets; policy open)",
      () => fold(aStep02.nextThreadOutRef, aEvidence, a, aStaged, 0),
    );
    const aFold1 = await measured(
      "forced-fold-01 (116 assets close the target)",
      () => fold(aFold0.nextThreadOutRef, aEvidence, a, aStaged, 1),
    );
    coverage.resumed();
    const aFinal = await measured("forced-permanent-proof-mint", () =>
      submitMintDeclaredAssetLimitStep04({
        ...common,
        threadOutRef: aFold1.nextThreadOutRef,
        evidence: aEvidence,
        referenceScriptUtxo: references[3],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(aFinal.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON_ARM, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");

    // --- B: an honest rejection reaches the crossing and is refused --------
    const bStep01 = await measured("step-01-honest-rejection", async () =>
      bind(await initialize(), bEvidence, membershipB),
    );
    const bStep02 = await measured("honest-rejection-direct-field", () =>
      decode(bStep01.nextThreadOutRef, bEvidence, b, bStaged),
    );
    const bStep03 = await measured("honest-rejection-crossing", () =>
      fold(bStep02.nextThreadOutRef, bEvidence, b, bStaged, 0),
    );
    await refused(
      "honest forced rejection: the terminal step refuses a crossing rejected decision",
      () =>
        submitMintDeclaredAssetLimitStep04Raw({
          ...common,
          threadOutRef: bStep03.nextThreadOutRef,
          policyIndex: 0,
          crossing: true,
          referenceScriptUtxo: references[3],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    expectPositiveMargins(rows, [
      "raw-carriage-publication",
      "raw-carriage-honest-rejection",
    ]);
    printLedger("mint-declared-asset-limit-forced-ledger", rows);
  }, 900_000);

  it("declares the complete lifecycle coverage it exercised", () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [
        "tx_membership",
        "forced_leaf",
        "field_carriage",
        "field_transaction",
        "grammar_checkpoint",
        "walk_checkpoint",
        "fold_budget",
        "wrong_successor",
      ],
      cancellablePhysicalSteps: ["step-01", "step-02", "step-03", "step-04"],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
  });
});
