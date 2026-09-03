import { spawn } from "node:child_process";

import {
  computeHash32,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { extractForcedLeafEvidence } from "../src/evidence/forced-leaf-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  decodeMissingNativeScriptTxGrammarCheckpoint,
  decodeMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  hashMissingNativeScriptTxGrammarCheckpoint,
  hashMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxSemanticCheckpoint,
} from "../src/missing-native-script-tx/staged-walk.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { createScriptIntegrityHashMissingTransactionPort } from "../src/script-integrity-hash-missing/actuator.js";
import { testingOnlyScriptIntegrityHashMissingArtifact } from "../src/script-integrity-hash-missing/artifact.js";
import type { ScriptIntegrityHashMissingContracts } from "../src/script-integrity-hash-missing/contracts.js";
import { prepareScriptIntegrityHashMissingEvidence } from "../src/script-integrity-hash-missing/family.js";
import {
  detectScriptIntegrityHashMissingFromReconstruction,
  reconstructScriptIntegrityHashMissingEvidence,
} from "../src/script-integrity-hash-missing/replay.js";
import { ScriptIntegrityStepDatums } from "../src/script-integrity-hash-missing/schemas.js";
import {
  submitScriptIntegrityHashMissingStep01Accepted,
  submitScriptIntegrityHashMissingStep02Accepted,
  submitScriptIntegrityHashMissingStep03Direct,
} from "../src/script-integrity-hash-missing/submit-direct.js";
import { submitScriptIntegrityHashMissingInit } from "../src/script-integrity-hash-missing/submit-init.js";
import {
  submitScriptIntegrityHashMissingCancel,
  submitScriptIntegrityHashMissingRedeemerGrammar,
  submitScriptIntegrityHashMissingScriptGrammar,
  submitScriptIntegrityHashMissingScriptScan,
  submitScriptIntegrityHashMissingStep04,
} from "../src/script-integrity-hash-missing/submitters.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { CURSOR_FAMILY_ACTION } from "../src/workflow/cursor-family-state.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const isolatedUplcWorker = new URL(
  "./support/isolated-uplc-evaluator-v1.cjs",
  import.meta.url,
);
const redeemerTags = [
  "spend",
  "mint",
  "publish",
  "withdraw",
  "vote",
  "propose",
] as const;
const runIsolatedUplcWorker = async (input: string): Promise<string> =>
  await new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [isolatedUplcWorker.pathname], {
      stdio: ["pipe", "pipe", "pipe"],
    });
    const stdout: Buffer[] = [];
    const stderr: Buffer[] = [];
    child.stdout.on("data", (chunk: Buffer) => stdout.push(chunk));
    child.stderr.on("data", (chunk: Buffer) => stderr.push(chunk));
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) {
        const output = Buffer.concat(stdout).toString("utf8");
        if (output.length === 0) {
          reject(
            new Error(
              `Isolated UPLC worker returned no output: ${Buffer.concat(stderr).toString("utf8")}`,
            ),
          );
        } else {
          resolve(output);
        }
      } else {
        reject(
          new Error(
            `Isolated UPLC worker exited ${String(code)}: ${Buffer.concat(stderr).toString("utf8")}`,
          ),
        );
      }
    });
    child.stdin.end(input);
  });
const makeIsolatedUplcEvaluator = () => ({
  name: "aiken-isolated-process-v1",
  evaluate: async ({
    tx,
    additionalUTxOs,
    context,
  }: Parameters<
    NonNullable<
      NonNullable<
        NonNullable<
          Parameters<typeof makeFaultProofEmulatorHarness>[0]
        >["lucidOptions"]
      >["evaluator"]
    >["evaluate"]
  >[0]) => {
    const stdout = await runIsolatedUplcWorker(
      JSON.stringify(
        {
          tx,
          additionalUTxOs,
          costModels: context.costModels.to_cbor_hex(),
          maxTxExSteps: context.protocolParameters.maxTxExSteps.toString(),
          maxTxExMem: context.protocolParameters.maxTxExMem.toString(),
          zeroTime: context.slotConfig.zeroTime.toString(),
          zeroSlot: context.slotConfig.zeroSlot.toString(),
          slotLength: context.slotConfig.slotLength,
        },
        (_key, value) =>
          typeof value === "bigint" ? { $bigint: value.toString() } : value,
      ),
    );
    return (JSON.parse(stdout) as string[]).map((hex) => {
      const redeemer = CML.LegacyRedeemer.from_cbor_hex(hex);
      const redeemerTag = redeemerTags[redeemer.tag()];
      if (redeemerTag === undefined) {
        throw new Error(`Unknown CML redeemer tag ${redeemer.tag()}`);
      }
      return {
        ex_units: {
          mem: Number(redeemer.ex_units().mem()),
          steps: Number(redeemer.ex_units().steps()),
        },
        redeemer_index: Number(redeemer.index()),
        redeemer_tag: redeemerTag,
      };
    });
  },
});

const field8Checkpoint = (
  checkpoint: ReturnType<typeof initialMissingNativeScriptTxGrammarCheckpoint>,
) => ({ ...checkpoint, fieldIndex: 8 });
const advanceField8 = (
  checkpoint: ReturnType<typeof field8Checkpoint>,
  items: readonly Uint8Array[],
  budget = 32,
) =>
  field8Checkpoint(
    advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: { ...checkpoint, fieldIndex: 6 },
      items,
      budget,
    }),
  );
const encodeField8 = (
  checkpoint: ReturnType<typeof field8Checkpoint>,
): Buffer => {
  const bytes = encodeMissingNativeScriptTxGrammarCheckpoint({
    ...checkpoint,
    fieldIndex: 6,
  });
  bytes[36] = 8;
  return bytes;
};
const decodeField8 = (
  bytes: Uint8Array,
): ReturnType<typeof field8Checkpoint> => {
  const canonicalField6Bytes = Buffer.from(bytes);
  canonicalField6Bytes[36] = 6;
  return field8Checkpoint(
    decodeMissingNativeScriptTxGrammarCheckpoint(canonicalField6Bytes),
  );
};
const hashField8 = (checkpoint: ReturnType<typeof field8Checkpoint>): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii"),
      encodeField8(checkpoint),
    ]),
  ).toString("hex");

describe("script-integrity-hash-missing real lifecycle", () => {
  it("publishes, proves accepted zero integrity hash, mints, and removes", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realScriptIntegrityHashMissing: true },
    });
    const chain =
      harness.contracts.fraudProofContracts.scriptIntegrityHashMissing;
    const family: ScriptIntegrityHashMissingContracts = {
      steps: chain.steps,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    };
    const category = harness.catalogue.categories.scriptIntegrityHashMissing!;
    const item = encodeMidgardVersionedScript({
      language: "PlutusV3",
      scriptBytes: Buffer.from([1]),
    });
    const scriptPreimage = encodeCbor([item]);
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: Buffer.alloc(32),
        auxiliaryDataHash: Buffer.alloc(32),
        fee: 1_000n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: scriptPreimage,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: { kind: "normal", nativeTx },
    });
    const acceptedDetections =
      detectScriptIntegrityHashMissingFromReconstruction({
        headerHash: block.reconstruction.headerHash,
        reconstruction: block.reconstruction,
      });
    expect(
      acceptedDetections.map(({ direction, source, transactionId }) => ({
        direction,
        source,
        transactionId,
      })),
    ).toEqual([
      {
        direction: "wrongfulAcceptance",
        source: "accepted",
        transactionId: block.nativeTxId,
      },
    ]);
    const replayEvidence = await reconstructScriptIntegrityHashMissingEvidence({
      evidence: {
        headerHash: block.reconstruction.headerHash,
        reconstruction: block.reconstruction,
      } as CanonicalBlockEvidence,
      transactionId: block.nativeTxId,
      direction: "wrongfulAcceptance",
    });
    expect(replayEvidence.scriptIntegrityHash).toBe("00".repeat(32));
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: block.header,
    });
    const refs: UTxO[] = [];
    const publication: number[] = [];
    for (const [index, step] of family.steps.entries()) {
      const captured = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `integrity step ${index + 1}`,
        }),
      );
      refs.push(captured.result.utxo);
      publication.push(captured.measurement.completeSignedBytes);
    }
    const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
    const witnessSet: SDK.NativeTxWitnessSetCompact = {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    };
    const subject = SDK.acceptedVerdictSubject(block.nativeTxId);
    const evidence = prepareScriptIntegrityHashMissingEvidence({
      finding: {
        category: "scriptIntegrityHashMissing",
        headerHash: setup.headerHash,
        transactionId: block.nativeTxId,
        direction: "wrongfulAcceptance",
        source: "accepted",
        rejectionReason: null,
      },
      subject,
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        nativeTx.compact,
      ).toString("hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
        addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
        scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
        redeemerTxWitsHash: Buffer.from(
          witnessSet.redeemer_tx_wits_hash,
          "hex",
        ),
      }).toString("hex"),
      fieldPreimageLengthsCbor: "80",
      scriptWitnessesPreimageCbor: scriptPreimage.toString("hex"),
      redeemersPreimageCbor: EMPTY_CBOR_LIST.toString("hex"),
      scriptIntegrityHash: "00".repeat(32),
      scriptLanguages: [3],
      redeemerCount: 0,
    });
    const ledger: {
      label: string;
      bytes: number;
      memory: string;
      cpu: string;
      margin: number;
    }[] = [];
    const measured = async <T>(label: string, operation: () => Promise<T>) => {
      let captured;
      try {
        captured = await captureEmulatorSubmission(harness.emulator, operation);
      } catch (cause) {
        throw new Error(
          `script-integrity lifecycle failed at ${label}: ${String(cause)}`,
        );
      }
      ledger.push({
        label,
        bytes: captured.measurement.completeSignedBytes,
        memory: captured.measurement.executionMemory.toString(),
        cpu: captured.measurement.executionSteps.toString(),
        margin: captured.measurement.l1ByteMargin,
      });
      return captured.result;
    };
    const initialize = () =>
      submitScriptIntegrityHashMissingInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const cancel = (threadOutRef: string, referenceScriptUtxo: UTxO) =>
      submitScriptIntegrityHashMissingCancel({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    if (block.txInclusion === null) throw new Error("normal inclusion missing");
    const accepted01 = (threadOutRef: string) =>
      submitScriptIntegrityHashMissingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion!,
        referenceScriptUtxo: refs[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const accepted02 = (threadOutRef: string) =>
      submitScriptIntegrityHashMissingStep02Accepted({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        header: block.header,
        subject,
        witnessSetHash:
          nativeTx.compact.transactionWitnessSetHash.toString("hex"),
        referenceScriptUtxo: refs[1]!,
      });
    const direct03 = (threadOutRef: string) =>
      submitScriptIntegrityHashMissingStep03Direct({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: evidence.nativeTxCompactCbor,
        witnessSet,
        referenceScriptUtxo: refs[2]!,
      });
    const cancel01 = await initialize();
    await measured("cancel-step01", () =>
      cancel(cancel01.nextThreadOutRef, refs[0]!),
    );
    const cancel02Init = await initialize();
    const cancel02State = await accepted01(cancel02Init.nextThreadOutRef);
    await measured("cancel-step02", () =>
      cancel(cancel02State.nextThreadOutRef, refs[1]!),
    );
    const cancel03Init = await initialize();
    const cancel03Bound = await accepted01(cancel03Init.nextThreadOutRef);
    const cancel03State = await accepted02(cancel03Bound.nextThreadOutRef);
    await measured("cancel-step03", () =>
      cancel(cancel03State.nextThreadOutRef, refs[2]!),
    );
    const cancel04Init = await initialize();
    const cancel04Bound = await accepted01(cancel04Init.nextThreadOutRef);
    const cancel04Subject = await accepted02(cancel04Bound.nextThreadOutRef);
    const cancel04State = await direct03(cancel04Subject.nextThreadOutRef);
    await measured("cancel-step04", () =>
      cancel(cancel04State.nextThreadOutRef, refs[6]!),
    );
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const artifact = testingOnlyScriptIntegrityHashMissingArtifact({
      detectionId: `script-integrity-hash-missing:accepted:0:${block.nativeTxId}`,
      evidence,
      source: {
        header: block.header,
        nativeTxCompactCbor: evidence.nativeTxCompactCbor,
        witnessSetCompactCbor: evidence.witnessSetCompactCbor,
        acceptedInclusion: block.txInclusion!,
      },
    });
    const port = createScriptIntegrityHashMissingTransactionPort({
      binding: {
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        definition: { headerHash: setup.headerHash },
        resolvedContracts: { category },
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as unknown as FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts: family,
      references: {
        steps: refs as unknown as readonly [
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
        ],
        witnesses: harness.witnessReferenceScripts as Required<
          typeof harness.witnessReferenceScripts
        >,
        fieldPreimageCertificateMint: refs[0]!,
      },
      lease: {
        acquire: async () => ({
          token: "script-integrity-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    const actorAction = (
      stage: "init" | `step_0${1 | 2 | 3 | 7}`,
      threadOutRef?: string,
    ): FraudProofWorkflowAction => {
      if (stage === "init")
        return {
          actionId: `init:${setup.fraudulentBlockOutRef}`,
          input: {
            schemaVersion: CURSOR_FAMILY_ACTION,
            category: "scriptIntegrityHashMissing" as const,
            stage,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          },
        };
      return {
        actionId: `${stage}:${threadOutRef!}:${setup.fraudulentBlockOutRef}`,
        input: {
          schemaVersion: CURSOR_FAMILY_ACTION,
          category: "scriptIntegrityHashMissing" as const,
          stage,
          ordinal: Number(stage.slice(-1)),
          threadOutRef: threadOutRef!,
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        },
      };
    };
    const actorSubmit = async (
      stage: "init" | `step_0${1 | 2 | 3 | 7}`,
      nextOrdinal: 1 | 2 | 3 | 7 | null,
      threadOutRef?: string,
    ) => {
      const captured = await port.capture({
        action: actorAction(stage, threadOutRef),
        artifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      await harness.proverLucid.awaitTx(txHash);
      if (nextOrdinal === null) return { txHash };
      const next = (
        await harness.proverLucid.utxosAt(
          family.steps[nextOrdinal - 1].spendingScriptAddress,
        )
      ).find((utxo) => utxo.txHash === txHash);
      if (next === undefined)
        throw new Error("package actuator omitted its next thread output");
      return {
        txHash,
        nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
      };
    };
    const init = await measured("init", () => actorSubmit("init", 1));
    const step01 = await measured("step01", () =>
      actorSubmit("step_01", 2, init.nextThreadOutRef),
    );
    const step02 = await measured("step02", () =>
      actorSubmit("step_02", 3, step01.nextThreadOutRef),
    );
    const step03 = await measured("step03", () =>
      actorSubmit("step_03", 7, step02.nextThreadOutRef),
    );
    const final = await measured("step04", () =>
      actorSubmit("step_07", null, step03.nextThreadOutRef),
    );
    const proofUnit = toUnit(
      family.fraudProof.policyId,
      `${category.categoryId}${setup.headerHash}`,
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        family.fraudProof.spendingScriptAddress,
        proofUnit,
      ),
    ).resolves.toHaveLength(1);
    const [proof] = await harness.proverLucid.utxosAtWithUnit(
      family.fraudProof.spendingScriptAddress,
      proofUnit,
    );
    if (proof === undefined) throw new Error("package actuator omitted proof");
    vi.setSystemTime(harness.emulator.now());
    await measured("removal", async () => {
      const captured = await port.capture({
        action: {
          actionId: `remove:${setup.fraudulentBlockOutRef}:${proof.txHash}#${proof.outputIndex.toString()}:${setup.fraudulentBlockOutRef}`,
          input: {
            schemaVersion: CURSOR_FAMILY_ACTION,
            category: "scriptIntegrityHashMissing",
            stage: "remove",
            fraudProofOutRef: `${proof.txHash}#${proof.outputIndex.toString()}`,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            nextRemovalOutRef: setup.fraudulentBlockOutRef,
            requiresMutationLease: false,
          },
        },
        artifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      await harness.proverLucid.awaitTx(txHash);
      return { txHash };
    });
    expect(publication.every((bytes) => bytes <= 16_384)).toBe(true);
    expect(final.txHash).toMatch(/^[0-9a-f]{64}$/u);
    for (const row of ledger) {
      expect(row.margin, row.label).toBeGreaterThan(0);
      expect(BigInt(row.memory), row.label).toBeLessThanOrEqual(16_500_000n);
      expect(BigInt(row.cpu), row.label).toBeLessThanOrEqual(10_000_000_000n);
    }
    console.info(
      `[script-integrity-hash-missing-publication] ${JSON.stringify(publication)}`,
    );
    console.info(
      `[script-integrity-hash-missing-fit-ledger] ${JSON.stringify(ledger)}`,
    );
  }, 600_000);

  it("proves an exact forced ScriptIntegrityHashMissing rejection", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realScriptIntegrityHashMissing: true },
    });
    const chain =
      harness.contracts.fraudProofContracts.scriptIntegrityHashMissing;
    const family: ScriptIntegrityHashMissingContracts = {
      steps: chain.steps,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    };
    const category = harness.catalogue.categories.scriptIntegrityHashMissing!;
    const item = encodeMidgardVersionedScript({
      language: "PlutusV3",
      scriptBytes: Buffer.from([2]),
    });
    const scriptPreimage = encodeCbor([item]);
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: Buffer.alloc(32, 1),
        auxiliaryDataHash: Buffer.alloc(32),
        fee: 2_000n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: scriptPreimage,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const orderKey = { transactionId: "ab".repeat(32), outputIndex: 0n };
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: {
        kind: "forced",
        nativeTx,
        orderKey,
        verdict: { ForcedTxInvalid: { reason: "ScriptIntegrityHashMissing" } },
      },
    });
    const forcedDetections = detectScriptIntegrityHashMissingFromReconstruction(
      {
        headerHash: block.reconstruction.headerHash,
        reconstruction: block.reconstruction,
      },
    );
    expect(
      forcedDetections.map(({ direction, source, transactionId }) => ({
        direction,
        source,
        transactionId,
      })),
    ).toEqual([
      {
        direction: "wrongfulRejection",
        source: "forced",
        transactionId: block.nativeTxId,
      },
    ]);
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: block.header,
    });
    const refs: UTxO[] = [];
    for (const [index, step] of family.steps.entries())
      refs.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `forced integrity ${index + 1}`,
          })
        ).utxo,
      );
    const eventKey = {
      ForcedTransactionEventKey: { tx_order_id: orderKey },
    } as const;
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    });
    const forcedLeaf = await extractForcedLeafEvidence({
      reconstruction: block.reconstruction,
      eventKey,
    });
    const subject = SDK.forcedVerdictSubject({
      transactionId: block.nativeTxId,
      sourceKey: orderKey,
      rejectionReason: "ScriptIntegrityHashMissing",
    });
    const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
    const witnessSet: SDK.NativeTxWitnessSetCompact = {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    };
    const evidence = prepareScriptIntegrityHashMissingEvidence({
      finding: {
        category: "scriptIntegrityHashMissing",
        headerHash: setup.headerHash,
        transactionId: block.nativeTxId,
        direction: "wrongfulRejection",
        source: "forced",
        rejectionReason: "ScriptIntegrityHashMissing",
      },
      subject,
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        nativeTx.compact,
      ).toString("hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
        addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
        scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
        redeemerTxWitsHash: Buffer.from(
          witnessSet.redeemer_tx_wits_hash,
          "hex",
        ),
      }).toString("hex"),
      fieldPreimageLengthsCbor: "80",
      scriptWitnessesPreimageCbor: scriptPreimage.toString("hex"),
      redeemersPreimageCbor: EMPTY_CBOR_LIST.toString("hex"),
      scriptIntegrityHash: "01".repeat(32),
      scriptLanguages: [3],
      redeemerCount: 0,
      forcedLeaf,
    });
    const forcedArtifact = testingOnlyScriptIntegrityHashMissingArtifact({
      detectionId: `script-integrity-hash-missing:forced:0:${block.nativeTxId}:wrongfulRejection`,
      evidence,
      source: {
        header: block.header,
        nativeTxCompactCbor: evidence.nativeTxCompactCbor,
        witnessSetCompactCbor: evidence.witnessSetCompactCbor,
        forcedHeader: block.header,
        forcedMembership: membership,
        forcedDirection: 1n,
      },
    });
    const forcedPort = createScriptIntegrityHashMissingTransactionPort({
      binding: {
        blueprint: harness.realBlueprint,
        deploymentInfo: {},
        network,
        definition: { headerHash: setup.headerHash },
        resolvedContracts: { category },
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as unknown as FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts: family,
      references: {
        steps: refs as unknown as readonly [
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
        ],
        witnesses: harness.witnessReferenceScripts as Required<
          typeof harness.witnessReferenceScripts
        >,
        fieldPreimageCertificateMint: refs[0]!,
      },
      lease: {
        acquire: async () => ({
          token: "forced-integrity-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    const forcedActorStep = async (ordinal: 1 | 2, threadOutRef: string) => {
      const stage = `step_0${ordinal.toString()}`;
      const captured = await forcedPort.capture({
        action: {
          actionId: `${stage}:${threadOutRef}:${setup.fraudulentBlockOutRef}`,
          input: {
            schemaVersion: CURSOR_FAMILY_ACTION,
            category: "scriptIntegrityHashMissing",
            stage,
            ordinal,
            threadOutRef,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          },
        },
        artifact: forcedArtifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      await harness.proverLucid.awaitTx(txHash);
      const next = (
        await harness.proverLucid.utxosAt(
          family.steps[ordinal].spendingScriptAddress,
        )
      ).find((utxo) => utxo.txHash === txHash);
      if (next === undefined)
        throw new Error("forced production actuator omitted next thread");
      return {
        txHash,
        nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
      };
    };
    const init = await submitScriptIntegrityHashMissingInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: family,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await forcedActorStep(1, init.nextThreadOutRef);
    const step02 = await forcedActorStep(2, step01.nextThreadOutRef);
    const step03 = await submitScriptIntegrityHashMissingStep03Direct({
      lucid: harness.proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      witnessSet,
      referenceScriptUtxo: refs[2]!,
    });
    const final = await submitScriptIntegrityHashMissingStep04({
      lucid: harness.proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[6]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(final.fraudProofUnit).toContain(category.categoryId);
  }, 600_000);

  it("splits 224+224 certified items across resumable ledger transactions", async () => {
    const isolatedEvaluator = makeIsolatedUplcEvaluator();
    expect(isolatedEvaluator.name).toBe("aiken-isolated-process-v1");
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realScriptIntegrityHashMissing: true },
      // Resource isolation only: the worker executes the same pinned local
      // Aiken UPLC evaluator with the unchanged harness protocol parameters.
      lucidOptions: { evaluator: isolatedEvaluator },
    });
    const chain =
      harness.contracts.fraudProofContracts.scriptIntegrityHashMissing;
    const family: ScriptIntegrityHashMissingContracts = {
      steps: chain.steps,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    };
    const category = harness.catalogue.categories.scriptIntegrityHashMissing!;
    const measurements: VanRossemFitMeasurement[] = [];
    const maximumShape =
      "224 script witnesses + 224 redeemers; certified two-chunk fields; 24-item resumable checkpoints";
    const measured = async <T>(
      name: string,
      kind: "publication" | "lifecycle",
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      captured.measurements.forEach((measurement, index) => {
        const measurementName =
          captured.measurements.length === 1
            ? name
            : `${name}-${index.toString().padStart(2, "0")}`;
        measurements.push({
          name: measurementName,
          kind,
          maximumShape,
          signedBytes: measurement.completeSignedBytes,
          memoryUnits: measurement.executionMemory,
          cpuUnits: measurement.executionSteps,
        });
        console.info(
          `[script-integrity-max-row] ${JSON.stringify({ name: measurementName, bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })}`,
        );
      });
      return captured.result;
    };
    const itemBudget = 24;
    const scriptItems = Array.from({ length: 224 }, (_, index) =>
      encodeMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: Buffer.alloc(70, (index % 250) + 1),
      }),
    );
    const redeemerItems = Array.from({ length: 224 }, (_, index) =>
      Buffer.alloc(70, (index % 250) + 1),
    );
    const scriptPreimage = encodeCbor(scriptItems);
    const redeemerPreimage = encodeCbor(redeemerItems);
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: Buffer.alloc(32),
        auxiliaryDataHash: Buffer.alloc(32),
        fee: 3_000n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: scriptPreimage,
        redeemerTxWitsPreimageCbor: redeemerPreimage,
      },
    });
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: { kind: "normal", nativeTx },
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: block.header,
    });
    const refs: UTxO[] = [];
    for (const [index, step] of family.steps.entries())
      refs.push(
        (
          await measured(
            `reference-script-${(index + 1).toString().padStart(2, "0")}`,
            "publication",
            () =>
              publishPlainReferenceScriptUtxo({
                lucid: harness.funderLucid,
                script: step.spendingScript,
                label: `max integrity ${index + 1}`,
              }),
          )
        ).utxo,
      );
    const certificateRef = (
      await measured("field-certificate-reference-script", "publication", () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: "integrity certificate mint",
        }),
      )
    ).utxo;
    const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact).toString(
      "hex",
    );
    const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
    const witnessSet: SDK.NativeTxWitnessSetCompact = {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    };
    const witnessSetCbor = encodeMidgardNativeTxWitnessSetCompact({
      addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
      scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
      redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
    }).toString("hex");
    const owner = harness.proverSigner.paymentKeyHash;
    const plan = (fieldIndex: 6 | 8, items: readonly Buffer[]) =>
      planFaultProofFieldOpening({
        fieldIndex,
        anchorTxId: block.nativeTxId,
        nativeTxCompactCbor: compactCbor,
        witnessSet,
        itemCbors: items,
        owner,
        publish: true,
        anchorWitnessSetHash:
          nativeTx.compact.transactionWitnessSetHash.toString("hex"),
        label: `integrity field ${fieldIndex}`,
      });
    const scriptPlan = plan(6, scriptItems);
    const redeemerPlan = plan(8, redeemerItems);
    expect(scriptPlan.plan.tier).toBe("Certified");
    expect(redeemerPlan.plan.tier).toBe("Certified");
    const publishAndCertify = async (planned: typeof scriptPlan) => {
      console.info(
        `[script-integrity-max] publishing field ${planned.plan.fieldIndex.toString()}`,
      );
      const chunks = await measured(
        `field-${planned.plan.fieldIndex.toString()}-chunks`,
        "publication",
        () =>
          publishFaultProofFieldCarriage({
            lucid: harness.proverLucid,
            signer: harness.proverSigner,
            planned,
            publisherAddress: harness.proverSigner.address,
            label: "integrity max carriage",
          }),
      );
      console.info(
        `[script-integrity-max] certifying field ${planned.plan.fieldIndex.toString()}`,
      );
      const certified = await measured(
        `field-${planned.plan.fieldIndex.toString()}-certificate`,
        "lifecycle",
        () =>
          certifyFaultProofFieldCarriage({
            lucid: harness.proverLucid,
            network,
            signer: harness.proverSigner,
            planned,
            certificatePolicyId: family.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              family.fieldPreimageCertificateMintingScript!,
            certificateReferenceScriptUtxo: certificateRef,
            chunkUtxos: chunks,
            compactCbor,
            witnessSetCompactCbor: witnessSetCbor,
          }),
      );
      console.info(
        `[script-integrity-max] certified field ${planned.plan.fieldIndex.toString()}`,
      );
      return { chunks, certificate: certified.certificateUtxo };
    };
    const scriptPublished = await publishAndCertify(scriptPlan);
    const redeemerPublished = await publishAndCertify(redeemerPlan);
    const carriage = (published: typeof scriptPublished) => [
      ...published.chunks,
      published.certificate,
    ];
    const opening = (
      planned: typeof scriptPlan,
      published: typeof scriptPublished,
      ref: UTxO,
    ) =>
      faultProofFieldOpening({
        planned,
        referenceInputs: [...carriage(published), ref],
        certificatePolicyId: family.fieldPreimageCertificatePolicyId,
        label: "integrity max opening",
      });
    const subject = SDK.acceptedVerdictSubject(block.nativeTxId);
    if (block.txInclusion === null)
      throw new Error("max normal inclusion missing");
    const productionEvidence = prepareScriptIntegrityHashMissingEvidence({
      finding: {
        category: "scriptIntegrityHashMissing",
        headerHash: setup.headerHash,
        transactionId: block.nativeTxId,
        direction: "wrongfulAcceptance",
        source: "accepted",
        rejectionReason: null,
      },
      subject,
      nativeTxCompactCbor: compactCbor,
      witnessSetCompactCbor: witnessSetCbor,
      fieldPreimageLengthsCbor: "80",
      scriptWitnessesPreimageCbor: scriptPreimage.toString("hex"),
      redeemersPreimageCbor: redeemerPreimage.toString("hex"),
      scriptIntegrityHash: "00".repeat(32),
      scriptLanguages: Array.from({ length: 224 }, () => 3 as const),
      redeemerCount: 224,
    });
    const productionArtifact = testingOnlyScriptIntegrityHashMissingArtifact({
      detectionId: `script-integrity-hash-missing:accepted:0:${block.nativeTxId}`,
      evidence: productionEvidence,
      source: {
        header: block.header,
        nativeTxCompactCbor: compactCbor,
        witnessSetCompactCbor: witnessSetCbor,
        acceptedInclusion: block.txInclusion,
      },
    });
    const productionPort = createScriptIntegrityHashMissingTransactionPort({
      binding: {
        blueprint: harness.realBlueprint,
        deploymentInfo: {},
        network,
        definition: { headerHash: setup.headerHash },
        resolvedContracts: { category },
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as unknown as FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts: family,
      references: {
        steps: refs as unknown as readonly [
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
        ],
        witnesses: harness.witnessReferenceScripts as Required<
          typeof harness.witnessReferenceScripts
        >,
        fieldPreimageCertificateMint: certificateRef,
      },
      lease: {
        acquire: async () => ({
          token: "script-integrity-max-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    const productionTransition = async (
      stage: "step_03" | "step_04",
      ordinal: 3 | 4,
      threadOutRef: string,
      nextOrdinal: 4,
    ) => {
      const captured = await productionPort.capture({
        action: {
          actionId: `${stage}:${threadOutRef}:${setup.fraudulentBlockOutRef}`,
          input: {
            schemaVersion: CURSOR_FAMILY_ACTION,
            category: "scriptIntegrityHashMissing",
            stage,
            ordinal,
            threadOutRef,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          },
        },
        artifact: productionArtifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      await harness.proverLucid.awaitTx(txHash);
      const next = (
        await harness.proverLucid.utxosAt(
          family.steps[nextOrdinal - 1].spendingScriptAddress,
        )
      ).find((utxo) => utxo.txHash === txHash);
      if (next === undefined)
        throw new Error("production staged actuator omitted next thread");
      return {
        txHash,
        outputIndex: BigInt(next.outputIndex),
        nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
      };
    };
    const datum = (index: number, data: unknown) =>
      Data.to(
        { fraud_prover: owner, data } as never,
        ScriptIntegrityStepDatums[index] as never,
      );
    const init = await measured("init", "lifecycle", () =>
      submitScriptIntegrityHashMissingInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        category,
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
    console.info("[script-integrity-max] initialized");
    const bound = await measured("step01", "lifecycle", () =>
      submitScriptIntegrityHashMissingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion!,
        referenceScriptUtxo: refs[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const subjectState = await measured("step02", "lifecycle", () =>
      submitScriptIntegrityHashMissingStep02Accepted({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        header: block.header,
        subject,
        witnessSetHash:
          nativeTx.compact.transactionWitnessSetHash.toString("hex"),
        referenceScriptUtxo: refs[1]!,
      }),
    );
    console.info(
      "[script-integrity-max] source bound; submitting first staged transition",
    );
    let grammar = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: initialMissingNativeScriptTxGrammarCheckpoint({
        txId: block.nativeTxId,
        items: scriptItems,
      }),
      items: scriptItems,
      budget: itemBudget,
    });
    let state: Record<string, unknown> = {
      subject,
      witness_set_hash:
        nativeTx.compact.transactionWitnessSetHash.toString("hex"),
      script_integrity_hash: "00".repeat(32),
      phase: {
        ScriptGrammar: {
          checkpoint_hash: hashMissingNativeScriptTxGrammarCheckpoint(grammar),
        },
      },
    };
    let transition = await measured("step03-start-staged", "lifecycle", () =>
      productionTransition("step_03", 3, subjectState.nextThreadOutRef, 4),
    );
    console.info("[script-integrity-max] first staged transition confirmed");
    let outRef = transition.nextThreadOutRef;
    let grammarResumeIndex = 0;
    while (grammar.nextItemIndex < scriptItems.length) {
      if (grammarResumeIndex === 0)
        console.info("[script-integrity-max] submitting first grammar resume");
      const prior = grammar;
      grammar = advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: grammar,
        items: scriptItems,
        budget: itemBudget,
      });
      state = {
        ...state,
        phase: {
          ScriptGrammar: {
            checkpoint_hash:
              hashMissingNativeScriptTxGrammarCheckpoint(grammar),
          },
        },
      };
      transition = await measured(
        `script-grammar-resume-${grammarResumeIndex.toString().padStart(2, "0")}`,
        "lifecycle",
        () =>
          grammarResumeIndex === 0
            ? productionTransition("step_04", 4, outRef, 4)
            : submitScriptIntegrityHashMissingScriptGrammar({
                lucid: harness.proverLucid,
                contracts: family,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: outRef,
                referenceScriptUtxo: refs[3]!,
                authenticatedCarriageUtxos: carriage(scriptPublished),
                closes: false,
                nextDatum: datum(3, state),
                buildArgs: ({ input_index, output_index }) => ({
                  Resume: {
                    input_index,
                    output_index,
                    opening: opening(scriptPlan, scriptPublished, refs[3]!),
                    checkpoint_bytes:
                      encodeMissingNativeScriptTxGrammarCheckpoint(
                        prior,
                      ).toString("hex"),
                    item_budget: BigInt(itemBudget),
                  },
                }),
              }),
      );
      grammarResumeIndex += 1;
      if (grammarResumeIndex === 1) {
        console.info("[script-integrity-max] first grammar resume confirmed");
        grammar = decodeMissingNativeScriptTxGrammarCheckpoint(
          encodeMissingNativeScriptTxGrammarCheckpoint(grammar),
        );
      }
      outRef = transition.nextThreadOutRef;
    }
    let semantic = advanceMissingNativeScriptTxSemanticCheckpoint({
      checkpoint: initialMissingNativeScriptTxSemanticCheckpoint({
        grammar,
        items: scriptItems,
      }),
      txId: block.nativeTxId,
      items: scriptItems,
      budget: itemBudget,
    });
    state = {
      ...state,
      phase: {
        ScriptScan: {
          checkpoint_hash:
            hashMissingNativeScriptTxSemanticCheckpoint(semantic),
          contains_non_native_script: true,
        },
      },
    };
    transition = await measured(
      "script-grammar-close-start-scan",
      "lifecycle",
      () =>
        submitScriptIntegrityHashMissingScriptGrammar({
          lucid: harness.proverLucid,
          contracts: family,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: outRef,
          referenceScriptUtxo: refs[3]!,
          authenticatedCarriageUtxos: carriage(scriptPublished),
          closes: true,
          nextDatum: datum(4, state),
          buildArgs: ({ input_index, output_index }) => ({
            StartScan: {
              input_index,
              output_index,
              opening: opening(scriptPlan, scriptPublished, refs[3]!),
              checkpoint_bytes:
                encodeMissingNativeScriptTxGrammarCheckpoint(grammar).toString(
                  "hex",
                ),
              item_budget: BigInt(itemBudget),
            },
          }),
        }),
    );
    outRef = transition.nextThreadOutRef;
    let scanResumeIndex = 0;
    while (semantic.nextItemIndex < scriptItems.length) {
      const prior = semantic;
      semantic = advanceMissingNativeScriptTxSemanticCheckpoint({
        checkpoint: semantic,
        txId: block.nativeTxId,
        items: scriptItems,
        budget: itemBudget,
      });
      const closes = semantic.nextItemIndex === scriptItems.length;
      state = {
        ...state,
        phase: closes
          ? { ScriptComplete: { contains_non_native_script: true } }
          : {
              ScriptScan: {
                checkpoint_hash:
                  hashMissingNativeScriptTxSemanticCheckpoint(semantic),
                contains_non_native_script: true,
              },
            },
      };
      transition = await measured(
        `script-scan-${scanResumeIndex.toString().padStart(2, "0")}`,
        "lifecycle",
        () =>
          submitScriptIntegrityHashMissingScriptScan({
            lucid: harness.proverLucid,
            contracts: family,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: outRef,
            referenceScriptUtxo: refs[4]!,
            authenticatedCarriageUtxos: carriage(scriptPublished),
            closes,
            nextDatum: datum(closes ? 5 : 4, state),
            buildArgs: ({ input_index, output_index }) => ({
              input_index,
              output_index,
              opening: opening(scriptPlan, scriptPublished, refs[4]!),
              checkpoint_bytes:
                encodeMissingNativeScriptTxSemanticCheckpoint(prior).toString(
                  "hex",
                ),
              item_budget: BigInt(itemBudget),
            }),
          }),
      );
      scanResumeIndex += 1;
      if (scanResumeIndex === 1) {
        semantic = decodeMissingNativeScriptTxSemanticCheckpoint(
          encodeMissingNativeScriptTxSemanticCheckpoint(semantic),
        );
      }
      outRef = transition.nextThreadOutRef;
    }
    let redeemerGrammar = advanceField8(
      field8Checkpoint(
        initialMissingNativeScriptTxGrammarCheckpoint({
          txId: block.nativeTxId,
          items: redeemerItems,
        }),
      ),
      redeemerItems,
      itemBudget,
    );
    state = {
      ...state,
      phase: {
        RedeemerGrammar: {
          checkpoint_hash: hashField8(redeemerGrammar),
          contains_non_native_script: true,
        },
      },
    };
    transition = await measured("redeemer-grammar-start", "lifecycle", () =>
      submitScriptIntegrityHashMissingRedeemerGrammar({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: outRef,
        referenceScriptUtxo: refs[5]!,
        authenticatedCarriageUtxos: carriage(redeemerPublished),
        closes: false,
        nextDatum: datum(5, state),
        buildArgs: ({ input_index, output_index }) => ({
          Start: {
            input_index,
            output_index,
            opening: opening(redeemerPlan, redeemerPublished, refs[5]!),
            item_budget: BigInt(itemBudget),
          },
        }),
      }),
    );
    outRef = transition.nextThreadOutRef;
    let redeemerResumeIndex = 0;
    while (redeemerGrammar.nextItemIndex < redeemerItems.length) {
      const prior = redeemerGrammar;
      redeemerGrammar = advanceField8(
        redeemerGrammar,
        redeemerItems,
        itemBudget,
      );
      state = {
        ...state,
        phase: {
          RedeemerGrammar: {
            checkpoint_hash: hashField8(redeemerGrammar),
            contains_non_native_script: true,
          },
        },
      };
      transition = await measured(
        `redeemer-grammar-resume-${redeemerResumeIndex.toString().padStart(2, "0")}`,
        "lifecycle",
        () =>
          submitScriptIntegrityHashMissingRedeemerGrammar({
            lucid: harness.proverLucid,
            contracts: family,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: outRef,
            referenceScriptUtxo: refs[5]!,
            authenticatedCarriageUtxos: carriage(redeemerPublished),
            closes: false,
            nextDatum: datum(5, state),
            buildArgs: ({ input_index, output_index }) => ({
              Resume: {
                input_index,
                output_index,
                opening: opening(redeemerPlan, redeemerPublished, refs[5]!),
                checkpoint_bytes: encodeField8(prior).toString("hex"),
                item_budget: BigInt(itemBudget),
              },
            }),
          }),
      );
      redeemerResumeIndex += 1;
      if (redeemerResumeIndex === 1) {
        const durableCheckpoint = encodeField8(redeemerGrammar);
        const durableHash = hashField8(redeemerGrammar);
        expect(() =>
          decodeMissingNativeScriptTxGrammarCheckpoint(durableCheckpoint),
        ).toThrow("must name field 6");
        redeemerGrammar = decodeField8(durableCheckpoint);
        expect(encodeField8(redeemerGrammar)).toEqual(durableCheckpoint);
        expect(hashField8(redeemerGrammar)).toBe(durableHash);
      }
      outRef = transition.nextThreadOutRef;
    }
    const decision = {
      subject,
      script_integrity_hash: "00".repeat(32),
      contains_non_native_script: true,
      has_redeemers: true,
    };
    transition = await measured("redeemer-grammar-finish", "lifecycle", () =>
      submitScriptIntegrityHashMissingRedeemerGrammar({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: outRef,
        referenceScriptUtxo: refs[5]!,
        authenticatedCarriageUtxos: carriage(redeemerPublished),
        closes: true,
        nextDatum: datum(6, decision),
        buildArgs: ({ input_index, output_index }) => ({
          Finish: {
            input_index,
            output_index,
            opening: opening(redeemerPlan, redeemerPublished, refs[5]!),
            checkpoint_bytes: encodeField8(redeemerGrammar).toString("hex"),
          },
        }),
      }),
    );
    const final = await measured("step04-mint", "lifecycle", () =>
      submitScriptIntegrityHashMissingStep04({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: transition.nextThreadOutRef,
        referenceScriptUtxo: refs[6]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(final.fraudProofUnit).toContain(category.categoryId);
    const removalRefs = await measured(
      "removal-reference-scripts",
      "publication",
      () =>
        publishRemovalReferenceScripts({
          lucid: harness.proverLucid,
          contracts: harness.contracts,
        }),
    );
    const now = BigInt(harness.emulator.now());
    await measured("state-queue-removal", "lifecycle", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalRefs.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "scriptIntegrityHashMissing",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    const blueprintBytes = await readFile(
      new URL("../../../onchain/aiken/plutus.json", import.meta.url),
    );
    const blueprint = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "scriptIntegrityHashMissing",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion:
        blueprint.preamble?.compiler?.version ?? "unknown-aiken-compiler",
      measurements,
    });
    expect(ledger.entries).toHaveLength(measurements.length);
    expect(
      ledger.entries.every(
        (entry) =>
          entry.signedByteMargin > 0 &&
          BigInt(entry.memoryUnitMargin) > 0n &&
          BigInt(entry.cpuUnitMargin) > 0n,
      ),
    ).toBe(true);
    expect(
      ledger.entries
        .filter((entry) => entry.kind === "publication")
        .every((entry) => (entry.publicationReserveMargin ?? -1) >= 0),
    ).toBe(true);
    console.info(
      `[script-integrity-hash-missing-max-fit-ledger] ${JSON.stringify(ledger)}`,
    );
  }, 1_200_000);
});
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
