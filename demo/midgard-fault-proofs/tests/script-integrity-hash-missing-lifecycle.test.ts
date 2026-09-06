import { EMPTY_NULL_ROOT } from "@al-ft/midgard-core";
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
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
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
  writeVanRossemFitLedger,
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
  encodeScriptIntegrityField8Checkpoint,
  hashScriptIntegrityField8Checkpoint,
  planScriptIntegrityHashMissingStagedWalk,
} from "../src/script-integrity-hash-missing/staged-plan.js";
import {
  submitScriptIntegrityHashMissingStep01Accepted,
  submitScriptIntegrityHashMissingStep01Forced,
  submitScriptIntegrityHashMissingStep02Accepted,
  submitScriptIntegrityHashMissingStep03Direct,
} from "../src/script-integrity-hash-missing/submit-direct.js";
import { submitScriptIntegrityHashMissingInit } from "../src/script-integrity-hash-missing/submit-init.js";
import {
  submitScriptIntegrityHashMissingCancel,
  submitScriptIntegrityHashMissingRedeemerGrammar,
  submitScriptIntegrityHashMissingScriptGrammar,
  submitScriptIntegrityHashMissingScriptScan,
  submitScriptIntegrityHashMissingStep02,
  submitScriptIntegrityHashMissingStep03,
  submitScriptIntegrityHashMissingStep04,
} from "../src/script-integrity-hash-missing/submitters.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { CURSOR_FAMILY_ACTION } from "../src/workflow/cursor-family-state.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { expectRegisteredChainParity } from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
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

const REASON = "ScriptIntegrityHashMissing";
const ABSENT_HASH = EMPTY_NULL_ROOT.toString("hex");
/** Every seam a step authenticates before it reads or commits anything. */
const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf",
  "compact_tx",
  "witness_set_anchor",
  "field_preimage",
  "field_certificate",
  "checkpoint",
] as const;
/** The seven physical scripts, in chain order; every one carries a cancel arm. */
const PHYSICAL_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "script-grammar",
  "script-scan",
  "redeemer-grammar",
  "step-04",
] as const;
const coverage = createLifecycleCoverageRecorder();

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. A fresh application of the same blueprint and
 * shared policies must reproduce it step for step before a suite drives it.
 */
const registeredFamily = async (harness: Harness) => {
  const registered =
    harness.contracts.fraudProofContracts.scriptIntegrityHashMissing;
  const category = harness.catalogue.categories.scriptIntegrityHashMissing!;
  const applied = await Effect.runPromise(
    SDK.buildScriptIntegrityHashMissingFaultProofContracts({
      blueprint: SDK.parseFaultProofBlueprint(
        structuredClone(harness.realBlueprint),
      ),
      network,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId:
        harness.contracts.fraudProofCatalogue.policyId,
    }),
  );
  expectRegisteredChainParity({
    registered,
    applied: applied.scriptIntegrityHashMissing.steps,
    category,
  });
  expect(applied.computationThread.policyId).toBe(
    harness.contracts.computationThread.policyId,
  );
  expect(applied.fraudProof.policyId).toBe(
    harness.contracts.fraudProof.policyId,
  );
  const family: ScriptIntegrityHashMissingContracts = {
    steps: registered.steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  return { family, category };
};

const nativeTxOf = ({
  scriptItems,
  redeemerItems,
  scriptIntegrityHash,
  fee,
}: {
  readonly scriptItems: readonly Buffer[];
  readonly redeemerItems: readonly Buffer[];
  readonly scriptIntegrityHash: Buffer;
  readonly fee: bigint;
}) =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash,
      auxiliaryDataHash: Buffer.alloc(32),
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: encodeCbor([...scriptItems]),
      redeemerTxWitsPreimageCbor: encodeCbor([...redeemerItems]),
    },
  });

const plutusScript = (byte: number) =>
  encodeMidgardVersionedScript({
    language: "PlutusV3",
    scriptBytes: Buffer.from([byte]),
  });

const FORCED_ORDER_KEY = { transactionId: "ab".repeat(32), outputIndex: 0n };

/**
 * One committed block on the registered chain with the family's seven
 * reference scripts published, plus raw submitters that hand the caller's
 * datum and redeemer to the chain unchanged, so every negative below is a
 * validator refusal rather than an off-chain guard.
 */
const makeScenario = async ({
  nativeTx,
  forcedReason,
}: {
  readonly nativeTx: ReturnType<typeof nativeTxOf>;
  readonly forcedReason?: SDK.RejectionReason;
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { realScriptIntegrityHashMissing: true },
  });
  const { family, category } = await registeredFamily(harness);
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject:
      forcedReason === undefined
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: FORCED_ORDER_KEY,
            verdict: { ForcedTxInvalid: { reason: forcedReason } },
          },
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
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `integrity scenario step ${(index + 1).toString()}`,
        })
      ).utxo,
    );
  const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact).toString(
    "hex",
  );
  const derived = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  const witnessSet: SDK.NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: Buffer.from(derived.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(derived.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(derived.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
  const witnessSetCbor = encodeMidgardNativeTxWitnessSetCompact({
    addrTxWitsHash: Buffer.from(derived.addrTxWitsHash),
    scriptTxWitsHash: Buffer.from(derived.scriptTxWitsHash),
    redeemerTxWitsHash: Buffer.from(derived.redeemerTxWitsHash),
  }).toString("hex");
  const witnessSetHash =
    nativeTx.compact.transactionWitnessSetHash.toString("hex");
  const owner = harness.proverSigner.paymentKeyHash;
  const common = (index: number) => ({
    lucid: harness.proverLucid,
    contracts: family,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
    referenceScriptUtxo: refs[index]!,
  });
  const datum = (index: number, data: unknown) =>
    Data.to(
      { fraud_prover: owner, data } as never,
      ScriptIntegrityStepDatums[index] as never,
    );
  const init = async () =>
    (
      await submitScriptIntegrityHashMissingInit({
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
      })
    ).nextThreadOutRef;
  const accepted01 = async (
    threadOutRef: string,
    txInclusion = block.txInclusion!,
  ) =>
    (
      await submitScriptIntegrityHashMissingStep01Accepted({
        ...common(0),
        blueprint: harness.realBlueprint,
        network,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      })
    ).nextThreadOutRef;
  const forced01 = async (threadOutRef: string) =>
    (
      await submitScriptIntegrityHashMissingStep01Forced({
        ...common(0),
        threadOutRef,
        direction: 1n,
      })
    ).nextThreadOutRef;
  const step02 = async (
    threadOutRef: string,
    {
      subject,
      anchoredWitnessSetHash = witnessSetHash,
      forcedMembership = null,
    }: {
      readonly subject: SDK.VerdictSubject;
      readonly anchoredWitnessSetHash?: string;
      readonly forcedMembership?: SDK.RootMembershipProof<
        SDK.OutputReference,
        SDK.ForcedInclusionTxV1
      > | null;
    },
  ) =>
    (
      await submitScriptIntegrityHashMissingStep02({
        ...common(1),
        threadOutRef,
        nextDatum: datum(2, {
          subject,
          witness_set_hash: anchoredWitnessSetHash,
        }),
        buildArgs: ({ input_index, output_index }) => ({
          input_index,
          output_index,
          header: block.header,
          forced_membership: forcedMembership,
        }),
      })
    ).nextThreadOutRef;
  const direct03 = async (
    threadOutRef: string,
    {
      decision,
      compact = compactCbor,
      scriptPreimage,
      redeemerPreimage,
      staged = false,
    }: {
      readonly decision: {
        readonly subject: SDK.VerdictSubject;
        readonly script_integrity_hash: string;
        readonly contains_non_native_script: boolean;
        readonly has_redeemers: boolean;
      };
      readonly compact?: string;
      readonly scriptPreimage: Buffer;
      readonly redeemerPreimage: Buffer;
      readonly staged?: boolean;
    },
  ) =>
    (
      await submitScriptIntegrityHashMissingStep03({
        ...common(2),
        threadOutRef,
        staged,
        nextDatum: datum(6, decision),
        buildArgs: ({ input_index, output_index }) => ({
          Direct: {
            input_index,
            output_index,
            native_tx_compact_cbor: compact,
            witness_set: witnessSet,
            script_witnesses: {
              Inline: { preimage: scriptPreimage.toString("hex") },
            },
            redeemers: {
              Inline: { preimage: redeemerPreimage.toString("hex") },
            },
          },
        }),
      })
    ).nextThreadOutRef;
  const step04 = (threadOutRef: string) =>
    submitScriptIntegrityHashMissingStep04({
      ...common(6),
      threadOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = (threadOutRef: string, index: number) =>
    submitScriptIntegrityHashMissingCancel({
      ...common(index),
      threadOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  return {
    harness,
    family,
    category,
    block,
    setup,
    refs,
    compactCbor,
    witnessSet,
    witnessSetCbor,
    witnessSetHash,
    owner,
    common,
    datum,
    init,
    accepted01,
    forced01,
    step02,
    direct03,
    step04,
    cancel,
  };
};

describe("script-integrity-hash-missing real lifecycle", () => {
  it("publishes, proves accepted absent integrity hash, mints, and removes", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realScriptIntegrityHashMissing: true },
    });
    const { family, category } = await registeredFamily(harness);
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
        scriptIntegrityHash: EMPTY_NULL_ROOT,
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
    expect(replayEvidence.scriptIntegrityHash).toBe(
      EMPTY_NULL_ROOT.toString("hex"),
    );
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
      scriptIntegrityHash: EMPTY_NULL_ROOT.toString("hex"),
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
    coverage.cancelled("step-01");
    const cancel02Init = await initialize();
    const cancel02State = await accepted01(cancel02Init.nextThreadOutRef);
    await measured("cancel-step02", () =>
      cancel(cancel02State.nextThreadOutRef, refs[1]!),
    );
    coverage.cancelled("step-02");
    const cancel03Init = await initialize();
    const cancel03Bound = await accepted01(cancel03Init.nextThreadOutRef);
    const cancel03State = await accepted02(cancel03Bound.nextThreadOutRef);
    await measured("cancel-step03", () =>
      cancel(cancel03State.nextThreadOutRef, refs[2]!),
    );
    coverage.cancelled("step-03");
    const cancel04Init = await initialize();
    const cancel04Bound = await accepted01(cancel04Init.nextThreadOutRef);
    const cancel04Subject = await accepted02(cancel04Bound.nextThreadOutRef);
    const cancel04State = await direct03(cancel04Subject.nextThreadOutRef);
    await measured("cancel-step04", () =>
      cancel(cancel04State.nextThreadOutRef, refs[6]!),
    );
    coverage.cancelled("step-04");
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
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("permanent_proof_token_and_descendant_removal");
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
    const { family, category } = await registeredFamily(harness);
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
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
  }, 600_000);

  it("splits 224+224 certified items across resumable ledger transactions", async () => {
    // This journey evaluates ~60 transactions in one worker. It used to hand
    // each evaluation to a fresh subprocess (tests/support/
    // isolated-uplc-evaluator-v1.cjs) purely to dodge the @lucid-evolution/uplc
    // linear-memory leak, at ~2.5 s of process spawn and module load per
    // evaluation — 150 s for a journey that runs in 19 s in-process. With the
    // allocator fix (lucid-evolution PR #728) the in-process evaluator is the
    // same pinned Aiken UPLC evaluator with no leak to dodge.
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realScriptIntegrityHashMissing: true },
    });
    const { family, category } = await registeredFamily(harness);
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
        scriptIntegrityHash: EMPTY_NULL_ROOT,
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
      scriptIntegrityHash: EMPTY_NULL_ROOT.toString("hex"),
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
      script_integrity_hash: ABSENT_HASH,
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
      outRef = transition.nextThreadOutRef;
      if (grammarResumeIndex === 1) {
        console.info("[script-integrity-max] first grammar resume confirmed");
        // A real interruption: the next resume starts from nothing but the
        // durable checkpoint bytes the previous transaction committed.
        grammar = decodeMissingNativeScriptTxGrammarCheckpoint(
          encodeMissingNativeScriptTxGrammarCheckpoint(grammar),
        );
        coverage.resumed();
        // Field 8's certificate and chunks cannot resume the field-6 grammar:
        // the certificate names its field, and the door refuses the mismatch.
        await expectOnchainRefusal(() =>
          submitScriptIntegrityHashMissingScriptGrammar({
            lucid: harness.proverLucid,
            contracts: family,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: outRef,
            referenceScriptUtxo: refs[3]!,
            authenticatedCarriageUtxos: carriage(redeemerPublished),
            closes: false,
            nextDatum: datum(3, state),
            buildArgs: ({ input_index, output_index }) => ({
              Resume: {
                input_index,
                output_index,
                opening: opening(redeemerPlan, redeemerPublished, refs[3]!),
                checkpoint_bytes:
                  encodeMissingNativeScriptTxGrammarCheckpoint(
                    grammar,
                  ).toString("hex"),
                item_budget: BigInt(itemBudget),
              },
            }),
          }),
        );
        coverage.seamMutated("field_certificate");
      }
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
      script_integrity_hash: ABSENT_HASH,
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
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(
        fileURLToPath(
          new URL(
            "../../../docs/fault-proofs/size-plans/script-integrity-hash-missing-v1-fit-ledger.json",
            import.meta.url,
          ),
        ),
        ledger,
      );
    }
    coverage.scenario("maximum_supported_evidence");
  }, 1_200_000);

  it("refuses an honest accepted block and every substituted accepted seam on chain", async () => {
    // Effectful (a PlutusV3 witness) with a genuine integrity hash: the
    // canonical rule finds no fault, so no acceptance thread may close.
    const plutus = plutusScript(7);
    const scriptPreimage = encodeCbor([plutus]);
    const redeemerPreimage = EMPTY_CBOR_LIST;
    const s = await makeScenario({
      nativeTx: nativeTxOf({
        scriptItems: [plutus],
        redeemerItems: [],
        scriptIntegrityHash: Buffer.alloc(32, 1),
        fee: 4_000n,
      }),
    });
    const subject = SDK.acceptedVerdictSubject(s.block.nativeTxId);
    const honest = {
      subject,
      script_integrity_hash: "01".repeat(32),
      contains_non_native_script: true,
      has_redeemers: false,
    };
    const thread = await s.init();
    // Transaction membership: a foreign transactions root cannot bind the
    // header's counted root, whatever proof rides with it.
    await expectOnchainRefusal(() =>
      s.accepted01(thread, {
        ...s.block.txInclusion!,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const bound = await s.accepted01(thread);
    // Subject coordinate: step 02 recomputes the subject from the bound
    // source and refuses a datum naming another transaction.
    await expectOnchainRefusal(() =>
      s.step02(bound, { subject: SDK.acceptedVerdictSubject("99".repeat(32)) }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    // Witness-set anchor: the carried anchor must be the bound one.
    await expectOnchainRefusal(() =>
      s.step02(bound, { subject, anchoredWitnessSetHash: "22".repeat(32) }),
    );
    coverage.seamMutated("witness_set_anchor");
    const anchored = await s.step02(bound, { subject });
    // Compact transaction: another transaction's bytes under the anchored id.
    const foreign = nativeTxOf({
      scriptItems: [plutus],
      redeemerItems: [],
      scriptIntegrityHash: Buffer.alloc(32, 1),
      fee: 5_000n,
    });
    await expectOnchainRefusal(() =>
      s.direct03(anchored, {
        decision: honest,
        compact: encodeMidgardNativeTxCompact(foreign.compact).toString("hex"),
        scriptPreimage,
        redeemerPreimage,
      }),
    );
    coverage.seamMutated("compact_tx");
    // Field preimage: a substituted script field under the anchored witness set.
    await expectOnchainRefusal(() =>
      s.direct03(anchored, {
        decision: honest,
        scriptPreimage: encodeCbor([plutusScript(8)]),
        redeemerPreimage,
      }),
    );
    coverage.seamMutated("field_preimage");
    // Wrong successor: the direct decision may only continue at step 04.
    await expectOnchainRefusal(() =>
      s.direct03(anchored, {
        decision: honest,
        scriptPreimage,
        redeemerPreimage,
        staged: true,
      }),
    );
    const decided = await s.direct03(anchored, {
      decision: honest,
      scriptPreimage,
      redeemerPreimage,
    });
    await expectOnchainRefusal(() => s.step04(decided));
    coverage.scenario("honest_accepted_block_refusal");
    coverage.reason(REASON);
  }, 600_000);

  it("refuses an honest forced rejection, a mutated forced reason, and a substituted forced leaf on chain", async () => {
    // Effectful with a absent integrity hash: the operator's rejection is
    // exactly right, so no wrongful-rejection thread may close.
    const plutus = plutusScript(9);
    const scriptPreimage = encodeCbor([plutus]);
    const redeemerPreimage = EMPTY_CBOR_LIST;
    const s = await makeScenario({
      nativeTx: nativeTxOf({
        scriptItems: [plutus],
        redeemerItems: [],
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        fee: 5_000n,
      }),
      forcedReason: REASON,
    });
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: s.block.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: { tx_order_id: FORCED_ORDER_KEY },
      },
    });
    const subject = SDK.forcedVerdictSubject({
      transactionId: s.block.nativeTxId,
      sourceKey: FORCED_ORDER_KEY,
      rejectionReason: REASON,
    });
    const pending = await s.forced01(await s.init());
    // Reason coordinate: the datum claims another family's reason for the
    // same leaf; the exact-reason bind refuses it.
    await expectOnchainRefusal(() =>
      s.step02(pending, {
        subject: SDK.forcedVerdictSubject({
          transactionId: s.block.nativeTxId,
          sourceKey: FORCED_ORDER_KEY,
          rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
        }),
        forcedMembership: membership,
      }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    // Forced leaf: a leaf carrying another verdict is not in the forced root.
    await expectOnchainRefusal(() =>
      s.step02(pending, {
        subject,
        forcedMembership: {
          ...membership,
          value: { ...membership.value, verdict: "ForcedTxValid" },
        },
      }),
    );
    coverage.seamMutated("forced_leaf");
    const anchored = await s.step02(pending, {
      subject,
      forcedMembership: membership,
    });
    const decided = await s.direct03(anchored, {
      decision: {
        subject,
        script_integrity_hash: ABSENT_HASH,
        contains_non_native_script: true,
        has_redeemers: false,
      },
      scriptPreimage,
      redeemerPreimage,
    });
    await expectOnchainRefusal(() => s.step04(decided));
    coverage.scenario("honest_forced_rejection_refusal");
  }, 600_000);

  it("walks the staged route below the direct item limit, refuses the direct route there, and cancels every staged step", async () => {
    // No script witnesses and one redeemer past the direct limit: the fault
    // holds through `has_redeemers`, and only the staged route can reach it.
    const redeemerItems = Array.from({ length: 65 }, (_, index) =>
      Buffer.from([index]),
    );
    const scriptPreimage = encodeCbor([]);
    const redeemerPreimage = encodeCbor(redeemerItems);
    const s = await makeScenario({
      nativeTx: nativeTxOf({
        scriptItems: [],
        redeemerItems,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        fee: 6_000n,
      }),
    });
    const subject = SDK.acceptedVerdictSubject(s.block.nativeTxId);
    const evidence = prepareScriptIntegrityHashMissingEvidence({
      finding: {
        category: "scriptIntegrityHashMissing",
        headerHash: s.setup.headerHash,
        transactionId: s.block.nativeTxId,
        direction: "wrongfulAcceptance",
        source: "accepted",
        rejectionReason: null,
      },
      subject,
      nativeTxCompactCbor: s.compactCbor,
      witnessSetCompactCbor: s.witnessSetCbor,
      fieldPreimageLengthsCbor: "80",
      scriptWitnessesPreimageCbor: scriptPreimage.toString("hex"),
      redeemersPreimageCbor: redeemerPreimage.toString("hex"),
      scriptIntegrityHash: ABSENT_HASH,
      scriptLanguages: [],
      redeemerCount: 65,
    });
    const artifact = testingOnlyScriptIntegrityHashMissingArtifact({
      detectionId: `script-integrity-hash-missing:accepted:0:${s.block.nativeTxId}`,
      evidence,
      source: {
        header: s.block.header,
        nativeTxCompactCbor: s.compactCbor,
        witnessSetCompactCbor: s.witnessSetCbor,
        acceptedInclusion: s.block.txInclusion!,
      },
    });
    const plan = (fieldIndex: 6 | 8, items: readonly Buffer[]) =>
      planFaultProofFieldOpening({
        fieldIndex,
        anchorTxId: s.block.nativeTxId,
        nativeTxCompactCbor: s.compactCbor,
        witnessSet: s.witnessSet,
        itemCbors: items,
        owner: s.owner,
        publish: true,
        anchorWitnessSetHash: s.witnessSetHash,
        label: `integrity small field ${fieldIndex.toString()}`,
      });
    const scriptPlan = plan(6, []);
    const redeemerPlan = plan(8, redeemerItems);
    expect(scriptPlan.plan.tier).toBe("RawUtxo");
    expect(redeemerPlan.plan.tier).toBe("RawUtxo");
    const publish = (planned: typeof scriptPlan) =>
      publishFaultProofFieldCarriage({
        lucid: s.harness.proverLucid,
        signer: s.harness.proverSigner,
        planned,
        publisherAddress: s.harness.proverSigner.address,
        label: "integrity small carriage",
      });
    await publish(scriptPlan);
    const redeemerPublished = await publish(redeemerPlan);
    const port = createScriptIntegrityHashMissingTransactionPort({
      binding: {
        blueprint: s.harness.realBlueprint,
        deploymentInfo: {},
        network,
        definition: { headerHash: s.setup.headerHash },
        resolvedContracts: { category: s.category },
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as unknown as FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">,
      lucid: s.harness.proverLucid,
      signer: s.harness.proverSigner,
      contracts: s.family,
      references: {
        steps: s.refs as unknown as readonly [
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
        ],
        witnesses: s.harness.witnessReferenceScripts as Required<
          typeof s.harness.witnessReferenceScripts
        >,
        fieldPreimageCertificateMint: s.refs[0]!,
      },
      lease: {
        acquire: async () => ({
          token: "script-integrity-small-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    /** Drives one production action and returns the thread it leaves at `nextIndex`. */
    const actor = async (
      stage: `step_0${3 | 4 | 5 | 6 | 7}`,
      nextIndex: number | null,
      threadOutRef: string,
    ) => {
      const captured = await port.capture({
        action: {
          actionId: `${stage}:${threadOutRef}:${s.setup.fraudulentBlockOutRef}`,
          input: {
            schemaVersion: CURSOR_FAMILY_ACTION,
            category: "scriptIntegrityHashMissing",
            stage,
            ordinal: Number(stage.slice(-1)),
            threadOutRef,
            stateQueueBlockOutRef: s.setup.fraudulentBlockOutRef,
          },
        },
        artifact,
      });
      const txHash = await submitCapturedTransaction(captured.transaction);
      await s.harness.proverLucid.awaitTx(txHash);
      if (nextIndex === null) return txHash;
      const next = (
        await s.harness.proverLucid.utxosAt(
          s.family.steps[nextIndex]!.spendingScriptAddress,
        )
      ).find((utxo) => utxo.txHash === txHash);
      if (next === undefined)
        throw new Error(`production actuator omitted the ${stage} thread`);
      return `${next.txHash}#${next.outputIndex.toString()}`;
    };
    const anchoredThread = async () =>
      s.step02(await s.accepted01(await s.init()), { subject });
    const anchored = await anchoredThread();
    // One past the direct bound: 65 redeemers refuse the direct route on chain.
    await expectOnchainRefusal(() =>
      s.direct03(anchored, {
        decision: {
          subject,
          script_integrity_hash: ABSENT_HASH,
          contains_non_native_script: false,
          has_redeemers: true,
        },
        scriptPreimage,
        redeemerPreimage,
      }),
    );
    // The production actuator selects the staged route: an empty field 6
    // completes its grammar and its walk inside their first batches.
    const grammar = await actor("step_03", 3, anchored);
    const scan = await actor("step_04", 4, grammar);
    const complete = await actor("step_05", 5, scan);
    const started = await actor("step_06", 5, complete);
    const staged = planScriptIntegrityHashMissingStagedWalk({
      transactionId: s.block.nativeTxId,
      scriptWitnessesPreimageCbor: scriptPreimage.toString("hex"),
      redeemersPreimageCbor: redeemerPreimage.toString("hex"),
    });
    const [first, second] = staged.redeemerGrammar;
    const redeemerOpening = faultProofFieldOpening({
      planned: redeemerPlan,
      referenceInputs: [...redeemerPublished, s.refs[5]!],
      certificatePolicyId: s.family.fieldPreimageCertificatePolicyId,
      label: "integrity small redeemer opening",
    });
    const resume = (checkpointBytes: Buffer, budget: bigint) =>
      submitScriptIntegrityHashMissingRedeemerGrammar({
        ...s.common(5),
        threadOutRef: started,
        authenticatedCarriageUtxos: redeemerPublished,
        closes: false,
        nextDatum: s.datum(5, {
          subject,
          witness_set_hash: s.witnessSetHash,
          script_integrity_hash: ABSENT_HASH,
          phase: {
            RedeemerGrammar: {
              checkpoint_hash: hashScriptIntegrityField8Checkpoint(second!),
              contains_non_native_script: false,
            },
          },
        }),
        buildArgs: ({ input_index, output_index }) => ({
          Resume: {
            input_index,
            output_index,
            opening: redeemerOpening,
            checkpoint_bytes: checkpointBytes.toString("hex"),
            item_budget: budget,
          },
        }),
      });
    // Consensus bound: one item past `staged_batch_limit` is refused.
    await expectOnchainRefusal(() =>
      resume(encodeScriptIntegrityField8Checkpoint(first!), 33n),
    );
    coverage.adjacentOverBoundRefused();
    // Checkpoint: a later position under the committed hash, then malformed bytes.
    await expectOnchainRefusal(() =>
      resume(encodeScriptIntegrityField8Checkpoint(second!), 24n),
    );
    await expectOnchainRefusal(() =>
      resume(
        encodeScriptIntegrityField8Checkpoint(first!).subarray(0, 40),
        24n,
      ),
    );
    coverage.seamMutated("checkpoint");
    const resumedOnce = await actor("step_06", 5, started);
    const resumedTwice = await actor("step_06", 5, resumedOnce);
    const decided = await actor("step_06", 6, resumedTwice);
    const minted = await actor("step_07", null, decided);
    expect(minted).toMatch(/^[0-9a-f]{64}$/u);
    coverage.reason(REASON, "accepted_invalid");
    // Cancel from every staged physical step.
    const atGrammar = await actor("step_03", 3, await anchoredThread());
    await s.cancel(atGrammar, 3);
    coverage.cancelled("script-grammar");
    const atScan = await actor(
      "step_04",
      4,
      await actor("step_03", 3, await anchoredThread()),
    );
    await s.cancel(atScan, 4);
    coverage.cancelled("script-scan");
    const atRedeemer = await actor(
      "step_06",
      5,
      await actor(
        "step_05",
        5,
        await actor(
          "step_04",
          4,
          await actor("step_03", 3, await anchoredThread()),
        ),
      ),
    );
    await s.cancel(atRedeemer, 5);
    coverage.cancelled("redeemer-grammar");
  }, 600_000);

  it("declares the complete lifecycle coverage it exercised", () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
  });
});
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
