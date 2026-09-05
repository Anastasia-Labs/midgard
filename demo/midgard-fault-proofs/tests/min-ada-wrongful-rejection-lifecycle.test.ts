import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
} from "@al-ft/midgard-validation";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  admitMinAdaForcedArtifact,
  MIN_ADA_FORCED_ARTIFACT,
} from "../src/min-ada/forced-artifact.js";
import { submitMinAdaCancel } from "../src/min-ada/submit-cancel.js";
import { submitMinAdaInit } from "../src/min-ada/submit-init.js";
import { submitMinAdaUtxoStep01 } from "../src/min-ada/submit-step-01.js";
import { submitMinAdaStep01Forced } from "../src/min-ada/submit-step-01-forced.js";
import { submitMinAdaUtxoStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaTxStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaUtxoStep03 } from "../src/min-ada/submit-step-03.js";
import { submitMinAdaUtxoStep04 } from "../src/min-ada/submit-step-04.js";
import { submitMinAdaStep05 } from "../src/min-ada/submit-step-05.js";
import {
  admitMinAdaWorkflowArtifact,
  prepareMinAdaWorkflowArtifact,
} from "../src/min-ada/workflow-artifact.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { publishProofChunks } from "../src/publish-proof-chunks.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { MIN_ADA_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import type { HistoricalNativeScriptCorpus } from "../src/workflow/historical-native-script-corpus.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { submitSecondHeaderTx } from "./support/emulator/setup-tx.js";
import { buildMinAdaPostUtxoEmulatorFixture } from "./support/final-catalogue-emulator.js";
import {
  makeMinAdaEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { registerChunkedVerifyRewardAccount } from "./support/submit-init-emulator-shared.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";
const rows: VanRossemFitMeasurement[] = [];
let scenarioSequence = 0;
afterAll(async () => {
  if (process.env.MIN_ADA_FIT_LEDGER_PATH)
    await writeVanRossemFitLedger(
      process.env.MIN_ADA_FIT_LEDGER_PATH,
      buildVanRossemFitLedger({
        category: "minAda",
        blueprintSha256: createHash("sha256")
          .update(await readFile(process.env.MIDGARD_REAL_BLUEPRINT_PATH!))
          .digest("hex"),
        compilerVersion: "aiken v1.1.23+5adf783",
        measurements: rows,
      }),
    );
});
const setup = async ({
  underfunded = false,
  depth = 0,
  wrongReason = false,
  outputBytes = 0,
  fieldBytes = 0,
  above = 0n,
  cancelAt = "",
  assetCount = 0,
  prefixCount = 0,
} = {}) => {
  const scenarioId = scenarioSequence++;
  const h = await makeMinAdaEmulatorHarness();
  const assets = new Map<string, Map<string, bigint>>();
  if (assetCount > 0) {
    const names = new Map<string, bigint>();
    for (let i = 0; i < assetCount; i++) {
      const name =
        i === 0
          ? Buffer.alloc(0)
          : i <= 256
            ? Buffer.from([i - 1])
            : Buffer.from([(i - 257) >> 8, (i - 257) & 255]);
      names.set(name.toString("hex"), i === assetCount - 1 ? 256n : 1n);
    }
    assets.set("44".repeat(28), names);
  }
  let padding = 0;
  const output = (lovelace: bigint) =>
    encodeMidgardTxOutput({
      ...(outputBytes === 0
        ? {}
        : {
            script_ref: {
              language: "PlutusV3" as const,
              scriptBytes: Buffer.alloc(padding),
            },
          }),
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
      value: { lovelace, assets },
    });
  if (outputBytes > 0) {
    while (output(2_000_000n).length < outputBytes) padding++;
    expect(output(2_000_000n).length).toBe(outputBytes);
  }
  let floor = 2_000_000n;
  for (let i = 0; i < 4; i++)
    floor = MIDGARD_COINS_PER_UTXO_BYTE * BigInt(output(floor).length + 160);
  const outputCbor = output(underfunded ? floor - 1n : floor + above);
  const prefix = Array.from({ length: prefixCount }, () =>
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x46)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    }),
  );
  let outputs = [...prefix, outputCbor];
  if (fieldBytes > 0) {
    let pad = 0;
    const sibling = () =>
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x45)]),
        value: { lovelace: 2_000_000n, assets: new Map() },
        script_ref: { language: "PlutusV3", scriptBytes: Buffer.alloc(pad) },
      });
    while (
      encodeMidgardFieldPreimage([sibling(), ...prefix, outputCbor]).length <
      fieldBytes
    )
      pad++;
    outputs = [sibling(), ...prefix, outputCbor];
    expect(encodeMidgardFieldPreimage(outputs).length).toBe(fieldBytes);
  }
  const submitted = materializeMidgardNativeTxFromCanonical(
    makeNativeTx({ spendInputCbors: [], fee: 7n, outputCbors: outputs }),
  );
  const tx = adjudicateMidgardNativeTxFullValidity(submitted, "TxIsInvalid");
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSource(tx);
  const credential = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const fixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120000,
      ) - 1,
  });
  const key = fixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const outputIndex = BigInt(outputs.length - 1);
  const reason = wrongReason
    ? ("FeeBelowMinimum" as const)
    : { OutputBelowMinAda: { output_index: outputIndex } };
  const value = {
    tx_id: txId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason } },
  };
  const keyBytes = Buffer.from(Data.to(key, SDK.OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(value as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  let root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const deep =
    depth > 0
      ? syntheticDeepMembershipProof({
          key: keyBytes,
          value: valueBytes,
          branchLevels: depth,
        })
      : undefined;
  if (deep)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
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
    key,
    value,
    proof: Data.from(
      deep?.proofCbor ?? (await trie.prove(keyBytes)).toCBOR().toString("hex"),
      SDK.Proof,
    ),
  };
  const header = {
    ...fixture.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const seeded = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  const source = { header, membership, direction: 1n };
  const scriptPublications: ReturnType<
    typeof import("./support/emulator/measurement.js").measureCompleteSignedTransaction
  >[] = [];
  const refs = await publishFinalFamilyReferenceScripts({
    lucid: h.proverLucid,
    family: h.family,
    label: "min-ada",
    onPublication: (_index, publication) =>
      scriptPublications.push(publication.publicationMeasurement),
  });

  const state = {
    grammar_checkpoint_hash: "",
    grammar_complete: false,
    walk_checkpoint_hash: "",
    direction: 1n,
    bad_tx_id: txId,
    fault: { MinAdaTx: { output_index: outputIndex } },
    post_utxo: null,
  };
  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: Number(outputIndex),
    outputCbor,
  });
  if (assetCount === 1304)
    expect(material.descriptor.cardanoValueSize).toBe(5000);
  const prepared = {
    kind: "min-ada-forced" as const,
    headerHash: seeded.headerHash,
    badTxId: txId,
    badOutputIndex: outputIndex,
    nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
    nativeTxCanonicalCbor:
      encodeMidgardNativeTxCanonical(submitted).toString("hex"),
    outputItemCbors: outputs.map((item) => item.toString("hex")),
    descriptorCbor: material.descriptorCbor.toString("hex"),
    fault: state.fault,
    subject: SDK.forcedVerdictSubject({
      transactionId: txId,
      sourceKey: key,
      rejectionReason: reason,
    }),
    state,
  };
  const artifact = {
    schemaVersion: MIN_ADA_FORCED_ARTIFACT,
    headerHash: seeded.headerHash,
    forcedIndex: 0,
    detectionId: `min-ada:forced:0:${txId}:${outputIndex}`,
    forcedSourceCbor: Data.to(
      source as never,
      SDK.MinAdaForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor:
      encodeMidgardNativeTxCanonical(submitted).toString("hex"),
  };
  const common = {
    lucid: h.proverLucid,
    contracts: h.family,
    categoryId: h.category.categoryId,
    signer: h.proverSigner,
  };
  const shape = `scenario-${scenarioId}-assets-${assetCount}-cancel-${cancelAt}-output-${outputCbor.length}-field-${encodeMidgardFieldPreimage(outputs).length}-mpf-${depth}-above-${above}`;
  let measurementSequence = 0;
  const record = (
    name: string,
    kind: "publication" | "lifecycle",
    measurements: readonly CompleteSignedTransactionMeasurement[],
  ) =>
    measurements.forEach((m, i) =>
      rows.push({
        name: `${shape}/${name}/${measurementSequence++}/${i}`,
        kind,
        maximumShape: shape,
        signedBytes: m.completeSignedBytes,
        memoryUnits: m.executionMemory,
        cpuUnits: m.executionSteps,
      }),
    );
  record("family-publications", "publication", scriptPublications);
  record("yield-publications", "publication", [
    seeded.minAdaYieldReferenceScripts!.tx.publicationMeasurement,
    seeded.minAdaYieldReferenceScripts!.utxo.publicationMeasurement,
  ]);
  const capture = async <T>(
    name: string,
    operation: () => Promise<T>,
    kind: "publication" | "lifecycle" = "lifecycle",
  ) => {
    const captured = await captureEmulatorSubmission(
      h.emulator,
      operation,
    ).catch((error) => {
      console.error("MIN_ADA_PHASE", name, shape);
      throw error;
    });
    record(name, kind, captured.measurements);
    return captured.result;
  };
  const initialize = () =>
    capture("init", () =>
      submitMinAdaInit({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
        network,
        signer: h.proverSigner,
        fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
  const init = await initialize();
  const prepareArtifact = async () => {
    const entry = {
      key,
      value,
      keyBytes,
      valueBytes,
      fullTransactionCbor: encodeMidgardNativeTxCanonical(submitted),
    };
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
    const fingerprint = eventKeyFingerprint(eventKey);
    const block = {
      headerHash: seeded.headerHash,
      header,
      transactions: [],
      reconstruction: {
        utxos: [],
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
    const replay = await MIN_ADA_COMPLETE_CANONICAL_REPLAY.replay(block);
    const selected = replay.detections.find(
      (d) => d.detectionId === artifact.detectionId,
    );
    if (!selected)
      throw new Error("installed complete replay missed forced minAda");
    if (depth > 0) return artifact;
    return prepareMinAdaWorkflowArtifact({
      evidence: block,
      historicalNativeScriptCorpus: {} as HistoricalNativeScriptCorpus,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: block.headerHash,
        category: "minAda",
        selected,
        detections: replay.detections,
        unprovableGaps: [],
      },
    });
  };
  return {
    h,
    common,
    capture,
    initialize,
    prepareArtifact,
    artifact,
    prepared,
    state,
    source,
    refs,
    seeded,
    threadOutRef: `${init.txHash}#${init.firstStepOutputIndex}`,
  };
};
describe("minimum-Ada forced rejection", () => {
  it.each([
    {},
    { above: 1n },
    {
      assetCount: 1304,
      outputBytes: 16384,
      fieldBytes: 32768,
      depth: 64,
      cancelAt: "scan",
    },
    { prefixCount: 700, fieldBytes: 32768, depth: 64 },
    { outputBytes: 16384, depth: 64 },
    { outputBytes: 16384, fieldBytes: 32768, depth: 64 },
    { outputBytes: 16384, fieldBytes: 32768, depth: 64, cancelAt: "opened" },
  ])(
    "authenticates exact output through registered mint and removal assets=$assetCount prefix=$prefixCount output=$outputBytes depth=$depth",
    async (options) => {
      const f = await setup(options);
      const workflowArtifact = await f.prepareArtifact();
      await admitMinAdaWorkflowArtifact(
        JSON.parse(JSON.stringify(workflowArtifact)),
      );
      const reopened = await admitMinAdaForcedArtifact(
        JSON.parse(JSON.stringify(workflowArtifact)),
      );
      const bound = await f.capture("bind", () =>
        submitMinAdaStep01Forced({
          ...f.common,
          threadOutRef: f.threadOutRef,
          state: reopened.evidence.state,
          forcedSource: reopened.forcedSource,
          referenceScriptUtxo: f.refs[0]!,
        }),
      );
      const planned = planFaultProofFieldOpening({
        fieldIndex: 2,
        anchorTxId: f.prepared.badTxId,
        nativeTxCompactCbor: f.prepared.nativeTxCompactCbor,
        itemCbors: f.prepared.outputItemCbors.map((item) =>
          Buffer.from(item, "hex"),
        ),
        owner: f.h.proverSigner.paymentKeyHash,
        publish: true,
        label: "min-ada outputs",
      });
      const carriage = await f.capture(
        "field-publications",
        () =>
          publishFaultProofFieldCarriage({
            lucid: f.h.proverLucid,
            signer: f.h.proverSigner,
            planned,
            publisherAddress: f.h.proverSigner.address,
            label: "min-ada outputs",
          }),
        "publication",
      );
      let certificateUtxo: import("@lucid-evolution/lucid").UTxO | undefined;
      if (planned.plan.tier === "Certified") {
        const ref = await f.capture(
          "certificate-publication",
          () =>
            publishPlainReferenceScriptUtxo({
              lucid: f.h.funderLucid,
              script: f.h.contracts.fieldPreimageCertificate.mintingScript,
              label: "min-ada field certificate",
            }),
          "publication",
        );
        const certified = await f.capture("certificate", () =>
          certifyFaultProofFieldCarriage({
            lucid: f.h.proverLucid,
            network,
            signer: f.h.proverSigner,
            planned,
            certificatePolicyId:
              f.h.contracts.fieldPreimageCertificate.policyId,
            certificateMintingScript:
              f.h.contracts.fieldPreimageCertificate.mintingScript,
            certificateReferenceScriptUtxo: ref.utxo,
            chunkUtxos: carriage,
            compactCbor: f.prepared.nativeTxCompactCbor,
            witnessSetCompactCbor:
              f.source.membership.value.source.witness_set_compact_cbor,
          }),
        );
        certificateUtxo = certified.certificateUtxo;
      }
      const opened = await f.capture("open", () =>
        submitMinAdaTxStep02({
          ...f.common,
          threadOutRef: bound.nextThreadOutRef,
          prepared: f.prepared,
          publishedCarriageUtxos: carriage,
          certificateUtxo,
          referenceScriptUtxo: f.refs[1]!,
          yieldReferenceScriptUtxo:
            f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
        }),
      );
      let predicateThread = opened.nextThreadOutRef;
      if (options.cancelAt === "opened" || options.cancelAt === "scan") {
        if (options.cancelAt === "scan") {
          const pause = new Error("simulated restart after scan submission");
          await f.capture("scan-checkpoint", async () => {
            try {
              await submitMinAdaUtxoStep03({
                ...f.common,
                threadOutRef: predicateThread,
                outputItemCbors: f.prepared.outputItemCbors,
                coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
                referenceScriptUtxo: f.refs[2]!,
                preSubmitBoundary: async ({ signed }) => {
                  await f.h.proverLucid.awaitTx(await signed.submit());
                  throw pause;
                },
              });
            } catch (error) {
              if (error !== pause) throw error;
            }
          });
          const live = await f.h.proverLucid.utxosAt(
            f.h.family.steps[2].spendingScriptAddress,
          );
          expect(live).toHaveLength(1);
          predicateThread = `${live[0]!.txHash}#${live[0]!.outputIndex}`;
        }
        await f.capture("cancel", () =>
          submitMinAdaCancel({
            ...f.common,
            threadOutRef: predicateThread,
            referenceScriptUtxo: f.refs[2]!,
            witnessReferenceScripts: f.h.witnessReferenceScripts,
          }),
        );
        const restart = await f.initialize();
        const retained = await admitMinAdaForcedArtifact(
          JSON.parse(JSON.stringify(f.artifact)),
        );
        const rebound = await f.capture("restart-bind", () =>
          submitMinAdaStep01Forced({
            ...f.common,
            threadOutRef: `${restart.txHash}#${restart.firstStepOutputIndex}`,
            state: retained.evidence.state,
            forcedSource: retained.forcedSource,
            referenceScriptUtxo: f.refs[0]!,
          }),
        );
        const reopened = await f.capture("restart-open", () =>
          submitMinAdaTxStep02({
            ...f.common,
            threadOutRef: rebound.nextThreadOutRef,
            prepared: retained.evidence,
            publishedCarriageUtxos: carriage,
            certificateUtxo,
            referenceScriptUtxo: f.refs[1]!,
            yieldReferenceScriptUtxo:
              f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
          }),
        );
        predicateThread = reopened.nextThreadOutRef;
      }
      const checked = await f.capture("predicate", () =>
        submitMinAdaUtxoStep03({
          ...f.common,
          threadOutRef: predicateThread,
          outputItemCbors: f.prepared.outputItemCbors,
          coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
          referenceScriptUtxo: f.refs[2]!,
        }),
      );
      const proof = await f.capture("mint", () =>
        submitMinAdaStep05({
          ...f.common,
          threadOutRef: checked.nextThreadOutRef,
          referenceScriptUtxo: f.refs[4]!,
          witnessReferenceScripts: f.h.witnessReferenceScripts,
        }),
      );
      expect(proof.txHash).toHaveLength(64);
      const removal = await f.capture(
        "removal-publications",
        () =>
          publishRemovalReferenceScripts({
            lucid: f.h.proverLucid,
            contracts: f.h.contracts,
          }),
        "publication",
      );
      const now = BigInt(f.h.emulator.now());
      await f.capture("remove", () =>
        submitRemoveFraudulentBlock({
          lucid: f.h.proverLucid,
          blueprint: f.h.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            f.h.contracts,
            f.h.catalogue,
            { removalReferenceScripts: removal.published },
          ),
          network,
          signer: f.h.proverSigner,
          fraudCategory: "minAda",
          fraudulentHeaderHash: f.seeded.headerHash,
          requireReferenceScripts: true,
          validFrom: now > 120000n ? now - 120000n : 0n,
          validTo: now + 300000n,
        }),
      );
      expect(
        await f.h.proverLucid.utxosAtWithUnit(
          f.h.contracts.stateQueue.spendingScriptAddress,
          f.seeded.stateQueueBlockUnit,
        ),
      ).toHaveLength(0);
      expect(
        await f.h.proverLucid.utxosAtWithUnit(
          f.h.family.fraudProof.spendingScriptAddress,
          proof.fraudProofUnit,
        ),
      ).toHaveLength(1);
    },
    900000,
  );
  it("refuses an honest underfunded output on chain", async () => {
    const f = await setup({ underfunded: true });
    await expect(admitMinAdaForcedArtifact(f.artifact)).rejects.toThrow(
      "no contradiction",
    );
    const bound = await submitMinAdaStep01Forced({
      ...f.common,
      threadOutRef: f.threadOutRef,
      state: f.state,
      forcedSource: f.source,
      referenceScriptUtxo: f.refs[0]!,
    });
    const opened = await submitMinAdaTxStep02({
      ...f.common,
      threadOutRef: bound.nextThreadOutRef,
      prepared: f.prepared,
      referenceScriptUtxo: f.refs[1]!,
      yieldReferenceScriptUtxo: f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
      unsafeSkipLocalViolationCheckForTest: true,
    });
    await expect(
      submitMinAdaUtxoStep03({
        ...f.common,
        threadOutRef: opened.nextThreadOutRef,
        outputItemCbors: f.prepared.outputItemCbors,
        coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
        referenceScriptUtxo: f.refs[2]!,
        unsafeSkipLocalViolationCheckForTest: true,
      }),
    ).rejects.toThrow();
  });
  it("rejects authenticated unrelated reason and source/index/direction substitutions", async () => {
    const unrelated = await setup({ wrongReason: true });
    await expect(
      submitMinAdaStep01Forced({
        ...unrelated.common,
        threadOutRef: unrelated.threadOutRef,
        state: unrelated.state,
        forcedSource: unrelated.source,
        referenceScriptUtxo: unrelated.refs[0]!,
      }),
    ).rejects.toThrow();
    const f = await setup();
    for (const mutation of [
      { state: { ...f.state, fault: { MinAdaTx: { output_index: 1n } } } },
      { forcedSource: { ...f.source, direction: 0n } },
      {
        forcedSource: {
          ...f.source,
          header: {
            ...f.source.header,
            forcedTransactionsRoot: "ff".repeat(32),
          },
        },
      },
    ])
      await expect(
        submitMinAdaStep01Forced({
          ...f.common,
          threadOutRef: f.threadOutRef,
          state: f.state,
          forcedSource: f.source,
          referenceScriptUtxo: f.refs[0]!,
          ...mutation,
        }),
      ).rejects.toThrow();
    for (const mutation of [
      { fullTransactionCbor: f.artifact.fullTransactionCbor + "00" },
      { forcedSourceCbor: f.artifact.forcedSourceCbor + "00" },
      { headerHash: "ff".repeat(28) },
      { detectionId: "min-ada:forced:1:wrong" },
    ])
      await expect(
        admitMinAdaForcedArtifact({ ...f.artifact, ...mutation }),
      ).rejects.toThrow();
  });
  it("retains maximum post-UTxO descriptor and both 64-node proof carriages through removal", async () => {
    const h = await makeMinAdaEmulatorHarness();
    let seq = 0;
    const shape = "maximum-post-utxo-descriptor-and-64-node-roots";
    const record = (
      name: string,
      kind: "publication" | "lifecycle",
      measurements: readonly CompleteSignedTransactionMeasurement[],
    ) =>
      measurements.forEach((m) =>
        rows.push({
          name: `${shape}/${name}/${seq++}`,
          kind,
          maximumShape: shape,
          signedBytes: m.completeSignedBytes,
          memoryUnits: m.executionMemory,
          cpuUnits: m.executionSteps,
        }),
      );
    const capture = async <T>(
      name: string,
      run: () => Promise<T>,
      kind: "publication" | "lifecycle" = "lifecycle",
    ) => {
      const result = await captureEmulatorSubmission(h.emulator, run).catch(
        (error) => {
          console.error("POST_PHASE", name);
          console.dir(error, { depth: 8 });
          throw error;
        },
      );
      record(name, kind, result.measurements);
      return result.result;
    };
    await capture("chunked-reward-registration", () =>
      registerChunkedVerifyRewardAccount(h.proverLucid, h.realBlueprint),
    );
    const refs = await capture(
      "family-publications",
      () =>
        publishFinalFamilyReferenceScripts({
          lucid: h.proverLucid,
          family: h.family,
          label: "min-ada max-utxo",
        }),
      "publication",
    );
    const base = await buildMinAdaPostUtxoEmulatorFixture({
      emptyPrevious: true,
    });
    let padding = 0;
    const output = () =>
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0]), Buffer.alloc(56, 0x55)]),
        value: { lovelace: 0n, assets: new Map() },
        datum: {
          kind: "inline",
          cbor: Buffer.from(Data.to("aa".repeat(4000)), "hex"),
        },
        script_ref: {
          language: "PlutusV3",
          scriptBytes: Buffer.alloc(padding),
        },
      });
    while (output().length < 16384) padding++;
    expect(output().length).toBe(16384);
    const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: output(),
    }).descriptorCbor;
    const key = Buffer.from(base.prepared.outRefKeyCbor, "hex");
    const deep = syntheticDeepMembershipProof({
      key,
      value: descriptor,
      branchLevels: 64,
    });
    const proof = Data.from(deep.proofCbor, SDK.Proof);
    const json = proof.map((step) => {
      if (!("Branch" in step)) throw new Error("expected branch");
      return {
        type: "branch",
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    });
    const priorRoot = MpfProof.fromJSON(key, descriptor, json)
      .verify(false)!
      .toString("hex");
    const preparedBase = {
      ...base.prepared,
      descriptorCbor: descriptor.toString("hex"),
      postUtxosRoot: deep.transactionsPhasRoot,
      prevUtxosRoot: priorRoot,
      postMembershipProof: proof,
      postMembershipProofCbor: deep.proofCbor,
      predecessorNonMembershipProof: proof,
      predecessorNonMembershipProofCbor: deep.proofCbor,
    };
    const first = await setupFraudulentBlock({
      funderLucid: h.funderLucid,
      emulator: h.emulator,
      contracts: h.contracts,
      catalogue: h.catalogue,
      fixture: { ...base, utxosRoot: priorRoot, headerDurationMs: 120000 },
    });
    const nextHeader = {
      ...first.header,
      prevHeaderHash: first.headerHash,
      prevUtxosRoot: priorRoot,
      utxosRoot: deep.transactionsPhasRoot,
      startTime: first.header.endTime,
      endTime: first.header.endTime + 120000n,
    };
    h.emulator.awaitSlot(
      Math.max(
        0,
        Math.ceil((Number(nextHeader.startTime) - h.emulator.now()) / 1000) + 1,
      ),
    );
    const second = await submitSecondHeaderTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      header: nextHeader,
    });
    const seeded = {
      ...first,
      fraudulentBlockOutRef: second.blockOutRef,
      headerHash: second.headerHash,
      stateQueueBlockUnit:
        h.contracts.stateQueue.policyId +
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        second.headerHash,
    };
    record("yield-publications", "publication", [
      seeded.minAdaYieldReferenceScripts!.tx.publicationMeasurement,
      seeded.minAdaYieldReferenceScripts!.utxo.publicationMeasurement,
    ]);
    const prepared = { ...preparedBase, headerHash: seeded.headerHash };
    const chunks: import("../src/proof-chunk-carriage.js").PublishedProofChunk[] =
      [];
    for (let i = 0; i < proof.length; i += 16) {
      const published = await capture(
        "proof-publications",
        () =>
          publishProofChunks({
            lucid: h.proverLucid,
            network,
            signer: h.proverSigner,
            proofCbor: Data.to(proof.slice(i, i + 16), SDK.Proof),
          }),
        "publication",
      );
      chunks.push(...published.chunks);
    }
    const common = {
      lucid: h.proverLucid,
      contracts: h.family,
      categoryId: h.category.categoryId,
      signer: h.proverSigner,
    };
    const initial = await capture("init", () =>
      submitMinAdaInit({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
        network,
        signer: h.proverSigner,
        fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
    const bound = await capture("bind", () =>
      submitMinAdaUtxoStep01({
        ...common,
        network,
        threadOutRef: `${initial.txHash}#${initial.firstStepOutputIndex}`,
        stateQueueBlockOutRef: seeded.fraudulentBlockOutRef,
        prepared,
        referenceScriptUtxo: refs[0]!,
      }),
    );
    const opened = await capture("membership", () =>
      submitMinAdaUtxoStep02({
        ...common,
        blueprint: h.realBlueprint,
        network,
        threadOutRef: bound.nextThreadOutRef,
        prepared,
        publishedProofChunks: chunks,
        referenceScriptUtxo: refs[1]!,
        yieldReferenceScriptUtxo: seeded.minAdaYieldReferenceScripts!.utxo.utxo,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
    const checked = await capture("predicate", () =>
      submitMinAdaUtxoStep03({
        ...common,
        threadOutRef: opened.nextThreadOutRef,
        coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
        referenceScriptUtxo: refs[2]!,
      }),
    );
    const culpable = await capture("predecessor", () =>
      submitMinAdaUtxoStep04({
        ...common,
        blueprint: h.realBlueprint,
        network,
        threadOutRef: checked.nextThreadOutRef,
        predecessorNonMembershipProofCbor: deep.proofCbor,
        publishedProofChunks: chunks,
        referenceScriptUtxo: refs[3]!,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
    const permanent = await capture("mint", () =>
      submitMinAdaStep05({
        ...common,
        threadOutRef: culpable.nextThreadOutRef,
        referenceScriptUtxo: refs[4]!,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
    const removal = await capture(
      "removal-publications",
      () =>
        publishRemovalReferenceScripts({
          lucid: h.proverLucid,
          contracts: h.contracts,
        }),
      "publication",
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
        fraudCategory: "minAda",
        fraudulentHeaderHash: seeded.headerHash,
        requireReferenceScripts: true,
        validFrom: now > 120000n ? now - 120000n : 0n,
        validTo: now + 300000n,
      }),
    );
    expect(
      await h.proverLucid.utxosAtWithUnit(
        h.contracts.stateQueue.spendingScriptAddress,
        seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(0);
    expect(
      await h.proverLucid.utxosAtWithUnit(
        h.family.fraudProof.spendingScriptAddress,
        permanent.fraudProofUnit,
      ),
    ).toHaveLength(1);
  }, 900000);
});
