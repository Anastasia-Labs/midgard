import "node:crypto";
import "node:fs/promises";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/field-opening.js";
import "../src/min-ada/forced-artifact.js";
import "../src/min-ada/submit-cancel.js";
import "../src/min-ada/submit-init.js";
import "../src/min-ada/submit-step-01.js";
import "../src/min-ada/submit-step-01-forced.js";
import "../src/min-ada/submit-step-02.js";
import "../src/min-ada/submit-step-03.js";
import "../src/min-ada/submit-step-04.js";
import "../src/min-ada/submit-step-05.js";
import "../src/min-ada/workflow-artifact.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "../src/publish-proof-chunks.js";
import "../src/remove-fraudulent-block.js";
import "../src/transition-trace/phas.js";
import "../src/transition-trace/reconstruct.js";
import "../src/workflow/complete-replay.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/emulator/setup-tx.js";
import "./support/final-catalogue-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./support/synthetic-deep-proof.js";
import "./min-ada-wrongful-rejection-lifecycle.rows.js";
import "./min-ada-wrongful-rejection-lifecycle.setup.js";

import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { admitMinAdaForcedArtifact } from "../src/min-ada/forced-artifact.js";
import { submitMinAdaCancel } from "../src/min-ada/submit-cancel.js";
import { submitMinAdaInit } from "../src/min-ada/submit-init.js";
import { submitMinAdaUtxoStep01 } from "../src/min-ada/submit-step-01.js";
import { submitMinAdaStep01Forced } from "../src/min-ada/submit-step-01-forced.js";
import { submitMinAdaUtxoStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaTxStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaUtxoStep03 } from "../src/min-ada/submit-step-03.js";
import { submitMinAdaUtxoStep04 } from "../src/min-ada/submit-step-04.js";
import { submitMinAdaStep05 } from "../src/min-ada/submit-step-05.js";
import { admitMinAdaWorkflowArtifact } from "../src/min-ada/workflow-artifact.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { publishProofChunks } from "../src/publish-proof-chunks.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { rows } from "./min-ada-wrongful-rejection-lifecycle.rows.js";
import { setup } from "./min-ada-wrongful-rejection-lifecycle.setup.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { submitSecondHeaderTx } from "./support/emulator/setup-tx.js";
import { buildMinAdaPostUtxoEmulatorFixture } from "./support/final-catalogue-emulator.js";
import {
  makeMinAdaEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import { registerChunkedVerifyRewardAccount } from "./support/submit-init-emulator-shared.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

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
      { namedByCaller: true },
    );
});

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
        anchorSourceKind: 1n,
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
              f.source.membership.value.submitted_source
                .witness_set_compact_cbor,
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
  }, 30_000);
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
    // Two real emulator deployments plus transaction evaluation exceed 5 s under load.
  }, 60_000);
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
