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
import "../src/min-ada/workflow-artifact.js";
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
import "./min-ada-wrongful-rejection-lifecycle.setup.js";

import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { admitMinAdaForcedArtifact } from "../src/min-ada/forced-artifact.js";
import { submitMinAdaCancel } from "../src/min-ada/submit-cancel.js";
import { submitMinAdaStep01Forced } from "../src/min-ada/submit-step-01-forced.js";
import { submitMinAdaTxStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaStep03 } from "../src/min-ada/submit-step-03.js";
import { submitMinAdaStep04 } from "../src/min-ada/submit-step-04.js";
import { admitMinAdaWorkflowArtifact } from "../src/min-ada/workflow-artifact.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { setup } from "./min-ada-wrongful-rejection-lifecycle.setup.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";

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
      const carriage = await f.capture("field-publications", () =>
        publishFaultProofFieldCarriage({
          lucid: f.h.proverLucid,
          signer: f.h.proverSigner,
          planned,
          publisherAddress: f.h.proverSigner.address,
          label: "min-ada outputs",
        }),
      );
      let certificateUtxo: import("@lucid-evolution/lucid").UTxO | undefined;
      if (planned.plan.tier === "Certified") {
        const ref = await f.capture("certificate-publication", () =>
          publishPlainReferenceScriptUtxo({
            lucid: f.h.funderLucid,
            script: f.h.contracts.fieldPreimageCertificate.mintingScript,
            label: "min-ada field certificate",
          }),
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
              await submitMinAdaStep03({
                ...f.common,
                threadOutRef: predicateThread,
                outputItemCbors: f.prepared.outputItemCbors,
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
        submitMinAdaStep03({
          ...f.common,
          threadOutRef: predicateThread,
          outputItemCbors: f.prepared.outputItemCbors,
          referenceScriptUtxo: f.refs[2]!,
        }),
      );
      const proof = await f.capture("mint", () =>
        submitMinAdaStep04({
          ...f.common,
          threadOutRef: checked.nextThreadOutRef,
          referenceScriptUtxo: f.refs[3]!,
          witnessReferenceScripts: f.h.witnessReferenceScripts,
        }),
      );
      expect(proof.txHash).toHaveLength(64);
      const removal = await f.capture("removal-publications", () =>
        publishRemovalReferenceScripts({
          lucid: f.h.proverLucid,
          contracts: f.h.contracts,
        }),
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
      submitMinAdaStep03({
        ...f.common,
        threadOutRef: opened.nextThreadOutRef,
        outputItemCbors: f.prepared.outputItemCbors,
        referenceScriptUtxo: f.refs[2]!,
        unsafeSkipLocalViolationCheckForTest: true,
      }),
    ).rejects.toThrow(/failed script execution Spend\[\d+\]/u);
  }, 30_000);
  it.each([undefined, "00", "1800", "ff"])(
    "convicts an accepted forced output below the floor through mint and removal with datum %s",
    async (datumHex) => {
      const f = await setup({ accepted: true, underfunded: true, datumHex });
      // The production detector and artifact admission derive the accepted
      // direction from the committed verdict.
      const workflowArtifact = await f.prepareArtifact();
      const reopened = await admitMinAdaForcedArtifact(
        JSON.parse(JSON.stringify(workflowArtifact)),
      );
      expect(reopened.evidence.state.direction).toBe(0n);
      expect(reopened.forcedSource.direction).toBe(0n);
      expect(reopened.evidence.subject).toStrictEqual(f.prepared.subject);
      const bound = await f.capture("bind", () =>
        submitMinAdaStep01Forced({
          ...f.common,
          threadOutRef: f.threadOutRef,
          state: reopened.evidence.state,
          forcedSource: reopened.forcedSource,
          referenceScriptUtxo: f.refs[0]!,
        }),
      );
      const opened = await f.capture("open", () =>
        submitMinAdaTxStep02({
          ...f.common,
          threadOutRef: bound.nextThreadOutRef,
          prepared: reopened.evidence,
          referenceScriptUtxo: f.refs[1]!,
          yieldReferenceScriptUtxo:
            f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
        }),
      );
      const checked = await f.capture("predicate", () =>
        submitMinAdaStep03({
          ...f.common,
          threadOutRef: opened.nextThreadOutRef,
          outputItemCbors: f.prepared.outputItemCbors,
          referenceScriptUtxo: f.refs[2]!,
        }),
      );
      const proof = await f.capture("mint", () =>
        submitMinAdaStep04({
          ...f.common,
          threadOutRef: checked.nextThreadOutRef,
          referenceScriptUtxo: f.refs[3]!,
          witnessReferenceScripts: f.h.witnessReferenceScripts,
        }),
      );
      const removal = await f.capture("removal-publications", () =>
        publishRemovalReferenceScripts({
          lucid: f.h.proverLucid,
          contracts: f.h.contracts,
        }),
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
    300_000,
  );
  it.each([undefined, "00", "1800", "ff"])(
    "refuses an honest accepted forced output at the predicate with datum %s",
    async (datumHex) => {
      const f = await setup({ accepted: true, datumHex });
      // Off chain the detector finds nothing to argue.
      await expect(f.prepareArtifact()).rejects.toThrow();
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
      // The local guard is skipped, so the refusal comes from the step-03
      // predicate evaluation itself.
      const refused = await submitMinAdaStep03({
        ...f.common,
        threadOutRef: opened.nextThreadOutRef,
        outputItemCbors: f.prepared.outputItemCbors,
        referenceScriptUtxo: f.refs[2]!,
        unsafeSkipLocalViolationCheckForTest: true,
      }).then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(refused).toBeDefined();
      expect(String(refused)).toMatch(/failed script execution Spend\[\d+\]/u);
    },
    60_000,
  );
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
      { state: { ...f.state, fault: { output_index: 1n } } },
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
});
