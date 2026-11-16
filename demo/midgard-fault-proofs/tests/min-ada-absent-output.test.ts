/**
 * A forced `OutputBelowMinAda` rejection naming an output field 2 does not
 * have (a negative index, or one past its last item) is wrongful: canonical
 * validation applies no minimum-Ada rule there. Step 02 closes a negative
 * index at once and compares any other against the certified walk's count
 * once the field's grammar is complete, writing `MinAdaTxAbsent`; step 03
 * moves it to the terminal and step 04 mints the proof. A claim that a held
 * output is absent is refused at step 02, and both forms convict at the
 * field's byte cap. An index past 2^53, up to either end of the rejection
 * codec's range, convicts the same way.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { submitMinAdaStep01Forced } from "../src/min-ada/submit-step-01-forced.js";
import { submitMinAdaTxStep02 } from "../src/min-ada/submit-step-02.js";
import { submitMinAdaStep03 } from "../src/min-ada/submit-step-03.js";
import { submitMinAdaStep04 } from "../src/min-ada/submit-step-04.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { setup } from "./min-ada-wrongful-rejection-lifecycle.setup.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { submitRawMinAdaAbsentOutputStep02 } from "./support/min-ada-raw.submit-absent-output-step02-raw.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

type Options = NonNullable<Parameters<typeof setup>[0]>;

/** A thread bound through step 01 with field 2's carriage published. */
const boundThread = async (options: Options) => {
  const f = await setup(options);
  const ownOutputs = createOwnOutputJournal();
  const bound = await submitMinAdaStep01Forced({
    ...f.common,
    threadOutRef: f.threadOutRef,
    state: f.state,
    forcedSource: f.source,
    referenceScriptUtxo: f.refs[0]!,
  });
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
    label: "min-ada absent outputs",
  });
  const carriage = await publishFaultProofFieldCarriage({
    ownOutputs,
    lucid: f.h.proverLucid,
    signer: f.h.proverSigner,
    planned,
    publisherAddress: f.h.proverSigner.address,
    label: "min-ada absent outputs",
  });
  // Field 2 past the raw-carriage bound opens through its certificate.
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            ownOutputs,
            lucid: f.h.proverLucid,
            network,
            signer: f.h.proverSigner,
            planned,
            certificatePolicyId:
              f.h.contracts.fieldPreimageCertificate.policyId,
            certificateMintingScript:
              f.h.contracts.fieldPreimageCertificate.mintingScript,
            certificateReferenceScriptUtxo: (
              await publishPlainReferenceScriptUtxo({
                lucid: f.h.funderLucid,
                script: f.h.contracts.fieldPreimageCertificate.mintingScript,
                label: "min-ada absent outputs certificate",
              })
            ).utxo,
            chunkUtxos: carriage,
            compactCbor: f.prepared.nativeTxCompactCbor,
            witnessSetCompactCbor:
              f.source.membership.value.submitted_source
                .witness_set_compact_cbor,
          })
        ).certificateUtxo
      : undefined;
  return {
    f,
    ownOutputs,
    carriage,
    certificateUtxo,
    threadOutRef: bound.nextThreadOutRef,
  };
};

/**
 * Step 02 through the grammar to the absent close, steps 03 and 04, and the
 * block's removal. Returns each stage's capture.
 */
const convict = async ({
  f,
  ownOutputs,
  carriage,
  certificateUtxo,
  threadOutRef,
}: Awaited<ReturnType<typeof boundThread>>) => {
  const closed = await captureEmulatorSubmission(f.h.emulator, () =>
    submitMinAdaTxStep02({
      ownOutputs,
      ...f.common,
      threadOutRef,
      prepared: f.prepared,
      publishedCarriageUtxos: carriage,
      certificateUtxo,
      referenceScriptUtxo: f.refs[1]!,
      yieldReferenceScriptUtxo: f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
    }),
  );
  expect(closed.result.nextStepIndex).toBe(2);
  const authenticated = await captureEmulatorSubmission(f.h.emulator, () =>
    submitMinAdaStep03({
      ...f.common,
      threadOutRef: closed.result.nextThreadOutRef,
      referenceScriptUtxo: f.refs[2]!,
    }),
  );
  const minted = await captureEmulatorSubmission(f.h.emulator, () =>
    submitMinAdaStep04({
      ...f.common,
      threadOutRef: authenticated.result.nextThreadOutRef,
      referenceScriptUtxo: f.refs[3]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    }),
  );
  expect(minted.result.fraudProofUnit).toBeTruthy();
  const removal = await publishRemovalReferenceScripts({
    lucid: f.h.proverLucid,
    contracts: f.h.contracts,
  });
  const now = BigInt(f.h.emulator.now());
  await submitRemoveFraudulentBlock({
    lucid: f.h.proverLucid,
    blueprint: f.h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(f.h.contracts, f.h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
    network,
    signer: f.h.proverSigner,
    fraudCategory: "minAda",
    fraudulentHeaderHash: f.seeded.headerHash,
    requireReferenceScripts: true,
    validFrom: now > 120000n ? now - 120000n : 0n,
    validTo: now + 300000n,
  });
  expect(
    await f.h.proverLucid.utxosAtWithUnit(
      f.h.contracts.stateQueue.spendingScriptAddress,
      f.seeded.stateQueueBlockUnit,
    ),
  ).toHaveLength(0);
  return { closed, authenticated, minted };
};

describe("minAda forced rejection of an absent output", () => {
  it("convicts an output coordinate past field 2's last item", async () => {
    const thread = await boundThread({ prefixCount: 1, namedOutputIndex: 2 });
    expect(thread.f.prepared.outputItemCbors).toHaveLength(2);
    await convict(thread);
  }, 900_000);

  it("convicts a negative output coordinate", async () => {
    const thread = await boundThread({ prefixCount: 1, namedOutputIndex: -1 });
    const { closed } = await convict(thread);
    // No grammar transaction precedes the close.
    expect(closed.measurements).toHaveLength(1);
  }, 900_000);

  it.each([
    { label: "2^53 + 1", outputIndex: 2n ** 53n + 1n },
    { label: "the codec's largest, 2^64 - 1", outputIndex: 2n ** 64n - 1n },
    { label: "the codec's smallest, -2^64", outputIndex: -(2n ** 64n) },
  ])(
    "convicts an output coordinate at $label",
    async ({ outputIndex }) => {
      const thread = await boundThread({
        prefixCount: 1,
        namedOutputIndex: outputIndex,
      });
      expect(thread.f.prepared.badOutputIndex).toBe(outputIndex);
      await convict(thread);
    },
    900_000,
  );

  it("refuses on chain a claim that a held output is absent", async () => {
    const thread = await boundThread({
      underfunded: true,
      prefixCount: 1,
      namedOutputIndex: 1,
    });
    const { f } = thread;
    // The field's grammar completes in one transaction, which stops short of
    // the output's scan.
    const grammar = await submitMinAdaTxStep02({
      ownOutputs: thread.ownOutputs,
      ...f.common,
      threadOutRef: thread.threadOutRef,
      prepared: f.prepared,
      publishedCarriageUtxos: thread.carriage,
      referenceScriptUtxo: f.refs[1]!,
      yieldReferenceScriptUtxo: f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
      awaitConfirmation: false,
      unsafeSkipLocalViolationCheckForTest: true,
    });
    expect(grammar.nextStepIndex).toBe(1);
    await f.h.proverLucid.awaitTx(grammar.txHash);
    await expectOnchainRefusal(
      () =>
        submitRawMinAdaAbsentOutputStep02({
          ...f.common,
          threadOutRef: grammar.nextThreadOutRef,
          nativeTxCompactCbor: f.prepared.nativeTxCompactCbor,
          outputItemCbors: f.prepared.outputItemCbors,
          publishedCarriageUtxos: thread.carriage,
          referenceScriptUtxo: f.refs[1]!,
          yieldReferenceScriptUtxo:
            f.seeded.minAdaYieldReferenceScripts!.tx.utxo,
        }),
      {
        refusedBy: "fraud_proofs/min_ada/step_02_yields",
        check: /^Validator returned false$/u,
      },
    );
  }, 900_000);

  it.each([
    { coordinate: "past the last of 702 outputs", namedOutputIndex: 702 },
    { coordinate: "negative over 702 outputs", namedOutputIndex: -1 },
  ])(
    "convicts a coordinate $coordinate at field 2's byte cap",
    async ({ coordinate, namedOutputIndex }) => {
      const thread = await boundThread({
        prefixCount: 700,
        fieldBytes: MIDGARD_CONSENSUS_LIMITS.maxOutputsPreimageBytes,
        namedOutputIndex,
      });
      expect(thread.f.prepared.outputItemCbors).toHaveLength(702);
      const { closed, authenticated, minted } = await convict(thread);
      for (const measurement of [
        ...closed.measurements,
        authenticated.measurement,
        minted.measurement,
      ]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent output (${coordinate}): step-02 close mem=${closed.measurement.executionMemory.toString()} cpu=${closed.measurement.executionSteps.toString()}; step-03 mem=${authenticated.measurement.executionMemory.toString()} cpu=${authenticated.measurement.executionSteps.toString()}; step-04 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
