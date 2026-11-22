import { type UTxO } from "@lucid-evolution/lucid";

import { buildCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import type { FaultProofFieldGrammarDeployment } from "../../src/field-grammar-certificate.js";
import { planFaultProofFieldOpening } from "../../src/field-opening.plan-fault-proof-field-opening.js";
import type { OutputReferenceScriptDecodingEvidence } from "../../src/output-reference-script-decoding/output-reference-script-decoding.js";
import { buildOutputReferenceScriptDecodingCancel } from "../../src/output-reference-script-decoding/submit-cancel.js";
import { buildOutputReferenceScriptDecodingStep01Accepted } from "../../src/output-reference-script-decoding/submit-step-01-accepted.js";
import {
  buildOutputReferenceScriptDecodingStep01Forced,
  buildOutputReferenceScriptDecodingStep01ForcedRaw,
} from "../../src/output-reference-script-decoding/submit-step-01-forced.js";
import {
  buildOutputReferenceScriptDecodingStep02,
  buildOutputReferenceScriptDecodingStep02Raw,
} from "../../src/output-reference-script-decoding/submit-step-02.js";
import { buildOutputReferenceScriptDecodingStep03 } from "../../src/output-reference-script-decoding/submit-step-03.js";
import { buildOutputReferenceScriptDecodingStep04 } from "../../src/output-reference-script-decoding/submit-step-04.js";
import {
  buildOutputReferenceScriptDecodingStep05,
  buildOutputReferenceScriptDecodingStep05Raw,
  type OutputReferenceScriptScanArgs,
  type OutputReferenceScriptScanState,
} from "../../src/output-reference-script-decoding/submit-step-05.js";
import {
  buildOutputReferenceScriptDecodingStep06,
  buildOutputReferenceScriptDecodingStep06Raw,
} from "../../src/output-reference-script-decoding/submit-step-06.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { type SubmitStep01TxInclusion } from "../../src/step-support.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { publishRemovalReferenceScripts } from "./emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.build-removal-deployment-info.js";
import { submitSetupTx } from "./emulator/setup-tx.submit-setup-tx.js";
import {
  type AcceptedSubject,
  type Measurement,
  network,
  type OutputReferenceContext,
} from "./output-reference-script-decoding-emulator.commit-accepted-block.js";
import { type ForcedLeaf } from "./output-reference-script-decoding-emulator.commit-forced-block.js";
import { recordPlutusItem } from "./script-decode-emulator.js";
import {
  buildAndSubmitUnjournaled,
  certifyCarriageUnjournaled,
  publishCarriageUnjournaled,
  submitTurnsUnjournaled,
} from "./submit-unjournaled.js";

export const makeOutputReferenceStages = ({
  context,
  fraudulentBlockOutRef,
  references,
  certificateReference,
}: {
  readonly context: OutputReferenceContext;
  readonly fraudulentBlockOutRef: string;
  readonly references: readonly UTxO[];
  readonly certificateReference: UTxO;
}) => {
  const { harness, contracts, catalogue, category } = context;
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const captured = <T>(operation: () => Promise<T>) =>
    captureEmulatorSubmission(harness.emulator, operation);

  const init = async () =>
    await captured(() =>
      buildAndSubmitUnjournaled(buildCommittedFieldShapeInit, {
        lucid,
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
        signer,
        fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const threadOf = (
    initialized: Awaited<ReturnType<typeof init>>["result"],
  ): string =>
    `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
  const threadUtxoOf = async (threadOutRef: string): Promise<UTxO> => {
    const [txHash, outputIndex] = threadOutRef.split("#");
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo === undefined) throw new Error("thread absent");
    return utxo;
  };

  const step01Accepted = async (
    initialized: Awaited<ReturnType<typeof init>>["result"],
    finding: {
      readonly subject: OutputReferenceScriptDecodingEvidence["subject"];
      readonly outputIndex: bigint;
    },
    txInclusion: SubmitStep01TxInclusion,
  ) =>
    await captured(async () =>
      buildAndSubmitUnjournaled(
        buildOutputReferenceScriptDecodingStep01Accepted,
        {
          lucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer,
          finding,
          threadUtxo: await threadUtxoOf(threadOf(initialized)),
          threadToken: {
            unit: initialized.computationThreadUnit,
            fraudulentHeaderHash: initialized.fraudulentHeaderHash,
          },
          stateQueueBlockOutRef: fraudulentBlockOutRef,
          txInclusion,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        },
      ),
    );
  const step01Forced = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    leaf: ForcedLeaf,
    header: Parameters<typeof submitSetupTx>[0]["header"],
  ) =>
    await captured(() =>
      buildAndSubmitUnjournaled(
        buildOutputReferenceScriptDecodingStep01Forced,
        {
          lucid,
          contracts,
          categoryId,
          signer,
          threadOutRef,
          evidence,
          forcedSource: {
            header,
            membership: leaf.membership,
          },
          referenceScriptUtxo: references[0]!,
        },
      ),
    );
  const step01ForcedRaw = async (
    args: Omit<
      Parameters<typeof buildOutputReferenceScriptDecodingStep01ForcedRaw>[0],
      "lucid" | "contracts" | "categoryId" | "signer" | "referenceScriptUtxo"
    >,
  ) =>
    await buildAndSubmitUnjournaled(
      buildOutputReferenceScriptDecodingStep01ForcedRaw,
      {
        lucid,
        contracts,
        categoryId,
        signer,
        referenceScriptUtxo: references[0]!,
        ...args,
      },
    );
  const step02 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    source: {
      readonly compactCborHex: string;
      readonly witnessSetCompactCborHex: string;
    },
    options: {
      readonly publishedCarriageUtxos?: readonly UTxO[];
      readonly certificateUtxo?: UTxO;
      /** The field grammar certificate, for a field past the walked limit. */
      readonly fieldGrammar?: FaultProofFieldGrammarDeployment;
    } = {},
  ) =>
    await captured(() =>
      submitTurnsUnjournaled(buildOutputReferenceScriptDecodingStep02, {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: source.compactCborHex,
        witnessSetCompactCbor: source.witnessSetCompactCborHex,
        publishCarriage: options.publishedCarriageUtxos === undefined,
        ...options,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
  /** Every descriptor window until the output scan closes canonical. */
  const step03Loop = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    onWindow?: (
      window: {
        readonly result: {
          readonly terminal: boolean;
          readonly nextThreadOutRef: string;
        };
        readonly measurement: Measurement;
      },
      index: number,
    ) => void,
  ) => {
    let current = threadOutRef;
    let windows = 0;
    for (;;) {
      const scan = await captured(() =>
        buildAndSubmitUnjournaled(buildOutputReferenceScriptDecodingStep03, {
          lucid,
          contracts,
          categoryId,
          signer,
          threadOutRef: current,
          evidence,
          referenceScriptUtxo: references[2]!,
        }),
      );
      onWindow?.(scan, windows);
      windows += 1;
      current = scan.result.nextThreadOutRef;
      if (scan.result.terminal) break;
    }
    return { threadOutRef: current, windows };
  };
  /**
   * `record` is the decode record step 04 reads: by default the item's own
   * (recorded on first use, none for a native item); `null` reads none.
   */
  const step04 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    compactCborHex: string,
    opened: {
      readonly carriageUtxos: readonly UTxO[];
      readonly certificateUtxo?: UTxO;
    },
    record?: UTxO | null,
  ) => {
    const scriptDecodeRecordUtxo =
      record === undefined
        ? await recordPlutusItem(
            harness.emulator,
            lucid,
            Buffer.from(evidence.referenceScriptItemHex, "hex"),
          )
        : (record ?? undefined);
    return await captured(() =>
      submitTurnsUnjournaled(buildOutputReferenceScriptDecodingStep04, {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: compactCborHex,
        publishedCarriageUtxos: opened.carriageUtxos,
        certificateUtxo: opened.certificateUtxo,
        referenceScriptUtxo: references[3]!,
        scriptDecodeRecordUtxo,
      }),
    );
  };
  const step05 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
  ) =>
    await captured(() =>
      buildAndSubmitUnjournaled(buildOutputReferenceScriptDecodingStep05, {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
      }),
    );
  /** Every scan transaction until the native scan closes. */
  const step05Loop = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    onScan?: (
      captured: Awaited<ReturnType<typeof step05>>,
      index: number,
    ) => void,
  ) => {
    let current = threadOutRef;
    let scans = 0;
    for (;;) {
      const scan = await step05(current, evidence);
      onScan?.(scan, scans);
      scans += 1;
      current = scan.result.nextThreadOutRef;
      if (scan.result.closed) break;
    }
    return { threadOutRef: current, scans };
  };
  const step05Raw = async (
    threadOutRef: string,
    args: OutputReferenceScriptScanArgs,
    nextState: OutputReferenceScriptScanState,
    nextStepIndex: 4 | 5,
  ) =>
    await buildAndSubmitUnjournaled(
      buildOutputReferenceScriptDecodingStep05Raw,
      {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        args,
        nextState,
        nextStepIndex,
        referenceScriptUtxo: references[4]!,
      },
    );
  const step06 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
  ) =>
    await captured(() =>
      buildAndSubmitUnjournaled(buildOutputReferenceScriptDecodingStep06, {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step06Raw = async (threadOutRef: string) =>
    await buildAndSubmitUnjournaled(
      buildOutputReferenceScriptDecodingStep06Raw,
      {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      },
    );
  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await captured(() =>
      buildAndSubmitUnjournaled(buildOutputReferenceScriptDecodingCancel, {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const remove = async (fraudulentHeaderHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    return await captured(() =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: "outputReferenceScriptDecoding",
        fraudulentHeaderHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "output-reference-script-decoding-emulator",
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
  };
  /**
   * A genuine certified field-2 carriage of another transaction: same chunk
   * bytes, but a certificate anchored to the other transaction id.
   */
  const certifyForeignField = async (
    other: AcceptedSubject,
    outputFieldPreimageHex: string,
  ) => {
    const items = (
      await import("@al-ft/midgard-core")
    ).decodeMidgardFieldPreimage(Buffer.from(outputFieldPreimageHex, "hex"));
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: 2,
      anchorTxId: other.nativeTxId,
      nativeTxCompactCbor: other.compactCborHex,
      itemCbors: items,
      owner: signer.paymentKeyHash,
      publish: true,
      label: "output-reference foreign field 2",
    });
    signer.selectWallet(lucid);
    const carriageUtxos = await publishCarriageUnjournaled({
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: "output-reference foreign field 2",
    });
    if (planned.plan.tier !== "Certified")
      throw new Error("foreign field is not certified");
    const { certificateUtxo } = await certifyCarriageUnjournaled({
      lucid,
      network,
      signer,
      planned,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      certificateMintingScript: contracts.fieldPreimageCertificateMintingScript,
      certificateReferenceScriptUtxo: certificateReference,
      chunkUtxos: carriageUtxos,
      compactCbor: other.compactCborHex,
      witnessSetCompactCbor: other.witnessSetCompactCborHex,
    });
    return { planned, carriageUtxos, certificateUtxo };
  };
  /** Step 02 over a caller-planned carriage (see `certifyForeignField`). */
  const step02Raw = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    carriage: Awaited<ReturnType<typeof certifyForeignField>>,
  ) =>
    await buildAndSubmitUnjournaled(
      buildOutputReferenceScriptDecodingStep02Raw,
      {
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        ...carriage,
        referenceScriptUtxo: references[1]!,
      },
    );

  return {
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03Loop,
    step04,
    step05,
    step05Loop,
    step05Raw,
    step06,
    step06Raw,
    cancel,
    remove,
    certifyForeignField,
  };
};
