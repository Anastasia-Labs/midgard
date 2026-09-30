import { type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  type OutputReferenceScriptDecodingEvidence,
  type OutputReferenceScriptScanArgs,
  type OutputReferenceScriptScanState,
  submitOutputReferenceScriptDecodingCancel,
  submitOutputReferenceScriptDecodingStep01Accepted,
  submitOutputReferenceScriptDecodingStep01Forced,
  submitOutputReferenceScriptDecodingStep01ForcedRaw,
  submitOutputReferenceScriptDecodingStep02,
  submitOutputReferenceScriptDecodingStep02Raw,
  submitOutputReferenceScriptDecodingStep03,
  submitOutputReferenceScriptDecodingStep04,
  submitOutputReferenceScriptDecodingStep05,
  submitOutputReferenceScriptDecodingStep05Raw,
  submitOutputReferenceScriptDecodingStep06,
  submitOutputReferenceScriptDecodingStep06Raw,
} from "../../src/output-reference-script-decoding/index.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { type SubmitStep01TxInclusion } from "../../src/step-support.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import {
  type AcceptedSubject,
  type Measurement,
  network,
  type OutputReferenceContext,
} from "./output-reference-script-decoding-emulator.commit-accepted-block.js";
import { type ForcedLeaf } from "./output-reference-script-decoding-emulator.commit-forced-block.js";
import {
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

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
      submitCommittedFieldShapeInit({
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
      readonly outputIndex: number;
    },
    txInclusion: SubmitStep01TxInclusion,
  ) =>
    await captured(async () =>
      submitOutputReferenceScriptDecodingStep01Accepted({
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
      }),
    );
  const step01Forced = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    leaf: ForcedLeaf,
    header: Parameters<typeof submitSetupTx>[0]["header"],
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep01Forced({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        forcedSource: { header, membership: leaf.membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      }),
    );
  const step01ForcedRaw = async (
    args: Omit<
      Parameters<typeof submitOutputReferenceScriptDecodingStep01ForcedRaw>[0],
      "lucid" | "contracts" | "categoryId" | "signer" | "referenceScriptUtxo"
    >,
  ) =>
    await submitOutputReferenceScriptDecodingStep01ForcedRaw({
      lucid,
      contracts,
      categoryId,
      signer,
      referenceScriptUtxo: references[0]!,
      ...args,
    });
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
    } = {},
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep02({
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
        submitOutputReferenceScriptDecodingStep03({
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
  const step04 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    compactCborHex: string,
    opened: {
      readonly carriageUtxos: readonly UTxO[];
      readonly certificateUtxo?: UTxO;
    },
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep04({
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
      }),
    );
  const step05 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep05({
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
    await submitOutputReferenceScriptDecodingStep05Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      args,
      nextState,
      nextStepIndex,
      referenceScriptUtxo: references[4]!,
    });
  const step06 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep06({
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
    await submitOutputReferenceScriptDecodingStep06Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      referenceScriptUtxo: references[5]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingCancel({
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
    const carriageUtxos = await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "output-reference foreign field 2",
    });
    if (planned.plan.tier !== "Certified")
      throw new Error("foreign field is not certified");
    const { certificateUtxo } = await certifyFaultProofFieldCarriage({
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
    await submitOutputReferenceScriptDecodingStep02Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      ...carriage,
      referenceScriptUtxo: references[1]!,
    });

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
