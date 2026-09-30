import { type UTxO } from "@lucid-evolution/lucid";

import {
  type ExecutionSourceAuthenticationData,
  planExecutionSourceScriptDecodingStep04,
  readExecutionSourceScanState,
  submitExecutionSourceScriptDecodingCancel,
  submitExecutionSourceScriptDecodingInit,
  submitExecutionSourceScriptDecodingStep01Accepted,
  submitExecutionSourceScriptDecodingStep01Forced,
  submitExecutionSourceScriptDecodingStep01ForcedRaw,
  submitExecutionSourceScriptDecodingStep02,
  submitExecutionSourceScriptDecodingStep02Raw,
  submitExecutionSourceScriptDecodingStep03,
  submitExecutionSourceScriptDecodingStep03Raw,
  submitExecutionSourceScriptDecodingStep04,
  submitExecutionSourceScriptDecodingStep04Raw,
  submitExecutionSourceScriptDecodingStep05,
  submitExecutionSourceScriptDecodingStep05Raw,
} from "../../src/execution-source-script-decoding/index.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { type ExecutionSourceContext } from "./execution-source-script-decoding-emulator.build-canonical-trace.js";
import {
  commitSubjectBlock,
  type SubjectFixture,
} from "./execution-source-script-decoding-emulator.build-subject-fixture.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";

export const makeExecutionSourceStages = ({
  context,
  setup,
  references,
}: {
  readonly context: ExecutionSourceContext;
  readonly setup: Awaited<ReturnType<typeof commitSubjectBlock>>;
  readonly references: readonly UTxO[];
}) => {
  const { harness, contracts, catalogue, category, validators } = context;
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const captured = <T>(operation: () => Promise<T>) =>
    captureEmulatorSubmission(harness.emulator, operation);
  const reference = (stepIndex: number): UTxO => {
    const utxo = references[stepIndex];
    if (utxo === undefined) throw new Error("reference script absent");
    return utxo;
  };
  const common = { lucid, contracts, categoryId, signer };

  const init = async () =>
    await captured(() =>
      submitExecutionSourceScriptDecodingInit({
        ...common,
        blueprint: harness.realBlueprint,
        network,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        fraudulentHeaderHash: setup.headerHash,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  /** A fresh thread at step 01, for seams and cancellations. */
  const freshThread = async () => (await init()).result.nextThreadOutRef;
  const step01Accepted = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    executionIndex = 0n,
    txInclusion = fixture.block.txInclusion,
  ) => {
    if (txInclusion === null) throw new Error("accepted inclusion absent");
    return await captured(() =>
      submitExecutionSourceScriptDecodingStep01Accepted({
        ...common,
        blueprint: harness.realBlueprint,
        network,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        header: fixture.header,
        executionIndex,
        referenceScriptUtxo: reference(0),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  };
  const step01Forced = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    executionIndex = 0n,
  ) => {
    if (fixture.membership === null) throw new Error("forced leaf absent");
    const membership = fixture.membership;
    return await captured(() =>
      submitExecutionSourceScriptDecodingStep01Forced({
        ...common,
        threadOutRef,
        header: fixture.header,
        membership,
        executionIndex,
        referenceScriptUtxo: reference(0),
      }),
    );
  };
  const step01ForcedRaw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep01ForcedRaw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep01ForcedRaw({
      ...common,
      referenceScriptUtxo: reference(0),
      ...args,
    });
  const step02 = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    authentication: ExecutionSourceAuthenticationData = fixture.authentication
      .authentication,
  ) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep02({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        authentication,
        referenceScriptUtxo: reference(1),
      }),
    );
  const step02Raw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep02Raw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep02Raw({
      ...common,
      referenceScriptUtxo: reference(1),
      ...args,
    });
  const step03 = async (threadOutRef: string, fixture: SubjectFixture) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep03({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        referenceScriptUtxo: reference(2),
      }),
    );
  const step03Raw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep03Raw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep03Raw({
      ...common,
      referenceScriptUtxo: reference(2),
      ...args,
    });
  const scanState = async (threadOutRef: string, stepIndex: 3 | 4 = 3) =>
    (await readExecutionSourceScanState({ ...common, threadOutRef, stepIndex }))
      .state;
  const plan04 = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    direction?: 0 | 1,
  ) =>
    planExecutionSourceScriptDecodingStep04({
      contracts,
      state: await scanState(threadOutRef),
      evidence: fixture.evidence,
      ...(direction === undefined ? {} : { direction }),
    });
  const step04 = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    direction?: 0 | 1,
  ) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep04({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        ...(direction === undefined ? {} : { direction }),
        referenceScriptUtxo: reference(3),
      }),
    );
  /** Every scan transaction until the item scan closes. */
  const step04Loop = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    onScan?: (
      captured: Awaited<ReturnType<typeof step04>>,
      index: number,
    ) => void,
    direction?: 0 | 1,
  ) => {
    let current = threadOutRef;
    let scans = 0;
    for (;;) {
      const scan = await step04(current, fixture, direction);
      onScan?.(scan, scans);
      scans += 1;
      current = scan.result.nextThreadOutRef;
      if (scan.result.closed) break;
    }
    return { threadOutRef: current, scans };
  };
  const step04Raw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep04Raw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep04Raw({
      ...common,
      referenceScriptUtxo: reference(3),
      ...args,
    });
  const step05 = async (threadOutRef: string, fixture: SubjectFixture) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep05({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        referenceScriptUtxo: reference(4),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step05Raw = async (threadOutRef: string) =>
    await submitExecutionSourceScriptDecodingStep05Raw({
      ...common,
      threadOutRef,
      referenceScriptUtxo: reference(4),
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingCancel({
        ...common,
        threadOutRef,
        referenceScriptUtxo: reference(stepIndex),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
    const base = buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const deploymentInfo = {
      ...base,
      contracts: {
        ...base.contracts,
        ...Object.fromEntries(
          validators.map((step, index) => [
            index === 0
              ? "fraudProofExecutionSourceScriptDecoding"
              : `fraudProofExecutionSourceScriptDecodingStep0${(index + 1).toString()}`,
            {
              scriptHash: step.spendingScriptHash,
              contract: {
                type: step.spendingScript.type,
                cborHex: step.spendingScript.script,
              },
            },
          ]),
        ),
      },
    };
    const now = BigInt(harness.emulator.now());
    return await captured(() =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: "executionSourceScriptDecoding",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "execution-source-script-decoding-emulator",
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
  return {
    init,
    freshThread,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03,
    step03Raw,
    scanState,
    plan04,
    step04,
    step04Loop,
    step04Raw,
    step05,
    step05Raw,
    cancel,
    remove,
  };
};
