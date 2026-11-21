/**
 * The watcher's installed fault-proof application, driven turn by turn over
 * a real emulator deployment of a script-decoding family.
 *
 * Every turn is what the watcher does for one admitted objective: the
 * registry's admitted runner (`WORKFLOW_RUNNER_FACTORIES`) loads the
 * runtime and runs the workflow over the watcher process's in-flight set,
 * which a restart empties. Only the manifest is stood in for: the
 * binding is the emulator deployment's (the calling test mocks the manifest
 * binding and the L1 observation port with it).
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Lucid,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import {
  watcherDeploymentReleaseEconomicsAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { makeWatcherDeploymentAuthorityFixture } from "midgard-watcher/tests/support/deployment-authority-fixture.build-watcher-deployment-authority-fixture";

import { WALLET_ONLY_VALIDITY_MS } from "../../src/wallet-only-validity.js";
import { createWorkflowActuationPermitController } from "../../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { familyStepContractNames } from "../../src/workflow/family-definition.family-definition.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../../src/workflow/family-l1-observation.js";
import { authenticatedStateQueueObservationDigest } from "../../src/workflow/header-classifier.authenticated-state-queue-observation-digest.js";
import { classifyHeader } from "../../src/workflow/header-classifier.classify-header.js";
import { createHeaderClassifier } from "../../src/workflow/header-classifier.create-header-classifier.js";
import type { InFlightTransaction } from "../../src/workflow/in-flight.js";
import {
  WORKFLOW_RUNNER_FACTORIES,
  WORKFLOW_RUNTIME_CONFIG,
} from "../../src/workflow/runtime.js";
import { SCRIPT_DECODE_SPEND_ROLE } from "../../src/workflow/script-decode-prerequisite.deployment.js";
import type { WalletOnlySend } from "../../src/workflow/wallet-only-send-gate.js";
import { restrictWorkflowWallet } from "../../src/workflow/workflow-wallet.js";
import type { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
} from "./emulator/emulator-context.js";
import type { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { publishRemovalReferenceScripts } from "./emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.build-removal-deployment-info.js";
import { submitSetupTx } from "./emulator/setup-tx.submit-setup-tx.js";
import { emulatorManifestProtocolParameters } from "./emulator-protocol-parameters.js";
import { deployHarnessFieldGrammarCertificate } from "./field-grammar-certificate-deployment-fixture.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.build-decoding-block-fixture.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import {
  fixtureLeaseCoordinator,
  FixtureRetainedDaSource,
} from "./script-decode-installed-host.js";
import { walletOnlySendAlwaysOpenOn } from "./wallet-only-send-always-open.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type Recorder = ReturnType<typeof recordCrossBlockRawEmulator>;
type Replayer = Parameters<typeof createHeaderClassifier>[0]["replayer"];
export type InstalledCategory =
  | "witnessScriptDecoding"
  | "outputReferenceScriptDecoding";

/** The family-definition surface the installed application reads. */
export type InstalledFamily = Parameters<typeof familyStepContractNames>[0] &
  Readonly<{
    category: InstalledCategory;
    stepDatumSchemas: readonly unknown[];
    witnessRoles: readonly string[];
  }>;

const COLLATERAL_OUTPUT_LOVELACE = 2_000_000_000n;
const RUNTIME_CONFIG_PATH = "/installed/script-decode-runtime.json";

/**
 * Publishes the family's remaining references and commits the block whose
 * subject is `nativeTx`: the steps come from the harness's fault-proof
 * publications, the certificate and the removal set are published here.
 */
export const commitScriptDecodeBlock = async ({
  harness,
  definition,
  nativeTx,
}: {
  readonly harness: Harness;
  readonly definition: InstalledFamily;
  readonly nativeTx: Extract<
    Parameters<typeof buildDecodingBlockFixture>[0]["subject"],
    { kind: "normal" }
  >["nativeTx"];
}) => {
  const steps = familyStepContractNames(definition).map((name) => {
    const published = harness.faultProofReferenceScripts[name];
    if (published === undefined)
      throw new Error(`harness did not publish step ${name}`);
    return [name, published.utxo] as const;
  });
  const certificate = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${definition.category} field certificate`,
    })
  ).utxo;
  // Both installed families read through the field grammar certificate.
  const fieldGrammar = await deployHarnessFieldGrammarCertificate(
    harness,
    definition.category,
  );
  const removal = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
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
  // An operator funds the watcher wallet with a dedicated collateral output
  // beside its funding.
  const split = await harness.proverLucid
    .newTx()
    .pay.ToAddress(harness.proverSigner.address, {
      lovelace: COLLATERAL_OUTPUT_LOVELACE,
    })
    .complete();
  await (await split.sign.withWallet().complete()).submit();
  harness.emulator.awaitBlock();
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return {
    block,
    headerHash: setup.headerHash,
    steps,
    certificate,
    fieldGrammar,
    removal,
  } as const;
};

/**
 * Binds the emulator deployment as the signed manifest would, classifies the
 * committed header through the authenticated raw L1 port, and returns the
 * watcher's objective: its decision and a `turn` that runs one watcher
 * application turn for it (permit mint, then the admitted runner).
 */
export const installScriptDecodeApplication = async ({
  harness,
  definition,
  replayer,
  committed,
  recorder,
  setBinding,
}: {
  readonly harness: Harness;
  readonly definition: InstalledFamily;
  readonly replayer: Replayer;
  readonly committed: Awaited<ReturnType<typeof commitScriptDecodeBlock>>;
  readonly recorder: Recorder;
  readonly setBinding: (binding: unknown) => void;
}) => {
  const { emulator, contracts } = harness;
  const { category } = definition;
  const { headerHash } = committed;
  // The default fixture: the selected profile's finality and economics.
  const identity = makeWatcherDeploymentAuthorityFixture().result;
  const fingerprint = identity.manifestId;
  const releaseFinalityAuthority =
    watcherDeploymentReleaseFinalityAuthority(identity);
  const releaseFinality = await releaseFinalityAuthority.verifyForWorkflow({
    deploymentFingerprint: fingerprint,
  });
  const releaseEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    identity,
  ).verifyForWorkflow({ deploymentFingerprint: fingerprint });
  const deploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    harness.catalogue,
    { removalReferenceScripts: committed.removal.published },
  );

  // The family's roster, resolved the way a manifest names it.
  const scriptDecode = scriptDecodeDeploymentOf(emulator);
  const witnessScripts = harness.witnessReferenceScripts as Readonly<
    Record<string, UTxO | undefined>
  >;
  const roster = new Map<string, UTxO>([
    ...committed.steps,
    ...definition.witnessRoles.map((role): [string, UTxO] => {
      const utxo = witnessScripts[role];
      if (utxo === undefined) throw new Error(`no witness reference ${role}`);
      return [role, utxo];
    }),
    ["fieldPreimageCertificateMint", committed.certificate],
    ...committed.fieldGrammar.roster,
    [SCRIPT_DECODE_SPEND_ROLE, scriptDecode.reference],
    ...SDK.SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name): [string, UTxO] => [
      SDK.SCRIPT_DECODE_STAKER_MEMBERS[name],
      scriptDecode.stakerReferences[name],
    ]),
  ]);
  const binding = {
    deploymentFingerprint: fingerprint,
    blueprintHash: releaseFinality.blueprintHash,
    network: "Custom",
    blueprint: harness.realBlueprint,
    blueprintJson: JSON.stringify(harness.realBlueprint),
    deploymentInfo,
    contractEntries: deploymentInfo.contracts,
    releaseFinality,
    releaseEconomics,
    cardanoProtocolParameters: emulatorManifestProtocolParameters(emulator),
    catalogue: {
      policyId: contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    fieldPreimageCertificate: {
      policyId: contracts.fieldPreimageCertificate.policyId,
      mintingScript: contracts.fieldPreimageCertificate.mintingScript,
    },
    referenceScriptsByContract: Object.fromEntries(
      [...roster].map(([name, utxo]) => [
        name,
        {
          outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
          scriptHash: validatorToScriptHash(utxo.scriptRef!),
        },
      ]),
    ),
    definition: {
      category,
      categoryId: harness.catalogue.categories[category]!.categoryId,
      headerHash,
      proverCredential: harness.proverSigner.paymentKeyHash,
      stateQueue: {
        policyId: contracts.stateQueue.policyId,
        address: contracts.stateQueue.spendingScriptAddress,
      },
      computationThread: {
        policyId: contracts.computationThread.policyId,
        steps: contracts.fraudProofContracts[category].steps.map(
          (step, index) => ({
            role: `computation_thread_step_${String(index + 1).padStart(2, "0")}`,
            address: step.spendingScriptAddress,
            datumSchema: definition.stepDatumSchemas[index]!,
          }),
        ),
      },
      proofToken: {
        policyId: contracts.fraudProof.policyId,
        address: contracts.fraudProof.spendingScriptAddress,
      },
      operatorDirectory: {
        activePolicyId: contracts.activeOperators.policyId,
        activeAddress: contracts.activeOperators.spendingScriptAddress,
        retiredPolicyId: contracts.retiredOperators.policyId,
        retiredAddress: contracts.retiredOperators.spendingScriptAddress,
      },
      schedulerAddress: contracts.scheduler.spendingScriptAddress,
    },
    resolvedContracts: {
      hubOraclePolicyId: contracts.hubOracle.policyId,
      category: harness.catalogue.categories[category],
      contracts: {
        ...contracts.fraudProofContracts,
        computationThread: contracts.computationThread,
        fraudProof: contracts.fraudProof,
      },
    },
  };
  setBinding(binding);
  const l1 = createFraudProofFamilyRawL1ObservationPort({
    authority: recorder.authority,
    releaseFinality,
    releaseEconomics,
    definition: binding.definition as never,
  });
  // Classify the committed header as the watcher's classifier does.
  const sources = [
    new FixtureRetainedDaSource(
      fingerprint,
      new Map([[headerHash, committed.block.payloadEnvelopeCbor]]),
    ),
  ];
  const observation = await l1.observeHeader({ headerHash });
  const decision = await classifyHeader({
    classifier: await createHeaderClassifier({
      deploymentFingerprint: fingerprint,
      replayer,
      releaseFinalityAuthority,
    }),
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
      }),
    sources,
  });
  if (decision.decision !== "fault_detected")
    throw new Error(`header classified ${decision.decision}`);
  const permit = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "0",
  }).permit;
  const slotConfig = harness.proverLucid.config().slotConfig;
  const stateQueueMutationLeaseCoordinator = fixtureLeaseCoordinator();
  const signer = harness.proverSigner;
  /**
   * The watcher process: the admitted runner, the process's in-flight set and
   * its restart wait. Booting again is a restart. The first process starts on
   * a chain nothing of ours was sent on, so its wait is already over; a
   * restarted one sends nothing that spends only wallet coins until one
   * wallet-only validity window has passed on the emulator's clock, so a
   * copy the stopped process sent can no longer land beside a rebuilt one.
   */
  const boot = (restarted: boolean) => {
    const runner = WORKFLOW_RUNNER_FACTORIES[category](
      async ({ invocation }) => {
        // The watcher's loader body (`buildCommonInfrastructure`).
        const execution = invocation as Partial<
          Parameters<typeof runner.runOrResume>[0]
        >;
        // The deployment's genesis slot mapping, as the watcher's Lucid takes
        // it from the manifest (a bare emulator Lucid starts its mapping now).
        const lucid = await Lucid(emulator, "Custom", { slotConfig });
        const restricted =
          execution.sends === undefined
            ? signer
            : restrictWorkflowWallet({ signer, sends: execution.sends });
        restricted.selectWallet(lucid);
        return {
          schemaVersion: WORKFLOW_RUNTIME_CONFIG,
          infrastructure: {
            manifest: {},
            blueprintJson: binding.blueprintJson,
            deploymentInfo,
            headerHash: invocation.headerHash,
            ...(execution.decisionDigest === undefined
              ? {}
              : { decisionDigest: execution.decisionDigest }),
            lucid,
            signer: restricted,
            l1Source: {} as never,
            stateQueueMutationLeaseCoordinator,
          },
          resolveReferenceScript: async ({
            contractName,
          }: {
            readonly contractName: string;
          }) => {
            const utxo = roster.get(contractName);
            if (utxo === undefined)
              throw new Error(`no published reference for ${contractName}`);
            return utxo;
          },
          retainedDaSources: sources,
          close: async () => undefined,
        } as never;
      },
    );
    const alwaysOpen = walletOnlySendAlwaysOpenOn(harness.proverLucid);
    const notBeforeMs = emulator.now() + WALLET_ONLY_VALIDITY_MS;
    const walletOnlySend: WalletOnlySend = restarted
      ? Object.freeze({
          open: () => emulator.now() >= notBeforeMs,
          latestValidToSlot: alwaysOpen.latestValidToSlot,
        })
      : alwaysOpen;
    return {
      runner,
      process: createTestWorkflowProcess(),
      walletOnlySend,
    } as const;
  };
  let instance = boot(false);
  /** What earlier processes sent, oldest first. */
  const before: InFlightTransaction[] = [];
  /** One watcher application turn for the objective. */
  const turn = async () =>
    (await instance.runner.runOrResume({
      category,
      deploymentFingerprint: fingerprint,
      headerHash,
      decisionDigest: decision.decisionDigest,
      actuationPermit: permit,
      sends: instance.process.sends({ category, headerHash }),
      runtimeConfigPath: RUNTIME_CONFIG_PATH,
      walletOnlySend: instance.walletOnlySend,
    })) as {
      readonly kind: string;
      readonly reason?: string;
      readonly resumeOnObservation?: true;
    };
  return {
    decision,
    l1,
    fingerprint,
    turn,
    /** Stops the watcher process and starts another, which holds nothing
     * the stopped one had in flight. */
    restart: () => {
      before.push(...instance.process.sent());
      instance = boot(true);
    },
    /** Every transaction the watcher sent for the objective, across
     * restarts, oldest first. */
    sent: () => Object.freeze([...before, ...instance.process.sent()]),
    close: async () => undefined,
  } as const;
};
