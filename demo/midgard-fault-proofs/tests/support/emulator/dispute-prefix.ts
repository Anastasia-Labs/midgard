/**
 * The deploy prefix of a forced validation-dispute scenario, built once per
 * test file and branched for every scenario.
 *
 * Every `runForcedValidationDisputeScenario` call used to start by deploying
 * the same thing: the party ledger, the PHAS membership reward account, the
 * reference-script nonce and auth policy, the minimal fault-proof contracts,
 * and the operator-lifecycle and witness reference-script publications. None of
 * it depends on the scenario. It is now built for real on the first call in a
 * process, captured as plain data, and every call (the first included) resumes
 * from a fresh copy of that capture: a new `Emulator` holding a deep copy of
 * the captured ledger, clock and parameters, new Lucid instances with the
 * captured slot configuration, protocol parameters and wallets, new signers,
 * and deep copies of the deployed contracts and publications. Nothing one
 * scenario does to its branch can reach the capture or the next branch.
 *
 * The prefix transactions are recorded once, as submitted, and replayed in
 * submission order to each scenario's `onSubmittedTransaction`, so a scenario
 * observes exactly the transaction sequence it observed before.
 */
import {
  Emulator,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  type ProtocolParameters,
  type SlotConfig,
} from "@lucid-evolution/lucid";

import {
  resolveProverSigner,
  validationDisputeValidityRange,
} from "../../../src/index.js";
import {
  alwaysSucceedsBlueprintPath,
  type Blueprint,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./blueprints.js";
import { buildCatalogueDeploymentInfo } from "./catalogue.js";
import { buildMinimalFaultProofContracts } from "./contracts.js";
import { createValidationDisputeParties } from "./dispute-staging.js";
import { registerPhasMembershipRewardAccount } from "./emulator-context.js";
import { measureCompleteSignedTransaction } from "./measurement.js";
import { createReferenceScriptPublisher } from "./reference-script-publisher.js";
import {
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
} from "./reference-scripts.js";

/**
 * Refuses anything `structuredClone` would not copy faithfully: functions,
 * symbols, class instances (Buffer, Map, Date, wasm handles), and typed arrays
 * other than a plain `Uint8Array`. A capture that passes is plain data, so its
 * deep copy is indistinguishable from it.
 */
const assertPlainData = (value: unknown, path: string): void => {
  if (value === null) return;
  switch (typeof value) {
    case "string":
    case "number":
    case "bigint":
    case "boolean":
    case "undefined":
      return;
    case "object":
      break;
    case "symbol":
    case "function":
      throw new Error(
        `Dispute deploy prefix capture holds a ${typeof value} at ${path}`,
      );
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype === Uint8Array.prototype) return;
  if (Array.isArray(value)) {
    if (prototype !== Array.prototype)
      throw new Error(`Dispute deploy prefix capture: ${path} is not an Array`);
    value.forEach((item, index) =>
      assertPlainData(item, `${path}[${index.toString()}]`),
    );
    return;
  }
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(
      `Dispute deploy prefix capture holds a non-plain object at ${path}`,
    );
  }
  if (Object.getOwnPropertySymbols(value).length > 0) {
    throw new Error(`Dispute deploy prefix capture: ${path} has symbol keys`);
  }
  for (const [key, item] of Object.entries(value)) {
    assertPlainData(item, `${path}.${key}`);
  }
};

const deepFreeze = <T>(value: T): T => {
  if (typeof value === "object" && value !== null && !Object.isFrozen(value)) {
    Object.freeze(value);
    for (const item of Object.values(value)) deepFreeze(item);
  }
  return value;
};

/** The emulator's own data fields, without its provider brand symbol. */
const captureEmulatorState = (emulator: Emulator): Record<string, unknown> => {
  const state = Object.fromEntries(Object.entries(emulator));
  assertPlainData(state, "emulator");
  return structuredClone(state);
};

const restoreEmulator = (state: Record<string, unknown>): Emulator => {
  const emulator = new Emulator([]);
  const constructed = Object.keys(emulator).sort();
  const captured = Object.keys(state).sort();
  if (constructed.join() !== captured.join()) {
    throw new Error(
      `Dispute deploy prefix capture fields [${captured.join()}] do not match a fresh Emulator's [${constructed.join()}]`,
    );
  }
  return Object.assign(emulator, structuredClone(state));
};

const prefixData = async ({
  emulator,
  operatorLucid,
  challengerLucid,
  realBlueprint,
  alwaysBlueprint,
}: {
  readonly emulator: Emulator;
  readonly operatorLucid: LucidEvolution;
  readonly challengerLucid: LucidEvolution;
  readonly realBlueprint: Blueprint;
  readonly alwaysBlueprint: Blueprint;
}) => {
  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const { nonceUtxo, referenceScriptAuth } =
    await createReferenceScriptPublisher(operatorLucid, emulator.now());
  const baseContracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    {
      referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
  );
  // Publish the four directory validators before operator setup samples time.
  const operatorLifecycleReferenceScripts =
    await publishOperatorLifecycleReferenceScripts({
      lucid: challengerLucid,
      contracts: {
        ...baseContracts,
        referenceScriptAuth,
        referenceScriptPublisher: {
          lucid: operatorLucid,
          reservedInputs: [nonceUtxo],
        },
      },
    });
  const catalogue = await buildCatalogueDeploymentInfo(
    baseContracts.fraudProofs,
  );
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: challengerLucid,
      realBlueprint,
      computationThreadMintingScript:
        baseContracts.computationThread.mintingScript,
      fraudProofMintingScript: baseContracts.fraudProof.mintingScript,
    });
  const operatorPaymentCredential = getAddressDetails(
    await operatorLucid.wallet().address(),
  ).paymentCredential;
  if (
    operatorPaymentCredential === undefined ||
    operatorPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected operator wallet to expose a payment key hash");
  }
  return {
    nonceUtxo,
    referenceScriptAuth,
    baseContracts,
    operatorLifecycleReferenceScripts,
    catalogue,
    witnessReferenceScripts,
    operatorPaymentKeyHash: operatorPaymentCredential.hash,
  };
};

type PrefixData = Awaited<ReturnType<typeof prefixData>>;

type DisputeDeployPrefixCapture = {
  readonly realBlueprint: Blueprint;
  readonly alwaysBlueprint: Blueprint;
  readonly operatorSeedPhrase: string;
  readonly challengerSeedPhrase: string;
  readonly slotConfig: SlotConfig;
  readonly lucidProtocolParameters: ProtocolParameters;
  readonly emulatorState: Record<string, unknown>;
  readonly data: PrefixData;
  readonly submittedTransactions: readonly string[];
};

const buildDisputeDeployPrefix =
  async (): Promise<DisputeDeployPrefixCapture> => {
    // Shared read-only by every branch: frozen, so a scenario that tried to
    // edit a blueprint in place would fail instead of leaking the edit.
    const realBlueprint = deepFreeze(readBlueprint(realBlueprintPath));
    const alwaysBlueprint = deepFreeze(
      readBlueprint(alwaysSucceedsBlueprintPath),
    );
    const { emulator, operator, challenger, operatorLucid, challengerLucid } =
      await createValidationDisputeParties();
    const operatorSlotConfig = operatorLucid.config().slotConfig;
    const challengerSlotConfig = challengerLucid.config().slotConfig;
    if (
      operatorSlotConfig === undefined ||
      challengerSlotConfig === undefined ||
      JSON.stringify(operatorSlotConfig) !==
        JSON.stringify(challengerSlotConfig)
    ) {
      throw new Error(
        "Expected both dispute party Lucids to share one emulator slot config",
      );
    }
    const operatorProtocolParameters =
      operatorLucid.config().protocolParameters;
    if (operatorProtocolParameters === undefined) {
      throw new Error(
        "Expected the operator Lucid to hold protocol parameters",
      );
    }
    assertPlainData(operatorProtocolParameters, "protocolParameters");
    const submittedTransactions: string[] = [];
    const submit = emulator.submitTx.bind(emulator);
    emulator.submitTx = async (transaction) => {
      const hash = await submit(transaction);
      submittedTransactions.push(transaction);
      return hash;
    };
    const data = await prefixData({
      emulator,
      operatorLucid,
      challengerLucid,
      realBlueprint,
      alwaysBlueprint,
    });
    delete (emulator as { submitTx?: unknown }).submitTx;
    assertPlainData(data, "prefix");
    return {
      realBlueprint,
      alwaysBlueprint,
      operatorSeedPhrase: operator.seedPhrase,
      challengerSeedPhrase: challenger.seedPhrase,
      slotConfig: { ...operatorSlotConfig },
      lucidProtocolParameters: structuredClone(operatorProtocolParameters),
      emulatorState: captureEmulatorState(emulator),
      data: structuredClone(data),
      submittedTransactions,
    };
  };

let capture: Promise<DisputeDeployPrefixCapture> | undefined;

/** Builds the prefix on first use; a failed build is retried by the next call. */
const disputeDeployPrefixCapture = (): Promise<DisputeDeployPrefixCapture> => {
  if (capture === undefined) {
    capture = buildDisputeDeployPrefix();
    capture.catch(() => {
      capture = undefined;
    });
  }
  return capture;
};

const branchLucid = async (
  emulator: Emulator,
  prefix: DisputeDeployPrefixCapture,
  seedPhrase: string,
) => {
  const lucid = await Lucid(emulator, "Custom", {
    slotConfig: prefix.slotConfig,
    presetProtocolParameters: structuredClone(prefix.lucidProtocolParameters),
  });
  lucid.selectWallet.fromSeed(seedPhrase);
  return lucid;
};

/**
 * A fresh, independent copy of the deployed dispute prefix: the same objects
 * `runForcedValidationDisputeScenario` used to build inline, at the same chain
 * state, after replaying the prefix's submitted transactions to
 * `onSubmittedTransaction`.
 */
export const branchDisputeDeployPrefix = async (
  onSubmittedTransaction?: (
    measurement: ReturnType<typeof measureCompleteSignedTransaction>,
    transactionCbor: string,
  ) => void,
) => {
  const prefix = await disputeDeployPrefixCapture();
  const emulator = restoreEmulator(prefix.emulatorState);
  const operatorLucid = await branchLucid(
    emulator,
    prefix,
    prefix.operatorSeedPhrase,
  );
  const challengerLucid = await branchLucid(
    emulator,
    prefix,
    prefix.challengerSeedPhrase,
  );
  const data = structuredClone(prefix.data);
  if (onSubmittedTransaction !== undefined) {
    const submit = emulator.submitTx.bind(emulator);
    emulator.submitTx = async (transaction) => {
      const hash = await submit(transaction);
      onSubmittedTransaction(
        measureCompleteSignedTransaction(transaction),
        transaction,
      );
      return hash;
    };
    for (const transaction of prefix.submittedTransactions) {
      onSubmittedTransaction(
        measureCompleteSignedTransaction(transaction),
        transaction,
      );
    }
  }
  const referenceScriptPublisher = {
    lucid: operatorLucid,
    reservedInputs: [data.nonceUtxo],
  };
  return {
    realBlueprint: prefix.realBlueprint,
    alwaysBlueprint: prefix.alwaysBlueprint,
    emulator,
    operator: { seedPhrase: prefix.operatorSeedPhrase },
    challenger: { seedPhrase: prefix.challengerSeedPhrase },
    operatorLucid,
    challengerLucid,
    operatorSigner: resolveProverSigner({
      network,
      walletSeedPhrase: prefix.operatorSeedPhrase,
    }),
    challengerSigner: resolveProverSigner({
      network,
      walletSeedPhrase: prefix.challengerSeedPhrase,
    }),
    validityRange: () => validationDisputeValidityRange(emulator.now()),
    nonceUtxo: data.nonceUtxo,
    referenceScriptAuth: data.referenceScriptAuth,
    referenceScriptPublisher,
    contracts: {
      ...data.baseContracts,
      referenceScriptAuth: data.referenceScriptAuth,
      referenceScriptPublisher,
      operatorLifecycleReferenceScripts: data.operatorLifecycleReferenceScripts,
    },
    catalogue: data.catalogue,
    witnessReferenceScripts: data.witnessReferenceScripts,
    operatorPaymentKeyHash: data.operatorPaymentKeyHash,
  };
};
