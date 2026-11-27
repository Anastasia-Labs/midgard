import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY } from "../src/runtime.fraud-proof-deployment-entries-by-category.js";
import { buildInit } from "../src/submit-init.submit-init.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import {
  awaitHeaderCommitWindow,
  prepareFamilyHistory,
} from "./support/emulator/family-history.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./support/emulator/header-fixtures.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { publishFraudProofChainReferenceScripts } from "./support/emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { submitSetupTx } from "./support/emulator/setup-tx.submit-setup-tx.js";
import { sortedDaEntries } from "./support/submit-init/transaction-material.js";
import { buildAndSubmit, type SubmittedStep } from "./support/submit-steps.js";
import { publishTransitionTraceYields } from "./support/transition-trace-yields.js";

export const historyRecords: unknown[] = [];

export type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

export type Setup = Awaited<ReturnType<typeof submitSetupTx>>;

export type DeploymentInfo = ReturnType<typeof buildRemovalDeploymentInfo>;

const address = (byte: string): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [byte.repeat(28)] },
  stakeCredential: null,
});

export const withdrawalInfo = (
  validity: SDK.WithdrawalValidity,
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: transitionTraceOutRef("71"),
    l2_owner: "72".repeat(28),
    l2_value: new Map([["", new Map([["", 50_000_000n]])]]),
    l1_address: address("73"),
    l1_datum: "NoDatum",
  },
  signature: ["74".repeat(32), "75".repeat(64)],
  validity,
});

const headerCounts = (header: SDK.Header) => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

/**
 * `header` (which has no events of its own) carrying one withdrawal unrelated
 * to the event a test accuses, so the block can be committed (a committed
 * block carries at least one event). Returns the DA entries `reconstruct`
 * needs for it.
 */
export const withUnrelatedWithdrawal = async (header: SDK.Header) => {
  if (header.totalEventCount !== 0n)
    throw new Error("withUnrelatedWithdrawal expects a header with no events");
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: transitionTraceOutRef("94") },
  };
  const step: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "Withdrawal",
    pre_utxos_root: header.prevUtxosRoot,
    post_utxos_root: header.prevUtxosRoot,
  };
  const withdrawals: SDKDa.DaPayloadEntry[] = [
    [
      Data.to(transitionTraceOutRef("94"), SDK.OutputReference),
      SDKFraudProof.committedWithdrawalValueBytes(
        withdrawalInfo("IncorrectWithdrawalSignature"),
      ),
    ],
  ];
  const transitionTrace = [
    transitionTraceDaEntry({
      key: 0n,
      keySchema: Data.Integer() as never,
      value: step,
      valueSchema: SDK.TransitionStepSchema,
    }),
  ];
  const eventToStep = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: SDK.EventKeySchema,
      value: { step_index: 0n, phase: "Withdrawal" },
      valueSchema: SDK.EventToStepValueSchema,
    }),
  ];
  const root = (
    domain: SDK.RootDomain,
    entries: readonly SDKDa.DaPayloadEntry[],
  ) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [withdrawalsRoot, traceRoot, mappingRoot] = await Promise.all([
    root(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitionTrace),
    root(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
  ]);
  return {
    header: {
      ...header,
      withdrawalsRoot: withdrawalsRoot.root,
      transitionTraceRoot: traceRoot.root,
      eventToStepRoot: mappingRoot.root,
      withdrawalCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
    },
    withdrawals,
    transitionTrace,
    eventToStep,
  };
};

export const reconstruct = async ({
  header,
  withdrawals = [],
  transitionTrace = [],
  eventToStep = [],
}: {
  readonly header: SDK.Header;
  readonly withdrawals?: readonly SDKDa.DaPayloadEntry[];
  readonly transitionTrace?: readonly SDKDa.DaPayloadEntry[];
  readonly eventToStep?: readonly SDKDa.DaPayloadEntry[];
}) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDKDa.encodeDaPayload({
      version: SDKDa.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        withdrawals: sortedDaEntries(withdrawals),
        forced_transactions: [],
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(transitionTrace),
        event_to_step: sortedDaEntries(eventToStep),
        transaction_preimages: [],
        forced_transaction_preimages: [],
        validation_traces: [],
        validation_trace_witnesses: [],
        counts: headerCounts(header),
      },
    }),
    { mode: "identity" },
  );
  return await reconstructDaPayload({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
};

export const makeHarness = async ({
  alwaysStateQueue = false,
}: {
  readonly alwaysStateQueue?: boolean;
} = {}) => {
  const base = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realTransitionTrace: true,
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue,
    },
  });
  const submit = base.emulator.submitTx.bind(base.emulator);
  const scenario = expect.getState().currentTestName;
  base.emulator.submitTx = async (transactionCbor) => {
    const txHash = await submit(transactionCbor);
    historyRecords.push({
      scenario,
      txHash,
      transactionCbor,
      measurement: measureCompleteSignedTransaction(transactionCbor),
      fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
    });
    return txHash;
  };
  const history = await prepareFamilyHistory(base, historyRecords);
  const harness = { ...base, contracts: history.contracts };
  const publications = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const transitionTraceReferenceScripts =
    await publishFraudProofChainReferenceScripts({
      lucid: harness.proverLucid,
      steps: harness.contracts.fraudProofContracts.transitionTrace.steps,
      entryNames: FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY.transitionTrace,
      familyLabel: "transition-trace",
    });
  const yields = await publishTransitionTraceYields(
    harness.proverLucid,
    harness.contracts,
  );
  return {
    harness,
    history,
    publications,
    transitionTraceReferenceScripts: {
      ...transitionTraceReferenceScripts,
      ...yields,
    },
  };
};

export const setupChallenge = async ({
  harness,
  publications,
  transitionTraceReferenceScripts,
  header,
  beforeHeaderCommit,
}: {
  readonly harness: Harness;
  readonly publications: Awaited<
    ReturnType<typeof publishRemovalReferenceScripts>
  >;
  readonly transitionTraceReferenceScripts: Awaited<
    ReturnType<typeof publishFraudProofChainReferenceScripts>
  >;
  readonly header: SDK.Header;
  readonly beforeHeaderCommit?: Parameters<
    typeof submitSetupTx
  >[0]["beforeHeaderCommit"];
}) => {
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
    beforeHeaderCommit,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
    {
      removalReferenceScripts: publications.published,
      fraudProofReferenceScripts: transitionTraceReferenceScripts,
    },
  );
  historyRecords.push({ header, headerHash: setup.headerHash, deploymentInfo });
  const init = await buildAndSubmit(buildInit, {
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "transitionTrace",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  expect(init.fraudCategoryId).toBe(
    harness.catalogue.categories.transitionTrace.categoryId,
  );
  expect(init.fraudulentHeaderHash).toBe(setup.headerHash);
  return { setup, deploymentInfo, init };
};

export const withdrawalIdFor = (
  history: Awaited<ReturnType<typeof prepareFamilyHistory>>,
): SDK.OutputReference => {
  const nonce = history.nonce("Withdrawal");
  return {
    transactionId: nonce.txHash,
    outputIndex: BigInt(nonce.outputIndex),
  };
};

export const setupWithdrawalChallenge = async ({
  harness,
  history,
  publications,
  transitionTraceReferenceScripts,
  header,
  inclusionTime,
}: Awaited<ReturnType<typeof makeHarness>> & {
  header: SDK.Header;
  inclusionTime: bigint;
}) => {
  const withdrawalId = withdrawalIdFor(history);
  let admission: Awaited<ReturnType<typeof history.admit>> | undefined;
  const lifecycle = await setupChallenge({
    harness,
    publications,
    transitionTraceReferenceScripts,
    header,
    beforeHeaderCommit: async (hub) => {
      admission = await history.admit(
        hub,
        {
          WithdrawalPayload: {
            event: {
              id: withdrawalId,
              info: withdrawalInfo("WithdrawalIsValid"),
            },
            refund_address: address("77"),
            refund_datum: "NoDatum",
          },
        },
        { ...header, endTime: inclusionTime },
        { lovelace: 25_000_000n },
      );
      awaitHeaderCommitWindow(harness.emulator, header);
    },
  });
  if (admission === undefined)
    throw new Error("Withdrawal admission did not run");
  return {
    lifecycle,
    withdrawalId,
    event: { utxo: admission.witness.anchor.utxo },
  };
};

export const firstThreadUtxo = async ({
  harness,
  init,
}: {
  readonly harness: Harness;
  readonly init: SubmittedStep<typeof buildInit>;
}) =>
  await expectSingleUtxoWithUnit(
    harness.proverLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );
