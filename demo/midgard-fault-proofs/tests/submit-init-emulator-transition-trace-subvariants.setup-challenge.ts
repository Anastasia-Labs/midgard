import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  reconstructDaPayload,
} from "../src/index.js";
import {
  awaitHeaderCommitWindow,
  prepareFamilyHistory,
} from "./support/emulator/family-history.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import { sortedDaEntries } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  makeFaultProofEmulatorHarness,
  network,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";
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

export const reconstruct = async ({
  header,
  withdrawals = [],
  transitionTrace = [],
  eventToStep = [],
}: {
  readonly header: SDK.Header;
  readonly withdrawals?: readonly SDK.DaPayloadEntry[];
  readonly transitionTrace?: readonly SDK.DaPayloadEntry[];
  readonly eventToStep?: readonly SDK.DaPayloadEntry[];
}) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDK.encodeDaPayload({
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: [],
        withdrawals: sortedDaEntries(withdrawals),
        forced_transactions: [],
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(transitionTrace),
        event_to_step: sortedDaEntries(eventToStep),
        transaction_preimages: [],
        forced_transaction_preimages: [],
        cek_program_material: [],
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
  const init = await submitInit({
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
  readonly init: Awaited<ReturnType<typeof submitInit>>;
}) =>
  await expectSingleUtxoWithUnit(
    harness.proverLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );
