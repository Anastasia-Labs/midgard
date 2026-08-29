/**
 * Transition-trace representation audit for fault variants that previously
 * had only direct validator vectors. Each positive case enters through the
 * registered catalogue category, routes to the selected real final validator,
 * mints the permanent fraud-proof token, and removes the condemned block.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  getAddressDetails,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildTransitionFaultProof,
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  reconstructDaPayloadV1,
  resolveTransitionTraceDeploymentContracts,
  submitRemoveFraudulentBlock,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  expectStateQueueHeaderOrder,
  sortedDaEntries,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  ledgerOrderedIndex,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  network,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
  TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarnessV1>>;
type Setup = Awaited<ReturnType<typeof submitSetupTx>>;
type DeploymentInfo = ReturnType<typeof buildRemovalDeploymentInfo>;

const address = (byte: string): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [byte.repeat(28)] },
  stakeCredential: null,
});

const withdrawalInfo = (
  validity: SDK.WithdrawalValidity,
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: transitionTraceOutRef("71"),
    l2_owner: "72".repeat(28),
    l2_value: new Map(),
    l1_address: address("73"),
    l1_datum: "NoDatum",
  },
  signature: ["74".repeat(32), "75".repeat(64)],
  validity,
});

const withdrawalDatum = ({
  id,
  inclusionTime,
}: {
  readonly id: SDK.OutputReference;
  readonly inclusionTime: bigint;
}): SDK.WithdrawalOrderDatum => ({
  event: { id, info: withdrawalInfo("WithdrawalIsValid") },
  inclusion_time: inclusionTime,
  witness: "76".repeat(28),
  refund_address: address("77"),
  refund_datum: "NoDatum",
});

const headerCounts = (header: SDK.HeaderV1) => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

const reconstruct = async ({
  header,
  withdrawals = [],
  transitionTrace = [],
  eventToStep = [],
}: {
  readonly header: SDK.HeaderV1;
  readonly withdrawals?: readonly SDK.DaPayloadEntry[];
  readonly transitionTrace?: readonly SDK.DaPayloadEntry[];
  readonly eventToStep?: readonly SDK.DaPayloadEntry[];
}) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payloadEnvelopeCbor = await wrapDaPayloadV1(
    SDK.encodeDaPayloadV1({
      version: SDK.DA_PAYLOAD_V1_VERSION,
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
        counts: headerCounts(header),
      },
    }),
    { mode: "identity" },
  );
  return await reconstructDaPayloadV1({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
};

const makeHarness = async ({
  alwaysStateQueue = false,
}: {
  readonly alwaysStateQueue?: boolean;
} = {}) => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realTransitionTrace: true,
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue,
    },
  });
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
      oversizedEntryNames: TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES,
    });
  return { harness, publications, transitionTraceReferenceScripts };
};

const setupChallenge = async ({
  harness,
  publications,
  transitionTraceReferenceScripts,
  header,
}: {
  readonly harness: Harness;
  readonly publications: Awaited<
    ReturnType<typeof publishRemovalReferenceScripts>
  >;
  readonly transitionTraceReferenceScripts: Awaited<
    ReturnType<typeof publishFraudProofChainReferenceScripts>
  >;
  readonly header: SDK.HeaderV1;
}) => {
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
    {
      removalReferenceScripts: publications.published,
      fraudProofReferenceScripts: transitionTraceReferenceScripts,
    },
  );
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

const mintWithdrawalEvent = async ({
  harness,
  datum,
}: {
  readonly harness: Harness;
  readonly datum: SDK.WithdrawalOrderDatum;
}): Promise<{ readonly utxo: UTxO; readonly assetName: string }> => {
  const assetName = await Effect.runPromise(
    SDK.withdrawalEventNonceV1(datum.event.id),
  );
  const unit = toUnit(harness.contracts.withdrawal.policyId, assetName);
  const unsigned = await harness.funderLucid
    .newTx()
    .mintAssets({ [unit]: 1n }, Data.void())
    .pay.ToContract(
      harness.contracts.withdrawal.spendingScriptAddress,
      { kind: "inline", value: SDK.withdrawalEventDatumBytesV1(datum) },
      { lovelace: 5_000_000n, [unit]: 1n },
    )
    .attach.MintingPolicy(harness.contracts.withdrawal.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
  return {
    assetName,
    utxo: await expectSingleUtxoWithUnit(
      harness.funderLucid,
      harness.contracts.withdrawal.spendingScriptAddress,
      unit,
    ),
  };
};

const firstThreadUtxo = async ({
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

const removeAndAssertPermanentProof = async ({
  harness,
  setup,
  deploymentInfo,
  proofResult,
}: {
  readonly harness: Harness;
  readonly setup: Setup;
  readonly deploymentInfo: DeploymentInfo;
  readonly proofResult: Awaited<ReturnType<typeof submitTransitionTraceProof>>;
}) => {
  const proofUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    proofResult.fraudProofAddress,
    proofResult.fraudProofUnit,
  );
  const paymentCredential = getAddressDetails(
    await harness.proverLucid.wallet().address(),
  ).paymentCredential;
  expect(paymentCredential?.type).toBe("Key");
  expect(Data.from(proofUtxo.datum!, SDK.FraudProofTokenDatum)).toEqual({
    fraud_prover: paymentCredential!.hash,
  });

  const now = BigInt(harness.emulator.now());
  const removal = await submitRemoveFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "transitionTrace",
    fraudulentHeaderHash: setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
  expect(removal.transactions.map(({ kind }) => kind)).toEqual([
    "remove-target",
  ]);
  await expectStateQueueHeaderOrder({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    expectedHeaderHashes: [],
  });
  await expect(
    harness.funderLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  const retained = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    proofResult.fraudProofAddress,
    proofResult.fraudProofUnit,
  );
  expect(outRefLabel(retained)).toBe(outRefLabel(proofUtxo));
  expect(retained.assets[proofResult.fraudProofUnit]).toBe(1n);
};

const alignedHeaderStart = async (harness: Harness) =>
  alignUnixTimeToEmulatorSlotBoundary(
    harness.funderLucid,
    harness.emulator.now() + 120_000,
  ) - 1;

describe("transition-trace omitted/out-of-window/count subvariant lifecycle", () => {
  it("routes an omitted due withdrawal to final 6 and removes the block", async () => {
    const { harness, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      await alignedHeaderStart(harness),
    );
    const lifecycle = await setupChallenge({
      harness,
      publications,
      transitionTraceReferenceScripts,
      header,
    });
    const withdrawalId = transitionTraceOutRef("81");
    const event = await mintWithdrawalEvent({
      harness,
      datum: withdrawalDatum({
        id: withdrawalId,
        inclusionTime: header.endTime,
      }),
    });
    const reconstruction = await reconstruct({ header });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOmittedDueL1EventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
          eventRefInputIndex: ledgerOrderedIndex(
            [
              lifecycle.setup.hubOracle,
              transitionTraceReferenceScripts[
                "fraudProofTransitionTraceL1Event"
              ]!.utxo,
              ...[
                harness.witnessReferenceScripts.computationThreadMint,
                harness.witnessReferenceScripts.fraudProofMint,
              ].filter((utxo): utxo is UTxO => utxo !== undefined),
              event.utxo,
            ],
            event.utxo,
            "omitted withdrawal reference input",
          ),
          eventAssetName: event.assetName,
        },
      }),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(6);
    const proofResult = await submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      additionalReferenceInputs: [event.utxo],
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    await removeAndAssertPermanentProof({
      harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("routes an out-of-window withdrawal to final 6 and removes the block", async () => {
    const { harness, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const operator = await funderPaymentKeyHash(harness.funderLucid);
    const startTime = await alignedHeaderStart(harness);
    const withdrawalId = transitionTraceOutRef("82");
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    };
    const committedInfo = withdrawalInfo("IncorrectWithdrawalSignature");
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "Withdrawal",
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    const mapping: SDK.EventToStepValue = {
      step_index: 0n,
      phase: "Withdrawal",
    };
    const withdrawals = [
      transitionTraceDaEntry({
        key: withdrawalId,
        keySchema: SDK.OutputReference as never,
        value: committedInfo,
        valueSchema: SDK.WithdrawalInfoSchema,
      }),
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
        value: mapping,
        valueSchema: SDK.EventToStepValueSchema,
      }),
    ];
    const [withdrawalsRoot, traceRoot, mappingRoot] = await Promise.all([
      buildCountedRoot(
        SDK.ROOT_DOMAINS.withdrawals,
        withdrawals.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      ),
      buildCountedRoot(
        SDK.ROOT_DOMAINS.transitionTrace,
        transitionTrace.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      ),
      buildCountedRoot(
        SDK.ROOT_DOMAINS.eventToStep,
        eventToStep.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      ),
    ]);
    const header: SDK.HeaderV1 = {
      ...makeHeader(operator, startTime),
      withdrawalsRoot: withdrawalsRoot.root,
      transitionTraceRoot: traceRoot.root,
      eventToStepRoot: mappingRoot.root,
      withdrawalCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
    };
    const lifecycle = await setupChallenge({
      harness,
      publications,
      transitionTraceReferenceScripts,
      header,
    });
    const event = await mintWithdrawalEvent({
      harness,
      datum: withdrawalDatum({
        id: withdrawalId,
        inclusionTime: header.endTime + 1n,
      }),
    });
    const reconstruction = await reconstruct({
      header,
      withdrawals,
      transitionTrace,
      eventToStep,
    });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOutOfWindowSourceEventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
          eventRefInputIndex: ledgerOrderedIndex(
            [
              lifecycle.setup.hubOracle,
              transitionTraceReferenceScripts[
                "fraudProofTransitionTraceL1Event"
              ]!.utxo,
              ...[
                harness.witnessReferenceScripts.computationThreadMint,
                harness.witnessReferenceScripts.fraudProofMint,
              ].filter((utxo): utxo is UTxO => utxo !== undefined),
              event.utxo,
            ],
            event.utxo,
            "out-of-window withdrawal reference input",
          ),
          eventAssetName: event.assetName,
          validityOverride: "IncorrectWithdrawalSignature",
        },
      }),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(6);
    const proofResult = await submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      additionalReferenceInputs: [event.utxo],
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    await removeAndAssertPermanentProof({
      harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("routes a transition-step count mismatch to final 0 and removes the block", async () => {
    const { harness, publications, transitionTraceReferenceScripts } =
      await makeHarness({
        // The production state queue rejects this malformed header before a
        // fault proof can observe it. Bypass admission only; the registered
        // transition-trace chain and removal transaction remain real.
        alwaysStateQueue: true,
      });
    const header: SDK.HeaderV1 = {
      ...makeHeader(
        await funderPaymentKeyHash(harness.funderLucid),
        await alignedHeaderStart(harness),
      ),
      transitionStepCount: 1n,
    };
    const lifecycle = await setupChallenge({
      harness,
      publications,
      transitionTraceReferenceScripts,
      header,
    });
    const proof = SDK.makeTransitionFaultProof({
      challengedHeaderHash: lifecycle.setup.headerHash,
      header,
      fault: SDK.countFault("HeaderTransitionStepCountMismatch"),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(0);
    const proofResult = await submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    await removeAndAssertPermanentProof({
      harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("rejects an honest late withdrawal accused as omitted at final 6", async () => {
    const { harness, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      await alignedHeaderStart(harness),
    );
    const lifecycle = await setupChallenge({
      harness,
      publications,
      transitionTraceReferenceScripts,
      header,
    });
    const withdrawalId = transitionTraceOutRef("83");
    const event = await mintWithdrawalEvent({
      harness,
      datum: withdrawalDatum({
        id: withdrawalId,
        inclusionTime: header.endTime + 1n,
      }),
    });
    const reconstruction = await reconstruct({ header });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOmittedDueL1EventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
          eventRefInputIndex: ledgerOrderedIndex(
            [
              lifecycle.setup.hubOracle,
              transitionTraceReferenceScripts[
                "fraudProofTransitionTraceL1Event"
              ]!.utxo,
              ...[
                harness.witnessReferenceScripts.computationThreadMint,
                harness.witnessReferenceScripts.fraudProofMint,
              ].filter((utxo): utxo is UTxO => utxo !== undefined),
              event.utxo,
            ],
            event.utxo,
            "honest late withdrawal reference input",
          ),
          eventAssetName: event.assetName,
        },
      }),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(6);
    const resolved = await resolveTransitionTraceDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
    await expect(
      submitTransitionTraceProof({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: lifecycle.deploymentInfo,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(
          await firstThreadUtxo({ harness, init: lifecycle.init }),
        ),
        proof,
        additionalReferenceInputs: [event.utxo],
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow();
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.transitionTrace.finals[6]!.spendingScriptAddress,
        lifecycle.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          resolved.contracts.fraudProof.policyId,
          lifecycle.init.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);
});
