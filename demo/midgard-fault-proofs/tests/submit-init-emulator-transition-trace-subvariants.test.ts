import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
} from "@al-ft/midgard-core";
/**
 * Transition-trace representation audit for fault variants that previously
 * had only direct validator vectors. Each positive case enters through the
 * registered catalogue category, routes to the selected real final validator,
 * mints the permanent fraud-proof token, and removes the condemned block.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  getAddressDetails,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it, vi } from "vitest";

import {
  buildCountedRoot,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildTransitionFaultProof,
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  reconstructDaPayload,
  resolveTransitionTraceDeploymentContracts,
  submitRemoveFraudulentBlock,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { prepareFamilyHistory } from "./support/emulator/family-history.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
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
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";
import { publishTransitionTraceYields } from "./support/transition-trace-yields.js";

const historyRecords: unknown[] = [];
afterAll(async () => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  await mkdir(directory, { recursive: true });
  await writeFile(
    join(directory, "transition-subvariant-history.json"),
    JSON.stringify(
      {
        scope:
          "Applied semantic subvariant lifecycle, actual withdrawal history admission/removal and forced timing; fixture catalogue governance",
        blueprintSha256: createHash("sha256")
          .update(await readFile(realBlueprintPath))
          .digest("hex"),
        records: historyRecords,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

const forcedWindowMeasurements: VanRossemFitMeasurement[] = [];
const forcedWindowCases = new Set<boolean>();
afterAll(async () => {
  expect(forcedWindowCases.size).toBe(2);
  const ledger = buildVanRossemFitLedger({
    category: "transitionTrace",
    blueprintSha256: createHash("sha256")
      .update(await readFile(realBlueprintPath))
      .digest("hex"),
    compilerVersion: "aiken v1.1.23+5adf783",
    measurements: forcedWindowMeasurements,
  });
  const ledgerPath = fileURLToPath(
    new URL(
      "../../../docs/fault-proofs/size-plans/transition-trace-forced-window-fit-ledger.json",
      import.meta.url,
    ),
  );
  if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
    await writeVanRossemFitLedger(ledgerPath, ledger);
    return;
  }
  const pinned = JSON.parse(
    await readFile(ledgerPath, "utf8"),
  ) as VanRossemFitLedger;
  expect(pinned).toEqual(
    buildVanRossemFitLedger({
      category: ledger.category,
      blueprintSha256: ledger.blueprintSha256,
      compilerVersion: ledger.compilerVersion,
      measurements: pinned.entries.map((entry) => ({
        ...entry,
        memoryUnits: BigInt(entry.memoryUnits),
        cpuUnits: BigInt(entry.cpuUnits),
      })),
    }),
  );
  // Fresh execution budgets may differ; scenario and row coverage must not.
  const roster = (candidate: VanRossemFitLedger) =>
    candidate.entries.map(({ name, kind, maximumShape }) => ({
      name,
      kind,
      maximumShape,
    }));
  expect(roster(ledger)).toEqual(roster(pinned));
});

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
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

const reconstruct = async ({
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

const makeHarness = async ({
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

const setupChallenge = async ({
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

const withdrawalIdFor = (
  history: Awaited<ReturnType<typeof prepareFamilyHistory>>,
): SDK.OutputReference => {
  const nonce = history.nonce("Withdrawal");
  return {
    transactionId: nonce.txHash,
    outputIndex: BigInt(nonce.outputIndex),
  };
};

const setupWithdrawalChallenge = async ({
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

const alignedHeaderStart = async (harness: Harness, leadTime = 120_000) =>
  alignUnixTimeToEmulatorSlotBoundary(
    harness.funderLucid,
    harness.emulator.now() + leadTime,
  ) - 1;

describe("transition-trace omitted/out-of-window/count subvariant lifecycle", () => {
  it("routes an omitted due withdrawal to final 6 and removes the block", async () => {
    const { harness, history, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      await alignedHeaderStart(harness, 240_000),
    );
    const { lifecycle, event, withdrawalId } = await setupWithdrawalChallenge({
      harness,
      history,
      publications,
      transitionTraceReferenceScripts,
      header,
      inclusionTime: header.endTime,
    });

    const reconstruction = await reconstruct({ header });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOmittedDueL1EventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
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

  it.each([false, true])(
    "authenticates a late rejected order from immutable submitted bytes (wrong reason %s)",
    async (wrongReason) => {
      const original = Emulator.prototype.submitTx;
      let index = 0;
      const capture = vi
        .spyOn(Emulator.prototype, "submitTx")
        .mockImplementation(async function (this: Emulator, cbor) {
          const result = await original.call(this, cbor);
          const m = measureCompleteSignedTransaction(cbor);
          const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
          forcedWindowMeasurements.push({
            name: `${wrongReason ? "wrong-reason" : "rejected-source"}/${index++}`,
            kind: Array.from({ length: outputs.len() }, (_, i) =>
              outputs.get(i),
            ).some((output) => output.script_ref() !== undefined)
              ? "publication"
              : "lifecycle",
            maximumShape: "immutable-submission-rejected-verdict",
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          });
          return result;
        });
      try {
        const { harness, publications, transitionTraceReferenceScripts } =
          await makeHarness();
        const operator = await funderPaymentKeyHash(harness.funderLucid);
        const startTime = await alignedHeaderStart(harness);
        const id = transitionTraceOutRef("91");
        const submitted = makeNativeTx({
          spendInputCbors: [],
          outputCbors: [],
          fee: 0n,
        });
        const rawSource = deriveMidgardForcedTxProofSource(
          materializeMidgardForcedTxFromCanonical(submitted),
        );
        const rejectedSource = deriveMidgardForcedTxProofSource(
          materializeMidgardForcedTxFromCanonical(submitted),
        );
        const sourceData = (
          source: typeof rawSource,
        ): SDK.ForcedTxProofSource => ({
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        });
        expect(sourceData(rawSource).compact_cbor).toBe(
          sourceData(rejectedSource).compact_cbor,
        );
        const committed: SDK.ForcedInclusionTxV1 = {
          tx_id: computeMidgardNativeTxId(submitted).toString("hex"),
          submitted_source: sourceData(rejectedSource),
          verdict: {
            ForcedTxInvalid: {
              reason: { PlutusExecutionFailed: { execution_index: 0n } },
            },
          },
        };
        const root = await buildCountedRoot(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          [
            {
              key: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
              value: Buffer.from(
                Data.to(committed, SDK.ForcedInclusionTxV1),
                "hex",
              ),
            },
          ],
        );
        const header: SDK.Header = {
          ...makeHeader(operator, startTime),
          forcedTransactionsRoot: root.root,
          forcedTransactionCount: 1n,
          totalEventCount: 1n,
          transitionStepCount: 1n,
          validationTraceCount: 1n,
          transitionTraceRoot: "ab".repeat(32),
          eventToStepRoot: "bc".repeat(32),
          validationTracesRoot: "cd".repeat(32),
        };
        const lifecycle = await setupChallenge({
          harness,
          publications,
          transitionTraceReferenceScripts,
          header,
        });
        const assetName = "98",
          unit = toUnit(harness.contracts.txOrder.policyId, assetName);
        const datum: SDK.TxOrderDatum = {
          event: {
            id,
            tx: {
              tx_id: committed.tx_id,
              submitted_source: sourceData(rawSource),
              transaction_commitment: Buffer.from(
                computeMidgardForcedTxProofCommitment(rawSource),
              ).toString("hex"),
            },
          },
          inclusion_time: header.endTime + 1n,
          witness: "76".repeat(28),
          refund_address: {
            paymentCredential: { PublicKeyCredential: ["77".repeat(28)] },
            stakeCredential: null,
          },
          refund_datum: "NoDatum",
        };
        const signed = await (
          await harness.funderLucid
            .newTx()
            .mintAssets({ [unit]: 1n }, Data.void())
            .pay.ToContract(
              harness.contracts.txOrder.spendingScriptAddress,
              { kind: "inline", value: Data.to(datum, SDK.TxOrderDatum) },
              { lovelace: 5000000n, [unit]: 1n },
            )
            .attach.MintingPolicy(harness.contracts.txOrder.mintingScript)
            .complete()
        ).sign
          .withWallet()
          .complete();
        await harness.funderLucid.awaitTx(await signed.submit());
        const event = await expectSingleUtxoWithUnit(
          harness.funderLucid,
          harness.contracts.txOrder.spendingScriptAddress,
          unit,
        );
        const refs = [
          lifecycle.setup.hubOracle,
          transitionTraceReferenceScripts.fraudProofTransitionTraceL1Event!
            .utxo,
          harness.witnessReferenceScripts.computationThreadMint!,
          harness.witnessReferenceScripts.fraudProofMint!,
          event,
        ];
        const proof: SDK.TransitionFaultProof = {
          challenged_header_hash: lifecycle.setup.headerHash,
          header,
          fault: {
            OutOfWindowSourceEvent: {
              witness: {
                OutOfWindowForcedTransaction: {
                  event_ref_input_index: ledgerOrderedIndex(
                    refs,
                    event,
                    "late rejected order",
                  ),
                  event_asset_name: assetName,
                  validity_override: wrongReason
                    ? {
                        ForcedTxInvalid: {
                          reason: {
                            PlutusExecutionFailed: { execution_index: 1n },
                          },
                        },
                      }
                    : committed.verdict,
                  source_membership: {
                    domain: root.domain,
                    root: root.root,
                    phas_root: root.phasRoot,
                    count: root.count,
                    key: id,
                    value: committed,
                    proof: [],
                  },
                },
              },
            },
          },
        };
        const thread = await firstThreadUtxo({ harness, init: lifecycle.init });
        const run = () =>
          submitTransitionTraceProof({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            deploymentInfo: lifecycle.deploymentInfo,
            network,
            signer: harness.proverSigner,
            threadOutRef: outRefLabel(thread),
            proof,
            additionalReferenceInputs: [event],
            witnessReferenceScripts: harness.witnessReferenceScripts,
            awaitConfirmation: true,
          });
        if (wrongReason) await expect(run()).rejects.toThrow();
        else
          await removeAndAssertPermanentProof({
            harness,
            setup: lifecycle.setup,
            deploymentInfo: lifecycle.deploymentInfo,
            proofResult: await run(),
          });
        forcedWindowCases.add(wrongReason);
      } finally {
        capture.mockRestore();
      }
    },
    180000,
  );

  it("routes an out-of-window withdrawal to final 6 and removes the block", async () => {
    const { harness, history, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const operator = await funderPaymentKeyHash(harness.funderLucid);
    const startTime = await alignedHeaderStart(harness, 240_000);
    const withdrawalId = withdrawalIdFor(history);
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
    const withdrawals: SDK.DaPayloadEntry[] = [
      [
        Data.to(withdrawalId, SDK.OutputReference),
        SDK.committedWithdrawalValueBytes(committedInfo),
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
    const header: SDK.Header = {
      ...makeHeader(operator, startTime),
      withdrawalsRoot: withdrawalsRoot.root,
      transitionTraceRoot: traceRoot.root,
      eventToStepRoot: mappingRoot.root,
      withdrawalCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
    };
    const { lifecycle, event } = await setupWithdrawalChallenge({
      harness,
      history,
      publications,
      transitionTraceReferenceScripts,
      header,
      inclusionTime: header.endTime + 1000n,
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
    const header: SDK.Header = {
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
    const { harness, history, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      await alignedHeaderStart(harness, 240_000),
    );
    const { lifecycle, event, withdrawalId } = await setupWithdrawalChallenge({
      harness,
      history,
      publications,
      transitionTraceReferenceScripts,
      header,
      inclusionTime: header.endTime + 1000n,
    });

    const reconstruction = await reconstruct({ header });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOmittedDueL1EventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
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
