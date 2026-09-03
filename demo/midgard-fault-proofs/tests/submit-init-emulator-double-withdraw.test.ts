/**
 * Both-polarity emulator coverage for the registered `double-withdraw`
 * family: payable duplicate -> permanent proof -> block removal, and honest
 * non-payable duplicate / same-leaf adversaries refused in the terminal script.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { DoubleWithdrawContracts } from "../src/double-withdraw/contracts-v1.js";
import {
  submitDoubleWithdrawCancel,
  submitDoubleWithdrawInit,
  submitDoubleWithdrawStep01,
  submitDoubleWithdrawStep02,
} from "../src/double-withdraw/index.js";
import {
  deriveDoubleWithdrawMembership,
  parseSubmitDoubleWithdrawInclusion,
  type SubmitDoubleWithdrawInclusion,
} from "../src/double-withdraw/submit-double-withdraw-step-01.js";
import { prepareDoubleWithdrawFromCommittedLeaves } from "../src/prepare-double-withdraw.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { fetchUtxoByOutRef, parseOutRef } from "../src/runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../src/submit-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import { outputWithDatumAndUnitPredicate } from "../src/tx-layout.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const FIRST_ID: SDK.OutputReference = {
  transactionId: "8b".repeat(32),
  outputIndex: 2n,
};
const SECOND_ID: SDK.OutputReference = {
  transactionId: "c4".repeat(32),
  outputIndex: 1n,
};
const SHARED_OUTREF: SDK.OutputReference = {
  transactionId: "7e".repeat(32),
  outputIndex: 1n,
};
const PAYABLE_INFO: SDK.WithdrawalInfo = {
  body: {
    l2_outref: SHARED_OUTREF,
    l2_owner: "9c".repeat(28),
    l2_value: new Map([["4b".repeat(28), new Map([["6d696467617264", 42n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["2b".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
};
const HONEST_DUPLICATE_INFO: SDK.WithdrawalInfo = {
  ...PAYABLE_INFO,
  validity: { SpentWithdrawalUtxo: { l2_tx_id: "5a".repeat(32) } },
};

const entry = (
  id: SDK.OutputReference,
  info: SDK.WithdrawalInfo,
): readonly [string, string] => [
  SDK.committedWithdrawalKeyBytes(id),
  SDK.committedWithdrawalValueBytes(info),
];

const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realDoubleWithdraw: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const doubleWithdraw = harness.contracts.doubleWithdraw;
  const category = harness.catalogue.categories.doubleWithdraw;
  if (doubleWithdraw === undefined || category === undefined) {
    throw new Error("double-withdraw harness contracts/category missing");
  }
  expect(category.categoryId).toBe(
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleWithdraw,
  );
  expect(category.scriptHash).toBe(doubleWithdraw.steps[0].spendingScriptHash);
  return { ...harness, doubleWithdraw, category };
};

const setupBlock = async ({
  harness,
  secondInfo,
}: {
  readonly harness: Awaited<ReturnType<typeof makeHarness>>;
  readonly secondInfo: SDK.WithdrawalInfo;
}) => {
  const entries = [entry(FIRST_ID, PAYABLE_INFO), entry(SECOND_ID, secondInfo)];
  const counted = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const start =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
    ...makeHeader(operatorVkey, start),
    withdrawalsRoot: counted.root,
    withdrawalCount: counted.count,
    totalEventCount: counted.count,
    transitionStepCount: counted.count,
    transitionTraceRoot: counted.root,
    eventToStepRoot: counted.root,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  return { entries, counted, header, setup };
};

const publishStepReferences = async ({
  lucid,
  contracts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DoubleWithdrawContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const first = await publishPlainReferenceScriptUtxo({
    lucid,
    script: contracts.steps[0].spendingScript,
    label: "double-withdraw step-01",
  });
  const second = await publishPlainReferenceScriptUtxo({
    lucid,
    script: contracts.steps[1].spendingScript,
    label: "double-withdraw step-02",
  });
  return [first.utxo, second.utxo];
};

const inclusionFor = async ({
  counted,
  leaf,
}: {
  readonly counted: Awaited<ReturnType<typeof buildCountedRoot>>;
  readonly leaf: readonly [string, string];
}): Promise<SubmitDoubleWithdrawInclusion> => {
  const proof = await keyValuePhasProof(
    { ...counted, root: counted.phasRoot },
    Buffer.from(leaf[0], "hex"),
    Buffer.from(leaf[1], "hex"),
  );
  return parseSubmitDoubleWithdrawInclusion({
    withdrawalIdCbor: leaf[0],
    withdrawalInfoCbor: leaf[1],
    withdrawalsPhasRoot: counted.phasRoot,
    withdrawalMembershipProofCbor: Data.to(proof, SDK.Proof),
  });
};

/** Terminal transaction without the submitter's decisive local rule check. */
const submitRawTerminal = async ({
  harness,
  signer,
  threadOutRef,
  blockOutRef,
  inclusion,
  referenceScript,
}: {
  readonly harness: Awaited<ReturnType<typeof makeHarness>>;
  readonly signer: typeof harness.proverSigner;
  readonly threadOutRef: string;
  readonly blockOutRef: string;
  readonly inclusion: SubmitDoubleWithdrawInclusion;
  readonly referenceScript: UTxO;
}): Promise<string> => {
  const { doubleWithdraw: contracts, category, proverLucid: lucid } = harness;
  const computationThreadReference =
    harness.witnessReferenceScripts.computationThreadMint;
  const fraudProofReference = harness.witnessReferenceScripts.fraudProofMint;
  if (
    computationThreadReference === undefined ||
    fraudProofReference === undefined
  ) {
    throw new Error("double-withdraw witness reference scripts missing");
  }
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "raw terminal thread"),
    label: "raw double-withdraw terminal thread",
  });
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: category.categoryId,
    categoryLabel: "double-withdraw",
  });
  const [hubOracleUtxo, blockUtxo] = await Promise.all([
    expectSingleUtxoWithUnit(
      lucid,
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      toUnit(contracts.hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
    ),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(blockOutRef, "raw terminal block"),
      label: "raw terminal state-queue block",
    }),
  ]);
  const node = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(blockUtxo),
  );
  const header = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(node),
  );
  const { committedWithdrawal } = await deriveDoubleWithdrawMembership({
    header,
    inclusion,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let outputIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    const ownInputIndex = SDK.requireInputIndex(
      ctx,
      threadUtxo,
      "raw double-withdraw terminal",
    );
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      fraudProofOutputMatches,
      "raw double-withdraw fraud-proof output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: ownInputIndex,
            output_index: outputIndex,
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw double-withdraw fraud-proof mint",
            ),
            hub_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              hubOracleUtxo,
              "raw double-withdraw hub",
            ),
            state_queue_node_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              blockUtxo,
              "raw double-withdraw block",
            ),
            committed_withdrawal: committedWithdrawal,
          },
        ],
      },
      SDK.DoubleWithdrawStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw terminal thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const proofMint = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw terminal fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw terminal thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const terminalBase = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      hubOracleUtxo,
      blockUtxo,
      referenceScript,
      computationThreadReference,
      fraudProofReference,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurn)
    .mintAssets({ [fraudProofUnit]: 1n }, proofMint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await terminalBase.complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error("raw terminal layout missing");
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

const submitRawCancel = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  categoryId,
  referenceScript,
  computationThreadReference,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DoubleWithdrawContracts;
  readonly signer: Awaited<ReturnType<typeof makeHarness>>["proverSigner"];
  readonly threadUtxo: UTxO;
  readonly categoryId: string;
  readonly referenceScript: UTxO;
  readonly computationThreadReference: UTxO;
}): Promise<string> => {
  const rawCancelSchema = SDK.faultProofStepRedeemerSchema(Data.Any());
  type RawCancelRedeemer = Data.Static<typeof rawCancelSchema>;
  const RawCancelRedeemer = rawCancelSchema as unknown as RawCancelRedeemer;
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: "double-withdraw",
  });
  signer.selectWallet(lucid);
  const fee = selectFeeInput(await lucid.wallet().getUtxos());
  const spend = ((ctx) =>
    Data.to(
      {
        Cancel: {
          input_index: SDK.requireInputIndex(ctx, threadUtxo, "raw cancel"),
          computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw cancel burn",
          ),
        },
      },
      RawCancelRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: token.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const cancelBase = lucid
    .newTx()
    .collectFrom([fee])
    .collectFrom([threadUtxo], spend)
    .readFrom([referenceScript, computationThreadReference])
    .mintAssets({ [token.unit]: -1n }, burn)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await cancelBase.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

describe("double-withdraw emulator lifecycle", () => {
  it("proves the payable duplicate, resumes from step-02, and removes the fraudulent block", async () => {
    const harness = await makeHarness();
    const block = await setupBlock({ harness, secondInfo: PAYABLE_INFO });
    const refs = await publishStepReferences({
      lucid: harness.funderLucid,
      contracts: harness.doubleWithdraw,
    });
    const plan = await prepareDoubleWithdrawFromCommittedLeaves({
      headerHash: block.setup.headerHash,
      committedWithdrawalsRoot: block.counted.root,
      withdrawalCount: block.counted.count,
      entries: block.entries,
    });
    expect(plan.firstLeaf.withdrawalId).toEqual(FIRST_ID);
    expect(plan.secondLeaf.withdrawalId).toEqual(SECOND_ID);

    const init = await submitDoubleWithdrawInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.doubleWithdraw,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitDoubleWithdrawStep01({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
      inclusion: parseSubmitDoubleWithdrawInclusion(plan.firstInclusion),
      referenceScriptUtxo: refs[0],
    });
    // Crash/resume surface: everything required is re-read from the surviving
    // thread, state-queue node and retained prepared inclusion.
    const resumedThread = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step01.secondStepAddress,
      init.computationThreadUnit,
    );
    expect(outRefLabel(resumedThread)).toBe(step01.nextThreadOutRef);
    const terminal = await submitDoubleWithdrawStep02({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(resumedThread),
      stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
      inclusion: parseSubmitDoubleWithdrawInclusion(plan.secondInclusion),
      referenceScriptUtxo: refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        step01.secondStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      terminal.fraudProofAddress,
      terminal.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(terminal.fraudProofOutRef);

    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const now = BigInt(harness.emulator.now());
    const removed = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: deployment,
      network,
      signer: harness.proverSigner,
      fraudCategory: "doubleWithdraw",
      fraudulentHeaderHash: block.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removed.fraudCategory).toBe("doubleWithdraw");
    expect(removed.transactions[0]?.slashingApproach).toBe(
      "SlashActiveOperator",
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        block.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      terminal.fraudProofAddress,
      terminal.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(terminal.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: harness.proverSigner,
        fraudCategory: "doubleWithdraw",
        fraudulentHeaderHash: block.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);

  it("refuses an honest non-payable duplicate and same-leaf pairing on chain, and enforces cancel ownership", async () => {
    const harness = await makeHarness();
    const block = await setupBlock({
      harness,
      secondInfo: HONEST_DUPLICATE_INFO,
    });
    const refs = await publishStepReferences({
      lucid: harness.funderLucid,
      contracts: harness.doubleWithdraw,
    });
    await expect(
      prepareDoubleWithdrawFromCommittedLeaves({
        headerHash: block.setup.headerHash,
        committedWithdrawalsRoot: block.counted.root,
        withdrawalCount: block.counted.count,
        entries: block.entries,
      }),
    ).rejects.toThrow(/no_payable_duplicate_pair/u);
    const [firstInclusion, secondInclusion] = await Promise.all([
      inclusionFor({ counted: block.counted, leaf: block.entries[0]! }),
      inclusionFor({ counted: block.counted, leaf: block.entries[1]! }),
    ]);
    const init = await submitDoubleWithdrawInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.doubleWithdraw,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitDoubleWithdrawStep01({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
      inclusion: firstInclusion,
      referenceScriptUtxo: refs[0],
    });
    await expect(
      submitDoubleWithdrawStep01({
        lucid: harness.proverLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: firstInclusion,
        referenceScriptUtxo: refs[0],
      }),
    ).rejects.toThrow(/not locked at double-withdraw step 01/u);
    await expect(
      submitDoubleWithdrawStep02({
        lucid: harness.proverLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: secondInclusion,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/second leaf is identical.*not payable/su);
    expect(
      await expectOnchainRefusal(() =>
        submitRawTerminal({
          harness,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          blockOutRef: block.setup.fraudulentBlockOutRef,
          inclusion: secondInclusion,
          referenceScript: refs[1],
        }),
      ),
    ).not.toBe("");
    await expect(
      submitDoubleWithdrawStep02({
        lucid: harness.proverLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: firstInclusion,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/second leaf is identical/u);
    expect(
      await expectOnchainRefusal(() =>
        submitRawTerminal({
          harness,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          blockOutRef: block.setup.fraudulentBlockOutRef,
          inclusion: firstInclusion,
          referenceScript: refs[1],
        }),
      ),
    ).not.toBe("");

    const outsider = generateEmulatorAccount({ lovelace: 0n });
    const outsiderLucid = await Lucid(harness.emulator, "Custom");
    outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
    const outsiderSigner = (
      await import("../src/runtime.js")
    ).resolveProverSigner({
      network,
      walletSeedPhrase: outsider.seedPhrase,
    });
    // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
    // derives the seed's base address while `resolveProverSigner` derives its
    // enterprise address, and the cancel submitter re-selects through the
    // signer, so funding only the base address strands the transaction.
    const funding = await harness.funderLucid
      .newTx()
      .pay.ToAddress(await outsiderLucid.wallet().address(), {
        lovelace: 1_000_000_000n,
      })
      .pay.ToAddress(outsiderSigner.address, { lovelace: 1_000_000_000n })
      .pay.ToAddress(outsiderSigner.address, { lovelace: 1_000_000_000n })
      .complete();
    await harness.funderLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    await expect(
      submitDoubleWithdrawCancel({
        lucid: outsiderLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        signer: outsiderSigner,
        threadOutRef: step01.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/only the prover can cancel/u);
    await expect(
      submitDoubleWithdrawStep02({
        lucid: outsiderLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: outsiderSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: secondInclusion,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/not the signing wallet/u);
    const threadUtxo = await fetchUtxoByOutRef({
      lucid: harness.proverLucid,
      outRef: parseOutRef(step01.nextThreadOutRef, "thread"),
      label: "double-withdraw step-02 thread",
    });
    expect(
      await expectOnchainRefusal(() =>
        submitRawCancel({
          lucid: outsiderLucid,
          contracts: harness.doubleWithdraw,
          signer: outsiderSigner,
          threadUtxo,
          categoryId: harness.category.categoryId,
          referenceScript: refs[1],
          computationThreadReference:
            harness.witnessReferenceScripts.computationThreadMint!,
        }),
      ),
    ).not.toBe("");
    const cancelled = await submitDoubleWithdrawCancel({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.doubleWithdraw.steps[1].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    // Re-init after the cancelled NFT burn and exercise the step-01 cancel arm.
    const retry = await submitDoubleWithdrawInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.doubleWithdraw,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const cancelledAtEntry = await submitDoubleWithdrawCancel({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: retry.nextThreadOutRef,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelledAtEntry.cancelledStepIndex).toBe(0);
  }, 600_000);
});
