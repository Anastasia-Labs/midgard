import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxIdV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytesV1,
  committedWithdrawalValueBytesV1,
  type MidgardTxInput,
  type OutputReference,
  Proof,
  ROOT_DOMAINS,
  type WithdrawalInfo,
  type WithdrawalSourceMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  prepareWithdrawnInputFromMaterialV1,
  submitWithdrawnInputInit,
  submitWithdrawnInputStep01,
  submitWithdrawnInputStep02,
  type WithdrawnInputCatalogueCategoryV1,
} from "../../src/index.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import { nativeTxFromCoreCompact } from "../../src/submit-step-01.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  trieRootHex,
  WITHDRAWN_INPUT_TEST_CATEGORY_ID_V1,
} from "./submit-init-emulator-shared.js";

export type WithdrawnInputFixtureModeV1 =
  | "fault"
  | "honestDifferentWithdrawal"
  | "invalidWithdrawal";

export type WithdrawnInputBlockFixtureV1 = {
  readonly mode: WithdrawnInputFixtureModeV1;
  readonly transactionsRoot: string;
  readonly committedTransactionsRoot: string;
  readonly withdrawalsPhasRoot: string;
  readonly committedWithdrawalsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly withdrawalCount: bigint;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly spendInputs: readonly MidgardTxInput[];
  readonly badInputIndex: number;
  readonly withdrawnInput: MidgardTxInput;
  readonly committedWithdrawal: WithdrawalInfo;
  readonly claimedWithdrawal: WithdrawalInfo;
  readonly withdrawalMembership: WithdrawalSourceMembershipProof;
  readonly withdrawalId: OutputReference;
  readonly nodeTransaction: {
    readonly nodeTxId: string;
    readonly txCbor: string;
  };
};

const inputCbor = (input: MidgardTxInput): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(input.tx_id, "hex"),
    outputIndex: Number(input.output_index),
  });

const withdrawalInfo = ({
  input,
  validity,
}: {
  readonly input: MidgardTxInput;
  readonly validity: WithdrawalInfo["validity"];
}): WithdrawalInfo => ({
  body: {
    l2_outref: {
      transactionId: input.tx_id,
      outputIndex: input.output_index,
    },
    l2_owner: "21".repeat(28),
    l2_value: new Map([["31".repeat(28), new Map([["4d47", 5n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["41".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["51".repeat(32), "61".repeat(64)],
  validity,
});

export const buildWithdrawnInputBlockFixtureV1 = async (
  mode: WithdrawnInputFixtureModeV1,
): Promise<WithdrawnInputBlockFixtureV1> => {
  const withdrawnInput: MidgardTxInput = {
    tx_id: "71".repeat(32),
    output_index: 3n,
  };
  const transaction = makeNativeTx({
    spendInputCbors: [inputCbor(withdrawnInput)],
    fee: 7n,
  });
  const txId = computeMidgardNativeTxIdV1(transaction).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompactV1(transaction.compact);
  const txStore = new Store(undefined);
  await txStore.ready();
  const txTrie = new Trie(txStore);
  await txTrie.insert(Buffer.from(txId, "hex"), compactCbor);
  const txProof = await txTrie.prove(Buffer.from(txId, "hex"));
  const transactionsRoot = trieRootHex(txTrie);
  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: transactionsRoot,
      count: 1n,
    }),
  );

  const committedInput =
    mode === "honestDifferentWithdrawal"
      ? { tx_id: "72".repeat(32), output_index: 4n }
      : withdrawnInput;
  const committedWithdrawal = withdrawalInfo({
    input: committedInput,
    validity:
      mode === "invalidWithdrawal"
        ? { SpentWithdrawalUtxo: { l2_tx_id: "73".repeat(32) } }
        : "WithdrawalIsValid",
  });
  const claimedWithdrawal =
    mode === "honestDifferentWithdrawal"
      ? withdrawalInfo({ input: withdrawnInput, validity: "WithdrawalIsValid" })
      : committedWithdrawal;
  const withdrawalId: OutputReference = {
    transactionId: "74".repeat(32),
    outputIndex: 0n,
  };
  const withdrawalKey = Buffer.from(
    committedWithdrawalKeyBytesV1(withdrawalId),
    "hex",
  );
  const withdrawalValue = Buffer.from(
    committedWithdrawalValueBytesV1(committedWithdrawal),
    "hex",
  );
  const withdrawalStore = new Store(undefined);
  await withdrawalStore.ready();
  const withdrawalTrie = new Trie(withdrawalStore);
  await withdrawalTrie.insert(withdrawalKey, withdrawalValue);
  const withdrawalProof = await withdrawalTrie.prove(withdrawalKey);
  const withdrawalsPhasRoot = trieRootHex(withdrawalTrie);
  const committedWithdrawalsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.withdrawals,
      phasRoot: withdrawalsPhasRoot,
      count: 1n,
    }),
  );
  const txInclusion: SubmitStep01TxInclusion = {
    nativeTxId: txId,
    nativeTx: nativeTxFromCoreCompact(transaction.compact),
    nativeTxCompactCbor: compactCbor.toString("hex"),
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(txProof.toCBOR().toString("hex"), Proof),
    txMembershipProofCbor: txProof.toCBOR().toString("hex"),
  };
  const withdrawalMembership: WithdrawalSourceMembershipProof = {
    domain: ROOT_DOMAINS.withdrawals,
    root: committedWithdrawalsRoot,
    phas_root: withdrawalsPhasRoot,
    count: 1n,
    key: withdrawalId,
    value: claimedWithdrawal,
    proof: Data.from(withdrawalProof.toCBOR().toString("hex"), Proof),
  };
  const nodeTransaction = {
    nodeTxId: txId,
    txCbor: encodeMidgardNativeTxCanonicalV1(transaction).toString("hex"),
  };
  if (mode === "fault") {
    const prepared = await prepareWithdrawnInputFromMaterialV1({
      headerHash: "75".repeat(28),
      transactions: [nodeTransaction],
      expectedTransactionsRoot: committedTransactionsRoot,
      withdrawals: [{ key: withdrawalId, value: committedWithdrawal }],
      expectedWithdrawalsRoot: committedWithdrawalsRoot,
    });
    if (
      prepared.withdrawnInput.tx_id !== withdrawnInput.tx_id ||
      prepared.withdrawnInput.output_index !== withdrawnInput.output_index
    ) {
      throw new Error("withdrawn-input preparer selected the wrong input");
    }
  }
  return {
    mode,
    transactionsRoot,
    committedTransactionsRoot,
    withdrawalsPhasRoot,
    committedWithdrawalsRoot,
    l2TransactionCount: 1n,
    withdrawalCount: 1n,
    txInclusion,
    spendInputs: [withdrawnInput],
    badInputIndex: 0,
    withdrawnInput,
    committedWithdrawal,
    claimedWithdrawal,
    withdrawalMembership,
    withdrawalId,
    nodeTransaction,
  };
};

export const makeWithdrawnInputEmulatorScenarioV1 = async (
  mode: WithdrawnInputFixtureModeV1,
) => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realWithdrawnInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = harness.contracts.withdrawnInput;
  const category = harness.catalogue.extraCategories.withdrawnInput;
  if (contracts === undefined || category === undefined) {
    throw new Error("withdrawn-input emulator contracts/category missing");
  }
  const fixture = await buildWithdrawnInputBlockFixtureV1(mode);
  const references: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: step.spendingScript,
          label: `withdrawn-input step 0${(index + 1).toString()}`,
        })
      ).utxo,
    );
  }
  const [step01Reference, step02Reference, step03Reference] = references;
  if (
    step01Reference === undefined ||
    step02Reference === undefined ||
    step03Reference === undefined
  ) {
    throw new Error("withdrawn-input reference-script publication incomplete");
  }
  const operatorKeyHash = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header = makeHeader(
    operatorKeyHash,
    startTime,
    fixture.committedTransactionsRoot,
    fixture.l2TransactionCount,
    fixture.committedWithdrawalsRoot,
    fixture.withdrawalCount,
  );
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: (await harness.funderLucid.wallet().getUtxos())[0]!,
    catalogue: harness.catalogue,
    header,
  });
  const explicitCategory: WithdrawnInputCatalogueCategoryV1 = {
    categoryId: WITHDRAWN_INPUT_TEST_CATEGORY_ID_V1,
    scriptHash: contracts.steps[0].spendingScriptHash,
    membershipProofCbor: category.membershipProofCbor,
  };
  const init = await submitWithdrawnInputInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts,
    category: explicitCategory,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
  });
  return {
    harness,
    contracts,
    category: explicitCategory,
    fixture,
    references: [step01Reference, step02Reference, step03Reference] as const,
    header,
    setup,
    init,
  };
};

export const advanceWithdrawnInputToStep03V1 = async (
  scenario: Awaited<ReturnType<typeof makeWithdrawnInputEmulatorScenarioV1>>,
) => {
  const step01 = await submitWithdrawnInputStep01({
    lucid: scenario.harness.proverLucid,
    blueprint: scenario.harness.realBlueprint,
    contracts: scenario.contracts,
    categoryId: scenario.category.categoryId,
    network,
    signer: scenario.harness.proverSigner,
    threadOutRef: scenario.init.nextThreadOutRef,
    stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    txInclusion: scenario.fixture.txInclusion,
    referenceScriptUtxo: scenario.references[0],
  });
  const step02 = await submitWithdrawnInputStep02({
    lucid: scenario.harness.proverLucid,
    contracts: scenario.contracts,
    categoryId: scenario.category.categoryId,
    signer: scenario.harness.proverSigner,
    threadOutRef: step01.nextThreadOutRef,
    evidence: {
      inputs: scenario.fixture.spendInputs,
      badInputIndex: scenario.fixture.badInputIndex,
      nativeTxCompactCbor: scenario.fixture.txInclusion.nativeTxCompactCbor,
    },
    referenceScriptUtxo: scenario.references[1],
  });
  return { step01, step02 };
};
