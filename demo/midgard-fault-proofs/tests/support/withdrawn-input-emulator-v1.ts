import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
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
  prepareWithdrawnInputFromMaterial,
  submitWithdrawnInputInit,
  submitWithdrawnInputStep01,
  submitWithdrawnInputStep02,
  type WithdrawnInputCatalogueCategory,
} from "../../src/index.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import { nativeTxFromCoreCompact } from "../../src/submit-step-01.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeHeader,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export type WithdrawnInputFixtureMode =
  | "fault"
  | "honestDifferentWithdrawal"
  | "invalidWithdrawal";

export type WithdrawnInputBlockFixture = {
  readonly mode: WithdrawnInputFixtureMode;
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
  encodeMidgardSpendInputItem({
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

export const buildWithdrawnInputBlockFixture = async (
  mode: WithdrawnInputFixtureMode,
  { decoySpendInputCount = 0 }: { readonly decoySpendInputCount?: number } = {},
): Promise<WithdrawnInputBlockFixture> => {
  const withdrawnInput: MidgardTxInput = {
    tx_id: "71".repeat(32),
    output_index: 3n,
  };
  // Decoys pad the committed field-0 preimage (each item a constant 40 §5.1
  // bytes) so a caller can size the field past §8.4's tier-1 bound; they are
  // never in the withdrawals set, so the withdrawn input stays the fault.
  const spendInputs = [
    withdrawnInput,
    ...Array.from({ length: decoySpendInputCount }, (_unused, index) => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    })),
  ].sort((left, right) => Buffer.compare(inputCbor(left), inputCbor(right)));
  const badInputIndex = spendInputs.findIndex(
    (input) =>
      input.tx_id === withdrawnInput.tx_id &&
      input.output_index === withdrawnInput.output_index,
  );
  if (badInputIndex < 0) {
    throw new Error("withdrawn input missing from the canonical spend list");
  }
  const transaction = makeNativeTx({
    spendInputCbors: spendInputs.map(inputCbor),
    fee: 7n,
  });
  const txId = computeMidgardNativeTxId(transaction).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(transaction.compact);
  const l2TransactionSourceCbor = l2TransactionSourceCborV1(transaction);
  const txStore = new Store(undefined);
  await txStore.ready();
  const txTrie = new Trie(txStore);
  await txTrie.insert(
    Buffer.from(txId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
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
    committedWithdrawalKeyBytes(withdrawalId),
    "hex",
  );
  const withdrawalValue = Buffer.from(
    committedWithdrawalValueBytes(committedWithdrawal),
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
    l2TransactionSourceCbor,
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
    txCbor: encodeMidgardNativeTxCanonical(transaction).toString("hex"),
  };
  if (mode === "fault") {
    const prepared = await prepareWithdrawnInputFromMaterial({
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
    spendInputs,
    badInputIndex,
    withdrawnInput,
    committedWithdrawal,
    claimedWithdrawal,
    withdrawalMembership,
    withdrawalId,
    nodeTransaction,
  };
};

export const makeWithdrawnInputEmulatorScenario = async (
  mode: WithdrawnInputFixtureMode,
  fixtureOptions: { readonly decoySpendInputCount?: number } = {},
) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realWithdrawnInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = harness.contracts.withdrawnInput;
  const category = harness.catalogue.categories.withdrawnInput;
  if (contracts === undefined || category === undefined) {
    throw new Error("withdrawn-input emulator contracts/category missing");
  }
  const fixture = await buildWithdrawnInputBlockFixture(mode, fixtureOptions);
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
  const explicitCategory: WithdrawnInputCatalogueCategory = {
    categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnInput,
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
    witnessReferenceScripts: harness.witnessReferenceScripts,
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

export const advanceWithdrawnInputToStep03 = async (
  scenario: Awaited<ReturnType<typeof makeWithdrawnInputEmulatorScenario>>,
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
    witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
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
