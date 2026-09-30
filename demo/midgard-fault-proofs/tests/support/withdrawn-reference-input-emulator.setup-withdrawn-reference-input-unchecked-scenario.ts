import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../../src/transition-trace/phas.js";
import type { WithdrawnReferenceInputContracts } from "../../src/withdrawn-reference-input/contracts.js";
import {
  type PreparedWithdrawnReferenceInput,
  prepareWithdrawnReferenceInput,
} from "../../src/withdrawn-reference-input/prepare-withdrawn-reference-input.js";
import { decodingSubjectTransaction } from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeHeader,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF: SDK.MidgardTxInput = {
  tx_id: "ab".repeat(32),
  output_index: 0n,
};

export const withdrawnReferenceInputInfo = ({
  outRef = WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF,
  validity = "WithdrawalIsValid",
}: {
  readonly outRef?: SDK.MidgardTxInput;
  readonly validity?: SDK.WithdrawalValidity;
} = {}): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: {
      transactionId: outRef.tx_id,
      outputIndex: outRef.output_index,
    },
    l2_owner: "55".repeat(28),
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["66".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["77".repeat(32), "88".repeat(64)],
  validity,
});

export type WithdrawnReferenceInputEmulatorHarness = Awaited<
  ReturnType<typeof makeWithdrawnReferenceInputEmulatorHarness>
>;

export const makeWithdrawnReferenceInputEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realWithdrawnReferenceInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.withdrawnReferenceInput;
  const category = harness.catalogue.categories.withdrawnReferenceInput;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the withdrawn-reference-input contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnReferenceInput
  ) {
    throw new Error("Unexpected withdrawn-reference-input category id");
  }
  return { ...harness, family, category };
};

export const publishWithdrawnReferenceInputReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: WithdrawnReferenceInputContracts;
}): Promise<readonly [UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `withdrawn-reference-input step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO];
};

export type WithdrawnReferenceInputScenario = {
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly blockTxs: readonly [
    { readonly nodeTxId: string; readonly txCbor: string },
  ];
  readonly withdrawals: readonly [SDK.WithdrawalEvent];
  readonly prepared: PreparedWithdrawnReferenceInput;
};

export const setupWithdrawnReferenceInputScenario = async ({
  harness,
  withdrawalInfo = withdrawnReferenceInputInfo(),
  decoyReferenceInputCount = 0,
}: {
  readonly harness: WithdrawnReferenceInputEmulatorHarness;
  readonly withdrawalInfo?: SDK.WithdrawalInfo;
  /**
   * Pads the committed field-1 preimage (each item a constant 40 §5.1 bytes)
   * so a caller can size the field past §8.4's tier-1 bound; decoys are never
   * in the withdrawals set, so the accused reference input stays the fault.
   */
  readonly decoyReferenceInputCount?: number;
}): Promise<WithdrawnReferenceInputScenario> => {
  const referenceInputCbors = [
    WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF,
    ...Array.from(
      { length: decoyReferenceInputCount },
      (_unused, index): SDK.MidgardTxInput => ({
        tx_id: (index + 1).toString(16).padStart(64, "0"),
        output_index: 0n,
      }),
    ),
  ]
    .map(SDK.encodeMidgardTxInputCanonical)
    .sort(Buffer.compare);
  const nativeTx = decodingSubjectTransaction({
    referenceInputCbors,
    fee: 1_000n,
  });
  const txId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const fullCbor = encodeMidgardNativeTxCanonical(nativeTx);
  // The header's normative transactions MPF commits
  // `Data(L2TransactionSourceV1)` per transaction id, which is the value the
  // preparer recounts, so the committed leaf is the source value rather than
  // the bare compact CBOR.
  const countedTransactions = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    [
      {
        key: Buffer.from(txId, "hex"),
        value: Buffer.from(l2TransactionSourceCborV1(nativeTx), "hex"),
      },
    ],
  );
  const withdrawal: SDK.WithdrawalEvent = {
    id: { transactionId: "42".repeat(32), outputIndex: 0n },
    info: withdrawalInfo,
  };
  const countedWithdrawals = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    [
      {
        key: Buffer.from(SDK.committedWithdrawalKeyBytes(withdrawal.id), "hex"),
        value: Buffer.from(
          SDK.committedWithdrawalValueBytes(withdrawal.info),
          "hex",
        ),
      },
    ],
  );
  const funderKeyHash = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
    ...makeHeader(funderKeyHash, startTime, countedTransactions.root, 1n),
    withdrawalsRoot: countedWithdrawals.root,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
    validationTraceCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const blockTxs = [
    { nodeTxId: txId, txCbor: fullCbor.toString("hex") },
  ] as const;
  const withdrawals = [withdrawal] as const;
  const prepared = await prepareWithdrawnReferenceInput({
    header,
    blockTxs,
    withdrawals,
    accusedTxId: txId,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  return { header, headerHash, setup, blockTxs, withdrawals, prepared };
};

/**
 * Commits an arbitrary withdrawal leaf while retaining valid tx-inclusion and
 * membership material. Adversarial tests use this to bypass the honest
 * classifier and reach the validator's exact refusal checks.
 */
export const setupWithdrawnReferenceInputUncheckedScenario = async ({
  harness,
  withdrawalInfo,
}: {
  readonly harness: WithdrawnReferenceInputEmulatorHarness;
  readonly withdrawalInfo: SDK.WithdrawalInfo;
}) => {
  const referenceInputs = [WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF] as const;
  const nativeTx = decodingSubjectTransaction({
    referenceInputCbors: [
      SDK.encodeMidgardTxInputCanonical(referenceInputs[0]),
    ],
    fee: 1_000n,
  });
  const txId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
  const l2TransactionSourceCbor = l2TransactionSourceCborV1(nativeTx);
  const countedTransactions = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    [
      {
        key: Buffer.from(txId, "hex"),
        value: Buffer.from(l2TransactionSourceCbor, "hex"),
      },
    ],
  );
  const txProof = await keyValuePhasProof(
    { ...countedTransactions, root: countedTransactions.phasRoot },
    Buffer.from(txId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const withdrawal: SDK.WithdrawalEvent = {
    id: { transactionId: "42".repeat(32), outputIndex: 0n },
    info: withdrawalInfo,
  };
  const withdrawalKey = Buffer.from(
    SDK.committedWithdrawalKeyBytes(withdrawal.id),
    "hex",
  );
  const withdrawalValue = Buffer.from(
    SDK.committedWithdrawalValueBytes(withdrawal.info),
    "hex",
  );
  const countedWithdrawals = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    [{ key: withdrawalKey, value: withdrawalValue }],
  );
  const withdrawalProof = await keyValuePhasProof(
    { ...countedWithdrawals, root: countedWithdrawals.phasRoot },
    withdrawalKey,
    withdrawalValue,
  );
  const funderKeyHash = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
    ...makeHeader(funderKeyHash, startTime, countedTransactions.root, 1n),
    withdrawalsRoot: countedWithdrawals.root,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
    validationTraceCount: 1n,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  const blockTxs = [
    {
      nodeTxId: txId,
      txCbor: encodeMidgardNativeTxCanonical(nativeTx).toString("hex"),
    },
  ] as const;
  const withdrawals = [withdrawal] as const;
  return {
    header,
    setup,
    referenceInputs,
    txInclusion: {
      nativeTxId: txId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor,
      transactionsPhasRoot: countedTransactions.phasRoot,
      txMembershipProof: txProof,
      txMembershipProofCbor: Data.to(txProof, SDK.Proof),
    },
    withdrawal,
    blockTxs,
    withdrawals,
    withdrawalMembership: {
      domain: SDK.ROOT_DOMAINS.withdrawals,
      root: countedWithdrawals.root,
      phas_root: countedWithdrawals.phasRoot,
      count: 1n,
      key: withdrawal.id,
      value: withdrawal.info,
      proof: withdrawalProof,
    } satisfies SDK.WithdrawalSourceMembershipProof,
  };
};
