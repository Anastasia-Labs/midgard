import * as SDK from "@/reserve-payout/primitives.js";
import { type Assets, CML, Data, type UTxO } from "@lucid-evolution/lucid";

import {
  assetsEqual,
  hasNonZeroAssetQuantity,
} from "@/reserve-payout/assets.js";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  type IndexedTxOutput,
} from "@/tx-out-ref-order.js";
import {
  compareOutRefs,
  findOutRefIndex,
  outRefLabel,
} from "@al-ft/midgard-core/out-ref";

export type AbsorbDepositLayout = {
  readonly depositInputIndex: bigint;
  readonly reserveOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly witnessUnregistrationRedeemerIndex: bigint;
  readonly inclusionProofWithdrawalRedeemerIndex: bigint;
};

export type InitializePayoutLayout = {
  readonly withdrawalInputIndex: bigint;
  readonly payoutOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly withdrawalBurnRedeemerIndex: bigint;
  readonly payoutMintRedeemerIndex: bigint;
  readonly withdrawalSpendRedeemerIndex: bigint;
  readonly witnessUnregistrationRedeemerIndex: bigint;
  readonly inclusionProofWithdrawalRedeemerIndex: bigint;
};

export type AddReserveFundsLayout = {
  readonly payoutInputIndex: bigint;
  readonly reserveInputIndex: bigint;
  readonly payoutOutputIndex: bigint;
  readonly reserveChangeOutputIndex: bigint | null;
  readonly payoutSpendRedeemerIndex: bigint;
  readonly reserveSpendRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export type ConcludePayoutLayout = {
  readonly payoutInputIndex: bigint;
  readonly l1OutputIndex: bigint;
  readonly payoutSpendRedeemerIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export type RefundWithdrawalLayout = {
  readonly withdrawalInputIndex: bigint;
  readonly refundOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly witnessUnregistrationRedeemerIndex: bigint;
  readonly inclusionProofWithdrawalRedeemerIndex: bigint;
};

const expectedTxInfoIndex = (
  pointers: readonly SDK.RedeemerPointer[],
  target: SDK.RedeemerPointer,
): bigint => SDK.resolveRedeemerTxInfoIndex({ pointers, target });

const actualTxInfoIndex = (
  tx: CML.Transaction,
  target: SDK.RedeemerPointer,
): bigint =>
  SDK.resolveRedeemerTxInfoIndex({
    pointers: SDK.getRedeemerPointersInContextOrder(tx),
    target,
  });

const mintPointerIndex = (
  policyIds: readonly string[],
  targetPolicyId: string,
): bigint => SDK.resolveMintPolicyContextIndex({ policyIds, targetPolicyId });

const requireReferenceInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
): bigint => {
  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    throw new Error("Transaction did not include reference inputs");
  }
  const index = findOutRefIndex(
    collectSortedInputOutRefs(referenceInputs),
    target,
  );
  if (index === undefined) {
    throw new Error(
      `Reference input ${outRefLabel(target)} missing from transaction`,
    );
  }
  return BigInt(index);
};

const requireInputIndex = (tx: CML.Transaction, target: UTxO): bigint => {
  const index = findOutRefIndex(
    collectSortedInputOutRefs(tx.body().inputs()),
    target,
  );
  if (index === undefined) {
    throw new Error(`Input ${outRefLabel(target)} missing from transaction`);
  }
  return BigInt(index);
};

const requireOutput = (
  tx: CML.Transaction,
  predicate: (output: IndexedTxOutput) => boolean,
  description: string,
): IndexedTxOutput => {
  const matches = collectIndexedOutputs(tx.body().outputs()).filter(predicate);
  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one ${description} output, found ${matches.length.toString()}`,
    );
  }
  return matches[0]!;
};

const outputHasNoDatum = (output: IndexedTxOutput): boolean =>
  output.datum === undefined && output.datumHash === undefined;

const outputDatumMatches = (
  output: IndexedTxOutput,
  datum: SDK.CardanoDatum,
): boolean => {
  if (datum === "NoDatum") {
    return outputHasNoDatum(output);
  }
  if ("DatumHash" in datum) {
    return (
      output.datumHash === datum.DatumHash.hash && output.datum === undefined
    );
  }
  return (
    output.datum === Data.to(datum.InlineDatum.data as any, Data.Any() as any)
  );
};

export const settlementDatumFromInput = (
  settlementRefInput: UTxO,
): SDK.SettlementDatum => {
  if (settlementRefInput.datum == null) {
    throw new Error(
      `Settlement reference input ${outRefLabel(settlementRefInput)} has no inline datum`,
    );
  }
  return Data.from(
    settlementRefInput.datum,
    SDK.SettlementDatum,
  ) as SDK.SettlementDatum;
};

const sameLayout = <L extends object>(left: L, right: L): boolean =>
  Object.entries(left).every(([key, value]) => right[key as keyof L] === value);

export const sameAbsorbDepositLayout = sameLayout<AbsorbDepositLayout>;
export const sameInitializePayoutLayout = sameLayout<InitializePayoutLayout>;
export const sameAddReserveFundsLayout = sameLayout<AddReserveFundsLayout>;
export const sameConcludePayoutLayout = sameLayout<ConcludePayoutLayout>;
export const sameRefundWithdrawalLayout = sameLayout<RefundWithdrawalLayout>;

export const initialAbsorbDepositLayout = ({
  inputs,
  referenceInputs,
  deposit,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly deposit: SDK.DepositUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): AbsorbDepositLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const pointers: SDK.RedeemerPointer[] = [
    {
      tag: CML.RedeemerTag.Spend,
      index: BigInt(findOutRefIndex(orderedInputs, deposit.utxo) ?? -1),
    },
    { tag: CML.RedeemerTag.Mint, index: 0n },
    { tag: CML.RedeemerTag.Cert, index: 0n },
    { tag: CML.RedeemerTag.Reward, index: 0n },
  ];
  return {
    depositInputIndex: BigInt(
      findOutRefIndex(orderedInputs, deposit.utxo) ?? -1,
    ),
    reserveOutputIndex: 0n,
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
    settlementRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, settlementRefInput) ?? -1,
    ),
    burnRedeemerIndex: expectedTxInfoIndex(pointers, pointers[1]!),
    witnessUnregistrationRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[2]!,
    ),
    inclusionProofWithdrawalRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[3]!,
    ),
  };
};

export const deriveAbsorbDepositLayout = ({
  tx,
  deposit,
  depositUnit,
  reserveAddress,
  reserveAssets,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly deposit: SDK.DepositUTxO;
  readonly depositUnit: string;
  readonly reserveAddress: string;
  readonly reserveAssets: Assets;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): AbsorbDepositLayout => {
  const depositInputIndex = requireInputIndex(tx, deposit.utxo);
  const reserveOutput = requireOutput(
    tx,
    (output) =>
      output.address === reserveAddress &&
      outputHasNoDatum(output) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, reserveAssets),
    `reserve absorption output at ${reserveAddress}`,
  );
  const mintPointer = { tag: CML.RedeemerTag.Mint, index: 0n };
  const certPointer = { tag: CML.RedeemerTag.Cert, index: 0n };
  const rewardPointer = { tag: CML.RedeemerTag.Reward, index: 0n };
  if ((deposit.utxo.assets[depositUnit] ?? 0n) !== 1n) {
    throw new Error(
      `Deposit input does not contain exactly one ${depositUnit}`,
    );
  }
  return {
    depositInputIndex,
    reserveOutputIndex: BigInt(reserveOutput.index),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
    settlementRefInputIndex: requireReferenceInputIndex(tx, settlementRefInput),
    burnRedeemerIndex: actualTxInfoIndex(tx, mintPointer),
    witnessUnregistrationRedeemerIndex: actualTxInfoIndex(tx, certPointer),
    inclusionProofWithdrawalRedeemerIndex: actualTxInfoIndex(tx, rewardPointer),
  };
};

export const initialInitializePayoutLayout = ({
  inputs,
  referenceInputs,
  withdrawal,
  hubOracleRefInput,
  settlementRefInput,
  withdrawalPolicyId,
  payoutPolicyId,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
  readonly withdrawalPolicyId: string;
  readonly payoutPolicyId: string;
}): InitializePayoutLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const withdrawalInputIndex = BigInt(
    findOutRefIndex(orderedInputs, withdrawal.utxo) ?? -1,
  );
  const withdrawalMintPointerIndex = mintPointerIndex(
    [withdrawalPolicyId, payoutPolicyId],
    withdrawalPolicyId,
  );
  const payoutMintPointerIndex = mintPointerIndex(
    [withdrawalPolicyId, payoutPolicyId],
    payoutPolicyId,
  );
  const pointers: SDK.RedeemerPointer[] = [
    { tag: CML.RedeemerTag.Spend, index: withdrawalInputIndex },
    { tag: CML.RedeemerTag.Mint, index: withdrawalMintPointerIndex },
    { tag: CML.RedeemerTag.Mint, index: payoutMintPointerIndex },
    { tag: CML.RedeemerTag.Cert, index: 0n },
    { tag: CML.RedeemerTag.Reward, index: 0n },
  ];
  return {
    withdrawalInputIndex,
    payoutOutputIndex: 0n,
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
    settlementRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, settlementRefInput) ?? -1,
    ),
    withdrawalBurnRedeemerIndex: expectedTxInfoIndex(pointers, pointers[1]!),
    payoutMintRedeemerIndex: expectedTxInfoIndex(pointers, pointers[2]!),
    withdrawalSpendRedeemerIndex: expectedTxInfoIndex(pointers, pointers[0]!),
    witnessUnregistrationRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[3]!,
    ),
    inclusionProofWithdrawalRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[4]!,
    ),
  };
};

export const deriveInitializePayoutLayout = ({
  tx,
  withdrawal,
  payoutAddress,
  payoutAssets,
  payoutDatumCbor,
  hubOracleRefInput,
  settlementRefInput,
  withdrawalPolicyId,
  payoutPolicyId,
}: {
  readonly tx: CML.Transaction;
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly payoutAddress: string;
  readonly payoutAssets: Assets;
  readonly payoutDatumCbor: string;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
  readonly withdrawalPolicyId: string;
  readonly payoutPolicyId: string;
}): InitializePayoutLayout => {
  const withdrawalInputIndex = requireInputIndex(tx, withdrawal.utxo);
  const payoutOutput = requireOutput(
    tx,
    (output) =>
      output.address === payoutAddress &&
      output.datum === payoutDatumCbor &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, payoutAssets),
    `payout initialization output at ${payoutAddress}`,
  );
  const withdrawalMintPointer = {
    tag: CML.RedeemerTag.Mint,
    index: mintPointerIndex(
      [withdrawalPolicyId, payoutPolicyId],
      withdrawalPolicyId,
    ),
  };
  const payoutMintPointer = {
    tag: CML.RedeemerTag.Mint,
    index: mintPointerIndex(
      [withdrawalPolicyId, payoutPolicyId],
      payoutPolicyId,
    ),
  };
  return {
    withdrawalInputIndex,
    payoutOutputIndex: BigInt(payoutOutput.index),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
    settlementRefInputIndex: requireReferenceInputIndex(tx, settlementRefInput),
    withdrawalBurnRedeemerIndex: actualTxInfoIndex(tx, withdrawalMintPointer),
    payoutMintRedeemerIndex: actualTxInfoIndex(tx, payoutMintPointer),
    withdrawalSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: withdrawalInputIndex,
    }),
    witnessUnregistrationRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
    }),
    inclusionProofWithdrawalRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Reward,
      index: 0n,
    }),
  };
};

export const initialAddReserveFundsLayout = ({
  inputs,
  referenceInputs,
  payoutInput,
  reserveInput,
  hubOracleRefInput,
  reserveChangeAssets,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly reserveChangeAssets: Assets;
}): AddReserveFundsLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const payoutInputIndex = BigInt(
    findOutRefIndex(orderedInputs, payoutInput) ?? -1,
  );
  const reserveInputIndex = BigInt(
    findOutRefIndex(orderedInputs, reserveInput) ?? -1,
  );
  const payoutPointer = { tag: CML.RedeemerTag.Spend, index: payoutInputIndex };
  const reservePointer = {
    tag: CML.RedeemerTag.Spend,
    index: reserveInputIndex,
  };
  const pointers = [payoutPointer, reservePointer];
  return {
    payoutInputIndex,
    reserveInputIndex,
    payoutOutputIndex: 0n,
    reserveChangeOutputIndex: hasNonZeroAssetQuantity(reserveChangeAssets)
      ? 1n
      : null,
    payoutSpendRedeemerIndex: expectedTxInfoIndex(pointers, payoutPointer),
    reserveSpendRedeemerIndex: expectedTxInfoIndex(pointers, reservePointer),
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
  };
};

export const deriveAddReserveFundsLayout = ({
  tx,
  payoutInput,
  reserveInput,
  payoutAddress,
  payoutOutputAssets,
  payoutDatumCbor,
  reserveAddress,
  reserveChangeAssets,
  hubOracleRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
  readonly payoutAddress: string;
  readonly payoutOutputAssets: Assets;
  readonly payoutDatumCbor: string;
  readonly reserveAddress: string;
  readonly reserveChangeAssets: Assets;
  readonly hubOracleRefInput: UTxO;
}): AddReserveFundsLayout => {
  const payoutInputIndex = requireInputIndex(tx, payoutInput);
  const reserveInputIndex = requireInputIndex(tx, reserveInput);
  const payoutOutput = requireOutput(
    tx,
    (output) =>
      output.address === payoutAddress &&
      output.datum === payoutDatumCbor &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, payoutOutputAssets),
    `updated payout output at ${payoutAddress}`,
  );
  const reserveChangeOutput = !hasNonZeroAssetQuantity(reserveChangeAssets)
    ? undefined
    : requireOutput(
        tx,
        (output) =>
          output.address === reserveAddress &&
          outputHasNoDatum(output) &&
          output.scriptRef === undefined &&
          assetsEqual(output.assets, reserveChangeAssets),
        `reserve change output at ${reserveAddress}`,
      );
  return {
    payoutInputIndex,
    reserveInputIndex,
    payoutOutputIndex: BigInt(payoutOutput.index),
    reserveChangeOutputIndex:
      reserveChangeOutput === undefined
        ? null
        : BigInt(reserveChangeOutput.index),
    payoutSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: payoutInputIndex,
    }),
    reserveSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: reserveInputIndex,
    }),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
  };
};

export const initialConcludePayoutLayout = ({
  inputs,
  referenceInputs,
  payoutInput,
  hubOracleRefInput,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly payoutInput: UTxO;
  readonly hubOracleRefInput: UTxO;
}): ConcludePayoutLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const payoutInputIndex = BigInt(
    findOutRefIndex(orderedInputs, payoutInput) ?? -1,
  );
  const spendPointer = { tag: CML.RedeemerTag.Spend, index: payoutInputIndex };
  const burnPointer = { tag: CML.RedeemerTag.Mint, index: 0n };
  const pointers = [spendPointer, burnPointer];
  return {
    payoutInputIndex,
    l1OutputIndex: 0n,
    payoutSpendRedeemerIndex: expectedTxInfoIndex(pointers, spendPointer),
    burnRedeemerIndex: expectedTxInfoIndex(pointers, burnPointer),
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
  };
};

export const deriveConcludePayoutLayout = ({
  tx,
  payoutInput,
  l1Address,
  l1Datum,
  l1Assets,
  hubOracleRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly payoutInput: UTxO;
  readonly l1Address: string;
  readonly l1Datum: SDK.CardanoDatum;
  readonly l1Assets: Assets;
  readonly hubOracleRefInput: UTxO;
}): ConcludePayoutLayout => {
  const payoutInputIndex = requireInputIndex(tx, payoutInput);
  const l1Output = requireOutput(
    tx,
    (output) =>
      output.address === l1Address &&
      outputDatumMatches(output, l1Datum) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, l1Assets),
    `payout destination output at ${l1Address}`,
  );
  return {
    payoutInputIndex,
    l1OutputIndex: BigInt(l1Output.index),
    payoutSpendRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Spend,
      index: payoutInputIndex,
    }),
    burnRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
    }),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
  };
};

export const initialRefundWithdrawalLayout = ({
  inputs,
  referenceInputs,
  withdrawal,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly inputs: readonly UTxO[];
  readonly referenceInputs: readonly UTxO[];
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): RefundWithdrawalLayout => {
  const orderedInputs = [...inputs].sort(compareOutRefs);
  const orderedRefs = [...referenceInputs].sort(compareOutRefs);
  const withdrawalInputIndex = BigInt(
    findOutRefIndex(orderedInputs, withdrawal.utxo) ?? -1,
  );
  const pointers: SDK.RedeemerPointer[] = [
    { tag: CML.RedeemerTag.Spend, index: withdrawalInputIndex },
    { tag: CML.RedeemerTag.Mint, index: 0n },
    { tag: CML.RedeemerTag.Cert, index: 0n },
    { tag: CML.RedeemerTag.Reward, index: 0n },
  ];
  return {
    withdrawalInputIndex,
    refundOutputIndex: 0n,
    hubRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, hubOracleRefInput) ?? -1,
    ),
    settlementRefInputIndex: BigInt(
      findOutRefIndex(orderedRefs, settlementRefInput) ?? -1,
    ),
    burnRedeemerIndex: expectedTxInfoIndex(pointers, pointers[1]!),
    witnessUnregistrationRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[2]!,
    ),
    inclusionProofWithdrawalRedeemerIndex: expectedTxInfoIndex(
      pointers,
      pointers[3]!,
    ),
  };
};

export const deriveRefundWithdrawalLayout = ({
  tx,
  withdrawal,
  refundAddress,
  refundDatum,
  refundAssets,
  hubOracleRefInput,
  settlementRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly refundAddress: string;
  readonly refundDatum: SDK.CardanoDatum;
  readonly refundAssets: Assets;
  readonly hubOracleRefInput: UTxO;
  readonly settlementRefInput: UTxO;
}): RefundWithdrawalLayout => {
  const withdrawalInputIndex = requireInputIndex(tx, withdrawal.utxo);
  const refundOutput = requireOutput(
    tx,
    (output) =>
      output.address === refundAddress &&
      outputDatumMatches(output, refundDatum) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, refundAssets),
    `withdrawal refund output at ${refundAddress}`,
  );
  return {
    withdrawalInputIndex,
    refundOutputIndex: BigInt(refundOutput.index),
    hubRefInputIndex: requireReferenceInputIndex(tx, hubOracleRefInput),
    settlementRefInputIndex: requireReferenceInputIndex(tx, settlementRefInput),
    burnRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
    }),
    witnessUnregistrationRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
    }),
    inclusionProofWithdrawalRedeemerIndex: actualTxInfoIndex(tx, {
      tag: CML.RedeemerTag.Reward,
      index: 0n,
    }),
  };
};
