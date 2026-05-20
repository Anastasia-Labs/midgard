import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type Assets, type UTxO } from "@lucid-evolution/lucid";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  compareOutRefs,
  findOutRefIndex,
  outRefLabel,
  type IndexedTxOutput,
} from "@/tx-context.js";
import {
  assetsEqual,
  normalizeAssets,
} from "@/transactions/reserve-payout/assets.js";

type RedeemerPointerLike = {
  readonly tag: number;
  readonly index: bigint;
};

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

const txInfoPurposeRank = (tag: number): number => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return 0;
    case CML.RedeemerTag.Mint:
      return 1;
    case CML.RedeemerTag.Cert:
      return 2;
    case CML.RedeemerTag.Reward:
      return 3;
    case CML.RedeemerTag.Voting:
      return 4;
    case CML.RedeemerTag.Proposing:
      return 5;
    default:
      return Number.MAX_SAFE_INTEGER;
  }
};

const samePointer = (
  left: RedeemerPointerLike,
  right: RedeemerPointerLike,
): boolean => left.tag === right.tag && left.index === right.index;

const expectedTxInfoIndex = (
  pointers: readonly RedeemerPointerLike[],
  target: RedeemerPointerLike,
): bigint => {
  const contextIndex = pointers.findIndex((pointer) =>
    samePointer(pointer, target),
  );
  if (contextIndex < 0) {
    throw new Error(
      `Expected redeemer pointer missing: tag=${target.tag.toString()},index=${target.index.toString()}`,
    );
  }
  const ordered = pointers
    .map((pointer, index) => ({ pointer, index }))
    .sort((left, right) => {
      const rankLeft = txInfoPurposeRank(left.pointer.tag);
      const rankRight = txInfoPurposeRank(right.pointer.tag);
      if (rankLeft !== rankRight) {
        return rankLeft - rankRight;
      }
      if (left.pointer.index !== right.pointer.index) {
        return left.pointer.index < right.pointer.index ? -1 : 1;
      }
      return left.index - right.index;
    });
  const txInfoIndex = ordered.findIndex(
    (entry) => entry.index === contextIndex,
  );
  if (txInfoIndex < 0) {
    throw new Error("Failed to derive expected tx-info redeemer index");
  }
  return BigInt(txInfoIndex);
};

const actualTxInfoIndex = (
  tx: CML.Transaction,
  target: RedeemerPointerLike,
): bigint => {
  const pointers = SDK.getRedeemerPointersInContextOrder(tx);
  const contextIndex = pointers.findIndex((pointer) =>
    samePointer(pointer, target),
  );
  if (contextIndex < 0) {
    throw new Error(
      `Transaction missing redeemer pointer tag=${target.tag.toString()},index=${target.index.toString()}`,
    );
  }
  const txInfoIndexes = SDK.getTxInfoRedeemerIndexes(pointers);
  const txInfoIndex = txInfoIndexes[contextIndex];
  if (txInfoIndex === undefined || txInfoIndex < 0) {
    throw new Error(
      `Transaction missing tx-info index for redeemer pointer tag=${target.tag.toString()},index=${target.index.toString()}`,
    );
  }
  return BigInt(txInfoIndex);
};

const comparePolicyIds = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

const mintPointerIndex = (
  policyIds: readonly string[],
  targetPolicyId: string,
): bigint => {
  const sorted = [
    ...new Set(policyIds.map((policy) => policy.toLowerCase())),
  ].sort(comparePolicyIds);
  const index = sorted.indexOf(targetPolicyId.toLowerCase());
  if (index < 0) {
    throw new Error(
      `Mint policy ${targetPolicyId} missing from mint policy set`,
    );
  }
  return BigInt(index);
};

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

export const sameAbsorbDepositLayout = (
  left: AbsorbDepositLayout,
  right: AbsorbDepositLayout,
): boolean =>
  left.depositInputIndex === right.depositInputIndex &&
  left.reserveOutputIndex === right.reserveOutputIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex &&
  left.settlementRefInputIndex === right.settlementRefInputIndex &&
  left.burnRedeemerIndex === right.burnRedeemerIndex &&
  left.witnessUnregistrationRedeemerIndex ===
    right.witnessUnregistrationRedeemerIndex &&
  left.inclusionProofWithdrawalRedeemerIndex ===
    right.inclusionProofWithdrawalRedeemerIndex;

export const sameInitializePayoutLayout = (
  left: InitializePayoutLayout,
  right: InitializePayoutLayout,
): boolean =>
  left.withdrawalInputIndex === right.withdrawalInputIndex &&
  left.payoutOutputIndex === right.payoutOutputIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex &&
  left.settlementRefInputIndex === right.settlementRefInputIndex &&
  left.withdrawalBurnRedeemerIndex === right.withdrawalBurnRedeemerIndex &&
  left.payoutMintRedeemerIndex === right.payoutMintRedeemerIndex &&
  left.withdrawalSpendRedeemerIndex === right.withdrawalSpendRedeemerIndex &&
  left.witnessUnregistrationRedeemerIndex ===
    right.witnessUnregistrationRedeemerIndex &&
  left.inclusionProofWithdrawalRedeemerIndex ===
    right.inclusionProofWithdrawalRedeemerIndex;

export const sameAddReserveFundsLayout = (
  left: AddReserveFundsLayout,
  right: AddReserveFundsLayout,
): boolean =>
  left.payoutInputIndex === right.payoutInputIndex &&
  left.reserveInputIndex === right.reserveInputIndex &&
  left.payoutOutputIndex === right.payoutOutputIndex &&
  left.reserveChangeOutputIndex === right.reserveChangeOutputIndex &&
  left.payoutSpendRedeemerIndex === right.payoutSpendRedeemerIndex &&
  left.reserveSpendRedeemerIndex === right.reserveSpendRedeemerIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex;

export const sameConcludePayoutLayout = (
  left: ConcludePayoutLayout,
  right: ConcludePayoutLayout,
): boolean =>
  left.payoutInputIndex === right.payoutInputIndex &&
  left.l1OutputIndex === right.l1OutputIndex &&
  left.payoutSpendRedeemerIndex === right.payoutSpendRedeemerIndex &&
  left.burnRedeemerIndex === right.burnRedeemerIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex;

export const sameRefundWithdrawalLayout = (
  left: RefundWithdrawalLayout,
  right: RefundWithdrawalLayout,
): boolean =>
  left.withdrawalInputIndex === right.withdrawalInputIndex &&
  left.refundOutputIndex === right.refundOutputIndex &&
  left.hubRefInputIndex === right.hubRefInputIndex &&
  left.settlementRefInputIndex === right.settlementRefInputIndex &&
  left.burnRedeemerIndex === right.burnRedeemerIndex &&
  left.witnessUnregistrationRedeemerIndex ===
    right.witnessUnregistrationRedeemerIndex &&
  left.inclusionProofWithdrawalRedeemerIndex ===
    right.inclusionProofWithdrawalRedeemerIndex;

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
  const pointers: RedeemerPointerLike[] = [
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
  const pointers: RedeemerPointerLike[] = [
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
    reserveChangeOutputIndex:
      Object.keys(normalizeAssets(reserveChangeAssets)).length === 0
        ? null
        : 1n,
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
  const normalizedReserveChange = normalizeAssets(reserveChangeAssets);
  const reserveChangeOutput =
    Object.keys(normalizedReserveChange).length === 0
      ? undefined
      : requireOutput(
          tx,
          (output) =>
            output.address === reserveAddress &&
            outputHasNoDatum(output) &&
            output.scriptRef === undefined &&
            assetsEqual(output.assets, normalizedReserveChange),
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
  const pointers: RedeemerPointerLike[] = [
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
