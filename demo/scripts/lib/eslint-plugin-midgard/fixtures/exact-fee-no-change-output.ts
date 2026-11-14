declare const lucid: any;
declare const penalty: bigint;
declare const plan: { feeLovelace: bigint };
declare const exactFeeCompleteOptions: (plan: unknown) => object;

export const withChange = async () => {
  // ruleid: midgard/exact-fee-no-change-output
  const tx = lucid.newTx().setMinFee(penalty);
  return tx.complete({ localUPLCEval: true });
};

export const maybeExact = async (exactly: boolean) => {
  // ruleid: midgard/exact-fee-no-change-output
  const tx = lucid.newTx().setMinFee(penalty);
  return tx.complete({ localUPLCEval: true, coinSelection: !exactly });
};

export const completedElsewhere = () => {
  // ruleid: midgard/exact-fee-no-change-output
  return lucid.newTx().setMinFee(penalty);
};

export const unrelatedCompletion = (tx: any) =>
  tx.complete({ localUPLCEval: true, coinSelection: false });

export const exact = async () => {
  // ok: midgard/exact-fee-no-change-output
  const tx = lucid.newTx().setMinFee(penalty);
  return tx.complete({ localUPLCEval: true, coinSelection: false });
};

export const throughPlan = async () => {
  // ok: midgard/exact-fee-no-change-output
  const tx = lucid.newTx().setMinFee(plan.feeLovelace);
  return tx.complete(exactFeeCompleteOptions(plan));
};

const exactOptions = { localUPLCEval: true, coinSelection: false } as const;

export const throughConstant = async () => {
  // ok: midgard/exact-fee-no-change-output
  const tx = lucid.newTx().setMinFee(penalty);
  return tx.complete({ ...exactOptions });
};

export const nestedBuild = async () => {
  const build = () => {
    // ok: midgard/exact-fee-no-change-output
    return lucid.newTx().setMinFee(penalty);
  };
  return build().complete({ localUPLCEval: true, coinSelection: false });
};
