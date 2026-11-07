import { ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";

import {
  hash32,
  input,
  output,
  raw,
  releaseEconomics,
} from "./raw-l1-terminal-fixture.output.js";

/** Separate the last target slash from an earlier, different operator's slash. */
export const finalDescendantRemoval = ({
  continuedTargetOutRef,
  stateAddress,
  rootUnit,
  rootDatum,
  proofOutRef,
  stateUnit,
  verifiedEconomics,
  activePolicy,
  activeAddress,
  operatorCredential,
  rewardAddress,
  distinctOperator,
  duplicateReward,
}: {
  readonly continuedTargetOutRef: string;
  readonly stateAddress: string;
  readonly rootUnit: string;
  readonly rootDatum: string;
  readonly proofOutRef: string;
  readonly stateUnit: string;
  readonly verifiedEconomics: typeof releaseEconomics;
  readonly activePolicy: string;
  readonly activeAddress: string;
  readonly operatorCredential: string;
  readonly rewardAddress: string;
  readonly distinctOperator: boolean;
  readonly duplicateReward: boolean;
}) => {
  const targetBondUnit = toUnit(
    activePolicy,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorCredential,
  );
  const targetBond = distinctOperator
    ? raw(
        `${hash32("43")}#0`,
        output({
          address: activeAddress,
          assets: {
            lovelace: BigInt(verifiedEconomics.policy.requiredBondLovelace),
            [targetBondUnit]: 1n,
          },
          datum: Data.to("" as never, Data.Bytes()),
        }),
      )
    : undefined;
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(continuedTargetOutRef));
  if (targetBond !== undefined) inputs.add(input(targetBond.outRef));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    output({
      address: stateAddress,
      assets: { lovelace: 3_000_000n, [rootUnit]: 1n },
      datum: rootDatum,
    }),
  );
  if (distinctOperator) {
    for (let n = 0; n < (duplicateReward ? 2 : 1); n += 1)
      outputs.add(
        output({
          address: rewardAddress,
          assets: {
            lovelace: BigInt(
              verifiedEconomics.policy.fraudProverRewardLovelace,
            ),
          },
        }),
      );
  }
  const body = CML.TransactionBody.new(
    inputs,
    outputs,
    distinctOperator
      ? BigInt(verifiedEconomics.policy.slashingPenaltyLovelace)
      : 200_000n,
  );
  const references = CML.TransactionInputList.new();
  references.add(input(proofOutRef));
  body.set_reference_inputs(references);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(stateUnit.slice(0, 56)),
    CML.AssetName.from_hex(stateUnit.slice(56)),
    -1n,
  );
  if (distinctOperator)
    mint.set(
      CML.ScriptHash.from_hex(activePolicy),
      CML.AssetName.from_hex(targetBondUnit.slice(56)),
      -1n,
    );
  body.set_mint(mint);
  return { body, targetBond, rootOutput: outputs.get(0) };
};
