/** The wallet a collateral test shows: an attempt's phase-2-invalid landing
 * spends its collateral and creates its collateral return. */
import type { UTxO } from "@lucid-evolution/lucid";

import {
  type setupFundingRecoveryFixture,
  walletAddress,
} from "./fault-proof-funding-fixture.js";

export type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

const coin = (outRef: string, lovelace: bigint): UTxO => {
  const [txHash, outputIndex] = outRef.split("#");
  return {
    txHash: txHash!,
    outputIndex: Number(outputIndex),
    address: walletAddress,
    assets: { lovelace },
  };
};

/** The wallet as the chain shows it: with the attempt's invalid landing, the
 * collateral is spent and its return exists; without it, the reverse. */
export const showLanding = (fixture: Fixture, landed: boolean) => {
  const collateral = fixture.collateralInput;
  const back = fixture.collateralReturn!;
  const [gone, shown] = landed
    ? [collateral.outRef, coin(back.outRef, BigInt(back.lovelace))]
    : [back.outRef, coin(collateral.outRef, BigInt(collateral.lovelace))];
  const kept = fixture.walletUtxos.filter(
    ({ txHash, outputIndex }) => `${txHash}#${outputIndex}` !== gone,
  );
  fixture.walletUtxos.splice(0, fixture.walletUtxos.length, ...kept, shown);
};

export const forfeitureOf = (fixture: Fixture) => ({
  kind: "collateral_forfeited",
  actionId: "init",
  txHash: fixture.transactionHash,
  collateralOutRefs: [fixture.collateralInput.outRef],
  collateralReturnOutRef: fixture.collateralReturn!.outRef,
});

export const heldOutRefs = async (fixture: Fixture) =>
  (await fixture.records())[0]!.activeInputs.map(({ outRef }) => outRef).sort();
