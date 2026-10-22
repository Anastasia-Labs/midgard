/** The close of a case its defender won, on the Open scene: once the case's
 * duration has passed with a thread still open, the case and its bond are
 * spent together, the case's tokens and computation thread burn, the bond
 * goes to the protocol treasury and the case's lovelace to its claimant.
 * Anyone may send it; the src builds only the convicting close. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  fromUnit,
  type LucidEvolution,
  type RedeemerContext,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { validationTraceMoveWindow } from "../../src/validation-trace-dispute/move-builders.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import { network } from "./submit-init-emulator-shared.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** The protocol treasury a forfeited bond goes to. */
export const protocolTreasury = (): string =>
  SDK.protocolTreasuryAddress(
    network,
    SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
  );

const tagOf = (utxo: UTxO): string =>
  Data.to(new Constr(0, [utxo.txHash, BigInt(utxo.outputIndex)]));

/** The units of `policyId` `utxo` holds, each negated. */
const burnOf = (utxo: UTxO, policyId: string): Record<string, bigint> =>
  Object.fromEntries(
    Object.keys(utxo.assets)
      .filter((unit) => unit.startsWith(policyId))
      .map((unit) => [unit, -1n]),
  );

/**
 * The DefenderWon CaseClose of `caseUtxo` with its `bond`, now, from
 * `lucid`'s wallet, before signing; `bondTo` replaces the treasury as the
 * bond's payee.
 */
export const defenderWonCaseClose = async ({
  scene,
  lucid,
  caseUtxo,
  bond,
  bondTo = protocolTreasury(),
}: {
  readonly scene: Scene;
  readonly lucid: LucidEvolution;
  readonly caseUtxo: UTxO;
  readonly bond: UTxO;
  readonly bondTo?: string;
}): Promise<TxSignBuilder> => {
  const { h, chain, references } = scene;
  const caseDatum = Data.from(caseUtxo.datum!, SDK.Case);
  const computationThread = h.contracts.computationThread;
  const threadBurn = burnOf(caseUtxo, computationThread.policyId);
  const [threadUnit] = Object.keys(threadBurn);
  if (threadUnit === undefined)
    throw new Error("the case holds no computation thread token");
  const carriage = witnessMintingPolicyCarriage({
    script: computationThread.mintingScript,
    referenceUtxo: h.witnessReferenceScripts?.computationThreadMint,
    label: "validation-trace defender-won close thread burn",
  });
  const close = (ctx: RedeemerContext) =>
    Data.to(
      {
        CaseClose: {
          bond_input_index: SDK.requireInputIndex(ctx, bond, "bond"),
          bond_output_index: 0n,
          remainder_output_index: 1n,
        },
      },
      SDK.CaseRedeemer,
    );
  const window = validationTraceMoveWindow(lucid);
  return await lucid
    .newTx()
    .collectFrom([caseUtxo], close)
    .collectFrom([bond], (ctx) =>
      Data.to(
        { case_input_index: SDK.requireInputIndex(ctx, caseUtxo, "case") },
        SDK.DisputeBondSpendRedeemer,
      ),
    )
    .mintAssets(
      threadBurn,
      Data.to(
        { Success: { burning_token_asset_name: fromUnit(threadUnit).name! } },
        SDK.FraudProofComputationThreadRedeemer,
      ),
    )
    .mintAssets(
      {
        ...burnOf(caseUtxo, chain.case.policyId),
        ...burnOf(bond, chain.case.policyId),
      },
      close,
    )
    .readFrom([
      scene.caseConfig,
      references.case,
      references.bond,
      ...carriage.referenceInputs,
    ])
    .pay.ToAddressWithData(
      bondTo,
      { kind: "inline", value: tagOf(bond) },
      { lovelace: bond.assets.lovelace ?? 0n },
    )
    .pay.ToAddressWithData(
      credentialToAddress(network, {
        type: "Key",
        hash: caseDatum.bounty_claimant,
      }),
      { kind: "inline", value: tagOf(caseUtxo) },
      { lovelace: caseUtxo.assets.lovelace ?? 0n },
    )
    .validFrom(window.validFrom)
    .validTo(window.validTo)
    .complete({ localUPLCEval: true });
};
